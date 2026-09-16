;;; opencode-shell-async.el --- Async scheduling for OpenCode Shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Latest-value idle scheduling and shared runtime storage.  Transport code may
;; enqueue state delivery here, but buffer rendering belongs to opencode-shell.

;;; Code:

(require 'cl-lib)
(require 'url-parse)

(defvar opencode-shell-async--runtimes (make-hash-table :test #'equal)
  "Shared runtime state keyed by server identity.")

(defvar opencode-shell-async--animation-subscribers
  (make-hash-table :test #'eq :weakness 'key)
  "Visible-buffer animation callbacks keyed by transcript buffer.")

(defvar opencode-shell-async--animation-timer nil
  "Single timer serving all visible transcript animations.")

(defconst opencode-shell-async--reconcile-interval 15)

(defvar-local opencode-shell-async--queue nil
  "Latest queued callback for each key in the current buffer.")

(defvar-local opencode-shell-async--idle-timer nil
  "Idle timer draining `opencode-shell-async--queue'.")

(defun opencode-shell-async-runtime-get (key)
  "Return shared runtime registered under KEY."
  (gethash key opencode-shell-async--runtimes))

(defun opencode-shell-async-runtime-put (key runtime)
  "Register RUNTIME under KEY and return it."
  (puthash key runtime opencode-shell-async--runtimes)
  runtime)

(defun opencode-shell-async-runtime-remove (key)
  "Remove and return the runtime registered under KEY."
  (prog1 (gethash key opencode-shell-async--runtimes)
    (remhash key opencode-shell-async--runtimes)))

(defun opencode-shell-async--deliver-runtime (runtime reason)
  "Coalesce RUNTIME subscriber callbacks for REASON."
  (maphash
   (lambda (buffer callback)
     (if (not (buffer-live-p buffer))
         (remhash buffer (plist-get runtime :subscribers))
       (with-current-buffer buffer
         (opencode-shell-async-enqueue
          buffer (list 'runtime reason) opencode-shell--generation callback))))
   (plist-get runtime :subscribers)))

(defun opencode-shell-async--runtime-log (runtime format-string &rest arguments)
  "Emit a privacy-safe runtime message for RUNTIME."
  (when-let ((logger (plist-get runtime :logger)))
    (funcall logger (apply #'format format-string arguments))))

(defun opencode-shell-async--poll-runtime (key)
  "Poll subscribers for KEY at fallback or reconciliation cadence."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (let* ((connected (plist-get runtime :connected))
           (ticks (1+ (or (plist-get runtime :ticks) 0)))
           (interval (or (plist-get runtime :poll-interval) 2))
           (reconcile-ticks (max 1 (/ opencode-shell-async--reconcile-interval
                                      interval))))
      (setf (plist-get runtime :ticks) ticks)
      (when (or (not connected) (zerop (% ticks reconcile-ticks)))
        (opencode-shell-async--runtime-log
         runtime "transport=%s wake=poll"
         (if connected "sse" "fallback"))
        (opencode-shell-async--deliver-runtime runtime 'poll)))))

(defun opencode-shell-async--parse-sse (runtime text)
  "Append TEXT and wake RUNTIME once per complete SSE data frame."
  (let ((input (concat (or (plist-get runtime :sse-input) "")
                       (replace-regexp-in-string "\r" "" text))))
    (while (string-match "\n\n" input)
      (let ((frame (substring input 0 (match-beginning 0))))
        (setq input (substring input (match-end 0)))
        (when (string-match-p "\\(?:^\\|\n\\)data:" frame)
          (opencode-shell-async--deliver-runtime runtime 'event))))
    (setf (plist-get runtime :sse-input) input)))

(defun opencode-shell-async--parse-chunks (runtime)
  "Decode complete HTTP chunks buffered in RUNTIME."
  (let ((input (or (plist-get runtime :input) "")) done)
    (while (and (not done) (string-match "\\`\\([[:xdigit:]]+\\)\r\n" input))
      (let* ((size (string-to-number (match-string 1 input) 16))
             (start (match-end 0))
             (end (+ start size)))
        (if (> (+ end 2) (length input))
            (setq done t)
          (unless (zerop size)
            (opencode-shell-async--parse-sse runtime (substring input start end)))
          (setq input (substring input (+ end 2)))
          (when (zerop size) (setq done t)))))
    (setf (plist-get runtime :input) input)))

(defun opencode-shell-async--stream-filter (key _process chunk)
  "Consume an HTTP SSE CHUNK for runtime KEY."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (setf (plist-get runtime :input)
          (concat (or (plist-get runtime :input) "") chunk))
    (unless (plist-get runtime :headers-done)
      (when (string-match "\r\n\r\n" (plist-get runtime :input))
        (let* ((input (plist-get runtime :input))
               (headers (substring input 0 (match-beginning 0))))
          (setf (plist-get runtime :input) (substring input (match-end 0))
                (plist-get runtime :headers-done) t
                (plist-get runtime :chunked)
                (string-match-p "transfer-encoding:[ \t]*chunked" (downcase headers))
                (plist-get runtime :connected)
                (string-match-p "\\`HTTP/[0-9.]+ 2[0-9][0-9]" headers)
                (plist-get runtime :backoff) 1)
          (opencode-shell-async--runtime-log
           runtime "transport=%s"
           (if (plist-get runtime :connected) "sse-connected" "fallback-http"))
          (unless (plist-get runtime :connected)
            (delete-process (plist-get runtime :process))))))
    (when (plist-get runtime :headers-done)
      (if (plist-get runtime :chunked)
          (opencode-shell-async--parse-chunks runtime)
        (let ((body (plist-get runtime :input)))
          (setf (plist-get runtime :input) "")
          (opencode-shell-async--parse-sse runtime body))))))

(defun opencode-shell-async--schedule-reconnect (key)
  "Schedule a bounded asynchronous reconnect for runtime KEY."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (unless (or (zerop (hash-table-count (plist-get runtime :subscribers)))
                (timerp (plist-get runtime :reconnect-timer)))
      (let ((delay (min 30 (or (plist-get runtime :backoff) 1))))
        (opencode-shell-async--runtime-log runtime "transport=fallback reconnect=%ss" delay)
        (setf (plist-get runtime :backoff) (min 30 (* 2 delay))
              (plist-get runtime :reconnect-timer)
              (run-at-time delay nil #'opencode-shell-async--connect key))))))

(defun opencode-shell-async--stream-sentinel (key process _event)
  "Handle PROCESS closure for runtime KEY without blocking."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (when (eq process (plist-get runtime :process))
      (setf (plist-get runtime :process) nil
            (plist-get runtime :connected) nil
            (plist-get runtime :headers-done) nil
            (plist-get runtime :input) ""
            (plist-get runtime :sse-input) "")
      (opencode-shell-async--runtime-log runtime "transport=fallback stream=closed")
      (opencode-shell-async--schedule-reconnect key))))

(defun opencode-shell-async--connect (key)
  "Open the direct HTTP SSE stream for runtime KEY."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (setf (plist-get runtime :reconnect-timer) nil)
    (when (and (plist-get runtime :sse-enabled)
               (not (process-live-p (plist-get runtime :process)))
               (> (hash-table-count (plist-get runtime :subscribers)) 0))
      (condition-case nil
          (let* ((url (url-generic-parse-url (plist-get runtime :url)))
                 (host (url-host url))
                 (port (or (url-port url) 80))
                 (path (or (url-filename url) "/event"))
                 (process
                  (make-network-process
                   :name (format "opencode-sse-%s" (sxhash-equal key))
                   :host host :service port :coding 'binary :noquery t
                   :filter (lambda (process chunk)
                             (opencode-shell-async--stream-filter key process chunk))
                   :sentinel (lambda (process event)
                               (opencode-shell-async--stream-sentinel key process event)))))
            (opencode-shell-async--runtime-log runtime "transport=sse state=connecting")
            (set-process-query-on-exit-flag process nil)
            (setf (plist-get runtime :process) process
                  (plist-get runtime :input) ""
                  (plist-get runtime :sse-input) "")
            (process-send-string
             process
             (concat "GET " path " HTTP/1.1\r\nHost: " host ":" (number-to-string port)
                     "\r\nAccept: text/event-stream\r\nCache-Control: no-cache\r\n"
                     (mapconcat (lambda (header)
                                  (format "%s: %s\r\n" (car header) (cdr header)))
                                (plist-get runtime :headers) "")
                     "Connection: keep-alive\r\n\r\n")))
        (error (opencode-shell-async--schedule-reconnect key))))))

(defun opencode-shell-async-subscribe-runtime
    (key buffer url headers sse-enabled poll-interval callback &optional logger)
  "Subscribe BUFFER to KEY runtime and invoke CALLBACK on event/poll wakes."
  (let ((runtime (or (gethash key opencode-shell-async--runtimes)
                     (list :subscribers (make-hash-table :test #'eq :weakness 'key)
                           :url url :headers headers :sse-enabled sse-enabled
                           :poll-interval poll-interval
                           :logger logger
                           :backoff 1 :ticks 0 :poll-timer nil
                           :reconnect-timer nil :process nil :connected nil
                           :headers-done nil :chunked nil :input ""
                           :sse-input ""))))
    (puthash buffer callback (plist-get runtime :subscribers))
    (puthash key runtime opencode-shell-async--runtimes)
    (unless (timerp (plist-get runtime :poll-timer))
      (setf (plist-get runtime :poll-timer)
            (run-at-time poll-interval poll-interval
                         #'opencode-shell-async--poll-runtime key)))
    (if sse-enabled
        (opencode-shell-async--connect key)
      (setf (plist-get runtime :connected) nil))
    runtime))

(defun opencode-shell-async-unsubscribe-runtime (key buffer)
  "Unsubscribe BUFFER and stop KEY runtime when it has no subscribers."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (remhash buffer (plist-get runtime :subscribers))
    (when (zerop (hash-table-count (plist-get runtime :subscribers)))
      (dolist (timer (list (plist-get runtime :poll-timer)
                           (plist-get runtime :reconnect-timer)))
        (when (timerp timer) (cancel-timer timer)))
      (when (process-live-p (plist-get runtime :process))
        (delete-process (plist-get runtime :process)))
      (remhash key opencode-shell-async--runtimes))))

(defun opencode-shell-async-drain (buffer)
  "Run the current latest-value queue for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp opencode-shell-async--idle-timer)
        (cancel-timer opencode-shell-async--idle-timer))
      (setq opencode-shell-async--idle-timer nil)
      (let ((queue (prog1 (nreverse opencode-shell-async--queue)
                     (setq opencode-shell-async--queue nil))))
        (dolist (entry queue)
          (let ((generation (nth 1 entry))
                (function (nth 2 entry))
                (arguments (nthcdr 3 entry)))
            (when (or (null generation)
                      (and (boundp 'opencode-shell--generation)
                           (= generation opencode-shell--generation)))
              (apply function arguments))))))))

(defun opencode-shell-async-enqueue (buffer key generation function &rest arguments)
  "Enqueue FUNCTION with ARGUMENTS for BUFFER under KEY.
Only the latest pending value for KEY and GENERATION is retained."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setf (alist-get key opencode-shell-async--queue nil nil #'equal)
            (cons generation (cons function arguments)))
      (unless (timerp opencode-shell-async--idle-timer)
        (setq opencode-shell-async--idle-timer
              (run-with-idle-timer 0 nil #'opencode-shell-async-drain buffer))))))

(defun opencode-shell-async-cancel (&optional buffer)
  "Cancel queued work for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp opencode-shell-async--idle-timer)
      (cancel-timer opencode-shell-async--idle-timer))
    (setq opencode-shell-async--idle-timer nil
          opencode-shell-async--queue nil)))

(defun opencode-shell-async--animation-tick ()
  "Run animation callbacks only for live, visible subscribed buffers."
  (maphash
   (lambda (buffer callback)
     (if (not (buffer-live-p buffer))
         (remhash buffer opencode-shell-async--animation-subscribers)
       (when (get-buffer-window buffer t)
         (with-current-buffer buffer (funcall callback)))))
   opencode-shell-async--animation-subscribers))

(defun opencode-shell-async-subscribe-animation (buffer interval callback)
  "Subscribe visible BUFFER to the shared animation CALLBACK at INTERVAL."
  (puthash buffer callback opencode-shell-async--animation-subscribers)
  (unless (timerp opencode-shell-async--animation-timer)
    (setq opencode-shell-async--animation-timer
          (run-at-time interval interval #'opencode-shell-async--animation-tick)))
  opencode-shell-async--animation-timer)

(defun opencode-shell-async-unsubscribe-animation (buffer)
  "Remove BUFFER from shared animation delivery."
  (remhash buffer opencode-shell-async--animation-subscribers)
  (when (and (zerop (hash-table-count opencode-shell-async--animation-subscribers))
             (timerp opencode-shell-async--animation-timer))
    (cancel-timer opencode-shell-async--animation-timer)
    (setq opencode-shell-async--animation-timer nil)))

(defun opencode-shell-async-reset ()
  "Cancel queued work in package buffers and clear shared runtimes."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'opencode-shell-async--queue buffer)
      (opencode-shell-async-cancel buffer)))
  (clrhash opencode-shell-async--animation-subscribers)
  (when (timerp opencode-shell-async--animation-timer)
    (cancel-timer opencode-shell-async--animation-timer))
  (setq opencode-shell-async--animation-timer nil)
  (maphash
   (lambda (_key runtime)
     (dolist (timer (list (plist-get runtime :poll-timer)
                          (plist-get runtime :reconnect-timer)))
       (when (timerp timer) (cancel-timer timer)))
     (when (process-live-p (plist-get runtime :process))
       (delete-process (plist-get runtime :process))))
   opencode-shell-async--runtimes)
  (clrhash opencode-shell-async--runtimes))

(provide 'opencode-shell-async)
;;; opencode-shell-async.el ends here
