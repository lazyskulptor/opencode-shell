;;; opencode-shell-async.el --- Async scheduling for OpenCode Shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Latest-value idle scheduling and shared runtime storage.  Transport code may
;; enqueue state delivery here, but buffer rendering belongs to opencode-shell.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'opencode-shell-sse)

(defvar opencode-shell-async--runtimes (make-hash-table :test #'equal)
  "Shared runtime state keyed by server identity.")

(defvar opencode-shell-async--animation-subscribers
  (make-hash-table :test #'eq :weakness 'key)
  "Visible-buffer animation callbacks keyed by transcript buffer.")

(defvar opencode-shell-async--animation-timer nil
  "Single timer serving all visible transcript animations.")

(defvar opencode-shell-async--animation-interval nil
  "Current cadence of the shared animation timer.")

(defconst opencode-shell-async--reconcile-interval 15)
(defconst opencode-shell-async--max-reconnect-failures 5)
(defconst opencode-shell-async--delivery-timeout 0.25)

(defvar-local opencode-shell-async--queue nil
  "Latest queued callback for each key in the current buffer.")

(defvar-local opencode-shell-async--idle-timer nil
  "Idle timer draining `opencode-shell-async--queue'.")

(defvar-local opencode-shell-async--delivery-timer nil
  "Bounded non-idle fallback timer for queued callback delivery.")

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

(defun opencode-shell-async--deliver-runtime (runtime _reason)
  "Coalesce RUNTIME subscriber callbacks under one wake key."
  (maphash
   (lambda (buffer callback)
     (if (not (buffer-live-p buffer))
         (remhash buffer (plist-get runtime :subscribers))
       (with-current-buffer buffer
          (opencode-shell-async-enqueue
           buffer 'runtime-wake opencode-shell--generation callback))))
   (plist-get runtime :subscribers)))

(defun opencode-shell-async--runtime-log (runtime format-string &rest arguments)
  "Emit a privacy-safe runtime message for RUNTIME."
  (when-let ((logger (plist-get runtime :logger)))
    (funcall logger (apply #'format format-string arguments))))

(defun opencode-shell-async--poll-runtime (key)
  "Poll subscribers for KEY at fallback or reconciliation cadence."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (let* ((connected (opencode-shell-sse-connected-p
                       (plist-get runtime :connection)))
           (ticks (1+ (or (plist-get runtime :ticks) 0)))
           (interval (or (plist-get runtime :poll-interval) 2))
           (reconcile-ticks
            (max 1 (round (/ opencode-shell-async--reconcile-interval
                             interval)))))
      (setf (plist-get runtime :ticks) ticks)
      (when (or (not connected) (zerop (% ticks reconcile-ticks)))
        (opencode-shell-async--runtime-log
         runtime "transport=%s wake=poll"
         (if connected "sse" "fallback"))
        (opencode-shell-async--deliver-runtime runtime 'poll)))))

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

(defun opencode-shell-async--transport-event (key attempt _event)
  "Wake KEY runtime for an SSE event from ATTEMPT."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (when (eq attempt (plist-get runtime :attempt))
      (setf (plist-get runtime :backoff) 1
            (plist-get runtime :failures) 0)
      (opencode-shell-async--runtime-log runtime "transport=sse wake=event")
      (opencode-shell-async--deliver-runtime runtime 'event))))

(defun opencode-shell-async--transport-open (key attempt)
  "Record a successful SSE handshake for KEY and ATTEMPT."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (when (eq attempt (plist-get runtime :attempt))
      (setf (plist-get runtime :backoff) 1
            (plist-get runtime :failures) 0)
      (opencode-shell-async--runtime-log runtime "transport=sse state=connected"))))

(defun opencode-shell-async--transport-error (key attempt error)
  "Apply typed transport ERROR to KEY runtime's current ATTEMPT."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (when (eq attempt (plist-get runtime :attempt))
      (let* ((type (plist-get error :type))
             (reason (plist-get error :reason))
             (permanent (memq type '(config protocol)))
             (failures (1+ (or (plist-get runtime :failures) 0))))
        (setf (plist-get runtime :connection) nil
              (plist-get runtime :failures) failures)
        (when (or permanent
                  (>= failures opencode-shell-async--max-reconnect-failures))
          (setf (plist-get runtime :sse-disabled) t))
        (opencode-shell-async--runtime-log
         runtime "transport=fallback error=%s reason=%s%s"
         type reason (if (plist-get runtime :sse-disabled) " circuit=open" ""))
        (unless (plist-get runtime :sse-disabled)
          (opencode-shell-async--schedule-reconnect key))))))

(defun opencode-shell-async--connect (key)
  "Open KEY's independent SSE transport."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (setf (plist-get runtime :reconnect-timer) nil)
    (when (and (plist-get runtime :sse-enabled)
               (not (plist-get runtime :sse-disabled))
               (not (memq (and (plist-get runtime :connection)
                               (opencode-shell-sse-connection-state
                                (plist-get runtime :connection)))
                          '(connecting streaming)))
               (> (hash-table-count (plist-get runtime :subscribers)) 0))
      (let ((attempt (make-symbol "opencode-sse-attempt")) connection)
        (setf (plist-get runtime :attempt) attempt)
        (opencode-shell-async--runtime-log runtime "transport=sse state=connecting")
        (setq connection
              (opencode-shell-sse-start
               (plist-get runtime :url) (plist-get runtime :headers)
               (lambda (event)
                 (opencode-shell-async--transport-event key attempt event))
               (lambda (error)
                 (opencode-shell-async--transport-error key attempt error))
               :on-open
               (lambda ()
                 (opencode-shell-async--transport-open key attempt))))
        (when (and (eq attempt (plist-get runtime :attempt))
                   (memq (opencode-shell-sse-connection-state connection)
                         '(connecting streaming)))
          (setf (plist-get runtime :connection) connection))))))

(defun opencode-shell-async-subscribe-runtime
    (key buffer url headers sse-enabled poll-interval callback &optional logger)
  "Subscribe BUFFER to KEY runtime and invoke CALLBACK on event/poll wakes."
  (unless (opencode-shell-async--positive-finite-number-p poll-interval)
    (error "Poll interval must be a positive finite number"))
  (let* ((existing (gethash key opencode-shell-async--runtimes))
         (_compatible
          (when (and existing
                     (not (and (equal url (plist-get existing :url))
                               (equal headers (plist-get existing :headers))
                               (eq (not (null sse-enabled))
                                   (not (null (plist-get existing :sse-enabled))))
                               (= poll-interval (plist-get existing :poll-interval)))))
            (error "Conflicting asynchronous runtime configuration")))
         (runtime (or existing
                       (list :subscribers (make-hash-table :test #'eq :weakness 'key)
                             :url url :headers headers :sse-enabled sse-enabled
                             :sse-disabled nil
                             :poll-interval poll-interval
                             :logger logger
                             :backoff 1 :ticks 0 :poll-timer nil
                             :reconnect-timer nil :connection nil
                             :attempt nil :failures 0))))
    (puthash buffer callback (plist-get runtime :subscribers))
    (puthash key runtime opencode-shell-async--runtimes)
    (unless (timerp (plist-get runtime :poll-timer))
      (setf (plist-get runtime :poll-timer)
            (run-at-time poll-interval poll-interval
                         #'opencode-shell-async--poll-runtime key)))
    (if sse-enabled
        (opencode-shell-async--connect key)
      (setf (plist-get runtime :connection) nil))
    runtime))

(defun opencode-shell-async-unsubscribe-runtime (key buffer)
  "Unsubscribe BUFFER and stop KEY runtime when it has no subscribers."
  (when-let ((runtime (gethash key opencode-shell-async--runtimes)))
    (remhash buffer (plist-get runtime :subscribers))
    (when (zerop (hash-table-count (plist-get runtime :subscribers)))
      (dolist (timer (list (plist-get runtime :poll-timer)
                           (plist-get runtime :reconnect-timer)))
        (when (timerp timer) (cancel-timer timer)))
      (opencode-shell-sse-stop (plist-get runtime :connection))
      (remhash key opencode-shell-async--runtimes))))

(defun opencode-shell-async--positive-finite-number-p (value)
  "Return non-nil when VALUE is a positive finite number."
  (and (numberp value)
       (> value 0)
       (not (string-match-p "\\(?:NaN\\|INF\\)" (format "%s" value)))))

(defun opencode-shell-async-drain (buffer)
  "Run the current latest-value queue for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp opencode-shell-async--idle-timer)
        (cancel-timer opencode-shell-async--idle-timer))
      (when (timerp opencode-shell-async--delivery-timer)
        (cancel-timer opencode-shell-async--delivery-timer))
      (setq opencode-shell-async--idle-timer nil
            opencode-shell-async--delivery-timer nil)
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
              (run-with-idle-timer 0 nil #'opencode-shell-async-drain buffer)))
      (unless (timerp opencode-shell-async--delivery-timer)
        (setq opencode-shell-async--delivery-timer
              (run-at-time opencode-shell-async--delivery-timeout nil
                           #'opencode-shell-async-drain buffer))))))

(defun opencode-shell-async-cancel (&optional buffer)
  "Cancel queued work for BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp opencode-shell-async--idle-timer)
      (cancel-timer opencode-shell-async--idle-timer))
    (when (timerp opencode-shell-async--delivery-timer)
      (cancel-timer opencode-shell-async--delivery-timer))
    (setq opencode-shell-async--idle-timer nil
          opencode-shell-async--delivery-timer nil
          opencode-shell-async--queue nil)))

(defun opencode-shell-async--animation-tick ()
  "Run animation callbacks only for live, visible subscribed buffers."
  (maphash
   (lambda (buffer subscription)
      (if (not (buffer-live-p buffer))
          (remhash buffer opencode-shell-async--animation-subscribers)
        (when (get-buffer-window buffer t)
          (with-current-buffer buffer (funcall (cdr subscription))))))
   opencode-shell-async--animation-subscribers)
  (when (zerop (hash-table-count opencode-shell-async--animation-subscribers))
    (when (timerp opencode-shell-async--animation-timer)
      (cancel-timer opencode-shell-async--animation-timer))
    (setq opencode-shell-async--animation-timer nil
          opencode-shell-async--animation-interval nil)))

(defun opencode-shell-async-subscribe-animation (buffer interval callback)
  "Subscribe visible BUFFER to the shared animation CALLBACK at INTERVAL."
  (unless (opencode-shell-async--positive-finite-number-p interval)
    (error "Animation interval must be a positive finite number"))
  (puthash buffer (cons interval callback)
           opencode-shell-async--animation-subscribers)
  (let (minimum)
    (maphash (lambda (_buffer subscription)
               (setq minimum (if minimum (min minimum (car subscription))
                               (car subscription))))
             opencode-shell-async--animation-subscribers)
    (when (and (timerp opencode-shell-async--animation-timer)
               (not (and (numberp opencode-shell-async--animation-interval)
                         (= minimum opencode-shell-async--animation-interval))))
      (cancel-timer opencode-shell-async--animation-timer)
      (setq opencode-shell-async--animation-timer nil))
    (setq opencode-shell-async--animation-interval minimum))
  (unless (timerp opencode-shell-async--animation-timer)
    (setq opencode-shell-async--animation-timer
          (run-at-time opencode-shell-async--animation-interval
                       opencode-shell-async--animation-interval
                       #'opencode-shell-async--animation-tick)))
  opencode-shell-async--animation-timer)

(defun opencode-shell-async-unsubscribe-animation (buffer)
  "Remove BUFFER from shared animation delivery."
  (remhash buffer opencode-shell-async--animation-subscribers)
  (if (zerop (hash-table-count opencode-shell-async--animation-subscribers))
      (progn
        (when (timerp opencode-shell-async--animation-timer)
          (cancel-timer opencode-shell-async--animation-timer))
        (setq opencode-shell-async--animation-timer nil
              opencode-shell-async--animation-interval nil))
    (let (minimum)
      (maphash (lambda (_buffer subscription)
                 (setq minimum (if minimum (min minimum (car subscription))
                                 (car subscription))))
               opencode-shell-async--animation-subscribers)
      (unless (= minimum opencode-shell-async--animation-interval)
        (when (timerp opencode-shell-async--animation-timer)
          (cancel-timer opencode-shell-async--animation-timer))
        (setq opencode-shell-async--animation-interval minimum
              opencode-shell-async--animation-timer
              (run-at-time minimum minimum
                           #'opencode-shell-async--animation-tick))))))

(defun opencode-shell-async-reset ()
  "Cancel queued work in package buffers and clear shared runtimes."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'opencode-shell-async--queue buffer)
      (opencode-shell-async-cancel buffer)))
  (clrhash opencode-shell-async--animation-subscribers)
  (when (timerp opencode-shell-async--animation-timer)
    (cancel-timer opencode-shell-async--animation-timer))
  (setq opencode-shell-async--animation-timer nil
        opencode-shell-async--animation-interval nil)
  (maphash
   (lambda (_key runtime)
     (dolist (timer (list (plist-get runtime :poll-timer)
                          (plist-get runtime :reconnect-timer)))
       (when (timerp timer) (cancel-timer timer)))
     (opencode-shell-sse-stop (plist-get runtime :connection)))
   opencode-shell-async--runtimes)
  (clrhash opencode-shell-async--runtimes))

(provide 'opencode-shell-async)
;;; opencode-shell-async.el ends here
