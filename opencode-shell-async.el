;;; opencode-shell-async.el --- Async scheduling for OpenCode Shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Latest-value idle scheduling and shared runtime storage.  Transport code may
;; enqueue state delivery here, but buffer rendering belongs to opencode-shell.

;;; Code:

(require 'cl-lib)

(defvar opencode-shell-async--runtimes (make-hash-table :test #'equal)
  "Shared runtime state keyed by server identity.")

(defvar opencode-shell-async--animation-subscribers
  (make-hash-table :test #'eq :weakness 'key)
  "Visible-buffer animation callbacks keyed by transcript buffer.")

(defvar opencode-shell-async--animation-timer nil
  "Single timer serving all visible transcript animations.")

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
  (clrhash opencode-shell-async--runtimes))

(provide 'opencode-shell-async)
;;; opencode-shell-async.el ends here
