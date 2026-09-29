;;; opencode-shell-recovery.el --- Bounded SSH retry policy -*- lexical-binding: t; -*-

;;; Commentary:
;; Server-keyed retry decisions.  The caller owns SSH processes and HTTP.

;;; Code:

(require 'cl-lib)

(defvar opencode-shell-recovery--states (make-hash-table :test #'equal))
(defconst opencode-shell-recovery--max-failures 5)
(defconst opencode-shell-recovery--attempt-timeout 10)

(defun opencode-shell-recovery--state (key)
  "Return the retry state for server KEY."
  (or (gethash key opencode-shell-recovery--states)
      (let ((state (list :offline nil :ready nil :epoch 0
                         :exhausted nil :failures 0
                         :retry-timer nil :retry-token nil
                         :deadline nil :attempt nil)))
        (puthash key state opencode-shell-recovery--states)
        state)))

(defun opencode-shell-recovery-offline-p (key)
  "Return whether KEY's SSH transport is offline."
  (plist-get (gethash key opencode-shell-recovery--states) :offline))

(defun opencode-shell-recovery-ready-p (key)
  "Return whether KEY has a verified, current SSH forwarding path."
  (plist-get (gethash key opencode-shell-recovery--states) :ready))

(defun opencode-shell-recovery-epoch (key)
  "Return KEY's current transport generation."
  (or (plist-get (gethash key opencode-shell-recovery--states) :epoch) 0))

(defun opencode-shell-recovery-exhausted-p (key)
  "Return whether KEY exhausted automatic retry attempts."
  (plist-get (gethash key opencode-shell-recovery--states) :exhausted))

(defun opencode-shell-recovery-mark-offline (key)
  "Mark KEY offline, returning non-nil only for a new outage."
  (let* ((state (opencode-shell-recovery--state key))
         (new (not (plist-get state :offline))))
    (setf (plist-get state :offline) t
          (plist-get state :ready) nil)
    (when new (cl-incf (plist-get state :epoch)))
    new))

(defun opencode-shell-recovery--clear-timers (state)
  "Cancel timers belonging to STATE."
  (dolist (timer (list (plist-get state :retry-timer)
                       (plist-get state :deadline)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (plist-get state :retry-timer) nil
        (plist-get state :retry-token) nil
         (plist-get state :deadline) nil
         (plist-get state :attempt) nil))

(defun opencode-shell-recovery--schedule (key state retry needed)
  "Schedule KEY's next bounded attempt using its existing failure count."
  (let ((token (make-symbol "ssh-retry")))
    (setf (plist-get state :retry-token) token
          (plist-get state :retry-timer)
          (run-at-time
           (min 30 (expt 2 (1- (plist-get state :failures)))) nil
           (lambda ()
             (when (eq token (plist-get (gethash key opencode-shell-recovery--states)
                                        :retry-token))
               (setf (plist-get state :retry-token) nil
                     (plist-get state :retry-timer) nil)
               (if (funcall needed) (funcall retry)
                 (opencode-shell-recovery-cancel key))))))))

(defun opencode-shell-recovery-failed (key retry needed)
  "Record KEY failure; schedule RETRY only while NEEDED remains true."
  (let ((state (opencode-shell-recovery--state key)))
    (unless (or (plist-get state :exhausted)
                (plist-get state :retry-token))
      (when (timerp (plist-get state :deadline))
        (cancel-timer (plist-get state :deadline)))
      (setf (plist-get state :deadline) nil
            (plist-get state :attempt) nil
            (plist-get state :offline) t
            (plist-get state :ready) nil)
      (let ((failures (1+ (plist-get state :failures))))
        (setf (plist-get state :failures) failures)
        (if (>= failures opencode-shell-recovery--max-failures)
            (setf (plist-get state :exhausted) t)
          (opencode-shell-recovery--schedule key state retry needed))))))

(defun opencode-shell-recovery-resume (key retry needed)
  "Resume KEY's paused retry timer for new demand without resetting its budget."
  (when-let ((state (gethash key opencode-shell-recovery--states)))
    (when (and (plist-get state :offline)
               (not (plist-get state :exhausted))
               (not (plist-get state :retry-token))
               (not (plist-get state :attempt))
               (> (plist-get state :failures) 0))
      (opencode-shell-recovery--schedule key state retry needed))))

(defun opencode-shell-recovery-watch-attempt (key expired)
  "Call EXPIRED if KEY's current asynchronous attempt never settles."
  (let* ((state (opencode-shell-recovery--state key))
         (token (make-symbol "ssh-attempt")))
    (when (timerp (plist-get state :deadline))
      (cancel-timer (plist-get state :deadline)))
    (setf (plist-get state :attempt) token
          (plist-get state :deadline)
          (run-at-time
           opencode-shell-recovery--attempt-timeout nil
           (lambda ()
             (when (eq token (plist-get (gethash key opencode-shell-recovery--states)
                                        :attempt))
               (setf (plist-get state :attempt) nil
                     (plist-get state :deadline) nil)
               (funcall expired)))))
    token))

(defun opencode-shell-recovery-success (key)
  "Reset KEY's recovery state after verified transport health."
  (let ((state (opencode-shell-recovery--state key)))
    (when (or (plist-get state :offline)
              (not (plist-get state :ready)))
      (cl-incf (plist-get state :epoch)))
    (opencode-shell-recovery--clear-timers state)
    (setf (plist-get state :offline) nil
          (plist-get state :ready) t
          (plist-get state :exhausted) nil
          (plist-get state :failures) 0)))

(defun opencode-shell-recovery-manual-reset (key)
  "Allow a new, explicitly requested attempt for KEY."
  (let ((state (opencode-shell-recovery--state key)))
    (cl-incf (plist-get state :epoch))
    (opencode-shell-recovery--clear-timers state)
    (setf (plist-get state :offline) t
          (plist-get state :ready) nil
          (plist-get state :exhausted) nil
          (plist-get state :failures) 0)))

(defun opencode-shell-recovery-cancel (key)
  "Forget KEY and cancel its remaining recovery timers."
  (when-let ((state (gethash key opencode-shell-recovery--states)))
    (opencode-shell-recovery--clear-timers state)
    (remhash key opencode-shell-recovery--states)))

(defun opencode-shell-recovery-suspend (key)
  "Stop KEY's retry timers without losing its offline/exhausted state."
  (when-let ((state (gethash key opencode-shell-recovery--states)))
    (opencode-shell-recovery--clear-timers state)))

(provide 'opencode-shell-recovery)
;;; opencode-shell-recovery.el ends here
