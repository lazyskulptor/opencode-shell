;;; opencode-shell-state.el --- Pure conversation lifecycle state -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Commentary:
;;
;; Buffer-independent conversation records and lifecycle predicates.  This file
;; intentionally has no package-module dependencies and no UI or I/O effects.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct (opencode-shell--turn (:constructor opencode-shell--make-turn))
  id server-user-id user assistant parts assistant-messages status acknowledged user-begin user-end
  response-begin response-end terminal-error locally-settled)

(defun opencode-shell-state-turn-terminal-p (status)
  "Return non-nil when STATUS is a terminal turn status."
  (eq status 'complete))

(defun opencode-shell-state-turn-active-p (status)
  "Return non-nil when STATUS represents unfinished turn work."
  (not (opencode-shell-state-turn-terminal-p status)))

(defun opencode-shell-state-request-phase (statuses submit-in-flight)
  "Derive aggregate request phase from turn STATUSES and SUBMIT-IN-FLIGHT.
The most specific active turn phase wins; local submission is sending when no
active server phase has been observed yet."
  (or (seq-find (lambda (status) (eq status 'thinking)) statuses)
      (seq-find (lambda (status) (eq status 'receiving)) statuses)
      (seq-find (lambda (status) (eq status 'recovering)) statuses)
      (seq-find (lambda (status) (eq status 'waiting)) statuses)
      (seq-find (lambda (status) (eq status 'error)) statuses)
      (seq-find (lambda (status) (eq status 'aborting)) statuses)
      (seq-find (lambda (status) (eq status 'sending)) statuses)
      (and submit-in-flight 'sending)
      'idle))

(defun opencode-shell-state-polling-needed-p (statuses submit-in-flight)
  "Return non-nil when unfinished STATUSES or SUBMIT-IN-FLIGHT require polling."
  (or submit-in-flight
      (seq-some #'opencode-shell-state-turn-active-p statuses)))

(defun opencode-shell-state-composer-ready-p
    (statuses submit-in-flight interaction-blocked-p hydration-complete-p)
  "Return non-nil when lifecycle inputs permit Composer editing.
STATUSES are authoritative turn statuses.  SUBMIT-IN-FLIGHT identifies a local
submission still being reconciled.  INTERACTION-BLOCKED-P represents permission
or question work, and HYDRATION-COMPLETE-P gates initial session snapshots."
  (and hydration-complete-p
       (not submit-in-flight)
       (not interaction-blocked-p)
       (not (seq-some #'opencode-shell-state-turn-active-p statuses))))

(defun opencode-shell-state-hydration-start (resources)
  "Return initial hydration state for authoritative RESOURCES."
  (list :pending (copy-sequence resources) :failed nil))

(defun opencode-shell-state-hydration-complete-p (state)
  "Return non-nil when STATE has no pending or failed resources."
  (and (null (plist-get state :pending))
       (null (plist-get state :failed))))

(defun opencode-shell-state-hydration-settle (state resource success)
  "Return STATE after RESOURCE settles with SUCCESS."
  (let ((pending (remove resource (plist-get state :pending)))
        (failed (remove resource (plist-get state :failed))))
    (unless success (setq failed (append failed (list resource))))
    (list :pending pending :failed failed)))

(defun opencode-shell-state-hydration-retry (state resources)
  "Return STATE with failed RESOURCES moved back to pending."
  (let ((pending (copy-sequence (plist-get state :pending)))
        (failed (copy-sequence (plist-get state :failed))))
    (dolist (resource resources)
      (when (memq resource failed)
        (setq failed (remove resource failed))
        (unless (memq resource pending)
          (setq pending (append pending (list resource))))))
    (list :pending pending :failed failed)))

(provide 'opencode-shell-state)
;;; opencode-shell-state.el ends here
