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

(provide 'opencode-shell-state)
;;; opencode-shell-state.el ends here
