;;; opencode-shell-interaction.el --- Human interaction lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Commentary:

;; Pure state transitions shared by permission and question requests.

;;; Code:

(require 'seq)

(defun opencode-shell-interaction-begin (state kind id)
  "Return STATE with KIND started for ID.
Signal `user-error' when KIND already has an active request."
  (when (assq kind state)
    (user-error "%s reply already in progress" (capitalize (symbol-name kind))))
  (append state (list (cons kind id))))

(defun opencode-shell-interaction-matches-p (state kind id)
  "Return non-nil when STATE records KIND as active for ID."
  (equal (alist-get kind state) id))

(defun opencode-shell-interaction-finish (state kind id)
  "Remove KIND from STATE only when its active request matches ID."
  (if (opencode-shell-interaction-matches-p state kind id)
      (assq-delete-all kind (copy-sequence state))
    state))

(defun opencode-shell-interaction-active-p (state &optional kind)
  "Return non-nil when STATE has an active request, optionally for KIND."
  (if kind (and (assq kind state) t) (and state t)))

(defun opencode-shell-interaction-remove-pending (pending id id-function)
  "Return PENDING without the item identified as ID by ID-FUNCTION.
Return the original list when no matching item exists."
  (if (seq-find (lambda (item) (equal id (funcall id-function item))) pending)
      (seq-remove (lambda (item) (equal id (funcall id-function item))) pending)
    pending))

(provide 'opencode-shell-interaction)
;;; opencode-shell-interaction.el ends here
