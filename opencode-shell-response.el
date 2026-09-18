;;; opencode-shell-response.el --- Assistant response normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Commentary:

;; Pure adapters from OpenCode assistant parts to payload-free response models.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun opencode-shell-response--get (object key)
  "Return KEY from alist OBJECT with symbol or string keys."
  (when (listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal))))

(defun opencode-shell-response-normalize-part (part)
  "Return a canonical, payload-free model for assistant PART."
  (let* ((raw-type (format "%s" (or (opencode-shell-response--get part 'type)
                                     "unknown")))
         (kind (cond ((member raw-type '("tool" "tool_use" "tool-result")) 'tool)
                     ((equal raw-type "text") 'text)
                     ((equal raw-type "reasoning") 'reasoning)
                     ((equal raw-type "step-finish") 'step-finish)
                     (t 'unknown)))
         (state (opencode-shell-response--get part 'state)))
    (list :kind kind
          :raw-type raw-type
          :name (and (eq kind 'tool)
                     (let ((name (or (opencode-shell-response--get part 'tool)
                                     (opencode-shell-response--get part 'name))))
                       (and name (format "%s" name))))
          :status (and (eq kind 'tool)
                       (let ((status
                              (or (opencode-shell-response--get state 'status)
                                  (opencode-shell-response--get part 'status))))
                         (and status (format "%s" status))))
          :text (and (memq kind '(text reasoning))
                     (or (opencode-shell-response--get part 'text) "")))))

(defun opencode-shell-response-part-display-text (part)
  "Return payload-free display text for PART."
  (let* ((model (opencode-shell-response-normalize-part part))
         (kind (plist-get model :kind)))
    (pcase kind
      ('text (plist-get model :text))
      ('tool (format "[tool %s: %s]"
                     (or (plist-get model :name) "")
                     (or (plist-get model :status) "pending")))
      (_ ""))))

(defun opencode-shell-response-running-tool-p (part)
  "Return non-nil when PART is an unsettled tool invocation."
  (let ((model (opencode-shell-response-normalize-part part)))
    (and (eq (plist-get model :kind) 'tool)
         (not (member (plist-get model :status) '("completed" "error"))))))

(defun opencode-shell-response-terminal-part-p (part)
  "Return non-nil when PART is terminal step evidence."
  (eq (plist-get (opencode-shell-response-normalize-part part) :kind)
      'step-finish))

(defun opencode-shell-response-part-state-label (part)
  "Return a payload-free type and status label for PART."
  (let* ((model (opencode-shell-response-normalize-part part))
         (raw-type (plist-get model :raw-type)))
    (if (eq (plist-get model :kind) 'tool)
        (format "%s:%s" raw-type (or (plist-get model :status) "unknown"))
      raw-type)))

(defun opencode-shell-response-phase (parts)
  "Return the nonterminal response phase represented by PARTS."
  (let ((models (mapcar #'opencode-shell-response-normalize-part parts)))
    (cond ((seq-some (lambda (model)
                       (memq (plist-get model :kind) '(text tool)))
                     models)
           'receiving)
          ((seq-some (lambda (model)
                       (eq (plist-get model :kind) 'reasoning))
                     models)
           'thinking)
          (t 'waiting))))

(defun opencode-shell-response-tool-names (parts)
  "Return tool names from PARTS in first-observed order without duplicates."
  (let (names)
    (dolist (part parts (nreverse names))
      (when-let ((name (plist-get (opencode-shell-response-normalize-part part)
                                  :name)))
        (unless (member name names) (push name names))))))

(defun opencode-shell-response-envelope-complete-p (info parts)
  "Return non-nil when INFO and PARTS contain authoritative completion evidence."
  (let ((finish (opencode-shell-response--get info 'finish)))
    (and (not (seq-some #'opencode-shell-response-running-tool-p parts))
         (not (equal (format "%s" finish) "tool-calls"))
         (or (and (opencode-shell-response--get
                   (opencode-shell-response--get info 'time) 'completed)
                  (or finish (opencode-shell-response--get info 'error)))
             (and (null finish)
                  (seq-some #'opencode-shell-response-terminal-part-p parts))))))

(provide 'opencode-shell-response)
;;; opencode-shell-response.el ends here
