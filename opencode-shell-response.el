;;; opencode-shell-response.el --- Assistant response normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure adapters from OpenCode assistant parts to payload-free response models.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defun opencode-shell-response--get (object key)
  "Return KEY from alist OBJECT with symbol or string keys."
  (when (listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal))))

(defun opencode-shell-response--replace-field (alist key value)
  "Return a shallow copy of ALIST with KEY set to VALUE."
  (let ((copy (mapcar (lambda (entry)
                        (if (consp entry) (cons (car entry) (cdr entry)) entry))
                      alist)))
    (setf (alist-get key copy) value)
    copy))

(defconst opencode-shell-response--bulk-state-keys
  '("input" "raw" "diff" "patch" "attachments")
  "Tool state fields never rendered and dropped from cached parts.")

(defun opencode-shell-response--compact-metadata (metadata)
  "Return METADATA reduced to short scalar entries.
Nested structures and long strings (e.g. a task's child `summary') are never
rendered; dropping them bounds memory and every later scan of the part."
  (if (not (listp metadata))
      metadata
    (seq-filter (lambda (entry)
                  (and (consp entry)
                       (let ((value (cdr entry)))
                         (or (numberp value) (memq value '(t nil))
                             (and (stringp value) (<= (length value) 200))))))
                metadata)))

(defun opencode-shell-response-compact-part (part)
  "Return PART with unrendered bulk tool and reasoning payloads compacted."
  (if (not (listp part))
      part
    (let ((type (format "%s" (or (opencode-shell-response--get part 'type) "")))
          (state (opencode-shell-response--get part 'state)))
      (cond
       ((equal type "reasoning")
        (let ((text (opencode-shell-response--get part 'text)))
          (if (and (stringp text) (> (length text) 256))
              (opencode-shell-response--replace-field
               part 'text (format "%d:%s" (length text)
                                  (substring text (- (length text) 64))))
            part)))
       ((and (listp state)
             (or (member type '("tool" "tool_use" "tool-result" "tool-invocation"))
                 (opencode-shell-response--get part 'tool)))
        (let* ((output (opencode-shell-response--get state 'output))
               (metadata (opencode-shell-response--get state 'metadata))
               (compact-metadata (opencode-shell-response--compact-metadata metadata))
               (compact-state
                (delq nil
                      (mapcar
                       (lambda (entry)
                         (when (consp entry)
                           (let ((k (format "%s" (car entry))))
                             (cond
                              ((member k opencode-shell-response--bulk-state-keys) nil)
                              ((and (equal k "output") (stringp output)
                                    (> (length output) 500))
                               (cons (car entry) (substring output 0 500)))
                              ((equal k "metadata") (cons (car entry) compact-metadata))
                              (t entry)))))
                       state))))
          (if (equal compact-state state)
              part
            (opencode-shell-response--replace-field part 'state compact-state))))
       (t part)))))

(defun opencode-shell-response-compact-envelope (envelope)
  "Return ENVELOPE with unrendered bulk payloads compacted in its parts."
  (if (not (listp envelope))
      envelope
    (let* ((parts (opencode-shell-response--get envelope 'parts))
           (compacted (mapcar #'opencode-shell-response-compact-part parts)))
      (if (cl-every #'eq parts compacted)
          envelope
        (opencode-shell-response--replace-field envelope 'parts compacted)))))

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

(defun opencode-shell-response-blocked-only-by-task-p (parts)
  "Return non-nil when the only unsettled tool parts in PARTS are `task' tools.
The task tool spawns a detached subagent and lingers `running' in the parent
even after the subagent ends, so when the session is idle such a part should no
longer block turn completion."
  (let ((running (seq-filter #'opencode-shell-response-running-tool-p parts)))
    (and running
         (seq-every-p
          (lambda (part)
            (equal (plist-get (opencode-shell-response-normalize-part part) :name)
                   "task"))
          running))))

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

(defun opencode-shell-response-tool-progress (parts)
  "Return ordered (NAME . STATUS) pairs from PARTS without tool payloads."
  (let (progress)
    (dolist (part parts (nreverse progress))
      (let* ((model (opencode-shell-response-normalize-part part))
             (name (plist-get model :name)))
        (when name
          (if-let ((known (assoc name progress)))
              (setcdr known (or (plist-get model :status) "pending"))
            (push (cons name (or (plist-get model :status) "pending"))
                  progress)))))))

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
