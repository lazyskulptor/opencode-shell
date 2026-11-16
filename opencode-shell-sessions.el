;;; opencode-shell-sessions.el --- Session/model display normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure, buffer-independent normalization for OpenCode session and model
;; records: update ordering, searchable text, and providerID/modelID display.
;; No buffer state, no I/O.  The session browser UI stays in `opencode-shell'.

;;; Code:

(require 'cl-lib)
(require 'map)

(defun opencode-shell-sessions--get (object key)
  "Get KEY from JSON OBJECT regardless of symbol/string representation."
  (or (map-elt object key)
      (and (symbolp key) (map-elt object (symbol-name key)))
      (and (stringp key) (map-elt object (intern key)))))

(defun opencode-shell--time (session)
  "Return SESSION update time as a sortable number."
  (let* ((time (opencode-shell-sessions--get session 'time))
         (updated (or (opencode-shell-sessions--get time 'updated)
                      (opencode-shell-sessions--get session 'updated))))
    (cond ((numberp updated) updated)
          ((stringp updated) (float-time (date-to-time updated)))
          (t 0))))

(defun opencode-shell--normalize-sessions (sessions)
  "Normalize and newest-first sort SESSIONS, deduplicating by ID.
Each retained session keeps its server-reported directory unchanged."
  (let ((seen (make-hash-table :test #'equal)) result)
    (dolist (session (sort (copy-sequence (or sessions nil))
                           (lambda (a b) (> (opencode-shell--time a)
                                            (opencode-shell--time b)))))
      (let ((id (opencode-shell-sessions--get session 'id)))
        (when (and id (not (gethash id seen)))
          (puthash id t seen)
          (push session result))))
    (nreverse result)))

(defun opencode-shell--model-value (model &optional provider-id)
  "Normalize MODEL to a providerID/modelID object."
  (cond
   ((stringp model)
    (let ((parts (split-string model "/")))
      `((providerID . ,(or provider-id (and (> (length parts) 1) (car parts))))
        (modelID . ,(car (last parts))))))
   ((listp model)
    (let ((model-id (or (opencode-shell-sessions--get model 'modelID)
                        (opencode-shell-sessions--get model 'id))))
      (and model-id
           `((providerID . ,(or (opencode-shell-sessions--get model 'providerID)
                                provider-id))
             (modelID . ,model-id)))))))

(defun opencode-shell--model-name (model &optional provider-id)
  "Return display name for normalized MODEL."
  (let* ((value (opencode-shell--model-value model provider-id))
         (provider (opencode-shell-sessions--get value 'providerID))
         (id (opencode-shell-sessions--get value 'modelID)))
    (if provider (format "%s/%s" provider id) (or id ""))))

(defun opencode-shell--session-text (session)
  "Return searchable text for SESSION."
  (concat
   (mapconcat #'identity
              (mapcar (lambda (key)
                        (format "%s" (or (opencode-shell-sessions--get session key) "")))
                      '(title id directory project agent modelID providerID)) " ")
   " " (opencode-shell--model-name (opencode-shell-sessions--get session 'model)
                                    (opencode-shell-sessions--get session 'providerID))))

(provide 'opencode-shell-sessions)
;;; opencode-shell-sessions.el ends here
