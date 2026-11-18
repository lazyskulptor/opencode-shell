;;; opencode-shell-messages.el --- Pure message merge primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure, buffer-independent primitives for merging incoming assistant message
;; envelopes and their parts into already-known ones.  These operate only on
;; parsed JSON alists; they hold no buffer state and perform no I/O.  The
;; stateful message cache and turn reconciliation live in `opencode-shell'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(defun opencode-shell-messages--get (object key)
  "Get KEY from JSON OBJECT regardless of symbol/string representation."
  (or (map-elt object key)
      (and (symbolp key) (map-elt object (symbol-name key)))
      (and (stringp key) (map-elt object (intern key)))))

(defun opencode-shell--part-id (part)
  "Return PART's stable identity, when available."
  (or (opencode-shell-messages--get part 'id)
      (opencode-shell-messages--get part 'callID)
      (opencode-shell-messages--get part 'callId)))

(defun opencode-shell--copy-alist-spine (alist)
  "Return a shallow copy of ALIST and its top-level cons cells."
  (mapcar (lambda (entry)
            (if (consp entry) (cons (car entry) (cdr entry)) entry))
          alist))

(defun opencode-shell--merge-alist (known incoming)
  "Return KNOWN alist with fields present in INCOMING replaced."
  (let ((result (opencode-shell--copy-alist-spine known)))
    (dolist (field incoming result)
      (setf (alist-get (car field) result) (cdr field)))))

(defun opencode-shell--merge-parts (known incoming)
  "Merge INCOMING message parts into KNOWN without dropping omitted parts."
  (if (null incoming)
      (copy-sequence known)
    (let* ((result (copy-sequence known))
           (tail (last result))
           (cells-by-id (make-hash-table :test #'equal)))
      (let ((cursor result))
        (while cursor
          (when-let* ((id (opencode-shell--part-id (car cursor))))
            (puthash id cursor cells-by-id))
          (setq cursor (cdr cursor))))
      (dolist (part incoming result)
        (let* ((id (opencode-shell--part-id part))
               (cell (and id (gethash id cells-by-id))))
          (if cell
              (setcar cell (opencode-shell--merge-alist (car cell) part))
            (let ((new-cell (list part)))
              (if tail
                  (progn
                    (setcdr tail new-cell)
                    (setq tail new-cell))
                (setq result new-cell
                      tail new-cell))
              (when id
                (puthash id new-cell cells-by-id)))))))))

(defun opencode-shell--merge-envelope (known incoming)
  "Merge partial assistant envelope INCOMING into KNOWN."
  (let* ((merged (opencode-shell--copy-alist-spine incoming))
         (known-info (opencode-shell-messages--get known 'info))
         (incoming-info (opencode-shell-messages--get incoming 'info))
         (info (if (eq known-info incoming-info)
                   known-info
                 (opencode-shell--merge-alist known-info incoming-info))))
    (when (and (not (eq known-info incoming-info))
               (or (assq 'time known-info) (assq 'time incoming-info)))
      (setf (alist-get 'time info)
            (opencode-shell--merge-alist
             (opencode-shell-messages--get known-info 'time)
             (opencode-shell-messages--get incoming-info 'time))))
    (setf (alist-get 'info merged) info)
    (setf (alist-get 'parts merged)
          (opencode-shell--merge-parts
           (opencode-shell-messages--get known 'parts)
           (opencode-shell-messages--get incoming 'parts)))
    merged))

(defun opencode-shell--message-envelope-id (envelope)
  "Return ENVELOPE's stable message ID."
  (opencode-shell-messages--get (opencode-shell-messages--get envelope 'info) 'id))

(provide 'opencode-shell-messages)
;;; opencode-shell-messages.el ends here
