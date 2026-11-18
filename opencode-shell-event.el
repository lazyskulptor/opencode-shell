;;; opencode-shell-event.el --- OpenCode application event decoding -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Decode privacy-sensitive SSE data into the small application event surface
;; understood by the transcript runtime.  Unknown or malformed data is reduced
;; to a reconciliation hint; raw payloads never leave this module.

;;; Code:

(require 'json)
(require 'subr-x)

(defun opencode-shell-event--get (object key)
  "Return KEY from alist OBJECT, accepting symbol and string keys."
  (when (listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'equal))))

(defconst opencode-shell-event--ignored-prefixes
  '("server." "lsp." "file." "todo." "vcs." "pty." "mcp." "tui."
    "installation." "ide." "command.")
  "SSE event type prefixes unrelated to transcript, permission, or question state.")

(defun opencode-shell-event--ignored-type-p (type)
  "Return non-nil when event TYPE does not affect transcript state."
  (and (stringp type)
       (or (member type '("session.diff" "session.compacted"))
           (seq-some (lambda (prefix) (string-prefix-p prefix type))
                     opencode-shell-event--ignored-prefixes))))

(defun opencode-shell-event--snapshot (type reason &optional session-id resource)
  "Return a bounded snapshot hint for TYPE and REASON.
When SESSION-ID is non-nil, scope the hint to that session."
  (let ((resource
         (or resource
             (cond ((and (eq reason 'unsupported)
                         (opencode-shell-event--ignored-type-p type))
                    'none)
                   ((and type (string-prefix-p "message." type)) 'messages)
                   ((and type (string-prefix-p "permission." type)) 'permissions)
                   ((and type (string-prefix-p "question." type)) 'questions)
                   ((and type (string-prefix-p "session." type)) 'messages)
                   (t 'all)))))
    (list :kind 'snapshot :type type :reason reason :session-id session-id
          :resource resource)))

(defun opencode-shell-event-decode (event)
  "Decode transport EVENT into a validated application event.
Known OpenCode 1.18.30 message events retain only the fields needed for local
state application.  Unknown or malformed values become snapshot hints."
  (condition-case nil
      (let* ((data (plist-get event :data))
             (payload (and (stringp data)
                           (json-parse-string data :object-type 'alist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil)))
             (type (opencode-shell-event--get payload 'type))
             (properties (opencode-shell-event--get payload 'properties)))
        (unless (and (stringp type) (listp properties))
          (error "Malformed application event"))
        (cond
         ((equal type "message.updated")
          (let* ((info (opencode-shell-event--get properties 'info))
                 (session-id (opencode-shell-event--get info 'sessionID))
                 (message-id (opencode-shell-event--get info 'id)))
            (if (and (stringp session-id) (stringp message-id))
                (list :kind 'message-updated :type type
                      :session-id session-id :message-id message-id :info info)
              (opencode-shell-event--snapshot type 'malformed))))
         ((equal type "message.removed")
          (let ((session-id (opencode-shell-event--get properties 'sessionID))
                (message-id (opencode-shell-event--get properties 'messageID)))
            (if (and (stringp session-id) (stringp message-id))
                (list :kind 'message-removed :type type
                      :session-id session-id :message-id message-id)
              (opencode-shell-event--snapshot type 'malformed))))
         ((equal type "message.part.updated")
          (let* ((part (opencode-shell-event--get properties 'part))
                 (session-id (opencode-shell-event--get part 'sessionID))
                 (message-id (opencode-shell-event--get part 'messageID))
                 (part-id (opencode-shell-event--get part 'id)))
            (if (and (stringp session-id) (stringp message-id) (stringp part-id))
                (list :kind 'part-updated :type type
                      :session-id session-id :message-id message-id
                      :part-id part-id :part part
                      :delta (opencode-shell-event--get properties 'delta))
              (opencode-shell-event--snapshot type 'malformed))))
         ((equal type "message.part.delta")
          (let ((session-id (opencode-shell-event--get properties 'sessionID))
                (message-id (opencode-shell-event--get properties 'messageID))
                (part-id (opencode-shell-event--get properties 'partID))
                (field (opencode-shell-event--get properties 'field))
                (delta (opencode-shell-event--get properties 'delta)))
            (if (and (stringp session-id) (stringp message-id) (stringp part-id)
                     (stringp field) (stringp delta))
                (list :kind 'part-delta :type type
                      :session-id session-id :message-id message-id
                      :part-id part-id :field field :delta delta)
              (opencode-shell-event--snapshot type 'malformed))))
         ((equal type "message.part.removed")
          (let ((session-id (opencode-shell-event--get properties 'sessionID))
                (message-id (opencode-shell-event--get properties 'messageID))
                (part-id (opencode-shell-event--get properties 'partID)))
            (if (and (stringp session-id) (stringp message-id) (stringp part-id))
                (list :kind 'part-removed :type type
                      :session-id session-id :message-id message-id :part-id part-id)
              (opencode-shell-event--snapshot type 'malformed))))
         (t
          (let* ((info (opencode-shell-event--get properties 'info))
                 (session-id
                  (or (opencode-shell-event--get properties 'sessionID)
                      (opencode-shell-event--get info 'sessionID)
                      (opencode-shell-event--get
                       (opencode-shell-event--get properties 'part) 'sessionID)
                      (and (string-prefix-p "session." type)
                           (opencode-shell-event--get info 'id))))
                 (status (and (equal type "session.status")
                              (opencode-shell-event--get
                               (opencode-shell-event--get properties 'status) 'type)))
                 (busy (equal status "busy")))
            (let ((snapshot
                   (opencode-shell-event--snapshot
                    type 'unsupported (and (stringp session-id) session-id)
                    (and busy 'none))))
              (when (stringp status) (setq snapshot (plist-put snapshot :status status)))
              (if (and (equal type "session.updated") (listp info))
                  (plist-put snapshot :info info)
                snapshot))))))
    (error (opencode-shell-event--snapshot nil 'malformed))))

(defun opencode-shell-event-coalesce (previous incoming)
  "Combine PREVIOUS queued event with INCOMING when both target the same part."
  (let ((prev-kind (plist-get previous :kind))
        (next-kind (plist-get incoming :kind)))
    (cond
     ((and (eq prev-kind 'part-delta)
           (eq next-kind 'part-delta)
           (equal (plist-get previous :field) (plist-get incoming :field)))
      (let ((copy (copy-sequence incoming)))
        (plist-put copy :delta
                   (concat (plist-get previous :delta)
                           (plist-get incoming :delta)))))
     ((and (eq prev-kind 'part-updated)
           (eq next-kind 'part-delta))
      (let* ((field-name (plist-get incoming :field))
             (field-sym (and (stringp field-name) (intern field-name)))
             (part (copy-sequence (plist-get previous :part)))
             (existing (opencode-shell-event--get part field-sym)))
        (when (and field-sym (or (null existing) (stringp existing)))
          (setf (alist-get field-sym part)
                (concat (or existing "") (plist-get incoming :delta)))
          (let ((copy (copy-sequence previous)))
            (plist-put copy :part part)))))
     (t incoming))))

(defun opencode-shell-event-identity (event)
  "Return a stable delivery identity for decoded EVENT."
  (let ((kind (plist-get event :kind)))
    (cond
     ((memq kind '(part-updated part-removed part-delta))
      (list 'part (plist-get event :session-id)
            (plist-get event :message-id) (plist-get event :part-id)))
     ((memq kind '(message-updated message-removed))
      (list 'message (plist-get event :session-id)
            (plist-get event :message-id)))
     (t
      (list 'snapshot (plist-get event :session-id)
            (plist-get event :resource))))))

(provide 'opencode-shell-event)
;;; opencode-shell-event.el ends here
