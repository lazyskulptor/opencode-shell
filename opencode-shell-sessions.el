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

(defun opencode-shell--child-session-p (session)
  "Return non-nil when SESSION belongs to a parent session."
  (let ((parent (or (opencode-shell-sessions--get session 'parentID)
                    (opencode-shell-sessions--get session 'parentId))))
    (and parent (not (equal parent "")))))

(defvar opencode-shell-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'opencode-shell--refresh)
    (define-key map (kbd "RET") #'opencode-shell--open-at-point)
    (define-key map (kbd "c") #'opencode-shell--create-session)
    (define-key map (kbd "/") #'opencode-shell--filter)
    (define-key map (kbd "T") #'opencode-shell--toggle-child-sessions)
    (define-key map (kbd "d") #'opencode-shell--delete-session)
    (define-key map (kbd "?") #'opencode-shell-sessions-help)
    map))

(defun opencode-shell-sessions-help ()
  "Display the session browser transient menu."
  (interactive)
  (require 'transient)
  (transient-setup 'opencode-shell-sessions-menu))

(declare-function transient-setup "transient")
(defvar opencode-shell-sessions-menu nil)

(require 'transient)
(transient-define-prefix opencode-shell-sessions-menu ()
    "OpenCode session actions."
    [["Session"
      ("RET" "Open" opencode-shell--open-at-point)
      ("c" "Create here" opencode-shell--create-session)
      ("d" "Delete" opencode-shell--delete-session)]
     ["List"
      ("g" "Refresh" opencode-shell--refresh)
      ("/" "Filter" opencode-shell--filter)
      ("T" "Toggle child sessions" opencode-shell--toggle-child-sessions)]
     ["Global"
      ("s" "Start (select profile)" opencode-shell-start)
       ("l" "Sessions (select profile)" opencode-shell)
       ("b" "Shell buffers" opencode-shell-switch-buffer)
       ("f" "Find saved session" opencode-shell-find-session)
       ("F" "Find session (server)" opencode-shell-find-session-with-server)]])

(define-derived-mode opencode-shell-sessions-mode tabulated-list-mode "OpenCode Sessions"
  "Browse canonical OpenCode sessions."
  (setq tabulated-list-format
        [("Title" 28 t) ("ID" 10 t) ("Agent" 12 t)
         ("Model" 18 t) ("Status" 10 t) ("Updated" 16 t)])
  (setq-local header-line-format nil)
  (setq tabulated-list-padding 2 tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'opencode-shell--refresh nil t)
  (add-hook 'window-configuration-change-hook
            #'opencode-shell--render-session-browser-if-visible nil t)
  (add-hook 'kill-buffer-hook #'opencode-shell-async-cancel nil t)
  (tabulated-list-init-header))

(defun opencode-shell--setup-sessions-evil-buffer ()
  "Install session browser bindings in the current Evil buffer."
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "g") nil)
    (dolist (binding `((,(kbd "RET") . opencode-shell--open-at-point)
                       (,(kbd "g r") . opencode-shell--refresh)
                       (,(kbd "c") . opencode-shell--create-session)
                       (,(kbd "/") . opencode-shell--filter)
                       (,(kbd "T") . opencode-shell--toggle-child-sessions)
                       (,(kbd "d") . opencode-shell--delete-session)
                       (,(kbd "?") . opencode-shell-sessions-help)))
      (evil-local-set-key 'normal (car binding) (cdr binding)))))

(add-hook 'opencode-shell-sessions-mode-hook
          #'opencode-shell--setup-sessions-evil-buffer)
(defcustom opencode-shell-recent-locations-file
  (locate-user-emacs-file "opencode-shell-locations.eld")
  "File used to persist recently opened session browser locations."
  :type 'file :group 'opencode-shell)

(defcustom opencode-shell-session-directory-overrides-file
  (locate-user-emacs-file "opencode-shell-session-directories.eld")
  "File used to persist client-side session directory moves."
  :type 'file :group 'opencode-shell)

(defun opencode-shell--load-recent-session-locations ()
  "Read persisted session browser locations, returning nil on failure."
  (condition-case nil
      (when (file-readable-p opencode-shell-recent-locations-file)
        (with-temp-buffer
          (insert-file-contents opencode-shell-recent-locations-file)
          (let ((value (read (current-buffer))))
            (and (listp value) value))))
    (error nil)))

(defun opencode-shell--save-recent-session-locations ()
  "Persist recently opened session browser locations."
  (make-directory (file-name-directory opencode-shell-recent-locations-file) t)
  (with-temp-file opencode-shell-recent-locations-file
    (let ((print-length nil) (print-level nil))
      (prin1 opencode-shell--recent-session-locations (current-buffer))
      (insert "\n"))))

(defvar opencode-shell--recent-session-locations
  (opencode-shell--load-recent-session-locations)
  "Persisted session browser locations opened by OpenCode Shell.")

(defun opencode-shell--load-session-directory-overrides ()
  "Read persisted session directory overrides, returning nil on failure."
  (condition-case nil
      (when (file-readable-p opencode-shell-session-directory-overrides-file)
        (with-temp-buffer
          (insert-file-contents opencode-shell-session-directory-overrides-file)
          (let ((value (read (current-buffer))))
            (and (listp value) value))))
    (error nil)))

(defun opencode-shell--save-session-directory-overrides ()
  "Persist client-side session directory overrides."
  (make-directory (file-name-directory
                   opencode-shell-session-directory-overrides-file) t)
  (with-temp-file opencode-shell-session-directory-overrides-file
    (let ((print-length nil) (print-level nil))
      (prin1 opencode-shell--session-directory-overrides (current-buffer))
      (insert "\n"))))

(defvar opencode-shell--session-directory-overrides
  (opencode-shell--load-session-directory-overrides)
  "Persisted directory overrides keyed by profile identity and session ID.")
(provide 'opencode-shell-sessions)
;;; opencode-shell-sessions.el ends here
