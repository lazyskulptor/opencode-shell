;;; opencode-shell.el --- Unofficial Emacs client for OpenCode -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes
;;; Commentary:

;; Unofficial Emacs client for sessions owned by an OpenCode HTTP server.

;;; Code:

(require 'url)
(require 'json)
(require 'tabulated-list)
(require 'subr-x)
(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'opencode-shell-render)

(defgroup opencode-shell nil "Unofficial Emacs client for OpenCode." :group 'tools)

(defcustom opencode-shell-base-url "http://127.0.0.1:4096"
  "OpenCode server base URL."
  :type 'string :group 'opencode-shell)

(defcustom opencode-shell-directory nil
  "Directory used to scope OpenCode requests, or nil."
  :type '(choice (const :tag "Unscoped" nil) directory)
  :group 'opencode-shell)

(defcustom opencode-shell-auth-function nil
  "Optional function returning an Authorization header value or nil.
The value is never included in client error messages."
  :type '(choice (const nil) function) :group 'opencode-shell)

(defcustom opencode-shell-auth-source-function #'opencode-shell--auth-source-resolve
  "Function called with a profile to obtain an Authorization value.
The returned value is used only while constructing one request and is never
stored in profile or server state.  Set this to nil to disable auth-source."
  :type '(choice (const nil) function) :group 'opencode-shell)

(defcustom opencode-shell-profiles nil
  "Named OpenCode connection profiles, represented as plists.
Supported keys include `:name', `:base-url', `:directory', `:workspace',
`:match', `:remote', auth-source lookup keys, and lifecycle keys."
  :type '(repeat plist) :group 'opencode-shell)

(defvar opencode-shell--servers (make-hash-table :test #'equal))
(defvar-local opencode-shell--profile nil)
(defvar-local opencode-shell--base-url nil)
(defvar-local opencode-shell--workspace nil)

(defconst opencode-shell--process-tail-limit 4096)

(defun opencode-shell--default-profile ()
  "Return the backwards-compatible implicit profile."
  (list :name "default" :base-url opencode-shell-base-url
        :directory opencode-shell-directory))

(defun opencode-shell--profile-name (profile)
  "Return a stable display name for PROFILE."
  (format "%s" (or (plist-get profile :name) "default")))

(defun opencode-shell--profile-key (profile)
  "Return a stable, non-secret identity key for PROFILE."
  (format "%s" (or (plist-get profile :id)
                    (concat (opencode-shell--profile-name profile) "|"
                            (string-remove-suffix
                             "/" (or (plist-get profile :base-url)
                                     opencode-shell-base-url))))))

(defun opencode-shell--validate-profiles ()
  "Signal a user error when configured profile names or identities collide."
  (let ((names (make-hash-table :test #'equal))
        (keys (make-hash-table :test #'equal)))
    (dolist (profile opencode-shell-profiles)
      (let ((name (opencode-shell--profile-name profile))
            (key (opencode-shell--profile-key profile)))
        (when (or (string-empty-p name) (gethash name names))
          (user-error "OpenCode profile names must be non-empty and unique: %s" name))
        (when (gethash key keys)
          (user-error "OpenCode profile identities must be unique: %s" key))
        (puthash name t names) (puthash key t keys)))))

(defun opencode-shell--profile-remote-p (profile)
  "Return non-nil when PROFILE represents a remote server."
  (or (plist-get profile :remote)
      (file-remote-p (or (plist-get profile :directory) ""))
      (let* ((url (url-generic-parse-url
                   (or (plist-get profile :base-url) opencode-shell-base-url)))
             (host (downcase (or (url-host url) ""))))
        (not (member host '("" "localhost" "127.0.0.1" "::1"))))))

(defun opencode-shell--canonical-directory (directory)
  "Return DIRECTORY in canonical directory form without remote I/O."
  (when directory
    (file-name-as-directory
     (if (file-remote-p directory) directory (expand-file-name directory)))))

(defun opencode-shell--profile-match-p (profile directory)
  "Return non-nil when PROFILE matches DIRECTORY."
  (let ((match (plist-get profile :match))
        (regexp (plist-get profile :match-regexp)))
    (cond ((functionp match) (funcall match directory))
          (regexp (string-match-p regexp directory))
          (t (let ((root (or match (plist-get profile :directory))))
               (and (stringp root)
                    (string-prefix-p (opencode-shell--canonical-directory root)
                                     (opencode-shell--canonical-directory directory))))))))

(defun opencode-shell--matching-profile (&optional directory)
  "Return the first profile matching DIRECTORY."
  (opencode-shell--validate-profiles)
  (let* ((directory (or directory default-directory))
         (matches (seq-filter (lambda (profile)
                                (opencode-shell--profile-match-p profile directory))
                              opencode-shell-profiles)))
    (car (sort matches
               (lambda (a b)
                 (> (length (format "%s" (or (plist-get a :match)
                                               (plist-get a :directory) "")))
                    (length (format "%s" (or (plist-get b :match)
                                               (plist-get b :directory) "")))))))))

(defun opencode-shell--read-profile ()
  "Read and return a configured profile."
  (unless opencode-shell-profiles (user-error "No OpenCode profiles configured"))
  (opencode-shell--validate-profiles)
  (let* ((table (mapcar (lambda (profile)
                          (cons (opencode-shell--profile-name profile) profile))
                        opencode-shell-profiles))
         (name (completing-read "OpenCode profile: " table nil t)))
    (cdr (assoc name table))))

(defun opencode-shell--resolve-profile (value)
  "Resolve profile VALUE, accepting a plist, name, or nil."
  (cond ((and (listp value) (plist-member value :base-url)) value)
        ((stringp value)
         (seq-find (lambda (p) (equal value (opencode-shell--profile-name p)))
                   opencode-shell-profiles))))

(defun opencode-shell--server-directory (directory profile)
  "Map Emacs DIRECTORY to the path understood by PROFILE's server."
  (when directory
    (let* ((native (or (file-remote-p directory 'localname) directory))
           (client-root (plist-get profile :directory))
           (root-native (and client-root
                             (or (file-remote-p client-root 'localname) client-root)))
           (workspace (plist-get profile :workspace)))
      (if (and workspace root-native
               (string-prefix-p (file-name-as-directory root-native)
                                (file-name-as-directory native)))
          (expand-file-name (file-relative-name native root-native) workspace)
        native))))

(defun opencode-shell--profile-directory (profile directory)
  "Return DIRECTORY in PROFILE server-native form."
  (opencode-shell--server-directory directory profile))

(defun opencode-shell--auth-source-resolve (profile)
  "Resolve PROFILE credentials through auth-source without retaining them."
  (require 'auth-source)
  (let ((source (or (plist-get profile :auth-source) profile)))
    (when-let* ((host (or (plist-get source :host)
                          (plist-get profile :auth-host)
                        (url-host (url-generic-parse-url
                                   (or (plist-get profile :base-url)
                                       opencode-shell-base-url)))))
              (entry (car (auth-source-search
                           :host host :user (or (plist-get source :user)
                                               (plist-get profile :auth-user))
                           :port (or (plist-get source :port)
                                    (plist-get profile :auth-port)) :max 1
                           :require '(:secret))))
              (secret (plist-get entry :secret))
              (value (if (functionp secret) (funcall secret) secret)))
      value)))

(defcustom opencode-shell-poll-interval 2
  "Seconds between transcript/status polls while a session buffer is live."
  :type 'number :group 'opencode-shell)

(defvar opencode-shell-prompt-history nil)
(defvar-local opencode-shell--sessions nil)
(defvar-local opencode-shell--session-status nil)
(defvar-local opencode-shell--directory nil)
(defvar-local opencode-shell--session-id nil)
(defvar-local opencode-shell--models nil)
(defvar-local opencode-shell--agents nil)
(defvar-local opencode-shell--selected-model nil)
(defvar-local opencode-shell--selected-agent nil)
(defvar-local opencode-shell--poll-timer nil)
(defvar-local opencode-shell--generation 0)
(defvar-local opencode-shell--in-flight nil)
(defvar-local opencode-shell--capabilities-loaded nil)
(defvar-local opencode-shell--capabilities-loading nil)
(defvar opencode-shell--generation-counter 0)
(defvar-local opencode-shell--filter "")

(defun opencode-shell--get (object key)
  "Get KEY from JSON OBJECT regardless of symbol/string representation."
  (or (map-elt object key)
      (and (symbolp key) (map-elt object (symbol-name key)))
      (and (stringp key) (map-elt object (intern key)))))

(defun opencode-shell--query (params)
  "Encode non-nil PARAMS as a query string."
  (mapconcat (lambda (pair)
               (concat (url-hexify-string (format "%s" (car pair))) "="
                       (url-hexify-string (format "%s" (cdr pair)))))
             (seq-filter #'cdr params) "&"))

(defun opencode-shell--url (path &optional params)
  "Build an API URL for PATH and PARAMS, adding directory scope."
  (let* ((query (opencode-shell--query
                 (append params
                         (and opencode-shell--directory
                              `((directory . ,opencode-shell--directory))))))
         (base (string-remove-suffix "/" (or opencode-shell--base-url
                                               (plist-get opencode-shell--profile :base-url)
                                               opencode-shell-base-url))))
    (concat base path (unless (string-empty-p query) (concat "?" query)))))

(defun opencode-shell--bounded-error (value)
  "Return VALUE as a bounded single-line error string."
  (truncate-string-to-width
   (replace-regexp-in-string "[\n\r\t ]+" " " (format "%s" value)) 300 nil nil t))

(defun opencode-shell--json-read-buffer ()
  "Read the HTTP response body at point as JSON."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "Malformed HTTP response"))
  (if (= (point) (point-max)) nil
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun opencode-shell--response-body ()
  "Return the response body without moving point."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\r?\n\r?\n" nil t)
        (buffer-substring-no-properties (point) (point-max)) "")))

(defun opencode-shell--request (method path callback &optional body params error-callback)
  "Send METHOD request to PATH and call CALLBACK with decoded JSON.
BODY is JSON encoded, PARAMS are query parameters, and ERROR-CALLBACK is
called after a transport, status, or decoding failure."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append '(("Accept" . "application/json"))
                  (and body '(("Content-Type" . "application/json")))
                  (when-let ((header
                              (opencode-shell--auth-header
                               (or opencode-shell--profile
                                   (opencode-shell--default-profile)))))
                    (list header))))
         (url-request-data (and body (encode-coding-string (json-serialize body) 'utf-8)))
         (origin (current-buffer)))
    (url-retrieve
     (opencode-shell--url path params)
     (lambda (status)
       (let ((response (current-buffer)))
         (unwind-protect
             (if-let ((err (plist-get status :error)))
                 (when (buffer-live-p origin)
                   (with-current-buffer origin
                     (message "OpenCode: %s" (opencode-shell--bounded-error err))
                     (when error-callback (funcall error-callback))))
               (condition-case err
                    (let ((code (or (bound-and-true-p url-http-response-status) 0)))
                      (if (not (<= 200 code 299))
                          (let ((response-body (opencode-shell--response-body)))
                            (when (buffer-live-p origin)
                              (with-current-buffer origin
                                (message "OpenCode: HTTP %s: %s" code
                                         (opencode-shell--bounded-error response-body))
                                (when error-callback (funcall error-callback)))))
                        (let ((value (unless (= code 204)
                                       (opencode-shell--json-read-buffer))))
                          (when (buffer-live-p origin)
                            (with-current-buffer origin (funcall callback value))))))
                 (error
                  (when (buffer-live-p origin)
                    (with-current-buffer origin
                      (message "OpenCode: %s" (opencode-shell--bounded-error
                                                (error-message-string err)))
                      (when error-callback (funcall error-callback)))))))
           (kill-buffer response))))
     nil t t)))

(defun opencode-shell--time (session)
  "Return SESSION update time as a sortable number."
  (let* ((time (opencode-shell--get session 'time))
         (updated (or (opencode-shell--get time 'updated)
                      (opencode-shell--get session 'updated))))
    (cond ((numberp updated) updated)
          ((stringp updated) (float-time (date-to-time updated)))
          (t 0))))

(defun opencode-shell--normalize-sessions (sessions)
  "Normalize and newest-first sort SESSIONS, deduplicating by ID."
  (let ((seen (make-hash-table :test #'equal)) result)
    (dolist (session (sort (copy-sequence (or sessions nil))
                           (lambda (a b) (> (opencode-shell--time a)
                                            (opencode-shell--time b)))))
      (let ((id (opencode-shell--get session 'id)))
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
    (let ((model-id (or (opencode-shell--get model 'modelID)
                        (opencode-shell--get model 'id))))
      (and model-id
           `((providerID . ,(or (opencode-shell--get model 'providerID) provider-id))
             (modelID . ,model-id)))))))

(defun opencode-shell--model-name (model &optional provider-id)
  "Return display name for normalized MODEL."
  (let* ((value (opencode-shell--model-value model provider-id))
         (provider (opencode-shell--get value 'providerID))
         (id (opencode-shell--get value 'modelID)))
    (if provider (format "%s/%s" provider id) (or id ""))))

(defun opencode-shell--status (id)
  "Return compact status for session ID."
  (let ((status (opencode-shell--get opencode-shell--session-status id)))
    (format "%s" (or (opencode-shell--get status 'type) status "idle"))))

(defun opencode-shell--session-text (session)
  "Return searchable text for SESSION."
  (concat
   (mapconcat #'identity
              (mapcar (lambda (key) (format "%s" (or (opencode-shell--get session key) "")))
                      '(title id directory project agent modelID providerID)) " ")
   " " (opencode-shell--model-name (opencode-shell--get session 'model)
                                      (opencode-shell--get session 'providerID))))

(defun opencode-shell--session-row (session)
  "Return a `tabulated-list-entries' row for SESSION."
  (let* ((id (opencode-shell--get session 'id))
         (model (or (and (opencode-shell--get session 'modelID)
                         (opencode-shell--model-name
                          (opencode-shell--get session 'modelID)
                          (opencode-shell--get session 'providerID)))
                    (opencode-shell--model-name (opencode-shell--get session 'model))))
         (updated (seconds-to-time (/ (opencode-shell--time session) 1000.0))))
    (list id (vector
              (or (opencode-shell--get session 'title) "Untitled")
              (truncate-string-to-width id 10 nil nil t)
              (or (opencode-shell--get session 'directory)
                  (opencode-shell--get session 'project) "")
              (or (opencode-shell--get session 'agent) "")
              (or model "") (opencode-shell--status id)
              (if (> (opencode-shell--time session) 0)
                  (format-time-string "%Y-%m-%d %H:%M" updated) "")))))

(defun opencode-shell--session-entries ()
  "Return filtered rows from the single normalized session collection."
  (mapcar #'opencode-shell--session-row
          (seq-filter
           (lambda (session)
             (or (string-empty-p opencode-shell--filter)
                 (string-match-p (regexp-quote (downcase opencode-shell--filter))
                                 (downcase (opencode-shell--session-text session)))))
           opencode-shell--sessions)))

(defvar opencode-shell-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'opencode-shell-refresh)
    (define-key map (kbd "RET") #'opencode-shell-open-at-point)
    (define-key map (kbd "c") #'opencode-shell-create-session)
    (define-key map (kbd "/") #'opencode-shell-filter)
    (define-key map (kbd "d") #'opencode-shell-delete-session)
    map))

(define-derived-mode opencode-shell-sessions-mode tabulated-list-mode "OpenCode Sessions"
  "Browse canonical OpenCode sessions."
  (setq tabulated-list-format
        [("Title" 28 t) ("ID" 10 t) ("Project" 28 t) ("Agent" 12 t)
         ("Model" 18 t) ("Status" 10 t) ("Updated" 16 t)])
  (setq tabulated-list-padding 2 tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'opencode-shell-refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun opencode-shell-sessions (&optional directory profile)
  "Open the session browser scoped to DIRECTORY and PROFILE.
For compatibility, DIRECTORY may itself be a profile plist or profile name."
  (interactive)
  (when-let ((as-profile (opencode-shell--resolve-profile directory)))
    (setq profile as-profile directory nil))
  (setq profile (or (opencode-shell--resolve-profile profile)
                    profile opencode-shell--profile
                    (opencode-shell--matching-profile (or directory default-directory))
                    (opencode-shell--default-profile)))
  (let ((buffer (get-buffer-create
                 (format "*OpenCode Shell Sessions:%s*"
                         (opencode-shell--profile-name profile)))))
    (with-current-buffer buffer
      (opencode-shell-sessions-mode)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--workspace (plist-get profile :workspace))
      (setq-local opencode-shell--directory
                  (opencode-shell--server-directory
                   (or directory (plist-get profile :directory)
                       opencode-shell-directory)
                   profile))
      (opencode-shell-refresh))
    (pop-to-buffer buffer)))

(defun opencode-shell-refresh ()
  "Refresh sessions and statuses, preserving point where possible."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (generation (cl-incf opencode-shell--generation)))
    (opencode-shell--request
     "GET" "/session/status"
     (lambda (statuses)
       (when (= generation opencode-shell--generation)
         (setq opencode-shell--session-status statuses)
         (opencode-shell--request
          "GET" "/session"
          (lambda (sessions)
            (when (= generation opencode-shell--generation)
              (setq opencode-shell--sessions (opencode-shell--normalize-sessions sessions)
                    tabulated-list-entries (opencode-shell--session-entries))
              (tabulated-list-print t)
              (when id (goto-char (point-min)) (search-forward id nil t))))))))))

(defun opencode-shell-filter (text)
  "Filter the session list by TEXT."
  (interactive (list (read-string "Filter sessions: " opencode-shell--filter)))
  (setq opencode-shell--filter text
        tabulated-list-entries (opencode-shell--session-entries))
  (tabulated-list-print t))

(defun opencode-shell-create-session (title)
  "Create a session named TITLE and open it."
  (interactive "sSession title: ")
  (let ((directory opencode-shell--directory)
        (profile opencode-shell--profile))
    (opencode-shell--request
     "POST" "/session"
     (lambda (session)
       (if profile
           (opencode-shell-open-session (opencode-shell--get session 'id) directory profile)
         (opencode-shell-open-session (opencode-shell--get session 'id) directory)))
     `((title . ,title)))))

(defun opencode-shell-open-at-point ()
  "Open the session at point."
  (interactive)
  (if-let ((id (tabulated-list-get-id)))
      (if opencode-shell--profile
          (opencode-shell-open-session id opencode-shell--directory opencode-shell--profile)
        (opencode-shell-open-session id opencode-shell--directory))
    (user-error "No session at point")))

(defun opencode-shell-delete-session ()
  "Delete the session at point after confirmation."
  (interactive)
  (let ((id (or (tabulated-list-get-id) (user-error "No session at point"))))
    (when (yes-or-no-p (format "Delete OpenCode session %s? " id))
      (opencode-shell--request "DELETE" (format "/session/%s" id)
                                (lambda (_) (opencode-shell-refresh))))))

(defun opencode-shell--normalize-models (response)
  "Return provider/model pairs normalized from provider RESPONSE."
  (mapcan
   (lambda (provider)
     (let ((provider-id (opencode-shell--get provider 'id)))
       (mapcar
        (lambda (entry)
          (let* ((key (and (consp entry) (atom (car entry)) (car entry)))
                 (model (if key (cdr entry) entry))
                 (value (or (opencode-shell--model-value model provider-id)
                            (and key (opencode-shell--model-value
                                      (format "%s" key) provider-id)))))
            (cons (opencode-shell--model-name value) value)))
        (opencode-shell--get provider 'models))))
   (or (opencode-shell--get response 'all)
       (opencode-shell--get response 'providers))))

(defun opencode-shell--normalize-agents (response)
  "Return name/agent pairs normalized from agent RESPONSE."
  (mapcar (lambda (agent)
            (let ((name (opencode-shell--get agent 'name))) (cons name agent)))
          response))

(defun opencode-shell--preserve-choice (choice choices)
  "Preserve CHOICE only when it still occurs in CHOICES."
  (and choice (seq-find (lambda (item) (equal (cdr item) choice)) choices) choice))

(defun opencode-shell--header ()
  "Return the current transcript header line."
  (format " OpenCode  %s  model:%s  agent:%s"
          (or opencode-shell--session-id "-")
          (or (car (rassoc opencode-shell--selected-model opencode-shell--models)) "server default")
          (or opencode-shell--selected-agent "server default")))

(defvar opencode-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'opencode-shell-resync)
    (define-key map (kbd "p") #'opencode-shell-prompt)
    (define-key map (kbd "a") #'opencode-shell-abort)
    (define-key map (kbd "m") #'opencode-shell-select-model)
    (define-key map (kbd "A") #'opencode-shell-select-agent)
    (define-key map (kbd "P") #'opencode-shell-permissions)
    (define-key map (kbd "Q") #'opencode-shell-questions)
    (define-key map (kbd "?") #'describe-mode)
    map))

(define-derived-mode opencode-shell-mode special-mode "OpenCode"
  "Read-only OpenCode transcript mode."
  (setq-local font-lock-defaults '(opencode-shell-render-font-lock-keywords t))
  (setq-local header-line-format '(:eval (opencode-shell--header)))
  (add-hook 'kill-buffer-hook #'opencode-shell--cleanup nil t))

(defun opencode-shell--cleanup ()
  "Cancel this buffer's timer and invalidate outstanding callbacks."
  (when (timerp opencode-shell--poll-timer) (cancel-timer opencode-shell--poll-timer))
  (setq opencode-shell--poll-timer nil)
  (setq opencode-shell--in-flight nil
        opencode-shell--capabilities-loading nil)
  (cl-incf opencode-shell--generation))

;;;###autoload
(defun opencode-shell-open-session (id &optional directory profile)
  "Open exact session ID scoped to DIRECTORY and PROFILE."
  (interactive "sSession ID: ")
  (let* ((explicit-profile (or profile opencode-shell--profile))
         (profile (or (opencode-shell--resolve-profile profile)
                      profile opencode-shell--profile
                      (opencode-shell--default-profile)))
         (buffer (get-buffer-create
                  (if explicit-profile
                      (format "*OpenCode Shell %s:%s*"
                              (opencode-shell--profile-name profile) id)
                    (format "*OpenCode Shell %s*" id)))))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-shell-mode) (opencode-shell--cleanup))
      (opencode-shell-mode)
      (setq-local opencode-shell--generation (cl-incf opencode-shell--generation-counter))
      (setq-local opencode-shell--session-id id)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--workspace (plist-get profile :workspace))
      (setq-local opencode-shell--directory
                  (opencode-shell--server-directory
                   (or directory (plist-get profile :directory)
                       opencode-shell-directory)
                   profile))
      (opencode-shell-resync t)
      (setq opencode-shell--poll-timer
            (run-at-time opencode-shell-poll-interval opencode-shell-poll-interval
                         (lambda (target)
                           (when (buffer-live-p target)
                             (with-current-buffer target (opencode-shell-resync))))
                         buffer)))
    (pop-to-buffer buffer)))

(defun opencode-shell--part-text (part)
  "Return display text for message PART."
  (pcase (opencode-shell--get part 'type)
    ("text" (or (opencode-shell--get part 'text) ""))
    ((or "tool" "tool_use" "tool-result")
     (format "[tool %s: %s]" (or (opencode-shell--get part 'tool) "")
             (or (opencode-shell--get (opencode-shell--get part 'state) 'status)
                 (opencode-shell--get part 'status) "pending")))
    (_ "")))

(defun opencode-shell--render-messages (messages)
  "Render chronological message envelopes from MESSAGES."
  (let ((inhibit-read-only t) (position (point)))
    (erase-buffer)
    (dolist (envelope messages)
      (let* ((info (opencode-shell--get envelope 'info))
             (role (or (opencode-shell--get info 'role) "message")))
        (insert (propertize (format "%s\n" (upcase role)) 'face 'bold))
        (dolist (part (opencode-shell--get envelope 'parts))
          (let ((text (opencode-shell--part-text part)))
            (unless (string-empty-p text)
              (opencode-shell-render-insert text) (insert "\n"))))
        (insert "\n")))
    (goto-char (min position (point-max)))))

(defun opencode-shell--guarded-request (key method path callback &optional body error-callback)
  "Request PATH once per generation under KEY."
  (unless (alist-get key opencode-shell--in-flight)
    (let ((generation opencode-shell--generation))
      (setf (alist-get key opencode-shell--in-flight) t)
      (opencode-shell--request
       method path
       (lambda (value)
         (when (= generation opencode-shell--generation)
           (setf (alist-get key opencode-shell--in-flight) nil)
           (funcall callback value)))
       body nil
       (lambda ()
         (when (= generation opencode-shell--generation)
           (setf (alist-get key opencode-shell--in-flight) nil)
           (when error-callback (funcall error-callback))))))))

(defun opencode-shell--question-prompt (question)
  "Return a readable answer prompt for QUESTION."
  (let* ((text (or (opencode-shell--get question 'question)
                   (opencode-shell--get question 'header) "Answer"))
         (options (opencode-shell--get question 'options)))
    (if options
        (format "%s [%s]: " text
                (mapconcat (lambda (option)
                             (format "%s" (or (opencode-shell--get option 'label)
                                                (opencode-shell--get option 'value)
                                                option)))
                           options ", "))
      (format "%s: " text))))

(defun opencode-shell--question-answer (question)
  "Read and return one string vector answer for QUESTION."
  (let* ((options (mapcar
                   (lambda (option)
                     (format "%s" (or (opencode-shell--get option 'label)
                                      (opencode-shell--get option 'value)
                                      option)))
                   (opencode-shell--get question 'options)))
         (prompt (opencode-shell--question-prompt question))
         (require-match (not (opencode-shell--get question 'custom))))
    (vconcat
     (if (opencode-shell--get question 'multiple)
         (completing-read-multiple prompt options nil require-match)
       (list (completing-read prompt options nil require-match))))))

(defun opencode-shell-resync (&optional capabilities)
  "Fully resync transcript, status, models, agents, and pending state."
  (interactive (list t))
  (opencode-shell--guarded-request
   'messages
    "GET" (format "/session/%s/message" opencode-shell--session-id)
    #'opencode-shell--render-messages)
  (when (and (or capabilities (not opencode-shell--capabilities-loaded))
             (not opencode-shell--capabilities-loading))
    (let ((remaining 2) failed)
      (setq opencode-shell--capabilities-loading t)
      (cl-labels ((settle (failure)
                    (setq failed (or failed failure)
                          remaining (1- remaining))
                    (when (zerop remaining)
                      (setq opencode-shell--capabilities-loading nil
                            opencode-shell--capabilities-loaded (not failed)))))
        (opencode-shell--guarded-request
         'providers "GET" "/provider"
         (lambda (response)
           (setq opencode-shell--models (opencode-shell--normalize-models response)
                 opencode-shell--selected-model
                 (opencode-shell--preserve-choice opencode-shell--selected-model
                                                    opencode-shell--models))
           (force-mode-line-update)
           (settle nil))
         nil (lambda () (settle t)))
        (opencode-shell--guarded-request
         'agents "GET" "/agent"
         (lambda (response)
           (setq opencode-shell--agents (opencode-shell--normalize-agents response))
           (unless (assoc opencode-shell--selected-agent opencode-shell--agents)
             (setq opencode-shell--selected-agent nil))
           (force-mode-line-update)
           (settle nil))
         nil (lambda () (settle t)))))))

(defun opencode-shell-select-model ()
  "Select a server-advertised model for subsequent prompts."
  (interactive)
  (unless opencode-shell--models (user-error "No models loaded; resync first"))
  (setq opencode-shell--selected-model
        (cdr (assoc (completing-read "Model: " opencode-shell--models nil t)
                    opencode-shell--models)))
  (force-mode-line-update))

(defun opencode-shell-select-agent ()
  "Select a server-advertised agent name for subsequent prompts."
  (interactive)
  (unless opencode-shell--agents (user-error "No agents loaded; resync first"))
  (setq opencode-shell--selected-agent
        (completing-read "Agent: " opencode-shell--agents nil t))
  (force-mode-line-update))

(defun opencode-shell--prompt-body (text)
  "Return the legacy prompt payload for TEXT and current selections."
  (append `((parts . [((type . "text") (text . ,text))]))
          (and opencode-shell--selected-model
               `((model . ,opencode-shell--selected-model)))
          (and opencode-shell--selected-agent
               `((agent . ,opencode-shell--selected-agent)))))

(defun opencode-shell-prompt (text)
  "Send TEXT asynchronously to the current session."
  (interactive (list (read-string "Prompt: " nil 'opencode-shell-prompt-history)))
  (unless (string-blank-p text)
    (opencode-shell--request
     "POST" (format "/session/%s/prompt_async" opencode-shell--session-id)
     (lambda (_) (opencode-shell-resync)) (opencode-shell--prompt-body text))))

(defun opencode-shell-abort ()
  "Abort work in the current session."
  (interactive)
  (opencode-shell--request "POST" (format "/session/%s/abort" opencode-shell--session-id)
                            (lambda (_) (opencode-shell-resync)) '()))

(defun opencode-shell--choose-pending (kind callback)
  "Fetch pending KIND and invoke CALLBACK with the selected object."
  (opencode-shell--request
   "GET" (concat "/" kind)
   (lambda (items)
     (unless items (user-error "No pending %s" kind))
     (let* ((table (mapcar (lambda (item)
                             (cons (format "%s: %s" (opencode-shell--get item 'id)
                                           (or (opencode-shell--get item 'title)
                                               (and (equal kind "permission")
                                                    (opencode-shell--permission-description item))
                                               (opencode-shell--get item 'permission) "pending"))
                                   item)) items))
            (choice (completing-read (format "%s: " (capitalize kind)) table nil t)))
       (funcall callback (cdr (assoc choice table)))))))

(defun opencode-shell--permission-value (value)
  "Return VALUE as bounded, single-line permission context."
  (truncate-string-to-width
   (replace-regexp-in-string
    "[\n\r\t ]+" " "
    (let ((print-length 8)
          (print-level 4)
          (print-escape-newlines t))
      (prin1-to-string value)))
   160 nil nil t))

(defun opencode-shell--permission-description (item)
  "Return concise, bounded user-facing context for permission ITEM."
  (string-join
   (delq nil
         (list (format "%s" (or (opencode-shell--get item 'permission) "permission"))
               (when-let ((patterns (opencode-shell--get item 'patterns)))
                 (format "patterns=%s" (opencode-shell--permission-value patterns)))
               (when-let ((always (opencode-shell--get item 'always)))
                 (format "always=%s" (opencode-shell--permission-value always)))
               (when-let ((tool (opencode-shell--get item 'tool)))
                 (format "tool=%s" (opencode-shell--permission-value tool)))
               (when-let ((metadata (opencode-shell--get item 'metadata)))
                 (format "metadata=%s" (opencode-shell--permission-value metadata)))))
   " | "))

(defun opencode-shell-permissions ()
  "Explicitly reply to a pending permission request."
  (interactive)
  (opencode-shell--choose-pending
   "permission"
   (lambda (item)
     (let* ((choices '(("Allow once" . "once")
                       ("Always allow" . "always")
                       ("Reject" . "reject")))
            (description (opencode-shell--permission-description item))
            (choice (completing-read
                     (format "%s: " description)
                     choices nil t))
            (reply (cdr (assoc choice choices))))
       (when (or (not (equal reply "always"))
                 (yes-or-no-p
                  (format "WARNING: Always allow persists for this session. %s? "
                          description)))
         (opencode-shell--request
          "POST" (format "/permission/%s/reply" (opencode-shell--get item 'id))
          (lambda (_) (message "Permission reply sent")) `((reply . ,reply))))))))

(defun opencode-shell-questions ()
  "Explicitly answer or reject a pending question."
  (interactive)
  (opencode-shell--choose-pending
   "question"
   (lambda (item)
     (let ((id (opencode-shell--get item 'id)))
       (if (yes-or-no-p "Answer this question? (No rejects) ")
            (let ((answers
                   (vconcat
                    (mapcar
                     #'opencode-shell--question-answer
                     (or (opencode-shell--get item 'questions) (list item))))))
              (opencode-shell--request
               "POST" (format "/question/%s/reply" id)
               (lambda (_) (message "Question reply sent")) `((answers . ,answers))))
         (opencode-shell--request
          "POST" (format "/question/%s/reject" id)
          (lambda (_) (message "Question rejected")) '()))))))

(defun opencode-shell--setup-evil ()
  "Install Evil integration when Evil is available."
  (declare-function evil-set-initial-state "evil-core")
  (declare-function evil-define-key* "evil-core")
  (evil-set-initial-state 'opencode-shell-mode 'normal)
  (evil-set-initial-state 'opencode-shell-sessions-mode 'normal)
  (evil-define-key* 'normal opencode-shell-mode-map
    (kbd "g r") #'opencode-shell-resync
    (kbd "RET") #'opencode-shell-prompt)
  (evil-define-key* 'normal opencode-shell-sessions-mode-map
    (kbd "RET") #'opencode-shell-open-at-point
    (kbd "g r") #'opencode-shell-refresh))

(defun opencode-shell--server-health-callback (profile callback status)
  "Handle a health response for PROFILE and report readiness to CALLBACK."
  (let ((response (current-buffer)))
    (unwind-protect
        (funcall callback
                 (and (not (plist-get status :error))
                      (<= 200 (or (bound-and-true-p url-http-response-status) 0) 299))
                 profile)
      (kill-buffer response))))

(defun opencode-shell--auth-header (profile)
  "Return PROFILE's authorization header pair, or nil."
  (when (or opencode-shell-auth-function opencode-shell-auth-source-function)
    (when-let ((token (if opencode-shell-auth-function
                          (funcall opencode-shell-auth-function)
                        (funcall opencode-shell-auth-source-function profile))))
      (cons (or (plist-get profile :auth-header) "Authorization") token))))

(defun opencode-shell--server-ready (profile callback)
  "Invoke CALLBACK with PROFILE when its health endpoint is ready."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (append '(("Accept" . "application/json"))
                 (when-let ((header (opencode-shell--auth-header profile)))
                   (list header)))))
    (url-retrieve
     (concat (string-remove-suffix "/" (or (plist-get profile :base-url)
                                             opencode-shell-base-url))
             (or (plist-get profile :health-path) "/health"))
     (lambda (status) (opencode-shell--server-health-callback profile callback status))
     nil t t)))

(defun opencode-shell--finish-start (key profile)
  "Finish coalesced server start KEY for PROFILE by invoking callbacks."
  (let* ((state (gethash key opencode-shell--servers))
         (callbacks (plist-get state :callbacks)))
    (setq state (plist-put state :callbacks nil)
          state (plist-put state :checking nil)
          state (plist-put state :starting nil))
    (if (plist-get state :owned)
        (puthash key state opencode-shell--servers)
      (remhash key opencode-shell--servers))
    (dolist (fn (nreverse callbacks)) (funcall fn profile))))

(defun opencode-shell--fail-start (key message-text)
  "Abandon start KEY and report MESSAGE-TEXT without process output."
  (let* ((state (gethash key opencode-shell--servers))
         (process (plist-get state :process)))
    (when (and (processp process) (process-live-p process))
      (delete-process process))
    (remhash key opencode-shell--servers)
    (message "OpenCode: %s" message-text)))

(defun opencode-shell--await-server (profile deadline)
  "Poll PROFILE health until DEADLINE."
  (let* ((key (opencode-shell--profile-key profile))
         (state (gethash key opencode-shell--servers)))
    (when (plist-get state :starting)
     (if (> (float-time) deadline)
        (progn
          (opencode-shell--fail-start key "server startup timed out"))
      (opencode-shell--server-ready
       profile
       (lambda (ready &optional _)
         (if ready
             (opencode-shell--finish-start key profile)
           (when (plist-get (gethash key opencode-shell--servers) :starting)
             (run-at-time .2 nil #'opencode-shell--await-server
                          profile deadline)))))))))

(defun opencode-shell--process-filter (process output)
  "Append OUTPUT to PROCESS buffer, retaining only a bounded tail."
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max)) (insert output)
        (when (> (buffer-size) opencode-shell--process-tail-limit)
          (delete-region (point-min)
                         (- (point-max) opencode-shell--process-tail-limit)))))))

(defun opencode-shell--process-sentinel (key process _event)
  "Handle an owned PROCESS exiting during startup for KEY."
  (when (and (memq (process-status process) '(exit signal failed))
             (eq process (plist-get (gethash key opencode-shell--servers) :process)))
    (if (plist-get (gethash key opencode-shell--servers) :starting)
        (opencode-shell--fail-start
         key (format "server exited before becoming healthy (status %s)"
                     (process-exit-status process)))
      (remhash key opencode-shell--servers))))

(defun opencode-shell--spawn-server (profile)
  "Start PROFILE exactly once and begin bounded health polling."
  (let* ((key (opencode-shell--profile-key profile))
         (state (gethash key opencode-shell--servers))
         (command (plist-get profile :start-command)))
    (unless (and (listp command) command (seq-every-p #'stringp command))
      (remhash key opencode-shell--servers)
      (user-error ":start-command must be a non-empty argv list"))
    (condition-case err
        (let* ((default-directory (or (plist-get profile :server-directory)
                                      default-directory))
               (process (make-process
                         :name (format "opencode-%s" key)
                         :buffer (get-buffer-create (format " *opencode-%s*" key))
                         :command command :noquery t :connection-type 'pipe
                         :filter #'opencode-shell--process-filter
                         :sentinel (lambda (process event)
                                     (opencode-shell--process-sentinel key process event)))))
          (puthash key (append (list :process process :owned t :starting t)
                               state)
                   opencode-shell--servers)
          (if (process-live-p process)
              (opencode-shell--await-server
               profile (+ (float-time) (or (plist-get profile :startup-timeout) 10)))
            (opencode-shell--fail-start key "server exited during startup")))
      (error (opencode-shell--fail-start
              key (format "could not start server: %s" (error-message-string err)))))))

(defun opencode-shell-start-server (&optional profile callback)
  "Start local PROFILE server and invoke CALLBACK when healthy.
Concurrent starts for one profile are coalesced.  Remote profiles are never
auto-started."
  (interactive)
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
         (key (opencode-shell--profile-key profile))
         (state (gethash key opencode-shell--servers))
         (command (plist-get profile :start-command)))
    (cond
     ((opencode-shell--profile-remote-p profile)
      (when (called-interactively-p 'interactive) (user-error "Remote profiles are not auto-started")))
     ((or (plist-get state :checking) (plist-get state :starting))
      (when callback
        (puthash key (plist-put state :callbacks
                                (cons callback (plist-get state :callbacks)))
                 opencode-shell--servers)))
     ((not command)
      (user-error "Profile has no :start-command"))
     (t
      (puthash key (list :checking t :callbacks (and callback (list callback)))
               opencode-shell--servers)
      (opencode-shell--server-ready
       profile (lambda (ready &optional _)
                 (when (plist-get (gethash key opencode-shell--servers) :checking)
                   (if ready
                       (opencode-shell--finish-start key profile)
                     (opencode-shell--spawn-server profile)))))))))

(defun opencode-shell-stop-server (&optional profile)
  "Stop PROFILE server only when this client owns its process."
  (interactive)
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
         (key (opencode-shell--profile-key profile))
         (state (gethash key opencode-shell--servers))
         (process (plist-get state :process)))
    (unless (and (plist-get state :owned) (process-live-p process))
      (user-error "OpenCode server is not owned by this client"))
    (delete-process process)
    (remhash key opencode-shell--servers)))

(defun opencode-shell-restart-server (&optional profile)
  "Restart an owned local PROFILE server."
  (interactive)
  (let ((profile (or profile opencode-shell--profile (opencode-shell--read-profile))))
    (opencode-shell-stop-server profile)
    (opencode-shell-start-server profile)))

(defun opencode-shell-stop-all-servers ()
  "Stop all live server processes owned by this package."
  (let (owned)
    (maphash (lambda (_key state)
               (when (plist-get state :owned)
                 (push (plist-get state :process) owned)))
             opencode-shell--servers)
    (clrhash opencode-shell--servers)
    (dolist (process owned)
      (when (process-live-p process) (delete-process process)))))

(add-hook 'kill-emacs-hook #'opencode-shell-stop-all-servers)

;;;###autoload
(defun opencode-shell-launch (&optional choose-profile)
  "Launch the matching profile, or select one with prefix CHOOSE-PROFILE."
  (interactive "P")
  (let ((profile (if choose-profile (opencode-shell--read-profile)
                   (or (opencode-shell--matching-profile)
                       (and opencode-shell-profiles (opencode-shell--read-profile))
                       (opencode-shell--default-profile)))))
    (if (and (plist-get profile :start-command)
             (not (opencode-shell--profile-remote-p profile)))
        (opencode-shell-start-server profile
                                      (lambda (ready) (opencode-shell-sessions nil ready)))
      (opencode-shell-sessions nil profile))))

;;;###autoload
(defun opencode-shell-open-profile (profile &optional directory)
  "Open PROFILE's session browser, optionally scoped to DIRECTORY."
  (interactive (list (opencode-shell--read-profile) nil))
  (opencode-shell-sessions directory profile))

(with-eval-after-load 'evil (opencode-shell--setup-evil))

(provide 'opencode-shell)
;;; opencode-shell.el ends here
