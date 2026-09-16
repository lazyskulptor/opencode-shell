;;; opencode-shell.el --- Unofficial Emacs client for OpenCode -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;; Keywords: tools, processes
;;; Commentary:

;; Unofficial Emacs client for sessions owned by an OpenCode HTTP server.
;; Network and process work must remain asynchronous; callbacks reconcile state
;; and visible UI updates are coalesced at idle time.  See
;; docs/async-runtime.md for the runtime invariants.

;;; Code:

(require 'url)
(require 'json)
(require 'tabulated-list)
(require 'subr-x)
(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'project)
(require 'opencode-shell-render)
(require 'opencode-shell-async)

(defgroup opencode-shell nil "Unofficial Emacs client for OpenCode." :group 'tools)

(defcustom opencode-shell-base-url "http://127.0.0.1:4199"
  "OpenCode server base URL."
  :type 'string :group 'opencode-shell)

(defcustom opencode-shell-directory nil
  "Directory used to scope OpenCode requests, or nil."
  :type '(choice (const :tag "Unscoped" nil) directory)
  :group 'opencode-shell)

(defcustom opencode-shell-recent-locations-file
  (locate-user-emacs-file "opencode-shell-locations.eld")
  "File used to persist recently opened session browser locations."
  :type 'file :group 'opencode-shell)

(defcustom opencode-shell-session-directory-overrides-file
  (locate-user-emacs-file "opencode-shell-session-directories.eld")
  "File used to persist client-side session directory moves."
  :type 'file :group 'opencode-shell)

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
`:match', `:remote', auth-source lookup keys,
and lifecycle keys."
  :type '(repeat plist) :group 'opencode-shell)

(defvar opencode-shell--servers (make-hash-table :test #'equal))
(defvar opencode-shell--generated-profile-commands nil)
(defconst opencode-shell--mode-commands
  '(opencode-shell--refresh opencode-shell--filter
    opencode-shell--create-session opencode-shell--open-at-point
    opencode-shell--delete-session opencode-shell--resync
    opencode-shell--select-model opencode-shell--select-agent
    opencode-shell--submit opencode-shell--abort opencode-shell--permissions
    opencode-shell--permission-allow-once
    opencode-shell--permission-allow-always
    opencode-shell--permission-reject opencode-shell--questions)
  "Private interactive commands used only by OpenCode mode maps.")
(defvar-local opencode-shell--profile nil)
(defvar-local opencode-shell--base-url nil)
(defvar-local opencode-shell--workspace nil)

(defconst opencode-shell--process-tail-limit 4096)

(defvar projectile-mode)
(declare-function projectile-project-root "projectile")

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

(defun opencode-shell--server-base-url (profile)
  "Return PROFILE's canonical server base URL."
  (let* ((url (url-generic-parse-url
               (or (plist-get profile :base-url) opencode-shell-base-url)))
         (scheme (downcase (or (url-type url) "http")))
         (host (downcase (or (url-host url) "")))
         (host (if (member host '("localhost" "127.0.0.1" "::1" "[::1]"))
                   "localhost" host))
         (port (url-port url))
         (port (unless (or (and (equal scheme "http") (equal port 80))
                           (and (equal scheme "https") (equal port 443)))
                 port))
         (path (or (url-filename url) "")))
    (format "%s://%s%s%s" scheme host (if port (format ":%s" port) "")
            (if (equal path "/") "" (string-remove-suffix "/" path)))))

(defun opencode-shell--server-key (profile)
  "Return the lifecycle identity for PROFILE's server."
  (opencode-shell--server-base-url profile))

(defun opencode-shell--server-lifecycle-config (profile)
  "Return normalized, non-secret lifecycle configuration for PROFILE."
  (list :start-command (copy-sequence (plist-get profile :start-command))
        :health-path (or (plist-get profile :health-path) "/health")
        :server-directory
        (opencode-shell--canonical-directory (plist-get profile :server-directory))
        :startup-timeout (or (plist-get profile :startup-timeout) 10)
        :stop-on-exit (if (plist-member profile :stop-on-exit)
                          (and (plist-get profile :stop-on-exit) t) t)
        :health-auth (cons (or (plist-get profile :auth-header) "Authorization")
                           (opencode-shell--auth-selectors profile))))

(defun opencode-shell--auth-selectors (profile)
  "Return PROFILE's effective auth-source selectors."
  (let ((source (or (plist-get profile :auth-source) profile)))
    (list :host (or (plist-get source :host)
                    (plist-get profile :auth-host)
                    (url-host (url-generic-parse-url
                               (or (plist-get profile :base-url)
                                   opencode-shell-base-url))))
          :port (or (plist-get source :port) (plist-get profile :auth-port))
          :user (or (plist-get source :user) (plist-get profile :auth-user)))))

(defun opencode-shell--validate-server-profile (profile &optional existing)
  "Validate PROFILE lifecycle compatibility with EXISTING state."
  (let* ((key (opencode-shell--server-key profile))
         (config (opencode-shell--server-lifecycle-config profile))
         (other (plist-get existing :config)))
    (when (and other (not (equal config other)))
      (user-error "OpenCode profiles sharing %s have incompatible server lifecycle configuration" key))
    config))

(defun opencode-shell--validate-profiles ()
  "Signal a user error when configured profile names or identities collide."
  (let ((names (make-hash-table :test #'equal))
        (keys (make-hash-table :test #'equal)))
    (let ((servers (make-hash-table :test #'equal)))
      (dolist (profile opencode-shell-profiles)
      (let ((name (opencode-shell--profile-name profile))
             (key (opencode-shell--profile-key profile))
             (server-key (opencode-shell--server-key profile)))
        (when (or (string-empty-p name) (gethash name names))
          (user-error "OpenCode profile names must be non-empty and unique: %s" name))
        (when (gethash key keys)
          (user-error "OpenCode profile identities must be unique: %s" key))
        (when-let ((config (gethash server-key servers)))
          (opencode-shell--validate-server-profile profile (list :config config)))
        (puthash server-key (opencode-shell--server-lifecycle-config profile) servers)
        (puthash name t names) (puthash key t keys))))))

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

(defun opencode-shell--resolve-or-read-profile (value)
  "Resolve VALUE or prompt for an OpenCode server profile."
  (or (opencode-shell--resolve-profile value) value
      (opencode-shell--read-profile)))

(defun opencode-shell--profile-command-segment (name)
  "Return a safe command segment for profile NAME."
  (let ((segment (downcase (replace-regexp-in-string "[ _]+" "-" name))))
    (unless (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" segment)
      (user-error "OpenCode profile name cannot form a command: %s" name))
    segment))

(defun opencode-shell--generated-command (name action)
  "Return an interactive command that applies ACTION to profile NAME."
  (lambda ()
    (interactive)
    (funcall action
             (or (opencode-shell--resolve-profile name)
                 (user-error "Unknown OpenCode server alias: %s" name)))))

(defun opencode-shell--project-directory (&optional directory)
  "Return the project root for DIRECTORY, falling back to DIRECTORY itself."
  (let* ((default-directory
          (opencode-shell--canonical-directory (or directory default-directory)))
         (root
          (or (when (and (boundp 'projectile-mode) projectile-mode
                         (fboundp 'projectile-project-root))
                (ignore-errors (projectile-project-root)))
              (when-let ((project (ignore-errors (project-current nil))))
                (ignore-errors (project-root project)))
              (locate-dominating-file default-directory ".git")
              default-directory)))
    (opencode-shell--canonical-directory root)))

(defun opencode-shell--current-server-directory (profile)
  "Return current `default-directory' as an absolute PROFILE server path."
  (let* ((directory (opencode-shell--project-directory))
         (client-root (plist-get profile :directory))
         (workspace (plist-get profile :workspace))
         (native (expand-file-name
                  (or (file-remote-p directory 'localname) directory)))
         (root-native (and client-root
                           (expand-file-name
                            (or (file-remote-p client-root 'localname)
                                client-root))))
         (mapped (or (null workspace) (null root-native)
                     (string-prefix-p (file-name-as-directory root-native)
                                      (file-name-as-directory native))
                     (string-prefix-p (file-name-as-directory
                                            (expand-file-name workspace))
                                      (file-name-as-directory native))))
         (server-directory (and mapped
                                (opencode-shell--server-directory directory profile))))
    (unless (and (stringp server-directory)
                 (file-name-absolute-p server-directory))
      (user-error "Current directory cannot be mapped to the OpenCode server"))
    (file-name-as-directory server-directory)))

(defun opencode-shell--create-and-open-session (profile directory)
  "Create and open a title-less PROFILE session in DIRECTORY."
  (let ((buffer (generate-new-buffer " *opencode-create-session*")))
    (with-current-buffer buffer
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--directory directory)
      (opencode-shell--request
       "POST" "/session"
       (lambda (session)
         (kill-buffer buffer)
         (opencode-shell-open-session
          (opencode-shell--get session 'id) directory profile))))))

(defun opencode-shell--start-session (profile &optional directory)
  "Ensure PROFILE readiness and create a session in DIRECTORY or the current path."
  (let ((directory (or directory (opencode-shell--current-server-directory profile))))
    (if (and (plist-get profile :start-command)
             (not (opencode-shell--profile-remote-p profile)))
        (opencode-shell--start-server
         profile
         (lambda (ready)
           (opencode-shell--create-and-open-session ready directory)))
      (opencode-shell--create-and-open-session profile directory))))

(defun opencode-shell--register-profile-commands ()
  "Refresh session and start commands for configured server aliases."
  (let ((seen (make-hash-table :test #'equal)) definitions)
    (dolist (profile opencode-shell-profiles)
      (let* ((name (opencode-shell--profile-name profile))
             (segment (opencode-shell--profile-command-segment name))
              (symbols (list (intern (format "opencode-shell-%s-sessions" segment))
                             (intern (format "opencode-shell-%s-start" segment)))))
        (dolist (symbol symbols)
          (when (gethash symbol seen)
            (user-error "OpenCode profile command collision: %s" symbol))
          (when (and (fboundp symbol)
                     (not (memq symbol opencode-shell--generated-profile-commands)))
            (user-error "OpenCode profile command already exists: %s" symbol))
          (puthash symbol t seen))
        (push (list symbols name) definitions)))
    (mapc (lambda (symbol) (when (fboundp symbol) (fmakunbound symbol)))
          opencode-shell--generated-profile-commands)
    (setq opencode-shell--generated-profile-commands nil)
    (dolist (definition (nreverse definitions))
      (pcase-let ((`((,sessions-symbol ,start-symbol) ,name) definition))
        (defalias sessions-symbol
          (opencode-shell--generated-command
            name (lambda (profile)
                   (opencode-shell--open-sessions
                    profile (opencode-shell--current-server-directory profile))))
          (format "Open the %s OpenCode session browser." name))
        (defalias start-symbol
          (opencode-shell--generated-command name #'opencode-shell--start-session)
          (format "Create a %s OpenCode session in the current directory." name))
        (push sessions-symbol opencode-shell--generated-profile-commands)
        (push start-symbol opencode-shell--generated-profile-commands)))
    (setq opencode-shell--generated-profile-commands
          (nreverse opencode-shell--generated-profile-commands))))

(defun opencode-shell--server-directory (directory profile)
  "Map Emacs DIRECTORY to the path understood by PROFILE's server."
  (when directory
    (let* ((native (expand-file-name
                    (or (file-remote-p directory 'localname) directory)))
            (client-root (plist-get profile :directory))
            (root-native (and client-root
                              (expand-file-name
                               (or (file-remote-p client-root 'localname)
                                   client-root))))
            (workspace (and-let* ((path (plist-get profile :workspace)))
                         (expand-file-name path))))
      (cond ((and workspace
                  (string-prefix-p (file-name-as-directory workspace)
                                   (file-name-as-directory native)))
             native)
            ((and workspace root-native
                  (string-prefix-p (file-name-as-directory root-native)
                                   (file-name-as-directory native)))
             (expand-file-name (file-relative-name native root-native) workspace))
            (t native)))))

(defun opencode-shell--client-directory (directory profile)
  "Map server-native DIRECTORY to the path understood by Emacs for PROFILE."
  (let* ((server-directory (expand-file-name (or directory default-directory)))
         (workspace (and-let* ((path (plist-get profile :workspace)))
                      (file-name-as-directory (expand-file-name path))))
         (client-root (and-let* ((path (plist-get profile :directory)))
                        (file-name-as-directory (expand-file-name path)))))
    (file-name-as-directory
     (if (and workspace client-root
              (string-prefix-p workspace
                               (file-name-as-directory server-directory)))
         (expand-file-name (file-relative-name server-directory workspace)
                           client-root)
       server-directory))))

(defun opencode-shell--profile-directory (profile directory)
  "Return DIRECTORY in PROFILE server-native form."
  (opencode-shell--server-directory directory profile))

(defun opencode-shell--buffer-scope (profile directory)
  "Return a stable buffer scope for PROFILE and server-native DIRECTORY."
  (format "%s:%s" (opencode-shell--profile-key profile) (or directory "")))

(defun opencode-shell--directory-leaf (directory)
  "Return a concise display leaf for DIRECTORY."
  (let* ((path (or directory default-directory "/"))
         (trimmed (string-remove-suffix "/" (file-name-as-directory path)))
         (leaf (file-name-nondirectory trimmed)))
    (if (string-empty-p leaf) "root" leaf)))

(defun opencode-shell--sessions-buffer (profile directory)
  "Return the live browser for PROFILE and DIRECTORY, when present."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'opencode-shell-sessions-mode)
            (equal (opencode-shell--profile-key opencode-shell--profile)
                   (opencode-shell--profile-key profile))
            (equal opencode-shell--directory directory))))
   (buffer-list)))

(defun opencode-shell--transcript-buffer (profile directory session-id)
  "Return the live transcript matching PROFILE, DIRECTORY, and SESSION-ID."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'opencode-shell-mode)
            (equal opencode-shell--session-id session-id)
            (equal (opencode-shell--profile-key opencode-shell--profile)
                   (opencode-shell--profile-key profile))
            (equal opencode-shell--directory directory))))
   (buffer-list)))

(defun opencode-shell--auth-source-resolve (profile)
  "Resolve PROFILE credentials through auth-source without retaining them."
  (require 'auth-source)
  (let ((selectors (opencode-shell--auth-selectors profile)))
    (when-let* ((host (plist-get selectors :host))
               (entry (car (auth-source-search
                            :host host :user (plist-get selectors :user)
                            :port (plist-get selectors :port) :max 1
                            :require '(:secret))))
              (secret (plist-get entry :secret))
              (value (if (functionp secret) (funcall secret) secret)))
      value)))

(defcustom opencode-shell-poll-interval 2
  "Seconds between transcript/status polls while a session buffer is live."
  :type 'number :group 'opencode-shell)

(defcustom opencode-shell-animation-interval 0.2
  "Seconds between UI-only request-status animation frames."
  :type 'number :group 'opencode-shell)

(defconst opencode-shell--spinner-character ?▰
  "Character appended by each transient-status animation tick.")

(defcustom opencode-shell-log-requests t
  "When non-nil, log API results without payloads or secrets."
  :type 'boolean :group 'opencode-shell)

(defcustom opencode-shell-log-buffer-name "*OpenCode Shell Log*"
  "Base name for API request log buffers."
  :type 'string :group 'opencode-shell)

(defvar opencode-shell--request-log-counter 0)
(defvar opencode-shell--session-id)
(defvar opencode-shell--profile)

(defun opencode-shell--log-buffer-name ()
  "Return the log buffer name for the current session or global requests."
  (if opencode-shell--session-id
      (format "%s-log" (buffer-name))
    opencode-shell-log-buffer-name))

(defun opencode-shell--log (format-string &rest args)
  "Append a timestamped API log line formatted with FORMAT-STRING and ARGS."
  (when opencode-shell-log-requests
    (with-current-buffer (get-buffer-create (opencode-shell--log-buffer-name))
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
                (apply #'format format-string args) "\n")))))

(defun opencode-shell-log ()
  "Display the OpenCode Shell API log buffer."
  (interactive)
  (let ((buffer (get-buffer-create (opencode-shell--log-buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode)))
    (pop-to-buffer buffer)))

(defvar opencode-shell--composer-start)
(defvar opencode-shell--transcript-end)

(defvar-local opencode-shell--sessions nil)
(defvar-local opencode-shell--browser-dirty nil)
(defvar-local opencode-shell--show-child-sessions nil)
(defvar-local opencode-shell--session-status nil)
(defvar-local opencode-shell--directory nil)
(defvar-local opencode-shell--session-id nil)
(defvar-local opencode-shell--models nil)
(defvar-local opencode-shell--agents nil)
(defvar-local opencode-shell--selected-model nil)
(defvar-local opencode-shell--selected-agent nil)
(defvar-local opencode-shell--poll-timer nil)
(defvar-local opencode-shell--runtime-key nil)
(defvar-local opencode-shell--animation-timer nil)
(defvar-local opencode-shell--animation-frame 0)
(defvar-local opencode-shell--render-dirty nil)
(defvar-local opencode-shell--render-event nil)
(defvar-local opencode-shell--generation 0)
(defvar-local opencode-shell--in-flight nil)
(defvar-local opencode-shell--capabilities-loaded nil)
(defvar-local opencode-shell--capabilities-loading nil)
(defvar-local opencode-shell--turns nil)
(defvar-local opencode-shell--rendered-turns nil)
(defvar-local opencode-shell--turn-counter 0)
(defvar-local opencode-shell--transcript-end nil)
(defvar-local opencode-shell--composer-start nil)
(defvar-local opencode-shell--request-status "idle")
(defvar-local opencode-shell--message-request-sequence 0)
(defvar-local opencode-shell--message-applied-sequence 0)
(defvar-local opencode-shell--poll-heartbeat 0)
(defvar-local opencode-shell--submit-in-flight nil)
(defvar-local opencode-shell--composer-visible t)
(defvar-local opencode-shell--composer-label-visible t)
(defvar-local opencode-shell--idle-completion-count 0)
(defvar-local opencode-shell--permissions nil)
(defvar-local opencode-shell--permission-begin nil)
(defvar-local opencode-shell--permission-end nil)
(defvar-local opencode-shell--permission-status-begin nil)
(defvar-local opencode-shell--permission-status-end nil)
(defvar-local opencode-shell--permission-sending nil)
(defvar-local opencode-shell--resolved-permissions nil)
(defvar-local opencode-shell--permission-refresh-pending nil)
(defvar-local opencode-shell--questions-pending nil)
(defvar-local opencode-shell--question-sending nil)
(defvar-local opencode-shell--question-refresh-pending nil)
(defvar-local opencode-shell--last-lifecycle-signature nil)
(defvar-local opencode-shell--table-render-width nil)
(defvar-local opencode-shell--table-rerendering nil)
(defvar opencode-shell--generation-counter 0)
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
(defvar-local opencode-shell--filter "")

(defface opencode-shell-user-face
  '((((class color) (background dark)) :foreground "#dca3a3" :weight bold)
    (((class color) (background light)) :foreground "#8b2252" :weight bold)
    (t :inherit font-lock-keyword-face :weight bold))
  "Restrained face for user labels." :group 'opencode-shell)
(defface opencode-shell-assistant-face
  '((((class color) (background dark)) :foreground "#8cd0d3" :weight bold)
    (((class color) (background light)) :foreground "#00688b" :weight bold)
    (t :inherit font-lock-function-name-face :weight bold))
  "Restrained face for assistant labels." :group 'opencode-shell)
(defface opencode-shell-waiting-face '((t :inherit shadow :slant italic))
  "Face for a turn awaiting a response." :group 'opencode-shell)
(defface opencode-shell-error-face '((t :inherit error))
  "Face for conversation transport errors." :group 'opencode-shell)
(defface opencode-shell-permission-face
  '((t :inherit warning :weight bold))
  "Face for pending permission requests." :group 'opencode-shell)

(cl-defstruct (opencode-shell--turn (:constructor opencode-shell--make-turn))
  id server-user-id user assistant parts assistant-messages status acknowledged user-begin user-end
  response-begin response-end terminal-error locally-settled)

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
                              (not (assoc 'directory params))
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

(defun opencode-shell--request (method path callback &optional body params error-callback)
  "Send METHOD request to PATH and call CALLBACK with decoded JSON.
BODY is JSON encoded, PARAMS are query parameters, and ERROR-CALLBACK is
called after a transport, status, or decoding failure."
  (let* ((url-proxy-services
          (if (opencode-shell--profile-remote-p
               (or opencode-shell--profile (opencode-shell--default-profile)))
              url-proxy-services
            nil))
         (url-request-method method)
         (url-request-extra-headers
          (append '(("Accept" . "application/json"))
                  (and body '(("Content-Type" . "application/json")))
                  (when-let ((header
                              (opencode-shell--auth-header
                               (or opencode-shell--profile
                                   (opencode-shell--default-profile)))))
                    (list header))))
         (url-request-data (and body (encode-coding-string (json-serialize body) 'utf-8)))
         (origin (current-buffer))
         (request-id (cl-incf opencode-shell--request-log-counter))
         (started (float-time))
         (poll-p (string-match-p "/session/[^/]+/message\\'" path)))
    (when (and opencode-shell-log-requests (not poll-p))
      (opencode-shell--log "OpenCode API #%d → %s %s" request-id method path))
    (url-retrieve
     (opencode-shell--url path params)
     (lambda (status)
       (let ((response (current-buffer)))
         (unwind-protect
              (if-let ((err (plist-get status :error)))
                  (when (buffer-live-p origin)
                    (with-current-buffer origin
                      (when opencode-shell-log-requests
                         (opencode-shell--log "OpenCode API #%d ← transport-error %.2fs [%s %s]"
                                             request-id (- (float-time) started) method path))
                       (message "OpenCode: %s" (opencode-shell--bounded-error err))
                       (when error-callback
                         (opencode-shell-async-enqueue
                          origin (list 'request-error request-id)
                          opencode-shell--generation error-callback))))
               (condition-case err
                    (let ((code (or (bound-and-true-p url-http-response-status) 0)))
                       (if (not (<= 200 code 299))
                          (when (buffer-live-p origin)
                            (with-current-buffer origin
                              (when opencode-shell-log-requests
                                 (opencode-shell--log "OpenCode API #%d ← HTTP %s %.2fs [%s %s]"
                                                     request-id code (- (float-time) started) method path))
                               (message "OpenCode: HTTP %s request failed" code)
                               (when error-callback
                                 (opencode-shell-async-enqueue
                                  origin (list 'request-error request-id)
                                  opencode-shell--generation error-callback))))
                        (let ((value (unless (= code 204)
                                       (opencode-shell--json-read-buffer))))
                          (when (buffer-live-p origin)
                            (with-current-buffer origin
                              (when opencode-shell-log-requests
                                 (opencode-shell--log "OpenCode API #%d ← HTTP %s %.2fs [%s %s]"
                                                     request-id code (- (float-time) started) method path))
                               (opencode-shell-async-enqueue
                                origin (list 'request request-id)
                                opencode-shell--generation callback value))))))
                 (error
                  (when (buffer-live-p origin)
                    (with-current-buffer origin
                      (when opencode-shell-log-requests
                         (opencode-shell--log "OpenCode API #%d ← decode-error %.2fs [%s %s]"
                                             request-id (- (float-time) started) method path))
                       (message "OpenCode: %s" (opencode-shell--bounded-error
                                                 (error-message-string err)))
                       (when error-callback
                         (opencode-shell-async-enqueue
                          origin (list 'request-error request-id)
                          opencode-shell--generation error-callback)))))))
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
  "Normalize and newest-first sort SESSIONS, deduplicating by ID.
Each retained session keeps its server-reported directory unchanged."
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
              (or (opencode-shell--get session 'agent) "")
              (or model "") (opencode-shell--status id)
               (if (> (opencode-shell--time session) 0)
                   (format-time-string "%Y-%m-%d %H:%M" updated) "")))))

(defun opencode-shell--session-directory-override (profile session-id)
  "Return the client-side directory override for PROFILE and SESSION-ID."
  (cdr (assoc (list (opencode-shell--profile-key profile) session-id)
              opencode-shell--session-directory-overrides)))

(defun opencode-shell--set-session-directory-override (profile session-id directory)
  "Persist DIRECTORY as PROFILE's effective location for SESSION-ID."
  (let ((key (list (opencode-shell--profile-key profile) session-id)))
    (setf (alist-get key opencode-shell--session-directory-overrides nil nil #'equal)
          (file-name-as-directory directory))
    (unless noninteractive (opencode-shell--save-session-directory-overrides))))

(defun opencode-shell--effective-session (session profile)
  "Return a copy of SESSION with PROFILE's directory override applied."
  (let* ((copy (copy-tree session))
         (id (opencode-shell--get copy 'id))
         (override (and id (opencode-shell--session-directory-override profile id))))
    (when override
      (setf (alist-get 'directory copy) override))
    copy))

(defun opencode-shell--sessions-in-directory (sessions profile directory)
  "Return SESSIONS whose effective PROFILE directory equals DIRECTORY."
  (let ((target (opencode-shell--canonical-directory directory)))
    (seq-filter
     (lambda (session)
       (and-let* ((value (opencode-shell--get session 'directory)))
         (equal target (opencode-shell--canonical-directory value))))
      (mapcar (lambda (session) (opencode-shell--effective-session session profile))
              sessions))))

(defun opencode-shell--relocated-session-ids (profile directory)
  "Return PROFILE session IDs relocated to DIRECTORY."
  (let ((profile-key (opencode-shell--profile-key profile))
        (target (opencode-shell--canonical-directory directory))
        result)
    (dolist (entry opencode-shell--session-directory-overrides (nreverse result))
      (when (and (equal (caar entry) profile-key)
                 (equal (opencode-shell--canonical-directory (cdr entry)) target))
        (push (cadar entry) result)))))

(defun opencode-shell--fetch-relocated-sessions (sessions callback)
  "Add sessions relocated to the current directory, then call CALLBACK."
  (let* ((present (mapcar (lambda (session) (opencode-shell--get session 'id)) sessions))
         (missing (seq-remove (lambda (id) (member id present))
                              (opencode-shell--relocated-session-ids
                               opencode-shell--profile opencode-shell--directory))))
    (if (null missing)
        (funcall callback sessions)
      (let ((remaining (length missing))
            (result sessions))
        (cl-labels ((finish (&optional session)
                      (when session (push session result))
                      (when (= (cl-decf remaining) 0)
                        (funcall callback result))))
          (dolist (id missing)
            (opencode-shell--request
             "GET" (format "/session/%s" id) #'finish nil '((directory)) #'finish)))))))

(defun opencode-shell--child-session-p (session)
  "Return non-nil when SESSION belongs to a parent session."
  (let ((parent (or (opencode-shell--get session 'parentID)
                    (opencode-shell--get session 'parentId))))
    (and parent (not (equal parent "")))))

(defun opencode-shell--session-entries ()
  "Return visible filtered rows from the normalized session collection."
  (mapcar #'opencode-shell--session-row
          (seq-filter
           (lambda (session)
             (and (or opencode-shell--show-child-sessions
                      (not (opencode-shell--child-session-p session)))
                  (or (string-empty-p opencode-shell--filter)
                      (string-match-p
                       (regexp-quote (downcase opencode-shell--filter))
                       (downcase (opencode-shell--session-text session))))))
           opencode-shell--sessions)))

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
       ("f" "Find session" opencode-shell-find-session)]])

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

(defun opencode-shell--remember-session-location (profile directory)
  "Remember PROFILE and DIRECTORY for session browser completion."
  (let* ((directory (concat (directory-file-name directory) "/"))
         (key (cons (opencode-shell--profile-key profile) directory)))
    (setq opencode-shell--recent-session-locations
          (cons key (delete key opencode-shell--recent-session-locations)))
    (unless noninteractive (opencode-shell--save-recent-session-locations))))

(defun opencode-shell--sessions (directory &optional profile current-window)
  "Open PROFILE's session browser scoped to server-native DIRECTORY.
When CURRENT-WINDOW is non-nil, display it in the selected window."
  (setq profile (or (opencode-shell--resolve-profile profile)
                    profile opencode-shell--profile
                    (opencode-shell--default-profile)))
  (opencode-shell--validate-profiles)
  (unless (and (stringp directory) (file-name-absolute-p directory))
    (user-error "OpenCode session directory must be absolute"))
  (setq directory (file-name-as-directory directory))
  (opencode-shell--remember-session-location profile directory)
  (let ((buffer (or (opencode-shell--sessions-buffer profile directory)
                    (generate-new-buffer
                     (format "*Opencode %s sessions*"
                             (opencode-shell--directory-leaf directory))))))
    (with-current-buffer buffer
      (opencode-shell-sessions-mode)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--workspace (plist-get profile :workspace))
      (setq-local opencode-shell--directory (file-name-as-directory directory))
      (setq-local default-directory
                  (opencode-shell--client-directory directory profile))
      (opencode-shell--refresh))
    (if current-window (switch-to-buffer buffer) (pop-to-buffer buffer))))

(defun opencode-shell--refresh ()
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
               (opencode-shell--fetch-relocated-sessions
                 sessions
                 (lambda (all-sessions)
                   (when (= generation opencode-shell--generation)
                     (opencode-shell-async-enqueue
                      (current-buffer) 'browser-snapshot generation
                      #'opencode-shell--apply-session-browser-snapshot
                      all-sessions id generation))))))
          nil
          '((limit . 1000))))))))

(defun opencode-shell--apply-session-browser-snapshot (sessions id generation)
  "Apply SESSIONS for GENERATION, preserving row ID when visible."
  (when (= generation opencode-shell--generation)
    (setq opencode-shell--sessions
          (opencode-shell--normalize-sessions
           (opencode-shell--sessions-in-directory
            sessions opencode-shell--profile opencode-shell--directory))
          tabulated-list-entries (opencode-shell--session-entries)
          opencode-shell--browser-dirty t)
    (when (get-buffer-window (current-buffer) t)
      (setq opencode-shell--browser-dirty nil)
      (tabulated-list-print t)
      (when id (goto-char (point-min)) (search-forward id nil t)))))

(defun opencode-shell--render-session-browser-if-visible ()
  "Render one pending browser snapshot when this buffer becomes visible."
  (when (and opencode-shell--browser-dirty
             (get-buffer-window (current-buffer) t))
    (setq opencode-shell--browser-dirty nil)
    (tabulated-list-print t)))

(defun opencode-shell--filter (text)
  "Filter the session list by TEXT."
  (interactive (list (read-string "Filter sessions: " opencode-shell--filter)))
  (setq opencode-shell--filter text
        tabulated-list-entries (opencode-shell--session-entries))
  (tabulated-list-print t))

(defun opencode-shell--toggle-child-sessions ()
  "Toggle display of child sessions in the current session browser."
  (interactive)
  (setq opencode-shell--show-child-sessions
        (not opencode-shell--show-child-sessions)
        tabulated-list-entries (opencode-shell--session-entries))
  (tabulated-list-print t)
  (message "Child sessions %s"
           (if opencode-shell--show-child-sessions "shown" "hidden")))

(defun opencode-shell--create-session ()
  "Create a session in the browser's fixed directory."
  (interactive)
  (opencode-shell--start-session
   (or opencode-shell--profile (opencode-shell--default-profile))
   opencode-shell--directory))

(defun opencode-shell--open-at-point ()
  "Open the session at point."
  (interactive)
  (if-let ((id (tabulated-list-get-id)))
      (let* ((session (seq-find
                       (lambda (item) (equal id (opencode-shell--get item 'id)))
                       opencode-shell--sessions))
             (directory (opencode-shell--get session 'directory)))
        (unless directory
          (user-error "Session %s has no server-reported directory" id))
        (if opencode-shell--profile
            (opencode-shell-open-session id directory opencode-shell--profile)
          (opencode-shell-open-session id directory)))
    (user-error "No session at point")))

(defun opencode-shell--delete-session ()
  "Delete the session at point after confirmation."
  (interactive)
  (let ((id (or (tabulated-list-get-id) (user-error "No session at point"))))
    (when (yes-or-no-p (format "Delete OpenCode session %s? " id))
      (opencode-shell--request "DELETE" (format "/session/%s" id)
                                (lambda (_)
                                  (let ((key (list (opencode-shell--profile-key
                                                    opencode-shell--profile)
                                                   id)))
                                    (setq opencode-shell--session-directory-overrides
                                          (assoc-delete-all
                                           key opencode-shell--session-directory-overrides
                                           #'equal))
                                    (unless noninteractive
                                      (opencode-shell--save-session-directory-overrides)))
                                  (opencode-shell--refresh))))))

(defun opencode-shell--normalize-models (response)
  "Return models from server-connected providers in RESPONSE."
  (let* ((connected-present (or (assq 'connected response)
                                (assoc "connected" response)))
         (connected (opencode-shell--get response 'connected)))
    (mapcan
     (lambda (provider)
       (let ((provider-id (opencode-shell--get provider 'id)))
         (when (or (not connected-present) (member provider-id connected))
           (mapcar
            (lambda (entry)
              (let* ((key (and (consp entry) (atom (car entry)) (car entry)))
                     (model (if key (cdr entry) entry))
                     (value (or (opencode-shell--model-value model provider-id)
                                (and key (opencode-shell--model-value
                                          (format "%s" key) provider-id)))))
                (cons (opencode-shell--model-name value) value)))
            (opencode-shell--get provider 'models)))))
     (or (opencode-shell--get response 'all)
         (opencode-shell--get response 'providers)))))

(defun opencode-shell--normalize-agents (response)
  "Return server-advertised visible primary agents from RESPONSE."
  (mapcar (lambda (agent)
            (let ((name (opencode-shell--get agent 'name))) (cons name agent)))
          (seq-filter
           (lambda (agent)
             (and (not (opencode-shell--get agent 'hidden))
                  (not (opencode-shell--get agent 'disabled))
                  (not (opencode-shell--get agent 'disable))
                  (let ((mode (opencode-shell--get agent 'mode)))
                    (or (null mode) (equal (format "%s" mode) "primary")))))
           response)))

(defun opencode-shell--preserve-choice (choice choices)
  "Preserve CHOICE only when it still occurs in CHOICES."
  (and choice (seq-find (lambda (item) (equal (cdr item) choice)) choices) choice))

(defun opencode-shell--mode-line-status ()
  "Return compact transcript status for the mode line."
  (format " [%s]" opencode-shell--request-status))

(defvar opencode-shell-header-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'opencode-shell-copy-session-id)
    map)
  "Mouse map for the session ID in transcript headers.")

(defun opencode-shell--header ()
  "Return live transcript title, agent, and model metadata."
  (let* ((left (format " %s  agent:%s  model:%s"
                       (or opencode-shell--session-title "Untitled")
                       (or opencode-shell--selected-agent "server default")
                       (or (car (rassoc opencode-shell--selected-model
                                       opencode-shell--models))
                           "server default")))
         (right-text (format "session:%s" (or opencode-shell--session-id "pending")))
         (right (if opencode-shell--session-id
                    (concat (propertize right-text
                                        'keymap opencode-shell-header-session-map
                                        'mouse-face 'mode-line-highlight
                                        'help-echo "mouse-1: Copy session ID")
                            " ")
                  (concat right-text " ")))
         (space (max 1 (- (window-total-width) (string-width left)
                          (string-width right)))))
    (concat left (make-string space ?\s) right)))

(defun opencode-shell-copy-session-id ()
  "Copy the current OpenCode session ID to the kill ring."
  (interactive)
  (unless opencode-shell--session-id (user-error "No OpenCode session ID"))
  (kill-new opencode-shell--session-id)
  (message "Copied OpenCode session ID"))

(defun opencode-shell--assert-session-operation-ready ()
  "Signal a user error unless the current session has stable history."
  (unless (and (derived-mode-p 'opencode-shell-mode) opencode-shell--session-id)
    (user-error "No OpenCode session in this buffer"))
  (when (or opencode-shell--submit-in-flight
            opencode-shell--permission-sending
            opencode-shell--question-sending
            opencode-shell--permissions
            opencode-shell--questions-pending
            (seq-some (lambda (turn)
                        (not (eq (opencode-shell--turn-status turn) 'complete)))
                      opencode-shell--turns))
    (user-error "Wait for the current OpenCode interaction to finish")))

(defun opencode-shell--fork-candidates ()
  "Return chronological minibuffer candidates for the current session."
  (let ((index 0) candidates)
    (dolist (turn opencode-shell--turns)
      (when-let ((id (opencode-shell--turn-server-user-id turn)))
        (cl-incf index)
        (let ((text (truncate-string-to-width
                     (replace-regexp-in-string
                      "[\n\r\t ]+" " " (or (opencode-shell--turn-user turn) ""))
                     70 nil nil t)))
          (push (cons (format "Before prompt %d: %s" index text) id)
                candidates))))
    (nreverse candidates)))

;;;###autoload
(defun opencode-shell-fork-session ()
  "Fork this session before a minibuffer-selected prompt."
  (interactive)
  (opencode-shell--assert-session-operation-ready)
  (let ((candidates (opencode-shell--fork-candidates)))
    (unless candidates (user-error "No forkable prompts in this session"))
    (let* ((choice (completing-read "Fork session: " candidates nil t))
           (message-id (cdr (assoc choice candidates)))
           (profile opencode-shell--profile)
           (directory opencode-shell--directory))
      (unless message-id (user-error "No fork boundary selected"))
      (opencode-shell--request
       "POST" (format "/session/%s/fork" opencode-shell--session-id)
       (lambda (session)
         (let ((id (opencode-shell--get session 'id))
               (fork-directory (or (opencode-shell--get session 'directory)
                                   directory)))
           (unless id (user-error "Fork response has no session ID"))
           (opencode-shell-open-session id fork-directory profile)))
       `((messageID . ,message-id)) nil
       (lambda () (message "OpenCode session fork failed"))))))

(defun opencode-shell--destination-server-directory (directory profile)
  "Return DIRECTORY's project root in PROFILE server-native form."
  (let* ((project-directory (opencode-shell--project-directory directory))
         (server-directory
          (opencode-shell--server-directory project-directory profile)))
    (unless (and (stringp server-directory)
                 (file-name-absolute-p server-directory))
      (user-error "Destination cannot be mapped to the OpenCode server"))
    (file-name-as-directory server-directory)))

;;;###autoload
(defun opencode-shell-move-session-directory ()
  "Change this transcript buffer's request scope to another project directory."
  (interactive)
  (unless (and (derived-mode-p 'opencode-shell-mode) opencode-shell--session-id)
    (user-error "No OpenCode session in this buffer"))
  (let* ((source-directory opencode-shell--directory)
         (profile opencode-shell--profile)
         (client-directory (opencode-shell--client-directory source-directory profile))
         (selected (read-directory-name "Move session to project: "
                                        client-directory nil t))
         (destination
          (opencode-shell--destination-server-directory selected profile)))
    (when (equal (opencode-shell--canonical-directory source-directory)
                 (opencode-shell--canonical-directory destination))
      (user-error "Session is already in that project directory"))
    (opencode-shell--set-session-directory-override
     profile opencode-shell--session-id destination)
    (opencode-shell--remember-session-location profile destination)
    (setq-local opencode-shell--directory destination
                default-directory
                (opencode-shell--client-directory destination profile))))

(defun opencode-shell--initialize-server-defaults ()
  "Initialize unset selections from OpenCode's build agent."
  (when-let* ((build (cdr (assoc "build" opencode-shell--agents)))
              (configured (opencode-shell--model-value
                           (opencode-shell--get build 'model)))
              (available (seq-find
                          (lambda (entry) (equal (cdr entry) configured))
                          opencode-shell--models)))
    (unless opencode-shell--selected-agent
      (setq opencode-shell--selected-agent "build"))
    (unless opencode-shell--selected-model
      (setq opencode-shell--selected-model (cdr available)))))

(defun opencode-shell-help ()
  "Display the transcript Transient menu."
  (interactive)
  (transient-setup 'opencode-shell-menu))

(transient-define-prefix opencode-shell-menu ()
  "OpenCode transcript actions."
  [["Session"
    ("RET" "Submit" opencode-shell--submit)
    ("g" "Resync" opencode-shell--resync)
    ("a" "Abort" opencode-shell--abort)]
   ["Options"
    ("m" "Model" opencode-shell--select-model)
    ("A" "Agent" opencode-shell--select-agent)
    ("p" "Permission" opencode-shell--permissions)
    ("q" "Question" opencode-shell--questions)]
   ["Global"
    ("b" "Shell buffers" opencode-shell-switch-buffer)
    ("f" "Find session" opencode-shell-find-session)
    ("l" "Session browser" opencode-shell)
    ("s" "Start session" opencode-shell-start)]])

(defvar opencode-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-shell--submit)
    (define-key map (kbd "s-<return>") #'opencode-shell--submit)
    (define-key map (kbd "C-c C-v") #'opencode-shell--select-model)
    (define-key map (kbd "C-c C-m") #'opencode-shell--select-agent)
    (define-key map (kbd "C-c C-g") #'opencode-shell--resync)
    (define-key map (kbd "C-c C-a") #'opencode-shell--abort)
    (define-key map (kbd "C-c C-p") #'opencode-shell--permissions)
    (define-key map (kbd "C-c C-y") #'opencode-shell--permission-allow-once)
    (define-key map (kbd "C-c C-l") #'opencode-shell--permission-allow-always)
    (define-key map (kbd "C-c C-n") #'opencode-shell--permission-reject)
    (define-key map (kbd "C-c C-q") #'opencode-shell--questions)
    (define-key map (kbd "C-c C-h") #'describe-mode)
    map))

(defvar opencode-shell-permission-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'opencode-shell--permission-allow-once)
    (define-key map (kbd "a") #'opencode-shell--permission-allow-always)
    (define-key map (kbd "n") #'opencode-shell--permission-reject)
    (define-key map (kbd "r") #'opencode-shell--permission-reject)
    map))

(defvar opencode-shell-question-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'opencode-shell--questions)
    (define-key map (kbd "a") #'opencode-shell--questions)
    (define-key map (kbd "r") #'opencode-shell--question-reject)
    map)
  "Keymap for an inline pending question.")

(defun opencode-shell--in-composer-p ()
  "Return non-nil when point is in the writable composer."
  (and (markerp opencode-shell--composer-start)
       (marker-position opencode-shell--composer-start)
       (>= (point) opencode-shell--composer-start)))

(defun opencode-shell--protect-transcript (begin end)
  "Reject user edits before or crossing the composer boundary."
  (when (and (not inhibit-read-only)
             (markerp opencode-shell--composer-start)
             (marker-position opencode-shell--composer-start)
             (or (< begin opencode-shell--composer-start)
                 (< end opencode-shell--composer-start)))
    (signal 'text-read-only (list "OpenCode transcript is read-only"))))

(defun opencode-shell--shift-undo-position (position threshold delta)
  "Shift signed undo POSITION at or after THRESHOLD by DELTA."
  (let ((sign (if (< position 0) -1 1))
        (absolute (abs position)))
    (* sign (if (>= absolute threshold) (+ absolute delta) absolute))))

(defun opencode-shell--shift-undo-entry (entry threshold delta)
  "Shift composer positions in undo ENTRY by DELTA after THRESHOLD."
  (cond
   ((integerp entry)
    (opencode-shell--shift-undo-position entry threshold delta))
   ((and (consp entry) (integerp (car entry)) (integerp (cdr entry)))
    (cons (opencode-shell--shift-undo-position (car entry) threshold delta)
          (opencode-shell--shift-undo-position (cdr entry) threshold delta)))
   ((and (consp entry) (stringp (car entry)) (integerp (cdr entry)))
    (cons (car entry)
          (opencode-shell--shift-undo-position (cdr entry) threshold delta)))
   ((and (consp entry) (null (car entry))
         (integerp (nth 3 entry))
         (integerp (cdr (nthcdr 3 entry))))
    (cons nil
          (cons (nth 1 entry)
                (cons (nth 2 entry)
                      (cons (opencode-shell--shift-undo-position
                             (nth 3 entry) threshold delta)
                            (opencode-shell--shift-undo-position
                             (cdr (nthcdr 3 entry)) threshold delta))))))
   ((and (listp entry) (eq (car entry) 'apply)
         (integerp (nth 1 entry))
         (integerp (nth 2 entry)) (integerp (nth 3 entry)))
    (let ((copy (copy-sequence entry)))
      (setf (nth 2 copy) (opencode-shell--shift-undo-position
                          (nth 2 copy) threshold delta)
            (nth 3 copy) (opencode-shell--shift-undo-position
                          (nth 3 copy) threshold delta))
      copy))
   (t entry)))

(defmacro opencode-shell--without-user-undo (&rest body)
  "Run BODY without adding package edits to the user's undo history."
  (declare (indent 0) (debug t))
  `(if (eq buffer-undo-list t)
       (progn ,@body)
     (let ((saved-undo buffer-undo-list)
           (old-composer-start (and (markerp opencode-shell--composer-start)
                                    (marker-position opencode-shell--composer-start)))
           result)
       (let ((buffer-undo-list t))
         (setq result (progn ,@body)))
       (when (and old-composer-start
                  (marker-position opencode-shell--composer-start))
         (let ((delta (- (marker-position opencode-shell--composer-start)
                         old-composer-start)))
           (setq saved-undo
                 (mapcar (lambda (entry)
                           (opencode-shell--shift-undo-entry
                            entry old-composer-start delta))
                         saved-undo))))
       (setq buffer-undo-list saved-undo)
       result)))

(define-derived-mode opencode-shell-mode text-mode "OpenCode"
  "OpenCode transcript mode with a writable bottom composer."
  (setq-local font-lock-defaults '(opencode-shell-render-font-lock-keywords t))
  (setq-local header-line-format '(:eval (opencode-shell--header)))
  (setq-local mode-line-process '(:eval (opencode-shell--mode-line-status)))
  (setq-local opencode-shell--turns nil opencode-shell--turn-counter 0
              opencode-shell--rendered-turns nil
              opencode-shell--permissions nil
              opencode-shell--request-status "idle")
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (propertize "Prompt> " 'read-only t
                        'opencode-shell-composer-label t
                        'rear-nonsticky '(read-only opencode-shell-composer-label)))
    (setq opencode-shell--composer-start (copy-marker (point) nil)
          opencode-shell--transcript-end (copy-marker (point) nil)
          opencode-shell--permission-begin (copy-marker (point) nil)
          opencode-shell--permission-end (copy-marker (point) nil)
          opencode-shell--permission-status-begin (copy-marker (point) nil)
           opencode-shell--permission-status-end (copy-marker (point) nil)))
  ;; Mode-owned scaffolding must never become the first undoable transcript edit.
  (setq buffer-undo-list nil)
  (goto-char (point-max))
  (add-hook 'before-change-functions #'opencode-shell--protect-transcript nil t)
  (add-hook 'window-configuration-change-hook
            #'opencode-shell--refresh-table-layout nil t)
  (add-hook 'window-configuration-change-hook
            #'opencode-shell--render-if-visible nil t)
  (add-hook 'kill-buffer-hook #'opencode-shell--cleanup nil t)
  )

(defun opencode-shell--cleanup ()
  "Cancel timers, close the session log, and invalidate callbacks."
  (opencode-shell-async-cancel)
  (remove-hook 'window-configuration-change-hook
               #'opencode-shell--refresh-table-layout t)
  (remove-hook 'window-configuration-change-hook
               #'opencode-shell--render-if-visible t)
  (when (and (timerp opencode-shell--poll-timer)
             (null opencode-shell--runtime-key))
    (cancel-timer opencode-shell--poll-timer))
  (when opencode-shell--runtime-key
    (opencode-shell-async-unsubscribe-runtime
     opencode-shell--runtime-key (current-buffer)))
  (when (and (timerp opencode-shell--animation-timer)
             (not (eq opencode-shell--animation-timer
                      opencode-shell-async--animation-timer)))
    (cancel-timer opencode-shell--animation-timer))
  (opencode-shell-async-unsubscribe-animation (current-buffer))
  (setq opencode-shell--poll-timer nil
        opencode-shell--animation-timer nil
        opencode-shell--runtime-key nil)
  (setq opencode-shell--in-flight nil
        opencode-shell--capabilities-loading nil)
  (when opencode-shell--session-id
    (when-let ((log-buffer (get-buffer (opencode-shell--log-buffer-name))))
      (kill-buffer log-buffer)))
  (cl-incf opencode-shell--generation))

(defun opencode-shell--stop-polling ()
  "Stop periodic network polling and UI animation in the current buffer."
  (opencode-shell-async-cancel)
  (when (and (timerp opencode-shell--poll-timer)
             (null opencode-shell--runtime-key))
    (cancel-timer opencode-shell--poll-timer))
  (when opencode-shell--runtime-key
    (opencode-shell-async-unsubscribe-runtime
     opencode-shell--runtime-key (current-buffer)))
  (when (and (timerp opencode-shell--animation-timer)
             (not (eq opencode-shell--animation-timer
                      opencode-shell-async--animation-timer)))
    (cancel-timer opencode-shell--animation-timer))
  (opencode-shell-async-unsubscribe-animation (current-buffer))
  (setq opencode-shell--poll-timer nil
        opencode-shell--animation-timer nil
        opencode-shell--runtime-key nil)
  (opencode-shell--log-lifecycle "poll-stop" t))

(defun opencode-shell--start-polling ()
  "Start periodic network polling and UI animation if needed."
  (unless opencode-shell--runtime-key
    (let ((buffer (current-buffer)))
      (setq opencode-shell--animation-frame 0)
      (opencode-shell--render-status-animation)
      (let* ((profile (or opencode-shell--profile (opencode-shell--default-profile)))
             (key (opencode-shell--server-key profile))
             (base (string-remove-suffix
                    "/" (or opencode-shell--base-url
                            (plist-get profile :base-url) opencode-shell-base-url)))
             (auth (opencode-shell--auth-header profile))
             (runtime
              (opencode-shell-async-subscribe-runtime
               key buffer (concat base "/event") (and auth (list auth))
               (not (opencode-shell--profile-remote-p profile))
               opencode-shell-poll-interval
               (lambda () (opencode-shell--resync nil))
               (lambda (event)
                 (when opencode-shell-log-requests
                   (opencode-shell--log "OpenCode async %s" event))))))
        (setq opencode-shell--runtime-key key
              opencode-shell--poll-timer (plist-get runtime :poll-timer)
              opencode-shell--animation-timer
            (opencode-shell-async-subscribe-animation
             buffer opencode-shell-animation-interval
             #'opencode-shell--animation-tick)))
      (opencode-shell--log-lifecycle "poll-start" t))))

;;;###autoload
(defun opencode-shell-open-session (id &optional directory profile)
  "Open exact session ID scoped to DIRECTORY and PROFILE."
  (interactive "sSession ID: ")
  (let* ((explicit-profile (or profile opencode-shell--profile))
          (profile (or (opencode-shell--resolve-profile profile)
                       profile opencode-shell--profile
                       (opencode-shell--default-profile)))
          (resolved-directory
           (opencode-shell--server-directory
            (or directory (plist-get profile :directory) opencode-shell-directory)
            profile))
          (buffer (or (opencode-shell--transcript-buffer
                       profile resolved-directory id)
                      (generate-new-buffer
                       (format "*Opencode %s shell*"
                               (opencode-shell--directory-leaf resolved-directory))))))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-shell-mode) (opencode-shell--cleanup))
      (opencode-shell-mode)
      (setq-local opencode-shell--generation (cl-incf opencode-shell--generation-counter))
      (setq-local opencode-shell--session-id id)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--workspace (plist-get profile :workspace))
      (setq-local opencode-shell--directory resolved-directory)
      (setq-local default-directory
                  (opencode-shell--client-directory resolved-directory profile))
      (setq-local opencode-shell--session-title nil)
      (opencode-shell--resync t)
      (opencode-shell--start-polling))
    (pop-to-buffer buffer)
    (opencode-shell--refresh-table-layout)
    (goto-char (point-max))
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

(defun opencode-shell--part-text (part)
  "Return display text for message PART."
  (pcase (opencode-shell--get part 'type)
    ("text" (or (opencode-shell--get part 'text) ""))
    ((or "tool" "tool_use" "tool-result")
     (format "[tool %s: %s]" (or (opencode-shell--get part 'tool) "")
             (or (opencode-shell--get (opencode-shell--get part 'state) 'status)
                 (opencode-shell--get part 'status) "pending")))
    (_ "")))

(defun opencode-shell--message-text (envelope)
  "Return exact visible text from ENVELOPE's text parts."
  (mapconcat #'identity
             (delq nil
                   (mapcar (lambda (part)
                             (when (equal (format "%s" (opencode-shell--get part 'type)) "text")
                               (opencode-shell--get part 'text)))
                            (opencode-shell--get envelope 'parts))) ""))

(defun opencode-shell--part-id (part)
  "Return PART's stable identity, when available."
  (or (opencode-shell--get part 'id)
      (opencode-shell--get part 'callID)
      (opencode-shell--get part 'callId)))

(defun opencode-shell--merge-alist (known incoming)
  "Return KNOWN alist with fields present in INCOMING replaced."
  (let ((result (copy-tree known)))
    (dolist (field incoming result)
      (setf (alist-get (car field) result) (cdr field)))))

(defun opencode-shell--merge-parts (known incoming)
  "Merge INCOMING message parts into KNOWN without dropping omitted parts."
  (let ((result (copy-sequence known)))
    (dolist (part incoming)
      (let* ((id (opencode-shell--part-id part))
             (cell (and id (seq-find (lambda (known-part)
                                       (equal id (opencode-shell--part-id known-part)))
                                     result))))
        (if cell
            (setcar (memq cell result) (opencode-shell--merge-alist cell part))
          (setq result (append result (list part))))))
    result))

(defun opencode-shell--merge-envelope (known incoming)
  "Merge partial assistant envelope INCOMING into KNOWN."
  (let* ((merged (copy-tree incoming))
         (known-info (opencode-shell--get known 'info))
         (incoming-info (opencode-shell--get incoming 'info))
         (info (opencode-shell--merge-alist known-info incoming-info)))
    (setf (alist-get 'time info)
          (opencode-shell--merge-alist (opencode-shell--get known-info 'time)
                                       (opencode-shell--get incoming-info 'time))
          (alist-get 'info merged) info)
    (setf (alist-get 'parts merged)
          (opencode-shell--merge-parts (opencode-shell--get known 'parts)
                                       (opencode-shell--get incoming 'parts)))
    merged))

(defun opencode-shell--terminal-part-p (part)
  "Return non-nil when PART authoritatively ends an assistant turn."
  (equal (format "%s" (opencode-shell--get part 'type)) "step-finish"))

(defun opencode-shell--running-tool-part-p (part)
  "Return non-nil when PART describes an unsettled tool invocation."
  (let ((type (format "%s" (opencode-shell--get part 'type)))
        (status (format "%s" (or (opencode-shell--get
                                   (opencode-shell--get part 'state) 'status)
                                  (opencode-shell--get part 'status) ""))))
    (and (member type '("tool" "tool_use" "tool-result"))
         (not (member status '("completed" "error"))))))

(defun opencode-shell--continuation-finish-p (finish)
  "Return non-nil when FINISH indicates more assistant steps will follow.
On OpenCode 1.18.30, \"tool-calls\" is the only observed non-terminal value;
every other populated value (observed: \"stop\") is terminal."
  (equal (format "%s" finish) "tool-calls"))

(defun opencode-shell--message-error-label (info)
  "Return a bounded, single-line human-readable label for INFO's terminal
error, or nil."
  (when-let ((err (opencode-shell--get info 'error)))
    (let* ((name (format "%s" (or (opencode-shell--get err 'name) "Error")))
           (message (opencode-shell--get (opencode-shell--get err 'data) 'message)))
      (truncate-string-to-width
       (replace-regexp-in-string
        "[\n\r\t ]+" " "
        (if message (format "%s: %s" name message) name))
       200 nil nil t))))

(defun opencode-shell--assistant-envelope-complete-p (envelope)
  "Return non-nil when ENVELOPE contains authoritative completion evidence."
  (let* ((info (opencode-shell--get envelope 'info))
         (parts (opencode-shell--get envelope 'parts))
         (finish (opencode-shell--get info 'finish)))
    (and (not (seq-some #'opencode-shell--running-tool-part-p parts))
         (not (opencode-shell--continuation-finish-p finish))
         (or (and (opencode-shell--get (opencode-shell--get info 'time) 'completed)
                  (or finish (opencode-shell--get info 'error)))
             (and (null finish) (seq-some #'opencode-shell--terminal-part-p parts))))))

(defun opencode-shell--lifecycle-id (value)
  "Return VALUE as bounded operational metadata."
  (if value (truncate-string-to-width (format "%s" value) 80 nil nil t) "-"))

(defun opencode-shell--part-state-label (part)
  "Return a payload-free type and status label for PART."
  (let* ((type (format "%s" (or (opencode-shell--get part 'type) "unknown")))
         (state (opencode-shell--get part 'state))
         (status (or (opencode-shell--get state 'status)
                     (opencode-shell--get part 'status))))
    (if (member type '("tool" "tool_use" "tool-result"))
        (format "%s:%s" type (or status "unknown"))
      type)))

(defun opencode-shell--count-labels (labels)
  "Return sorted occurrence counts for payload-free LABELS."
  (let (counts)
    (dolist (label labels)
      (setf (alist-get label counts nil nil #'equal)
            (1+ (or (alist-get label counts nil nil #'equal) 0))))
    (mapconcat (lambda (entry) (format "%s=%d" (car entry) (cdr entry)))
               (sort counts (lambda (a b) (string< (car a) (car b)))) ",")))

(defun opencode-shell--lifecycle-summary ()
  "Return a deterministic privacy-safe polling state summary."
  (let* ((turns opencode-shell--turns)
         (assistants (apply #'append
                            (mapcar #'opencode-shell--turn-assistant-messages turns)))
         (last-envelope (cdr (car (last assistants))))
         (last-info (opencode-shell--get last-envelope 'info))
         (parts (apply #'append
                       (mapcar (lambda (turn) (or (opencode-shell--turn-parts turn) nil))
                               turns)))
         (nonterminal (seq-filter
                       (lambda (turn) (not (eq (opencode-shell--turn-status turn) 'complete)))
                       turns))
         (submit-turn (and opencode-shell--submit-in-flight
                           (opencode-shell--turn-by-id opencode-shell--submit-in-flight turns)))
         (blocked-permission (opencode-shell--permission-blocked-p))
         (blockers (delq nil
                         (list (and blocked-permission "permission")
                               (and opencode-shell--submit-in-flight "submit")
                               (and nonterminal "nonterminal")
                               (and (not (equal opencode-shell--request-status "idle"))
                                    "request-status"))))
         (finish (opencode-shell--get last-info 'finish))
         (completed (opencode-shell--get (opencode-shell--get last-info 'time)
                                          'completed)))
    (format (concat "turns=%d assistants=%d nonterminal=%s last-assistant=%s "
                    "finish=%s completed=%s parts=%s session-status=%s "
                    "permissions=%d reply-in-flight=%s submit=%s submit-match=%s "
                    "request-status=%s blockers=%s")
            (length turns) (length assistants)
            (if nonterminal
                (mapconcat (lambda (turn)
                             (format "%s:%s"
                                     (opencode-shell--lifecycle-id
                                      (opencode-shell--turn-id turn))
                                     (opencode-shell--turn-status turn)))
                           nonterminal ",")
              "none")
            (opencode-shell--lifecycle-id (opencode-shell--get last-info 'id))
            (if finish (opencode-shell--lifecycle-id finish) "absent")
            (if completed "present" "absent")
            (or (opencode-shell--count-labels
                 (mapcar #'opencode-shell--part-state-label parts)) "none")
            (opencode-shell--status opencode-shell--session-id)
            (length opencode-shell--permissions)
            (if opencode-shell--permission-sending "yes" "no")
            (opencode-shell--lifecycle-id opencode-shell--submit-in-flight)
            (if submit-turn "yes" "no")
            opencode-shell--request-status
            (if blockers (mapconcat #'identity blockers ",") "none"))))

(defun opencode-shell--log-lifecycle (&optional event force)
  "Log privacy-safe lifecycle state after EVENT when it changed.
When FORCE is non-nil, emit the state even when its signature is unchanged."
  (when opencode-shell-log-requests
    (let ((summary (opencode-shell--lifecycle-summary)))
      (when (or force (not (equal summary opencode-shell--last-lifecycle-signature)))
        (setq opencode-shell--last-lifecycle-signature summary)
        (opencode-shell--log "Lifecycle%s %s"
                             (if event (format "[%s]" event) "") summary)))))

(defun opencode-shell--response-phase (turn)
  "Return the nonterminal response phase for TURN's authoritative parts."
  (let ((parts (opencode-shell--turn-parts turn)))
    (cond ((seq-some (lambda (part)
                       (member (format "%s" (opencode-shell--get part 'type))
                               '("text" "tool" "tool_use" "tool-result")))
                     parts) 'receiving)
          ((seq-some (lambda (part)
                       (equal (format "%s" (opencode-shell--get part 'type)) "reasoning"))
                     parts) 'thinking)
          (t 'waiting))))

(defun opencode-shell--turn-by-server-id (id turns)
  "Find the turn with server user ID ID in TURNS."
  (seq-find (lambda (turn) (equal id (opencode-shell--turn-server-user-id turn))) turns))

(defun opencode-shell--turn-by-id (id turns)
  "Find the turn whose stable client or server ID equals ID."
  (seq-find (lambda (turn)
              (or (equal id (opencode-shell--turn-id turn))
                  (equal id (opencode-shell--turn-server-user-id turn))))
            turns))

(defun opencode-shell--settle-superseded-turns (turns &optional all)
  "Settle nonterminal TURNS superseded by a later prompt.
When ALL is non-nil, settle every existing nonterminal turn because a new
prompt is about to be appended.  Otherwise leave the newest turn active."
  (dolist (turn (if all turns (butlast turns)))
    (unless (eq (opencode-shell--turn-status turn) 'complete)
      (setf (opencode-shell--turn-status turn) 'complete
            (opencode-shell--turn-terminal-error turn)
            "Interrupted: Superseded by a later prompt"
            (opencode-shell--turn-locally-settled turn) t)))
  turns)

(defun opencode-shell--normalize-turns (messages)
  "Reconcile server MESSAGES into stable buffer-local turn records."
  (let ((old opencode-shell--turns) observed current used)
    (dolist (envelope messages)
      (let* ((info (opencode-shell--get envelope 'info))
             (role (format "%s" (or (opencode-shell--get info 'role) "")))
             (id (opencode-shell--get info 'id))
             (parent (or (opencode-shell--get info 'parentID)
                         (opencode-shell--get info 'parentId))))
        (cond
         ((equal role "user")
          (let* ((text (opencode-shell--message-text envelope))
                  (turn (or (opencode-shell--turn-by-id id old)
                           (seq-find
                            (lambda (candidate)
                              (and (null (opencode-shell--turn-server-user-id candidate))
                                   (not (memq candidate used))
                                   (equal text (opencode-shell--turn-user candidate))))
                            old)
                            (opencode-shell--make-turn
                             :id (or id (format "turn-%d" (cl-incf opencode-shell--turn-counter)))))))
            (setf (opencode-shell--turn-server-user-id turn) id
                  (opencode-shell--turn-acknowledged turn) t
                  (opencode-shell--turn-user turn) text
                  (opencode-shell--turn-parts turn) nil
                  (opencode-shell--turn-status turn)
                  (if (or (opencode-shell--turn-assistant turn)
                          (opencode-shell--turn-locally-settled turn))
                      'complete 'waiting))
            (setq current turn)
            (push turn used)
            (push turn observed)))
          ((equal role "assistant")
           (let* ((turn (or (and parent (opencode-shell--turn-by-id parent (append observed old)))
                             current)))
            (when turn
              (let* ((messages (opencode-shell--turn-assistant-messages turn))
                     (entry (assoc id messages)))
                 (if entry (setcdr entry (opencode-shell--merge-envelope (cdr entry) envelope))
                   (setq messages (append messages (list (cons id envelope)))))
                (setf (opencode-shell--turn-assistant-messages turn) messages
                      (opencode-shell--turn-parts turn)
                      (apply #'append (mapcar (lambda (item)
                                                (opencode-shell--get (cdr item) 'parts))
                                              messages))
                      (opencode-shell--turn-assistant turn)
                      (mapconcat (lambda (item) (opencode-shell--message-text (cdr item)))
                                 messages "")
                      (opencode-shell--turn-terminal-error turn)
                      (if (and (opencode-shell--assistant-envelope-complete-p
                                (cdar (last messages)))
                               (not (seq-some #'opencode-shell--running-tool-part-p
                                              (opencode-shell--turn-parts turn))))
                          (opencode-shell--message-error-label
                           (opencode-shell--get (cdar (last messages)) 'info))
                        (opencode-shell--turn-terminal-error turn))
                      (opencode-shell--turn-status turn)
                      (cond
                       ((and (opencode-shell--assistant-envelope-complete-p
                              (cdar (last messages)))
                             (not (seq-some #'opencode-shell--running-tool-part-p
                                            (opencode-shell--turn-parts turn))))
                        'complete)
                       ((opencode-shell--turn-locally-settled turn) 'complete)
                       (t (opencode-shell--response-phase turn)))
                      (opencode-shell--turn-locally-settled turn)
                      (and (opencode-shell--turn-locally-settled turn)
                           (not (and (opencode-shell--assistant-envelope-complete-p
                                      (cdar (last messages)))
                                     (not (seq-some #'opencode-shell--running-tool-part-p
                                                    (opencode-shell--turn-parts turn))))))))))))))
    (setq observed (nreverse observed))
    (let ((result (copy-sequence old)))
      (dolist (turn observed)
        (unless (memq turn result)
          (setq result (append result (list turn)))))
      (opencode-shell--settle-superseded-turns result))))

(defun opencode-shell--composer-text ()
  "Return the composer contents without properties."
  (buffer-substring-no-properties opencode-shell--composer-start (point-max)))

(defun opencode-shell--composer-visible-p ()
  "Return non-nil when the prompt composer should be displayed."
  opencode-shell--composer-visible)

(defun opencode-shell--human-interaction-blocked-p ()
  "Return non-nil while a human interaction blocks new input."
  (or opencode-shell--permissions opencode-shell--permission-sending
      opencode-shell--questions-pending opencode-shell--question-sending))

(defalias 'opencode-shell--permission-blocked-p
  #'opencode-shell--human-interaction-blocked-p)

(defun opencode-shell--session-permissions (items)
  "Return permission ITEMS belonging to the current session."
  (seq-filter
   (lambda (item)
     (let ((session (or (opencode-shell--get item 'sessionID)
                        (opencode-shell--get item 'sessionId))))
       (equal session opencode-shell--session-id)))
   items))

(defun opencode-shell--permission-id (item)
  "Return ITEM's usable permission ID, or nil."
  (let ((id (opencode-shell--get item 'id)))
    (and id (not (string-empty-p (format "%s" id))) (format "%s" id))))

(defun opencode-shell--deduplicate-permissions (items)
  "Return ID-bearing ITEMS once each, preserving their first order."
  (let (seen result)
    (dolist (item items (nreverse result))
      (when-let ((id (opencode-shell--permission-id item)))
        (unless (member id seen)
          (push id seen)
           (push item result))))))

(defun opencode-shell--session-questions (items)
  "Return question ITEMS belonging to the current session."
  (seq-filter
   (lambda (item)
     (equal (or (opencode-shell--get item 'sessionID)
                (opencode-shell--get item 'sessionId))
            opencode-shell--session-id))
   items))

(defun opencode-shell--question-id (item)
  "Return ITEM's usable question ID, or nil."
  (let ((id (opencode-shell--get item 'id)))
    (and id (not (string-empty-p (format "%s" id))) (format "%s" id))))

(defun opencode-shell--deduplicate-questions (items)
  "Return ID-bearing question ITEMS once each in first-seen order."
  (let (seen result)
    (dolist (item items (nreverse result))
      (when-let ((id (opencode-shell--question-id item)))
        (unless (member id seen)
          (push id seen)
          (push item result))))))

(defun opencode-shell--receive-questions (items &optional defer-render)
  "Store the authoritative current-session question snapshot ITEMS.
When DEFER-RENDER is non-nil, coalesce presentation at idle time."
  (opencode-shell--consume-question-refresh-pending)
  (setq opencode-shell--questions-pending
        (opencode-shell--deduplicate-questions
         (opencode-shell--session-questions items)))
  (when (opencode-shell--human-interaction-blocked-p)
    (setq opencode-shell--composer-visible nil
          opencode-shell--idle-completion-count 0)
    (opencode-shell--start-polling))
  (if defer-render
      (opencode-shell--schedule-render "questions")
    (opencode-shell--render-turns)
    (opencode-shell--render-permissions)
    (opencode-shell--log-lifecycle "questions")))

(defun opencode-shell--refresh-questions ()
  "Fetch `/question', deferring once when that request is in flight."
  (if (alist-get 'questions opencode-shell--in-flight)
      (setq opencode-shell--question-refresh-pending t)
    (opencode-shell--guarded-request
     'questions "GET" "/question"
     (lambda (items) (opencode-shell--receive-questions items t))
     nil #'opencode-shell--consume-question-refresh-pending)))

(defun opencode-shell--consume-question-refresh-pending ()
  "Reissue a deferred `/question' fetch."
  (when opencode-shell--question-refresh-pending
    (setq opencode-shell--question-refresh-pending nil)
    (opencode-shell--refresh-questions)))

(defun opencode-shell--resolved-permission (id)
  "Return the resolved permission record for ID."
  (seq-find (lambda (record) (equal id (opencode-shell--get record 'id)))
             opencode-shell--resolved-permissions))

(defun opencode-shell--permission-result-text (record)
  "Return the compact resolved permission line for RECORD."
  (format "PERMISSION %s: %s\n"
          (upcase (opencode-shell--get record 'reply))
          (opencode-shell--get record 'description)))

(defun opencode-shell--insert-permission-results (anchor)
  "Insert resolved permission records belonging after turn ANCHOR."
  (dolist (record (opencode-shell--deduplicate-permissions
                   opencode-shell--resolved-permissions))
    (when (equal anchor (opencode-shell--get record 'after-turn-id))
      (insert (propertize (opencode-shell--permission-result-text record)
                          'font-lock-face 'shadow
                          'read-only t 'rear-nonsticky '(read-only face))))))

(defun opencode-shell--insert-unanchored-permission-results ()
  "Insert resolved records whose anchor is absent from current turns."
  (let ((turn-ids (mapcar #'opencode-shell--turn-id opencode-shell--turns)))
    (dolist (record (opencode-shell--deduplicate-permissions
                     opencode-shell--resolved-permissions))
      (let ((anchor (opencode-shell--get record 'after-turn-id)))
        (when (and anchor (not (member anchor turn-ids)))
          (insert (propertize (opencode-shell--permission-result-text record)
                              'font-lock-face 'shadow
                              'read-only t 'rear-nonsticky '(read-only face))))))))

(defun opencode-shell--commit-permission-result (record)
  "Replace the active permission card with a fixed transcript RECORD."
  (opencode-shell--without-user-undo
   (let ((inhibit-read-only t)
         (composer-gap (- opencode-shell--composer-start
                          opencode-shell--permission-end)))
    (save-excursion
      (goto-char opencode-shell--permission-begin)
      (delete-region opencode-shell--permission-begin opencode-shell--permission-end)
      (insert (propertize (opencode-shell--permission-result-text record)
                          'font-lock-face 'shadow
                          'read-only t 'rear-nonsticky '(read-only face)))
      (set-marker opencode-shell--transcript-end (point))
      (set-marker opencode-shell--permission-begin (point))
      (set-marker opencode-shell--permission-end (point))
      (set-marker opencode-shell--composer-start (+ (point) composer-gap))))))

(defun opencode-shell--permission-at-point ()
  "Return the permission object at point, or the first pending request."
  (or (get-text-property (point) 'opencode-shell-permission)
      (car opencode-shell--permissions)
      (user-error "No pending permission")))

(defun opencode-shell--render-permissions ()
  "Render pending permissions as one boxed read-only region before composer."
  (opencode-shell--without-user-undo
   (let* ((draft (opencode-shell--composer-text))
         (offset (and (opencode-shell--in-composer-p)
                      (- (point) opencode-shell--composer-start)))
          (inhibit-read-only t))
    (save-excursion
      (when-let ((label-pos (text-property-any
                            (point-min) opencode-shell--composer-start
                            'opencode-shell-composer-label t)))
        (delete-region label-pos (+ label-pos (length "Prompt> ")))
        (setq opencode-shell--composer-label-visible nil))
      (goto-char opencode-shell--permission-begin)
      (delete-region opencode-shell--permission-begin opencode-shell--composer-start)
      (when (looking-back "Prompt> " (line-beginning-position))
        (delete-region (- (point) (length "Prompt> ")) (point)))
      (set-marker opencode-shell--permission-begin (point))
      (dolist (item (seq-take (opencode-shell--deduplicate-permissions
                               opencode-shell--permissions) 1))
        (let ((begin (point))
              (permission (format "%s" (or (opencode-shell--get item 'permission)
                                             "permission")))
              (context (opencode-shell--permission-context item)))
          (insert (propertize "┌─ PERMISSION ─────────────────────────────\n"
                              'font-lock-face 'opencode-shell-permission-face)
                  "│\n"
                  "│  " permission "\n"
                  (if context (concat "│\n│  " context "\n") "")
                  "│\n"
                  "│  C-c C-y once  C-c C-l always  C-c C-n reject\n"
                  "└───────────────────────────────────────────\n")
          (add-text-properties
           begin (point)
           `(read-only t rear-nonsticky (read-only keymap)
               keymap ,opencode-shell-permission-map
               opencode-shell-permission ,item))))
      (when (and (null opencode-shell--permissions)
                 opencode-shell--questions-pending)
        (let* ((item (car opencode-shell--questions-pending))
               (question (car (or (opencode-shell--get item 'questions)
                                   (list item))))
               (prompt (or (opencode-shell--get question 'question)
                           "Answer required"))
               (header (opencode-shell--get question 'header))
               (begin (point)))
          (insert (propertize "┌─ QUESTION ───────────────────────────────\n"
                              'font-lock-face 'opencode-shell-permission-face)
                  "│\n"
                  (if (and header (not (equal header prompt)))
                      (concat "│  " header "\n│\n")
                    "")
                  "│  " prompt "\n"
                  "│\n"
                  "│  RET/a answer  r reject\n"
                  "└───────────────────────────────────────────\n")
          (add-text-properties
           begin (point)
           `(read-only t rear-nonsticky (read-only keymap)
             keymap ,opencode-shell-question-map
             opencode-shell-question ,item))))
      (set-marker opencode-shell--permission-status-begin (point))
      (when-let ((status (opencode-shell--permission-status-display)))
        (insert status))
      (set-marker opencode-shell--permission-status-end (point))
      (set-marker opencode-shell--permission-end (point))
      (when (opencode-shell--composer-visible-p)
        (insert (propertize "Prompt> " 'read-only t
                            'opencode-shell-composer-label t
                            'rear-nonsticky '(read-only opencode-shell-composer-label))))
      (setq opencode-shell--composer-label-visible
            (opencode-shell--composer-visible-p))
      (set-marker opencode-shell--composer-start (point)))
    (when offset
      (goto-char (min (point-max) (+ opencode-shell--composer-start offset))))
    (unless (equal draft (opencode-shell--composer-text))
      (error "Permission rendering changed composer text")))))

(defun opencode-shell--refresh-permissions ()
  "Fetch the current `/permission' snapshot outside the normal poll cadence.
Defers to `opencode-shell--permission-refresh-pending' when a `/permission'
request is already in flight, so the deferred fetch still runs once that
request settles."
  (if (alist-get 'permissions opencode-shell--in-flight)
      (setq opencode-shell--permission-refresh-pending t)
    (opencode-shell--guarded-request
     'permissions "GET" "/permission"
     (lambda (items) (opencode-shell--receive-permissions items t))
     nil #'opencode-shell--consume-permission-refresh-pending)))

(defun opencode-shell--consume-permission-refresh-pending ()
  "Reissue a `/permission' fetch deferred while one was already in flight."
  (when opencode-shell--permission-refresh-pending
    (setq opencode-shell--permission-refresh-pending nil)
    (opencode-shell--refresh-permissions)))

(defun opencode-shell--receive-permissions (items &optional defer-render)
  "Store session-scoped permission ITEMS and update their display.
When DEFER-RENDER is non-nil, coalesce presentation at idle time."
  (opencode-shell--consume-permission-refresh-pending)
  (setq opencode-shell--permissions
        (seq-remove
         (lambda (item)
           (opencode-shell--resolved-permission
            (opencode-shell--permission-id item)))
         (opencode-shell--deduplicate-permissions
          (opencode-shell--session-permissions items))))
  (when (opencode-shell--permission-blocked-p)
    (setq opencode-shell--composer-visible nil
          opencode-shell--idle-completion-count 0)
    (opencode-shell--start-polling))
  (if defer-render
      (opencode-shell--schedule-render "permissions")
    (opencode-shell--render-turns)
    (opencode-shell--render-permissions)
    (opencode-shell--log-lifecycle "permissions")))

(defun opencode-shell--replace-composer (text &optional offset)
  "Replace the composer with TEXT and place point at OFFSET or its end."
  (opencode-shell--without-user-undo
   (let ((inhibit-read-only t))
    (delete-region opencode-shell--composer-start (point-max))
    (goto-char opencode-shell--composer-start)
    (insert text)
     (goto-char (+ opencode-shell--composer-start (or offset (length text)))))))

(defun opencode-shell--discard-turn-markers (turn)
  "Detach all rendered region markers owned by TURN."
  (dolist (marker (list (opencode-shell--turn-user-begin turn)
                        (opencode-shell--turn-user-end turn)
                        (opencode-shell--turn-response-begin turn)
                        (opencode-shell--turn-response-end turn)))
    (when (markerp marker) (set-marker marker nil))))

(defun opencode-shell--insert-turn-blocks (turn)
  "Insert immutable user and response blocks for TURN before the composer."
  (let ((user-begin (point)))
    (insert (propertize "USER>\n" 'font-lock-face 'opencode-shell-user-face
                        'rear-nonsticky '(font-lock-face))
            (or (opencode-shell--turn-user turn) "") "\n\n")
    (let ((user-end (point)))
      (opencode-shell--insert-permission-results
       (opencode-shell--turn-id turn))
      (let ((response-begin (point)))
      (insert (opencode-shell--tool-name-display turn))
      (if (eq (opencode-shell--turn-status turn) 'complete)
          (let ((answer (opencode-shell--assistant-display-text turn)))
          (insert (propertize "ASSISTANT>\n" 'face 'opencode-shell-assistant-face)
                  (or answer "")
                  (opencode-shell--turn-terminal-error-suffix turn)
                  "\n\n"))
        (insert (propertize
     (pcase (opencode-shell--turn-status turn)
       ('sending (opencode-shell--status-display "Sending"))
       ('thinking (opencode-shell--status-display "Thinking"))
       ('receiving (opencode-shell--status-display "Receiving"))
       ('recovering (opencode-shell--status-display "Recovering"))
       ('aborting (opencode-shell--status-display "Aborting"))
       ('error "Request state is uncertain; resync with g r\n\n")
       (_ (opencode-shell--status-display "Waiting for response")))
                 'face (if (eq (opencode-shell--turn-status turn) 'error)
                           'opencode-shell-error-face 'opencode-shell-waiting-face))))
      (let ((response-end (point)))
        (add-text-properties user-begin user-end
                             '(read-only t rear-nonsticky (read-only face)))
        (add-text-properties response-begin response-end
                             '(read-only t rear-nonsticky (read-only face)))
        (setf (opencode-shell--turn-user-begin turn) (copy-marker user-begin)
              (opencode-shell--turn-user-end turn) (copy-marker user-end)
               (opencode-shell--turn-response-begin turn) (copy-marker response-begin)
               (opencode-shell--turn-response-end turn) (copy-marker response-end)))))))

(defun opencode-shell--turn-rendered-p (turn)
  "Return non-nil when TURN owns valid rendered markers in this buffer."
  (let ((markers (list (opencode-shell--turn-user-begin turn)
                       (opencode-shell--turn-user-end turn)
                       (opencode-shell--turn-response-begin turn)
                       (opencode-shell--turn-response-end turn))))
    (and (seq-every-p (lambda (marker)
                        (and (markerp marker)
                             (marker-position marker)
                             (eq (marker-buffer marker) (current-buffer))
                             (<= (point-min) (marker-position marker) (point-max))))
                      markers)
         (apply #'<= (mapcar #'marker-position markers)))))

(defun opencode-shell--response-display (turn)
  "Return the propertized response display for TURN."
  (concat
   (opencode-shell--tool-name-display turn)
   (if (eq (opencode-shell--turn-status turn) 'complete)
       (concat (propertize "ASSISTANT>\n" 'font-lock-face 'opencode-shell-assistant-face)
               (opencode-shell--assistant-display-text turn)
               (opencode-shell--turn-terminal-error-suffix turn) "\n\n")
     (propertize
      (pcase (opencode-shell--turn-status turn)
        ('sending (opencode-shell--status-display "Sending"))
        ('thinking (opencode-shell--status-display "Thinking"))
        ('receiving (opencode-shell--status-display "Receiving"))
        ('recovering (opencode-shell--status-display "Recovering"))
        ('aborting (opencode-shell--status-display "Aborting"))
        ('error "Request failed\n\n")
        (_ (opencode-shell--status-display "Waiting for response")))
      'face (if (eq (opencode-shell--turn-status turn) 'error)
                'opencode-shell-error-face 'opencode-shell-waiting-face)))))

(defun opencode-shell--tool-name-display (turn)
  "Return payload-free tool names observed in TURN."
  (let (names)
    (dolist (part (opencode-shell--turn-parts turn))
      (when (member (format "%s" (opencode-shell--get part 'type))
                    '("tool" "tool_use" "tool-result"))
        (when-let ((name (or (opencode-shell--get part 'tool)
                             (opencode-shell--get part 'name))))
          (cl-pushnew (format "%s" name) names :test #'equal))))
    (if names
        (propertize
         (concat (mapconcat (lambda (name) (format "TOOL> %s" name))
                            (nreverse names) "\n")
                 "\n\n")
         'font-lock-face 'shadow)
      "")))

(defun opencode-shell--permission-status-turn ()
  "Return the latest nonterminal turn while permission blocks input."
  (and (opencode-shell--permission-blocked-p)
       (seq-find (lambda (turn)
                   (not (eq (opencode-shell--turn-status turn) 'complete)))
                 (reverse opencode-shell--turns))))

(defun opencode-shell--permission-status-display ()
  "Return transient status displayed below the permission card."
  (when-let ((turn (opencode-shell--permission-status-turn)))
    (opencode-shell--response-display turn)))

(defun opencode-shell--status-display (label)
  "Return LABEL with the current UI-only spinner frame."
  (format "%s %s\n\n" label
          (make-string (1+ opencode-shell--animation-frame)
                       opencode-shell--spinner-character)))

(defun opencode-shell--transcript-window ()
  "Return the preferred live window displaying the current transcript."
  (if (eq (window-buffer (selected-window)) (current-buffer))
      (selected-window)
    (get-buffer-window (current-buffer) t)))

(defun opencode-shell--assistant-display-text (turn)
  "Return TURN's assistant text adapted to the current presentation width."
  (let ((raw (or (opencode-shell--turn-assistant turn) "")))
    (if opencode-shell--table-render-width
        (opencode-shell-render-tables raw opencode-shell--table-render-width)
      raw)))

(defun opencode-shell--turn-terminal-error-suffix (turn)
  "Return a bounded, propertized terminal-error annotation for TURN, or \"\"."
  (if-let ((label (opencode-shell--turn-terminal-error turn)))
      (propertize (format "\n[%s]\n" label) 'font-lock-face 'opencode-shell-error-face)
    ""))

(defun opencode-shell--refresh-table-layout ()
  "Rerender completed tables when the transcript window width changes."
  (unless opencode-shell--table-rerendering
    (when-let* ((window (opencode-shell--transcript-window))
                (width (window-body-width window)))
      (unless (equal width opencode-shell--table-render-width)
        (let* ((opencode-shell--table-rerendering t)
               (windows (get-buffer-window-list (current-buffer) nil t))
               (starts (mapcar (lambda (item)
                                 (cons item (copy-marker (window-start item))))
                               windows)))
          (unwind-protect
              (progn
                (setq opencode-shell--table-render-width width)
                (when opencode-shell--turns
                  (opencode-shell--render-turns)))
            (dolist (entry starts)
              (when (window-live-p (car entry))
                (set-window-start (car entry) (cdr entry) t))
              (set-marker (cdr entry) nil))))))))

(defun opencode-shell--update-turn-response (turn)
  "Update only TURN's immutable response block."
  (let ((begin (opencode-shell--turn-response-begin turn))
        (end (opencode-shell--turn-response-end turn))
        (user-end-position
         (marker-position (opencode-shell--turn-user-end turn)))
        (display (opencode-shell--response-display turn))
        (inhibit-read-only t))
    (unless (string= display (buffer-substring begin end))
      (let ((position (marker-position begin)))
        (delete-region begin end)
        (goto-char position)
        ;; Advance every downstream transcript/composer marker past the
        ;; replacement, then restore this region's opening marker explicitly.
        (insert-before-markers display)
        (set-marker (opencode-shell--turn-user-end turn) user-end-position)
        (set-marker begin position)
        (add-text-properties begin (point)
                           '(read-only t rear-nonsticky (read-only face)))
        (set-marker end (point))))))

(defun opencode-shell--render-status-animation ()
  "Rerender only live transient status regions for the current frame."
  (opencode-shell--without-user-undo
    (let ((permission-turn (opencode-shell--permission-status-turn)))
      (save-excursion
        (dolist (turn opencode-shell--rendered-turns)
          (when (and (not (eq turn permission-turn))
                     (not (eq (opencode-shell--turn-status turn) 'complete))
                     (opencode-shell--turn-rendered-p turn))
            (opencode-shell--update-turn-response turn)))
      (when (and (markerp opencode-shell--permission-status-begin)
                 (marker-position opencode-shell--permission-status-begin)
                 (markerp opencode-shell--permission-status-end)
                 (marker-position opencode-shell--permission-status-end))
        (let* ((begin opencode-shell--permission-status-begin)
               (end opencode-shell--permission-status-end)
               (display (or (opencode-shell--permission-status-display) ""))
               (inhibit-read-only t))
          (unless (string= display (buffer-substring begin end))
            (let ((position (marker-position begin)))
              (delete-region begin end)
              (goto-char position)
              (insert-before-markers display)
              (set-marker begin position)
              (set-marker end (point))))))))))

(defun opencode-shell--animation-tick ()
  "Advance one UI-only spinner frame without issuing network requests."
  (cl-incf opencode-shell--animation-frame)
  (opencode-shell--render-status-animation))

(defun opencode-shell--render-turns (&optional force)
  "Render immutable turn blocks without changing composer bytes or point.
When FORCE is non-nil, rebuild every turn so anchored event positions settle."
  (opencode-shell--without-user-undo
   (let* ((composer-offset (and (opencode-shell--in-composer-p)
                                (- (point) opencode-shell--composer-start)))
         (composer-text (opencode-shell--composer-text))
         (old-point (point))
          (inhibit-read-only t))
    (let* ((known-count (length opencode-shell--rendered-turns))
           (rendered-valid
            (seq-every-p #'opencode-shell--turn-rendered-p
                         opencode-shell--rendered-turns))
           (append-only
            (and (not force)
                 rendered-valid
                 (<= known-count (length opencode-shell--turns))
                  (cl-every #'eq opencode-shell--rendered-turns
                           (seq-take opencode-shell--turns known-count)))))
      (delete-region opencode-shell--permission-begin opencode-shell--composer-start)
      (set-marker opencode-shell--permission-end opencode-shell--permission-begin)
      (set-marker opencode-shell--composer-start opencode-shell--permission-begin)
      (when append-only
        (dolist (turn opencode-shell--rendered-turns)
          (save-excursion (opencode-shell--update-turn-response turn))))
      (if append-only
          (save-excursion
            (when (< known-count (length opencode-shell--turns))
              (goto-char opencode-shell--transcript-end)
              (dolist (turn (nthcdr known-count opencode-shell--turns))
                (opencode-shell--insert-turn-blocks turn))))
        (dolist (turn opencode-shell--turns) (opencode-shell--discard-turn-markers turn))
        (delete-region opencode-shell--transcript-end opencode-shell--composer-start)
        (delete-region (point-min) opencode-shell--transcript-end)
        (goto-char (point-min))
        (opencode-shell--insert-permission-results nil)
        (opencode-shell--insert-unanchored-permission-results)
        (dolist (turn opencode-shell--turns)
          (opencode-shell--insert-turn-blocks turn))
        (setq opencode-shell--composer-label-visible nil))
      (when (and append-only
                 (not (opencode-shell--composer-visible-p))
                 opencode-shell--composer-label-visible
                 (>= opencode-shell--composer-start (length "Prompt> ")))
        (delete-region (- opencode-shell--composer-start (length "Prompt> "))
                       opencode-shell--composer-start)
        (setq opencode-shell--composer-label-visible nil))
      (when (and append-only
                 (opencode-shell--composer-visible-p)
                 (not opencode-shell--composer-label-visible)
                 (or opencode-shell--submit-in-flight
                     opencode-shell--rendered-turns))
        (when-let ((label-pos (text-property-any
                              (point-min) opencode-shell--composer-start
                              'opencode-shell-composer-label t)))
          (delete-region label-pos (+ label-pos (length "Prompt> "))))
        (goto-char opencode-shell--composer-start)
        (insert (propertize "Prompt> " 'read-only t
                            'opencode-shell-composer-label t
                            'rear-nonsticky '(read-only opencode-shell-composer-label)))
        (setq opencode-shell--composer-label-visible t))
      (setq opencode-shell--rendered-turns (copy-sequence opencode-shell--turns))
      (save-excursion
        (goto-char (max (point-min)
                        (- (point-max) (length composer-text))))
        (set-marker opencode-shell--transcript-end (point))
        (set-marker opencode-shell--permission-begin (point))
         (set-marker opencode-shell--permission-end (point))
         (set-marker opencode-shell--composer-start (point))))
    (opencode-shell--render-permissions)
    (if composer-offset
        (goto-char (min (point-max) (+ opencode-shell--composer-start composer-offset)))
      (goto-char (min old-point opencode-shell--transcript-end))))))

(defun opencode-shell--flush-render ()
  "Render the latest reconciled state when the current buffer is visible."
  (when (and opencode-shell--render-dirty
             (get-buffer-window (current-buffer) t))
    (let ((event opencode-shell--render-event))
      (setq opencode-shell--render-dirty nil
            opencode-shell--render-event nil)
      (opencode-shell--render-turns)
      (opencode-shell--log-lifecycle event)
      (force-mode-line-update))))

(defun opencode-shell--schedule-render (&optional event)
  "Mark presentation dirty and coalesce visible rendering under EVENT."
  (setq opencode-shell--render-dirty t
        opencode-shell--render-event (or event opencode-shell--render-event))
  (when (get-buffer-window (current-buffer) t)
    (opencode-shell-async-enqueue
     (current-buffer) 'render opencode-shell--generation
     #'opencode-shell--flush-render))
  (unless (get-buffer-window (current-buffer) t)
    (opencode-shell--log-lifecycle "render-deferred:hidden")))

(defun opencode-shell--render-if-visible ()
  "Schedule one render when a dirty transcript becomes visible."
  (when (and opencode-shell--render-dirty
             (get-buffer-window (current-buffer) t))
    (opencode-shell--schedule-render opencode-shell--render-event)))

(defun opencode-shell--render-messages (messages &optional sequence defer-render)
  "Reconcile chronological message envelopes from MESSAGES.
Render immediately unless DEFER-RENDER is non-nil."
  (when (or (null sequence) (> sequence opencode-shell--message-applied-sequence))
    (when sequence (setq opencode-shell--message-applied-sequence sequence))
    (setq opencode-shell--turns (opencode-shell--normalize-turns messages))
    (setq opencode-shell--request-status
        (cond ((seq-some (lambda (turn) (eq (opencode-shell--turn-status turn) 'thinking))
                         opencode-shell--turns) "thinking")
              ((seq-some (lambda (turn) (eq (opencode-shell--turn-status turn) 'receiving))
                         opencode-shell--turns) "receiving")
              ((seq-some (lambda (turn) (eq (opencode-shell--turn-status turn) 'recovering))
                         opencode-shell--turns) "recovering")
              ((seq-some (lambda (turn) (eq (opencode-shell--turn-status turn) 'waiting))
                         opencode-shell--turns) "waiting")
              ((seq-some (lambda (turn) (eq (opencode-shell--turn-status turn) 'error))
                         opencode-shell--turns) "error")
               (t "idle")))
    (when-let ((turn (seq-find
                      (lambda (entry)
                        (equal opencode-shell--submit-in-flight
                               (opencode-shell--turn-id entry)))
                      opencode-shell--turns)))
      (when (eq (opencode-shell--turn-status turn) 'complete)
        (unless (opencode-shell--permission-blocked-p)
          (setq opencode-shell--submit-in-flight nil
                opencode-shell--composer-visible t))))
    (when (and (not (opencode-shell--permission-blocked-p))
               (null opencode-shell--submit-in-flight)
               (equal opencode-shell--request-status "idle"))
      (opencode-shell--stop-polling))
    (let ((event (if sequence (format "messages:%d" sequence) "messages")))
      (if defer-render
          (opencode-shell--schedule-render event)
        (setq opencode-shell--render-dirty t
              opencode-shell--render-event event)
        (if (get-buffer-window (current-buffer) t)
            (opencode-shell--flush-render)
          ;; Direct callers, including deterministic tests and initial buffer
          ;; construction, require an immediate render even without a window.
          (let ((opencode-shell--render-dirty t))
            (opencode-shell--render-turns)
            (opencode-shell--log-lifecycle event)
            (force-mode-line-update)
            (setq opencode-shell--render-dirty nil
                  opencode-shell--render-event nil)))))))

(defun opencode-shell--complete-idle-turn ()
  "Record that idle status alone is not assistant completion evidence."
  (setq opencode-shell--idle-completion-count 0))

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

(defun opencode-shell--refresh-session-metadata ()
  "Refresh title metadata for the current session."
  (opencode-shell--guarded-request
   'session "GET" "/session"
   (lambda (sessions)
     (when-let ((session (seq-find
                          (lambda (item)
                            (equal opencode-shell--session-id
                                   (opencode-shell--get item 'id)))
                          sessions)))
       (setq opencode-shell--session-title
             (or (opencode-shell--get session 'title) "Untitled"))
       (force-mode-line-update)))
   nil nil))

(defun opencode-shell--resync (&optional full)
  "Resync polling state, and when FULL also metadata and capabilities."
  (interactive (list t))
  (when full (opencode-shell--refresh-session-metadata))
  (unless (alist-get 'messages opencode-shell--in-flight)
    (let ((sequence (cl-incf opencode-shell--message-request-sequence)))
      (setq opencode-shell--animation-frame 0)
      (when opencode-shell--turns
        (opencode-shell--render-status-animation))
      (setq opencode-shell--poll-heartbeat (% (1+ opencode-shell--poll-heartbeat) 3))
      (when opencode-shell--turns (opencode-shell--render-turns))
      (opencode-shell--guarded-request
       'messages
       "GET" (format "/session/%s/message" opencode-shell--session-id)
        (lambda (messages) (opencode-shell--render-messages messages sequence t)))))
  (opencode-shell--guarded-request
   'status "GET" "/session/status"
    (lambda (statuses)
      (setq opencode-shell--session-status statuses)
      (opencode-shell--complete-idle-turn)
      (opencode-shell--log-lifecycle "status")))
  (opencode-shell--guarded-request
   'permissions "GET" "/permission"
   (lambda (items) (opencode-shell--receive-permissions items t))
   nil #'opencode-shell--consume-permission-refresh-pending)
  (opencode-shell--refresh-questions)
  (when (and full
             (not opencode-shell--capabilities-loading))
    (let ((remaining 2) failed)
      (setq opencode-shell--capabilities-loading t)
      (cl-labels ((settle (failure)
                    (setq failed (or failed failure)
                          remaining (1- remaining))
                     (when (zerop remaining)
                       (setq opencode-shell--capabilities-loading nil
                             opencode-shell--capabilities-loaded (not failed))
                       (unless failed (opencode-shell--initialize-server-defaults))
                       (force-mode-line-update))))
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

(defun opencode-shell--select-model ()
  "Select a server-advertised model for subsequent prompts."
  (interactive)
  (unless opencode-shell--models (user-error "No models loaded; resync first"))
  (let* ((choice (completing-read "Model: " opencode-shell--models nil t))
         (entry (assoc choice opencode-shell--models)))
    (unless entry (user-error "Model is no longer available: %s" choice))
    (setq opencode-shell--selected-model (cdr entry)))
  (force-mode-line-update))

(defun opencode-shell--select-agent ()
  "Select a server-advertised agent name for subsequent prompts."
  (interactive)
  (unless opencode-shell--agents (user-error "No agents loaded; resync first"))
  (let* ((choice (completing-read "Agent: " opencode-shell--agents nil t))
         (entry (assoc choice opencode-shell--agents)))
    (unless entry (user-error "Agent is no longer available: %s" choice))
    (setq opencode-shell--selected-agent (car entry)))
  (force-mode-line-update))

(defun opencode-shell--prompt-body (text)
  "Return the legacy prompt payload for TEXT and current selections."
  (append `((parts . [((type . "text") (text . ,text))]))
          (and opencode-shell--selected-model
               `((model . ,opencode-shell--selected-model)))
          (and opencode-shell--selected-agent
               `((agent . ,opencode-shell--selected-agent)))))

(defun opencode-shell--submit ()
  "Commit and asynchronously submit the current multiline composer."
  (interactive)
  (let ((text (opencode-shell--composer-text)))
    (when opencode-shell--submit-in-flight
      (user-error "A prompt delivery is already being reconciled"))
    (when (string-blank-p text) (user-error "Prompt is blank"))
    (opencode-shell--settle-superseded-turns opencode-shell--turns t)
    (let ((turn (opencode-shell--make-turn
                  :id (format "msg_%s_%d" (format-time-string "%s%N")
                              (cl-incf opencode-shell--turn-counter))
                  :user text :status 'sending)))
      (setq opencode-shell--turns (append opencode-shell--turns (list turn))
            opencode-shell--request-status "sending"
            opencode-shell--composer-visible nil
            opencode-shell--idle-completion-count 0
            opencode-shell--submit-in-flight (opencode-shell--turn-id turn))
       (opencode-shell--replace-composer "")
       (opencode-shell--render-turns)
       (setq buffer-undo-list nil)
       (undo-boundary)
       (opencode-shell--start-polling)
      (opencode-shell--log-lifecycle "submit" t)
      (force-mode-line-update)
       (opencode-shell--request
        "POST" (format "/session/%s/prompt_async" opencode-shell--session-id)
        (lambda (_)
         (setf (opencode-shell--turn-status turn) 'waiting)
         (opencode-shell--render-turns)
         (opencode-shell--log-lifecycle "submit-ack")
         (opencode-shell--resync))
        (cons `(messageID . ,(opencode-shell--turn-id turn))
              (opencode-shell--prompt-body text)) nil
        (lambda ()
         (setf (opencode-shell--turn-status turn) 'recovering)
         (setq opencode-shell--request-status "recovering")
         (opencode-shell--render-turns)
         (opencode-shell--log-lifecycle "submit-error" t)
         (opencode-shell--resync)
         (force-mode-line-update))))))

(defun opencode-shell--abort ()
  "Abort work in the current session."
  (interactive)
  (setq opencode-shell--request-status "aborting")
  (let ((target (car (last opencode-shell--turns))))
    (when (and target
               (not (eq (opencode-shell--turn-status target) 'complete)))
      (setf (opencode-shell--turn-status target) 'aborting)
      (opencode-shell--render-turns))
    (opencode-shell--request "POST" (format "/session/%s/abort" opencode-shell--session-id)
                             (lambda (_)
                               (when (and target
                                          (not (eq (opencode-shell--turn-status target)
                                                   'complete)))
                                 (setf (opencode-shell--turn-status target) 'complete
                                       (opencode-shell--turn-terminal-error target)
                                       "MessageAbortedError: Aborted"
                                       (opencode-shell--turn-locally-settled target) t))
                               (when (or (null target)
                                         (equal opencode-shell--submit-in-flight
                                                (opencode-shell--turn-id target)))
                                 (setq opencode-shell--submit-in-flight nil))
                               (unless (or opencode-shell--submit-in-flight
                                           (opencode-shell--permission-blocked-p))
                                 (setq opencode-shell--composer-visible t))
                               (opencode-shell--render-turns)
                               (opencode-shell--log-lifecycle "abort-ack" t)
                               (opencode-shell--resync)) '())))

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
    (if (and (listp value) (seq-every-p #'stringp value))
        (string-join value " · ")
      (format "%s" value)))
   120 nil nil t))

(defun opencode-shell--permission-description (item)
  "Return concise, bounded user-facing context for permission ITEM."
  (let ((permission (format "%s" (or (opencode-shell--get item 'permission)
                                       "permission")))
        (context (opencode-shell--permission-context item)))
    (if context
        (format "%s: %s" permission context)
      permission)))

(defun opencode-shell--permission-context (item)
  "Return ITEM's concise actionable command or target, or nil."
  (let ((metadata (opencode-shell--get item 'metadata))
        (tool (opencode-shell--get item 'tool)))
    (when-let ((context (or (opencode-shell--get metadata 'command)
                            (opencode-shell--get tool 'command)
                            (car-safe (opencode-shell--get item 'patterns)))))
      (opencode-shell--permission-value context))))

(defun opencode-shell--permissions ()
  "Move to the first pending inline permission request."
  (interactive)
  (unless opencode-shell--permissions (user-error "No pending permission"))
  (goto-char opencode-shell--permission-begin))

(defun opencode-shell--permission-reply (reply)
  "Send REPLY for the inline permission at point."
  (let* ((item (opencode-shell--permission-at-point))
         (id (opencode-shell--permission-id item)))
    (when opencode-shell--permission-sending
      (user-error "Permission reply already in progress"))
    (setq opencode-shell--permission-sending id)
    (opencode-shell--log-lifecycle "permission-reply" t)
    (opencode-shell--request
     "POST" (format "/permission/%s/reply" id)
     (lambda (_)
        (unless (opencode-shell--resolved-permission id)
          (let ((record `((id . ,id) (reply . ,reply)
                          (description . ,(opencode-shell--permission-description item))
                          (after-turn-id . ,(when-let ((turn (car (last opencode-shell--turns))))
                                              (opencode-shell--turn-id turn))))))
            (setq opencode-shell--resolved-permissions
                  (append opencode-shell--resolved-permissions (list record)))
            (opencode-shell--commit-permission-result record)))
       (when (equal opencode-shell--permission-sending id)
         (setq opencode-shell--permission-sending nil))
        (setq opencode-shell--permissions
              (seq-remove (lambda (entry)
                            (equal id (opencode-shell--permission-id entry)))
                          opencode-shell--permissions))
        (opencode-shell--render-turns t)
        (opencode-shell--refresh-permissions)
        (unless (opencode-shell--permission-blocked-p)
          (opencode-shell--resync))
         (opencode-shell--log-lifecycle "permission-reply-ok")
         (message "Permission %s" reply))
     `((reply . ,reply)) nil
     (lambda ()
        (when (equal opencode-shell--permission-sending id)
          (setq opencode-shell--permission-sending nil))
        (opencode-shell--refresh-permissions)
        (opencode-shell--log-lifecycle "permission-reply-error" t)
        (message "Permission reply failed; refreshing pending permissions")))))

(defun opencode-shell--permission-allow-once ()
  "Allow the inline permission once."
  (interactive)
  (opencode-shell--permission-reply "once"))

(defun opencode-shell--permission-allow-always ()
  "Always allow the inline permission."
  (interactive)
  (opencode-shell--permission-reply "always"))

(defun opencode-shell--permission-reject ()
  "Reject the inline permission."
  (interactive)
  (opencode-shell--permission-reply "reject"))

(defun opencode-shell--questions ()
  "Explicitly answer or reject a pending question."
  (interactive)
  (if-let ((item (or (get-text-property (point) 'opencode-shell-question)
                     (car opencode-shell--questions-pending))))
      (opencode-shell--question-reply item nil)
    (opencode-shell--choose-pending
     "question" (lambda (item) (opencode-shell--question-reply item t)))))

(defun opencode-shell--question-reply (item confirm)
  "Answer ITEM, prompting for rejection first when CONFIRM is non-nil."
  (when opencode-shell--question-sending
    (user-error "Question reply already in progress"))
  (if (and confirm (not (yes-or-no-p "Answer this question? (No rejects) ")))
      (opencode-shell--question-reject item)
    (let* ((id (opencode-shell--question-id item))
           (answers (vconcat
                     (mapcar #'opencode-shell--question-answer
                             (or (opencode-shell--get item 'questions)
                                 (list item))))))
      (setq opencode-shell--question-sending id)
      (opencode-shell--request
       "POST" (format "/question/%s/reply" id)
       (lambda (_)
         (setq opencode-shell--question-sending nil
               opencode-shell--questions-pending
               (seq-remove (lambda (entry)
                            (equal id (opencode-shell--question-id entry)))
                           opencode-shell--questions-pending))
         (opencode-shell--render-permissions)
         (opencode-shell--resync nil)
         (message "Question reply sent"))
       `((answers . ,answers)) nil
       (lambda ()
         (setq opencode-shell--question-sending nil)
         (opencode-shell--resync nil)
         (message "Question reply failed"))))))

(defun opencode-shell--question-reject (&optional item)
  "Reject pending question ITEM or the current inline question."
  (interactive)
  (when opencode-shell--question-sending
    (user-error "Question reply already in progress"))
  (setq item (or item (get-text-property (point) 'opencode-shell-question)
                 (car opencode-shell--questions-pending)
                 (user-error "No pending question")))
  (let ((id (opencode-shell--question-id item)))
    (setq opencode-shell--question-sending id)
    (opencode-shell--request
     "POST" (format "/question/%s/reject" id)
     (lambda (_)
       (setq opencode-shell--question-sending nil
             opencode-shell--questions-pending
             (seq-remove (lambda (entry)
                         (equal id (opencode-shell--question-id entry)))
                        opencode-shell--questions-pending))
       (opencode-shell--render-permissions)
       (opencode-shell--resync nil)
       (message "Question rejected"))
     '() nil
     (lambda ()
       (setq opencode-shell--question-sending nil)
       (opencode-shell--resync nil)
       (message "Question rejection failed")))))

(defun opencode-shell--setup-evil ()
  "Install Evil integration when Evil is available."
  (declare-function evil-set-initial-state "evil-core")
  (declare-function evil-define-key* "evil-core")
  (evil-set-initial-state 'opencode-shell-mode 'normal)
  (evil-set-initial-state 'opencode-shell-sessions-mode 'normal)
  (evil-define-key* 'normal opencode-shell-sessions-mode-map (kbd "g") nil)
  (evil-define-key* 'normal opencode-shell-mode-map
    (kbd "RET") #'opencode-shell--submit
    (kbd "<return>") #'opencode-shell--submit
    (kbd "g r") #'opencode-shell--resync
    (kbd "?") #'opencode-shell-help
    (kbd "C-c C-c") #'opencode-shell--submit
    (kbd "C-c C-v") #'opencode-shell--select-model
    (kbd "C-c C-m") #'opencode-shell--select-agent)
  (evil-define-key* 'insert opencode-shell-mode-map
    (kbd "?") #'self-insert-command
    (kbd "RET") #'newline
    (kbd "<return>") #'newline)
  (evil-define-key* 'normal opencode-shell-sessions-mode-map
    (kbd "RET") #'opencode-shell--open-at-point
    (kbd "g r") #'opencode-shell--refresh
    (kbd "c") #'opencode-shell--create-session
    (kbd "/") #'opencode-shell--filter
    (kbd "d") #'opencode-shell--delete-session
    (kbd "?") #'opencode-shell-sessions-help))

(defun opencode-shell--evil-move-to-composer ()
  "Move point to the composer when entering Evil insert state."
  (when (and (derived-mode-p 'opencode-shell-mode)
             (not (opencode-shell--in-composer-p)))
    (goto-char (point-max))))

(defun opencode-shell--enable-evil-composer-hook ()
  "Install the buffer-local Evil insert-state hook."
  (when (boundp 'evil-insert-state-entry-hook)
    (add-hook 'evil-insert-state-entry-hook
              #'opencode-shell--evil-move-to-composer nil t)))

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

(defun opencode-shell--attempt-current-p (key attempt)
  "Return non-nil when ATTEMPT is KEY's current start attempt."
  (eq attempt (plist-get (gethash key opencode-shell--servers) :attempt)))

(defun opencode-shell--finish-start (key attempt)
  "Finish coalesced server start KEY for ATTEMPT."
  (let* ((state (gethash key opencode-shell--servers))
         (callbacks (plist-get state :callbacks)))
    (when (opencode-shell--attempt-current-p key attempt)
      (setq state (plist-put state :callbacks nil)
            state (plist-put state :checking nil)
            state (plist-put state :starting nil)
            state (plist-put state :attempt nil))
      (puthash key state opencode-shell--servers)
      (dolist (entry (nreverse callbacks))
        (funcall (car entry) (cdr entry))))))

(defun opencode-shell--fail-start (key attempt message-text)
  "Abandon KEY's ATTEMPT and report MESSAGE-TEXT."
  (let* ((state (gethash key opencode-shell--servers))
         (process (plist-get state :process)))
    (when (opencode-shell--attempt-current-p key attempt)
      (when (and (plist-get state :starting)
                 (processp process) (process-live-p process))
        (delete-process process))
      (remhash key opencode-shell--servers)
      (message "OpenCode: %s" message-text))))

(defun opencode-shell--process-tail (process)
  "Return PROCESS's bounded output tail, or nil when empty."
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (= (point-min) (point-max))
          (buffer-substring-no-properties
           (max (point-min) (- (point-max) opencode-shell--process-tail-limit))
           (point-max)))))))

(defun opencode-shell--await-server (profile deadline attempt)
  "Poll PROFILE health until DEADLINE for ATTEMPT."
  (let* ((key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers)))
    (when (and (opencode-shell--attempt-current-p key attempt)
               (plist-get state :starting))
     (if (> (float-time) deadline)
          (opencode-shell--fail-start
           key attempt (or (plist-get state :exit-error)
                           "server startup timed out"))
       (opencode-shell--server-ready
        profile
        (lambda (ready &optional _)
          (when (opencode-shell--attempt-current-p key attempt)
            (if ready
                (opencode-shell--finish-start key attempt)
              (when (plist-get (gethash key opencode-shell--servers) :starting)
                (run-at-time .2 nil #'opencode-shell--await-server
                             profile deadline attempt))))))))))

(defun opencode-shell--process-filter (process output)
  "Append OUTPUT to PROCESS buffer, retaining only a bounded tail."
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max)) (insert output)
        (when (> (buffer-size) opencode-shell--process-tail-limit)
          (delete-region (point-min)
                         (- (point-max) opencode-shell--process-tail-limit)))))))

(defun opencode-shell--process-sentinel (key attempt process _event)
  "Handle an owned PROCESS exiting during startup for KEY."
  (when (and (opencode-shell--attempt-current-p key attempt)
             (memq (process-status process) '(exit signal failed))
             (eq process (plist-get (gethash key opencode-shell--servers) :process)))
    (if (plist-get (gethash key opencode-shell--servers) :starting)
        (let* ((state (gethash key opencode-shell--servers))
               (tail (opencode-shell--process-tail process))
               (message-text
                (format "server exited before becoming healthy (status %s)%s"
                        (process-exit-status process)
                        (if tail (format ":\n%s" tail) ""))))
          ;; Another concurrent starter may have won the port.  Keep polling
          ;; this attempt so a healthy endpoint can be adopted safely.
          (setq state (plist-put state :process nil)
                state (plist-put state :owned nil)
                state (plist-put state :exit-error message-text))
          (puthash key state opencode-shell--servers))
      (remhash key opencode-shell--servers))))

(defun opencode-shell--spawn-server (profile attempt)
  "Start PROFILE exactly once and begin bounded health polling."
  (let* ((key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers))
         (command (plist-get profile :start-command)))
    (unless (and (listp command) command (seq-every-p #'stringp command))
      (when (opencode-shell--attempt-current-p key attempt)
        (remhash key opencode-shell--servers))
      (user-error ":start-command must be a non-empty argv list"))
    (when (opencode-shell--attempt-current-p key attempt)
      (condition-case err
        (let* ((default-directory (or (plist-get profile :server-directory)
                                      default-directory))
               (process (make-process
                         :name (format "opencode-%s" key)
                         :buffer (get-buffer-create (format " *opencode-%s*" key))
                         :command command :noquery t :connection-type 'pipe
                         :filter #'opencode-shell--process-filter
                          :sentinel (lambda (process event)
                                      (opencode-shell--process-sentinel
                                       key attempt process event)))))
          (puthash key (append (list :process process :owned t :starting t)
                               state)
                   opencode-shell--servers)
          (if (process-live-p process)
              (opencode-shell--await-server
               profile (+ (float-time) (or (plist-get profile :startup-timeout) 10))
               attempt)
            (opencode-shell--process-sentinel key attempt process "exited")))
       (error (opencode-shell--fail-start
               key attempt
               (format "could not start server: %s" (error-message-string err))))))))

(defun opencode-shell--start-server (&optional profile callback)
  "Start local PROFILE server and invoke CALLBACK when healthy.
Concurrent starts for one server are coalesced.  Remote profiles are never
auto-started."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
          (key (opencode-shell--server-key profile))
          (state (gethash key opencode-shell--servers))
          (command (plist-get profile :start-command)))
    (let ((config (opencode-shell--validate-server-profile profile state)))
    (cond
     ((opencode-shell--profile-remote-p profile)
      (when (called-interactively-p 'interactive) (user-error "Remote profiles are not auto-started")))
     ((or (plist-get state :checking) (plist-get state :starting))
      (when callback
        (puthash key (plist-put state :callbacks
                                (cons (cons callback profile)
                                      (plist-get state :callbacks)))
                  opencode-shell--servers)))
     ((not command)
      (user-error "Profile has no :start-command"))
     ((and (plist-get state :owned)
           (process-live-p (plist-get state :process)))
      (let ((attempt (gensym "opencode-start-")))
       (puthash key (plist-put (plist-put (plist-put state :checking t)
                                         :attempt attempt)
                               :callbacks (and callback (list (cons callback profile))))
                 opencode-shell--servers)
       (opencode-shell--server-ready
        profile (lambda (_ready &optional _)
                  (when (opencode-shell--attempt-current-p key attempt)
                    (opencode-shell--finish-start key attempt))))))
     (t
      (let ((attempt (gensym "opencode-start-")))
       (puthash key (list :checking t :attempt attempt :config config
                          :callbacks (and callback (list (cons callback profile))))
                opencode-shell--servers)
       (opencode-shell--server-ready
        profile (lambda (ready &optional _)
                  (when (opencode-shell--attempt-current-p key attempt)
                    (if ready
                        (opencode-shell--finish-start key attempt)
                      (opencode-shell--spawn-server profile attempt)))))))))))

(defun opencode-shell--stop-server (&optional profile)
  "Stop PROFILE server only when this client owns its process."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
          (key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers))
         (process (plist-get state :process)))
    (unless (and (plist-get state :owned) (process-live-p process))
      (user-error "OpenCode server is not owned by this client"))
    (delete-process process)
    (remhash key opencode-shell--servers)))

(defun opencode-shell--restart-server (&optional profile)
  "Restart an owned local PROFILE server."
  (let ((profile (or profile opencode-shell--profile (opencode-shell--read-profile))))
    (opencode-shell--stop-server profile)
    (opencode-shell--start-server profile)))

(defun opencode-shell--stop-all-servers ()
  "Stop owned servers whose shared lifecycle requests exit cleanup."
  (let (owned)
    (maphash (lambda (_key state)
               (when (and (plist-get state :owned)
                          (plist-get (plist-get state :config) :stop-on-exit))
                 (push (plist-get state :process) owned)))
             opencode-shell--servers)
    (clrhash opencode-shell--servers)
    (dolist (process owned)
      (when (process-live-p process) (delete-process process)))))

(add-hook 'kill-emacs-hook #'opencode-shell--stop-all-servers)

(defun opencode-shell--open-sessions (profile &optional directory current-window)
  "Prepare PROFILE and open its session browser for DIRECTORY.
When CURRENT-WINDOW is non-nil, display it in the selected window."
  (setq directory (or directory (opencode-shell--current-server-directory profile)))
  (if (and (plist-get profile :start-command)
           (not (opencode-shell--profile-remote-p profile)))
      (opencode-shell--start-server
       profile (lambda (ready) (opencode-shell--sessions directory ready current-window)))
    (opencode-shell--sessions directory profile current-window)))

;;;###autoload
(defun opencode-shell (&optional profile)
  "Select an OpenCode server and open sessions for the current directory."
  (interactive (list (opencode-shell--read-profile)))
  (opencode-shell--open-sessions
   (opencode-shell--resolve-or-read-profile profile)))

;;;###autoload
(defun opencode-shell-start (&optional profile)
  "Select an OpenCode PROFILE and start a session in the current directory."
  (interactive (list (opencode-shell--read-profile)))
  (opencode-shell--start-session
   (opencode-shell--resolve-or-read-profile profile)))

;;;###autoload
(defun opencode-shell-switch-buffer ()
  "Select and display a live OpenCode transcript buffer."
  (interactive)
  (let* ((buffers (seq-filter
                   (lambda (buffer)
                      (with-current-buffer buffer
                        (derived-mode-p 'opencode-shell-mode)))
                   (buffer-list)))
         (candidates
          (mapcar (lambda (buffer)
                    (cons (with-current-buffer buffer
                            (format "%s  —  %s"
                                    (or opencode-shell--session-title "Untitled")
                                    (buffer-name buffer)))
                          buffer))
                  buffers)))
    (unless candidates (user-error "No OpenCode buffers"))
    (switch-to-buffer
     (cdr (assoc (completing-read "OpenCode shell: " candidates nil t)
                 candidates)))))

(defun opencode-shell--session-browser-candidate (profile directory active)
  "Return a styled PROFILE and DIRECTORY completion candidate.
ACTIVE means that their session browser is already live."
  (cons (propertize (format "%s %s : %s"
                            (if active "●" "○")
                            (opencode-shell--profile-name profile) directory)
                    'face (if active 'opencode-shell-active-session-face
                            'opencode-shell-recent-session-face))
        (cons profile directory)))

;;;###autoload
(defun opencode-shell-find-session (&optional profile)
  "Select a live or recent PROFILE directory and open its session browser."
  (interactive)
  (opencode-shell--validate-profiles)
  (let* ((profiles (if profile
                       (list (opencode-shell--resolve-or-read-profile profile))
                     opencode-shell-profiles))
         (pending (length profiles))
         (locations (make-hash-table :test #'equal)))
    (unless profiles (user-error "No OpenCode profiles configured"))
    (cl-labels
         ((remember (candidate-profile directory updated active)
            (when (stringp directory)
              (setq directory (concat (directory-file-name directory) "/"))
              (let* ((key (cons (opencode-shell--profile-key candidate-profile) directory))
                    (existing (gethash key locations)))
               (when (or (null existing) active (> updated (nth 2 existing)))
                 (puthash key (list candidate-profile directory updated active) locations)))))
         (finish ()
           (when (= (cl-decf pending) 0)
             (let (entries)
               (maphash (lambda (_ entry) (push entry entries)) locations)
                (setq entries (sort entries
                                    (lambda (a b)
                                      (if (eq (nth 3 a) (nth 3 b))
                                          (string-lessp
                                           (downcase (format "%s : %s"
                                                             (opencode-shell--profile-name (nth 0 a))
                                                             (nth 1 a)))
                                           (downcase (format "%s : %s"
                                                             (opencode-shell--profile-name (nth 0 b))
                                                             (nth 1 b))))
                                        (nth 3 a)))))
                (unless entries (user-error "No OpenCode session locations"))
                 (condition-case nil
                     (let* ((active (seq-filter (lambda (entry) (nth 3 entry)) entries))
                            (inactive (seq-remove (lambda (entry) (nth 3 entry)) entries))
                            (make-candidate
                             (lambda (entry)
                               (opencode-shell--session-browser-candidate
                                (nth 0 entry) (nth 1 entry) (nth 3 entry))))
                            (separator (propertize "──────── inactive ────────" 'face 'shadow))
                            (candidates
                             (append (mapcar make-candidate active)
                                     (and inactive (list (cons separator nil)))
                                     (mapcar make-candidate inactive)))
                            location)
                       (while (null location)
                         (let ((choice (completing-read
                                        "OpenCode profile : path: " candidates nil t)))
                           (setq location (cdr (assoc choice candidates)))))
                       (opencode-shell--open-sessions
                        (car location) (cdr location) t))
                  (quit nil))))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'opencode-shell-sessions-mode)
            (remember opencode-shell--profile opencode-shell--directory
                      most-positive-fixnum t))))
      (dolist (location opencode-shell--recent-session-locations)
        (when-let ((candidate-profile
                    (seq-find (lambda (item)
                                (equal (car location)
                                       (opencode-shell--profile-key item)))
                              profiles)))
          (remember candidate-profile (cdr location) 0 nil)))
      (dolist (candidate-profile profiles)
        (let ((buffer (generate-new-buffer " *opencode-find-session*")))
          (with-current-buffer buffer
            (setq-local opencode-shell--profile candidate-profile
                        opencode-shell--base-url
                        (or (plist-get candidate-profile :base-url) opencode-shell-base-url))
            (opencode-shell--request
             "GET" "/session"
             (lambda (response)
               (dolist (session
                        (mapcar
                         (lambda (item)
                           (opencode-shell--effective-session
                            item candidate-profile))
                         (opencode-shell--normalize-sessions response)))
                 (remember candidate-profile (opencode-shell--get session 'directory)
                           (opencode-shell--time session) nil))
               (kill-buffer buffer)
               (finish))
             nil '((limit . 1000))
             (lambda ()
               (kill-buffer buffer)
               (finish)))))))))

;;;###autoload
(defun opencode-shell-status (profile)
  "Select PROFILE and report its health and client ownership."
  (interactive (list (opencode-shell--read-profile)))
  (opencode-shell--server-ready
   (opencode-shell--resolve-or-read-profile profile)
   (lambda (ready checked-profile)
     (let* ((state (gethash (opencode-shell--server-key checked-profile)
                            opencode-shell--servers))
            (ownership (if (plist-get state :owned) "owned" "external")))
       (message "OpenCode %s: %s (%s)"
                (opencode-shell--profile-name checked-profile)
                (if ready "ready" "unreachable") ownership)))))

;;;###autoload
(defun opencode-shell-restart (profile)
  "Select and restart an owned local PROFILE server."
  (interactive (list (opencode-shell--read-profile)))
  (opencode-shell--restart-server
   (opencode-shell--resolve-or-read-profile profile)))

;;;###autoload
(defun opencode-shell-reload ()
  "Reload OpenCode Shell sources and refresh existing package buffers."
  (interactive)
  (opencode-shell-async-reset)
  (let* ((main (or load-file-name (locate-library "opencode-shell")))
         (directory (and main (file-name-directory main)))
         (render (and directory (expand-file-name "opencode-shell-render.el" directory)))
         (async (and directory (expand-file-name "opencode-shell-async.el" directory)))
         (source (and directory (expand-file-name "opencode-shell.el" directory))))
    (unless (and render async source
                 (file-exists-p render)
                 (file-exists-p async)
                 (file-exists-p source))
      (user-error "Cannot locate OpenCode Shell source files"))
    (load render nil nil t)
    (load async nil nil t)
    (load source nil nil t)
    (when-let ((setting (locate-library "opencode-shell-setting")))
      (load setting nil nil t))
    (opencode-shell--register-profile-commands)
    (when (fboundp 'opencode-shell--setup-evil)
      (when (featurep 'evil) (opencode-shell--setup-evil)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (cond
         ((derived-mode-p 'opencode-shell-sessions-mode)
          (use-local-map opencode-shell-sessions-mode-map)
          (opencode-shell--setup-sessions-evil-buffer)
          (setq-local header-line-format nil))
         ((derived-mode-p 'opencode-shell-mode)
          (use-local-map opencode-shell-mode-map)
          (setq-local header-line-format '(:eval (opencode-shell--header))
                      mode-line-process '(:eval (opencode-shell--mode-line-status)))
          (when (markerp opencode-shell--composer-start)
            (goto-char (point-max)))))))
    (force-mode-line-update t)
    (message "Reloaded OpenCode Shell")))

(opencode-shell--register-profile-commands)

(dolist (command opencode-shell--mode-commands)
  (put command 'completion-predicate #'ignore))

(with-eval-after-load 'evil
  (opencode-shell--setup-evil)
  (add-hook 'opencode-shell-mode-hook #'opencode-shell--enable-evil-composer-hook)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-shell-mode)
        (opencode-shell--enable-evil-composer-hook)))))

(provide 'opencode-shell)
;;; opencode-shell.el ends here
