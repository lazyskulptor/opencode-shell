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
(require 'opencode-shell-state)
(require 'opencode-shell-interaction)
(require 'opencode-shell-response)
(require 'opencode-shell-recovery)

(defgroup opencode-shell nil "Unofficial Emacs client for OpenCode." :group 'tools)

(defcustom opencode-shell-base-url "http://127.0.0.1:4199"
  "OpenCode server base URL."
  :type 'string :group 'opencode-shell)

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
Supported keys include `:name', `:base-url', optional remote `:directory',
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
    opencode-shell--permission-reject opencode-shell--questions
    opencode-shell--previous-turn opencode-shell--next-turn)
  "Private interactive commands used only by OpenCode mode maps.")
(defvar-local opencode-shell--profile nil)
(defvar-local opencode-shell--base-url nil)

(defconst opencode-shell--process-tail-limit 4096)
(defconst opencode-shell--snapshot-request-timeout 20
  "Maximum seconds a read-only snapshot may hold an in-flight guard.")

(defvar projectile-mode)
(declare-function projectile-project-root "projectile")

(defun opencode-shell--default-profile ()
  "Return the backwards-compatible implicit profile."
  (list :name "default" :base-url opencode-shell-base-url))

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
        :restart-command (copy-sequence (plist-get profile :restart-command))
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

(defun opencode-shell--profile-client-directory (directory profile)
  "Expand DIRECTORY's TRAMP home alias using PROFILE's configured home."
  (let* ((root (plist-get profile :directory))
         (remote (file-remote-p directory))
         (localname (and remote (file-remote-p directory 'localname))))
    (if (and root remote
             (equal remote (file-remote-p root))
             (string-prefix-p "~/" localname))
        (concat remote
                (expand-file-name (substring localname 2)
                                  (file-remote-p root 'localname)))
      directory)))

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

(defun opencode-shell--profile-for-directory (&optional directory)
  "Return the most specific profile containing DIRECTORY.
Signal a user error when no configured profile contains it or equally specific
profiles make the result ambiguous."
  (unless opencode-shell-profiles
    (user-error "No OpenCode profiles configured"))
  (opencode-shell--validate-profiles)
  (let* ((directory (opencode-shell--canonical-directory
                     (or directory default-directory)))
         (remote (file-remote-p directory))
         best best-length ambiguous local-defaults)
    (dolist (profile opencode-shell-profiles)
      (when-let ((root (opencode-shell--canonical-directory
                        (plist-get profile :directory))))
        (when (and (equal remote (file-remote-p root))
                   (string-prefix-p root
                                    (opencode-shell--canonical-directory
                                     (opencode-shell--profile-client-directory
                                      directory profile))))
          (let ((length (length root)))
            (cond ((or (null best-length) (> length best-length))
                   (setq best profile best-length length ambiguous nil))
                  ((= length best-length)
                   (setq ambiguous t))))))
      (when (and (not remote)
                 (not (plist-get profile :directory))
                 (not (opencode-shell--profile-remote-p profile)))
        (push profile local-defaults)))
    (cond (ambiguous
           (user-error "Multiple OpenCode profiles match %s" directory))
          (best best)
          ((= (length local-defaults) 1) (car local-defaults))
          ((> (length local-defaults) 1)
           (user-error "Multiple local OpenCode profiles match %s" directory))
           (t (user-error "No OpenCode profile matches %s" directory)))))

(defun opencode-shell--profile-for-command (value)
  "Resolve explicit profile VALUE or infer one from `default-directory'."
  (cond ((and value (listp value)) value)
        (value
         (or (opencode-shell--resolve-profile value)
             (user-error "Unknown OpenCode profile: %s" value)))
        (t (opencode-shell--profile-for-directory))))

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
  (let* ((directory (opencode-shell--profile-client-directory
                     (opencode-shell--project-directory) profile))
         (client-root (plist-get profile :directory))
         (remote (file-remote-p directory))
         (root-remote (and client-root (file-remote-p client-root)))
         (server-directory
          (and (or (not root-remote)
                   (and (equal remote root-remote)
                        (string-prefix-p
                         (file-name-as-directory
                          (file-remote-p client-root 'localname))
                         (file-name-as-directory
                          (file-remote-p directory 'localname)))))
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
          (opencode-shell--get session 'id) directory profile))
       nil nil (lambda () (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defun opencode-shell--start-server-for-command (profile callback)
  "Start PROFILE for a one-shot browser or session command."
  (if (opencode-shell--ssh-forwarded-profile-p profile)
      (opencode-shell--start-server
       profile callback (lambda () (opencode-shell--ssh-disconnected profile)))
    (opencode-shell--start-server profile callback)))

(defun opencode-shell--start-session (profile &optional directory)
  "Ensure PROFILE readiness and create a session in DIRECTORY or the current path."
  (let ((directory (or directory (opencode-shell--current-server-directory profile))))
    (if (plist-get profile :start-command)
        (opencode-shell--start-server-for-command
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
    (setq directory (opencode-shell--profile-client-directory directory profile))
    (let ((remote (file-remote-p directory))
          (root (plist-get profile :directory)))
      (when (and remote root (not (equal remote (file-remote-p root))))
        (user-error "Current directory cannot be mapped to the OpenCode server"))
      (expand-file-name (or (file-remote-p directory 'localname) directory)))))
(defun opencode-shell--client-directory (directory profile)
  "Map server-native DIRECTORY to the path understood by Emacs for PROFILE."
  (let* ((server-directory (expand-file-name (or directory default-directory)))
         (client-root (plist-get profile :directory))
         (remote (and client-root (file-remote-p client-root))))
    (file-name-as-directory
     (if remote (concat remote server-directory) server-directory))))
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

(defun opencode-shell--sessions-buffer-name (directory)
  "Return the session browser display name for DIRECTORY."
  (concat "*oc+sh$ " (opencode-shell--directory-leaf directory) " sessions*"))

(defun opencode-shell--transcript-buffer-name (directory &optional title)
  "Return the transcript display name for DIRECTORY with TITLE.
TITLE is omitted when nil, empty, or the \"Untitled\" placeholder."
  (concat "*oc-sh➜ "
          (if (and title (not (string-empty-p title))
                   (not (equal title "Untitled")))
              (concat title " — " (opencode-shell--directory-leaf directory))
            (opencode-shell--directory-leaf directory))
          "*"))

(defun opencode-shell--rename-transcript-for-title (title)
  "Rename the current transcript buffer to reflect TITLE.
Move the associated API log buffer so its name stays in sync."
  (let* ((new-name (opencode-shell--transcript-buffer-name
                    opencode-shell--directory title))
         (old-name (buffer-name)))
    (unless (equal new-name old-name)
      (let ((log (get-buffer (concat old-name "-log"))))
        (rename-buffer new-name t)
        (let ((actual-name (buffer-name)))
          (when log
            (with-current-buffer log
              (rename-buffer (concat actual-name "-log") t))))))))

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
  "Character occupying the fixed-width transient-status spinner slot.")

(defconst opencode-shell--spinner-max-width 10
  "Maximum display width of the transient-status spinner.")

(defcustom opencode-shell-log-requests nil
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
(defvar-local opencode-shell--configured-model nil)
(defvar-local opencode-shell--agent-model-history nil)
(defvar-local opencode-shell--agent-model-overrides nil)
(defvar-local opencode-shell--last-history-agent nil)
(defvar-local opencode-shell--selection-user-chosen-p nil)
(defvar-local opencode-shell--selected-model nil)
(defvar-local opencode-shell--selected-agent nil)
(defvar-local opencode-shell--runtime-key nil)
(defvar-local opencode-shell--connection-stale nil)
(defvar-local opencode-shell--animation-frame 0)
(defvar-local opencode-shell--spinner-overlays nil)
(defvar-local opencode-shell--composer-overlay nil)
(defvar-local opencode-shell--composer-protection-overlay nil)
(defvar-local opencode-shell--composer-input-ready nil)
(defvar-local opencode-shell--turn-gutters nil)
(defvar-local opencode-shell--render-dirty nil)
(defvar-local opencode-shell--render-event nil)
(defvar-local opencode-shell--render-force nil)
(defvar-local opencode-shell--render-dirty-turns nil)
(defvar-local opencode-shell--generation 0)
(defvar-local opencode-shell--in-flight nil)
(defvar-local opencode-shell--capabilities-loaded nil)
(defvar-local opencode-shell--capabilities-loading nil)
(defvar opencode-shell--capabilities-cache (make-hash-table :test #'equal)
  "Global cache of normalized capabilities keyed by server key.")
(defvar opencode-shell--models-cache nil
  "Internal cache of the last normalized models pair (RESPONSE . NORMALIZED).")
(defvar-local opencode-shell--turns nil)
(defvar-local opencode-shell--rendered-turns nil)
(defvar-local opencode-shell--turn-counter 0)
(defvar-local opencode-shell--transcript-end nil)
(defvar-local opencode-shell--composer-start nil)
(defvar-local opencode-shell--internal-edit nil)
(defconst opencode-shell--composer-label "\n"
  "Structural line above the composer; the prompt glyph is shown via the overlay.")
(defconst opencode-shell--composer-prompt "➜"
  "Prompt glyph displayed at the start of the composer lines.")
(defconst opencode-shell--gutter-glyph-user "$"
  "Gutter glyph shown on the first line of a user turn.")
(defconst opencode-shell--gutter-glyph-assistant "»"
  "Single-character gutter glyph shown on the first line of an assistant turn.")
(defconst opencode-shell--composer-sentinel " "
  "Structural same-line cursor target for an empty composer.")
(defvar-local opencode-shell--request-status "idle")
(defvar-local opencode-shell--message-request-sequence 0)
(defvar-local opencode-shell--message-applied-sequence 0)
(defvar-local opencode-shell--message-envelopes nil)
(defvar-local opencode-shell--message-order nil)
(defvar-local opencode-shell--removed-message-ids nil)
(defvar-local opencode-shell--normalized-changed-turns nil)
(defvar-local opencode-shell--message-state-revision 0)
(defvar-local opencode-shell--submit-in-flight nil)
(defvar-local opencode-shell--hydration-state nil)
(defvar-local opencode-shell--composer-label-visible t)
(defvar-local opencode-shell--permissions nil)
(defvar-local opencode-shell--permission-begin nil)
(defvar-local opencode-shell--permission-end nil)
(defvar-local opencode-shell--permission-status-begin nil)
(defvar-local opencode-shell--permission-status-end nil)
(defvar-local opencode-shell--interaction-state nil)
(defvar-local opencode-shell--resolved-permissions nil)
(defvar-local opencode-shell--permission-refresh-pending nil)
(defvar-local opencode-shell--questions-pending nil)
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
(defface opencode-shell-prompt-face
  '((((class color) (background dark)) :foreground "#7ec699" :weight bold)
    (((class color) (background light)) :foreground "#22863a" :weight bold)
    (t :inherit font-lock-constant-face :weight bold))
  "Face for the composer prompt glyph." :group 'opencode-shell)
(defface opencode-shell-waiting-face '((t :inherit shadow :slant italic))
  "Face for a turn awaiting a response." :group 'opencode-shell)
(defface opencode-shell-error-face '((t :inherit error))
  "Face for conversation transport errors." :group 'opencode-shell)
(defface opencode-shell-permission-face
  '((t :inherit warning :weight bold))
  "Face for pending permission requests." :group 'opencode-shell)
(defface opencode-shell-composer-face
  '((((class color) (background dark)) :background "#2f3338" :extend t)
    (((class color) (background light)) :background "#f0f2f4" :extend t)
    (t :inherit default))
  "Subtle background face for the writable composer." :group 'opencode-shell)
(defface opencode-shell-user-gutter-face
  '((((class color) (background dark)) :background "#4a2c2c")
    (((class color) (background light)) :background "#f2dcdc")
    (t :inherit shadow))
  "Gutter strip background marking user turns." :group 'opencode-shell)
(defface opencode-shell-assistant-gutter-face
  '((((class color) (background dark)) :background "#243a3c")
    (((class color) (background light)) :background "#dceef0")
    (t :inherit shadow))
  "Gutter strip background marking assistant turns." :group 'opencode-shell)
(defface opencode-shell-active-session-face
  '((t :inherit success :weight bold))
  "Face for an active session-browser location." :group 'opencode-shell)
(defface opencode-shell-recent-session-face
  '((t :inherit shadow))
  "Face for a saved session-browser location." :group 'opencode-shell)



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

(defun opencode-shell--routine-snapshot-path-p (path)
  "Return non-nil when successful PATH polling should stay out of logs."
  (or (member path '("/permission" "/question"))
      (string-match-p "/session/[^/]+/message\\'" path)))

(defun opencode-shell--ssh-forwarded-profile-p (profile)
  "Return non-nil when PROFILE reaches its loopback HTTP endpoint over SSH."
  (let* ((command (plist-get profile :start-command))
         (url (url-generic-parse-url (or (plist-get profile :base-url)
                                         opencode-shell-base-url))))
    (and (plist-get profile :remote)
         (consp command)
         (stringp (car command))
         (equal (file-name-nondirectory (car command)) "ssh")
         (member (url-host url) '("localhost" "127.0.0.1" "::1")))))

(defun opencode-shell--ssh-disconnected (profile)
  "Pause SSH-backed PROFILE's HTTP polling after transport loss."
  (let ((key (opencode-shell--server-key profile))
        (origin (current-buffer)))
    (when (opencode-shell-recovery-mark-offline key)
      (opencode-shell-async-pause-runtime key)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'opencode-shell-mode)
                     opencode-shell--profile
                     (equal key (opencode-shell--server-key opencode-shell--profile)))
            (setq-local opencode-shell--connection-stale t
                        opencode-shell--in-flight nil
                        opencode-shell--capabilities-loading nil)
            (dolist (resource '(messages permissions questions))
              (opencode-shell--settle-hydration resource nil)))))
      (message "OpenCode: SSH transport offline; retry with g r")
      (let ((needed
             (lambda ()
               (or (and (buffer-live-p origin)
                        (with-current-buffer origin
                          (or (derived-mode-p 'opencode-shell-mode)
                              (derived-mode-p 'opencode-shell-sessions-mode))))
                    (when-let ((runtime (opencode-shell-async-runtime-get key)))
                      (> (hash-table-count (plist-get runtime :subscribers)) 0))
                    (opencode-shell--ssh-prune-waiters key)))))
        (opencode-shell-recovery-failed
         key (lambda () (opencode-shell--ssh-retry profile nil needed)) needed)))))

(defun opencode-shell--ssh-retry (profile &optional on-ready needed)
  "Retry PROFILE's SSH transport once through the existing start flow."
  (let ((key (opencode-shell--server-key profile)))
    (unless (opencode-shell-recovery-exhausted-p key)
      (opencode-shell-recovery-watch-attempt
       key
       (lambda ()
         (when-let ((state (gethash key opencode-shell--servers)))
            (when (or (plist-get state :checking)
                      (plist-get state :starting))
             (opencode-shell--fail-start key (plist-get state :attempt)
                                         "SSH readiness timed out")))))
      (opencode-shell--start-server
       profile
       (lambda (_ready)
         (opencode-shell-recovery-success key)
         (opencode-shell-async-resume-runtime key)
         (opencode-shell--ssh-reconcile key)
         (when on-ready (funcall on-ready)))
       (lambda ()
         (opencode-shell-recovery-failed
          key (lambda () (opencode-shell--ssh-retry profile on-ready needed))
          (or needed
               (lambda ()
                 (or (when-let ((runtime (opencode-shell-async-runtime-get key)))
                       (> (hash-table-count (plist-get runtime :subscribers)) 0))
                      (opencode-shell--ssh-prune-waiters key)))))) t))))

(defun opencode-shell--ssh-reconcile (key)
  "Full-resync KEY's subscribed transcripts after SSH recovery."
  (when-let ((runtime (opencode-shell-async-runtime-get key)))
    (maphash
     (lambda (buffer _subscription)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (derived-mode-p 'opencode-shell-mode)
                      opencode-shell--connection-stale)
             (setq opencode-shell--connection-stale nil)
             (opencode-shell--resync t)))))
     (plist-get runtime :subscribers))))

(defun opencode-shell--ssh-suspend-if-unused (key)
  "Suspend KEY's offline recovery when no transcript or command awaits it."
  (when (and key (opencode-shell-recovery-offline-p key)
             (not (opencode-shell-async-runtime-get key))
             (not (opencode-shell--ssh-prune-waiters key))
             (not (plist-get (gethash key opencode-shell--servers)
                              :failure-callbacks)))
    (opencode-shell-recovery-suspend key)))

(defun opencode-shell--ssh-prune-waiters (key)
  "Drop KEY's pending GET callbacks whose original buffer is no longer current."
  (when-let ((state (gethash key opencode-shell--servers)))
    (setf (plist-get state :callbacks)
          (seq-filter
           (lambda (entry)
             (if (and (consp entry) (keywordp (car entry)))
                 (let ((origin (plist-get entry :origin)))
                   (and (buffer-live-p origin)
                        (with-current-buffer origin
                          (= (plist-get entry :generation)
                             opencode-shell--generation))))
               t))
           (plist-get state :callbacks))
          (plist-get state :failure-callbacks)
          (seq-filter
           (lambda (entry)
             (if (and (consp entry) (eq (car entry) 'deferred))
                 (let ((origin (nth 2 entry)))
                   (and (buffer-live-p origin)
                        (with-current-buffer origin
                          (= (nth 3 entry) opencode-shell--generation))))
               t))
           (plist-get state :failure-callbacks)))
    (plist-get state :callbacks)))

(defun opencode-shell--ssh-invoke-start-callback (entry)
  "Call an ordinary start callback or a live pending GET ENTRY."
  (if (and (consp entry) (keywordp (car entry)))
      (funcall (plist-get entry :callback) (plist-get entry :profile))
    (funcall (car entry) (cdr entry))))

(defun opencode-shell--ssh-wait-request
    (profile key origin generation method path callback body params error-callback)
  "Defer a read-only request until KEY's existing SSH start succeeds."
  (if (not (equal method "GET"))
      (when error-callback (funcall error-callback))
    (let ((state (or (gethash key opencode-shell--servers)
                     (list :profile profile
                           :config (opencode-shell--server-lifecycle-config profile)))))
      (setf (plist-get state :callbacks)
            (cons
             (list :callback
                   (lambda (_ready)
                     (when (buffer-live-p origin)
                       (with-current-buffer origin
                         (when (and (= generation opencode-shell--generation)
                                    (opencode-shell-recovery-ready-p key))
                           (opencode-shell--request-direct
                             method path callback body params error-callback)))))
                   :profile profile :origin origin :generation generation)
             (plist-get state :callbacks)))
      (when error-callback
        (setf (plist-get state :failure-callbacks)
              (cons (list 'deferred
                          (lambda ()
                            (when (buffer-live-p origin)
                              (with-current-buffer origin
                                (when (= generation opencode-shell--generation)
                                  (funcall error-callback)))))
                          origin generation)
                    (plist-get state :failure-callbacks))))
       (puthash key state opencode-shell--servers)
       (opencode-shell-recovery-resume
        key (lambda () (opencode-shell--ssh-retry profile))
        (lambda () (opencode-shell--ssh-prune-waiters key))))))

(defun opencode-shell--ssh-watch-start (key)
  "Bound KEY's pending SSH health/start attempt."
  (unless (let ((state (gethash key opencode-shell--servers)))
            (or (plist-get state :checking) (plist-get state :starting)))
    (opencode-shell-recovery-watch-attempt
     key
     (lambda ()
       (when-let ((state (gethash key opencode-shell--servers)))
         (when (or (plist-get state :checking) (plist-get state :starting))
           (opencode-shell--fail-start key (plist-get state :attempt)
                                        "SSH readiness timed out")))))))

(defun opencode-shell--request (method path callback &optional body params error-callback)
  "Send request after an SSH-backed PROFILE has verified its forwarding."
  (let* ((profile (or opencode-shell--profile (opencode-shell--default-profile)))
         (key (opencode-shell--server-key profile))
         (origin (current-buffer))
         (generation opencode-shell--generation))
    (if (not (and (opencode-shell--ssh-forwarded-profile-p profile)
                  (not (opencode-shell-recovery-ready-p key))))
        (opencode-shell--request-direct method path callback body params error-callback)
      (cond
       ((opencode-shell-recovery-exhausted-p key)
        (when error-callback (funcall error-callback)))
       ((opencode-shell-recovery-offline-p key)
        (opencode-shell--ssh-wait-request
         profile key origin generation method path callback body params error-callback))
       ((equal method "GET")
        (let ((starting (let ((state (gethash key opencode-shell--servers)))
                          (or (plist-get state :checking)
                              (plist-get state :starting)))))
          (opencode-shell--ssh-wait-request
           profile key origin generation method path callback body params error-callback)
          (unless starting
            (opencode-shell--ssh-watch-start key)
            (opencode-shell--start-server
             profile nil (lambda () (opencode-shell--ssh-disconnected profile))))))
       (t
        (opencode-shell--ssh-watch-start key)
        (opencode-shell--start-server
         profile
         (lambda (_ready)
           (when (buffer-live-p origin)
             (with-current-buffer origin
               (when (= generation opencode-shell--generation)
                 (opencode-shell--request-direct method path callback body params
                                                 error-callback)))))
         (lambda ()
           (when (buffer-live-p origin)
             (with-current-buffer origin
               (when (= generation opencode-shell--generation)
                 (opencode-shell--ssh-disconnected profile)
                 (when error-callback (funcall error-callback))))))
         t))))))

(defun opencode-shell--request-direct (method path callback &optional body params error-callback)
  "Send METHOD request to PATH and call CALLBACK with decoded JSON.
BODY is JSON encoded, PARAMS are query parameters, and ERROR-CALLBACK is
called after a transport, status, or decoding failure."
  (let* ((profile (or opencode-shell--profile (opencode-shell--default-profile)))
         (ssh (opencode-shell--ssh-forwarded-profile-p profile))
         (key (opencode-shell--server-key profile))
         (transport-epoch (and ssh (opencode-shell-recovery-epoch key)))
         (url-proxy-services
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
         (request-generation opencode-shell--generation)
         (request-id (cl-incf opencode-shell--request-log-counter))
         (started (float-time))
         (quiet-success (opencode-shell--routine-snapshot-path-p path)))
    (when (and opencode-shell-log-requests (not quiet-success))
      (opencode-shell--log "OpenCode API #%d → %s %s" request-id method path))
    (url-retrieve
     (opencode-shell--url path params)
     (lambda (status)
       (let ((response (current-buffer)))
          (unwind-protect
              (unless (and ssh (not (= transport-epoch
                                       (opencode-shell-recovery-epoch key))))
               (if-let ((err (plist-get status :error)))
                   (when (and (buffer-live-p origin)
                              (with-current-buffer origin
                                (= request-generation opencode-shell--generation)))
                    (with-current-buffer origin
                       (when (opencode-shell--ssh-forwarded-profile-p
                              (or opencode-shell--profile
                                  (opencode-shell--default-profile)))
                         (opencode-shell--ssh-disconnected
                          (or opencode-shell--profile
                              (opencode-shell--default-profile))))
                       (when opencode-shell-log-requests
                          (opencode-shell--log "OpenCode API #%d ← transport-error %.2fs [%s %s]"
                                              request-id (- (float-time) started) method path))
                        (unless (opencode-shell--ssh-forwarded-profile-p
                                 (or opencode-shell--profile
                                     (opencode-shell--default-profile)))
                          (message "OpenCode: %s" (opencode-shell--bounded-error err)))
                       (when error-callback
                         (opencode-shell-async-enqueue
                          origin (list 'request-error request-id)
                           request-generation error-callback))))
               (condition-case err
                    (let ((code (or (bound-and-true-p url-http-response-status) 0)))
                       (if (not (<= 200 code 299))
                           (when (and (buffer-live-p origin)
                                      (with-current-buffer origin
                                        (= request-generation opencode-shell--generation)))
                            (with-current-buffer origin
                              (when opencode-shell-log-requests
                                 (opencode-shell--log "OpenCode API #%d ← HTTP %s %.2fs [%s %s]"
                                                     request-id code (- (float-time) started) method path))
                               (message "OpenCode: HTTP %s request failed" code)
                               (when error-callback
                                 (opencode-shell-async-enqueue
                                  origin (list 'request-error request-id)
                                   request-generation error-callback))))
                        (let ((value (unless (= code 204)
                                       (opencode-shell--json-read-buffer))))
                           (when (and (buffer-live-p origin)
                                      (with-current-buffer origin
                                        (= request-generation opencode-shell--generation)))
                            (with-current-buffer origin
                              (when (and opencode-shell-log-requests
                                         (not quiet-success))
                                 (opencode-shell--log "OpenCode API #%d ← HTTP %s %.2fs [%s %s]"
                                                     request-id code (- (float-time) started) method path))
                               (opencode-shell-async-enqueue
                                origin (list 'request request-id)
                                 request-generation callback value))))))
                 (error
                   (when (and (buffer-live-p origin)
                              (with-current-buffer origin
                                (= request-generation opencode-shell--generation)))
                    (with-current-buffer origin
                      (when opencode-shell-log-requests
                         (opencode-shell--log "OpenCode API #%d ← decode-error %.2fs [%s %s]"
                                             request-id (- (float-time) started) method path))
                       (message "OpenCode: %s" (opencode-shell--bounded-error
                                                 (error-message-string err)))
                       (when error-callback
                         (opencode-shell-async-enqueue
                          origin (list 'request-error request-id)
                           request-generation error-callback))))))))
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

(defun opencode-shell--session-location-directory (directory)
  "Return canonical DIRECTORY spelling for persisted session locations."
  (if (string-match-p "\\`/+\\'" directory)
      "/"
    (file-name-as-directory (directory-file-name directory))))

(defun opencode-shell--remember-session-location (profile directory)
  "Remember PROFILE and DIRECTORY for session browser completion."
  (let* ((directory (opencode-shell--session-location-directory directory))
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
                     (opencode-shell--sessions-buffer-name directory)))))
    (with-current-buffer buffer
      (opencode-shell-sessions-mode)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
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
            (opencode-shell-open-session id directory opencode-shell--profile t)
          (opencode-shell-open-session id directory nil t)))
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
  (if (and opencode-shell--models-cache
           (eq (car opencode-shell--models-cache) response))
      (cdr opencode-shell--models-cache)
    (let* ((connected-present (or (assq 'connected response)
                                  (assoc "connected" response)))
           (connected (opencode-shell--get response 'connected))
           (normalized
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
      (setq opencode-shell--models-cache (cons response normalized))
      normalized)))

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
  "Return live transcript agent, model, and session metadata."
  (let* ((left (format " agent:%s  model:%s"
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
            (opencode-shell-interaction-active-p opencode-shell--interaction-state)
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

(defun opencode-shell--available-model (model)
  "Return MODEL normalized to a connected server-advertised model, or nil."
  (when-let ((value (opencode-shell--model-value model)))
    (when-let ((entry (seq-find (lambda (item) (equal (cdr item) value))
                                opencode-shell--models)))
      (cdr entry))))

(defun opencode-shell--agent-model (agent)
  "Return AGENT's highest-priority available model."
  (let ((configured (cdr (assoc agent opencode-shell--agents))))
    (seq-some #'opencode-shell--available-model
              (list (alist-get agent opencode-shell--agent-model-overrides nil nil #'equal)
                    (alist-get agent opencode-shell--agent-model-history nil nil #'equal)
                    (and configured (opencode-shell--get configured 'model))
                    opencode-shell--configured-model))))

(defun opencode-shell--activate-agent (agent &optional user-chosen)
  "Select visible AGENT and its resolved model.
When USER-CHOSEN is non-nil, later history hydration does not replace it."
  (unless (assoc agent opencode-shell--agents)
    (user-error "Agent is no longer available: %s" agent))
  (setq opencode-shell--selected-agent agent
        opencode-shell--selected-model (opencode-shell--agent-model agent)
        opencode-shell--selection-user-chosen-p
        (or opencode-shell--selection-user-chosen-p user-chosen))
  (force-mode-line-update))

(defun opencode-shell--restore-agent-model-history (messages)
  "Restore per-agent model history and active agent from user MESSAGES."
  (let (history last-agent)
    (dolist (envelope messages)
      (let ((info (opencode-shell--get envelope 'info)))
        (when (equal (format "%s" (opencode-shell--get info 'role)) "user")
          (when-let ((agent (opencode-shell--get info 'agent)))
            (setq last-agent agent)
            (when-let ((model (opencode-shell--model-value
                               (opencode-shell--get info 'model))))
              (setf (alist-get agent history nil nil #'equal) model))))))
    (setq opencode-shell--agent-model-history history
          opencode-shell--last-history-agent last-agent)
    (dolist (override (copy-sequence opencode-shell--agent-model-overrides))
      (when (equal (cdr override)
                   (alist-get (car override) history nil nil #'equal))
        (setq opencode-shell--agent-model-overrides
              (assoc-delete-all (car override)
                                opencode-shell--agent-model-overrides
                                #'equal))))
    (when opencode-shell--capabilities-loaded
      (opencode-shell--initialize-server-defaults))))

(defun opencode-shell--initialize-server-defaults ()
  "Initialize agent and model selections from session history or build defaults."
  (when opencode-shell--agents
    (let ((agent (or (and (not opencode-shell--selection-user-chosen-p)
                          (assoc opencode-shell--last-history-agent
                                 opencode-shell--agents)
                          opencode-shell--last-history-agent)
                     opencode-shell--selected-agent
                     (and (assoc "build" opencode-shell--agents) "build")
                     (caar opencode-shell--agents))))
      (when agent
        (setq opencode-shell--selected-agent agent
              opencode-shell--selected-model (opencode-shell--agent-model agent))))))

(defun opencode-shell--apply-cached-capabilities (&optional profile)
  "Populate buffer capabilities from global cache for PROFILE if available.
Return non-nil when cached capabilities were applied."
  (let* ((profile (or profile opencode-shell--profile))
         (key (and profile (opencode-shell--server-key profile)))
         (cached (and key (gethash key opencode-shell--capabilities-cache))))
    (when cached
      (setq opencode-shell--models (plist-get cached :models)
            opencode-shell--agents (plist-get cached :agents)
            opencode-shell--configured-model (plist-get cached :configured-model)
            opencode-shell--selected-model
            (opencode-shell--preserve-choice opencode-shell--selected-model
                                             opencode-shell--models))
      (unless (assoc opencode-shell--selected-agent opencode-shell--agents)
        (setq opencode-shell--selected-agent nil))
      (opencode-shell--initialize-server-defaults)
      (setq opencode-shell--capabilities-loaded t)
      t)))

(defun opencode-shell--reset-capabilities-cache ()
  "Clear the global capabilities and model normalization caches."
  (clrhash opencode-shell--capabilities-cache)
  (setq opencode-shell--models-cache nil))

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
    ("n" "Next prompt" opencode-shell--next-turn)
    ("P" "Previous prompt" opencode-shell--previous-turn)
    ("p" "Permission" opencode-shell--permissions)
    ("q" "Question" opencode-shell--questions)]
   ["Global"
    ("b" "Shell buffers" opencode-shell-switch-buffer)
    ("f" "Find saved session" opencode-shell-find-session)
    ("F" "Find session (server)" opencode-shell-find-session-with-server)
    ("l" "Session browser" opencode-shell)
    ("s" "Start session" opencode-shell-start)]])

(defvar opencode-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'opencode-shell--submit)
    (define-key map (kbd "s-<return>") #'opencode-shell--submit)
    (define-key map (kbd "C-c C-v") #'opencode-shell--select-model)
    (define-key map (kbd "C-c C-m") #'opencode-shell--select-agent)
    (define-key map (kbd "C-<tab>") #'opencode-shell--next-agent)
    (define-key map (kbd "C-c C-g") #'opencode-shell--resync)
    (define-key map (kbd "C-c C-a") #'opencode-shell--abort)
    (define-key map (kbd "C-c C-p") #'opencode-shell--permissions)
    (define-key map (kbd "C-c C-y") #'opencode-shell--permission-allow-once)
    (define-key map (kbd "C-c C-l") #'opencode-shell--permission-allow-always)
    (define-key map (kbd "C-c C-n") #'opencode-shell--permission-reject)
    (define-key map (kbd "C-c C-q") #'opencode-shell--questions)
    (define-key map (kbd "C-c C-h") #'describe-mode)
    (define-key map (kbd "C-n") #'opencode-shell--next-turn)
    (define-key map (kbd "C-p") #'opencode-shell--previous-turn)
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

(defun opencode-shell--point-in-composer-p ()
  "Return non-nil when point is geometrically inside the composer."
  (and (markerp opencode-shell--composer-start)
       (marker-position opencode-shell--composer-start)
       (>= (point) opencode-shell--composer-start)))

(defun opencode-shell--in-composer-p ()
  "Return non-nil when point is in the visible writable composer."
  (and (opencode-shell--composer-visible-p)
       (opencode-shell--point-in-composer-p)))

(defun opencode-shell--turn-navigation-target (direction)
  "Return the nearest live turn prompt position in DIRECTION from point."
  (let ((position (point)) candidates)
    (dolist (turn opencode-shell--turns)
      (when-let ((marker (opencode-shell--turn-user-begin turn)))
        (when (and (marker-position marker)
                   (eq (marker-buffer marker) (current-buffer)))
          (let ((candidate (marker-position marker)))
            (when (if (eq direction 'next)
                      (> candidate position)
                    (< candidate position))
              (push candidate candidates))))))
    (when (and (eq direction 'next)
               (opencode-shell--composer-visible-p)
               (markerp opencode-shell--composer-start)
               (marker-position opencode-shell--composer-start)
               (> opencode-shell--composer-start position))
      (push opencode-shell--composer-start candidates))
    (if (eq direction 'next)
        (and candidates (apply #'min candidates))
      (and candidates (apply #'max candidates)))))

(defun opencode-shell--move-turn (direction)
  "Move to the nearest rendered user prompt in DIRECTION."
  (if-let ((target (opencode-shell--turn-navigation-target direction)))
      (progn
        (when (and (/= target opencode-shell--composer-start)
                   (bound-and-true-p evil-local-mode)
                   (fboundp 'evil-normal-state))
          (evil-normal-state))
        (goto-char target))
    (user-error "No %s submitted prompt" direction)))

(defun opencode-shell--previous-turn ()
  "Move to the previous rendered user prompt."
  (interactive)
  (opencode-shell--move-turn 'previous))

(defun opencode-shell--next-turn ()
  "Move to the next rendered user prompt."
  (interactive)
  (opencode-shell--move-turn 'next))

(defun opencode-shell--composer-boundary ()
  "Return the position where the protected composer region begins.
While the structural composer label is present this is the label's start, so an
edit may remove the label itself (restored afterwards) while the transcript
stays protected.  When the label is already gone the boundary is the composer
start, keeping the transcript protected across follow-up edits."
  (when (and (markerp opencode-shell--composer-start)
             (marker-position opencode-shell--composer-start))
    (if (and (> opencode-shell--composer-start (point-min))
             (get-text-property (1- opencode-shell--composer-start)
                                'opencode-shell-composer-label))
        (- opencode-shell--composer-start
           (length opencode-shell--composer-label))
      opencode-shell--composer-start)))

(defun opencode-shell--protect-transcript (begin end)
  "Reject user edits outside or crossing the visible composer boundary."
  (when (and (not opencode-shell--internal-edit)
             (markerp opencode-shell--composer-start)
             (marker-position opencode-shell--composer-start)
             (or (not (opencode-shell--composer-visible-p))
                 (< begin (opencode-shell--composer-boundary))
                 (< end (opencode-shell--composer-boundary))))
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

(defvar opencode-shell--undo-extra-shifts nil
  "Additional (THRESHOLD . DELTA) shifts applied to preserved undo entries.
The values are expressed in pre-edit coordinates and applied highest
threshold first, so a caller can account for internal edits that shift
buffer positions other than the composer start (for example relocating the
structural composer sentinel).")

(defmacro opencode-shell--without-user-undo (&rest body)
  "Run BODY without adding package edits to the user's undo history."
  (declare (indent 0) (debug t))
  `(let ((opencode-shell--internal-edit t))
     (if (eq buffer-undo-list t)
         (progn ,@body)
       (let ((saved-undo buffer-undo-list)
           (old-composer-start (and (markerp opencode-shell--composer-start)
                                    (marker-position opencode-shell--composer-start)))
           (extra-shifts (sort (copy-sequence opencode-shell--undo-extra-shifts)
                               (lambda (a b) (> (car a) (car b)))))
           result)
       (let ((buffer-undo-list t))
         (setq result (progn ,@body)))
       ;; Interior edits (highest threshold first) are in pre-edit coordinates.
       (dolist (shift extra-shifts)
         (setq saved-undo
               (mapcar (lambda (entry)
                         (opencode-shell--shift-undo-entry
                          entry (car shift) (cdr shift)))
                       saved-undo)))
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
         result))))

(defun opencode-shell--insert-composer-label ()
  "Insert the structural label immediately before the composer.
The label is not marked read-only so a native Evil line deletion on the sole
composer line can remove it; `opencode-shell--ensure-composer-label' restores
exactly one label afterwards."
  (insert (propertize opencode-shell--composer-label
                      'opencode-shell-composer-label t
                      'rear-nonsticky '(opencode-shell-composer-label))))

(defun opencode-shell--insert-composer-sentinel ()
  "Insert the property-marked same-line composer sentinel at point."
  (insert (propertize opencode-shell--composer-sentinel
                      'opencode-shell-composer-sentinel t
                      'rear-nonsticky '(opencode-shell-composer-sentinel))))

(defun opencode-shell--remove-composer-labels (&optional limit)
  "Remove all characters with the composer label property up to LIMIT."
  (let ((pos (point-min))
        (limit (or limit (point-max))))
    (while (setq pos (text-property-any pos limit 'opencode-shell-composer-label t))
      (let ((end pos))
        (while (and (< end limit)
                    (get-text-property end 'opencode-shell-composer-label))
          (setq end (1+ end)))
        (delete-region pos end)))))

(defun opencode-shell--ensure-composer-label ()
  "Restore the structural composer label when an edit removed it.
Native Evil line deletion on the sole trailing-newline-less composer line also
removes the preceding label newline; this recreates exactly one label without
polluting the user's undo history."
  (when (and (not opencode-shell--internal-edit)
             (opencode-shell--composer-visible-p)
             (markerp opencode-shell--composer-start)
             (marker-position opencode-shell--composer-start)
             (not (and (> opencode-shell--composer-start (point-min))
                       (get-text-property
                        (1- opencode-shell--composer-start)
                        'opencode-shell-composer-label))))
    (opencode-shell--without-user-undo
      (let ((opencode-shell--internal-edit t)
            (inhibit-read-only t)
            (position (copy-marker (point) t)))
        (opencode-shell--remove-composer-labels opencode-shell--composer-start)
        (goto-char opencode-shell--composer-start)
        (opencode-shell--insert-composer-label)
        (set-marker opencode-shell--composer-start (point))
        (goto-char position)
        (set-marker position nil)))))

(defun opencode-shell--composer-sentinel-positions ()
  "Return buffer positions of the structural composer sentinels."
  (let ((position opencode-shell--composer-start)
        positions)
    (while (setq position (text-property-any
                           position (point-max)
                           'opencode-shell-composer-sentinel t))
      (push position positions)
      (setq position (1+ position)))
    (nreverse positions)))

(defun opencode-shell--ensure-composer-sentinel (&rest _ignored)
  "Keep exactly one structural sentinel at the composer end."
  (opencode-shell--ensure-composer-label)
  (when (and (not opencode-shell--internal-edit)
             (opencode-shell--composer-visible-p)
             (markerp opencode-shell--composer-start)
             (marker-position opencode-shell--composer-start))
    (when (or (not (get-text-property (1- (point-max))
                                      'opencode-shell-composer-sentinel))
              (text-property-any opencode-shell--composer-start
                                 (1- (point-max))
                                 'opencode-shell-composer-sentinel t))
      ;; Relocating a sentinel that a user edit pushed away from the composer
      ;; end shifts later positions, so preserved undo entries need the same
      ;; shift or undo would operate on stale ranges.
      (let ((opencode-shell--undo-extra-shifts
             (mapcar (lambda (pos) (cons pos -1))
                     (opencode-shell--composer-sentinel-positions)))
            (position (copy-marker (point))))
        (opencode-shell--without-user-undo
          (let ((opencode-shell--internal-edit t)
                (inhibit-read-only t))
            (while-let ((sentinel
                         (text-property-any opencode-shell--composer-start
                                            (point-max)
                                            'opencode-shell-composer-sentinel t)))
              (delete-region sentinel (1+ sentinel)))
            (goto-char (point-max))
            (opencode-shell--insert-composer-sentinel)))
        (goto-char position)
        (set-marker position nil)))
    (when (and (opencode-shell--point-in-composer-p)
               (= (point) (point-max)))
      (goto-char (1- (point-max))))))

(defun opencode-shell--refresh-composer-overlay ()
  "Show the composer background exactly over the visible editable region."
  (if (and (opencode-shell--composer-visible-p)
           (markerp opencode-shell--composer-start)
           (marker-position opencode-shell--composer-start))
      (progn
        (unless (overlayp opencode-shell--composer-overlay)
          (setq opencode-shell--composer-overlay
                (make-overlay opencode-shell--composer-start (point-max)
                              nil nil t)))
        (move-overlay opencode-shell--composer-overlay
                      opencode-shell--composer-start (point-max))
        (overlay-put opencode-shell--composer-overlay
                     'face 'opencode-shell-composer-face)
        (overlay-put opencode-shell--composer-overlay 'line-prefix
                     (propertize (concat opencode-shell--composer-prompt " ")
                                 'font-lock-face 'opencode-shell-prompt-face))
        (overlay-put opencode-shell--composer-overlay 'after-string
                     (propertize " "
                                 'face 'opencode-shell-composer-face
                                 'display '(space :align-to right-fringe))))
    (when (overlayp opencode-shell--composer-overlay)
      (delete-overlay opencode-shell--composer-overlay))
    (setq opencode-shell--composer-overlay nil)))

(define-derived-mode opencode-shell-mode text-mode "OpenCode"
  "OpenCode transcript mode with a writable bottom composer."
  (setq-local buffer-read-only nil)
  (setq-local font-lock-defaults '(opencode-shell-render-font-lock-keywords t))
  (setq-local header-line-format '(:eval (opencode-shell--header)))
  (setq-local mode-line-process '(:eval (opencode-shell--mode-line-status)))
  (setq-local opencode-shell--turn-gutters (make-hash-table :test #'eq))
  (setq-local opencode-shell--turns nil opencode-shell--turn-counter 0
              opencode-shell--rendered-turns nil
               opencode-shell--message-envelopes (make-hash-table :test #'equal)
               opencode-shell--message-order nil
               opencode-shell--removed-message-ids (make-hash-table :test #'equal)
              opencode-shell--normalized-changed-turns nil
              opencode-shell--message-state-revision 0
              opencode-shell--render-dirty-turns nil
              opencode-shell--permissions nil
              opencode-shell--request-status "idle")
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (opencode-shell--insert-composer-label)
    (setq opencode-shell--composer-start (copy-marker (point) nil)
          opencode-shell--transcript-end (copy-marker (point) nil)
          opencode-shell--permission-begin (copy-marker (point) nil)
          opencode-shell--permission-end (copy-marker (point) nil)
          opencode-shell--permission-status-begin (copy-marker (point) nil)
           opencode-shell--permission-status-end (copy-marker (point) nil)))
   (opencode-shell--insert-composer-sentinel)
   (opencode-shell--refresh-composer-overlay)
   (opencode-shell--configure-evil-buffer)
   (opencode-shell--sync-input-policy)
  ;; Mode-owned scaffolding must never become the first undoable transcript edit.
  (setq buffer-undo-list nil)
   (goto-char opencode-shell--composer-start)
   (add-hook 'before-change-functions #'opencode-shell--protect-transcript nil t)
    (add-hook 'post-command-hook #'opencode-shell--ensure-composer-sentinel nil t)
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
  (when opencode-shell--runtime-key
    (opencode-shell-async-unsubscribe-runtime
      opencode-shell--runtime-key (current-buffer))
    (opencode-shell--ssh-suspend-if-unused opencode-shell--runtime-key))
  (opencode-shell-async-unsubscribe-animation (current-buffer))
  (opencode-shell--clear-spinner-overlays)
   (dolist (overlay (list opencode-shell--composer-overlay
                          opencode-shell--composer-protection-overlay))
     (when (overlayp overlay) (delete-overlay overlay)))
   (opencode-shell--clear-all-turn-gutters)
   (setq opencode-shell--composer-overlay nil
         opencode-shell--composer-protection-overlay nil)
  (setq opencode-shell--runtime-key nil)
  (setq opencode-shell--in-flight nil
        opencode-shell--capabilities-loading nil)
  (when opencode-shell--session-id
    (when-let ((log-buffer (get-buffer (opencode-shell--log-buffer-name))))
      (kill-buffer log-buffer)))
  (cl-incf opencode-shell--generation))

(defun opencode-shell--stop-polling ()
  "Stop periodic network polling and UI animation in the current buffer."
  (opencode-shell-async-cancel)
  (when opencode-shell--runtime-key
    (opencode-shell-async-unsubscribe-runtime
      opencode-shell--runtime-key (current-buffer))
    (opencode-shell--ssh-suspend-if-unused opencode-shell--runtime-key))
  (opencode-shell-async-unsubscribe-animation (current-buffer))
  (opencode-shell--clear-spinner-overlays)
  (setq opencode-shell--runtime-key nil)
  (opencode-shell--log-lifecycle "poll-stop" t))

(defun opencode-shell--start-polling ()
  "Start periodic network polling and UI animation if needed."
  (unless opencode-shell--runtime-key
    (let ((buffer (current-buffer)))
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
                    (opencode-shell--log "OpenCode async %s" event)))
                opencode-shell--session-id
                 #'opencode-shell--receive-application-event)))
        (ignore runtime)
         (when (opencode-shell-recovery-offline-p key)
           (setq-local opencode-shell--connection-stale t)
           (opencode-shell-async-pause-runtime key)
           (opencode-shell-recovery-resume
            key (lambda () (opencode-shell--ssh-retry profile))
            (lambda ()
              (when-let ((current (opencode-shell-async-runtime-get key)))
                (> (hash-table-count (plist-get current :subscribers)) 0)))))
        (setq opencode-shell--runtime-key key)
        (opencode-shell-async-subscribe-animation
         buffer opencode-shell-animation-interval
         #'opencode-shell--animation-tick))
      (opencode-shell--log-lifecycle "poll-start" t))))

;;;###autoload
(defun opencode-shell-open-session (id &optional directory profile current-window)
  "Open exact session ID scoped to DIRECTORY and PROFILE.
When CURRENT-WINDOW is non-nil, display it in the selected window."
  (interactive "sSession ID: ")
  (let* ((explicit-profile (or profile opencode-shell--profile))
          (profile (or (opencode-shell--resolve-profile profile)
                       profile opencode-shell--profile
                       (opencode-shell--default-profile)))
          (resolved-directory
           (opencode-shell--server-directory
            (or directory (opencode-shell--current-server-directory profile))
            profile))
          (buffer (or (opencode-shell--transcript-buffer
                       profile resolved-directory id)
                      (generate-new-buffer
                       (opencode-shell--transcript-buffer-name resolved-directory)))))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-shell-mode) (opencode-shell--cleanup))
      (opencode-shell-mode)
      (setq-local opencode-shell--generation (cl-incf opencode-shell--generation-counter))
      (setq-local opencode-shell--session-id id)
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--base-url (or (plist-get profile :base-url)
                                                opencode-shell-base-url))
      (setq-local opencode-shell--directory resolved-directory)
      (setq-local default-directory
                  (opencode-shell--client-directory resolved-directory profile))
      (setq-local opencode-shell--session-title nil)
      (opencode-shell--apply-cached-capabilities profile)
      (opencode-shell--begin-initial-hydration)
      (opencode-shell--resync t)
      (opencode-shell--start-polling))
    (if current-window (switch-to-buffer buffer) (pop-to-buffer buffer))
    (opencode-shell--refresh-table-layout)
    (goto-char (or (and (markerp opencode-shell--composer-start)
                             (marker-position opencode-shell--composer-start))
                        (point-max)))
    (when (fboundp 'evil-normal-state)
      (evil-normal-state))))

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
    (when (or (assq 'time known-info) (assq 'time incoming-info))
      (setf (alist-get 'time info)
            (opencode-shell--merge-alist (opencode-shell--get known-info 'time)
                                         (opencode-shell--get incoming-info 'time))))
    (setf (alist-get 'info merged) info)
    (setf (alist-get 'parts merged)
          (opencode-shell--merge-parts (opencode-shell--get known 'parts)
                                       (opencode-shell--get incoming 'parts)))
    merged))

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

(defun opencode-shell--lifecycle-id (value)
  "Return VALUE as bounded operational metadata."
  (if value (truncate-string-to-width (format "%s" value) 80 nil nil t) "-"))

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
         (blocked-permission (opencode-shell--human-interaction-blocked-p))
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
                 (mapcar #'opencode-shell-response-part-state-label parts)) "none")
            (opencode-shell--status opencode-shell--session-id)
            (length opencode-shell--permissions)
            (if (opencode-shell-interaction-active-p
                  opencode-shell--interaction-state 'permission)
                 "yes" "no")
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

(defun opencode-shell--aggregate-turn (turn)
  "Recompute TURN presentation state once from its merged assistant messages."
  (let* ((messages (opencode-shell--turn-assistant-messages turn))
         (parts (apply #'append
                       (mapcar (lambda (item)
                                 (opencode-shell--get (cdr item) 'parts))
                               messages)))
         (last-envelope (cdr (car (last messages))))
         (complete (and last-envelope
                        (opencode-shell-response-envelope-complete-p
                          (opencode-shell--get last-envelope 'info)
                          (opencode-shell--get last-envelope 'parts))
                        (not (seq-some #'opencode-shell-response-running-tool-p parts)))))
    (setf (opencode-shell--turn-parts turn) parts
          (opencode-shell--turn-assistant turn)
          (mapconcat (lambda (item) (opencode-shell--message-text (cdr item)))
                     messages "")
          (opencode-shell--turn-terminal-error turn)
          (if complete
              (opencode-shell--message-error-label
               (opencode-shell--get last-envelope 'info))
            (opencode-shell--turn-terminal-error turn))
          (opencode-shell--turn-status turn)
          (cond (complete 'complete)
                ((opencode-shell--turn-locally-settled turn) 'complete)
                (t (opencode-shell-response-phase
                     (opencode-shell--turn-parts turn))))
          (opencode-shell--turn-locally-settled turn)
          (and (opencode-shell--turn-locally-settled turn) (not complete)))
    turn))

(defun opencode-shell--normalize-turns (messages)
  "Reconcile server MESSAGES into stable buffer-local turn records."
  (let ((old opencode-shell--turns) observed current changed
        (touched (make-hash-table :test #'eq))
        (turn-index (make-hash-table :test #'equal))
        (assistant-indexes (make-hash-table :test #'eq))
        (local-turns-by-text (make-hash-table :test #'equal))
        (old-set (make-hash-table :test #'eq)))
    (dolist (turn old)
      (puthash turn t old-set)
      (when-let ((id (opencode-shell--turn-id turn)))
        (puthash id turn turn-index))
      (when-let ((id (opencode-shell--turn-server-user-id turn)))
        (puthash id turn turn-index))
      (unless (opencode-shell--turn-server-user-id turn)
        (push turn (gethash (opencode-shell--turn-user turn) local-turns-by-text))))
    (maphash (lambda (text turns)
               (puthash text (nreverse turns) local-turns-by-text))
             local-turns-by-text)
    (dolist (envelope messages)
      (let* ((info (opencode-shell--get envelope 'info))
             (role (format "%s" (or (opencode-shell--get info 'role) "")))
             (id (opencode-shell--get info 'id))
             (parent (or (opencode-shell--get info 'parentID)
                         (opencode-shell--get info 'parentId))))
        (cond
          ((equal role "user")
           (let* ((text (opencode-shell--message-text envelope))
                   (local-candidates (gethash text local-turns-by-text))
                   (turn (or (and id (gethash id turn-index))
                            (car local-candidates)
                             (opencode-shell--make-turn
                              :id (or id (format "turn-%d" (cl-incf opencode-shell--turn-counter)))))))
            (when (and local-candidates (eq turn (car local-candidates)))
              (puthash text (cdr local-candidates) local-turns-by-text))
            (let ((before (opencode-shell--turn-state-signature turn)))
            (setf (opencode-shell--turn-server-user-id turn) id
                  (opencode-shell--turn-acknowledged turn) t
                  (opencode-shell--turn-user turn) text)
              (unless (opencode-shell--turn-assistant-messages turn)
                (setf (opencode-shell--turn-status turn)
                      (if (opencode-shell--turn-locally-settled turn)
                          'complete 'waiting)))
              (unless (equal before (opencode-shell--turn-state-signature turn))
                (cl-pushnew turn changed :test #'eq)))
            (when id (puthash id turn turn-index))
            (setq current turn)
            (push turn observed)))
          ((equal role "assistant")
            (let* ((turn (or (and parent (gethash parent turn-index))
                              current)))
             (when turn
               (let* ((messages (opencode-shell--turn-assistant-messages turn))
                      (state
                       (or (gethash turn assistant-indexes)
                           (let ((index (make-hash-table :test #'equal)))
                             (dolist (entry messages)
                               (puthash (car entry) entry index))
                             (let ((value (list :index index :tail (last messages))))
                               (puthash turn value assistant-indexes)
                               value))))
                      (index (plist-get state :index))
                      (entry (and id (gethash id index)))
                     (merged (if entry
                                 (opencode-shell--merge-envelope (cdr entry) envelope)
                               envelope)))
                (unless (and entry (equal (cdr entry) merged))
                  (if entry
                      (setcdr entry merged)
                    (let ((new-entry (cons id merged))
                          (tail (plist-get state :tail)))
                      (if tail
                          (progn
                            (setcdr tail (list new-entry))
                            (setf (plist-get state :tail) (cdr tail)))
                        (setq messages (list new-entry))
                        (setf (plist-get state :tail) messages))
                      (when id (puthash id new-entry index))))
                  (setf (opencode-shell--turn-assistant-messages turn) messages)
                   (puthash turn t touched)))))))))
    (maphash
     (lambda (turn _)
       (let ((before (opencode-shell--turn-state-signature turn)))
         (opencode-shell--aggregate-turn turn)
         (unless (equal before (opencode-shell--turn-state-signature turn))
           (cl-pushnew turn changed :test #'eq))))
    touched)
    (setq observed (nreverse observed))
    (let (new-turns)
      (dolist (turn observed)
        (unless (gethash turn old-set)
          (push turn new-turns)
          (cl-pushnew turn changed :test #'eq)))
      (let ((result (append (copy-sequence old) (nreverse new-turns))))
      (let ((before (mapcar #'opencode-shell--turn-state-signature result)))
        (opencode-shell--settle-superseded-turns result)
        (cl-mapc (lambda (turn signature)
                   (unless (equal signature (opencode-shell--turn-state-signature turn))
                     (cl-pushnew turn changed :test #'eq)))
                 result before))
      (setq opencode-shell--normalized-changed-turns (nreverse changed))
        result))))

(defun opencode-shell--message-envelope-id (envelope)
  "Return ENVELOPE's stable message ID."
  (opencode-shell--get (opencode-shell--get envelope 'info) 'id))

(defun opencode-shell--ensure-message-cache ()
  "Ensure the current transcript owns an initialized message cache."
  (unless (hash-table-p opencode-shell--message-envelopes)
    (setq opencode-shell--message-envelopes (make-hash-table :test #'equal)
          opencode-shell--message-order nil))
  (unless (hash-table-p opencode-shell--removed-message-ids)
    (setq opencode-shell--removed-message-ids (make-hash-table :test #'equal))))

(defun opencode-shell--cache-message-envelope (envelope)
  "Merge ENVELOPE into the buffer-local message index and return the result."
  (opencode-shell--ensure-message-cache)
  (let* ((id (opencode-shell--message-envelope-id envelope))
         (known (and id (gethash id opencode-shell--message-envelopes)))
         (merged (if known (opencode-shell--merge-envelope known envelope) envelope)))
    (when id
      (unless known
        (setq opencode-shell--message-order
              (append opencode-shell--message-order (list id))))
      (puthash id merged opencode-shell--message-envelopes))
    merged))

(defun opencode-shell--cache-message-snapshot (messages &optional authoritative)
  "Merge chronological MESSAGES into the message index.
When AUTHORITATIVE is non-nil, remove cached server state absent from MESSAGES.
Return a plist containing affected turns and whether a full render is required."
  (opencode-shell--ensure-message-cache)
  (let ((ids (delq nil (mapcar #'opencode-shell--message-envelope-id messages)))
        changed force)
    (when authoritative
      (dolist (id ids) (remhash id opencode-shell--removed-message-ids)))
    (when authoritative
      (let ((present (make-hash-table :test #'equal)))
        (dolist (id ids) (puthash id t present))
        (let (removed)
          (maphash (lambda (id _)
                     (unless (gethash id present) (push id removed)))
                   opencode-shell--message-envelopes)
          (dolist (id removed)
            (remhash id opencode-shell--message-envelopes)))
        (setq opencode-shell--message-order ids)
        (let ((kept
               (seq-filter
                (lambda (turn)
                  (let ((server-id (opencode-shell--turn-server-user-id turn)))
                    (or (null server-id) (gethash server-id present))))
                opencode-shell--turns)))
          (unless (= (length kept) (length opencode-shell--turns))
            (setq force t
                  opencode-shell--turns kept)))
        (dolist (turn opencode-shell--turns)
          (let* ((known (opencode-shell--turn-assistant-messages turn))
                 (kept (seq-filter (lambda (entry) (gethash (car entry) present))
                                   known)))
            (unless (= (length known) (length kept))
              (setf (opencode-shell--turn-assistant-messages turn) kept)
              (opencode-shell--aggregate-turn turn)
              (push turn changed))))))
    (dolist (envelope messages)
      (opencode-shell--cache-message-envelope envelope))
    (list :changed-turns changed :force force)))

(defun opencode-shell--cached-assistants-for-parent (parent-id)
  "Return cached assistant envelopes belonging to PARENT-ID in order."
  (delq nil
        (mapcar
         (lambda (id)
           (let* ((envelope (gethash id opencode-shell--message-envelopes))
                  (info (opencode-shell--get envelope 'info)))
             (when (and envelope
                        (equal (format "%s" (opencode-shell--get info 'role))
                               "assistant")
                        (equal (or (opencode-shell--get info 'parentID)
                                   (opencode-shell--get info 'parentId))
                               parent-id))
               envelope)))
         opencode-shell--message-order)))

(defun opencode-shell--rebuild-turn-from-message-cache (parent-id)
  "Rebuild and return PARENT-ID's turn from cached assistant envelopes."
  (when-let ((turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)))
    (setf (opencode-shell--turn-assistant-messages turn) nil
          (opencode-shell--turn-parts turn) nil
          (opencode-shell--turn-assistant turn) "")
    (opencode-shell--normalize-turns
     (opencode-shell--cached-assistants-for-parent parent-id))
    turn))

(defun opencode-shell--turn-state-signature (turn)
  "Return TURN state that can affect transcript presentation."
  (and turn
       (list (opencode-shell--turn-id turn)
             (opencode-shell--turn-server-user-id turn)
             (opencode-shell--turn-user turn)
             (opencode-shell--turn-assistant turn)
             (copy-tree (opencode-shell--turn-parts turn))
             (opencode-shell--turn-status turn)
             (opencode-shell--turn-acknowledged turn)
             (opencode-shell--turn-locally-settled turn)
             (opencode-shell--turn-terminal-error turn))))

(defun opencode-shell--schedule-event-reconciliation (&optional resource)
  "Coalesce an authoritative snapshot reconciliation for RESOURCE."
  (let ((resource (or resource 'all)))
    (opencode-shell-async-enqueue
     (current-buffer) (list 'event-reconcile resource) opencode-shell--generation
     #'opencode-shell--resync nil resource)))

(defun opencode-shell--apply-message-event (event)
  "Apply validated message EVENT locally, returning non-nil on success."
  (let* ((kind (plist-get event :kind))
         (message-id (plist-get event :message-id))
         (known (and message-id
                     (gethash message-id opencode-shell--message-envelopes)))
         parent-id turn before force)
    (pcase kind
      ('message-updated
       (let* ((info (plist-get event :info))
               (role (format "%s" (opencode-shell--get info 'role)))
               (parent (or (opencode-shell--get info 'parentID)
                           (opencode-shell--get info 'parentId))))
          (unless (and parent (gethash parent opencode-shell--removed-message-ids))
            (remhash message-id opencode-shell--removed-message-ids)
            (let ((envelope (opencode-shell--cache-message-envelope
                             `((info . ,info)
                               (parts . ,(opencode-shell--get known 'parts))))))
              (if (equal role "user")
                  (progn
                    (setq turn (opencode-shell--turn-by-id message-id opencode-shell--turns)
                          before (opencode-shell--turn-state-signature turn))
                    (setq opencode-shell--turns
                          (opencode-shell--normalize-turns (list envelope)))
                    (setq turn (opencode-shell--turn-by-id message-id opencode-shell--turns)))
                (setq parent-id parent)
                (when parent-id
                  (setq turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                        before (opencode-shell--turn-state-signature turn))
                  (opencode-shell--rebuild-turn-from-message-cache parent-id)))))))
      ('part-updated
       (when known
         (let* ((info (opencode-shell--get known 'info))
                (part (plist-get event :part)))
           (setq parent-id (or (opencode-shell--get info 'parentID)
                               (opencode-shell--get info 'parentId))
                 turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                 before (opencode-shell--turn-state-signature turn))
           (opencode-shell--cache-message-envelope
            `((info . ,info) (parts . (,part))))
           (opencode-shell--rebuild-turn-from-message-cache parent-id))))
      ('part-removed
       (when known
         (let ((info (opencode-shell--get known 'info)))
           (setq parent-id (or (opencode-shell--get info 'parentID)
                               (opencode-shell--get info 'parentId))
                 turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                 before (opencode-shell--turn-state-signature turn))
           (setf (alist-get 'parts known)
                 (seq-remove
                  (lambda (part)
                    (equal (opencode-shell--get part 'id)
                           (plist-get event :part-id)))
                  (opencode-shell--get known 'parts)))
           (puthash message-id known opencode-shell--message-envelopes)
           (opencode-shell--rebuild-turn-from-message-cache parent-id))))
      ('message-removed
       (let* ((info (opencode-shell--get known 'info))
              (role (and known (format "%s" (opencode-shell--get info 'role))))
              (user-turn (opencode-shell--turn-by-id message-id opencode-shell--turns))
              (children (make-hash-table :test #'equal)))
         (puthash message-id t opencode-shell--removed-message-ids)
         (remhash message-id opencode-shell--message-envelopes)
         (setq opencode-shell--message-order
               (delete message-id opencode-shell--message-order))
         (maphash
          (lambda (id envelope)
            (let ((child-info (opencode-shell--get envelope 'info)))
              (when (equal message-id
                           (or (opencode-shell--get child-info 'parentID)
                               (opencode-shell--get child-info 'parentId)))
                (puthash id t children))))
          opencode-shell--message-envelopes)
         (if (or (equal role "user") user-turn (> (hash-table-count children) 0))
             (progn
               (maphash
                (lambda (id _)
                  (remhash id opencode-shell--message-envelopes))
                children)
               (setq opencode-shell--message-order
                     (seq-remove (lambda (id) (gethash id children))
                                 opencode-shell--message-order)
                     opencode-shell--turns
                     (seq-remove
                      (lambda (entry)
                        (equal message-id
                               (opencode-shell--turn-server-user-id entry)))
                      opencode-shell--turns)
                     force t
                     turn t
                     before nil))
           (when known
             (setq parent-id (or (opencode-shell--get info 'parentID)
                                 (opencode-shell--get info 'parentId))
                   turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                   before (opencode-shell--turn-state-signature turn))
             (opencode-shell--rebuild-turn-from-message-cache parent-id))))))
    (when turn
      (opencode-shell--update-message-lifecycle-state)
      (unless (equal before (and (not (eq turn t))
                                 (opencode-shell--turn-state-signature turn)))
        (opencode-shell--schedule-render
         (format "event:%s" (or (plist-get event :type) kind)) force
         (and (not (eq turn t)) (list turn))))
      t)))

(defun opencode-shell--receive-application-event (event)
  "Apply decoded application EVENT or reconcile when it is not safe locally."
  (when (derived-mode-p 'opencode-shell-mode)
    (opencode-shell--ensure-message-cache)
    (when (memq (plist-get event :kind)
                '(message-updated message-removed part-updated part-removed))
      (opencode-shell--reset-spinner-frame))
    (if (and (memq (plist-get event :kind)
                   '(message-updated message-removed part-updated part-removed))
             (opencode-shell--apply-message-event event))
        (cl-incf opencode-shell--message-state-revision)
      (opencode-shell--schedule-event-reconciliation
       (plist-get event :resource)))))

(defun opencode-shell--composer-text ()
  "Return composer contents without properties or its sentinel."
  (let ((end (if (get-text-property (1- (point-max))
                                    'opencode-shell-composer-sentinel)
                 (1- (point-max))
               (point-max))))
    (buffer-substring-no-properties opencode-shell--composer-start end)))

(defun opencode-shell--composer-visible-p ()
  "Return non-nil when authoritative lifecycle state permits editing."
  (opencode-shell-state-composer-ready-p
   (mapcar #'opencode-shell--turn-status opencode-shell--turns)
   opencode-shell--submit-in-flight
   (opencode-shell--human-interaction-blocked-p)
   (opencode-shell--initial-hydration-complete-p)))

(defun opencode-shell--initial-hydration-complete-p ()
  "Return non-nil when initial authoritative snapshots have settled."
  (or (null opencode-shell--hydration-state)
      (opencode-shell-state-hydration-complete-p
       opencode-shell--hydration-state)))

(defun opencode-shell--begin-initial-hydration ()
  "Block Composer until all authoritative session resources settle."
  (setq opencode-shell--hydration-state
        (opencode-shell-state-hydration-start
         '(messages permissions questions)))
  (opencode-shell--render-turns)
  (opencode-shell--render-permissions))

(defun opencode-shell--settle-hydration (resource success)
  "Record RESOURCE hydration SUCCESS and schedule its readiness transition."
  (when opencode-shell--hydration-state
    (let ((next (opencode-shell-state-hydration-settle
                 opencode-shell--hydration-state resource success)))
      (unless (equal next opencode-shell--hydration-state)
        (setq opencode-shell--hydration-state next)
        (opencode-shell--schedule-render "hydration" t)))))

(defun opencode-shell--retry-hydration (resources)
  "Move failed RESOURCES back to pending before retrying them."
  (when opencode-shell--hydration-state
    (setq opencode-shell--hydration-state
          (opencode-shell-state-hydration-retry
           opencode-shell--hydration-state resources))))

(defun opencode-shell--human-interaction-blocked-p ()
  "Return non-nil while a human interaction blocks new input."
  (or opencode-shell--permissions
      (opencode-shell-interaction-active-p opencode-shell--interaction-state)
      opencode-shell--questions-pending))

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
  (opencode-shell--settle-hydration 'questions t)
  (let ((questions (opencode-shell--deduplicate-questions
                    (opencode-shell--session-questions items))))
    (unless (equal questions opencode-shell--questions-pending)
      (setq opencode-shell--questions-pending questions)
      (when (opencode-shell--human-interaction-blocked-p)
        (opencode-shell--start-polling))
      (if defer-render
          (opencode-shell--schedule-render "questions")
        (opencode-shell--render-turns)
        (opencode-shell--render-permissions)
        (opencode-shell--log-lifecycle "questions")))))

(defun opencode-shell--refresh-questions ()
  "Fetch `/question', deferring once when that request is in flight."
  (if (alist-get 'questions opencode-shell--in-flight)
      (setq opencode-shell--question-refresh-pending t)
    (opencode-shell--guarded-request
     'questions "GET" "/question"
     (lambda (items) (opencode-shell--receive-questions items t))
     nil (lambda ()
           (opencode-shell--consume-question-refresh-pending)
           (opencode-shell--settle-hydration 'questions nil)))))

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
         (offset (and (opencode-shell--point-in-composer-p)
                      (- (point) opencode-shell--composer-start)))
          (inhibit-read-only t))
    (save-excursion
      (when opencode-shell--composer-label-visible
        (opencode-shell--remove-composer-labels opencode-shell--composer-start)
        (setq opencode-shell--composer-label-visible nil))
      (goto-char opencode-shell--permission-begin)
      (delete-region opencode-shell--permission-begin opencode-shell--composer-start)
      (when (looking-back (regexp-quote opencode-shell--composer-label)
                          (max (point-min)
                               (- (point) (length opencode-shell--composer-label))))
        (delete-region (- (point) (length opencode-shell--composer-label))
                       (point)))
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
        (opencode-shell--insert-composer-label))
      (setq opencode-shell--composer-label-visible
            (opencode-shell--composer-visible-p))
      (set-marker opencode-shell--composer-start (point)))
    (when offset
      (goto-char (min (point-max) (+ opencode-shell--composer-start offset))))
    (unless (equal draft (opencode-shell--composer-text))
      (error "Permission rendering changed composer text"))))
  (opencode-shell--refresh-composer-overlay)
  (opencode-shell--refresh-spinner-overlays))

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
     nil (lambda ()
           (opencode-shell--consume-permission-refresh-pending)
           (opencode-shell--settle-hydration 'permissions nil)))))

(defun opencode-shell--consume-permission-refresh-pending ()
  "Reissue a `/permission' fetch deferred while one was already in flight."
  (when opencode-shell--permission-refresh-pending
    (setq opencode-shell--permission-refresh-pending nil)
    (opencode-shell--refresh-permissions)))

(defun opencode-shell--receive-permissions (items &optional defer-render)
  "Store session-scoped permission ITEMS and update their display.
When DEFER-RENDER is non-nil, coalesce presentation at idle time."
  (opencode-shell--consume-permission-refresh-pending)
  (opencode-shell--settle-hydration 'permissions t)
  (let ((permissions
         (seq-remove
          (lambda (item)
            (opencode-shell--resolved-permission
             (opencode-shell--permission-id item)))
          (opencode-shell--deduplicate-permissions
           (opencode-shell--session-permissions items)))))
    (unless (equal permissions opencode-shell--permissions)
      (setq opencode-shell--permissions permissions)
      (when (opencode-shell--human-interaction-blocked-p)
        (opencode-shell--start-polling))
      (if defer-render
          (opencode-shell--schedule-render "permissions")
        (opencode-shell--render-turns)
        (opencode-shell--render-permissions)
        (opencode-shell--log-lifecycle "permissions")))))

(defun opencode-shell--replace-composer (text &optional offset)
  "Replace the composer with TEXT and place point at OFFSET or its end."
  (opencode-shell--without-user-undo
    (let ((inhibit-read-only t))
      (delete-region opencode-shell--composer-start (point-max))
      (goto-char opencode-shell--composer-start)
      (insert text)
      (opencode-shell--insert-composer-sentinel)
      (goto-char (+ opencode-shell--composer-start (or offset (length text))))))
  (opencode-shell--refresh-composer-overlay))

(defun opencode-shell--discard-turn-markers (turn)
  "Detach all rendered region markers owned by TURN."
  (opencode-shell--clear-turn-gutters turn)
  (dolist (marker (list (opencode-shell--turn-user-begin turn)
                        (opencode-shell--turn-user-end turn)
                        (opencode-shell--turn-response-begin turn)
                        (opencode-shell--turn-response-end turn)))
    (when (markerp marker) (set-marker marker nil))))

(defconst opencode-shell--gutter-margin
  (propertize " " 'display '(space :width 1))
  "Colorless one-cell margin placed right of a role gutter strip.")

(defun opencode-shell--clear-turn-gutters (turn)
  "Delete every role gutter overlay owned by TURN."
  (when (hash-table-p opencode-shell--turn-gutters)
    (let ((entry (gethash turn opencode-shell--turn-gutters)))
      (dolist (role '(:user :response))
        (dolist (overlay (plist-get entry role))
          (when (overlayp overlay) (delete-overlay overlay))))
      (remhash turn opencode-shell--turn-gutters))))

(defun opencode-shell--clear-all-turn-gutters ()
  "Delete every role gutter overlay in the current buffer."
  (when (hash-table-p opencode-shell--turn-gutters)
    (maphash (lambda (turn _) (opencode-shell--clear-turn-gutters turn))
             opencode-shell--turn-gutters)
    (clrhash opencode-shell--turn-gutters)))

(defun opencode-shell--gutter-line-end (position)
  "Return the exclusive end of the display line beginning at POSITION."
  (save-excursion
    (goto-char position)
    (min (point-max) (1+ (line-end-position)))))

(defun opencode-shell--apply-turn-gutter (turn role begin end glyph label-face gutter-face)
  "Apply ROLE gutter overlays for TURN over BEGIN..END.
The first line shows GLYPH with LABEL-FACE; every line receives a
GUTTER-FACE background strip no wider than GLYPH, followed by a colorless
one-cell margin.  Native line numbers are never hidden.  Overlays (not
text properties) keep gutter glyphs out of copied text."
  (unless (hash-table-p opencode-shell--turn-gutters)
    (setq opencode-shell--turn-gutters (make-hash-table :test #'eq)))
  (let ((entry (or (gethash turn opencode-shell--turn-gutters)
                   (puthash turn (list :user nil :response nil)
                            opencode-shell--turn-gutters)))
        (blank (concat (propertize (make-string (length glyph) ?\s)
                                   'face gutter-face)
                       opencode-shell--gutter-margin))
        (first t)
        overlays)
    (dolist (overlay (plist-get entry role))
      (when (overlayp overlay) (delete-overlay overlay)))
    (save-excursion
      (goto-char begin)
      (while (and (< (point) end) (not (eobp)))
        (let ((overlay (make-overlay (point)
                                     (opencode-shell--gutter-line-end (point))
                                     nil t nil)))
          (overlay-put overlay 'line-prefix
                       (if first
                           (concat (propertize glyph
                                               'face (list label-face gutter-face))
                                   opencode-shell--gutter-margin)
                         blank))
          ;; Word-wrapped continuations of any line (including the first)
          ;; keep the color strip aligned without repeating the glyph.
          (overlay-put overlay 'wrap-prefix blank)
          (push overlay overlays)
          (setq first nil))
        (forward-line 1)))
    (setq entry (plist-put entry role overlays))
    (puthash turn entry opencode-shell--turn-gutters)))

(defun opencode-shell--assistant-text (turn)
  "Return TURN's assistant display text, or nil when it has none."
  (let ((text (opencode-shell--assistant-display-text turn)))
    (unless (string-empty-p text) text)))

(defun opencode-shell--apply-response-gutter (turn response-begin response-end)
  "Apply TURN's assistant gutter within RESPONSE-BEGIN..RESPONSE-END.
Only a completed turn renders its assistant body; status and tool-name
text stay unmarked so transient spinners keep no role label."
  (when (eq (opencode-shell--turn-status turn) 'complete)
    (let* ((text (opencode-shell--assistant-text turn))
           (tool-length (length (opencode-shell--tool-name-display turn)))
           (start (min response-end (+ response-begin tool-length)))
           (end (min response-end (+ start (length text)))))
      (when (< start end)
        (opencode-shell--apply-turn-gutter
         turn :response start end
         opencode-shell--gutter-glyph-assistant
         'opencode-shell-assistant-face
         'opencode-shell-assistant-gutter-face)))))

(defun opencode-shell--insert-user-prompt (turn)
  "Insert TURN's immutable user prompt and return its bounds."
  (let ((begin (point))
        (body (or (opencode-shell--turn-user turn) "")))
    (let ((body-begin (point)))
      (insert (propertize body 'face 'opencode-shell-composer-face)
              (propertize "\n" 'face 'opencode-shell-composer-face))
      (opencode-shell--apply-turn-gutter turn :user body-begin (point)
                                         opencode-shell--gutter-glyph-user
                                         'opencode-shell-user-face
                                         'opencode-shell-user-gutter-face)
      (let ((background (make-overlay body-begin (point) nil nil nil)))
        (overlay-put background 'face 'opencode-shell-composer-face)
        (overlay-put background 'priority 1)
        (overlay-put background 'evaporate t)))
    (insert "\n")
    (cons begin (point))))

(defun opencode-shell--insert-turn-blocks (turn)
  "Insert immutable user and canonical response blocks for TURN."
  (pcase-let ((`(,user-begin . ,user-end)
               (opencode-shell--insert-user-prompt turn)))
    (opencode-shell--insert-permission-results (opencode-shell--turn-id turn))
    (let ((response-begin (point)))
      (insert (opencode-shell--response-display turn))
      (let ((response-end (point)))
        (opencode-shell--apply-response-gutter turn response-begin response-end)
        (add-text-properties user-begin user-end
                             '(read-only t rear-nonsticky (read-only face)))
        (add-text-properties response-begin response-end
                             '(read-only t rear-nonsticky (read-only face)))
        (setf (opencode-shell--turn-user-begin turn) (copy-marker user-begin)
              (opencode-shell--turn-user-end turn) (copy-marker user-end)
              (opencode-shell--turn-response-begin turn)
              (copy-marker response-begin)
              (opencode-shell--turn-response-end turn)
              (copy-marker response-end))))))

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

(defun opencode-shell--response-display (turn &optional relocated)
  "Return the propertized response display for TURN."
  (if (and (not relocated)
           (eq turn (opencode-shell--permission-status-turn)))
      ""
    (concat
   (opencode-shell--tool-name-display turn)
   (if (eq (opencode-shell--turn-status turn) 'complete)
       (concat (or (opencode-shell--assistant-display-text turn) "")
               (opencode-shell--turn-terminal-error-suffix turn) "\n\n")
     (propertize
      (pcase (opencode-shell--turn-status turn)
        ('sending (opencode-shell--status-display "Sending"))
        ('thinking (opencode-shell--status-display "Thinking"))
        ('receiving (opencode-shell--status-display "Receiving"))
        ('recovering (opencode-shell--status-display "Recovering"))
        ('aborting (opencode-shell--status-display "Aborting"))
        ('error "Request state is uncertain; resync with g r\n\n")
        (_ (opencode-shell--status-display "Waiting for response")))
       'face (if (eq (opencode-shell--turn-status turn) 'error)
                 'opencode-shell-error-face 'opencode-shell-waiting-face))))))

(defun opencode-shell--tool-name-display (turn)
  "Return payload-free tool names observed in TURN."
  (let ((names (opencode-shell-response-tool-names
                (opencode-shell--turn-parts turn))))
    (if names
        (propertize
         (concat (mapconcat (lambda (name) (format "TOOL> %s" name))
                            names "\n")
                 "\n\n")
         'font-lock-face 'shadow)
      "")))

(defun opencode-shell--permission-status-turn ()
  "Return the latest nonterminal turn while permission blocks input."
  (and (opencode-shell--human-interaction-blocked-p)
       (seq-find (lambda (turn)
                   (not (eq (opencode-shell--turn-status turn) 'complete)))
                 (reverse opencode-shell--turns))))

(defun opencode-shell--permission-status-display ()
  "Return hydration recovery or transient interaction status."
  (cond
   ((plist-get opencode-shell--hydration-state :failed)
    (propertize "Session sync failed; retry with g r\n\n"
                'face 'opencode-shell-error-face))
   ((plist-get opencode-shell--hydration-state :pending)
    (propertize (opencode-shell--status-display "Loading session")
                'face 'opencode-shell-waiting-face))
   ((when-let ((turn (opencode-shell--permission-status-turn)))
      (opencode-shell--response-display turn t)))))

(defun opencode-shell--status-display (label)
  "Return LABEL with the current UI-only spinner frame."
  (concat label " "
          (propertize (string opencode-shell--spinner-character)
                      'opencode-shell-spinner t)
          "\n\n"))

(defun opencode-shell--clear-spinner-overlays ()
  "Delete presentation-only spinner overlays in the current buffer."
  (mapc #'delete-overlay opencode-shell--spinner-overlays)
  (setq opencode-shell--spinner-overlays nil))

(defun opencode-shell--spinner-frame ()
  "Return the current right-growing spinner display frame."
  (make-string (1+ opencode-shell--animation-frame)
               opencode-shell--spinner-character))

(defun opencode-shell--reset-spinner-frame ()
  "Reset transient progress presentation to its first frame."
  (setq opencode-shell--animation-frame 0)
  (opencode-shell--render-status-animation))

(defun opencode-shell--refresh-spinner-overlays ()
  "Recreate spinner overlays for status slots in the current buffer."
  (opencode-shell--clear-spinner-overlays)
  (let ((position (point-min)))
    (while (setq position
                 (text-property-any position (point-max)
                                    'opencode-shell-spinner t))
      (let ((overlay (make-overlay position (1+ position) nil t nil)))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'display (opencode-shell--spinner-frame))
        (push overlay opencode-shell--spinner-overlays))
      (setq position (1+ position)))))

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
        (set-marker end (point))
        (opencode-shell--apply-response-gutter turn position (point))))))

(defun opencode-shell--render-status-animation ()
  "Update transient spinner presentation without modifying buffer text."
  (setq opencode-shell--spinner-overlays
        (seq-filter #'overlay-buffer opencode-shell--spinner-overlays))
  (dolist (overlay opencode-shell--spinner-overlays)
    (overlay-put overlay 'display (opencode-shell--spinner-frame)))
  (when opencode-shell--spinner-overlays
    (force-window-update (current-buffer))))

(defun opencode-shell--animation-tick ()
  "Advance one UI-only spinner frame without issuing network requests."
  (setq opencode-shell--animation-frame
        (mod (1+ opencode-shell--animation-frame)
             opencode-shell--spinner-max-width))
  (opencode-shell--render-status-animation))

(defun opencode-shell--render-turns (&optional force changed-turns)
  "Render immutable turn blocks without changing composer bytes or point.
When FORCE is non-nil, rebuild every turn so anchored event positions settle.
Otherwise update only CHANGED-TURNS when that list is non-nil."
  (opencode-shell--without-user-undo
   (let* ((composer-offset (and (opencode-shell--point-in-composer-p)
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
        (dolist (turn (or changed-turns opencode-shell--rendered-turns))
          (when (memq turn opencode-shell--rendered-turns)
            (save-excursion (opencode-shell--update-turn-response turn)))))
      (if append-only
          (save-excursion
            (when (< known-count (length opencode-shell--turns))
              (goto-char opencode-shell--transcript-end)
              (dolist (turn (nthcdr known-count opencode-shell--turns))
                (opencode-shell--insert-turn-blocks turn))))
        (dolist (turn opencode-shell--turns) (opencode-shell--discard-turn-markers turn))
        ;; Drop gutters of turns no longer in the transcript before rebuilding.
        (opencode-shell--clear-all-turn-gutters)
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
                 opencode-shell--composer-label-visible)
        (opencode-shell--remove-composer-labels opencode-shell--composer-start)
        (setq opencode-shell--composer-label-visible nil))
      (when (and append-only
                 (opencode-shell--composer-visible-p)
                 (not opencode-shell--composer-label-visible)
                 (or opencode-shell--submit-in-flight
                     opencode-shell--rendered-turns))
        (opencode-shell--remove-composer-labels opencode-shell--composer-start)
        (goto-char opencode-shell--composer-start)
        (opencode-shell--insert-composer-label)
        (setq opencode-shell--composer-label-visible t))
      (setq opencode-shell--rendered-turns (copy-sequence opencode-shell--turns))
      (save-excursion
        (goto-char (max (point-min)
                        (- (point-max) 1 (length composer-text))))
        (set-marker opencode-shell--transcript-end (point))
        (set-marker opencode-shell--permission-begin (point))
         (set-marker opencode-shell--permission-end (point))
         (set-marker opencode-shell--composer-start (point))))
    (opencode-shell--render-permissions)
    (if composer-offset
        (goto-char (min (point-max) (+ opencode-shell--composer-start composer-offset)))
      (goto-char (min old-point opencode-shell--transcript-end)))))
   (opencode-shell--sync-input-policy))

(defun opencode-shell--flush-render ()
  "Render the latest reconciled state when the current buffer is visible."
  (when (and opencode-shell--render-dirty
             (get-buffer-window (current-buffer) t))
    (let ((event opencode-shell--render-event)
          (force opencode-shell--render-force)
          (changed-turns opencode-shell--render-dirty-turns))
      (setq opencode-shell--render-dirty nil
            opencode-shell--render-event nil
            opencode-shell--render-force nil
            opencode-shell--render-dirty-turns nil)
      (opencode-shell--render-turns force changed-turns)
      (opencode-shell--log-lifecycle event)
      (force-mode-line-update))))

(defun opencode-shell--schedule-render (&optional event force changed-turns)
  "Mark presentation dirty and coalesce visible rendering under EVENT.
When FORCE is non-nil, rebuild turn blocks during the next render.  Merge
CHANGED-TURNS into the response blocks pending incremental update."
  (setq opencode-shell--render-dirty t
        opencode-shell--render-event (or event opencode-shell--render-event)
        opencode-shell--render-force (or force opencode-shell--render-force)
        opencode-shell--render-dirty-turns
        (seq-uniq (append opencode-shell--render-dirty-turns changed-turns) #'eq))
  (when (get-buffer-window (current-buffer) t)
    (opencode-shell-async-enqueue
     (current-buffer) 'render opencode-shell--generation
     #'opencode-shell--flush-render))
  (unless (get-buffer-window (current-buffer) t)
    (opencode-shell--log-lifecycle "render-deferred:hidden")))

(defun opencode-shell--render-if-visible ()
  "Schedule one render when a dirty transcript becomes visible."
  (when (and opencode-shell--connection-stale opencode-shell--profile
             (get-buffer-window (current-buffer) t)
             (opencode-shell-recovery-ready-p
              (opencode-shell--server-key opencode-shell--profile)))
    (setq opencode-shell--connection-stale nil)
    (opencode-shell--resync t))
  (when (and opencode-shell--render-dirty
             (get-buffer-window (current-buffer) t))
        (opencode-shell--schedule-render opencode-shell--render-event
                                         opencode-shell--render-force
                                         opencode-shell--render-dirty-turns)))

(defun opencode-shell--transcript-state-signature ()
  "Return the normalized transcript state that can affect presentation."
  (list
   (mapcar
    (lambda (turn)
      (list (opencode-shell--turn-id turn)
            (opencode-shell--turn-server-user-id turn)
            (opencode-shell--turn-user turn)
            (opencode-shell--turn-assistant turn)
            (copy-tree (opencode-shell--turn-parts turn))
            (opencode-shell--turn-status turn)
            (opencode-shell--turn-acknowledged turn)
            (opencode-shell--turn-locally-settled turn)
            (opencode-shell--turn-terminal-error turn)))
    opencode-shell--turns)
   opencode-shell--request-status
   opencode-shell--submit-in-flight
   (opencode-shell--composer-visible-p)))

(defun opencode-shell--message-lifecycle-signature ()
  "Return top-level message lifecycle state outside individual turns."
  (list opencode-shell--request-status
        opencode-shell--submit-in-flight
        (opencode-shell--composer-visible-p)))

(defun opencode-shell--update-message-lifecycle-state ()
  "Derive request and composer lifecycle state from normalized turns."
  (setq opencode-shell--request-status
        (symbol-name
         (opencode-shell-state-request-phase
          (mapcar #'opencode-shell--turn-status opencode-shell--turns)
          opencode-shell--submit-in-flight)))
  (when-let ((turn (seq-find
                    (lambda (entry)
                      (equal opencode-shell--submit-in-flight
                             (opencode-shell--turn-id entry)))
                    opencode-shell--turns)))
    (when (eq (opencode-shell--turn-status turn) 'complete)
      (setq opencode-shell--submit-in-flight nil)))
  (when (and (not (opencode-shell--human-interaction-blocked-p))
             (not (opencode-shell-state-polling-needed-p
                   (mapcar #'opencode-shell--turn-status opencode-shell--turns)
                   opencode-shell--submit-in-flight
                    (not (opencode-shell--initial-hydration-complete-p)))))
    (opencode-shell--stop-polling)))


(defun opencode-shell--render-messages
    (messages &optional sequence defer-render authoritative)
  "Reconcile chronological message envelopes from MESSAGES.
Render immediately unless DEFER-RENDER is non-nil.  When AUTHORITATIVE is
non-nil, remove cached server messages absent from the snapshot."
  (when (or (null sequence) (> sequence opencode-shell--message-applied-sequence))
    (let ((before (opencode-shell--message-lifecycle-signature))
          cache-result)
      (when sequence (setq opencode-shell--message-applied-sequence sequence))
      (setq cache-result
            (opencode-shell--cache-message-snapshot messages authoritative))
      (when authoritative
        (opencode-shell--restore-agent-model-history messages))
      (setq opencode-shell--turns (opencode-shell--normalize-turns messages))
      (setq opencode-shell--normalized-changed-turns
            (seq-uniq
             (append (plist-get cache-result :changed-turns)
                     opencode-shell--normalized-changed-turns)
             #'eq))
      (opencode-shell--update-message-lifecycle-state)
      (when (or opencode-shell--normalized-changed-turns
                (not (equal before (opencode-shell--message-lifecycle-signature))))
        (when authoritative (opencode-shell--reset-spinner-frame))
        (let ((event (if sequence (format "messages:%d" sequence) "messages")))
          (if defer-render
              (opencode-shell--schedule-render
               event (plist-get cache-result :force)
               opencode-shell--normalized-changed-turns)
            (setq opencode-shell--render-dirty t
                  opencode-shell--render-event event
                  opencode-shell--render-force (plist-get cache-result :force)
                  opencode-shell--render-dirty-turns
                  opencode-shell--normalized-changed-turns)
            (if (get-buffer-window (current-buffer) t)
                (opencode-shell--flush-render)
              ;; Direct callers, including deterministic tests and initial buffer
              ;; construction, require an immediate render even without a window.
              (let ((opencode-shell--render-dirty t))
                (opencode-shell--render-turns
                 (plist-get cache-result :force)
                 opencode-shell--normalized-changed-turns)
                (opencode-shell--log-lifecycle event)
                (force-mode-line-update))
              (setq opencode-shell--render-dirty nil
                    opencode-shell--render-event nil
                    opencode-shell--render-dirty-turns nil))))))))

(defun opencode-shell--guarded-request (key method path callback &optional body error-callback)
  "Request PATH once per generation under KEY."
  (unless (alist-get key opencode-shell--in-flight)
    (let ((origin (current-buffer))
          (generation opencode-shell--generation)
          (request-mode major-mode)
          (attempt (make-symbol "opencode-snapshot")) timer response)
      (setf (alist-get key opencode-shell--in-flight) attempt)
      (cl-labels ((current-p ()
                    (and (eq major-mode request-mode)
                         (= generation opencode-shell--generation)
                         (eq attempt (alist-get key opencode-shell--in-flight))))
                  (settle (success value)
                    (when (current-p)
                      (when (timerp timer) (cancel-timer timer))
                      (setf (alist-get key opencode-shell--in-flight) nil)
                      (if success (funcall callback value)
                        (when error-callback (funcall error-callback))))))
        (when (equal method "GET")
          (setq timer
                (run-at-time
                 opencode-shell--snapshot-request-timeout nil
                 (lambda ()
                   (when (buffer-live-p origin)
                     (with-current-buffer origin
                       (when (current-p)
                         (when (buffer-live-p response)
                           (when-let ((process (get-buffer-process response)))
                             (delete-process process))
                           (kill-buffer response))
                         (settle nil nil))))))))
        (setq response
              (opencode-shell--request
               method path
               (lambda (value) (settle t value))
               body nil
               (lambda () (settle nil nil))))))))

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
       (opencode-shell--rename-transcript-for-title opencode-shell--session-title)
       (force-mode-line-update)))
   nil nil))

(defun opencode-shell--resync (&optional full resource manual)
  "Resync RESOURCE, or all polling state when RESOURCE is nil or `all'.
When FULL is non-nil, also refresh metadata and capabilities."
  (interactive (list t nil t))
  (if (and manual
           opencode-shell--profile
           (opencode-shell--ssh-forwarded-profile-p opencode-shell--profile)
           (opencode-shell-recovery-offline-p
            (opencode-shell--server-key opencode-shell--profile)))
      (let ((origin (current-buffer))
            (generation opencode-shell--generation)
            (profile opencode-shell--profile))
        (opencode-shell-recovery-manual-reset (opencode-shell--server-key profile))
        (opencode-shell--ssh-retry
         profile
         (lambda ()
           (when (buffer-live-p origin)
             (with-current-buffer origin
               (when (= generation opencode-shell--generation)
                 (unless (when-let ((runtime (opencode-shell-async-runtime-get
                                             (opencode-shell--server-key profile))))
                           (gethash origin (plist-get runtime :subscribers)))
                   (setq opencode-shell--connection-stale nil)
                   (opencode-shell--resync t))))))
         (lambda () (buffer-live-p origin))))
    (opencode-shell--resync-snapshots full resource)))

(defun opencode-shell--resync-snapshots (&optional full resource)
  "Fetch the existing authoritative session snapshots."
  (opencode-shell--retry-hydration
   (if (memq resource '(nil all))
       '(messages permissions questions)
     (list resource)))
  (when full (opencode-shell--refresh-session-metadata))
  (when (memq resource '(nil all messages))
    (unless (alist-get 'messages opencode-shell--in-flight)
      (let ((sequence (cl-incf opencode-shell--message-request-sequence))
            (revision opencode-shell--message-state-revision))
        (opencode-shell--guarded-request
         'messages
         "GET" (format "/session/%s/message" opencode-shell--session-id)
         (lambda (messages)
           (opencode-shell--settle-hydration 'messages t)
           (if (= revision opencode-shell--message-state-revision)
               (opencode-shell--render-messages messages sequence t t)
             (opencode-shell--schedule-event-reconciliation 'messages)))
         nil (lambda () (opencode-shell--settle-hydration 'messages nil))))))
  (when (memq resource '(nil all permissions))
    (opencode-shell--guarded-request
     'permissions "GET" "/permission"
     (lambda (items) (opencode-shell--receive-permissions items t))
     nil #'opencode-shell--consume-permission-refresh-pending))
  (when (memq resource '(nil all questions))
    (opencode-shell--refresh-questions))
  (when (and full
             (not opencode-shell--capabilities-loading))
    (let ((remaining 3) failed)
      (setq opencode-shell--capabilities-loading t)
      (cl-labels ((settle (failure)
                    (setq failed (or failed failure)
                          remaining (1- remaining))
                     (when (zerop remaining)
                       (setq opencode-shell--capabilities-loading nil
                             opencode-shell--capabilities-loaded (not failed))
                       (unless failed
                         (when-let ((key (and opencode-shell--profile
                                              (opencode-shell--server-key opencode-shell--profile))))
                           (puthash key
                                    (list :models opencode-shell--models
                                          :agents opencode-shell--agents
                                          :configured-model opencode-shell--configured-model)
                                    opencode-shell--capabilities-cache))
                         (opencode-shell--initialize-server-defaults))
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
          nil (lambda () (settle t)))
         (opencode-shell--guarded-request
          'config "GET" "/config"
          (lambda (response)
            (setq opencode-shell--configured-model
                  (opencode-shell--model-value
                   (opencode-shell--get response 'model)))
            (settle nil))
          nil (lambda () (settle t)))))))

(defun opencode-shell--select-model ()
  "Select a server-advertised model for subsequent prompts."
  (interactive)
  (unless opencode-shell--models (user-error "No models loaded; resync first"))
  (let* ((choice (completing-read "Model: " opencode-shell--models nil t))
         (entry (assoc choice opencode-shell--models)))
    (unless entry (user-error "Model is no longer available: %s" choice))
    (setq opencode-shell--selected-model (cdr entry)
          opencode-shell--selection-user-chosen-p t)
    (when opencode-shell--selected-agent
      (setf (alist-get opencode-shell--selected-agent
                        opencode-shell--agent-model-overrides nil nil #'equal)
            (cdr entry))))
  (force-mode-line-update))

(defun opencode-shell--select-agent ()
  "Select a server-advertised agent name for subsequent prompts."
  (interactive)
  (unless opencode-shell--agents (user-error "No agents loaded; resync first"))
  (let* ((choice (completing-read "Agent: " opencode-shell--agents nil t))
         (entry (assoc choice opencode-shell--agents)))
    (unless entry (user-error "Agent is no longer available: %s" choice))
    (opencode-shell--activate-agent (car entry) t)))

(defun opencode-shell--next-agent ()
  "Select the next visible primary agent, wrapping at the end."
  (interactive)
  (unless opencode-shell--agents (user-error "No agents loaded; resync first"))
  (let* ((names (mapcar #'car opencode-shell--agents))
         (tail (member opencode-shell--selected-agent names)))
    (opencode-shell--activate-agent (or (cadr tail) (car names)) t)))

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
  (when (and opencode-shell--profile
             (opencode-shell--ssh-forwarded-profile-p opencode-shell--profile)
             (not (opencode-shell-recovery-ready-p
                   (opencode-shell--server-key opencode-shell--profile))))
    (user-error "OpenCode SSH transport offline; retry with g r"))
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
         (opencode-shell--schedule-render "submit-ack")
         (opencode-shell--log-lifecycle "submit-ack")
         (opencode-shell--resync))
        (cons `(messageID . ,(opencode-shell--turn-id turn))
              (opencode-shell--prompt-body text)) nil
        (lambda ()
         (setf (opencode-shell--turn-status turn) 'recovering)
         (setq opencode-shell--request-status "recovering")
         (opencode-shell--schedule-render "submit-error")
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
                                (opencode-shell--schedule-render "abort-ack")
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

(defun opencode-shell--interaction-request (kind id method path success
                                                    &optional body failure)
  "Send a KIND request for ID and settle only its matching callback."
  (setq opencode-shell--interaction-state
        (opencode-shell-interaction-begin
         opencode-shell--interaction-state kind id))
  (opencode-shell--request
   method path
   (lambda (result)
     (when (opencode-shell-interaction-matches-p
            opencode-shell--interaction-state kind id)
       (setq opencode-shell--interaction-state
             (opencode-shell-interaction-finish
              opencode-shell--interaction-state kind id))
       (funcall success result)))
   body nil
   (lambda ()
     (when (opencode-shell-interaction-matches-p
            opencode-shell--interaction-state kind id)
       (setq opencode-shell--interaction-state
             (opencode-shell-interaction-finish
              opencode-shell--interaction-state kind id))
       (when failure (funcall failure))))))

(defun opencode-shell--permission-reply (reply)
  "Send REPLY for the inline permission at point."
  (let* ((item (opencode-shell--permission-at-point))
         (id (opencode-shell--permission-id item)))
    (opencode-shell--log-lifecycle "permission-reply" t)
    (opencode-shell--interaction-request
     'permission id "POST" (format "/permission/%s/reply" id)
     (lambda (_)
       (unless (opencode-shell--resolved-permission id)
         (let ((record `((id . ,id) (reply . ,reply)
                         (description . ,(opencode-shell--permission-description item))
                         (after-turn-id . ,(when-let ((turn (car (last opencode-shell--turns))))
                                             (opencode-shell--turn-id turn))))))
           (setq opencode-shell--resolved-permissions
                 (append opencode-shell--resolved-permissions (list record)))))
       (setq opencode-shell--permissions
             (opencode-shell-interaction-remove-pending
              opencode-shell--permissions id #'opencode-shell--permission-id))
       (opencode-shell--schedule-render "permission-reply-ok" t)
       (opencode-shell--refresh-permissions)
       (unless (opencode-shell--human-interaction-blocked-p)
         (opencode-shell--resync))
       (opencode-shell--log-lifecycle "permission-reply-ok")
       (message "Permission %s" reply))
     `((reply . ,reply))
     (lambda ()
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
  (when (opencode-shell-interaction-active-p
         opencode-shell--interaction-state 'question)
    (user-error "Question reply already in progress"))
  (if (and confirm (not (yes-or-no-p "Answer this question? (No rejects) ")))
      (opencode-shell--question-reject item)
    (let* ((id (opencode-shell--question-id item))
           (answers (vconcat
                     (mapcar #'opencode-shell--question-answer
                             (or (opencode-shell--get item 'questions)
                                 (list item))))))
      (opencode-shell--interaction-request
       'question id "POST" (format "/question/%s/reply" id)
       (lambda (_)
         (setq opencode-shell--questions-pending
               (opencode-shell-interaction-remove-pending
                opencode-shell--questions-pending id #'opencode-shell--question-id))
         (opencode-shell--schedule-render "question-reply")
         (opencode-shell--resync nil)
         (message "Question reply sent"))
       `((answers . ,answers))
       (lambda ()
         (opencode-shell--resync nil)
         (message "Question reply failed"))))))

(defun opencode-shell--question-reject (&optional item)
  "Reject pending question ITEM or the current inline question."
  (interactive)
  (when (opencode-shell-interaction-active-p
         opencode-shell--interaction-state 'question)
    (user-error "Question reply already in progress"))
  (setq item (or item (get-text-property (point) 'opencode-shell-question)
                 (car opencode-shell--questions-pending)
                 (user-error "No pending question")))
  (let ((id (opencode-shell--question-id item)))
    (opencode-shell--interaction-request
     'question id "POST" (format "/question/%s/reject" id)
     (lambda (_)
       (setq opencode-shell--questions-pending
             (opencode-shell-interaction-remove-pending
              opencode-shell--questions-pending id #'opencode-shell--question-id))
       (opencode-shell--schedule-render "question-reject")
       (opencode-shell--resync nil)
       (message "Question rejected"))
     '()
     (lambda ()
       (opencode-shell--resync nil)
       (message "Question rejection failed")))))

(defun opencode-shell--sync-input-policy ()
  "Synchronize regional Composer protection with lifecycle readiness."
  (let ((ready (opencode-shell--composer-visible-p)))
    (when (and opencode-shell--composer-input-ready
               (not ready)
               (bound-and-true-p evil-local-mode)
               (fboundp 'evil-insert-state-p)
               (evil-insert-state-p)
               (fboundp 'evil-normal-state))
      (evil-normal-state))
    (setq-local opencode-shell--composer-input-ready ready)
    (if ready
        (when (overlayp opencode-shell--composer-protection-overlay)
          (delete-overlay opencode-shell--composer-protection-overlay)
          (setq opencode-shell--composer-protection-overlay nil))
      (when (and (markerp opencode-shell--composer-start)
                 (marker-position opencode-shell--composer-start))
        (unless (overlayp opencode-shell--composer-protection-overlay)
          (setq opencode-shell--composer-protection-overlay
                (make-overlay opencode-shell--composer-start (point-max) nil nil t)))
        (move-overlay opencode-shell--composer-protection-overlay
                      opencode-shell--composer-start (point-max))
        (overlay-put opencode-shell--composer-protection-overlay 'read-only t)
        (overlay-put opencode-shell--composer-protection-overlay
                     'modification-hooks nil)))))

(defvar evil-move-beyond-eol nil)
(defvar evil-local-mode)

(defun opencode-shell--configure-evil-buffer ()
  "Apply Evil policy local to an OpenCode transcript buffer."
  (setq-local evil-move-beyond-eol t))

(defun opencode-shell--evil-open-below (count)
  "Reuse an empty composer from `Prompt>`; otherwise open below COUNT times."
  (interactive "p")
  (declare-function evil-insert-state "evil-states")
  (declare-function evil-open-below "evil-commands")
  (cond
   ((not (opencode-shell--evil-edit-allowed-p t)) nil)
   ((and (string-empty-p (opencode-shell--composer-text))
         (= (line-beginning-position)
            (- opencode-shell--composer-start
               (length opencode-shell--composer-label))))
    (goto-char opencode-shell--composer-start)
     (evil-insert-state))
     (t (evil-open-below count))))

(defun opencode-shell--evil-backspace ()
  "Delete backward in the Composer without crossing its empty boundary."
  (interactive)
  (declare-function evil-delete-backward-char-and-join "evil-commands")
  (unless (and (opencode-shell--in-composer-p)
               (string-empty-p (opencode-shell--composer-text))
               (= (point) opencode-shell--composer-start))
    (call-interactively #'evil-delete-backward-char-and-join)))

(defun opencode-shell--evil-edit-allowed-p (&optional prompt-line-p)
  "Return non-nil when the current position may invoke an Evil edit.
When PROMPT-LINE-P is non-nil, also allow the empty Composer's `Prompt>` line."
  (or (opencode-shell--in-composer-p)
      (and prompt-line-p
           (opencode-shell--composer-visible-p)
           (string-empty-p (opencode-shell--composer-text))
           (= (line-beginning-position)
              (- opencode-shell--composer-start
                 (length opencode-shell--composer-label))))))

(defconst opencode-shell--evil-mutating-commands
  '(evil-append evil-append-line evil-change evil-change-line
    evil-delete evil-delete-line evil-indent evil-insert evil-insert-line
    evil-invert-char evil-join evil-open-above evil-open-below
    evil-paste-after evil-paste-before evil-replace evil-replace-state
    evil-shift-left evil-shift-right evil-substitute evil-substitute-line)
  "Evil commands that may change transcript bytes.")

(defun opencode-shell--evil-edit-in-composer (command)
  "Run Evil editing COMMAND only at the visible writable Composer."
  (interactive)
  (when (opencode-shell--in-composer-p)
    (call-interactively command)))

(defun opencode-shell--evil-gated-command (command)
  "Return the interactive gate command for Evil COMMAND."
  (let ((wrapper (intern (format "opencode-shell--evil-gate-%s" command))))
    (unless (fboundp wrapper)
      (fset wrapper
            `(lambda ()
               (interactive)
               (opencode-shell--evil-edit-in-composer #',command))))
    wrapper))

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
    (kbd "o") #'opencode-shell--evil-open-below
    (kbd "C-n") #'opencode-shell--next-turn
    (kbd "C-p") #'opencode-shell--previous-turn
    (kbd "C-c C-c") #'opencode-shell--submit
    (kbd "C-c C-v") #'opencode-shell--select-model
    (kbd "C-c C-m") #'opencode-shell--select-agent
    (kbd "C-<tab>") #'opencode-shell--next-agent)
   (dolist (command opencode-shell--evil-mutating-commands)
     (define-key opencode-shell-mode-map (vector 'remap command)
       (opencode-shell--evil-gated-command command)))
   (evil-define-key* 'insert opencode-shell-mode-map
     (kbd "?") #'self-insert-command
     (kbd "DEL") #'opencode-shell--evil-backspace
     (kbd "<backspace>") #'opencode-shell--evil-backspace
     (kbd "RET") #'newline
    (kbd "<return>") #'newline
    (kbd "C-n") #'opencode-shell--next-turn
    (kbd "C-p") #'opencode-shell--previous-turn
    (kbd "C-<tab>") #'opencode-shell--next-agent)
  (evil-define-key* 'normal opencode-shell-sessions-mode-map
    (kbd "RET") #'opencode-shell--open-at-point
    (kbd "g r") #'opencode-shell--refresh
    (kbd "c") #'opencode-shell--create-session
    (kbd "/") #'opencode-shell--filter
    (kbd "d") #'opencode-shell--delete-session
    (kbd "?") #'opencode-shell-sessions-help))

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
         (callbacks (append (plist-get state :retry-callbacks)
                            (plist-get state :callbacks))))
     (when (opencode-shell--attempt-current-p key attempt)
       (when (opencode-shell--ssh-forwarded-profile-p (plist-get state :profile))
         (opencode-shell-recovery-success key)
         (opencode-shell-async-resume-runtime key))
        (setq state (plist-put state :callbacks nil)
              state (plist-put state :retry-callbacks nil)
              state (plist-put state :failure-callbacks nil)
            state (plist-put state :checking nil)
            state (plist-put state :starting nil)
            state (plist-put state :attempt nil))
      (puthash key state opencode-shell--servers)
       (dolist (entry (nreverse callbacks))
         (opencode-shell--ssh-invoke-start-callback entry)))))

(defun opencode-shell--fail-start (key attempt message-text)
  "Abandon KEY's ATTEMPT and report MESSAGE-TEXT."
  (let* ((state (gethash key opencode-shell--servers))
          (process (plist-get state :process))
          (profile (plist-get state :profile))
          (failures (plist-get state :failure-callbacks))
          (ssh (and profile (opencode-shell--ssh-forwarded-profile-p profile))))
    (when (and state (opencode-shell--attempt-current-p key attempt))
      (when (and (plist-get state :starting)
                  (not ssh) (processp process) (process-live-p process))
        (delete-process process))
      (if ssh
          (progn
            (unless (and (processp process) (process-live-p process))
              (setf (plist-get state :process) nil
                    (plist-get state :owned) nil))
            (setf (plist-get state :checking) nil
                  (plist-get state :starting) nil
                  (plist-get state :attempt) nil
                  (plist-get state :retry-callbacks) nil
                  (plist-get state :failure-callbacks)
                  (seq-filter #'consp failures))
            (puthash key state opencode-shell--servers)
            (opencode-shell--ssh-disconnected profile)
            (dolist (failure failures)
              (when (functionp failure) (funcall failure)))
            (when (opencode-shell-recovery-exhausted-p key)
              (dolist (failure (plist-get state :failure-callbacks))
                (funcall (cadr failure)))
              (setf (plist-get state :callbacks) nil
                    (plist-get state :failure-callbacks) nil)
              (unless (plist-get state :owned)
                (remhash key opencode-shell--servers))))
        (remhash key opencode-shell--servers)
        (message "OpenCode: %s" message-text)
        (dolist (failure failures) (funcall failure))))))

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
  (let ((state (gethash key opencode-shell--servers)))
    (when (and state
               (memq (process-status process) '(exit signal failed))
               (eq process (plist-get state :process)))
      (if (plist-get state :stopping)
          (remhash key opencode-shell--servers)
        (if-let ((restart-profile (plist-get state :restart-profile)))
          (let ((callbacks (plist-get state :callbacks)))
            (remhash key opencode-shell--servers)
            (opencode-shell--start-server
             restart-profile
             (lambda (_ready)
                (dolist (entry (nreverse callbacks))
                  (opencode-shell--ssh-invoke-start-callback entry)))))
        (if (and (plist-get state :starting)
                 (opencode-shell--attempt-current-p key attempt))
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
           (remhash key opencode-shell--servers)
           (when-let ((profile (plist-get state :profile)))
             (when (opencode-shell--ssh-forwarded-profile-p profile)
                (opencode-shell--ssh-disconnected profile)))))))))

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
        (let* ((default-directory
                 (or (and (opencode-shell--profile-remote-p profile)
                          temporary-file-directory)
                     (plist-get profile :server-directory)
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

(defun opencode-shell--start-server
    (&optional profile callback failure-callback retry-callback)
  "Start PROFILE's configured process and invoke CALLBACK when healthy.
Concurrent starts for one server are coalesced.  A remote profile may use
`:start-command' to establish its transport, such as an SSH tunnel."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
          (key (opencode-shell--server-key profile))
          (state (gethash key opencode-shell--servers))
          (command (plist-get profile :start-command)))
    (let ((config (opencode-shell--validate-server-profile profile state)))
    (cond
      ((or (plist-get state :checking)
           (plist-get state :starting)
           (plist-get state :restart-profile))
       (when callback
          (let ((slot (if retry-callback :retry-callbacks :callbacks)))
            (puthash key (plist-put state slot
                                    (cons (cons callback profile)
                                          (plist-get state slot)))
                     opencode-shell--servers)))
        (when failure-callback
         (puthash key (plist-put state :failure-callbacks
                                 (cons failure-callback
                                       (plist-get state :failure-callbacks)))
                   opencode-shell--servers)))
      ((not command)
       (user-error "Profile has no :start-command"))
      ((and (plist-get state :owned)
            (process-live-p (plist-get state :process)))
      (let ((attempt (gensym "opencode-start-")))
        (puthash key (plist-put (plist-put (plist-put state :checking t)
                                          :attempt attempt)
                                 :callbacks (append (and (not retry-callback) callback
                                                         (list (cons callback profile)))
                                                    (plist-get state :callbacks)))
                  opencode-shell--servers)
        (setq state (gethash key opencode-shell--servers))
         (setf (plist-get state :profile) profile
               (plist-get state :retry-callbacks)
               (append (and retry-callback callback (list (cons callback profile)))
                       (plist-get state :retry-callbacks))
               (plist-get state :failure-callbacks)
              (append (and failure-callback (list failure-callback))
                      (plist-get state :failure-callbacks)))
        (opencode-shell--server-ready
         profile (lambda (ready &optional _)
                   (when (opencode-shell--attempt-current-p key attempt)
                     (if (or ready (not (opencode-shell--ssh-forwarded-profile-p profile)))
                         (opencode-shell--finish-start key attempt)
                        (opencode-shell--fail-start key attempt "SSH health unavailable")))))))
     (t
      (let ((attempt (gensym "opencode-start-")))
        (puthash key (list :checking t :attempt attempt :config config
                           :profile profile
                           :failure-callbacks
                           (append (and failure-callback (list failure-callback))
                                   (plist-get state :failure-callbacks))
                           :callbacks
                           (append (and (not retry-callback) callback
                                        (list (cons callback profile)))
                                   (plist-get state :callbacks))
                           :retry-callbacks
                           (append (and retry-callback callback
                                        (list (cons callback profile)))
                                   (plist-get state :retry-callbacks)))
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
     (puthash key (plist-put state :stopping t) opencode-shell--servers)
     (delete-process process)
    (remhash key opencode-shell--servers)))

(defun opencode-shell--restart-server (&optional profile)
  "Restart PROFILE's server.
When :restart-command is configured, it must fully stop the server (e.g.
via pkill) and return promptly; this client then starts a fresh instance
via :start-command once the kill command exits.  This works even when the
current server is not owned by this client.  Without :restart-command,
only an owned live process can be restarted."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
         (key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers))
         (process (plist-get state :process))
         (restart-cmd (plist-get profile :restart-command)))
    (unless (or (and (plist-get state :owned) (process-live-p process))
                restart-cmd)
      (user-error "OpenCode server is not owned by this client"))
    (when (plist-get state :restart-profile)
      (user-error "OpenCode server restart is already in progress"))
    (puthash key (plist-put state :restart-profile profile) opencode-shell--servers)
    (if restart-cmd
        (make-process
         :name (format "opencode-restart-%s" key)
         :command restart-cmd
         :noquery t
         :sentinel (lambda (_p _event)
                     (when (plist-get (gethash key opencode-shell--servers) :restart-profile)
                       (when (and process (process-live-p process))
                         (delete-process process))
                       (remhash key opencode-shell--servers)
                       (opencode-shell--start-server profile))))
      (delete-process process))))

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
  (if (plist-get profile :start-command)
       (opencode-shell--start-server-for-command
        profile (lambda (ready) (opencode-shell--sessions directory ready current-window)))
    (opencode-shell--sessions directory profile current-window)))

;;;###autoload
(defun opencode-shell (&optional profile)
  "Open sessions using PROFILE or the profile matching the current directory."
  (interactive)
  (opencode-shell--open-sessions
   (opencode-shell--profile-for-command profile)))

;;;###autoload
(defun opencode-shell-start (&optional profile)
  "Start a session using PROFILE or the profile matching the current directory."
  (interactive)
  (opencode-shell--start-session
   (opencode-shell--profile-for-command profile)))

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

(defun opencode-shell--forget-session-location (profile-key directory)
  "Remove persisted PROFILE-KEY and DIRECTORY, returning non-nil when found."
  (let* ((exact (cons profile-key directory))
         (canonical
          (cons profile-key (opencode-shell--session-location-directory directory)))
         (location
          (cond ((member exact opencode-shell--recent-session-locations) exact)
                ((member canonical opencode-shell--recent-session-locations)
                 canonical))))
    (when location
      (setq opencode-shell--recent-session-locations
            (delete location opencode-shell--recent-session-locations))
      (opencode-shell--save-recent-session-locations)
      t)))

(defun opencode-shell--live-browser-locations ()
  "Return (profile-key . directory) pairs for live session browser buffers."
  (let (locations)
    (dolist (buffer (buffer-list) locations)
      (with-current-buffer buffer
        (when (derived-mode-p 'opencode-shell-sessions-mode)
          (push (cons (opencode-shell--profile-key opencode-shell--profile)
                      (opencode-shell--session-location-directory
                       opencode-shell--directory))
                locations))))))

(defun opencode-shell--saved-session-location-entries (profiles)
  "Return saved location entries belonging to PROFILES."
  (let ((live (opencode-shell--live-browser-locations)))
    (delq nil
          (mapcar
           (lambda (location)
             (when-let ((profile
                         (seq-find (lambda (item)
                                     (equal (car location)
                                            (opencode-shell--profile-key item)))
                                   profiles)))
               (list profile (cdr location) 0
                     (member (cons (opencode-shell--profile-key profile)
                                   (opencode-shell--session-location-directory
                                    (cdr location)))
                             live))))
           opencode-shell--recent-session-locations))))

(defvar-local opencode-shell--session-location-minibuffer-mode nil)

(defun opencode-shell--install-session-location-minibuffer-map (command)
  "Install COMMAND for `C-k' above completion frontend maps in this minibuffer."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") command)
    (setq-local opencode-shell--session-location-minibuffer-mode t)
    (setq-local emulation-mode-map-alists
                (cons `((opencode-shell--session-location-minibuffer-mode . ,map))
                      emulation-mode-map-alists))))

(defun opencode-shell--session-location-candidate (text candidates)
  "Return the uniquely matching location candidate for minibuffer TEXT."
  (or (seq-find (lambda (item) (string= text (car item))) candidates)
      (let ((matches
             (seq-filter (lambda (item)
                           (string-match-p (regexp-quote text) (car item)))
                         candidates)))
        (and (= (length matches) 1) (car matches)))))

(defun opencode-shell--selected-session-location-candidate (candidates)
  "Return the currently highlighted location from CANDIDATES."
  (let ((selected
         (and (fboundp 'vertico--candidate)
              (ignore-errors (vertico--candidate)))))
    (or (and selected (assoc selected candidates))
        (opencode-shell--session-location-candidate
         (minibuffer-contents-no-properties) candidates))))

(defun opencode-shell--read-session-location (entries)
  "Read one PROFILE/DIRECTORY entry from ENTRIES with local deletion support."
  (let (choice deleted)
    (while (null choice)
      (let* ((active (seq-filter (lambda (entry) (nth 3 entry)) entries))
             (inactive (seq-remove (lambda (entry) (nth 3 entry)) entries))
             (make-candidate
              (lambda (entry)
                (opencode-shell--session-browser-candidate
                 (nth 0 entry) (nth 1 entry) (nth 3 entry))))
             (separator (propertize "──────── inactive ────────" 'face 'shadow))
             (candidates
              (append (mapcar make-candidate active)
                      (and active inactive (list (cons separator nil)))
                      (mapcar make-candidate inactive))))
        (unless candidates (user-error "No saved OpenCode session locations"))
        (setq deleted nil)
        (condition-case nil
             (let ((delete-location
                    (lambda ()
                      (interactive)
                      (let* ((candidate
                              (opencode-shell--selected-session-location-candidate
                               candidates))
                             (location (and candidate (cdr candidate))))
                        (unless location
                          (user-error "Select a saved location to delete"))
                        (unless (opencode-shell--forget-session-location
                                 (opencode-shell--profile-key (car location))
                                 (cdr location))
                          (user-error "Location is not saved"))
                        (setq entries
                              (seq-remove
                               (lambda (entry)
                                 (and (equal (nth 0 entry) (car location))
                                      (equal (nth 1 entry) (cdr location))))
                               entries)
                              deleted t)
                        (abort-recursive-edit)))))
              (let ((selection
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (opencode-shell--install-session-location-minibuffer-map
                            delete-location))
                       (completing-read "OpenCode profile : path: " candidates nil t))))
                (setq choice (assoc selection candidates))))
          (quit (unless deleted (signal 'quit nil))))
        (cond (deleted
               (setq choice (and (null entries) '(nil . nil))))
              ((and choice (null (cdr choice)))
               (setq choice nil)))))
    (cdr choice)))

;;;###autoload
(defun opencode-shell-find-session (&optional profile)
  "Select a saved PROFILE directory and open its session browser."
  (interactive)
  (opencode-shell--validate-profiles)
  (let* ((profiles (if profile
                       (list (opencode-shell--resolve-or-read-profile profile))
                     opencode-shell-profiles)))
    (unless profiles (user-error "No OpenCode profiles configured"))
    (condition-case nil
        (when-let ((location
                    (opencode-shell--read-session-location
                     (opencode-shell--saved-session-location-entries profiles))))
          (opencode-shell--open-sessions (car location) (cdr location) t))
      (quit nil))))

(defun opencode-shell-find-session-with-server (&optional profile)
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
              (setq directory (opencode-shell--session-location-directory directory))
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
                     (when-let ((location
                                 (opencode-shell--read-session-location entries)))
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
  (let ((active-buffers
         (seq-filter
          (lambda (buffer)
            (buffer-local-value 'opencode-shell--runtime-key buffer))
          (buffer-list))))
    (opencode-shell-async-reset)
    (let* ((main (or load-file-name (locate-library "opencode-shell")))
           (directory (and main (file-name-directory main)))
           (names '("opencode-shell-render.el" "opencode-shell-async.el"
                    "opencode-shell-state.el" "opencode-shell-interaction.el"
                    "opencode-shell-response.el" "opencode-shell.el"))
           (sources (and directory
                         (mapcar (lambda (name) (expand-file-name name directory))
                                 names))))
      (unless (and sources (seq-every-p #'file-exists-p sources))
        (user-error "Cannot locate OpenCode Shell source files"))
      (dolist (source sources) (load source nil nil t))
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
            (opencode-shell--configure-evil-buffer)
            (opencode-shell--refresh-composer-overlay)
            (opencode-shell--sync-input-policy)))))
      (dolist (buffer active-buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq opencode-shell--runtime-key nil)
            (opencode-shell--start-polling))))
      (force-mode-line-update t)
      (message "Reloaded OpenCode Shell"))))

(opencode-shell--register-profile-commands)

(dolist (command opencode-shell--mode-commands)
  (put command 'completion-predicate #'ignore))

(with-eval-after-load 'evil
  (opencode-shell--setup-evil)
  ;; Remove relocation hooks left in buffers by older loaded versions.
  (remove-hook 'opencode-shell-mode-hook
               (intern "opencode-shell--enable-evil-composer-hook"))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-shell-mode)
        (remove-hook 'evil-insert-state-entry-hook
                     (intern "opencode-shell--evil-move-to-composer") t)))))

(provide 'opencode-shell)
;;; opencode-shell.el ends here
