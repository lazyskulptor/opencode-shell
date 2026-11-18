;;; opencode-shell.el --- Interactive client for OpenCode HTTP sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Assisted-by: Gemini:gemini-3-pro
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, processes
;; URL: https://github.com/lazyskulptor/opencode-shell
;; SPDX-License-Identifier: MIT

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
(require 'opencode-shell-messages)
(require 'opencode-shell-sessions)
(require 'opencode-shell-recovery)
(require 'opencode-shell-transport)

(defgroup opencode-shell nil "Unofficial Emacs client for OpenCode." :group 'tools)

(defcustom opencode-shell-base-url "http://127.0.0.1:4199"
  "OpenCode server base URL."
  :type 'string :group 'opencode-shell)


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

(defconst opencode-shell--snapshot-request-timeout 20
  "Maximum seconds a read-only snapshot may hold an in-flight guard.")

(defvar projectile-mode)
(declare-function projectile-project-root "ext:projectile")
(declare-function vertico--exhibit "ext:vertico" ())
(defvar opencode-shell--directory)
(defvar opencode-shell--session-id)

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
        (when-let* ((config (gethash server-key servers)))
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
        (not (member host '("" "localhost" "127.0.0.1" "::1" "[::1]"))))))

(defun opencode-shell--profile-sse-eligible-p (profile)
  "Return non-nil when PROFILE's /event SSE stream uses a local socket.
True when the base URL host is loopback, which holds for a directly local
server and for an SSH port-forwarded profile (base URL 127.0.0.1/localhost).
Such profiles take incremental SSE updates instead of re-parsing the whole
transcript on every poll, the dominant cost on a large session."
  (let* ((url (url-generic-parse-url
               (or (plist-get profile :base-url) opencode-shell-base-url)))
         (host (downcase (or (url-host url) ""))))
    (and (member host '("" "localhost" "127.0.0.1" "::1" "[::1]")) t)))

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
      (when-let* ((root (opencode-shell--canonical-directory
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
              (when-let* ((project (ignore-errors (project-current nil))))
                (ignore-errors (project-root project)))
              (locate-dominating-file default-directory ".git")
              default-directory)))
    (opencode-shell--canonical-directory root)))

(defun opencode-shell--current-server-directory (profile)
  "Return current `default-directory' as an absolute PROFILE server path."
  (let* ((client-root (plist-get profile :directory))
         (current (opencode-shell--canonical-directory
                   (opencode-shell--profile-client-directory default-directory profile)))
         (project (opencode-shell--canonical-directory
                   (opencode-shell--profile-client-directory
                    (opencode-shell--project-directory) profile)))
         (directory
          (if (and client-root (file-remote-p current)
                   (equal project (opencode-shell--canonical-directory client-root))
                   (not (equal current project))
                   (string-prefix-p project current))
              current
            project))
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
  "Start PROFILE for a one-shot command and call CALLBACK when ready."
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
(defvar-local opencode-shell--last-refresh-escalation 0
  "`float-time' of the last failed-refresh connection reload.")
(defvar-local opencode-shell--show-child-sessions nil)
(defvar-local opencode-shell--session-status nil)
(defvar-local opencode-shell--directory nil)
(defvar-local opencode-shell--session-id nil)
(defvar-local opencode-shell--session-title nil)
(defvar-local opencode-shell--subagent-session-p nil
  "Non-nil when the current session itself belongs to a parent session.
A subagent session is driven by its parent's `task' tool call rather than
direct user input, so its composer stays hidden and `opencode-shell--submit'
refuses to send a prompt into it.  Set when the owning `/session' snapshot
is known; nil (permit editing) until then.")
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
(defvar-local opencode-shell--last-event-time nil
  "Time of the last validated application event delivered to this transcript.")
(defvar-local opencode-shell--reconciled-session-updated nil
  "Last `/session/:id' `time.updated' marker confirmed by a full snapshot.
Set to the request's captured marker only after its authoritative snapshot has
been applied, never from a probe or a partial merge.")
(defvar-local opencode-shell--pending-session-updated nil
  "Marker whose change was deferred because SSE was delivering.
While SSE is live a marker advance is not dropped but held here; once SSE goes
quiet the next probe performs the authoritative resync to confirm it.")

(defconst opencode-shell--sse-delivery-window 5
  "Seconds within which a received SSE event counts as live incremental delivery.")
(defvar-local opencode-shell--pending-session-updated-since nil)
(defvar-local opencode-shell--last-message-recovery-attempt-at nil)
(defvar-local opencode-shell--last-authoritative-message-sync-at nil)
(defconst opencode-shell--message-recovery-delay 30)
(defconst opencode-shell--message-integrity-interval 120)
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
(defvar-local opencode-shell--rendered-anchor-signature nil)
(defvar-local opencode-shell--rendered-anchor-map nil)
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
(defvar-local opencode-shell--message-order-tail nil)
(defvar-local opencode-shell--parent-assistant-ids nil)
(defvar-local opencode-shell--orphan-message-parts nil)
(defvar-local opencode-shell--recent-turn-cache nil)
(defvar-local opencode-shell--transient-reconcile-pending nil)
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
(defvar-local opencode-shell--known-descendant-session-ids nil)
(defvar-local opencode-shell--probed-session-ids nil)
(defvar-local opencode-shell--task-parts-seen nil
  "Hash set of `task' tool part ids that already triggered a session lookup.
A quiet subagent (one that never raises a permission or question) would
otherwise only ever surface on a manual or reconnect-triggered full resync;
seeing its spawning `task' tool part for the first time triggers one instead,
without adding a `/session' call to the lean periodic poll cycle.")
(defvar-local opencode-shell--descendant-session-anchors nil
  "Alist of subagent session id to its discovery-time turn id.
Fallback anchor used when the spawning turn cannot be derived from tool-call
part data.")
(defvar opencode-shell--descendant-anchor-map nil
  "Alist id->turn-id bound during a full turn rebuild.")
(defvar-local opencode-shell--descendant-session-status nil
  "Alist of known subagent session id to its own running state.")
(defvar-local opencode-shell--session-idle-p nil
  "Non-nil when `/session/status' last reported this session as not busy.
Lets a turn left incomplete only by a detached `task' tool settle, matching the
web UI.  Nil (unknown) is conservative and keeps the original behavior.")
(defvar-local opencode-shell--last-lifecycle-signature nil)
(defvar-local opencode-shell--relocated-turn nil
  "Turn whose response block was suppressed for permission-box relocation.")
(defvar-local opencode-shell--table-render-width nil)
(defvar-local opencode-shell--table-rerendering nil)
(defvar-local opencode-shell--table-layout-timer nil)
(defvar opencode-shell--generation-counter 0)
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

(defun opencode-shell--ssh-disconnected (profile)
  "Pause SSH-backed PROFILE's HTTP polling after transport loss."
  (let ((key (opencode-shell--server-key profile)))
    (when (opencode-shell-recovery-mark-offline key)
      (opencode-shell-recovery-suspend key)
      (opencode-shell-async-pause-runtime key)
      (when-let* ((state (gethash key opencode-shell--servers)))
        (let ((process (plist-get state :process)))
          (setf (plist-get state :process) nil
                (plist-get state :owned) nil)
          (puthash key state opencode-shell--servers)
          (when (and (processp process) (process-live-p process))
            (delete-process process))))
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
      (message "OpenCode: SSH transport offline; retry with g r"))))

(defun opencode-shell--ssh-reconcile (key)
  "Full-resync KEY's subscribed transcripts after SSH recovery."
  (when-let* ((runtime (opencode-shell-async-runtime-get key)))
    (maphash
     (lambda (buffer _subscription)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (derived-mode-p 'opencode-shell-mode)
                      opencode-shell--connection-stale)
             (setq opencode-shell--connection-stale nil)
             (opencode-shell--resync t)))))
     (plist-get runtime :subscribers))))


(defun opencode-shell--status (id)
  "Return compact status for session ID."
  (let ((status (opencode-shell--get opencode-shell--session-status id)))
    (format "%s" (or (opencode-shell--get status 'type) status "idle"))))

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
  "Add relocated entries to SESSIONS, then call CALLBACK."
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

(defconst opencode-shell--refresh-escalation-cooldown 15
  "Minimum seconds between failed-refresh connection reloads.")

(defun opencode-shell--refresh ()
  "Refresh sessions and statuses, preserving point where possible.
On transport failure, escalate to re-establish the server or SSH tunnel and
retry, so a dropped port forward self-heals instead of silently leaving a
stale list."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (generation (setq opencode-shell--generation
                          (cl-incf opencode-shell--generation-counter))))
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
          '((limit . 1000))
          (lambda () (opencode-shell--refresh-failed generation)))))
     nil nil
     (lambda () (opencode-shell--refresh-failed generation)))))

(defun opencode-shell--refresh-failed (generation)
  "Re-establish the server or SSH tunnel after a failed GENERATION refresh.
Guarded by a cooldown so a persistent outage reloads the connection at most once
per cooldown window; `opencode-shell--start-server' coalesces concurrent starts
and the transport's own recovery backoff bounds the underlying retries.  The
cooldown is the sole guard, so a reload whose server start never completes does
not permanently block later re-escalation."
  (when (and (= generation opencode-shell--generation)
             (> (- (float-time) opencode-shell--last-refresh-escalation)
                opencode-shell--refresh-escalation-cooldown))
    (setq opencode-shell--last-refresh-escalation (float-time))
    (opencode-shell--note-request-error
      "OpenCode: session refresh failed; re-establishing connection")
    (let ((origin (current-buffer))
          (profile opencode-shell--profile))
      (cl-flet ((retry ()
                  (when (buffer-live-p origin)
                    (with-current-buffer origin
                      (opencode-shell--refresh)))))
        (if (plist-get profile :start-command)
            (opencode-shell--start-server-for-command
             profile (lambda (_ready) (retry)))
          (retry))))))

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
      (when id
        (let ((previous (point)))
          (goto-char (point-min))
          (while (and (not (eobp)) (not (equal id (tabulated-list-get-id))))
            (forward-line 1))
          (when (eobp) (goto-char previous)))))))

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
  (if-let* ((id (tabulated-list-get-id)))
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

(defvar-local opencode-shell--header-model-cache nil
  "Cached `(SELECTED-MODEL MODELS . LABEL)' tuple for header-line rendering.")

(defun opencode-shell--header-model-label ()
  "Return the display label for `opencode-shell--selected-model'."
  (if (and (consp opencode-shell--header-model-cache)
           (eq (car opencode-shell--header-model-cache)
               opencode-shell--selected-model)
           (eq (cadr opencode-shell--header-model-cache)
               opencode-shell--models))
      (cddr opencode-shell--header-model-cache)
    (let ((label (or (car (rassoc opencode-shell--selected-model
                                  opencode-shell--models))
                     "server default")))
      (setq opencode-shell--header-model-cache
            (cons opencode-shell--selected-model
                  (cons opencode-shell--models label)))
      label)))

(defun opencode-shell--header ()
  "Return live transcript agent, model, and session metadata."
  (let* ((left (format " agent:%s  model:%s"
                       (or opencode-shell--selected-agent "server default")
                       (opencode-shell--header-model-label)))
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

(defun opencode-shell--assert-session-identified ()
  "Signal a user error unless the current buffer owns an OpenCode session.
Unlike `opencode-shell--assert-session-idle', this does not require settled
history: forking is bounded by an explicit `messageID' boundary and only
copies history strictly before it, so a later turn still in progress does not
affect the fork's correctness."
  (unless (and (derived-mode-p 'opencode-shell-mode) opencode-shell--session-id)
    (user-error "No OpenCode session in this buffer")))

(defun opencode-shell--assert-session-idle ()
  "Signal a user error unless the current session has settled, stable history."
  (opencode-shell--assert-session-identified)
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
      (when-let* ((id (opencode-shell--turn-server-user-id turn)))
        (cl-incf index)
        (let ((text (truncate-string-to-width
                     (replace-regexp-in-string
                      "[\n\r\t ]+" " " (or (opencode-shell--turn-user turn) ""))
                     70 nil nil t))
              (suffix (if (eq (opencode-shell--turn-status turn) 'complete)
                          "" " (in progress)")))
          (push (cons (format "Before prompt %d: %s%s" index text suffix) id)
                candidates))))
    (nreverse candidates)))

;;;###autoload
(defun opencode-shell-fork-session ()
  "Fork this session before a minibuffer-selected prompt.
Available even while a later turn is still in progress; the fork only copies
history strictly before the chosen prompt."
  (interactive)
  (opencode-shell--assert-session-identified)
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

(defun opencode-shell--title-text-from-reply (parts)
  "Return the first non-empty text part's trimmed string from PARTS, or nil."
  (seq-some (lambda (part)
              (let ((model (opencode-shell-response-normalize-part part)))
                (when (eq (plist-get model :kind) 'text)
                  (let ((text (string-trim (or (plist-get model :text) ""))))
                    (and (not (string-empty-p text)) text)))))
            parts))

(defconst opencode-shell--rename-session-timeout 120
  "Seconds to wait for the title agent's reply in `opencode-shell-rename-session'.
Longer than `opencode-shell--url-retrieve-timeout' because this request
blocks on an LLM completion rather than an ordinary API round trip.")

;;;###autoload
(defun opencode-shell-rename-session ()
  "Ask OpenCode's native title agent to retitle this session from its context.
Sends a throwaway message to the hidden `title' agent, drops that exchange
from the transcript, applies the reply as the session title via a `PATCH',
and deletes the throwaway messages server-side."
  (interactive)
  (opencode-shell--assert-session-idle)
  (message "Generating OpenCode session title...")
  (let ((opencode-shell--url-retrieve-timeout opencode-shell--rename-session-timeout))
   (opencode-shell--request
    "POST" (format "/session/%s/message" opencode-shell--session-id)
   (lambda (reply)
     (let* ((info (opencode-shell--get reply 'info))
            (assistant-id (opencode-shell--get info 'id))
            (user-id (opencode-shell--get info 'parentID))
            (title (opencode-shell--title-text-from-reply
                    (opencode-shell--get reply 'parts))))
       ;; Remove the throwaway exchange from the transcript at once and
       ;; tombstone it so any in-flight SSE updates cannot re-add it; removing
       ;; the user message also drops its assistant child locally.
       (when user-id
         (opencode-shell--apply-message-event
          (list :kind 'message-removed :message-id user-id)))
       (dolist (id (delq nil (list assistant-id user-id)))
         (opencode-shell--request
          "DELETE" (format "/session/%s/message/%s"
                            opencode-shell--session-id id)
          #'ignore nil nil #'ignore))
       (if (not title)
           (message "OpenCode title agent returned no title")
         (opencode-shell--request
          "PATCH" (format "/session/%s" opencode-shell--session-id)
          (lambda (_session)
            (setq opencode-shell--session-title title)
            (opencode-shell--rename-transcript-for-title title)
            (force-mode-line-update)
            (message "Renamed OpenCode session to %S" title))
          `((title . ,title)) nil
          (lambda () (message "OpenCode session rename failed"))))))
   `((parts . [((type . "text")
                (text . "Based on this entire conversation so far, generate an updated title."))])
     (agent . "title"))
    nil
    (lambda () (message "OpenCode title generation failed")))))

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
  (when-let* ((value (opencode-shell--model-value model)))
    (when-let* ((entry (seq-find (lambda (item) (equal (cdr item) value))
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
          (when-let* ((agent (opencode-shell--get info 'agent)))
            (setq last-agent agent)
            (when-let* ((model (opencode-shell--model-value
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
    ("R" "Reconnect (force)" opencode-shell-restart)
    ("a" "Abort" opencode-shell--abort)]
   ["Options"
    ("m" "Model" opencode-shell--select-model)
    ("A" "Agent" opencode-shell--select-agent)
    ("n" "Next prompt" opencode-shell--next-turn)
    ("P" "Previous prompt" opencode-shell--previous-turn)
    ("p" "Permission" opencode-shell--permissions)
    ("q" "Question" opencode-shell--questions)
    ("o" "Open subagent session" opencode-shell--open-descendant-session)]
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
    (define-key map (kbd "C-c C-r") #'opencode-shell--resync)
    (define-key map (kbd "C-c C-k") #'opencode-shell-restart)
    (define-key map (kbd "C-c C-a") #'opencode-shell--abort)
    (define-key map (kbd "C-c C-p") #'opencode-shell--permissions)
    (define-key map (kbd "C-c C-y") #'opencode-shell--permission-allow-once)
    (define-key map (kbd "C-c C-l") #'opencode-shell--permission-allow-always)
    (define-key map (kbd "C-c C-n") #'opencode-shell--permission-reject)
    (define-key map (kbd "C-c C-q") #'opencode-shell--questions)
    (define-key map (kbd "C-c C-o") #'opencode-shell--open-descendant-session)
    (define-key map (kbd "C-c ?") #'opencode-shell-help)
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
      (when-let* ((marker (opencode-shell--turn-user-begin turn)))
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
  (if-let* ((target (opencode-shell--turn-navigation-target direction)))
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
  "Reject user edits from BEGIN to END outside the writable Composer."
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
  (let* ((prev-turn (car (last (butlast opencode-shell--turns))))
         (scan-start (if (and prev-turn
                              (markerp (opencode-shell--turn-response-end prev-turn))
                              (marker-position (opencode-shell--turn-response-end prev-turn)))
                         (max (point-min)
                              (marker-position (opencode-shell--turn-response-end prev-turn)))
                       (point-min)))
         (pos scan-start)
         (limit-marker (copy-marker (or limit (point-max)))))
    (unwind-protect
        (while (setq pos (text-property-any pos limit-marker
                                            'opencode-shell-composer-label t))
          (let ((end (next-single-property-change
                      pos 'opencode-shell-composer-label nil limit-marker)))
            (delete-region pos end)))
      (set-marker limit-marker nil))))

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
  (setq-local font-lock-multiline t)
  (setq-local header-line-format '(:eval (opencode-shell--header)))
  (setq-local mode-line-process '(:eval (opencode-shell--mode-line-status)))
  (setq-local opencode-shell--turn-gutters (make-hash-table :test #'eq))
  (setq-local opencode-shell--turns nil opencode-shell--turn-counter 0
              opencode-shell--rendered-turns nil
               opencode-shell--message-envelopes (make-hash-table :test #'equal)
               opencode-shell--message-order nil
               opencode-shell--message-order-tail nil
               opencode-shell--parent-assistant-ids (make-hash-table :test #'equal)
               opencode-shell--orphan-message-parts (make-hash-table :test #'equal)
               opencode-shell--recent-turn-cache nil
               opencode-shell--transient-reconcile-pending nil
               opencode-shell--removed-message-ids (make-hash-table :test #'equal)
              opencode-shell--normalized-changed-turns nil
              opencode-shell--message-state-revision 0
              opencode-shell--render-dirty-turns nil
              opencode-shell--relocated-turn nil
              opencode-shell--permissions nil
              opencode-shell--known-descendant-session-ids nil
              opencode-shell--probed-session-ids nil
              opencode-shell--task-parts-seen (make-hash-table :test #'equal)
              opencode-shell--descendant-session-anchors nil
              opencode-shell--descendant-session-status nil
              opencode-shell--session-idle-p nil
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
            #'opencode-shell--schedule-table-layout nil t)
  (add-hook 'window-configuration-change-hook
            #'opencode-shell--render-if-visible nil t)
  (add-hook 'kill-buffer-hook #'opencode-shell--cleanup nil t)
  (add-hook 'change-major-mode-hook #'opencode-shell--cleanup nil t))

(defun opencode-shell--cleanup ()
  "Cancel timers, close the session log, and invalidate callbacks."
  (opencode-shell-async-cancel)
  (when (timerp opencode-shell--table-layout-timer)
    (cancel-timer opencode-shell--table-layout-timer))
  (setq opencode-shell--table-layout-timer nil)
  (remove-hook 'window-configuration-change-hook
               #'opencode-shell--schedule-table-layout t)
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
    (when-let* ((log-buffer (get-buffer (opencode-shell--log-buffer-name))))
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

(defun opencode-shell--sse-recently-delivered-p ()
  "Return non-nil when a validated SSE event arrived within the delivery window."
  (and opencode-shell--last-event-time
       (<= (- (float-time) opencode-shell--last-event-time)
           opencode-shell--sse-delivery-window)))

(defun opencode-shell--confirm-session-updated (marker)
  "Confirm request-local MARKER after its authoritative snapshot applied.
Newer pending markers remain outstanding for a subsequent snapshot."
  (setq opencode-shell--last-authoritative-message-sync-at (float-time))
  (when marker
    (setq opencode-shell--reconciled-session-updated
          (max marker (or opencode-shell--reconciled-session-updated 0)))
    (when (and opencode-shell--pending-session-updated
               (<= opencode-shell--pending-session-updated
                   opencode-shell--reconciled-session-updated))
      (setq opencode-shell--pending-session-updated nil
            opencode-shell--pending-session-updated-since nil))))

(defun opencode-shell--message-integrity-due-p (now)
  "Whether an active or fallback transcript needs an integrity snapshot at NOW."
  (and opencode-shell--last-authoritative-message-sync-at
       (>= (- now opencode-shell--last-authoritative-message-sync-at)
           opencode-shell--message-integrity-interval)
       (or (seq-some (lambda (turn)
                       (not (eq (opencode-shell--turn-status turn) 'complete)))
                     opencode-shell--turns)
           (when-let* ((runtime (and opencode-shell--runtime-key
                                   (opencode-shell-async-runtime-get opencode-shell--runtime-key))))
             (and (not (plist-get runtime :paused))
                  (not (opencode-shell-async-sse-connected-p opencode-shell--runtime-key)))))))

(defun opencode-shell--reconcile-quiet ()
  "Reconcile cheap resources and fetch full history only when it changed.
Always reconciles cheap resources and probes `GET /session/:id' for its
`time.updated' marker (a few hundred bytes).  A full message resync runs only
when that marker advanced and SSE is not already delivering the change, so idle
or large sessions never re-download the transcript every tick, while changes SSE
missed are still recovered automatically without a manual resync."
  (opencode-shell--guarded-request
   'permissions "GET" "/permission"
   (lambda (items) (opencode-shell--receive-permissions items t))
   nil #'opencode-shell--consume-permission-refresh-pending)
  (opencode-shell--refresh-questions)
  (opencode-shell--refresh-descendant-status)
  (when opencode-shell--session-id
    (opencode-shell--guarded-request
     'session-updated "GET" (format "/session/%s" opencode-shell--session-id)
      (lambda (session)
        (let ((updated (opencode-shell--time session)))
          (when (> updated 0)
            (let ((now (float-time)))
             (cond
             ;; Unknown baseline or an advance: schedule recovery.  While SSE is
             ;; delivering we only DEFER; the marker is confirmed later by the
             ;; authoritative snapshot callback, never from this probe, so a
             ;; failed or skipped fetch cannot permanently consume the advance.
             ((or (null opencode-shell--reconciled-session-updated)
                  (> updated opencode-shell--reconciled-session-updated))
              (setq opencode-shell--pending-session-updated updated)
              (unless opencode-shell--pending-session-updated-since
                (setq opencode-shell--pending-session-updated-since now)))
             ;; The applied snapshot already covers the marker: clear any stale
             ;; deferral.
             (opencode-shell--pending-session-updated
              (setq opencode-shell--pending-session-updated nil
                    opencode-shell--pending-session-updated-since nil)))
             (when (and (not (alist-get 'messages opencode-shell--in-flight))
                        (or (null opencode-shell--last-message-recovery-attempt-at)
                            (>= (- now opencode-shell--last-message-recovery-attempt-at)
                                opencode-shell--message-recovery-delay))
                        (or (opencode-shell--message-integrity-due-p now)
                            (and opencode-shell--pending-session-updated
                                 (or (not (opencode-shell--sse-recently-delivered-p))
                                     (>= (- now opencode-shell--pending-session-updated-since)
                                         opencode-shell--message-recovery-delay)))))
               (opencode-shell--resync nil))))))
     nil nil)))

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
                ;; SSE delivers incremental deltas, avoiding a full-transcript
                ;; re-fetch/re-parse on every poll (the dominant cost on large
                ;; sessions).  Enabled whenever the /event socket is local
                ;; (direct or SSH port-forwarded); the poll remains the
                ;; authoritative fallback when SSE drops.
                (opencode-shell--profile-sse-eligible-p profile)
                opencode-shell-poll-interval
                (lambda ()
                  (if (opencode-shell--initial-hydration-complete-p)
                      (opencode-shell--reconcile-quiet)
                    (opencode-shell--resync nil)))
                (lambda (event)
                  (when opencode-shell-log-requests
                    (opencode-shell--log "OpenCode async %s" event)))
                opencode-shell--session-id
                 #'opencode-shell--receive-application-event)))
        (ignore runtime)
         (when (opencode-shell-recovery-offline-p key)
           (setq-local opencode-shell--connection-stale t)
           (opencode-shell-async-pause-runtime key))
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
  (let* ((profile (or (opencode-shell--resolve-profile profile)
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

(defun opencode-shell--message-error-label (info)
  "Return a bounded single-line label for INFO's terminal error, or nil."
  (when-let* ((err (opencode-shell--get info 'error)))
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

(defun opencode-shell--turn-by-id (id turns)
  "Find the turn in TURNS whose stable client or server ID equals ID."
  (let ((cached (and (consp opencode-shell--recent-turn-cache)
                     (eq (car opencode-shell--recent-turn-cache) turns)
                     (cdr opencode-shell--recent-turn-cache))))
    (if (and cached
             (or (equal id (opencode-shell--turn-id cached))
                 (equal id (opencode-shell--turn-server-user-id cached))))
        cached
      (let ((found (seq-find (lambda (turn)
                               (or (equal id (opencode-shell--turn-id turn))
                                   (equal id (opencode-shell--turn-server-user-id turn))))
                             turns)))
        (when found
          (setq opencode-shell--recent-turn-cache (cons turns found)))
        found))))

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

(defun opencode-shell--note-task-parts (parts)
  "Trigger one session-list lookup per newly observed `task' tool part in PARTS.
A quiet subagent only ever surfaces on a full resync; this makes seeing its
spawning tool call for the first time trigger one, without adding a
`/session' call to the lean periodic poll cycle."
  (dolist (part parts)
    (when-let* ((id (opencode-shell--part-id part)))
      (unless (gethash id opencode-shell--task-parts-seen)
        (let ((type (opencode-shell--get part 'type)))
          (when (or (equal type "tool")
                    (equal type "tool-invocation")
                    (opencode-shell--get part 'tool))
            (let ((model (opencode-shell-response-normalize-part part)))
              (when (and (eq (plist-get model :kind) 'tool)
                         (equal (plist-get model :name) "task"))
                (puthash id t opencode-shell--task-parts-seen)
                (opencode-shell--refresh-session-metadata)))))))))

(defun opencode-shell--aggregate-turn (turn)
  "Recompute TURN presentation state once from its merged assistant messages."
  (let* ((messages (opencode-shell--turn-assistant-messages turn))
         (parts (apply #'append
                       (mapcar (lambda (item)
                                 (opencode-shell--get (cdr item) 'parts))
                               messages)))
         (last-envelope (cdr (car (last messages))))
         (complete
          (and last-envelope
               (or (and (opencode-shell-response-envelope-complete-p
                         (opencode-shell--get last-envelope 'info)
                         (opencode-shell--get last-envelope 'parts))
                        (not (seq-some #'opencode-shell-response-running-tool-p
                                       parts)))
                   ;; opencode leaves the parent's `task' tool `running' after a
                   ;; subagent detaches and omits completion metadata; trust
                   ;; `/session/status' idle to complete the turn, as the web UI
                   ;; does, when nothing but task tools remain unsettled.
                   (and opencode-shell--session-idle-p
                        (opencode-shell-response-blocked-only-by-task-p parts))))))
    (opencode-shell--note-task-parts parts)
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

(defun opencode-shell--normalize-turns (messages &optional authoritative)
  "Reconcile server MESSAGES into stable buffer-local turn records.
AUTHORITATIVE replaces assistant state and restores observed user order; partial
events merge instead.  Unacknowledged local prompts retain their draft text."
  (let ((old opencode-shell--turns) observed current changed
        (touched (make-hash-table :test #'eq))
        (turn-index (make-hash-table :test #'equal))
        (assistant-indexes (make-hash-table :test #'eq))
        (prior-signatures (and authoritative (make-hash-table :test #'eq)))
        (local-turns-by-text (make-hash-table :test #'equal))
        (old-set (make-hash-table :test #'eq)))
    (when authoritative
      (dolist (turn old)
        (puthash turn (opencode-shell--turn-state-signature turn) prior-signatures)
        (when (opencode-shell--turn-assistant-messages turn)
          (setf (opencode-shell--turn-assistant-messages turn) nil)
          (puthash turn t touched))))
    (dolist (turn old)
      (puthash turn t old-set)
      (when-let* ((id (opencode-shell--turn-id turn)))
        (puthash id turn turn-index))
      (when-let* ((id (opencode-shell--turn-server-user-id turn)))
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
           (let* ((raw-text (opencode-shell--message-text envelope))
                  (has-parts (not (null (opencode-shell--get envelope 'parts))))
                  (local-candidates (gethash raw-text local-turns-by-text))
                  (turn (or (and id (gethash id turn-index))
                            (car local-candidates)
                            (opencode-shell--make-turn
                             :id (or id (format "turn-%d" (cl-incf opencode-shell--turn-counter))))))
                  (text (if (and (or (not authoritative)
                                    (null (opencode-shell--turn-server-user-id turn)))
                                 (not has-parts)
                                 (string-empty-p raw-text)
                                 (not (string-empty-p (or (opencode-shell--turn-user turn) ""))))
                            (opencode-shell--turn-user turn)
                          raw-text)))
            (when (and local-candidates (eq turn (car local-candidates)))
              (puthash text (cdr local-candidates) local-turns-by-text))
            (let ((next-status (if (opencode-shell--turn-assistant-messages turn)
                                   (opencode-shell--turn-status turn)
                                 (if (opencode-shell--turn-locally-settled turn)
                                     'complete 'waiting))))
              (unless (and (equal (opencode-shell--turn-server-user-id turn) id)
                           (eq (opencode-shell--turn-acknowledged turn) t)
                           (equal (opencode-shell--turn-user turn) text)
                           (eq (opencode-shell--turn-status turn) next-status))
                (setf (opencode-shell--turn-server-user-id turn) id
                      (opencode-shell--turn-acknowledged turn) t
                      (opencode-shell--turn-user turn) text
                      (opencode-shell--turn-status turn) next-status)
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
                      (entry (and id (gethash id index))))
                (unless (and entry (eq (cdr entry) envelope))
                  (let ((merged (if entry
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
                      (puthash turn t touched)))))))))))
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
      (let ((result (if authoritative
                        (append observed
                                (seq-filter (lambda (turn)
                                              (and (null (opencode-shell--turn-server-user-id turn))
                                                   (not (memq turn observed)))) old))
                      (append (copy-sequence old) (nreverse new-turns)))))
        (dolist (turn (butlast result))
          (unless (eq (opencode-shell--turn-status turn) 'complete)
            (cl-pushnew turn changed :test #'eq)))
        (opencode-shell--settle-superseded-turns result)
        (when authoritative
          (setq changed (seq-filter
                         (lambda (turn)
                           (not (equal (gethash turn prior-signatures)
                                       (opencode-shell--turn-state-signature turn))))
                         changed)))
        (setq opencode-shell--normalized-changed-turns (nreverse changed))
        result))))

(defun opencode-shell--compact-part (part)
  "Drop unrendered bulk payloads from PART while preserving metadata."
  (opencode-shell-response-compact-part part))

(defun opencode-shell--compact-envelope (envelope)
  "Return ENVELOPE with unrendered bulk payloads compacted in its parts."
  (opencode-shell-response-compact-envelope envelope))

(defun opencode-shell--ensure-message-cache ()
  "Ensure the current transcript owns an initialized message cache."
  (unless (hash-table-p opencode-shell--message-envelopes)
    (setq opencode-shell--message-envelopes (make-hash-table :test #'equal)
          opencode-shell--message-order nil
          opencode-shell--message-order-tail nil
          opencode-shell--parent-assistant-ids (make-hash-table :test #'equal)))
  (unless (hash-table-p opencode-shell--parent-assistant-ids)
    (setq opencode-shell--parent-assistant-ids (make-hash-table :test #'equal)))
  (unless (hash-table-p opencode-shell--orphan-message-parts)
    (setq opencode-shell--orphan-message-parts (make-hash-table :test #'equal)))
  (unless (hash-table-p opencode-shell--removed-message-ids)
    (setq opencode-shell--removed-message-ids (make-hash-table :test #'equal))))

(defun opencode-shell--cache-message-envelope (envelope &optional authoritative)
  "Store ENVELOPE in the buffer-local message index and return the result.
AUTHORITATIVE replaces the envelope; otherwise merge a partial event."
  (opencode-shell--ensure-message-cache)
  (setq envelope (opencode-shell--compact-envelope envelope))
  (let* ((id (opencode-shell--message-envelope-id envelope))
         (known (and id (gethash id opencode-shell--message-envelopes)))
         (merged (if (and known (not authoritative))
                     (if (eq known envelope)
                         known
                       (let ((candidate (opencode-shell--merge-envelope known envelope)))
                         (if (equal known candidate) known candidate)))
                   envelope)))
    (when id
      (unless known
        (let ((cell (list id)))
          (if (and opencode-shell--message-order
                   (consp opencode-shell--message-order-tail)
                   (null (cdr opencode-shell--message-order-tail)))
              (setcdr opencode-shell--message-order-tail cell)
            (setq opencode-shell--message-order
                  (nconc opencode-shell--message-order cell)))
          (setq opencode-shell--message-order-tail cell)))
      (unless (eq known merged)
        (puthash id merged opencode-shell--message-envelopes))
      (let* ((info (opencode-shell--get merged 'info))
             (role (format "%s" (or (opencode-shell--get info 'role) "")))
             (parent (or (opencode-shell--get info 'parentID)
                         (opencode-shell--get info 'parentId))))
        (when (and (equal role "assistant") parent)
          (let ((existing-ids (gethash parent opencode-shell--parent-assistant-ids)))
            (unless (member id existing-ids)
              (puthash parent (append existing-ids (list id))
                       opencode-shell--parent-assistant-ids))))))
    merged))

(defun opencode-shell--cache-message-snapshot (messages &optional authoritative)
  "Merge chronological MESSAGES into the message index.
When AUTHORITATIVE is non-nil, remove cached server state absent from MESSAGES.
Return a plist containing affected turns and whether a full render is required."
  (opencode-shell--ensure-message-cache)
  (let ((ids (delq nil (mapcar #'opencode-shell--message-envelope-id messages)))
        changed force)
    (when authoritative
      (dolist (id ids) (remhash id opencode-shell--removed-message-ids))
      (clrhash opencode-shell--parent-assistant-ids)
      (clrhash opencode-shell--orphan-message-parts)
      (let ((present (make-hash-table :test #'equal)))
        (dolist (id ids) (puthash id t present))
        (let (removed)
          (maphash (lambda (id _)
                     (unless (gethash id present) (push id removed)))
                   opencode-shell--message-envelopes)
          (dolist (id removed)
            (remhash id opencode-shell--message-envelopes)))
        (setq opencode-shell--message-order (copy-sequence ids)
              opencode-shell--message-order-tail (last opencode-shell--message-order))
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
    (let ((cached (mapcar (lambda (message)
                           (opencode-shell--cache-message-envelope message authoritative)) messages)))
      (list :changed-turns changed :force force :messages cached))))

(defun opencode-shell--cached-assistants-for-parent (parent-id)
  "Return cached assistant envelopes belonging to PARENT-ID in order."
  (if (and (hash-table-p opencode-shell--parent-assistant-ids)
           (gethash parent-id opencode-shell--parent-assistant-ids))
      (delq nil
            (mapcar (lambda (id)
                      (gethash id opencode-shell--message-envelopes))
                    (gethash parent-id opencode-shell--parent-assistant-ids)))
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
           opencode-shell--message-order))))

(defun opencode-shell--rebuild-turn-from-message-cache (parent-id)
  "Rebuild and return PARENT-ID's turn from cached assistant envelopes."
  (when-let* ((turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)))
    (let ((assistants (opencode-shell--cached-assistants-for-parent parent-id)))
      (setf (opencode-shell--turn-assistant-messages turn)
            (mapcar (lambda (envelope)
                      (cons (opencode-shell--message-envelope-id envelope)
                            envelope))
                    assistants)
            (opencode-shell--turn-parts turn) nil
            (opencode-shell--turn-assistant turn) "")
      (when assistants
        (opencode-shell--aggregate-turn turn))
      (opencode-shell--settle-superseded-turns opencode-shell--turns)
      turn)))

(defvar-local opencode-shell--normalized-parts-cache nil
  "Weak `eq' table: TURN -> (PARTS . normalized-parts), keyed by parts identity.")

(defun opencode-shell--normalized-parts (turn)
  "Return TURN's parts normalized, caching the projection by parts identity.
`opencode-shell--aggregate-turn' replaces `opencode-shell--turn-parts' with a
fresh list on each recompute, so an unchanged turn reuses its cached projection
instead of re-normalizing every part on each signature/display-cache probe."
  (unless (hash-table-p opencode-shell--normalized-parts-cache)
    (setq opencode-shell--normalized-parts-cache
          (make-hash-table :test 'eq :weakness 'key)))
  (let* ((parts (opencode-shell--turn-parts turn))
         (cached (gethash turn opencode-shell--normalized-parts-cache)))
    (if (and cached (eq (car cached) parts))
        (cdr cached)
      (let ((normalized (mapcar #'opencode-shell-response-normalize-part parts)))
        (puthash turn (cons parts normalized) opencode-shell--normalized-parts-cache)
        normalized))))

(defun opencode-shell--turn-state-signature (turn)
  "Return TURN state that can affect transcript presentation."
  (and turn
       (list (opencode-shell--turn-id turn)
             (opencode-shell--turn-server-user-id turn)
             (opencode-shell--turn-user turn)
             (opencode-shell--turn-assistant turn)
             (opencode-shell--normalized-parts turn)
             (opencode-shell--turn-status turn)
             (opencode-shell--turn-acknowledged turn)
             (opencode-shell--turn-locally-settled turn)
             (opencode-shell--turn-terminal-error turn))))

(defun opencode-shell--schedule-event-reconciliation (&optional resource quiet)
  "Coalesce an authoritative snapshot reconciliation for RESOURCE.
QUIET reconciles cheap resources plus a change-gated message re-fetch (see
`opencode-shell--reconcile-quiet'); otherwise RESOURCE is fully re-fetched."
  (let ((resource (or resource 'all)))
    (opencode-shell-async-enqueue
     (current-buffer) (list 'event-reconcile resource) opencode-shell--generation
     (if quiet
         #'opencode-shell--reconcile-quiet
       (lambda () (opencode-shell--resync nil resource))))))

(defun opencode-shell--apply-message-event (event)
  "Apply validated message EVENT locally, returning non-nil on success."
  (let* ((kind (plist-get event :kind))
         (message-id (plist-get event :message-id))
         (known (and message-id
                     (gethash message-id opencode-shell--message-envelopes)))
         parent-id turn before force)
    (cl-labels
        ((sync-envelope (candidate &optional skip-unchanged)
           (let* ((info (opencode-shell--get candidate 'info))
                  (role (format "%s" (opencode-shell--get info 'role)))
                  (parent (or (opencode-shell--get info 'parentID)
                              (opencode-shell--get info 'parentId))))
             (if (equal role "user")
                 (let ((envelope (opencode-shell--cache-message-envelope candidate)))
                   (setq turn (opencode-shell--turn-by-id message-id opencode-shell--turns)
                         before (opencode-shell--turn-state-signature turn)
                         opencode-shell--turns
                         (opencode-shell--normalize-turns (list envelope))
                         turn (opencode-shell--turn-by-id message-id opencode-shell--turns))
                   (when (and turn (opencode-shell--cached-assistants-for-parent message-id))
                     (opencode-shell--rebuild-turn-from-message-cache message-id)))
               (setq parent-id parent)
               (when parent-id
                 (setq turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                       before (opencode-shell--turn-state-signature turn))
                 (let ((merged (opencode-shell--cache-message-envelope candidate)))
                   (unless (and skip-unchanged (eq known merged))
                     (opencode-shell--rebuild-turn-from-message-cache parent-id)))))))
         (delta-part (parts part-id field delta)
           (let* ((existing (seq-find (lambda (p) (equal (opencode-shell--part-id p) part-id))
                                      parts))
                  (field-sym (intern field))
                  (old-val (and existing (opencode-shell--get existing field-sym))))
             (if existing
                 (if (equal (format "%s" (or (opencode-shell--get existing 'type) ""))
                            "reasoning")
                     existing
                   (opencode-shell--merge-alist
                    existing `((,field-sym . ,(concat (if (stringp old-val) old-val "") delta)))))
               (and (equal field "text")
                    `((id . ,part-id) (messageID . ,message-id)
                      (type . "text") (text . ,delta)))))))
      (pcase kind
        ('message-updated
         (let* ((info (plist-get event :info))
                (parent (or (opencode-shell--get info 'parentID)
                            (opencode-shell--get info 'parentId)))
                (orphan-parts (and message-id
                                   (gethash message-id opencode-shell--orphan-message-parts))))
           (when message-id
             (remhash message-id opencode-shell--orphan-message-parts))
           (unless (and parent (gethash parent opencode-shell--removed-message-ids))
             (remhash message-id opencode-shell--removed-message-ids)
             (sync-envelope `((info . ,info)
                              (parts . ,(opencode-shell--merge-parts
                                         (opencode-shell--get known 'parts)
                                         orphan-parts)))))))
        ('part-updated
         (if known
             (sync-envelope `((info . ,(opencode-shell--get known 'info))
                              (parts . (,(plist-get event :part))))
                            t)
           (when (and message-id
                      (plist-get event :part)
                      (not (gethash message-id opencode-shell--removed-message-ids)))
             (puthash message-id
                      (opencode-shell--merge-parts
                       (gethash message-id opencode-shell--orphan-message-parts)
                       (list (opencode-shell--compact-part (plist-get event :part))))
                      opencode-shell--orphan-message-parts)
             (setq opencode-shell--transient-reconcile-pending t))))
        ('part-delta
         (let ((part-id (plist-get event :part-id))
               (field (plist-get event :field))
               (delta (plist-get event :delta)))
           (if (and known part-id (stringp field) (not (string-empty-p field))
                    (stringp delta))
               (when-let* ((updated (delta-part (opencode-shell--get known 'parts)
                                               part-id field delta)))
                 (sync-envelope `((info . ,(opencode-shell--get known 'info))
                                  (parts . (,updated)))
                                t))
             (when (and message-id part-id (equal field "text") (stringp delta)
                        (not (gethash message-id opencode-shell--removed-message-ids)))
               (let* ((orphans (gethash message-id opencode-shell--orphan-message-parts))
                      (updated (delta-part orphans part-id field delta)))
                 (puthash message-id
                          (opencode-shell--merge-parts orphans (list updated))
                          opencode-shell--orphan-message-parts)
                 (setq opencode-shell--transient-reconcile-pending t))))))
         ('part-removed
          (let ((orphans (gethash message-id opencode-shell--orphan-message-parts)))
            (when orphans
              (let ((kept (seq-remove (lambda (part)
                                        (equal (opencode-shell--part-id part)
                                               (plist-get event :part-id))) orphans)))
                (if kept (puthash message-id kept opencode-shell--orphan-message-parts)
                  (remhash message-id opencode-shell--orphan-message-parts)))))
         (when known
           (let* ((info (opencode-shell--get known 'info))
                  (role (format "%s" (opencode-shell--get info 'role))))
             (setf (alist-get 'parts known)
                   (seq-remove
                    (lambda (part)
                      (equal (opencode-shell--get part 'id)
                             (plist-get event :part-id)))
                    (opencode-shell--get known 'parts)))
             (puthash message-id known opencode-shell--message-envelopes)
             (if (equal role "user")
                 (setq turn (opencode-shell--turn-by-id message-id opencode-shell--turns)
                       before (opencode-shell--turn-state-signature turn)
                       opencode-shell--turns (opencode-shell--normalize-turns (list known))
                       turn (opencode-shell--turn-by-id message-id opencode-shell--turns))
               (setq parent-id (or (opencode-shell--get info 'parentID)
                                   (opencode-shell--get info 'parentId))
                     turn (opencode-shell--turn-by-id parent-id opencode-shell--turns)
                     before (opencode-shell--turn-state-signature turn))
               (opencode-shell--rebuild-turn-from-message-cache parent-id)))))
      ('message-removed
       (let* ((info (opencode-shell--get known 'info))
              (role (and known (format "%s" (opencode-shell--get info 'role))))
              (user-turn (opencode-shell--turn-by-id message-id opencode-shell--turns))
              (children (make-hash-table :test #'equal)))
         (puthash message-id t opencode-shell--removed-message-ids)
         (remhash message-id opencode-shell--orphan-message-parts)
         (remhash message-id opencode-shell--message-envelopes)
         (setq opencode-shell--message-order
               (delete message-id opencode-shell--message-order)
               opencode-shell--message-order-tail
               (last opencode-shell--message-order))
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
               (remhash message-id opencode-shell--parent-assistant-ids)
               (maphash
                (lambda (id _)
                  (remhash id opencode-shell--message-envelopes))
                children)
               (setq opencode-shell--message-order
                     (seq-remove (lambda (id) (gethash id children))
                                 opencode-shell--message-order)
                     opencode-shell--message-order-tail
                     (last opencode-shell--message-order)
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
             (when parent-id
               (puthash parent-id
                        (delete message-id
                                (gethash parent-id opencode-shell--parent-assistant-ids))
                        opencode-shell--parent-assistant-ids))
             (opencode-shell--rebuild-turn-from-message-cache parent-id)))))))
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
  (when (and (derived-mode-p 'opencode-shell-mode)
             (not (eq (plist-get event :resource) 'none)))
    (setq opencode-shell--last-event-time (float-time))
    (opencode-shell--ensure-message-cache)
    (let ((kind (plist-get event :kind))
          (type (plist-get event :type)))
      (when (memq kind
                  '(message-updated message-removed part-updated part-delta part-removed))
        (setq opencode-shell--session-idle-p nil)
        (opencode-shell--reset-spinner-frame))
      (cond
       ((and (memq kind
                   '(message-updated message-removed part-updated part-delta part-removed))
             (opencode-shell--apply-message-event event))
        (cl-incf opencode-shell--message-state-revision)
        (when (and opencode-shell--transient-reconcile-pending
                   (zerop (hash-table-count opencode-shell--orphan-message-parts)))
          (setq opencode-shell--transient-reconcile-pending nil
                opencode-shell-async--queue
                (assoc-delete-all '(event-reconcile messages)
                                  opencode-shell-async--queue))))
       ((equal type "session.updated")
        (when-let* ((title (opencode-shell--get (plist-get event :info) 'title)))
          (when (and (stringp title) (not (string-empty-p title))
                     (not (equal title opencode-shell--session-title)))
            (setq opencode-shell--session-title title)
            (opencode-shell--rename-transcript-for-title title)
            (force-mode-line-update))))
       ((member type '("session.idle" "session.status"))
         (setq opencode-shell--session-idle-p
               (or (equal type "session.idle")
                   (equal (plist-get event :status) "idle")))
        (let (changed)
          (dolist (turn opencode-shell--turns)
            (unless (eq (opencode-shell--turn-status turn) 'complete)
              (let ((before (opencode-shell--turn-state-signature turn)))
                (opencode-shell--aggregate-turn turn)
                (unless (equal before (opencode-shell--turn-state-signature turn))
                  (push turn changed)))))
          (opencode-shell--update-message-lifecycle-state)
          (when changed
            (opencode-shell--schedule-render
             (format "event:%s" type) nil (nreverse changed)))
          (when opencode-shell--known-descendant-session-ids
            (opencode-shell--refresh-descendant-status))
          (when (or (null opencode-shell--turns)
                    opencode-shell--submit-in-flight
            (seq-some (lambda (turn)
                        (not (eq (opencode-shell--turn-status turn) 'complete)))
                      opencode-shell--turns))
            (opencode-shell--schedule-event-reconciliation 'messages t))))
       (t
        (opencode-shell--schedule-event-reconciliation
         (or (plist-get event :resource) 'messages)))))))

(defun opencode-shell--composer-text ()
  "Return composer contents without properties or its sentinel."
  (let ((end (if (get-text-property (1- (point-max))
                                    'opencode-shell-composer-sentinel)
                 (1- (point-max))
               (point-max))))
    (buffer-substring-no-properties opencode-shell--composer-start end)))

(defun opencode-shell--composer-visible-p ()
  "Return non-nil when authoritative lifecycle state permits editing."
  (and (not opencode-shell--subagent-session-p)
       (opencode-shell-state-composer-ready-p
        (mapcar #'opencode-shell--turn-status opencode-shell--turns)
        opencode-shell--submit-in-flight
        (opencode-shell--human-interaction-blocked-p)
        (opencode-shell--initial-hydration-complete-p))))

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
  (opencode-shell--render-turns nil 'none)
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

(defun opencode-shell--interaction-session-id (item)
  "Return ITEM's session ID under either casing."
  (or (opencode-shell--get item 'sessionID)
      (opencode-shell--get item 'sessionId)))

(defun opencode-shell--interaction-session-visible-p (session)
  "Return non-nil when SESSION is the current session or a known descendant."
  (or (equal session opencode-shell--session-id)
      (member session opencode-shell--known-descendant-session-ids)))

(defun opencode-shell--session-parent-map (sessions)
  "Return a hash table mapping each session id in SESSIONS to its parent id.
Roots map to nil; ids absent from SESSIONS are simply not present."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (session sessions map)
      (when-let* ((id (opencode-shell--get session 'id)))
        (let ((parent (or (opencode-shell--get session 'parentID)
                          (opencode-shell--get session 'parentId))))
          (puthash id (and parent (not (equal parent "")) parent) map))))))

(defun opencode-shell--session-lineage (session parent-map target)
  "Classify SESSION against TARGET by walking PARENT-MAP toward the root.
Return `descendant' when TARGET is SESSION or one of its ancestors, `foreign'
when the chain ends at a root other than TARGET, or `unknown' when an ancestor
is missing from PARENT-MAP (not yet indexed) so classification must wait."
  (let ((current session)
        (seen (make-hash-table :test #'equal)))
    (catch 'done
      (while t
        (when (equal current target) (throw 'done 'descendant))
        (when (gethash current seen) (throw 'done 'foreign))
        (puthash current t seen)
        (let ((parent (gethash current parent-map 'absent)))
          (cond
           ((eq parent 'absent) (throw 'done 'unknown))
           ((null parent) (throw 'done 'foreign))
           (t (setq current parent))))))))

(defun opencode-shell--unresolved-interaction-sessions (items)
  "Return ITEMS' distinct sessions not yet known to belong here or ruled out."
  (seq-uniq
   (seq-remove
    (lambda (session)
      (or (null session)
          (opencode-shell--interaction-session-visible-p session)
          (member session opencode-shell--probed-session-ids)))
    (mapcar #'opencode-shell--interaction-session-id items))))

(defun opencode-shell--current-turn-anchor ()
  "Return the current last turn's id, or nil when there are no turns."
  (when-let* ((turn (car (last opencode-shell--turns))))
    (opencode-shell--turn-id turn)))

(defun opencode-shell--anchor-descendant-sessions (ids)
  "Record the current turn as the fallback discovery anchor for each of IDS.
Sessions already anchored keep their first-seen turn."
  (let ((anchor (opencode-shell--current-turn-anchor)))
    (dolist (id ids)
      (unless (assoc id opencode-shell--descendant-session-anchors)
        (push (cons id anchor) opencode-shell--descendant-session-anchors)))))

(defun opencode-shell--string-leaves (value)
  "Return every string leaf in VALUE, a parsed JSON alist, list, or vector."
  (cond
   ((stringp value) (list value))
   ((consp value)
    (nconc (opencode-shell--string-leaves (car value))
           (opencode-shell--string-leaves (cdr value))))
   ((vectorp value)
    (mapcan #'opencode-shell--string-leaves (append value nil)))))

(defvar-local opencode-shell--spawn-haystacks nil
  "Weak `eq' cache of TURN to (PARTS . TEXT) used for subagent anchoring.")

(defun opencode-shell--turn-spawn-haystack (turn)
  "Return TURN's searchable task-tool text, cached until its parts change.
The spawned child id lives in the small `metadata' sub-object (e.g.
`sessionId') or the `<task id=...>' header at the start of `output'.  Only
those bounded fields are collected, once per parts revision, so anchoring
every subagent against every turn is a cheap `string-search' instead of a
per-(id, turn) walk and regexp over the part payloads."
  (unless (hash-table-p opencode-shell--spawn-haystacks)
    (setq opencode-shell--spawn-haystacks
          (make-hash-table :test #'eq :weakness 'key)))
  (let ((parts (opencode-shell--turn-parts turn))
        (cached (gethash turn opencode-shell--spawn-haystacks)))
    (if (and cached (eq (car cached) parts))
        (cdr cached)
      (let (chunks)
        (dolist (part parts)
          (let ((model (opencode-shell-response-normalize-part part)))
            (when (and (eq (plist-get model :kind) 'tool)
                       (member (plist-get model :name) '("task" nil)))
              (when-let* ((state (opencode-shell-response--get part 'state)))
                (let ((output (opencode-shell-response--get state 'output)))
                  (setq chunks
                        (nconc chunks
                               (opencode-shell--string-leaves
                                (opencode-shell-response--get state 'metadata))
                               (and (stringp output)
                                    (list (substring output 0
                                                     (min 500 (length output))))))))))))
        (let ((text (mapconcat #'identity chunks "\n")))
          (puthash turn (cons parts text) opencode-shell--spawn-haystacks)
          text)))))

(defun opencode-shell--turn-spawned-session-p (turn session-id)
  "Return non-nil when TURN holds a task tool call referencing SESSION-ID."
  (and (string-search session-id (opencode-shell--turn-spawn-haystack turn)) t))

(defun opencode-shell--compute-descendant-anchor-map ()
  "Return an alist of known subagent id to the turn id it renders under.
Prefer the turn whose tool call spawned it; fall back to its discovery anchor."
  (mapcar
   (lambda (id)
     (cons id
           (or (seq-some (lambda (turn)
                           (and (opencode-shell--turn-spawned-session-p turn id)
                                (opencode-shell--turn-id turn)))
                         opencode-shell--turns)
               (alist-get id opencode-shell--descendant-session-anchors
                          nil nil #'equal))))
   opencode-shell--known-descendant-session-ids))

(defun opencode-shell--discover-descendant-sessions (items)
  "Resolve ITEMS' unrecognized sessions against this session's subagent subtree.
Bubbles up permissions and questions raised by any descendant session (a direct
subagent or a subagent of a subagent, found by walking the `parentID' chain up
to the current session) so they surface and can be answered here instead of
silently stalling the subagent forever.

A session whose chain ends at a root other than the current session is ruled
out permanently; one whose chain reaches an ancestor still absent from the
`/session' snapshot is left unresolved so a subagent not yet indexed is
re-probed on a later poll rather than dropped forever.  Overrides the default
row truncation so a descendant beyond the first page is still found."
  (when-let* ((unresolved (opencode-shell--unresolved-interaction-sessions items)))
    (opencode-shell--guarded-request
     'descendant-sessions "GET" "/session"
     (lambda (sessions)
       (let ((parent-map (opencode-shell--session-parent-map sessions))
             new foreign)
         (dolist (id unresolved)
           (pcase (opencode-shell--session-lineage
                   id parent-map opencode-shell--session-id)
             ('descendant (push id new))
             ('foreign (push id foreign))))
         (when foreign
           (setq opencode-shell--probed-session-ids
                 (seq-uniq (append opencode-shell--probed-session-ids foreign))))
         (when new
           (setq opencode-shell--known-descendant-session-ids
                 (seq-uniq (append opencode-shell--known-descendant-session-ids
                                   new)))
           (opencode-shell--anchor-descendant-sessions new)
           (opencode-shell--refresh-permissions)
           (opencode-shell--refresh-questions)
           ;; Full rebuild: the line anchors inline to an already-rendered turn.
           (opencode-shell--schedule-render "descendant-sessions" t))))
     nil nil '((limit . 1000)))))

(defun opencode-shell--sync-descendant-sessions (sessions)
  "Reconcile the known subagent set against the full SESSIONS snapshot.
Considers every session (not only ones with a pending interaction), so a quiet
subagent still appears; replaces the known set with the current descendants so
finished or deleted ones drop.  Reuses an already-fetched SESSIONS list to avoid
its own request."
  (let* ((parent-map (opencode-shell--session-parent-map sessions))
         (descendants
          (sort
           (seq-uniq
            (seq-filter
             (lambda (id)
               (and id
                    (not (equal id opencode-shell--session-id))
                    (eq (opencode-shell--session-lineage
                         id parent-map opencode-shell--session-id)
                        'descendant)))
             (mapcar (lambda (session) (opencode-shell--get session 'id))
                     sessions)))
           #'string<)))
    ;; Sorted compare (`seq-set-equal-p' needs Emacs 28.1; this targets 27.1).
    (unless (equal descendants
                   (sort (copy-sequence
                          opencode-shell--known-descendant-session-ids)
                         #'string<))
      (let ((new (seq-difference descendants
                                 opencode-shell--known-descendant-session-ids)))
        (setq opencode-shell--known-descendant-session-ids descendants
              opencode-shell--descendant-session-anchors
              (seq-filter (lambda (entry) (member (car entry) descendants))
                          opencode-shell--descendant-session-anchors))
        (opencode-shell--anchor-descendant-sessions new))
      (opencode-shell--refresh-permissions)
      (opencode-shell--refresh-questions)
      (opencode-shell--schedule-render "descendant-sessions" t))))

(defun opencode-shell--session-permissions (items)
  "Return permission ITEMS belonging to the current session or its subagents."
  (seq-filter
   (lambda (item)
     (opencode-shell--interaction-session-visible-p
      (opencode-shell--interaction-session-id item)))
   items))

(defun opencode-shell--permission-id (item)
  "Return ITEM's usable permission ID, or nil."
  (let ((id (opencode-shell--get item 'id)))
    (and id (not (string-empty-p (format "%s" id))) (format "%s" id))))

(defun opencode-shell--deduplicate-permissions (items)
  "Return ID-bearing ITEMS once each, preserving their first order."
  (let (seen result)
    (dolist (item items (nreverse result))
      (when-let* ((id (opencode-shell--permission-id item)))
        (unless (member id seen)
          (push id seen)
           (push item result))))))

(defun opencode-shell--session-questions (items)
  "Return question ITEMS belonging to the current session or its subagents."
  (seq-filter
   (lambda (item)
     (opencode-shell--interaction-session-visible-p
      (opencode-shell--interaction-session-id item)))
   items))

(defun opencode-shell--question-id (item)
  "Return ITEM's usable question ID, or nil."
  (let ((id (opencode-shell--get item 'id)))
    (and id (not (string-empty-p (format "%s" id))) (format "%s" id))))

(defun opencode-shell--deduplicate-questions (items)
  "Return ID-bearing question ITEMS once each in first-seen order."
  (let (seen result)
    (dolist (item items (nreverse result))
      (when-let* ((id (opencode-shell--question-id item)))
        (unless (member id seen)
          (push id seen)
          (push item result))))))

(defun opencode-shell--receive-questions (items &optional defer-render)
  "Store the authoritative current-session question snapshot ITEMS.
When DEFER-RENDER is non-nil, coalesce presentation at idle time."
  (opencode-shell--consume-question-refresh-pending)
  (opencode-shell--settle-hydration 'questions t)
  (opencode-shell--discover-descendant-sessions items)
  (let ((questions (opencode-shell--deduplicate-questions
                    (opencode-shell--session-questions items))))
    (unless (equal questions opencode-shell--questions-pending)
      (setq opencode-shell--questions-pending questions)
      (when (opencode-shell--human-interaction-blocked-p)
        (opencode-shell--start-polling))
      (if defer-render
          (opencode-shell--schedule-render "questions")
        (opencode-shell--render-turns nil 'none)
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

(defun opencode-shell--permission-at-point ()
  "Return the permission object at point, or the first pending request."
  (or (get-text-property (point) 'opencode-shell-permission)
      (car opencode-shell--permissions)
      (user-error "No pending permission")))

(defun opencode-shell--insert-button-text (label)
  "Insert LABEL at point and return its (BEGIN . END) buffer positions."
  (let ((begin (point)))
    (insert label)
    (cons begin (point))))

(defun opencode-shell--activate-button (span command)
  "Make buffer SPAN clickable, invoking COMMAND on activation.
SPAN is a (BEGIN . END) pair, as returned by
`opencode-shell--insert-button-text'.  With `follow-link' set to `mouse-face',
Emacs translates a primary click on SPAN into a link activation event;
see `mouse-on-link-p'.
Other keys fall back to whichever keymap already covers SPAN, so bare
keyboard shortcuts keep working."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (get-text-property (car span) 'keymap))
    (define-key map [mouse-2] command)
    (add-text-properties
     (car span) (cdr span)
     (list 'mouse-face 'highlight 'follow-link 'mouse-face
           'help-echo (format "mouse-1: %s"
                              (buffer-substring-no-properties (car span) (cdr span)))
           'keymap map))))

(defvar opencode-shell-descendant-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'opencode-shell--open-descendant-session)
    map)
  "Keymap for an inline subagent session reference.")

(defun opencode-shell--open-descendant-session ()
  "Open the subagent session at point, the only known one, or prompt.
With point on a rendered subagent line, open that session.  Otherwise, open
the sole known descendant session directly, or prompt among several."
  (interactive)
  (let ((id (or (get-text-property (point) 'opencode-shell-descendant-session)
                (and (= (length opencode-shell--known-descendant-session-ids) 1)
                     (car opencode-shell--known-descendant-session-ids))
                (and opencode-shell--known-descendant-session-ids
                     (completing-read "Open subagent session: "
                                      opencode-shell--known-descendant-session-ids
                                      nil t))
                (user-error "No subagent session to open"))))
    (opencode-shell-open-session id opencode-shell--directory opencode-shell--profile)))

(defun opencode-shell--insert-descendant-session-line (id &optional running)
  "Insert one clickable line referencing subagent session ID at point.
When RUNNING is non-nil, append a spinner cell driven by the existing
status-spinner overlays."
  (let ((begin (point)) label)
    (insert "◇ subagent ")
    (setq label (opencode-shell--insert-button-text id))
    (insert "  ")
    (insert (propertize "(C-c C-o to open)" 'font-lock-face 'shadow))
    (when running
      (insert "  " (propertize (string opencode-shell--spinner-character)
                               'opencode-shell-spinner t)))
    (insert "\n")
    (add-text-properties
     begin (point)
     `(read-only t rear-nonsticky (read-only keymap)
         keymap ,opencode-shell-descendant-session-map
         opencode-shell-descendant-session ,id))
    (opencode-shell--activate-button label #'opencode-shell--open-descendant-session)))

(defun opencode-shell--descendant-anchor-running-p (anchor)
  "Return non-nil when turn id ANCHOR is a present, still-active turn.
A subagent anchored to an in-progress turn is treated as running."
  (when-let* ((turn (opencode-shell--turn-by-id anchor opencode-shell--turns)))
    (opencode-shell-state-turn-active-p (opencode-shell--turn-status turn))))

(defun opencode-shell--descendant-session-running-p (id anchor)
  "Return non-nil when subagent ID is running.
Prefer its own `/session/status'; before the first poll fall back to whether
its spawning turn ANCHOR is still active."
  (let ((known (assoc id opencode-shell--descendant-session-status)))
    (if known (cdr known)
      (opencode-shell--descendant-anchor-running-p anchor))))

(defun opencode-shell--refresh-descendant-status ()
  "Record which known subagents are running from `/session/status'.
Only runs while subagents are known; a status change schedules a rebuild."
  (when opencode-shell--known-descendant-session-ids
    (opencode-shell--guarded-request
     'descendant-status "GET" "/session/status"
     (lambda (statuses)
       (cl-flet ((busy-p (id)
                   (let* ((s (opencode-shell--get statuses id))
                          (type (and s (format "%s" (or (opencode-shell--get s 'type)
                                                        s)))))
                     (and type (not (equal type "idle"))))))
         (let* ((new-map (mapcar (lambda (id) (cons id (busy-p id)))
                                 opencode-shell--known-descendant-session-ids))
                (self-idle (not (busy-p opencode-shell--session-id)))
                (descendant-changed
                 (not (equal new-map opencode-shell--descendant-session-status)))
                (self-idle-changed
                 (not (eq self-idle opencode-shell--session-idle-p))))
           (when (or descendant-changed self-idle-changed)
             (let ((before-lifecycle (opencode-shell--message-lifecycle-signature))
                   changed-turns)
               (setq opencode-shell--descendant-session-status new-map
                     opencode-shell--session-idle-p self-idle)
               ;; Re-derive turn completion (self-idle may settle a task-only turn)
               ;; before re-rendering.
               (when self-idle-changed
                 (dolist (turn opencode-shell--turns)
                   (unless (eq (opencode-shell--turn-status turn) 'complete)
                     (let ((before (opencode-shell--turn-state-signature turn)))
                       (opencode-shell--aggregate-turn turn)
                       (unless (equal before (opencode-shell--turn-state-signature turn))
                         (push turn changed-turns)))))
                 (setq changed-turns (nreverse changed-turns)))
               (opencode-shell--update-message-lifecycle-state)
               (when (or descendant-changed
                         changed-turns
                         (not (equal before-lifecycle
                                     (opencode-shell--message-lifecycle-signature))))
                 (opencode-shell--schedule-render
                  "descendant-status" descendant-changed changed-turns)))))))
     nil nil)))

(defun opencode-shell--insert-descendant-sessions-for-turn (anchor)
  "Insert subagent lines anchored to turn ID ANCHOR.
Show a spinner for each running subagent.  Read the anchor mapping from
`opencode-shell--descendant-anchor-map'."
  (dolist (id opencode-shell--known-descendant-session-ids)
    (when (equal anchor (alist-get id opencode-shell--descendant-anchor-map
                                   nil nil #'equal))
      (opencode-shell--insert-descendant-session-line
       id (opencode-shell--descendant-session-running-p id anchor)))))

(defun opencode-shell--render-permissions ()
  "Apply only changed interaction text, retaining unchanged native markers."
  (let* ((draft (opencode-shell--composer-text))
         (offset (and (opencode-shell--point-in-composer-p)
                      (- (point) opencode-shell--composer-start)))
         (permissions opencode-shell--permissions)
         (questions opencode-shell--questions-pending)
         (ready (opencode-shell--composer-visible-p))
         (status (opencode-shell--permission-status-display))
         (context (when permissions
                    (opencode-shell--permission-context
                     (car (opencode-shell--deduplicate-permissions permissions)))))
         (projection
          (with-temp-buffer
            (opencode-shell--insert-composer-sentinel)
            (setq-local opencode-shell--composer-start (copy-marker (point-min))
                        opencode-shell--permission-begin (copy-marker (point-min))
                        opencode-shell--permission-end (copy-marker (point-min))
                        opencode-shell--permission-status-begin (copy-marker (point-min))
                        opencode-shell--permission-status-end (copy-marker (point-min))
                        opencode-shell--permissions permissions
                        opencode-shell--questions-pending questions)
            (cl-letf (((symbol-function 'opencode-shell--composer-visible-p) (lambda () ready))
                      ((symbol-function 'opencode-shell--permission-status-display) (lambda () status))
                      ((symbol-function 'opencode-shell--permission-context) (lambda (_) context)))
              (opencode-shell--insert-interaction-region))
            (list (buffer-substring (point-min) opencode-shell--composer-start)
                  (1- opencode-shell--permission-status-begin)
                  (1- opencode-shell--permission-status-end))))
         (begin (marker-position opencode-shell--permission-begin))
         (inhibit-read-only t))
    (opencode-shell--without-user-undo
      (save-excursion
        ;; The response's final blank line and Composer label share a newline.
        ;; Only claim that trailing newline once, when the preceding region
        ;; still holds its full unshared terminator.
        (when (and (> begin (point-min))
                   (eq (char-before begin) ?\n)
                   (if-let* ((last (car (last opencode-shell--rendered-turns))))
                       (let ((expected (length (opencode-shell--cached-response-display last))))
                         (if (> expected 0)
                             (= (- begin (opencode-shell--turn-response-begin last))
                                expected)
                           (and (> begin (1+ (point-min)))
                                (eq (char-before (1- begin)) ?\n))))
                     (or (get-text-property (1- begin) 'opencode-shell-composer-label)
                         (and (> begin (1+ (point-min)))
                              (eq (char-before (1- begin)) ?\n)))))
          (cl-decf begin))
        (opencode-shell--replace-display-region begin opencode-shell--composer-start (car projection))
        (set-marker opencode-shell--transcript-end begin)
        (set-marker opencode-shell--permission-begin begin)
        (when-let* ((last (car (last opencode-shell--rendered-turns))))
          (set-marker (opencode-shell--turn-response-end last) begin))
        (set-marker opencode-shell--permission-status-begin (+ begin (nth 1 projection)))
        (set-marker opencode-shell--permission-status-end (+ begin (nth 2 projection)))
        (set-marker opencode-shell--permission-end (+ begin (length (car projection))))
        (set-marker opencode-shell--composer-start opencode-shell--permission-end)
        (setq opencode-shell--composer-label-visible ready))
      (when offset (goto-char (+ opencode-shell--composer-start offset))))
    (unless (equal draft (opencode-shell--composer-text))
      (error "Interaction rendering changed composer text"))
    (opencode-shell--refresh-composer-overlay)
    (opencode-shell--refresh-spinner-overlays)))

(defun opencode-shell--insert-interaction-region ()
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
              (context (opencode-shell--permission-context item))
              once always reject)
          (insert (propertize "┌─ PERMISSION ─────────────────────────────\n"
                              'font-lock-face 'opencode-shell-permission-face)
                  "│\n"
                  "│  " permission "\n"
                  (if context (concat "│\n│  " context "\n") "")
                  "│\n"
                  "│  C-c C-y ")
          (setq once (opencode-shell--insert-button-text "once"))
          (insert "  C-c C-l ")
          (setq always (opencode-shell--insert-button-text "always"))
          (insert "  C-c C-n ")
          (setq reject (opencode-shell--insert-button-text "reject"))
          (insert "\n└───────────────────────────────────────────\n")
          (add-text-properties
           begin (point)
           `(read-only t rear-nonsticky (read-only keymap)
               keymap ,opencode-shell-permission-map
               opencode-shell-permission ,item))
          (opencode-shell--activate-button once #'opencode-shell--permission-allow-once)
          (opencode-shell--activate-button always #'opencode-shell--permission-allow-always)
          (opencode-shell--activate-button reject #'opencode-shell--permission-reject)))
      (when (and (null opencode-shell--permissions)
                 opencode-shell--questions-pending)
        (let* ((item (car opencode-shell--questions-pending))
               (question (car (or (opencode-shell--get item 'questions)
                                   (list item))))
               (prompt (or (opencode-shell--get question 'question)
                           "Answer required"))
               (header (opencode-shell--get question 'header))
               (begin (point))
               answer reject)
          (insert (propertize "┌─ QUESTION ───────────────────────────────\n"
                              'font-lock-face 'opencode-shell-permission-face)
                  "│\n"
                  (if (and header (not (equal header prompt)))
                      (concat "│  " header "\n│\n")
                    "")
                  "│  " prompt "\n"
                  "│\n"
                  "│  RET/a ")
          (setq answer (opencode-shell--insert-button-text "answer"))
          (insert "  r ")
          (setq reject (opencode-shell--insert-button-text "reject"))
          (insert "\n└───────────────────────────────────────────\n")
          (add-text-properties
           begin (point)
           `(read-only t rear-nonsticky (read-only keymap)
             keymap ,opencode-shell-question-map
             opencode-shell-question ,item))
          (opencode-shell--activate-button answer #'opencode-shell--questions)
          (opencode-shell--activate-button reject #'opencode-shell--question-reject)))
      (set-marker opencode-shell--permission-status-begin (point))
      (when-let* ((status (opencode-shell--permission-status-display)))
        (insert status))
      (set-marker opencode-shell--permission-status-end (point))
      (set-marker opencode-shell--permission-end (point))
      (when (opencode-shell--composer-visible-p)
        (opencode-shell--insert-composer-label))
      (setq opencode-shell--composer-label-visible
            (opencode-shell--composer-visible-p))
      (set-marker opencode-shell--composer-start (point))
      (put-text-property opencode-shell--permission-begin
                         opencode-shell--composer-start
                         'fontified t))
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
  (opencode-shell--discover-descendant-sessions items)
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
        (opencode-shell--render-turns nil 'none)
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
        overlays)
    (dolist (overlay (plist-get entry role))
      (when (overlayp overlay) (delete-overlay overlay)))
    (when (and (< begin end) (< begin (point-max)))
      (let* ((first-end (min end (opencode-shell--gutter-line-end begin)))
             (first-overlay (make-overlay begin first-end nil t nil)))
        (overlay-put first-overlay 'line-prefix
                     (concat (propertize glyph
                                         'face (list label-face gutter-face))
                             opencode-shell--gutter-margin))
        ;; Word-wrapped continuations of any line (including the first)
        ;; keep the color strip aligned without repeating the glyph.
        (overlay-put first-overlay 'wrap-prefix blank)
        (push first-overlay overlays)
        (when (< first-end end)
          (let ((rest-overlay (make-overlay first-end end nil t nil)))
            (overlay-put rest-overlay 'line-prefix blank)
            (overlay-put rest-overlay 'wrap-prefix blank)
            (push rest-overlay overlays)))))
    (setq entry (plist-put entry role overlays))
    (puthash turn entry opencode-shell--turn-gutters)))

(defun opencode-shell--apply-response-gutter (turn response-begin response-end)
  "Apply TURN's assistant gutter within RESPONSE-BEGIN..RESPONSE-END.
Only a completed turn renders its assistant body; status and tool-name
text stay unmarked so transient spinners keep no role label."
  (when (eq (opencode-shell--turn-status turn) 'complete)
    (when-let* ((start (text-property-any response-begin response-end
                                          'opencode-shell-assistant-body t))
                (end (next-single-property-change
                      start 'opencode-shell-assistant-body nil response-end)))
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
  "Insert immutable user and canonical response blocks for TURN.
Also inserts any subagent session line anchored to TURN, placed before the
response block so a `Receiving'/`Thinking' spinner for an active turn always
renders last, below every subagent line rather than above it."
  (pcase-let ((`(,user-begin . ,user-end)
               (opencode-shell--insert-user-prompt turn)))
    (opencode-shell--insert-permission-results (opencode-shell--turn-id turn))
    (opencode-shell--insert-descendant-sessions-for-turn
     (opencode-shell--turn-id turn))
    (let ((response-begin (point)))
      (insert (opencode-shell--cached-response-display turn))
      (let ((response-end (point)))
        (opencode-shell--apply-response-gutter turn response-begin response-end)
        (add-text-properties user-begin user-end
                             '(read-only t rear-nonsticky (read-only face)))
        (put-text-property user-begin response-begin 'fontified t)
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
  "Return the propertized response display for TURN.
Show the response even in the interaction region when RELOCATED is non-nil."
  (if (and (not relocated)
           (eq turn (opencode-shell--permission-status-turn)))
      ""
    (let* ((raw-body (or (opencode-shell--assistant-display-text turn) ""))
           (body (if (string-empty-p raw-body)
                     ""
                   (propertize raw-body 'opencode-shell-assistant-body t))))
      (concat
       (opencode-shell--tool-name-display turn)
       (if (eq (opencode-shell--turn-status turn) 'complete)
           (concat body
                   (opencode-shell--turn-terminal-error-suffix turn) "\n\n")
         (concat (if (string-empty-p body) ""
                   (concat body (if (string-suffix-p "\n" body) "\n" "\n\n")))
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
                            'opencode-shell-error-face
                          'opencode-shell-waiting-face))))))))

(defvar-local opencode-shell--response-display-cache nil
  "Per-turn cache of rendered response strings, keyed by turn identity.
A weak-key `eq' table so dropped turns are collected automatically.")

(defun opencode-shell--cached-response-display (turn)
  "Return TURN's response display, reusing a cached projection when possible.
The cache is keyed by TURN identity and validated by its presentation signature
plus the render width, so an unchanged turn is not re-projected (the costly
markdown/table work) on a full rebuild or reconcile.  The permission status turn
is never cached: its display depends on interaction state outside the signature."
  (if (eq turn (opencode-shell--permission-status-turn))
      (opencode-shell--response-display turn)
    (unless opencode-shell--response-display-cache
      (setq opencode-shell--response-display-cache
            (make-hash-table :test 'eq :weakness 'key)))
    (let ((sig (cons opencode-shell--table-render-width
                     (opencode-shell--turn-state-signature turn)))
          (cached (gethash turn opencode-shell--response-display-cache)))
      (if (and cached (equal (car cached) sig))
          (cdr cached)
        (let ((display (opencode-shell--response-display turn)))
          (puthash turn (cons sig display) opencode-shell--response-display-cache)
          display)))))

(defun opencode-shell--tool-name-display (turn)
  "Return payload-free tool names observed in TURN."
  (let* ((complete (eq (opencode-shell--turn-status turn) 'complete))
         (tools (if complete
                    (opencode-shell-response-tool-names
                     (opencode-shell--turn-parts turn))
                  (opencode-shell-response-tool-progress
                   (opencode-shell--turn-parts turn)))))
    (if tools
        (propertize
         (concat (mapconcat (lambda (tool)
                              (if complete (format "TOOL> %s" tool)
                                (format "TOOL> %s: %s" (car tool) (cdr tool))))
                            tools "\n")
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
   ((when-let* ((turn (opencode-shell--permission-status-turn)))
      (opencode-shell--response-display turn t)))))

(defconst opencode-shell--spinner-neon-colors
  '("#ff2d55" "#ff8a00" "#ffe600" "#39ff6a" "#22d3ee" "#b14dff")
  "Retro neon chaser palette cycled by the transient-status spinner.
Colors travel across the growing dot row and the status label one step per
animation tick, like a carnival marquee chase light.")

(defun opencode-shell--spinner-neon-color (index)
  "Return the neon chaser color for position INDEX, wrapping as needed."
  (let ((n (length opencode-shell--spinner-neon-colors)))
    (nth (mod index n) opencode-shell--spinner-neon-colors)))

(defun opencode-shell--status-display (label)
  "Return LABEL with the current UI-only spinner frame.
Both LABEL and the spinner dots are neon-chase colored in lockstep (see
`opencode-shell--spinner-neon-colors'); LABEL is colored as a single unit --
one large dot sitting just before the first real dot in the chase -- not
letter by letter, so it shifts color without looking like it is flickering."
  (concat (propertize label 'opencode-shell-spinner-label t) " "
          (propertize (string opencode-shell--spinner-character)
                      'opencode-shell-spinner t)
          "\n\n"))

(defun opencode-shell--clear-spinner-overlays ()
  "Delete presentation-only spinner overlays in the current buffer."
  (mapc #'delete-overlay opencode-shell--spinner-overlays)
  (setq opencode-shell--spinner-overlays nil))

(defun opencode-shell--spinner-frame ()
  "Return the current right-growing, neon-chasing spinner display frame."
  (let ((base opencode-shell--animation-frame))
    (mapconcat
     (lambda (offset)
       (propertize (string opencode-shell--spinner-character)
                   'face (list :foreground
                               (opencode-shell--spinner-neon-color
                                (+ base offset)))))
     (number-sequence 0 base) "")))

(defun opencode-shell--spinner-label-face ()
  "Return the current neon chaser face for a spinner's status label.
The label is one step behind the first dot, same as a single large dot
leading the chase."
  (list :foreground
        (opencode-shell--spinner-neon-color
         (1- opencode-shell--animation-frame))))

(defun opencode-shell--reset-spinner-frame ()
  "Reset transient progress presentation to its first frame."
  (unless (zerop opencode-shell--animation-frame)
    (setq opencode-shell--animation-frame 0)
    (opencode-shell--render-status-animation)))

(defun opencode-shell--spinner-scan-start ()
  "Return the earliest buffer position that may hold a live spinner."
  (if (and opencode-shell--known-descendant-session-ids
           (or (null opencode-shell--descendant-session-status)
               (seq-some #'cdr opencode-shell--descendant-session-status)))
      (point-min)
    (let ((start (and (markerp opencode-shell--permission-begin)
                      (marker-position opencode-shell--permission-begin))))
      (dolist (turn opencode-shell--turns)
        (unless (eq (opencode-shell--turn-status turn) 'complete)
          (when-let* ((pos (and (markerp (opencode-shell--turn-response-begin turn))
                               (marker-position (opencode-shell--turn-response-begin turn)))))
            (setq start (if start (min start pos) pos)))))
      (if start
          (max (point-min) start)
        (point-min)))))

(defun opencode-shell--refresh-spinner-overlays ()
  "Recreate spinner overlays for status slots in the current buffer."
  (opencode-shell--clear-spinner-overlays)
  ;; Label overlays go first so the dots overlay this status slot ends up at
  ;; the head of `opencode-shell--spinner-overlays' (tests key off `car' to
  ;; find the dots overlay specifically).
  (let* ((scan-start (opencode-shell--spinner-scan-start))
         (position scan-start))
    (while (setq position
                 (text-property-any position (point-max)
                                    'opencode-shell-spinner-label t))
      (let ((end (next-single-property-change
                  position 'opencode-shell-spinner-label nil (point-max))))
        (let ((overlay (make-overlay position end nil t nil)))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'opencode-shell-spinner-label t)
          (overlay-put overlay 'face (opencode-shell--spinner-label-face))
          (push overlay opencode-shell--spinner-overlays))
        (setq position end)))
    (setq position scan-start)
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
  (if-let* ((label (opencode-shell--turn-terminal-error turn)))
      (propertize (format "\n[%s]\n" label) 'font-lock-face 'opencode-shell-error-face)
    ""))

(defun opencode-shell--schedule-table-layout ()
  "Defer `opencode-shell--refresh-table-layout' off the window hook.
Running the width-driven re-render synchronously inside
`window-configuration-change-hook' lets the render's own buffer/window size
change re-enter the hook, which can recurse until the Lisp stack is exhausted.
Scheduling the work on an idle timer makes the hook return immediately and
coalesces repeated changes into a single off-hook render."
  (unless (or opencode-shell--table-rerendering
              (when-let* ((window (opencode-shell--transcript-window)))
                (equal (window-body-width window)
                       opencode-shell--table-render-width)))
    (let ((buffer (current-buffer)))
      (when (timerp opencode-shell--table-layout-timer)
        (cancel-timer opencode-shell--table-layout-timer))
      (setq opencode-shell--table-layout-timer
            (run-with-idle-timer
             0 nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq opencode-shell--table-layout-timer nil)
                   (opencode-shell--refresh-table-layout)))))))))

(defun opencode-shell--refresh-table-layout ()
  "Rerender completed tables when the transcript window width changes."
  (unless opencode-shell--table-rerendering
    (when-let* ((window (opencode-shell--transcript-window))
                (width (window-body-width window)))
      (unless (equal width opencode-shell--table-render-width)
        (let ((table-turns
               (seq-filter
                (lambda (turn)
                  (when-let* ((raw (opencode-shell--turn-assistant turn)))
                    (string-match-p "|" raw)))
                opencode-shell--rendered-turns)))
          (setq opencode-shell--table-render-width width)
          (cond
           (opencode-shell--render-dirty
            (setq opencode-shell--render-dirty-turns
                  (seq-uniq (append opencode-shell--render-dirty-turns
                                    table-turns)
                            #'eq))
            (let ((opencode-shell--table-rerendering t))
              (opencode-shell--flush-render)))
           ((or table-turns
                (< (length opencode-shell--rendered-turns)
                   (length opencode-shell--turns)))
            (let ((opencode-shell--table-rerendering t))
              (opencode-shell--render-turns nil (or table-turns 'none))))))))))

(defun opencode-shell--replace-display-region (begin end display)
  "Replace only the changed middle of BEGIN..END with propertized DISPLAY.
Leave unchanged prefix/suffix characters, and their native position markers,
in place.  Return non-nil when text changed."
  (let* ((begin (if (markerp begin) (marker-position begin) begin))
         (old (buffer-substring begin end))
         (old-length (length old)) (new-length (length display))
         (changed (not (equal-including-properties old display)))
         (prefix 0) (suffix 0))
    (unless (string= old display)
      (while (and (< prefix (min old-length new-length))
                  (= (aref old prefix) (aref display prefix)))
        (cl-incf prefix))
      (while (and (< suffix (- (min old-length new-length) prefix))
                  (= (aref old (- old-length suffix 1))
                     (aref display (- new-length suffix 1))))
        (cl-incf suffix))
      (goto-char (+ begin prefix))
      (delete-region (point) (- end suffix))
      (insert-before-markers (substring display prefix (- new-length suffix))))
    ;; Equal characters can carry a different question/permission identity or
    ;; keymap. Refresh properties in place without deleting retained text.
    (when changed
      (let ((position 0))
        (while (< position new-length)
          (let ((next (next-property-change position display new-length)))
            (set-text-properties (+ begin position) (+ begin next)
                                 (text-properties-at position display))
            (setq position next)))))
    changed))

(defun opencode-shell--update-turn-response (turn)
  "Update only TURN's immutable response block."
  (let ((begin (opencode-shell--turn-response-begin turn))
        (end (opencode-shell--turn-response-end turn))
        (user-end-position
         (marker-position (opencode-shell--turn-user-end turn)))
        (display (opencode-shell--cached-response-display turn))
        (inhibit-read-only t))
    (let ((position (marker-position begin)))
      (when (opencode-shell--replace-display-region begin end display)
        (set-marker (opencode-shell--turn-user-end turn) user-end-position)
        (set-marker begin position)
        (add-text-properties begin end
                            '(read-only t rear-nonsticky (read-only face)))
        (opencode-shell--apply-response-gutter turn position end)))))

(defun opencode-shell--render-status-animation ()
  "Update transient spinner presentation without modifying buffer text."
  (setq opencode-shell--spinner-overlays
        (seq-filter #'overlay-buffer opencode-shell--spinner-overlays))
  (dolist (overlay opencode-shell--spinner-overlays)
    (if (overlay-get overlay 'opencode-shell-spinner-label)
        (overlay-put overlay 'face (opencode-shell--spinner-label-face))
      (overlay-put overlay 'display (opencode-shell--spinner-frame))))
  (when opencode-shell--spinner-overlays
    (force-window-update (current-buffer))))

(defun opencode-shell--animation-tick ()
  "Advance one UI-only spinner frame without issuing network requests."
  (setq opencode-shell--animation-frame
        (mod (1+ opencode-shell--animation-frame)
             opencode-shell--spinner-max-width))
  (opencode-shell--render-status-animation))

(defun opencode-shell--turns-to-update (changed-turns)
  "Return rendered turns needing incremental response-block updates.
When CHANGED-TURNS is nil, return all rendered turns for direct callers.
When CHANGED-TURNS is `none' or a list, return explicit turns plus any
rendered nonterminal or relocated turn."
  (if (null changed-turns)
      opencode-shell--rendered-turns
    (let ((explicit (if (eq changed-turns 'none) nil changed-turns))
          (current-relocated (opencode-shell--permission-status-turn))
          result)
      (dolist (turn explicit)
        (when (memq turn opencode-shell--rendered-turns)
          (push turn result)))
      (dolist (turn (list opencode-shell--relocated-turn current-relocated))
        (when (and turn
                   (memq turn opencode-shell--rendered-turns)
                   (not (memq turn result)))
          (push turn result)))
      (dolist (turn opencode-shell--rendered-turns)
        (when (and (not (eq (opencode-shell--turn-status turn) 'complete))
                   (not (memq turn result)))
          (push turn result)))
      (nreverse result))))

(defcustom opencode-shell-max-rendered-turns 30
  "Maximum number of most-recent turns drawn in a transcript buffer.
Older turns stay tracked in `opencode-shell--turns' for state and reconciliation
but are not drawn, keeping each render/reconcile O(this) regardless of how large
the session grows -- the dominant cost that makes big sessions stutter on
single-threaded Emacs.  nil draws every turn."
  :type '(choice (const :tag "All" nil) integer) :group 'opencode-shell)

(defun opencode-shell--rendered-turn-window ()
  "Return the turns to draw: all of them, or the most recent capped window.
See `opencode-shell-max-rendered-turns'."
  (let ((cap opencode-shell-max-rendered-turns)
        (turns opencode-shell--turns))
    (if (and (integerp cap) (> cap 0) (> (length turns) cap))
        (last turns cap)
      turns)))

(defun opencode-shell--turn-prefix-display (turn)
  "Project TURN's prompt and anchored lines using the canonical inserters."
  (let ((turns opencode-shell--turns)
        (resolved opencode-shell--resolved-permissions)
        (ids opencode-shell--known-descendant-session-ids)
        (anchors opencode-shell--descendant-anchor-map)
        (statuses opencode-shell--descendant-session-status))
    (with-temp-buffer
      (let ((opencode-shell--turns turns)
            (opencode-shell--resolved-permissions resolved)
            (opencode-shell--known-descendant-session-ids ids)
            (opencode-shell--descendant-anchor-map anchors)
            (opencode-shell--descendant-session-status statuses))
        (opencode-shell--insert-user-prompt (copy-opencode-shell--turn turn))
        (opencode-shell--insert-permission-results (opencode-shell--turn-id turn))
        (opencode-shell--insert-descendant-sessions-for-turn (opencode-shell--turn-id turn))
        (buffer-string)))))

(defun opencode-shell--update-turn-prefix (turn)
  "Minimally update TURN's prompt/anchors without replacing its response."
  (let* ((begin (marker-position (opencode-shell--turn-user-begin turn)))
         (display (opencode-shell--turn-prefix-display turn)))
    (opencode-shell--replace-display-region
     begin (opencode-shell--turn-response-begin turn) display)
    (set-marker (opencode-shell--turn-user-begin turn) begin)
    (set-marker (opencode-shell--turn-user-end turn)
                (+ begin (length (or (opencode-shell--turn-user turn) "")) 1))
    (set-marker (opencode-shell--turn-response-begin turn) (+ begin (length display)))
    (add-text-properties begin (+ begin (length display))
                         '(read-only t fontified t rear-nonsticky (read-only face)))
    (opencode-shell--apply-turn-gutter
     turn :user begin (opencode-shell--turn-user-end turn)
     opencode-shell--gutter-glyph-user 'opencode-shell-user-face 'opencode-shell-user-gutter-face)))

(defun opencode-shell--reconcile-turn-blocks (turns force changed-turns)
  "Reconcile the order of TURNS, retaining live blocks where possible.
If FORCE is non-nil, update all projections; otherwise use CHANGED-TURNS."
  (let ((old opencode-shell--rendered-turns)
        (updates (if force turns (opencode-shell--turns-to-update changed-turns)))
        fontify)
    (unless (seq-every-p #'opencode-shell--turn-rendered-p old)
      (dolist (turn (seq-uniq (append old turns) #'eq))
        (opencode-shell--discard-turn-markers turn))
      (opencode-shell--clear-all-turn-gutters)
      (delete-region (point-min) opencode-shell--transcript-end)
      (setq old nil))
    ;; Leading resolved results are independent of turn blocks.
    (let ((resolved opencode-shell--resolved-permissions)
          (all-turns opencode-shell--turns))
      (opencode-shell--replace-display-region
       (point-min) (if old (opencode-shell--turn-user-begin (car old))
                     opencode-shell--transcript-end)
       (with-temp-buffer
         (let ((opencode-shell--resolved-permissions resolved)
               (opencode-shell--turns all-turns))
           (opencode-shell--insert-permission-results nil)
           (opencode-shell--insert-unanchored-permission-results)
           (buffer-string)))))
    (let ((cursor (if old (marker-position (opencode-shell--turn-user-begin (car old)))
                    (marker-position opencode-shell--transcript-end))))
      (dolist (turn turns)
        (when (memq turn old)
          (unless (eq turn (car old))
            (let ((end (marker-position (opencode-shell--turn-user-begin turn))))
              (while (not (eq turn (car old)))
                (opencode-shell--discard-turn-markers (pop old)))
              (delete-region cursor end)))
          (save-excursion
            (opencode-shell--update-turn-prefix turn)
            (when (memq turn updates)
              (opencode-shell--update-turn-response turn)
              (push turn fontify))
            (when force
              (opencode-shell--apply-response-gutter
               turn (opencode-shell--turn-response-begin turn)
               (opencode-shell--turn-response-end turn))))
          (pop old)
          (setq cursor (if old (marker-position (opencode-shell--turn-user-begin (car old)))
                         (marker-position (opencode-shell--turn-response-end turn)))))
        (unless (opencode-shell--turn-rendered-p turn)
          (goto-char cursor)
          (when (and (null old) (> (point) (point-min))
                     (eq (char-before) ?\n)
                     (not (eq (char-before (1- (point))) ?\n)))
            (insert (propertize "\n" 'read-only t 'rear-nonsticky '(read-only))))
          (opencode-shell--insert-turn-blocks turn)
          (setq cursor (point))
          ;; A following block's opening marker must follow the inserted block.
          (when old (set-marker (opencode-shell--turn-user-begin (car old)) cursor))
          (push turn fontify)))
      (when old
        (let ((end (marker-position opencode-shell--transcript-end)))
          (dolist (turn old) (opencode-shell--discard-turn-markers turn))
          (delete-region cursor end)))
      (set-marker opencode-shell--transcript-end cursor))
    (or (nreverse fontify) 'none)))

(defun opencode-shell--render-turns (&optional force changed-turns)
  "Render immutable turn blocks without changing composer bytes or point.
When FORCE is non-nil, refresh every turn's projection and gutter overlays.
Otherwise update only CHANGED-TURNS when that list is non-nil."
  (opencode-shell--without-user-undo
   (let* ((composer-offset (and (opencode-shell--point-in-composer-p)
                                (- (point) opencode-shell--composer-start)))
         (old-point (copy-marker (point)))
         (composer-boundary (copy-marker opencode-shell--composer-start t))
          (inhibit-read-only t)
          ;; Only the most-recent window of turns is drawn (see
          ;; `opencode-shell--rendered-turn-window'); state keeps every turn.
          (rlist (opencode-shell--rendered-turn-window))
          fontify-turns)
     (unwind-protect
      (progn
    (let* ((signature
            (list opencode-shell--known-descendant-session-ids
                  (mapcar (lambda (turn)
                            (list turn
                                  (seq-filter (lambda (part)
                                                (equal (opencode-shell--get part 'tool) "task"))
                                              (opencode-shell--turn-parts turn))))
                          rlist)))
           (opencode-shell--descendant-anchor-map
            (if (and (not force) (equal signature opencode-shell--rendered-anchor-signature))
                opencode-shell--rendered-anchor-map
              (setq opencode-shell--rendered-anchor-signature signature
                    opencode-shell--rendered-anchor-map
                    (and opencode-shell--known-descendant-session-ids
                         (opencode-shell--compute-descendant-anchor-map))))))
      (setq fontify-turns
            (opencode-shell--reconcile-turn-blocks rlist force changed-turns))
      (setq opencode-shell--relocated-turn
            (opencode-shell--permission-status-turn))
      (setq opencode-shell--rendered-turns (copy-sequence rlist))
      (set-marker opencode-shell--permission-begin opencode-shell--transcript-end)
      (set-marker opencode-shell--composer-start composer-boundary))
    (opencode-shell--render-permissions)
    (opencode-shell--fontify-transcript fontify-turns)
    (if composer-offset
        (goto-char (min (point-max) (+ opencode-shell--composer-start composer-offset)))
      (goto-char (min old-point (point-max)))))
      (set-marker old-point nil)
      (set-marker composer-boundary nil)))
   (opencode-shell--sync-input-policy)))

(defun opencode-shell--fontify-transcript (&optional turns)
  "Apply Markdown font-lock faces to assistant response bodies in TURNS.
Scope fontification to each turn's assistant text, leaving the manual faces on
user prompts, tool lines, and status markers untouched, so color appears even
when `global-font-lock-mode' / jit-lock is not driving this buffer.  The
`fontified' property, marked over the whole response region once handled, lets
later renders skip settled turns without re-scanning."
  (when (and font-lock-defaults (not (eq turns 'none)))
    (dolist (turn (or turns opencode-shell--rendered-turns))
      (when (opencode-shell--turn-rendered-p turn)
        (let ((rb (marker-position (opencode-shell--turn-response-begin turn)))
              (re (marker-position (opencode-shell--turn-response-end turn))))
          (when (and (< rb re) (text-property-not-all rb re 'fontified t))
            (let* ((start (text-property-any
                           rb re 'opencode-shell-assistant-body t))
                   (end (and start
                             (next-single-property-change
                              start 'opencode-shell-assistant-body nil re))))
              (when (and start end (< start end))
                (font-lock-ensure start end))
              (let ((inhibit-read-only t))
                (put-text-property rb re 'fontified t))
              (when (and (eq (opencode-shell--turn-status turn) 'complete)
                         (hash-table-p opencode-shell--response-display-cache))
                (when-let* ((cached (gethash turn opencode-shell--response-display-cache))
                            (orig (cdr cached)))
                  (let* ((rel-start (and start (- start rb)))
                         (rel-end (and end (- end rb)))
                         (updated
                          (if (and rel-start rel-end
                                   (<= 0 rel-start)
                                   (<= rel-start rel-end)
                                   (<= rel-end (length orig)))
                              (concat (substring orig 0 rel-start)
                                      (let ((s (buffer-substring start end)))
                                        (remove-text-properties
                                         0 (length s)
                                         '(read-only nil rear-nonsticky nil) s)
                                        s)
                                      (substring orig rel-end))
                            (copy-sequence orig))))
                    (put-text-property 0 (length updated) 'fontified t updated)
                    (setcdr cached updated)))))))))))

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
      (opencode-shell--render-turns force (or changed-turns 'none))
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
   ;; An open session must observe new requests even while all known turns are
   ;; complete. Restore observation when displaying an older unsubscribed buffer.
  (when (and opencode-shell--session-id
             (not opencode-shell--runtime-key)
              (get-buffer-window (current-buffer) t))
    (opencode-shell--start-polling))
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
  (when-let* ((turn (seq-find
                    (lambda (entry)
                      (equal opencode-shell--submit-in-flight
                             (opencode-shell--turn-id entry)))
                    opencode-shell--turns)))
    (when (eq (opencode-shell--turn-status turn) 'complete)
      (setq opencode-shell--submit-in-flight nil)))
  (let ((needed (opencode-shell-state-polling-needed-p
                 (mapcar #'opencode-shell--turn-status opencode-shell--turns)
                 opencode-shell--submit-in-flight
                 (not (opencode-shell--initial-hydration-complete-p)))))
    (cond
     ;; Enforce the invariant symmetrically: an active turn (e.g. `receiving')
     ;; must have a live runtime, or it is stuck forever with nothing to update
     ;; it.  Starting is idempotent, so re-deriving lifecycle self-heals a buffer
     ;; whose polling was torn down while work remained.
      ((and (or needed opencode-shell--session-id) (not opencode-shell--runtime-key))
       (opencode-shell--start-polling))
      ;; Turn completion is not session closure. In particular, cancellation
      ;; here would discard queued permission/question events behind completion.
      ((and (not opencode-shell--session-id)
            (not needed) (not (opencode-shell--human-interaction-blocked-p)))
      (opencode-shell--stop-polling)))))


(defun opencode-shell--render-messages
    (messages &optional sequence defer-render authoritative)
  "Reconcile chronological message envelopes from MESSAGES.
Ignore a non-nil SEQUENCE that is no newer than the applied sequence.
Render immediately unless DEFER-RENDER is non-nil.  When AUTHORITATIVE is
non-nil, remove cached server messages absent from the snapshot."
  (when (or (null sequence) (> sequence opencode-shell--message-applied-sequence))
    (let ((before (opencode-shell--message-lifecycle-signature))
          (prior-order (mapcar #'opencode-shell--turn-id opencode-shell--turns))
          cache-result)
      (when sequence (setq opencode-shell--message-applied-sequence sequence))
      (setq cache-result
            (opencode-shell--cache-message-snapshot messages authoritative)
            messages (or (plist-get cache-result :messages) messages))
      (when authoritative
        (opencode-shell--restore-agent-model-history messages))
      (setq opencode-shell--turns (opencode-shell--normalize-turns messages authoritative))
      (when (and authoritative
                 (not (equal prior-order (mapcar #'opencode-shell--turn-id opencode-shell--turns))))
        (setq cache-result (plist-put cache-result :force t)))
      (setq opencode-shell--normalized-changed-turns
            (seq-uniq
             (append (plist-get cache-result :changed-turns)
                     opencode-shell--normalized-changed-turns)
             #'eq))
      (opencode-shell--update-message-lifecycle-state)
      (when (or (plist-get cache-result :force)
                opencode-shell--normalized-changed-turns
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
                 (or opencode-shell--normalized-changed-turns 'none))
                (opencode-shell--log-lifecycle event)
                (force-mode-line-update))
              (setq opencode-shell--render-dirty nil
                    opencode-shell--render-event nil
                    opencode-shell--render-dirty-turns nil))))))))

(defun opencode-shell--guarded-request (key method path callback &optional body error-callback params)
  "Request PATH with METHOD once per generation under KEY.
PARAMS, when non-nil, are query parameters forwarded to the underlying request."
  (unless (alist-get key opencode-shell--in-flight)
    (let ((origin (current-buffer))
          (generation opencode-shell--generation)
          (request-mode major-mode)
          (attempt (make-symbol "opencode-snapshot")) timer response cancel-request)
      (setf (alist-get key opencode-shell--in-flight) attempt)
      (cl-labels ((owned-p ()
                    (eq attempt (alist-get key opencode-shell--in-flight)))
                  (current-p ()
                    (and (eq major-mode request-mode)
                         (= generation opencode-shell--generation)
                         (owned-p)))
                  (release ()
                    (when (timerp timer) (cancel-timer timer))
                    (opencode-shell-async-unregister-request origin cancel-request)
                    (setf (alist-get key opencode-shell--in-flight) nil))
                  (cancel ()
                    (when (owned-p)
                      (release)
                      (opencode-shell--kill-url-buffer response)))
                  (settle (success value)
                    ;; Release the guard whenever THIS attempt resolves, even if
                    ;; the generation or mode changed meanwhile.  Clearing only
                    ;; while current left a request that settled after a
                    ;; generation bump holding its guard forever, which blocked
                    ;; the resource (e.g. `messages') and stuck a turn in
                    ;; `receiving' until a manual `g r'.  Apply the result only
                    ;; while still current.
                    (when (owned-p)
                      (let ((apply (current-p)))
                        (release)
                        (when apply
                          (if success (funcall callback value)
                            (when error-callback (funcall error-callback))))))))
        (setq cancel-request #'cancel)
        (opencode-shell-async-register-request origin cancel-request)
        (when (equal method "GET")
          (setq timer
                (run-at-time
                 opencode-shell--snapshot-request-timeout nil
                 (lambda ()
                   (when (buffer-live-p origin)
                     (with-current-buffer origin
                       (when (owned-p)
                         (opencode-shell--kill-url-buffer response)
                         (settle nil nil))))))))
        (setq response
              (opencode-shell--request
               method path
               (lambda (value) (settle t value))
               body params
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
  "Refresh title metadata for the current session.
Also classifies the same `/session' snapshot for subagent descendants (see
`opencode-shell--sync-descendant-sessions'), so a quiet subagent surfaces on
the next full resync without a dedicated request of its own."
  (opencode-shell--guarded-request
   'session "GET" "/session"
   (lambda (sessions)
     (when-let* ((session (seq-find
                          (lambda (item)
                            (equal opencode-shell--session-id
                                   (opencode-shell--get item 'id)))
                          sessions)))
       (setq opencode-shell--session-title
             (or (opencode-shell--get session 'title) "Untitled"))
       (opencode-shell--rename-transcript-for-title opencode-shell--session-title)
       (let ((subagent (opencode-shell--child-session-p session)))
         (unless (eq subagent opencode-shell--subagent-session-p)
           (setq opencode-shell--subagent-session-p subagent)
           (opencode-shell--schedule-render "session-metadata")))
       (force-mode-line-update))
     (opencode-shell--sync-descendant-sessions sessions))
   nil nil '((limit . 1000))))

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
                 (unless (when-let* ((runtime (opencode-shell-async-runtime-get
                                             (opencode-shell--server-key profile))))
                           (gethash origin (plist-get runtime :subscribers)))
                   (setq opencode-shell--connection-stale nil)
                   (opencode-shell--resync t))))))
         (lambda () (buffer-live-p origin))))
    (opencode-shell--resync-snapshots full resource)))

(defun opencode-shell--resync-snapshots (&optional full resource)
  "Fetch authoritative snapshots for RESOURCE, including metadata if FULL."
  (opencode-shell--retry-hydration
   (if (memq resource '(nil all))
       '(messages permissions questions)
     (list resource)))
  (when full (opencode-shell--refresh-session-metadata))
  (when (memq resource '(nil all messages))
    (unless (alist-get 'messages opencode-shell--in-flight)
      (let ((sequence (cl-incf opencode-shell--message-request-sequence))
            (revision opencode-shell--message-state-revision)
            (marker opencode-shell--pending-session-updated))
        (setq opencode-shell--last-message-recovery-attempt-at (float-time))
        (opencode-shell--guarded-request
         'messages
         "GET" (format "/session/%s/message" opencode-shell--session-id)
         (lambda (messages)
           (opencode-shell--settle-hydration 'messages t)
           (if (= revision opencode-shell--message-state-revision)
               (when (> sequence opencode-shell--message-applied-sequence)
                 (setq opencode-shell--transient-reconcile-pending nil)
                 (opencode-shell--render-messages messages sequence t t)
                 (opencode-shell--confirm-session-updated marker))
             (opencode-shell--ensure-message-cache)
             (let ((unseen
                    (seq-remove
                     (lambda (envelope)
                       (let ((id (opencode-shell--message-envelope-id envelope)))
                         (or (null id)
                             (gethash id opencode-shell--message-envelopes)
                             (gethash id opencode-shell--removed-message-ids))))
                     messages)))
               (when unseen
                 (opencode-shell--render-messages unseen nil t nil)))
             (setq opencode-shell--transient-reconcile-pending t)
             (opencode-shell--schedule-event-reconciliation 'messages)))
         nil (lambda () (opencode-shell--settle-hydration 'messages nil))))))
  (when (memq resource '(nil all permissions))
    (opencode-shell--guarded-request
     'permissions "GET" "/permission"
     (lambda (items) (opencode-shell--receive-permissions items t))
     nil #'opencode-shell--consume-permission-refresh-pending))
  (when (memq resource '(nil all questions))
    (opencode-shell--refresh-questions))
  (when (memq resource '(nil all))
    (opencode-shell--refresh-descendant-status))
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
                         (when-let* ((key (and opencode-shell--profile
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
  (when opencode-shell--subagent-session-p
    (user-error "Subagent sessions are driven by their parent's task tool, not direct prompts"))
  (when (and opencode-shell--profile
             (opencode-shell--ssh-forwarded-profile-p opencode-shell--profile)
             (not (opencode-shell-recovery-ready-p
                   (opencode-shell--server-key opencode-shell--profile))))
    (user-error "OpenCode SSH transport offline; retry with g r"))
  (let ((text (opencode-shell--composer-text)))
    (when opencode-shell--submit-in-flight
      (user-error "A prompt delivery is already being reconciled"))
    (when (string-blank-p text) (user-error "Prompt is blank"))
    (let ((settled (seq-filter
                    (lambda (existing)
                      (not (eq (opencode-shell--turn-status existing) 'complete)))
                    opencode-shell--turns)))
      (opencode-shell--settle-superseded-turns opencode-shell--turns t)
      (let ((turn (opencode-shell--make-turn
                    :id (format "msg_%s_%d" (format-time-string "%s%N")
                                (cl-incf opencode-shell--turn-counter))
                    :user text :status 'sending)))
        (setq opencode-shell--turns (append opencode-shell--turns (list turn))
              opencode-shell--session-idle-p nil
              opencode-shell--request-status "sending"
              opencode-shell--submit-in-flight (opencode-shell--turn-id turn))
        (opencode-shell--replace-composer "")
        (opencode-shell--render-turns nil (or settled 'none))
        (setq buffer-undo-list nil)
        (undo-boundary)
        (opencode-shell--start-polling)
        (opencode-shell--log-lifecycle "submit" t)
        (force-mode-line-update)
        (opencode-shell--request
         "POST" (format "/session/%s/prompt_async" opencode-shell--session-id)
         (lambda (_)
           (when (eq (opencode-shell--turn-status turn) 'sending)
             (setf (opencode-shell--turn-status turn) 'waiting)
             (opencode-shell--schedule-render "submit-ack" nil (list turn)))
           (opencode-shell--log-lifecycle "submit-ack")
           (unless (and opencode-shell--runtime-key
                        (opencode-shell-async-sse-connected-p
                         opencode-shell--runtime-key))
             (opencode-shell--resync)))
         (cons `(messageID . ,(opencode-shell--turn-id turn))
               (opencode-shell--prompt-body text)) nil
         (lambda ()
           (setf (opencode-shell--turn-status turn) 'recovering)
           (setq opencode-shell--request-status "recovering")
           (opencode-shell--schedule-render "submit-error" nil (list turn))
           (opencode-shell--log-lifecycle "submit-error" t)
           (opencode-shell--resync)
           (force-mode-line-update)))))))

(defun opencode-shell--abort ()
  "Abort work in the current session."
  (interactive)
  (setq opencode-shell--request-status "aborting")
  (let ((target (car (last opencode-shell--turns))))
    (when (and target
               (not (eq (opencode-shell--turn-status target) 'complete)))
      (setf (opencode-shell--turn-status target) 'aborting)
      (opencode-shell--render-turns nil (list target)))
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
                               (opencode-shell--schedule-render
                                "abort-ack" nil (and target (list target)))
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
    (when-let* ((context (or (opencode-shell--get metadata 'command)
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
  "Send a KIND request for ID using METHOD and PATH.
Send BODY and settle only the matching request, calling SUCCESS or FAILURE."
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
                         (after-turn-id . ,(when-let* ((turn (car (last opencode-shell--turns))))
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
  (if-let* ((item (or (get-text-property (point) 'opencode-shell-question)
                     (car opencode-shell--questions-pending))))
      (opencode-shell--question-reply item nil)
    (opencode-shell--choose-pending
     "question" (lambda (item) (opencode-shell--question-reply item t)))))

(defun opencode-shell--question-reply (item confirm)
  "Answer ITEM, prompting for rejection first when CONFIRM is non-nil."
  (when (opencode-shell-interaction-active-p
         opencode-shell--interaction-state 'question)
    (user-error "Question reply already in progress"))
  (if (and confirm (not (yes-or-no-p "Answer this question (no rejects)? ")))
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

(defvar evil-move-beyond-eol)
(defvar evil-local-mode)

(defun opencode-shell--configure-evil-buffer ()
  "Apply Evil policy local to an OpenCode transcript buffer."
  (unless (default-boundp 'evil-move-beyond-eol)
    (set-default 'evil-move-beyond-eol nil))
  (setq-local evil-move-beyond-eol t))

(defun opencode-shell--evil-open-below (count)
  "Reuse an empty composer from `Prompt>`; otherwise open below COUNT times."
  (interactive "p")
  (declare-function evil-insert-state "ext:evil-states")
  (declare-function evil-open-below "ext:evil-commands")
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
  (declare-function evil-delete-backward-char-and-join "ext:evil-commands")
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
  (declare-function evil-set-initial-state "ext:evil-core")
  (declare-function evil-define-key* "ext:evil-core")
  (evil-set-initial-state 'opencode-shell-mode 'normal)
  (evil-set-initial-state 'opencode-shell-sessions-mode 'normal)
  (evil-define-key* 'normal opencode-shell-sessions-mode-map (kbd "g") nil)
  (evil-define-key* 'normal opencode-shell-mode-map
    (kbd "RET") #'opencode-shell--submit
    (kbd "<return>") #'opencode-shell--submit
   (kbd "g r") #'opencode-shell--resync
   (kbd "g R") #'opencode-shell-restart
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

(defconst opencode-shell--switch-buffer-status-width 12
  "Display columns reserved for the leading status cell.")

(defconst opencode-shell--switch-buffer-status-gap 2
  "Display columns between the status cell and the buffer name.")

(defconst opencode-shell--switch-buffer-busy-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Single-cell spinner frames for a busy transcript.")

(defvar opencode-shell--switch-buffer-frame-index 0
  "Frame index used by the status cell; advanced only while the menu is open.")

(defvar opencode-shell--switch-buffer-session nil
  "Active switch-buffer menu token, or nil; stale ticks ignore themselves.")

(defun opencode-shell--switch-buffer-frame (kind)
  "Return the one-cell glyph for status KIND at the current frame."
  (pcase kind
    ('busy (aref opencode-shell--switch-buffer-busy-frames
                 (mod opencode-shell--switch-buffer-frame-index
                      (length opencode-shell--switch-buffer-busy-frames))))
    ('permission "!")
    ('question "?")
    (_ "○")))

(defun opencode-shell--buffer-list-status (buffer)
  "Return BUFFER's observed work status: permission, question, busy or idle.
Reads existing buffer-local lifecycle state; issues no requests."
  (with-current-buffer buffer
    (cond
     ((or opencode-shell--permissions
          (opencode-shell-interaction-active-p
           opencode-shell--interaction-state 'permission))
      'permission)
     ((or opencode-shell--questions-pending
          (opencode-shell-interaction-active-p
           opencode-shell--interaction-state 'question))
      'question)
     ((opencode-shell-state-polling-needed-p
       (mapcar #'opencode-shell--turn-status opencode-shell--turns)
       opencode-shell--submit-in-flight
       (not (opencode-shell--initial-hydration-complete-p)))
      'busy)
     (t 'idle))))

(defun opencode-shell--switch-buffer-status-prefix (kind)
  "Return a fixed-width leading status cell for KIND, then a gap.
The status cell is `opencode-shell--switch-buffer-status-width' display
columns and the whole prefix is that width plus the gap, so buffer names
start in the same column regardless of the rotating glyph."
  (let* ((label (pcase kind
                  ('permission "permission")
                  ('question "question")
                  ('busy "busy")
                  (_ "idle")))
         (text (format "%s %s" (opencode-shell--switch-buffer-frame kind) label))
         (pad (max 0 (- opencode-shell--switch-buffer-status-width
                        (string-width text))))
         (face (pcase kind
                 ('permission 'error)
                 ('question 'warning)
                 ('busy 'opencode-shell-waiting-face)
                 (_ 'shadow))))
    (concat (propertize (concat text (make-string pad ?\s)) 'face face)
            (make-string opencode-shell--switch-buffer-status-gap ?\s))))

(defun opencode-shell--switch-buffer-affixation (candidates)
  "Return an `:affixation-function' mapping names to CANDIDATES."
  (let ((blank (make-string (+ opencode-shell--switch-buffer-status-width
                               opencode-shell--switch-buffer-status-gap)
                            ?\s)))
    (lambda (names)
      (mapcar
       (lambda (name)
         (let ((buffer (cdr (assoc name candidates))))
           (list name
                 (if (buffer-live-p buffer)
                     (opencode-shell--switch-buffer-status-prefix
                      (opencode-shell--buffer-list-status buffer))
                   blank)
                 "")))
       names))))

(defun opencode-shell--switch-buffer-vertico-p ()
  "Return non-nil when Vertico is the active completion UI in this session."
  (and (bound-and-true-p vertico-mode) (fboundp 'vertico--exhibit)))

(defun opencode-shell--switch-buffer-active-p (session)
  "Return non-nil when SESSION still owns the currently active minibuffer."
  (and (eq session opencode-shell--switch-buffer-session)
       (let ((window (active-minibuffer-window)))
         (and (eq window (plist-get session :window))
              (buffer-live-p (plist-get session :buffer))
              (eq (window-buffer window) (plist-get session :buffer))))))

(defun opencode-shell--switch-buffer-statuses (candidates)
  "Return the current observed status list for CANDIDATES."
  (mapcar (lambda (candidate)
            (let ((buffer (cdr candidate)))
              (and (buffer-live-p buffer)
                   (opencode-shell--buffer-list-status buffer))))
          candidates))

(defun opencode-shell--switch-buffer-tick (session)
  "Advance the menu spinner for SESSION and request one idle redraw.
Repaints when any candidate is busy or when observed candidate statuses
changed since the last displayed snapshot; fully settled lists stay inert."
  (when (and (opencode-shell--switch-buffer-active-p session)
             (opencode-shell--switch-buffer-vertico-p))
    (let* ((candidates (plist-get session :candidates))
           (statuses (opencode-shell--switch-buffer-statuses candidates))
           (busy (memq 'busy statuses))
           (changed (not (equal statuses (plist-get session :statuses)))))
      (when (or busy changed)
        (when busy
          (setq opencode-shell--switch-buffer-frame-index
                (1+ opencode-shell--switch-buffer-frame-index)))
        (opencode-shell-async-enqueue
         (plist-get session :buffer) 'switch-buffer-frame nil
         (lambda ()
           (when (and (opencode-shell--switch-buffer-active-p session)
                      (opencode-shell--switch-buffer-vertico-p))
             (setf (plist-get session :statuses)
                   (opencode-shell--switch-buffer-statuses candidates))
             (vertico--exhibit))))))))

;;;###autoload
(defun opencode-shell-switch-buffer ()
  "Select and display a live OpenCode transcript buffer.
Candidates are shown by buffer name with a fixed-width status column, and a
busy spinner animates while the Vertico menu stays open."
  (interactive)
  (let* ((buffers (seq-filter
                   (lambda (buffer)
                      (with-current-buffer buffer
                        (derived-mode-p 'opencode-shell-mode)))
                   (buffer-list)))
         (candidates (mapcar (lambda (buffer) (cons (buffer-name buffer) buffer))
                             buffers)))
    (unless candidates (user-error "No OpenCode buffers"))
    (let* ((opencode-shell--switch-buffer-frame-index 0)
           (session (list :window nil :buffer nil :candidates nil
                          :statuses nil :animate nil))
           (opencode-shell--switch-buffer-session session)
           setup)
      (setq setup
            (lambda ()
              (when-let* ((window (active-minibuffer-window)))
                (unless (plist-get session :buffer)
                  (let ((buffer (window-buffer window)))
                    (setf (plist-get session :window) window
                          (plist-get session :buffer) buffer
                          (plist-get session :candidates) candidates
                          (plist-get session :statuses)
                          (opencode-shell--switch-buffer-statuses candidates)
                          (plist-get session :animate)
                          (opencode-shell--switch-buffer-vertico-p))
                    (when (plist-get session :animate)
                      (opencode-shell-async-subscribe-animation
                       buffer opencode-shell-animation-interval
                       (lambda () (opencode-shell--switch-buffer-tick session)))))))))
      (unwind-protect
          (progn
            (add-hook 'minibuffer-setup-hook setup)
            (let* ((completion-extra-properties
                    (plist-put (copy-sequence completion-extra-properties)
                               :affixation-function
                               (opencode-shell--switch-buffer-affixation candidates)))
                   (selected
                    (cdr (assoc (completing-read "OpenCode shell: " candidates nil t)
                                candidates))))
              (unless (buffer-live-p selected)
                (user-error "Selected OpenCode buffer was closed"))
              (switch-to-buffer selected)))
        (setq opencode-shell--switch-buffer-session nil)
        (remove-hook 'minibuffer-setup-hook setup)
        (when-let* ((buffer (plist-get session :buffer)))
          (opencode-shell-async-unsubscribe-animation buffer)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode-shell-async-cancel))))))))

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
             (when-let* ((profile
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
  "Return the unique match for minibuffer TEXT in CANDIDATES."
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
        (when-let* ((location
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
                     (when-let* ((location
                                 (opencode-shell--read-session-location entries)))
                       (opencode-shell--open-sessions
                        (car location) (cdr location) t))
                  (quit nil))))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'opencode-shell-sessions-mode)
                     (seq-some (lambda (profile)
                                 (equal (opencode-shell--profile-key profile)
                                        (opencode-shell--profile-key opencode-shell--profile))) profiles))
            (remember opencode-shell--profile opencode-shell--directory
                      most-positive-fixnum t))))
      (dolist (location opencode-shell--recent-session-locations)
        (when-let* ((candidate-profile
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


(defun opencode-shell--migrate-live-buffer ()
  "Upgrade in-memory caches and gutter overlays in the current transcript buffer."
  (when (derived-mode-p 'opencode-shell-mode)
    (opencode-shell--ensure-message-cache)
    (clrhash opencode-shell--parent-assistant-ids)
    (dolist (id opencode-shell--message-order)
      (when-let* ((envelope (gethash id opencode-shell--message-envelopes)))
        (let* ((compacted (opencode-shell--compact-envelope envelope))
               (info (opencode-shell--get compacted 'info))
               (role (format "%s" (or (opencode-shell--get info 'role) "")))
               (parent (or (opencode-shell--get info 'parentID)
                           (opencode-shell--get info 'parentId))))
          (unless (eq envelope compacted)
            (puthash id compacted opencode-shell--message-envelopes))
          (when (and (equal role "assistant") parent)
            (let ((existing-ids (gethash parent opencode-shell--parent-assistant-ids)))
              (unless (member id existing-ids)
                (puthash parent (append existing-ids (list id))
                         opencode-shell--parent-assistant-ids)))))))
    (setq opencode-shell--message-order-tail (last opencode-shell--message-order))
    (let (rebuild-gutters)
      (dolist (turn opencode-shell--turns)
        (setf (opencode-shell--turn-assistant-messages turn)
              (mapcar (lambda (entry)
                        (cons (car entry)
                              (opencode-shell--compact-envelope (cdr entry))))
                      (opencode-shell--turn-assistant-messages turn))
              (opencode-shell--turn-parts turn)
              (mapcar #'opencode-shell--compact-part
                      (opencode-shell--turn-parts turn)))
        (when (hash-table-p opencode-shell--turn-gutters)
          (let ((entry (gethash turn opencode-shell--turn-gutters)))
            (when (or (> (length (plist-get entry :user)) 2)
                      (> (length (plist-get entry :response)) 2))
              (setq rebuild-gutters t)))))
      (when rebuild-gutters
        (opencode-shell--render-turns t)))))

(defun opencode-shell--migrate-live-buffers ()
  "Upgrade in-memory caches and gutter overlays across all live transcript buffers."
  (when (and load-file-name (not (fboundp 'opencode-shell-event-coalesce)))
    (let ((dir (file-name-directory load-file-name)))
      (dolist (name '("opencode-shell-sse.el" "opencode-shell-event.el"))
        (let ((path (expand-file-name name dir)))
          (when (file-exists-p path)
            (load path nil nil t))))))
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'opencode-shell-mode)
          (opencode-shell--migrate-live-buffer))))))

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
           (names '("opencode-shell-render.el" "opencode-shell-sse.el"
                    "opencode-shell-event.el" "opencode-shell-async.el"
                    "opencode-shell-state.el" "opencode-shell-interaction.el"
                    "opencode-shell-response.el" "opencode-shell-recovery.el"
                    "opencode-shell-transport.el" "opencode-shell.el"))
           (sources (and directory
                         (mapcar (lambda (name) (expand-file-name name directory))
                                 names))))
      (unless (and sources (seq-every-p #'file-exists-p sources))
        (user-error "Cannot locate OpenCode Shell source files"))
      (dolist (source sources) (load source nil nil t))
    (when-let* ((setting (locate-library "opencode-shell-setting")))
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
            (opencode-shell--migrate-live-buffer)
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
(opencode-shell--migrate-live-buffers)

(dolist (command opencode-shell--mode-commands)
  (put command 'completion-predicate #'ignore))

(defun opencode-shell--activate-evil ()
  "Activate Evil integration and clean up legacy buffer hooks when Evil is loaded."
  (when (featurep 'evil)
    (opencode-shell--setup-evil)
    (remove-hook 'opencode-shell-mode-hook
                 (intern "opencode-shell--enable-evil-composer-hook"))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'opencode-shell-mode)
          (remove-hook 'evil-insert-state-entry-hook
                       (intern "opencode-shell--evil-move-to-composer") t))))))

(opencode-shell--activate-evil)
(add-hook 'evil-mode-hook #'opencode-shell--activate-evil)
(add-hook 'opencode-shell-mode-hook #'opencode-shell--activate-evil)
(add-hook 'opencode-shell-sessions-mode-hook #'opencode-shell--activate-evil)

(provide 'opencode-shell)
;;; opencode-shell.el ends here
