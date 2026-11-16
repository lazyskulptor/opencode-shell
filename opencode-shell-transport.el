;;; opencode-shell-transport.el --- HTTP/SSH transport layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Server process lifecycle, HTTP request dispatch, and bounded SSH retry
;; orchestration.  Two glue functions, `opencode-shell--ssh-disconnected' and
;; `opencode-shell--ssh-reconcile', stay in opencode-shell.el because they
;; mutate buffer-local UI state directly; this file calls them via the
;; forward declarations below.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'json)
(require 'opencode-shell-async)
(require 'opencode-shell-recovery)

;; Defined in opencode-shell.el, which always `require's this file before
;; defining them.  By the time any callback below actually runs, loading is
;; complete.  Declared only so the byte-compiler doesn't flag these as typos.
(defvar opencode-shell--profile)
(defvar opencode-shell--base-url)
(defvar opencode-shell--directory)
(defvar opencode-shell--generation)
(defvar opencode-shell-base-url)
(defvar opencode-shell-auth-function)
(defvar opencode-shell-auth-source-function)
(defvar opencode-shell-log-requests)
(defvar url-http-no-retry)
(declare-function opencode-shell--ssh-disconnected "opencode-shell")
(declare-function opencode-shell--ssh-reconcile "opencode-shell")
(declare-function opencode-shell--default-profile "opencode-shell")
(declare-function opencode-shell--profile-remote-p "opencode-shell")
(declare-function opencode-shell--read-profile "opencode-shell")
(declare-function opencode-shell--resolve-or-read-profile "opencode-shell")
(declare-function opencode-shell--profile-name "opencode-shell")
(declare-function opencode-shell--server-lifecycle-config "opencode-shell")
(declare-function opencode-shell--validate-server-profile "opencode-shell")
(declare-function opencode-shell--log "opencode-shell")

(defvar opencode-shell--servers (make-hash-table :test #'equal))
(defconst opencode-shell--process-tail-limit 4096)
(defvar opencode-shell--request-log-counter 0)
(defvar-local opencode-shell--request-error-notified nil
  "Non-nil once a request failure was reported, until the next success.
Collapses the burst of identical transport failures a dropped network or a
machine returning from sleep produces into a single message.")
(defconst opencode-shell--url-retrieve-timeout 20
  "Seconds to wait for a `url-retrieve' response before treating it as failed.
A connection that hangs silently -- neither succeeding nor erroring, the
common symptom right after a network change leaves a tunnel half-dead --
otherwise never completes: its callback never fires, so it never surfaces as
a transport error the SSH recovery machinery could act on, and its buffer
and process leak indefinitely.  Callers whose request can legitimately take
longer, such as one that blocks on an LLM completion, should `let'-bind this
dynamically-scoped constant around their call, the same way this file
already binds `url-request-method' and friends around `url-retrieve'.")
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
(defun opencode-shell--server-health-callback (profile callback status)
  "Handle a health response for PROFILE and report readiness to CALLBACK.
The url response buffer is killed off-stack: calling `kill-buffer' directly in
this `url-retrieve' sentinel synchronously pumps other pending health
connections' sentinels, which re-enter this callback and can recurse until the
Lisp stack is exhausted when the endpoint is unreachable (e.g. a dead SSH
tunnel refusing every probe).  Deferring the kill breaks that chain."
  (let ((response (current-buffer))
        (ready (and (not (plist-get status :error))
                    (<= 200 (or (bound-and-true-p url-http-response-status) 0)
                        299))))
    (unwind-protect
        (funcall callback ready profile)
      (when (buffer-live-p response)
        (opencode-shell--terminate-url-buffer-process response)
        (run-at-time 0 nil #'opencode-shell--kill-url-buffer response)))))

(defun opencode-shell--auth-header (profile)
  "Return PROFILE's authorization header pair, or nil."
  (when (or opencode-shell-auth-function opencode-shell-auth-source-function)
    (when-let ((token (if opencode-shell-auth-function
                          (funcall opencode-shell-auth-function)
                        (funcall opencode-shell-auth-source-function profile))))
      (cons (or (plist-get profile :auth-header) "Authorization") token))))

(defun opencode-shell--server-ready (profile callback)
  "Invoke CALLBACK with PROFILE when its health endpoint is ready."
  (let (;; `opencode-shell-recovery-watch-attempt' already bounds how long a
        ;; health check may run (`opencode-shell-recovery--attempt-timeout');
        ;; keep this well above it so it is a backstop, never a competing
        ;; timeout that fires first and leaves the real one as dead code.
        (opencode-shell--url-retrieve-timeout
         (* 3 opencode-shell-recovery--attempt-timeout))
        (url-request-method "GET")
        (url-request-extra-headers
         (append '(("Accept" . "application/json"))
                 (when-let ((header (opencode-shell--auth-header profile)))
                   (list header)))))
    (opencode-shell--url-retrieve-no-query
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
      ;; A tunnel that merely looks "alive" (process-live-p) but no longer
      ;; routes traffic (e.g. right after a network change, well before SSH's
      ;; own keepalive notices and exits it) would otherwise never get
      ;; replaced: `--start-server' only spawns fresh when the owned process
      ;; has actually exited, so every retry would just re-probe the same
      ;; dead tunnel forever.  Kill it here once it has failed more than once
      ;; (tolerating a single transient blip) so the next retry spawns fresh.
      (when (and (processp process) (process-live-p process)
                 (or (plist-get state :starting)
                     (and ssh (>= (opencode-shell-recovery-failure-count key)
                                  opencode-shell-recovery--blips-tolerated-before-kill))))
        (delete-process process))
      (if ssh
          (progn
            (unless (and (processp process) (process-live-p process))
              (setf (plist-get state :process) nil
                    (plist-get state :owned) nil))
            (setf (plist-get state :checking) nil
                  (plist-get state :starting) nil
                  (plist-get state :attempt) nil
                  (plist-get state :callbacks) nil
                  (plist-get state :retry-callbacks) nil
                  (plist-get state :failure-callbacks) nil)
            (puthash key state opencode-shell--servers)
            (opencode-shell--ssh-disconnected profile)
            (dolist (failure failures)
              (cond ((functionp failure) (funcall failure))
                    ((and (consp failure) (functionp (cadr failure)))
                     (funcall (cadr failure)))))
            (unless (plist-get (gethash key opencode-shell--servers) :owned)
              (remhash key opencode-shell--servers)))
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
  "Poll PROFILE health until DEADLINE for ATTEMPT.
DEADLINE is only re-checked once each `opencode-shell--server-ready' call
settles, so a single hung health probe can delay the eventual \"startup timed
out\" failure past DEADLINE by up to that call's own timeout -- bounded, but
not exact."
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
  "Reconnect PROFILE's transport without touching :restart-command.
A foreign process cannot be restarted, but an owned process (an SSH tunnel
for a forwarded profile, or the server itself for a local one) is torn down
and respawned, and nothing currently running (e.g. one already force-killed
by automatic recovery) is simply started fresh.  See
`opencode-shell--restart-service-process' for actually restarting the
service behind a :restart-command profile."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
         (key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers))
         (process (plist-get state :process)))
    (unless (or (not process) (plist-get state :owned))
      (user-error "OpenCode server is not owned by this client"))
    (when (plist-get state :restart-profile)
      (user-error "OpenCode server restart is already in progress"))
    (cond
     ((and process (process-live-p process))
      (puthash key (plist-put state :restart-profile profile) opencode-shell--servers)
      (delete-process process))
     (t
      (remhash key opencode-shell--servers)
      (opencode-shell--start-server profile)))))

(defun opencode-shell--restart-service-process (&optional profile)
  "Restart the service behind PROFILE via its configured :restart-command.
It must fully stop the server (e.g. via pkill) and return promptly; this
client then starts a fresh instance via :start-command once the kill command
exits.  This works even when the current server is not owned by this
client.  Unlike `opencode-shell--restart-server', this always invokes
:restart-command and so always attempts to restart the actual service, not
just this client's local transport to it."
  (let* ((profile (or profile opencode-shell--profile (opencode-shell--read-profile)))
         (key (opencode-shell--server-key profile))
         (state (gethash key opencode-shell--servers))
         (process (plist-get state :process))
         (restart-cmd (plist-get profile :restart-command)))
    (unless restart-cmd
      (user-error "Profile has no :restart-command; use `opencode-shell-restart' to just reconnect"))
    (when (plist-get state :restart-profile)
      (user-error "OpenCode server restart is already in progress"))
    (puthash key (plist-put state :restart-profile profile) opencode-shell--servers)
    (make-process
     :name (format "opencode-restart-%s" key)
     :command restart-cmd
     :noquery t
     :sentinel (lambda (_p _event)
                 (when (plist-get (gethash key opencode-shell--servers) :restart-profile)
                   (when (and process (process-live-p process))
                     (delete-process process))
                   (remhash key opencode-shell--servers)
                   (opencode-shell--start-server profile))))))

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

(defun opencode-shell--http-response-body ()
  "Return the raw HTTP response body string after headers in current buffer."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "Malformed HTTP response"))
  (buffer-substring-no-properties (point) (point-max)))

(defun opencode-shell--json-parse-body (raw)
  "Decode HTTP response body string RAW as JSON, or nil when empty."
  (unless (string-empty-p raw)
    (json-parse-string (if (multibyte-string-p raw)
                           raw
                         (decode-coding-string raw 'utf-8 t))
                       :object-type 'alist :array-type 'list
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
    (and (opencode-shell--profile-remote-p profile)
         (consp command)
         (stringp (car command))
         (equal (file-name-nondirectory (car command)) "ssh")
         (member (url-host url) '("localhost" "127.0.0.1" "::1")))))

(defun opencode-shell--ssh-retry (profile &optional on-ready _needed)
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
         (opencode-shell--ssh-disconnected profile))
       t))))

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
    (profile key origin generation method path callback body params error-callback
             &optional timeout)
  "Defer a read-only request until KEY's existing SSH start succeeds.
TIMEOUT, when given, is the caller's `opencode-shell--url-retrieve-timeout'
captured before deferring, re-applied once the deferred request actually
dispatches so it survives outliving that dynamic extent."
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
                           (let ((opencode-shell--url-retrieve-timeout
                                  (or timeout opencode-shell--url-retrieve-timeout)))
                             (opencode-shell--request-direct
                               method path callback body params error-callback))))))
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
       (puthash key state opencode-shell--servers))))

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
  "Send request after an SSH-backed PROFILE has verified its forwarding.
Captures the caller's current `opencode-shell--url-retrieve-timeout' as a
lexical value up front so a caller that `let'-binds a longer timeout (e.g.
`opencode-shell-rename-session') keeps it even when the actual request is
dispatched later from an SSH-readiness callback, outside that dynamic
extent."
  (let* ((profile (or opencode-shell--profile (opencode-shell--default-profile)))
         (key (opencode-shell--server-key profile))
         (origin (current-buffer))
         (generation opencode-shell--generation)
         (timeout opencode-shell--url-retrieve-timeout))
    (if (not (and (opencode-shell--ssh-forwarded-profile-p profile)
                  (not (opencode-shell-recovery-ready-p key))))
        (opencode-shell--request-direct method path callback body params error-callback)
      (cond
       ((opencode-shell-recovery-exhausted-p key)
        (when error-callback (funcall error-callback)))
       ((opencode-shell-recovery-offline-p key)
        (opencode-shell--ssh-wait-request
         profile key origin generation method path callback body params error-callback timeout))
       ((equal method "GET")
        (let ((starting (let ((state (gethash key opencode-shell--servers)))
                          (or (plist-get state :checking)
                              (plist-get state :starting)))))
          (opencode-shell--ssh-wait-request
           profile key origin generation method path callback body params error-callback timeout)
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
                 (let ((opencode-shell--url-retrieve-timeout timeout))
                   (opencode-shell--request-direct method path callback body params
                                                   error-callback))))))
         (lambda ()
           (when (buffer-live-p origin)
             (with-current-buffer origin
               (when (= generation opencode-shell--generation)
                 (opencode-shell--ssh-disconnected profile)
                 (when error-callback (funcall error-callback))))))
         t))))))

(defun opencode-shell--note-request-error (format-string &rest args)
  "Report a request failure once per outage in the current buffer.
Shows FORMAT-STRING with ARGS via `message' only when no failure has been
reported since the last successful request, so a dropped network or a machine
waking from sleep — which fails every in-flight and polling request at once —
produces a single notice instead of a flood.  A successful request resets
`opencode-shell--request-error-notified' so a later outage notifies again."
  (unless opencode-shell--request-error-notified
    (setq opencode-shell--request-error-notified t)
    (apply #'message format-string args)))

(defun opencode-shell--silence-url-buffer-process (buffer)
  "Disable retry and exit query flags on `url-retrieve' BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local url-http-no-retry t
                  buffer-undo-list t))
    (when-let ((process (get-buffer-process buffer)))
      (set-process-query-on-exit-flag process nil)
      (let ((sentinel (process-sentinel process)))
        (when (and sentinel
                   (not (eq sentinel (process-get process 'opencode-shell-sentinel))))
          (let ((wrapped
                 (lambda (proc event)
                   (unwind-protect
                       (funcall sentinel proc event)
                     (opencode-shell--silence-url-buffer-process buffer)))))
            (process-put process 'opencode-shell-sentinel wrapped)
            (set-process-sentinel process wrapped)))))))

(defun opencode-shell--terminate-url-buffer-process (buffer)
  "Silence and delete any process attached to `url-retrieve' BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local url-http-no-retry t
                  buffer-undo-list t))
    (let (process)
      (while (setq process (get-buffer-process buffer))
        (set-process-query-on-exit-flag process nil)
        (set-process-filter process #'ignore)
        (set-process-sentinel process #'ignore)
        (delete-process process)))))

(defun opencode-shell--kill-url-buffer (buffer)
  "Kill `url-retrieve' BUFFER without exit prompts or sentinel retries."
  (when (buffer-live-p buffer)
    (opencode-shell--terminate-url-buffer-process buffer)
    (let (kill-buffer-query-functions)
      (kill-buffer buffer))))

(defun opencode-shell--url-retrieve-no-query (url callback &optional cbargs silent inhibit-cookies)
  "Call `url-retrieve' guarded against query-on-exit and a silent hang.
Disables query-on-exit for the response process (see
`opencode-shell--request-direct') and, if CALLBACK has not fired within
`opencode-shell--url-retrieve-timeout' seconds, invokes it once with a
synthesized `:error' status so a hang surfaces the same way an explicit
connection error would, instead of leaking its buffer and process forever.
SETTLED guards against both firing: `run-at-time' is not guaranteed non-nil
(tests commonly stub it to return nil), so TIMER itself cannot double as
that guard."
  (let (settled response-buffer timer)
    (setq timer
          (run-at-time
           opencode-shell--url-retrieve-timeout nil
           (lambda ()
             (unless settled
               (setq settled t)
               (when (buffer-live-p response-buffer)
                 (opencode-shell--terminate-url-buffer-process response-buffer)
                 (with-current-buffer response-buffer
                   (funcall callback '(:error "opencode-shell: request timed out"))))))))
    (setq response-buffer
          (let ((default-directory temporary-file-directory)
                (file-name-handler-alist nil))
            (url-retrieve
             url
             (lambda (status &rest args)
               (unless settled
                 (setq settled t)
                 (when (timerp timer) (cancel-timer timer))
                 (apply callback status args)))
             cbargs silent inhibit-cookies)))
    (opencode-shell--silence-url-buffer-process response-buffer)
    response-buffer))

(defun opencode-shell--request-direct-async
    (path origin generation request-id key ssh transport-epoch
          callback error-callback params)
  "Fetch PATH in a subprocess and apply CALLBACK under the same guards as
`opencode-shell--request-direct': generation, buffer liveness, SSH epoch, and
the deferred keyed application of the parsed value."
  (opencode-shell--fetch-snapshot-async
   path
   (lambda (value)
     (when (and (not (and ssh (not (= transport-epoch
                                     (opencode-shell-recovery-epoch key)))))
                (buffer-live-p origin)
                (with-current-buffer origin
                  (= generation opencode-shell--generation)))
       (with-current-buffer origin
         (opencode-shell-async-resume-runtime key)
         (opencode-shell-async-enqueue
          origin (list 'request request-id) generation
          (lambda ()
            (setq opencode-shell--request-error-notified nil)
            (funcall callback value))))))
   (lambda ()
     (when (and (buffer-live-p origin)
                (with-current-buffer origin
                  (= generation opencode-shell--generation)))
       (with-current-buffer origin
         (unless ssh (opencode-shell-async-pause-runtime key))
         (when error-callback
           (opencode-shell-async-enqueue
            origin (list 'request-error request-id) generation error-callback)))))
   params))

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
    (if (and (equal method "GET")
             (opencode-shell--large-snapshot-path-p path)
             (opencode-shell--async-fetch-available-p))
        (opencode-shell--request-direct-async
         path origin request-generation request-id key ssh transport-epoch
         callback error-callback params)
      (opencode-shell--url-retrieve-no-query
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
                       (if (opencode-shell--ssh-forwarded-profile-p
                            (or opencode-shell--profile
                                (opencode-shell--default-profile)))
                           (opencode-shell--ssh-disconnected
                            (or opencode-shell--profile
                                (opencode-shell--default-profile)))
                         (opencode-shell-async-pause-runtime key))
                       (when opencode-shell-log-requests
                          (opencode-shell--log "OpenCode API #%d ← transport-error %.2fs [%s %s]"
                                              request-id (- (float-time) started) method path))
                        (unless (opencode-shell--ssh-forwarded-profile-p
                                 (or opencode-shell--profile
                                     (opencode-shell--default-profile)))
                          (opencode-shell--note-request-error
                           "OpenCode: %s" (opencode-shell--bounded-error err)))
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
                               (opencode-shell--note-request-error
                                "OpenCode: HTTP %s request failed" code)
                               (when error-callback
                                 (opencode-shell-async-enqueue
                                  origin (list 'request-error request-id)
                                   request-generation error-callback))))
                        (when (and (buffer-live-p origin)
                                   (with-current-buffer origin
                                     (= request-generation opencode-shell--generation)))
                          (let ((raw-body (unless (= code 204)
                                            (opencode-shell--http-response-body))))
                            (with-current-buffer origin
                              (opencode-shell-async-resume-runtime key)
                              (opencode-shell-async-enqueue
                               origin (list 'request request-id)
                               request-generation
                               (lambda ()
                                 (condition-case err
                                     (let ((value (and raw-body
                                                       (opencode-shell--json-parse-body raw-body))))
                                       (when (and opencode-shell-log-requests
                                                  (not quiet-success))
                                         (opencode-shell--log "OpenCode API #%d ← HTTP %s %.2fs [%s %s]"
                                                              request-id code (- (float-time) started) method path))
                                       ;; Success clears the once-per-outage notice.
                                       (setq opencode-shell--request-error-notified nil)
                                       (funcall callback value))
                                   (error
                                    (when opencode-shell-log-requests
                                      (opencode-shell--log "OpenCode API #%d ← decode-error %.2fs [%s %s]"
                                                           request-id (- (float-time) started) method path))
                                    (opencode-shell--note-request-error
                                     "OpenCode: %s" (opencode-shell--bounded-error
                                                     (error-message-string err)))
                                    (when error-callback
                                      (funcall error-callback)))))))))))
                 (error
                   (when (and (buffer-live-p origin)
                              (with-current-buffer origin
                                (= request-generation opencode-shell--generation)))
                    (with-current-buffer origin
                      (when opencode-shell-log-requests
                         (opencode-shell--log "OpenCode API #%d ← decode-error %.2fs [%s %s]"
                                             request-id (- (float-time) started) method path))
                       (opencode-shell--note-request-error
                        "OpenCode: %s" (opencode-shell--bounded-error
                                         (error-message-string err)))
                       (when error-callback
                         (opencode-shell-async-enqueue
                          origin (list 'request-error request-id)
                           request-generation error-callback))))))))
            (opencode-shell--kill-url-buffer response))))
     nil t t))))
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
  "Reconnect PROFILE's owned transport, or the current buffer's profile.
`opencode-shell--resync' (bound to `g r') only refreshes session data; while
offline it also retries the existing SSH transport through the ordinary
bounded-retry path, but never force-kills a tunnel that still looks alive.
This command tears down and respawns the transport unconditionally -- use it
when an SSH tunnel is alive but no longer functional, e.g. after a network
change, without waiting for the automatic retry budget.  It never invokes a
profile's :restart-command, so it never attempts to restart the actual
service -- only this client's connection to it.  Use
`opencode-shell-restart-service' to restart the service itself."
  (interactive (list (opencode-shell--resolve-or-read-profile opencode-shell--profile)))
  (opencode-shell--restart-server (opencode-shell--resolve-or-read-profile profile)))

;;;###autoload
(defun opencode-shell-restart-service (profile)
  "Restart the service behind PROFILE via its configured :restart-command.
Unlike `opencode-shell-restart', which only ever reconnects this client's
existing transport, this actually restarts the service on the other end
(e.g. killing and relaunching a remote opencode process over SSH) -- a
riskier, slower operation that only applies to profiles with
:restart-command configured; errors otherwise."
  (interactive (list (opencode-shell--resolve-or-read-profile opencode-shell--profile)))
  (opencode-shell--restart-service-process (opencode-shell--resolve-or-read-profile profile)))

(defcustom opencode-shell-async-snapshot-fetch nil
  "When non-nil, fetch large transcript snapshots in a subprocess.
Keeps the multi-MB `GET /session/:id/message' receive off the main thread by
running curl out of process and parsing only its finished output.  Off by
default because it bypasses the `url-retrieve' path; enable it for very large
sessions where the receive burst is felt."
  :type 'boolean :group 'opencode-shell)

(defun opencode-shell--async-fetch-available-p ()
  "Return non-nil when the subprocess snapshot fetch path can be used."
  (and opencode-shell-async-snapshot-fetch (executable-find "curl")))

(defun opencode-shell--large-snapshot-path-p (path)
  "Return non-nil when PATH is a large transcript snapshot worth offloading."
  (and (stringp path) (string-match-p "/session/[^/]+/message\\'" path)))

(defun opencode-shell--fetch-snapshot-async (path callback &optional error-callback params)
  "Fetch large JSON snapshot PATH in a subprocess and call CALLBACK with the
decoded value on completion, or ERROR-CALLBACK on failure. Falls back to
`opencode-shell--request-direct' when the subprocess path is unavailable, so
`url-retrieve' stubs and non-curl environments keep working unchanged."
  (if (not (opencode-shell--async-fetch-available-p))
      (opencode-shell--request-direct "GET" path callback nil params error-callback)
    (let* ((profile (or opencode-shell--profile (opencode-shell--default-profile)))
           (auth (opencode-shell--auth-header profile))
           (header-file (and auth (make-temp-file "opencode-header-")))
           (buffer (generate-new-buffer " *oc-async-fetch*")))
      (when header-file
        (with-temp-file header-file
          (insert (format "%s: %s\n" (car auth) (cdr auth)))))
      (make-process
       :name "opencode-async-fetch" :buffer buffer :noquery t
       :command (append (list "curl" "-fsS" (opencode-shell--url path params))
                        (and header-file (list "-H" (concat "@" header-file))))
       :sentinel
       (lambda (proc event)
         (unwind-protect
             (if (string-prefix-p "finished" event)
                 (let ((body (with-current-buffer (process-buffer proc)
                               (buffer-string))))
                   (condition-case err
                       (funcall callback (opencode-shell--json-parse-body body))
                     (error (when error-callback (funcall error-callback)))))
               (when error-callback (funcall error-callback)))
           (kill-buffer (process-buffer proc))
           (when header-file (ignore-errors (delete-file header-file)))))))))

(provide 'opencode-shell-transport)
;;; opencode-shell-transport.el ends here
