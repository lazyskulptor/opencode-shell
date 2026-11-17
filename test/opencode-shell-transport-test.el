;;; opencode-shell-transport-test.el --- Transport layer tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'opencode-shell)
(require 'opencode-shell-recovery)

(defconst opencode-shell-test--local-profile
  '(:name "local" :base-url "http://127.0.0.1:7777/"
    :start-command ("opencode" "serve") :server-directory "/tmp"
    :startup-timeout 2 :auth-source (:host "localhost" :port 7777)))

(defconst opencode-shell-test--remote-profile
  '(:name "remote" :base-url "https://code.example.test"
    :directory "/ssh:code.example.test:/home/test/"
    :start-command ("opencode" "serve")))

(ert-deftest opencode-shell-local-requests-bypass-url-proxy ()
  (let ((url-proxy-services '(("http" . "proxy.example:3128"))) seen)
    (with-temp-buffer
      (setq-local opencode-shell--profile opencode-shell-test--local-profile)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (setq seen url-proxy-services))))
        (opencode-shell--request "GET" "/question" #'ignore)))
    (should-not seen)))


(ert-deftest opencode-shell-url-retrieve-no-query-disables-process-query-on-exit ()
  "The shared `url-retrieve' wrapper must disable its response process's query."
  (let* ((response-buffer (generate-new-buffer " *fake-http*"))
         (process (make-network-process
                   :name "opencode-test-fake-http" :buffer response-buffer
                   :family 'ipv4 :server t :host 'local :service t)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process t)
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (&rest _) response-buffer)))
            (opencode-shell--url-retrieve-no-query "http://example.invalid" #'ignore))
          (should-not (process-query-on-exit-flag process))
          (should (buffer-local-value 'url-http-no-retry response-buffer)))
      (delete-process process)
      (kill-buffer response-buffer))))


(ert-deftest opencode-shell-url-retrieve-no-query-silences-sentinel-replacement-and-kill ()
  "Sentinel-replaced retry processes and buffer kills must never prompt."
  (let* ((response-buffer (generate-new-buffer " *fake-retry-http*"))
         (proc1 (make-network-process
                 :name "opencode-test-http-1" :buffer response-buffer
                 :family 'ipv4 :server t :host 'local :service t))
         proc2 prompted)
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc1 t)
          (set-process-sentinel
           proc1
           (lambda (_proc _event)
             (setq proc2 (make-network-process
                          :name "opencode-test-http-2" :buffer response-buffer
                          :family 'ipv4 :server t :host 'local :service t))
             (set-process-query-on-exit-flag proc2 t)))
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (&rest _) response-buffer))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq prompted t) nil)))
            (opencode-shell--url-retrieve-no-query "http://example.invalid" #'ignore)
            (funcall (process-sentinel proc1) proc1 "connection broken")
            (should (processp proc2))
            (should-not (process-query-on-exit-flag proc2))
            (opencode-shell--kill-url-buffer response-buffer)
            (should-not prompted)
            (should-not (buffer-live-p response-buffer))))
      (when (and proc1 (process-live-p proc1)) (delete-process proc1))
      (when (and proc2 (process-live-p proc2)) (delete-process proc2))
      (when (buffer-live-p response-buffer)
        (let (kill-buffer-query-functions) (kill-buffer response-buffer))))))


(ert-deftest opencode-shell-ssh-forwarded-profile-infers-remote-from-tramp-directory ()
  "SSH port-forward profiles with TRAMP `:directory' need not repeat `:remote t'."
  (let ((profile '(:name "tramp-ssh"
                   :base-url "http://127.0.0.1:4099"
                   :directory "/ssh:dev@example.test:/home/dev"
                   :start-command ("ssh" "-N" "-L" "4099:127.0.0.1:4099" "dev@example.test"))))
    (should (opencode-shell--ssh-forwarded-profile-p profile))))


(ert-deftest opencode-shell-server-ready-uses-no-query-retrieve ()
  "Health polling must go through the shared no-query `url-retrieve' wrapper."
  (let (used)
    (cl-letf (((symbol-function 'opencode-shell--url-retrieve-no-query)
               (lambda (&rest args) (setq used args) nil)))
      (opencode-shell--server-ready opencode-shell-test--local-profile #'ignore))
    (should used)))


(ert-deftest opencode-shell-url-retrieve-no-query-times-out-hung-connection ()
  "A connection that never calls back must still surface as a transport error."
  (let* ((response-buffer (generate-new-buffer " *fake-hung-http*"))
         received timer-fn)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (&rest _) response-buffer))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat fn &rest _) (setq timer-fn fn) nil)))
            (opencode-shell--url-retrieve-no-query
             "http://example.invalid"
             (lambda (status) (push status received))))
          (should-not received)
          (funcall timer-fn)
          (should (= (length received) 1))
          (should (plist-get (car received) :error)))
      (when (buffer-live-p response-buffer) (kill-buffer response-buffer)))))


(ert-deftest opencode-shell-url-retrieve-no-query-settles-once-on-real-completion ()
  "A normal completion before the timeout must not also fire a synthetic error."
  (let* ((response-buffer (generate-new-buffer " *fake-ok-http*"))
         received timer-fn real-callback)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (_url callback &rest _)
                       (setq real-callback callback)
                       response-buffer))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat fn &rest _) (setq timer-fn fn) nil)))
            (opencode-shell--url-retrieve-no-query
             "http://example.invalid"
             (lambda (status) (push status received))))
          (funcall real-callback '(:ok t))
          (funcall timer-fn)
          (should (= (length received) 1))
          (should (equal (car received) '(:ok t))))
      (when (buffer-live-p response-buffer) (kill-buffer response-buffer)))))


(ert-deftest opencode-shell-request-disables-process-query-on-exit ()
  "A connection that never calls back must not nag to confirm a kill on exit."
  (let* ((response-buffer (generate-new-buffer " *fake-http*"))
         (process (make-network-process
                   :name "opencode-test-fake-http" :buffer response-buffer
                   :family 'ipv4 :server t :host 'local :service t)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process t)
          (with-temp-buffer
            (setq-local opencode-shell--profile opencode-shell-test--local-profile)
            (cl-letf (((symbol-function 'url-retrieve)
                       (lambda (&rest _) response-buffer)))
              (opencode-shell--request "GET" "/question" #'ignore)))
          (should-not (process-query-on-exit-flag process)))
      (delete-process process)
      (kill-buffer response-buffer))))


(ert-deftest opencode-shell-http-status-and-bounded-errors ()
  (should (<= (string-width (opencode-shell--bounded-error
                             (concat "bad\n" (make-string 500 ?x)))) 300))
  (let (called messages)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_ callback &rest _)
                 (with-temp-buffer
                   (insert "HTTP/1.1 204 No Content\r\n\r\n")
                   (setq-local url-http-response-status 204)
                   (funcall callback nil))))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (opencode-shell--request "GET" "/ok" (lambda (value) (setq called (list value))))
      (opencode-shell-async-drain (current-buffer))
      (should (equal called '(nil)))
      (setq called nil)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_ callback &rest _)
                   (with-temp-buffer
                     (insert "HTTP/1.1 300 Multiple\r\n\r\nnot success")
                     (setq-local url-http-response-status 300)
                     (funcall callback nil)))))
        (opencode-shell--request "GET" "/bad" (lambda (_) (setq called t))))
      (opencode-shell-async-drain (current-buffer))
      (should-not called)
      (should (string-match-p "HTTP 300 request failed" (car messages)))
      (should-not (seq-some (lambda (text) (string-match-p "not success" text)) messages)))))


(ert-deftest opencode-shell-health-callback-defers-buffer-kill-off-stack ()
  ;; Killing the url response buffer synchronously in this sentinel pumps other
  ;; pending health sentinels, which re-enter the callback and can recurse until
  ;; the Lisp stack overflows.  The kill must be deferred, not synchronous.
  (let ((buffer (generate-new-buffer " *oc-health-test*"))
        (scheduled 0) killed ready)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat &rest _) (cl-incf scheduled) 'timer))
                  ((symbol-function 'kill-buffer)
                   (lambda (&rest _) (setq killed t))))
          (with-current-buffer buffer
            (setq-local url-http-response-status 200)
            (opencode-shell--server-health-callback
             '(:name "p") (lambda (r _profile) (setq ready r)) nil))
          ;; Callback ran synchronously with the right readiness...
          (should (eq ready t))
          ;; ...but the buffer kill was scheduled, not performed inline.
          (should (= scheduled 1))
          (should-not killed))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))


(ert-deftest opencode-shell-transport-errors-are-reported-once-per-outage ()
  (with-temp-buffer
    (let ((fail t) messages)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_ callback &rest _)
                   (with-temp-buffer
                     (if fail
                         (funcall callback '(:error (error connection-failed)))
                       (insert "HTTP/1.1 204 No Content\r\n\r\n")
                       (setq-local url-http-response-status 204)
                       (funcall callback nil)))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        ;; A burst of failures during one outage reports exactly once.
        (dotimes (_ 5)
          (opencode-shell--request "GET" "/x" #'ignore nil nil #'ignore)
          (opencode-shell-async-drain (current-buffer)))
        (should (= (length messages) 1))
        ;; A success clears the notice so the next outage reports again.
        (setq fail nil)
        (opencode-shell--request "GET" "/x" #'ignore nil nil #'ignore)
        (opencode-shell-async-drain (current-buffer))
        (setq fail t)
        (dotimes (_ 3)
          (opencode-shell--request "GET" "/x" #'ignore nil nil #'ignore)
          (opencode-shell-async-drain (current-buffer)))
        (should (= (length messages) 2))))))


(ert-deftest opencode-shell-ssh-gate-defers-http-until-ready ()
  (let* ((profile '(:name "ssh-gate" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (starts 0) (sent 0))
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--directory "/remote/")
      (cl-letf (((symbol-function 'opencode-shell--start-server)
                 (lambda (_profile callback &rest _)
                   (should-not callback)
                   (cl-incf starts)))
                ((symbol-function 'url-retrieve)
                 (lambda (&rest _) (cl-incf sent))))
        (opencode-shell--request "GET" "/session" #'ignore)
        (should (= sent 0))
        (should (= starts 1))
        (opencode-shell-recovery-success key)
        (opencode-shell--ssh-invoke-start-callback
         (car (plist-get (gethash key opencode-shell--servers) :callbacks)))
        (should (= sent 1))))))


(ert-deftest opencode-shell-ssh-refusal-enters-quiet-offline-state ()
  (let* ((profile '(:name "ssh-gate" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         (runtime (list :subscribers (make-hash-table :test #'eq)
                        :poll-timer nil :paused nil :poll-interval 2))
         (sent 0) (starts 0) notices)
    (puthash key runtime opencode-shell-async--runtimes)
    (opencode-shell-recovery-success key)
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (setq-local opencode-shell--directory "/remote/")
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (cl-incf sent)
                   (let ((response (generate-new-buffer " *offline-reply*")))
                     (with-current-buffer response
                       (funcall callback '(:error (error connection-failed)))))))
                ((symbol-function 'run-at-time) (lambda (&rest _) nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) notices)))
                ((symbol-function 'opencode-shell--start-server)
                 (lambda (&rest _) (cl-incf starts))))
        (opencode-shell--request "GET" "/session" #'ignore)
        (should (opencode-shell-recovery-offline-p key))
        (should (plist-get runtime :paused))
        (opencode-shell--request "GET" "/session" #'ignore)
        (should (= sent 1))
        (should (= starts 0))
        (should (= (length notices) 1))))))


(ert-deftest opencode-shell-ssh-gate-times-out-one-shot-request ()
  (let* ((profile '(:name "ssh-timeout" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         timers health (failures 0) sent)
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'opencode-shell--server-ready)
                 (lambda (_profile callback) (setq health callback)))
                ((symbol-function 'url-retrieve)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat callback &rest args)
                   (push (cons callback args) timers)))
                ((symbol-function 'message) #'ignore))
        (opencode-shell--request "GET" "/session" #'ignore nil nil
                                 (lambda () (cl-incf failures)))
        (should (plist-get (gethash key opencode-shell--servers) :checking))
        (apply (caar timers) (cdar timers))
        (should (= failures 1))
        (should-not (plist-get (gethash key opencode-shell--servers) :checking))
        (funcall health t)
        (should (= failures 1))
        (should-not sent)))))


(ert-deftest opencode-shell-ssh-disconnect-does-not-schedule-background-retries ()
  (let* ((profile '(:name "ssh-no-retry" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         (process (make-network-process
                   :name "opencode-test-disconnect-tunnel" :server t
                   :family 'ipv4 :host 'local :service t :noquery t))
         scheduled)
    (unwind-protect
        (with-temp-buffer
          (opencode-shell-mode)
          (setq-local opencode-shell--profile profile)
          (puthash key (list :process process :owned t :profile profile)
                   opencode-shell--servers)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay _repeat callback &rest args)
                       (push (list delay callback args) scheduled)
                       nil))
                    ((symbol-function 'message) #'ignore))
            (opencode-shell--ssh-disconnected profile)
            (should (opencode-shell-recovery-offline-p key))
            (should-not (process-live-p process))
            (should-not scheduled)))
      (ignore-errors (delete-process process)))))


(ert-deftest opencode-shell-ssh-stuck-tunnel-is-killed-on-repeat-health-failure ()
  "A live but no-longer-functional SSH tunnel is force-killed after one blip."
  (let* ((profile '(:name "ssh-stuck" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (process (make-network-process
                   :name "opencode-test-stuck-tunnel" :server t
                   :family 'ipv4 :host 'local :service t :noquery t))
         (opencode-shell--servers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash key (list :process process :owned t :checking t
                             :attempt 'attempt1 :profile profile)
                   opencode-shell--servers)
          (puthash key (list :offline nil :ready nil :epoch 0 :exhausted nil
                             :failures 1 :retry-timer nil :retry-token nil
                             :deadline nil :attempt nil)
                   opencode-shell-recovery--states)
          (cl-letf (((symbol-function 'opencode-shell--ssh-disconnected) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (opencode-shell--fail-start key 'attempt1 "SSH health unavailable"))
          (should-not (process-live-p process)))
      (ignore-errors (delete-process process)))))


(ert-deftest opencode-shell-ssh-tunnel-survives-single-blip ()
  "A first health-check failure against a live tunnel does not kill it yet."
  (let* ((profile '(:name "ssh-blip" :remote t
                    :base-url "http://127.0.0.1:4546"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (process (make-network-process
                   :name "opencode-test-blip-tunnel" :server t
                   :family 'ipv4 :host 'local :service t :noquery t))
         (opencode-shell--servers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash key (list :process process :owned t :checking t
                             :attempt 'attempt1 :profile profile)
                   opencode-shell--servers)
          (puthash key (list :offline nil :ready nil :epoch 0 :exhausted nil
                             :failures 0 :retry-timer nil :retry-token nil
                             :deadline nil :attempt nil)
                   opencode-shell-recovery--states)
          (cl-letf (((symbol-function 'opencode-shell--ssh-disconnected) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (opencode-shell--fail-start key 'attempt1 "SSH health unavailable"))
          (should (process-live-p process)))
      (ignore-errors (delete-process process)))))


(ert-deftest opencode-shell-ssh-spawn-timeout-kills-stuck-process ()
  "A fresh-spawn readiness timeout kills its process for SSH profiles too."
  (let* ((profile '(:name "ssh-spawn-timeout" :remote t
                    :base-url "http://127.0.0.1:4547"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (process (make-network-process
                   :name "opencode-test-spawn-timeout" :server t
                   :family 'ipv4 :host 'local :service t :noquery t))
         (opencode-shell--servers (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash key (list :process process :owned t :starting t
                             :attempt 'attempt1 :profile profile)
                   opencode-shell--servers)
          (puthash key (list :offline nil :ready nil :epoch 0 :exhausted nil
                             :failures 0 :retry-timer nil :retry-token nil
                             :deadline nil :attempt nil)
                   opencode-shell-recovery--states)
          (cl-letf (((symbol-function 'opencode-shell--ssh-disconnected) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (opencode-shell--fail-start key 'attempt1 "SSH readiness timed out"))
          (should-not (process-live-p process)))
      (ignore-errors (delete-process process)))))


(ert-deftest opencode-shell-explicit-ssh-stop-does-not-auto-reconnect ()
  (let* ((profile '(:name "ssh-stop" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal)))
    (puthash key (list :profile profile :owned t :process 'owned)
             opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'delete-process)
               (lambda (_) (opencode-shell--process-sentinel key nil 'owned "exited"))))
      (opencode-shell--stop-server profile)
      (should-not (opencode-shell-recovery-offline-p key)))))


(ert-deftest opencode-shell-ssh-offline-read-waits-for-next-healthy-attempt ()
  (let* ((profile '(:name "ssh-wait" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (sent 0))
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (opencode-shell-recovery-mark-offline key)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (cl-incf sent)))
                ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
        (opencode-shell--request "GET" "/session" #'ignore)
        (should (= sent 0))
        (let ((pending (plist-get (gethash key opencode-shell--servers)
                                  :callbacks)))
          (should (= (length pending) 1))
          (opencode-shell-recovery-success key)
           (opencode-shell--ssh-invoke-start-callback (car pending))
          (should (= sent 1)))))))


(ert-deftest opencode-shell-ssh-old-http-error-cannot-reopen-outage ()
  (let* ((profile '(:name "ssh-old" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         transport-callback)
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (opencode-shell-recovery-success key)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (setq transport-callback callback)))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
        (opencode-shell--request "GET" "/session" #'ignore)
        (opencode-shell-recovery-mark-offline key)
        (opencode-shell-recovery-success key)
        (let ((response (generate-new-buffer " *old-http-reply*")))
          (with-current-buffer response
            (funcall transport-callback '(:error (error connection-failed)))))
        (should (opencode-shell-recovery-ready-p key))
        (should-not (opencode-shell-recovery-offline-p key))))))


(ert-deftest opencode-shell-ssh-retry-expires-starting-health-attempt ()
  (let* ((profile '(:name "ssh-starting" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         deadline)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat callback &rest args)
                 (unless deadline (setq deadline (cons callback args)))
                 nil))
              ((symbol-function 'opencode-shell--start-server)
               (lambda (_profile _callback &rest _)
                 (puthash key (list :starting t :attempt 'attempt :profile profile)
                          opencode-shell--servers)))
              ((symbol-function 'message) #'ignore))
      (opencode-shell--ssh-retry profile)
      (apply (car deadline) (cdr deadline))
      (should-not (plist-get (gethash key opencode-shell--servers) :starting)))))


(ert-deftest opencode-shell-ssh-pending-read-fails-immediately-on-start-failure ()
  (let* ((profile '(:name "ssh-pending-fail" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         scheduled (checks 0) (sent 0) (failures 0))
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat callback &rest args)
                   (push (list delay callback args) scheduled) nil))
                ((symbol-function 'opencode-shell--server-ready)
                 (lambda (_profile callback)
                   (cl-incf checks)
                   (funcall callback nil)))
                ((symbol-function 'opencode-shell--spawn-server)
                 (lambda (_profile attempt)
                   (opencode-shell--fail-start key attempt "unavailable")))
                ((symbol-function 'url-retrieve)
                 (lambda (&rest _) (cl-incf sent)))
                ((symbol-function 'message) #'ignore))
        (opencode-shell--request "GET" "/session" #'ignore nil nil
                                 (lambda () (cl-incf failures)))
        (should (= checks 1))
        (should (= failures 1))
        (should (= sent 0))
        (should (opencode-shell-recovery-offline-p key))
        (should-not (plist-get (gethash key opencode-shell--servers) :callbacks))
        (should-not (cl-find 1 scheduled :key #'car))))))


(ert-deftest opencode-shell-ssh-initial-mutation-is-not-replayed-after-start-failure ()
  (let* ((profile '(:name "ssh-initial-post" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         scheduled (checks 0) (sent 0) (failures 0))
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay _repeat callback &rest args)
                   (push (list delay callback args) scheduled) nil))
                ((symbol-function 'opencode-shell--server-ready)
                 (lambda (_profile callback)
                   (cl-incf checks)
                   (funcall callback nil)))
                ((symbol-function 'opencode-shell--spawn-server)
                 (lambda (_profile attempt)
                   (opencode-shell--fail-start key attempt "unavailable")))
                ((symbol-function 'url-retrieve)
                 (lambda (&rest _) (cl-incf sent)))
                ((symbol-function 'message) #'ignore))
        (opencode-shell--request "POST" "/session" #'ignore nil nil
                                 (lambda () (cl-incf failures)))
        (should (= checks 1))
        (should (= failures 1))
        (should (= sent 0))
        (should-not (cl-find 1 scheduled :key #'car))))))


(ert-deftest opencode-shell-ssh-killed-waiter-is-pruned ()
  (let* ((profile '(:name "ssh-killed-waiter" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (key (opencode-shell--server-key profile))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         (origin (generate-new-buffer " *ssh-killed-waiter*"))
         scheduled)
    (unwind-protect
        (with-current-buffer origin
          (opencode-shell-mode)
          (setq-local opencode-shell--profile profile)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (delay _repeat callback &rest args)
                       (push (list delay callback args) scheduled) nil))
                    ((symbol-function 'message) #'ignore))
            (opencode-shell--ssh-disconnected profile)
            (opencode-shell--request "GET" "/session" #'ignore)
            (should-not scheduled)
            (kill-buffer origin)
            (opencode-shell--ssh-prune-waiters key)
            (should-not (plist-get (gethash key opencode-shell--servers)
                                   :callbacks))))
      (when (buffer-live-p origin) (kill-buffer origin)))))


(ert-deftest opencode-shell-transport-error-pauses-runtime-polling-until-success ()
  "Non-SSH profiles also pause background polling on transport error and resume on success."
  (let* ((profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile))
         (opencode-shell-async--runtimes (make-hash-table :test #'equal))
         (subscribers (make-hash-table :test #'eq))
         (fail t))
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local opencode-shell--profile profile)
      (puthash (current-buffer) (list :callback #'ignore) subscribers)
      (puthash key (list :subscribers subscribers :poll-timer 'timer
                         :paused nil :poll-interval 2)
               opencode-shell-async--runtimes)
       (cl-letf (((symbol-function 'timerp) (lambda (v) (eq v 'timer)))
                 ((symbol-function 'cancel-timer) #'ignore)
                 ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                 ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (with-temp-buffer
                     (if fail
                         (funcall callback '(:error (error connection-failed)))
                       (insert "HTTP/1.1 200 OK\r\n\r\n[]")
                       (setq-local url-http-response-status 200)
                       (funcall callback nil))))))
        (opencode-shell--request "GET" "/session" #'ignore)
        (should (plist-get (gethash key opencode-shell-async--runtimes) :paused))
        (setq fail nil)
        (opencode-shell--request "GET" "/session" #'ignore)
        (should-not (plist-get (gethash key opencode-shell-async--runtimes) :paused))))))


(ert-deftest opencode-shell-ssh-gate-coalesces-health-for-two-requests ()
  (let* ((profile '(:name "ssh-shared" :remote t
                    :base-url "http://127.0.0.1:4545"
                    :start-command ("ssh" "-N" "host")))
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         (health-checks 0) health (sent 0))
    (with-temp-buffer
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'opencode-shell--server-ready)
                 (lambda (_profile callback)
                   (cl-incf health-checks)
                   (setq health callback)))
                ((symbol-function 'url-retrieve)
                 (lambda (&rest _) (cl-incf sent)))
                ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
        (opencode-shell--request "GET" "/session" #'ignore)
        (opencode-shell--request "GET" "/permission" #'ignore)
        (should (= health-checks 1))
        (should (= sent 0))
        (funcall health t)
        (should (= sent 2))))))


(ert-deftest opencode-shell-routine-snapshot-paths-are-classified ()
  (dolist (path '("/session/s/message" "/permission" "/question"))
    (should (opencode-shell--routine-snapshot-path-p path)))
  (dolist (path '("/session/status" "/session" "/provider" "/agent"))
    (should-not (opencode-shell--routine-snapshot-path-p path))))


(ert-deftest opencode-shell-routine-snapshot-success-is-quiet-but-failure-is-logged ()
  (with-temp-buffer
    (let ((opencode-shell-log-requests t)
          logs)
      (cl-letf (((symbol-function 'opencode-shell--log)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) logs)))
                ((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (with-temp-buffer
                     (insert "HTTP/1.1 200 OK\r\n\r\n[]")
                     (setq-local url-http-response-status 200)
                     (funcall callback nil)))))
        (opencode-shell--request "GET" "/permission" #'ignore)
        (opencode-shell-async-drain (current-buffer)))
      (should-not logs)
      (cl-letf (((symbol-function 'opencode-shell--log)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) logs)))
                ((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (with-temp-buffer
                     (funcall callback '(:error (error connection-failed)))))))
        (opencode-shell--request "GET" "/permission" #'ignore))
      (should (seq-some (lambda (line)
                          (string-match-p "transport-error" line))
                        logs)))))


(ert-deftest opencode-shell-request-ignores-killed-origin ()
  (let ((origin (generate-new-buffer " *oc-dead-origin*")) retrieve-callback called)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq retrieve-callback callback))))
      (with-current-buffer origin
        (opencode-shell--request "GET" "/late" (lambda (_) (setq called t))))
      (kill-buffer origin)
      (with-temp-buffer
        (insert "HTTP/1.1 200 OK\r\n\r\n[]")
        (setq-local url-http-response-status 200)
        (funcall retrieve-callback nil))
      (should-not called))))


(ert-deftest opencode-shell-request-drops-response-from-prior-generation ()
  (with-temp-buffer
    (setq-local opencode-shell--generation 4)
    (let (retrieve-callback called)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _) (setq retrieve-callback callback))))
        (opencode-shell--request "GET" "/stale" (lambda (_) (setq called t)))
        (cl-incf opencode-shell--generation)
        (with-temp-buffer
          (insert "HTTP/1.1 200 OK\r\n\r\n[]")
          (setq-local url-http-response-status 200)
          (funcall retrieve-callback nil))
        (opencode-shell-async-drain (current-buffer))
        (should-not called)))))


(ert-deftest opencode-shell-stale-request-failure-is-not-user-visible ()
  (with-temp-buffer
    (setq-local opencode-shell--generation 7)
    (let (retrieve-callback messages logs)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _) (setq retrieve-callback callback)))
                ((symbol-function 'message)
                 (lambda (&rest args) (push args messages)))
                ((symbol-function 'opencode-shell--log)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) logs))))
        (opencode-shell--request "GET" "/stale-error" #'ignore)
        (cl-incf opencode-shell--generation)
        (with-temp-buffer
          (funcall retrieve-callback '(:error (error connection-failed))))
        (should-not messages)
        (should-not (seq-some (lambda (line)
                                (string-match-p "transport-error" line))
                              logs))))))


(ert-deftest opencode-shell-profile-scopes-url-directory-and-auth ()
  (let ((opencode-shell-auth-source-function
         (lambda (profile)
           (should (equal profile opencode-shell-test--local-profile))
           "Bearer profile-secret"))
        captured-url captured-headers messages)
    (with-temp-buffer
      (setq-local opencode-shell--profile opencode-shell-test--local-profile)
      (setq-local opencode-shell--directory "/server/project")
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url _callback &rest _)
                   (setq captured-url url captured-headers url-request-extra-headers)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (opencode-shell--request "GET" "/session" #'ignore)))
    (should (string-prefix-p "http://127.0.0.1:7777/session?" captured-url))
    (should (string-match-p "directory=%2Fserver%2Fproject" captured-url))
    (should (equal (cdr (assoc "Authorization" captured-headers))
                   "Bearer profile-secret"))
    (should-not (string-match-p "profile-secret"
                                (format "%S%S" opencode-shell-test--local-profile messages)))))


(ert-deftest opencode-shell-server-health-reuse-does-not-spawn ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal)) spawned callback)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (profile ready) (funcall ready profile)))
              ((symbol-function 'make-process) (lambda (&rest _) (setq spawned t))))
      (opencode-shell--start-server opencode-shell-test--local-profile
                                    (lambda (profile) (setq callback profile))))
    (should-not spawned)
    (should (equal callback opencode-shell-test--local-profile))
    (let ((state (gethash (opencode-shell--server-key opencode-shell-test--local-profile)
                          opencode-shell--servers)))
      (should state)
      (should-not (plist-get state :owned))
      (should (plist-get state :config)))))


(ert-deftest opencode-shell-server-start-is-single-per-server-and-let-binds-directory ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal))
        (make-calls 0) process-directory timers health-callback)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (setq health-callback callback)))
              ((symbol-function 'make-process)
               (lambda (&rest _)
                 (cl-incf make-calls) (setq process-directory default-directory) 'fake-process))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (push args timers) 'fake-timer)))
      (opencode-shell--start-server opencode-shell-test--local-profile #'ignore)
      (opencode-shell--start-server opencode-shell-test--local-profile #'ignore)
      (funcall health-callback nil)
      (funcall health-callback nil))
    (should (= make-calls 1))
    (should (equal (directory-file-name process-directory) "/tmp"))
    (should timers)))


(ert-deftest opencode-shell-server-wait-is-bounded ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
          (key (opencode-shell--server-key opencode-shell-test--local-profile))
         scheduled messages)
    (puthash key (list :callbacks (list #'ignore) :starting t :attempt 'attempt)
             opencode-shell--servers)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 20.0))
              ((symbol-function 'run-at-time) (lambda (&rest args) (push args scheduled)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (opencode-shell--await-server opencode-shell-test--local-profile 10.0 'attempt))
    (should-not scheduled)
    (should-not (plist-get (gethash key opencode-shell--servers) :callbacks))
    (should (string-match-p "timed out" (car messages)))))


(ert-deftest opencode-shell-exited-child-adopts-healthy-endpoint ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile))
         (process 'child) callback-profile)
    (puthash key (list :process process :owned t :starting t :attempt 'attempt
                       :callbacks (list (cons (lambda (value)
                                                (setq callback-profile value))
                                              profile)))
             opencode-shell--servers)
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 1))
              ((symbol-function 'process-buffer) (lambda (_) nil))
              ((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (funcall callback t))))
      (opencode-shell--process-sentinel key 'attempt process "exited")
      (should (gethash key opencode-shell--servers))
      (opencode-shell--await-server profile (+ (float-time) 1) 'attempt))
    (should (equal callback-profile profile))
    (let ((state (gethash key opencode-shell--servers)))
      (should state)
      (should-not (plist-get state :owned))
      (should-not (plist-get state :starting)))))


(ert-deftest opencode-shell-exited-child-unhealthy-reports-output-tail ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile))
         (process 'child)
         (buffer (generate-new-buffer " *oc-exit-tail*")) messages)
    (unwind-protect
        (progn
          (with-current-buffer buffer (insert "address already in use"))
          (puthash key (list :process process :owned t :starting t :attempt 'attempt)
                   opencode-shell--servers)
          (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_) 1))
                    ((symbol-function 'process-buffer) (lambda (_) buffer))
                    ((symbol-function 'float-time) (lambda (&optional _) 20.0))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (opencode-shell--process-sentinel key 'attempt process "exited")
            (should (gethash key opencode-shell--servers))
            (opencode-shell--await-server profile 10.0 'attempt))
          (should-not (gethash key opencode-shell--servers))
          (should (string-match-p "status 1" (car messages)))
          (should (string-match-p "address already in use" (car messages))))
      (kill-buffer buffer))))


(ert-deftest opencode-shell-explicit-stop-restart-and-owned-only-stop ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
          (key (opencode-shell--server-key profile)) deleted started)
    (puthash key (list :process 'owned :owned t) opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process 'owned)))
              ((symbol-function 'delete-process) (lambda (process) (setq deleted process))))
      (opencode-shell--stop-server profile))
    (should (eq deleted 'owned))
    (should-not (gethash key opencode-shell--servers))
    (puthash key (list :process 'foreign :owned nil) opencode-shell--servers)
    (should-error (opencode-shell--stop-server profile) :type 'user-error)
    (puthash key (list :process 'owned :owned t) opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'opencode-shell--start-server)
               (lambda (value &rest _) (setq started value))))
      (opencode-shell--restart-server profile)
      (should-not started)
      (opencode-shell--process-sentinel key nil 'owned "exited"))
    (should (equal started profile))))


(ert-deftest opencode-shell-restart-server-starts-fresh-when-nothing-is-running ()
  "Restarting a profile with no owned/foreign process just starts it."
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile))
         started)
    (cl-letf (((symbol-function 'opencode-shell--start-server)
               (lambda (value &rest _) (setq started value))))
      (opencode-shell--restart-server profile))
    (should (equal started profile))
    (should-not (gethash key opencode-shell--servers))))


(ert-deftest opencode-shell-restart-service-requires-restart-command ()
  "Restarting the service on a profile without :restart-command errors clearly."
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile))
    (should-error (opencode-shell--restart-service-process profile) :type 'user-error)))


(ert-deftest opencode-shell-restart-service-runs-configured-command ()
  "A configured :restart-command runs, then a fresh instance is started."
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile (plist-put (copy-tree opencode-shell-test--local-profile)
                              :restart-command '("pkill" "-f" "opencode")))
         (key (opencode-shell--server-key profile))
         (old-process 'owned)
         sentinel command started)
    (puthash key (list :process old-process :owned t) opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process old-process)))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq command (plist-get args :command)
                       sentinel (plist-get args :sentinel))))
              ((symbol-function 'opencode-shell--start-server)
               (lambda (value &rest _) (setq started value))))
      (opencode-shell--restart-service-process profile)
      (should (equal command '("pkill" "-f" "opencode")))
      (should (plist-get (gethash key opencode-shell--servers) :restart-profile))
      (should-not started)
      (funcall sentinel nil "finished"))
    (should (equal started profile))
    (should-not (gethash key opencode-shell--servers))))


(ert-deftest opencode-shell-restart-resolves-string-profile-non-interactively ()
  "Calling `opencode-shell-restart' with a profile name (not a plist) works."
  (let ((opencode-shell-profiles (list opencode-shell-test--remote-profile))
        restarted)
    (cl-letf (((symbol-function 'opencode-shell--restart-server)
               (lambda (profile) (setq restarted profile))))
      (opencode-shell-restart (opencode-shell--profile-name
                                opencode-shell-test--remote-profile)))
    (should (equal restarted opencode-shell-test--remote-profile))))


(ert-deftest opencode-shell-restart-defaults-to-buffer-profile ()
  "Invoked from a transcript, `opencode-shell-restart' must not prompt."
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--profile opencode-shell-test--local-profile)
    (let (restarted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "must not prompt in a shell buffer")))
                ((symbol-function 'opencode-shell--restart-server)
                 (lambda (profile) (setq restarted profile))))
        (call-interactively #'opencode-shell-restart))
      (should (equal restarted opencode-shell-test--local-profile)))))


(ert-deftest opencode-shell-restart-prompts-outside-a-shell-buffer ()
  "Outside any transcript, `opencode-shell-restart' still prompts for a profile."
  (with-temp-buffer
    (let (restarted (opencode-shell--profile nil))
      (cl-letf (((symbol-function 'opencode-shell--read-profile)
                 (lambda () opencode-shell-test--remote-profile))
                ((symbol-function 'opencode-shell--restart-server)
                 (lambda (profile) (setq restarted profile))))
        (call-interactively #'opencode-shell-restart))
      (should (equal restarted opencode-shell-test--remote-profile)))))


(ert-deftest opencode-shell-request-preserves-let-bound-timeout-through-ssh-start ()
  "A caller's custom url-retrieve timeout must survive the async SSH-start handoff."
  (let* ((profile opencode-shell-test--remote-profile)
         (opencode-shell--servers (make-hash-table :test #'equal))
         (opencode-shell-recovery--states (make-hash-table :test #'equal))
         seen-timeout)
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'opencode-shell--ssh-watch-start) #'ignore)
                ((symbol-function 'opencode-shell--start-server)
                 (lambda (_profile on-ready &rest _) (funcall on-ready t)))
                ((symbol-function 'opencode-shell--request-direct)
                 (lambda (&rest _) (setq seen-timeout opencode-shell--url-retrieve-timeout))))
        (let ((opencode-shell--url-retrieve-timeout 999))
          (opencode-shell--request "POST" "/slow" #'ignore)))
      (should (= seen-timeout 999)))))


(ert-deftest opencode-shell-remote-profile-starts-configured-transport ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal)) spawned)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (funcall callback nil)))
              ((symbol-function 'opencode-shell--spawn-server)
               (lambda (profile _attempt) (setq spawned profile))))
      (opencode-shell--start-server opencode-shell-test--remote-profile #'ignore))
    (should (eq spawned opencode-shell-test--remote-profile))))


(ert-deftest opencode-shell-remote-transport-always-starts-locally ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile (copy-tree opencode-shell-test--remote-profile))
         (key (opencode-shell--server-key profile))
         (attempt 'remote-attempt) cwd)
    (setf (plist-get profile :server-directory)
          "/ssh:code.example.test:/srv/project/")
    (puthash key (list :attempt attempt) opencode-shell--servers)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (setq cwd default-directory) 'transport))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'opencode-shell--await-server) #'ignore))
      (opencode-shell--spawn-server profile attempt))
    (should (equal cwd temporary-file-directory))))


(ert-deftest opencode-shell-health-probe-is-async-and-coalesced ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal)) probe (spawned 0))
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (setq probe callback)))
               ((symbol-function 'opencode-shell--spawn-server)
                (lambda (_profile _attempt) (cl-incf spawned))))
      (opencode-shell--start-server opencode-shell-test--local-profile #'ignore)
      (opencode-shell--start-server opencode-shell-test--local-profile #'ignore)
      (should (= spawned 0))
      (funcall probe nil)
      (should (= spawned 1)))))


(ert-deftest opencode-shell-shared-server-healthy-reuse-is-profile-isolated ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (one opencode-shell-test--local-profile)
         (two (plist-put (copy-tree one) :name "other"))
         (profiles nil) (probes 0) spawned)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback)
                 (cl-incf probes) (funcall callback t)))
              ((symbol-function 'make-process)
               (lambda (&rest _) (setq spawned t))))
      (opencode-shell--start-server one (lambda (profile) (push profile profiles)))
      (opencode-shell--start-server two (lambda (profile) (push profile profiles))))
    (should (= probes 2))
    (should-not spawned)
    (should (equal profiles (list two one)))))


(ert-deftest opencode-shell-shared-server-coalesces-spawn-and-callbacks ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (one opencode-shell-test--local-profile)
         (two (plist-put (copy-tree one) :name "other"))
         probe (spawns 0) profiles)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (setq probe callback)))
               ((symbol-function 'opencode-shell--spawn-server)
                (lambda (profile attempt)
                  (cl-incf spawns)
                  (opencode-shell--finish-start
                   (opencode-shell--server-key profile) attempt))))
      (opencode-shell--start-server one (lambda (profile) (push profile profiles)))
      (opencode-shell--start-server two (lambda (profile) (push profile profiles)))
      (funcall probe nil))
    (should (= spawns 1))
    (should (equal profiles (list two one)))))


(ert-deftest opencode-shell-shared-server-rejects-conflicting-config ()
  (let* ((one opencode-shell-test--local-profile)
         (two (plist-put (copy-tree one) :name "other"))
         (two (plist-put two :health-path "/ready"))
         (opencode-shell-profiles (list one two)))
    (should-error (opencode-shell--validate-profiles) :type 'user-error))
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (one opencode-shell-test--local-profile)
         (two (plist-put (copy-tree one) :startup-timeout 99)))
    (puthash (opencode-shell--server-key one)
             (list :config (opencode-shell--server-lifecycle-config one))
             opencode-shell--servers)
    (should-error (opencode-shell--start-server two) :type 'user-error)))


(ert-deftest opencode-shell-server-key-canonicalizes-safe-equivalents ()
  (dolist (urls '(("http://localhost" "http://127.0.0.1:80/")
                  ("https://localhost" "https://[::1]:443/")))
    (should (equal (opencode-shell--server-key (list :base-url (car urls)))
                   (opencode-shell--server-key (list :base-url (cadr urls))))))
  (should-not (equal (opencode-shell--server-key '(:base-url "http://localhost"))
                     (opencode-shell--server-key '(:base-url "https://localhost"))))
  (should-not (equal (opencode-shell--server-key '(:base-url "http://localhost"))
                     (opencode-shell--server-key '(:base-url "http://localhost:81")))))


(ert-deftest opencode-shell-owned-live-server-survives-failed-health-probe ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile)) callback spawned)
    (puthash key (list :process 'owned :owned t
                       :config (opencode-shell--server-lifecycle-config profile))
             opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile ready) (funcall ready nil)))
              ((symbol-function 'make-process) (lambda (&rest _) (setq spawned t))))
      (opencode-shell--start-server profile (lambda (value) (setq callback value))))
    (let ((state (gethash key opencode-shell--servers)))
      (should-not spawned)
      (should (eq (plist-get state :process) 'owned))
      (should (plist-get state :owned))
      (should (equal callback profile)))))


(ert-deftest opencode-shell-stale-start-callback-cannot-mutate-new-attempt ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
         (key (opencode-shell--server-key profile)) probes callbacks spawned)
    (cl-letf (((symbol-function 'opencode-shell--server-ready)
               (lambda (_profile callback) (push callback probes)))
              ((symbol-function 'opencode-shell--spawn-server)
               (lambda (_profile _attempt) (setq spawned t))))
      (opencode-shell--start-server profile (lambda (_) (push 'old callbacks)))
      (let ((old-probe (car probes)))
        (remhash key opencode-shell--servers)
        (opencode-shell--start-server profile (lambda (_) (push 'new callbacks)))
        (let ((new-state (gethash key opencode-shell--servers)))
          (funcall old-probe nil)
          (should-not spawned)
          (should (eq new-state (gethash key opencode-shell--servers)))
          (funcall (car probes) t))))
    (should (equal callbacks '(new)))))


(ert-deftest opencode-shell-shared-stop-restart-and-distinct-independence ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (one opencode-shell-test--local-profile)
         (two (plist-put (copy-tree one) :name "other"))
         (distinct (plist-put (copy-tree one) :base-url "http://127.0.0.1:8888"))
         (key (opencode-shell--server-key one)) deleted started)
    (should-not (equal key (opencode-shell--server-key distinct)))
    (puthash key (list :process 'owned :owned t) opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) (lambda (process) (setq deleted process))))
      (opencode-shell--stop-server two))
    (should (eq deleted 'owned))
    (puthash key (list :process 'owned :owned t) opencode-shell--servers)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'opencode-shell--start-server)
               (lambda (profile &rest _) (setq started profile))))
      (opencode-shell--restart-server two)
      (opencode-shell--process-sentinel key nil 'owned "exited"))
    (should (equal started two))))


(ert-deftest opencode-shell-custom-raw-auth-header-covers-health ()
  (let ((profile '(:name "auth" :base-url "http://localhost:9"
                   :auth-header "X-OpenCode-Token"))
        (opencode-shell-auth-source-function (lambda (_) "raw-secret")) headers)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _)
                 (setq headers url-request-extra-headers))))
      (opencode-shell--server-ready profile #'ignore))
    (should (equal (cdr (assoc "X-OpenCode-Token" headers)) "raw-secret"))
    (should-not (string-match-p "Bearer" (format "%S" headers)))))


(ert-deftest opencode-shell-process-tail-and-package-exit-cleanup ()
  (let ((buffer (generate-new-buffer " *oc-tail*")) deleted
        (opencode-shell--servers (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) buffer))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'delete-process) (lambda (p) (push p deleted))))
          (opencode-shell--process-filter 'process (make-string 5000 ?x))
          (should (<= (with-current-buffer buffer (buffer-size))
                      opencode-shell--process-tail-limit))
          (puthash "one" '(:process owned :owned t
                           :config (:stop-on-exit t)) opencode-shell--servers)
          (puthash "two" '(:process foreign :owned nil) opencode-shell--servers)
          (opencode-shell--stop-all-servers)
          (should (equal deleted '(owned)))
          (should (= (hash-table-count opencode-shell--servers) 0)))
      (kill-buffer buffer))))


(ert-deftest opencode-shell-make-process-error-clears-start-state ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal)) messages)
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "spawn failed")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (puthash (opencode-shell--server-key opencode-shell-test--local-profile)
               '(:starting t :attempt attempt :callbacks (ignore)) opencode-shell--servers)
      (opencode-shell--spawn-server opencode-shell-test--local-profile 'attempt))
    (should (= (hash-table-count opencode-shell--servers) 0))
    (should (string-match-p "could not start server" (car messages)))))


(ert-deftest opencode-shell-request-direct-returns-response-buffer ()
  (let ((response (generate-new-buffer " *oc-http-test*"))
        (profile '(:name "test" :base-url "http://127.0.0.1:4199")))
    (unwind-protect
        (with-temp-buffer
          (opencode-shell-mode)
          (setq-local opencode-shell--profile profile)
          (cl-letf (((symbol-function 'url-retrieve)
                     (lambda (&rest _) response)))
            (should (eq (opencode-shell--request-direct "GET" "/session" #'ignore)
                        response))))
      (when (buffer-live-p response)
        (kill-buffer response)))))


(ert-deftest opencode-shell-transport-isolates-remote-default-directory-and-defers-json-parse ()
  (let (seen-dir seen-handlers retrieve-cb parsed decoded-value)
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local default-directory "/ssh:remote-box:/srv/work/")
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (setq seen-dir default-directory
                         seen-handlers file-name-handler-alist
                         retrieve-cb callback)
                   nil))
                ((symbol-function 'opencode-shell--json-parse-body)
                 (lambda (raw)
                   (setq parsed t)
                   (json-parse-string raw :object-type 'alist :array-type 'list
                                      :null-object nil :false-object nil))))
        (opencode-shell--request "GET" "/permission"
                                 (lambda (val) (setq decoded-value val)))
        (should (equal seen-dir temporary-file-directory))
        (should-not seen-handlers)
        (with-temp-buffer
          (insert "HTTP/1.1 200 OK\r\n\r\n[{\"id\":\"p1\"}]")
          (setq-local url-http-response-status 200)
          (funcall retrieve-cb nil))
        (should-not parsed)
        (opencode-shell-async-drain (current-buffer))
        (should parsed)
        (should (equal decoded-value '(((id . "p1")))))))))


(ert-deftest opencode-shell-async-fetch-calls-callback-on-finished ()
  "The subprocess snapshot fetch decodes finished stdout and calls the callback."
  (let (sentinel result buf)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () t))
              ((symbol-function 'opencode-shell--url)
               (lambda (&rest _) "http://x/message"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       buf (plist-get args :buffer))
                 (list 'proc)))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'process-buffer) (lambda (_p) buf)))
      (opencode-shell--fetch-snapshot-async "/x"
                                            (lambda (value) (setq result value))
                                            #'ignore)
      (should sentinel)
      (with-current-buffer buf (insert "{\"a\":1}"))
      (funcall sentinel 'proc "finished\n")
      (should (equal result '((a . 1)))))))

(ert-deftest opencode-shell-async-fetch-ignores-nonterminal-sentinel ()
  "A non-terminal sentinel notification must not settle or invoke callbacks."
  (let (sentinel called buf)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () t))
              ((symbol-function 'opencode-shell--url)
               (lambda (&rest _) "http://x/message"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       buf (plist-get args :buffer))
                 (list 'proc)))
              ((symbol-function 'process-status) (lambda (_p) 'run))
              ((symbol-function 'process-buffer) (lambda (_p) buf)))
      (opencode-shell--fetch-snapshot-async "/x"
                                            (lambda (_v) (setq called t))
                                            (lambda () (setq called t)))
      (with-current-buffer buf (insert "partial"))
      (funcall sentinel 'proc "running\n")
      (should-not called))))

(ert-deftest opencode-shell-async-fetch-reports-curl-failure ()
  "A non-zero curl exit invokes the error callback, not the success callback."
  (let (sentinel got-error buf)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () t))
              ((symbol-function 'opencode-shell--url)
               (lambda (&rest _) "http://x/message"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       buf (plist-get args :buffer))
                 (list 'proc)))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 22))
              ((symbol-function 'process-buffer) (lambda (_p) buf)))
      (opencode-shell--fetch-snapshot-async "/x" #'ignore
                                            (lambda () (setq got-error t)))
      (funcall sentinel 'proc "exited abnormally\n")
      (should got-error))))

(ert-deftest opencode-shell-async-fetch-timeout-settles-once ()
  "A timeout fires the error callback once; a later terminal sentinel is ignored."
  (let (sentinel timeout-fn errs succ buf)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () t))
              ((symbol-function 'opencode-shell--url)
               (lambda (&rest _) "http://x/message"))
              ((symbol-function 'run-at-time)
               (lambda (_secs _rep fn &rest _) (setq timeout-fn fn) nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       buf (plist-get args :buffer))
                 (list 'proc)))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 0))
              ((symbol-function 'process-live-p) (lambda (_p) nil))
              ((symbol-function 'process-buffer) (lambda (_p) buf)))
      (opencode-shell--fetch-snapshot-async
       "/x" (lambda (_v) (setq succ t))
       (lambda () (setq errs (1+ (or errs 0)))))
      (with-current-buffer buf (insert "{\"a\":1}"))
      (funcall timeout-fn)
      (should (= errs 1))
      (funcall sentinel 'proc "finished\n")
      (should (= errs 1))
      (should-not succ)
      (should-not (buffer-live-p buf)))))

(ert-deftest opencode-shell-async-fetch-cleans-up-when-error-callback-throws ()
  "Cleanup still runs when a terminal error callback signals."
  (let (sentinel buf)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () t))
              ((symbol-function 'opencode-shell--url)
               (lambda (&rest _) "http://x/message"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq sentinel (plist-get args :sentinel)
                       buf (plist-get args :buffer))
                 (list 'proc)))
              ((symbol-function 'process-status) (lambda (_p) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_p) 22))
              ((symbol-function 'process-buffer) (lambda (_p) buf)))
      (opencode-shell--fetch-snapshot-async
       "/x" #'ignore (lambda () (error "boom")))
      (should-error (funcall sentinel 'proc "exited abnormally\n"))
      (should-not (buffer-live-p buf)))))

(ert-deftest opencode-shell-async-fetch-falls-back-when-unavailable ()
  "Without curl, the async fetch defers to the synchronous request-direct path."
  (let (fallback)
    (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
               (lambda () nil))
              ((symbol-function 'opencode-shell--request-direct)
               (lambda (&rest _) (setq fallback t))))
      (opencode-shell--fetch-snapshot-async "/x" #'ignore)
      (should fallback))))

(ert-deftest opencode-shell-request-direct-uses-async-fetch-when-enabled ()
  "With async snapshot fetch enabled, a message GET uses the subprocess path."
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell-base-url "http://127.0.0.1:4099")
    (let ((opencode-shell-async-snapshot-fetch t) (async nil) (url nil))
      (cl-letf (((symbol-function 'opencode-shell--auth-header) #'ignore)
                ((symbol-function 'opencode-shell--fetch-snapshot-async)
                 (lambda (&rest _) (setq async t)))
                ((symbol-function 'opencode-shell--url-retrieve-no-query)
                 (lambda (&rest _) (setq url t))))
        (opencode-shell--request-direct "GET" "/session/x/message" #'ignore)
        (should async)
        (should-not url)))))

(ert-deftest opencode-shell-request-direct-uses-url-retrieve-by-default ()
  "By default a message GET stays on the url-retrieve path."
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell-base-url "http://127.0.0.1:4099")
    (let ((opencode-shell-async-snapshot-fetch nil) (async nil) (url nil))
      (cl-letf (((symbol-function 'opencode-shell--auth-header) #'ignore)
                ((symbol-function 'opencode-shell--fetch-snapshot-async)
                 (lambda (&rest _) (setq async t)))
                ((symbol-function 'opencode-shell--url-retrieve-no-query)
                 (lambda (&rest _) (setq url t))))
        (opencode-shell--request-direct "GET" "/session/x/message" #'ignore)
        (should url)
        (should-not async)))))

(ert-deftest opencode-shell-async-fetch-success-error-is-not-decode-error ()
  "A throwing application callback is not a second transport failure."
  (with-temp-buffer
    (let (sentinel response (successes 0) (failures 0))
      (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
                 (lambda () t))
                ((symbol-function 'opencode-shell--auth-header) #'ignore)
                ((symbol-function 'opencode-shell--url) (lambda (&rest _) "/fixture"))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq sentinel (plist-get args :sentinel)
                         response (plist-get args :buffer))
                   'fetch))
                ((symbol-function 'process-status) (lambda (_) 'exit))
                ((symbol-function 'process-exit-status) (lambda (_) 0))
                ((symbol-function 'process-buffer) (lambda (_) response)))
        (opencode-shell--fetch-snapshot-async
         "/fixture"
         (lambda (_value) (cl-incf successes) (error "application failed"))
         (lambda () (cl-incf failures)))
        (with-current-buffer response (insert "[]"))
        (should-error (funcall sentinel 'fetch "finished"))
        (should (= successes 1))
        (should (= failures 0))
        (should-not (buffer-live-p response))))))

(ert-deftest opencode-shell-async-fetch-owner-lifecycle-cancels-request ()
  "Every owner exit path cancels only its request and releases its resources."
  (dolist (action '(cancel kill unsubscribe reset response-kill mode-change))
    (let ((owner (generate-new-buffer " *fetch-owner*"))
          response sentinel deadline
          (deleted 0) (callbacks 0))
      (unwind-protect
          (with-current-buffer owner
            (let ((schedule (symbol-function 'run-at-time)))
              (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
                         (lambda () t))
                        ((symbol-function 'opencode-shell--auth-header) #'ignore)
                        ((symbol-function 'opencode-shell--url)
                         (lambda (&rest _) "/fixture"))
                        ((symbol-function 'run-at-time)
                         (lambda (_seconds repeat callback &rest args)
                           (setq deadline (apply schedule 600 repeat callback args))))
                        ((symbol-function 'make-process)
                         (lambda (&rest args)
                           (setq sentinel (plist-get args :sentinel)
                                 response (plist-get args :buffer))
                           'fetch))
                        ((symbol-function 'processp) (lambda (p) (eq p 'fetch)))
                        ((symbol-function 'process-live-p) (lambda (_) (= deleted 0)))
                        ((symbol-function 'process-status) (lambda (_) 'signal))
                        ((symbol-function 'delete-process)
                         (lambda (p)
                           (cl-incf deleted)
                           ;; Exercise synchronous sentinel re-entry on cancellation.
                           (funcall sentinel p "killed"))))
                (opencode-shell--fetch-snapshot-async
                 "/fixture" (lambda (_) (cl-incf callbacks))
                 (lambda () (cl-incf callbacks)))
                (pcase action
                  ('cancel (opencode-shell-async-cancel))
                  ('kill (kill-buffer owner))
                  ('unsubscribe (opencode-shell-async-unsubscribe-runtime 'fixture owner))
                  ('reset (opencode-shell-async-reset))
                  ('response-kill (kill-buffer response))
                  ('mode-change (fundamental-mode)))
                (should (= deleted 1))
                (should (= callbacks 0))
                (should-not (buffer-live-p response))
                (should-not (memq deadline timer-list))
                (funcall sentinel 'fetch "late terminal notification")
                (should (= callbacks 0)))))
        (when (timerp deadline) (cancel-timer deadline))
        (when (buffer-live-p response) (kill-buffer response))
        (when (buffer-live-p owner) (kill-buffer owner))))))

(ert-deftest opencode-shell-async-fetch-start-failure-cleans-allocated-resources ()
  "Both allocation and spawn failures release partially acquired resources."
  (dolist (failure '(allocation spawn))
    (with-temp-buffer
      (let (header response deadline (failures 0)
            (new-buffer (symbol-function 'generate-new-buffer))
            (temp-file (symbol-function 'make-temp-file))
            (schedule (symbol-function 'run-at-time)))
        (unwind-protect
            (cl-letf (((symbol-function 'opencode-shell--async-fetch-available-p)
                       (lambda () t))
                      ((symbol-function 'opencode-shell--auth-header)
                       (lambda (_) '("X-Fixture" . "fixture")))
                      ((symbol-function 'opencode-shell--url)
                       (lambda (&rest _) "/fixture"))
                      ((symbol-function 'make-temp-file)
                       (lambda (&rest args) (setq header (apply temp-file args))))
                      ((symbol-function 'generate-new-buffer)
                       (lambda (&rest args)
                         (if (eq failure 'allocation) (error "allocation failed")
                           (setq response (apply new-buffer args)))))
                      ((symbol-function 'run-at-time)
                       (lambda (_seconds repeat callback &rest args)
                         (setq deadline (apply schedule 600 repeat callback args))))
                      ((symbol-function 'make-process)
                       (lambda (&rest _) (error "spawn failed"))))
              (opencode-shell--fetch-snapshot-async
               "/fixture" #'ignore (lambda () (cl-incf failures)))
              (should (= failures 1))
              (should-not (file-exists-p header))
              (should-not (buffer-live-p response))
              (should-not (memq deadline timer-list))
              (should-not opencode-shell-async--request-cancellations))
          (when (and header (file-exists-p header)) (delete-file header))
          (when (timerp deadline) (cancel-timer deadline))
          (when (buffer-live-p response) (kill-buffer response)))))))

(ert-deftest opencode-shell-async-request-cancellation-is-owner-scoped ()
  "Unsubscribing one owner must not cancel another owner's request."
  (with-temp-buffer
    (let ((other (generate-new-buffer " *other-request-owner*"))
          (ours 0) (theirs 0))
      (unwind-protect
          (progn
            (opencode-shell-async-register-request
             (current-buffer) (lambda () (cl-incf ours)))
            (opencode-shell-async-register-request
             other (lambda () (cl-incf theirs)))
            (opencode-shell-async-unsubscribe-runtime 'fixture (current-buffer))
            (should (= ours 1))
            (should (= theirs 0))
            (opencode-shell-async-cancel other)
            (should (= ours 1))
            (should (= theirs 1)))
        (kill-buffer other)))))

(ert-deftest opencode-shell-local-auto-recovery-shared-health-and-resync ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (opencode-shell-recovery--states (make-hash-table :test #'equal))
        (profile opencode-shell-test--local-profile)
        scheduled health (probes 0) (resyncs 0))
    (with-temp-buffer
      (opencode-shell-mode)
      (let* ((key (opencode-shell--server-key profile))
             (subscribers (make-hash-table :test #'eq))
             (runtime (list :subscribers subscribers :poll-interval 2 :paused t)))
        (puthash (current-buffer) (list :callback #'ignore) subscribers)
        (puthash key runtime opencode-shell-async--runtimes)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (setq scheduled (lambda () (apply callback args))) nil))
                  ((symbol-function 'opencode-shell--server-ready)
                   (lambda (_profile callback &rest _)
                     (cl-incf probes) (setq health callback) nil))
                  ((symbol-function 'opencode-shell--resync)
                   (lambda (&rest _) (cl-incf resyncs))))
          (opencode-shell--local-recovery-start profile)
          (let ((first scheduled))
            (opencode-shell--local-recovery-start profile)
            (should (eq first scheduled)))
          (funcall scheduled)
          (should (= probes 1))
          (funcall health t profile)
          (opencode-shell-async-drain (current-buffer))
          (should-not (plist-get runtime :paused))
          (should (= resyncs 1))
          (funcall health t profile)
          (opencode-shell-async-drain (current-buffer))
          (should (= resyncs 1)))))))

(ert-deftest opencode-shell-local-auto-recovery-final-unsubscribe-cancels ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (opencode-shell-recovery--states (make-hash-table :test #'equal))
        (profile opencode-shell-test--local-profile) scheduled health)
    (with-temp-buffer
      (let* ((key (opencode-shell--server-key profile))
             (subscribers (make-hash-table :test #'eq))
             (runtime (list :subscribers subscribers :paused t)))
        (puthash (current-buffer) #'ignore subscribers)
        (puthash key runtime opencode-shell-async--runtimes)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (setq scheduled (lambda () (apply callback args))) nil))
                  ((symbol-function 'opencode-shell--server-ready)
                   (lambda (_profile callback &rest _) (setq health callback) nil)))
          (opencode-shell--local-recovery-start profile)
          (funcall scheduled)
          (opencode-shell-async-unsubscribe-runtime key (current-buffer))
          (should-not (gethash (list 'local-http key) opencode-shell-recovery--states))
          (funcall health t profile)
          (should-not (opencode-shell-async-runtime-get key)))))))

(ert-deftest opencode-shell-local-auto-recovery-health-classification ()
  (dolist (reason '(401 403 503 transport))
    (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
          (opencode-shell-recovery--states (make-hash-table :test #'equal))
          (profile opencode-shell-test--local-profile)
          scheduled reject stale (schedules 0))
      (with-temp-buffer
        (let* ((key (opencode-shell--server-key profile))
               (subscribers (make-hash-table :test #'eq))
               (runtime (list :subscribers subscribers :paused t)))
          (puthash (current-buffer) #'ignore subscribers)
          (puthash key runtime opencode-shell-async--runtimes)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_delay _repeat callback &rest args)
                       (cl-incf schedules)
                       (setq scheduled (lambda () (apply callback args))) nil))
                    ((symbol-function 'opencode-shell--server-ready)
                     (lambda (_profile success &optional failure)
                       (setq stale success reject failure) nil)))
            (opencode-shell--local-recovery-start profile)
            (funcall scheduled)
            (let ((before schedules))
              (funcall reject reason)
              (if (memq reason '(401 403))
                  (progn
                    (should (= schedules before))
                    ;; Late failures from the same outage must not restart an
                    ;; authentication/configuration failure loop.
                    (opencode-shell--local-recovery-start profile)
                    (should (= schedules before)))
                (should (= schedules (1+ before)))))
            (funcall stale t profile)
            (should (plist-get runtime :paused))
            (opencode-shell-async-unsubscribe-runtime key (current-buffer))))))))

(ert-deftest opencode-shell-local-auto-recovery-cancel-probe-deadlines ()
  "Final unsubscribe cancels the health HTTP backstop as well as retry deadlines."
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (opencode-shell-recovery--states (make-hash-table :test #'equal))
        (profile opencode-shell-test--local-profile) timers cancelled response)
    (with-temp-buffer
      (let* ((key (opencode-shell--server-key profile))
             (subscribers (make-hash-table :test #'eq))
             (runtime (list :subscribers subscribers :paused t)))
        (puthash (current-buffer) #'ignore subscribers)
        (puthash key runtime opencode-shell-async--runtimes)
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args)
                     (let ((timer (list 'timer callback args))) (push timer timers) timer)))
                  ((symbol-function 'timerp) (lambda (x) (eq (car-safe x) 'timer)))
                  ((symbol-function 'cancel-timer) (lambda (x) (push x cancelled)))
                  ((symbol-function 'opencode-shell--auth-header) #'ignore)
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (setq response (generate-new-buffer " *health-fixture*")))))
          (unwind-protect
              (progn
                (opencode-shell--local-recovery-start profile)
                (apply (nth 1 (car timers)) (nth 2 (car timers)))
                (let ((backstop (car timers)))
                  (opencode-shell-async-unsubscribe-runtime key (current-buffer))
                  (should-not (buffer-live-p response))
                  (should (memq backstop cancelled))))
            (when (buffer-live-p response) (kill-buffer response))))))))

(provide 'opencode-shell-transport-test)
;;; opencode-shell-transport-test.el ends here
