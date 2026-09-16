;;; opencode-shell-test.el --- Tests for opencode-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'ert)
(require 'opencode-shell)
(require 'opencode-shell-acceptance-test)
(require 'completion-polling-regression)

(defvar opencode-shell-test--local-profile)
(defvar opencode-shell-test--remote-profile)
(defvar projectile-mode)

(ert-deftest opencode-shell-query-and-payload ()
  (should (equal (opencode-shell--query '((directory . "/tmp/a b") (empty)))
                 "directory=%2Ftmp%2Fa%20b"))
  (with-temp-buffer
    (setq-local opencode-shell--base-url "http://127.0.0.1:4199"
                opencode-shell--directory "/implicit/")
    (should (equal (opencode-shell--url
                    "/session" '((directory . "/explicit/") (limit . 1000)))
                   "http://127.0.0.1:4199/session?directory=%2Fexplicit%2F&limit=1000")))
  (with-temp-buffer
    (setq-local opencode-shell--selected-model
                '((providerID . "openai") (modelID . "gpt")))
    (setq-local opencode-shell--selected-agent "build")
    (let ((body (opencode-shell--prompt-body "안녕")))
      (should (equal (alist-get 'agent body) "build"))
      (should (equal (alist-get 'model body) opencode-shell--selected-model))
      (should (equal (alist-get 'text (aref (alist-get 'parts body) 0)) "안녕")))))

(ert-deftest opencode-shell-local-requests-bypass-url-proxy ()
  (let ((url-proxy-services '(("http" . "proxy.example:3128"))) seen)
    (with-temp-buffer
      (setq-local opencode-shell--profile opencode-shell-test--local-profile)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (&rest _) (setq seen url-proxy-services))))
        (opencode-shell--request "GET" "/question" #'ignore)))
    (should-not seen)))

(ert-deftest opencode-shell-session-normalization-and-filter ()
  (let* ((old '((id . "old") (title . "Alpha") (time (updated . 1000))))
         (new '((id . "new") (title . "Beta") (directory . "/work")
                 (time (updated . 3000))))
         (child '((id . "child") (title . "Reviewer") (parentID . "new")
                  (agent . "reviewer") (time (updated . 4000)))))
    (with-temp-buffer
      (setq-local opencode-shell--sessions
                   (opencode-shell--normalize-sessions (list old new child old)))
      (should (equal (mapcar (lambda (x) (opencode-shell--get x 'id))
                              opencode-shell--sessions) '("child" "new" "old")))
      (should (equal (mapcar #'car (opencode-shell--session-entries))
                     '("new" "old")))
      (setq-local opencode-shell--show-child-sessions t)
      (should (equal (mapcar #'car (opencode-shell--session-entries))
                     '("child" "new" "old")))
      (setq-local opencode-shell--filter "work")
      (should (equal (mapcar #'car (opencode-shell--session-entries)) '("new"))))))

(ert-deftest opencode-shell--open-at-point-forwards-explicit-directory ()
  (with-temp-buffer
    (insert (propertize "one" 'tabulated-list-id "s1"))
    (goto-char (point-min))
    (setq-local opencode-shell--directory "/list/root")
    (setq-local opencode-shell--sessions
                '(((id . "s1") (directory . "/scope/exact"))))
    (let (opened)
      (cl-letf (((symbol-function 'opencode-shell-open-session)
                 (lambda (id directory) (setq opened (list id directory)))))
        (opencode-shell--open-at-point)
        (should (equal opened '("s1" "/scope/exact")))))))

(ert-deftest opencode-shell-browser-create-reuses-start-session-flow ()
  (with-temp-buffer
    (setq-local opencode-shell--profile opencode-shell-test--remote-profile)
    (let (started)
      (cl-letf (((symbol-function 'opencode-shell--start-session)
                 (lambda (profile directory) (setq started (list profile directory)))))
        (opencode-shell--create-session)
        (should (equal started
                       (list opencode-shell-test--remote-profile nil)))))))

(ert-deftest opencode-shell-start-session-reads-directory-and-creates-titleless-session ()
  (let ((profile opencode-shell-test--local-profile) created)
    (cl-letf (((symbol-function 'opencode-shell--current-server-directory)
               (lambda (_) "/server/chosen/"))
              ((symbol-function 'opencode-shell--start-server)
               (lambda (value callback) (funcall callback value)))
              ((symbol-function 'opencode-shell--create-and-open-session)
               (lambda (value directory) (setq created (list value directory)))))
      (opencode-shell--start-session profile)
      (should (equal created (list profile "/server/chosen/"))))))

(ert-deftest opencode-shell-create-and-open-session-omits-title ()
  (let ((profile opencode-shell-test--remote-profile) request opened)
    (cl-letf (((symbol-function 'opencode-shell--request)
               (lambda (method path callback &optional body)
                 (setq request (list method path body))
                 (funcall callback '((id . "new")))))
              ((symbol-function 'opencode-shell-open-session)
               (lambda (id directory value)
                 (setq opened (list id directory value)))))
      (opencode-shell--create-and-open-session profile "/srv/chosen/")
      (should (equal request '("POST" "/session" nil)))
      (should (equal opened (list "new" "/srv/chosen/" profile))))))

(defun opencode-shell-test--complete-user-turn (id text)
  "Return a completed user turn with server ID ID and TEXT."
  (opencode-shell--make-turn
   :id id :server-user-id id :user text :status 'complete :acknowledged t))

(ert-deftest opencode-shell-fork-candidates-are-chronological-and-disambiguated ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--turns
          (list (opencode-shell-test--complete-user-turn "msg-1" "same\nprompt")
                (opencode-shell-test--complete-user-turn "msg-2" "same prompt")))
    (should (equal (opencode-shell--fork-candidates)
                   '(("Before prompt 1: same prompt" . "msg-1")
                     ("Before prompt 2: same prompt" . "msg-2"))))))

(ert-deftest opencode-shell-fork-session-selects-boundary-and-opens-result ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--session-id "ses-source"
                opencode-shell--profile opencode-shell-test--local-profile
                opencode-shell--directory "/server/project/"
                opencode-shell--turns
                (list (opencode-shell-test--complete-user-turn "msg-1" "first")
                      (opencode-shell-test--complete-user-turn "msg-2" "second")))
    (let (request opened)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (caar (cdr candidates))))
                ((symbol-function 'opencode-shell--request)
                 (lambda (method path callback &optional body params _error)
                   (setq request (list method path body params))
                   (funcall callback '((id . "ses-fork")
                                       (directory . "/server/project/")))))
                ((symbol-function 'opencode-shell-open-session)
                 (lambda (id directory profile)
                   (setq opened (list id directory profile)))))
        (opencode-shell-fork-session))
      (should (equal request
                     '("POST" "/session/ses-source/fork"
                       ((messageID . "msg-2")) nil)))
      (should (equal opened
                     (list "ses-fork" "/server/project/"
                           opencode-shell-test--local-profile))))))

(ert-deftest opencode-shell-fork-session-rejects-active-interaction ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--session-id "ses-source"
                opencode-shell--submit-in-flight t)
    (should-error (opencode-shell-fork-session) :type 'user-error))
  (should-not (lookup-key opencode-shell-mode-map (kbd "C-c C-f"))))

(ert-deftest opencode-shell-fork-session-requires-server-prompt-id ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--session-id "ses-source"
                opencode-shell--turns
                (list (opencode-shell--make-turn
                       :id "local" :user "pending" :status 'complete)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "empty candidates must not prompt")))
              ((symbol-function 'opencode-shell--request)
               (lambda (&rest _) (ert-fail "empty candidates must not request"))))
      (should-error (opencode-shell-fork-session) :type 'user-error))))

(ert-deftest opencode-shell-move-directory-updates-current-buffer-only ()
  (with-temp-buffer
    (opencode-shell-mode)
    (insert "draft")
    (let* ((buffer (current-buffer))
           (turns (list (opencode-shell-test--complete-user-turn "msg-1" "done")))
           (poll-timer 'existing-poll-timer)
           (opencode-shell--session-directory-overrides nil))
      (setq-local opencode-shell--session-id "ses-source"
                  opencode-shell--profile opencode-shell-test--local-profile
                  opencode-shell--directory "/server/project/old/"
                  opencode-shell--turns turns
                  opencode-shell--poll-timer poll-timer)
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) "/client/project/new/nested/"))
                ((symbol-function 'opencode-shell--project-directory)
                 (lambda (&optional _) "/client/project/new/"))
                ((symbol-function 'opencode-shell--request)
                 (lambda (&rest _) (ert-fail "directory change must not request")))
                ((symbol-function 'opencode-shell-open-session)
                 (lambda (&rest _) (ert-fail "directory change must not reopen"))))
        (opencode-shell-move-session-directory))
      (should (eq (current-buffer) buffer))
      (should (equal opencode-shell--session-id "ses-source"))
      (should (eq opencode-shell--turns turns))
      (should (eq opencode-shell--poll-timer poll-timer))
      (should (equal (opencode-shell--composer-text) "draft"))
      (should (equal opencode-shell--directory "/server/project/new/"))
      (should (equal (opencode-shell--session-directory-override
                      opencode-shell-test--local-profile "ses-source")
                     "/server/project/new/"))
      (should (equal default-directory "/client/project/new/")))))

(ert-deftest opencode-shell-move-directory-maps-remote-scope-in-place ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((opencode-shell--session-directory-overrides nil))
      (setq-local opencode-shell--session-id "ses-source"
                  opencode-shell--profile opencode-shell-test--remote-profile
                  opencode-shell--directory "/srv/project/old/")
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) "/ssh:code.example.test:/srv/project/new/"))
                ((symbol-function 'opencode-shell--project-directory)
                 (lambda (&optional directory) directory)))
        (opencode-shell-move-session-directory))
      (should (equal opencode-shell--directory "/srv/project/new/"))
      (should (equal default-directory
                     "/ssh:code.example.test:/srv/project/new/")))))

(ert-deftest opencode-shell-directory-overrides-relocate-session-list-rows ()
  (let* ((profile opencode-shell-test--local-profile)
         (key (list (opencode-shell--profile-key profile) "moved"))
         (opencode-shell--session-directory-overrides
          (list (cons key "/server/project/new/")))
         (sessions '(((id . "moved") (directory . "/server/project/old/"))
                     ((id . "native") (directory . "/server/project/new/")))))
    (should (equal (mapcar (lambda (session) (opencode-shell--get session 'id))
                           (opencode-shell--sessions-in-directory
                            sessions profile "/server/project/new/"))
                   '("moved" "native")))
    (should-not (opencode-shell--sessions-in-directory
                 sessions profile "/server/project/old/"))))

(ert-deftest opencode-shell-session-browser-fetches-relocated-sessions-by-id ()
  (with-temp-buffer
    (opencode-shell-sessions-mode)
    (setq-local opencode-shell--profile opencode-shell-test--local-profile
                opencode-shell--directory "/server/project/new/")
    (let ((opencode-shell--session-directory-overrides
           (list (cons (list (opencode-shell--profile-key opencode-shell-test--local-profile)
                             "moved")
                       "/server/project/new/")))
          requests)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &optional _body params &rest _)
                   (push (list path callback params) requests))))
        (opencode-shell--refresh)
        (funcall (cadr (assoc "/session/status" requests)) nil)
        (let ((session-query (nth 2 (assoc "/session" requests))))
          (should (equal session-query '((limit . 1000))))
          (should (string-match-p
                   "directory=%2Fserver%2Fproject%2Fnew%2F"
                   (opencode-shell--url "/session" session-query))))
        (funcall (cadr (assoc "/session" requests))
                 '(((id . "native") (directory . "/server/project/new/"))))
        (let ((relocated-request (assoc "/session/moved" requests)))
          (should (equal (nth 2 relocated-request) '((directory))))
          (funcall (cadr relocated-request)
                   '((id . "moved") (directory . "/server/project/old/"))))
        (opencode-shell-async-drain (current-buffer))
        (should (equal (mapcar (lambda (session) (opencode-shell--get session 'id))
                               opencode-shell--sessions)
                       '("moved" "native")))))))

(ert-deftest opencode-shell-move-directory-rejects-current-project ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--session-id "ses-source"
                opencode-shell--profile opencode-shell-test--local-profile
                opencode-shell--directory "/server/project/")
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) "/client/project/nested/"))
              ((symbol-function 'opencode-shell--project-directory)
               (lambda (&optional _) "/client/project/")))
      (should-error (opencode-shell-move-session-directory) :type 'user-error)))
  (should-not (lookup-key opencode-shell-mode-map (kbd "C-c C-d"))))

(ert-deftest opencode-shell-model-agent-normalization ()
  (let* ((models (opencode-shell--normalize-models
                  '((all . (((id . "p") (models . (("m" . ((id . "m")))))))))))
         (agents (opencode-shell--normalize-agents
                  '(((name . "build")) ((name . "plan"))))))
    (should (equal (caar models) "p/m"))
    (should (equal (mapcar #'car agents) '("build" "plan")))
    (should (equal (opencode-shell--preserve-choice
                    '((providerID . "p") (modelID . "m")) models)
                   '((providerID . "p") (modelID . "m"))))))

(ert-deftest opencode-shell-model-agent-normalization-filters-server-scope ()
  (let ((models
         (opencode-shell--normalize-models
          '((connected . ("ready"))
            (all . (((id . "ready")
                     (models . (("one" . ((id . "one"))))))
                    ((id . "other")
                     (models . (("two" . ((id . "two")))))))))))
        (agents
         (opencode-shell--normalize-agents
          '(((name . "build") (mode . "primary"))
            ((name . "hidden") (mode . "primary") (hidden . t))
            ((name . "worker") (mode . "subagent"))))))
    (should (equal (mapcar #'car models) '("ready/one")))
    (should (equal (mapcar #'car agents) '("build")))
    (should-not
     (opencode-shell--normalize-models
      '((connected . ())
         (all . (((id . "ready")
                  (models . (("one" . ((id . "one")))))))))))))

(ert-deftest opencode-shell-model-object-and-session-display ()
  (let ((models (opencode-shell--normalize-models
                 '((all . (((id . "p")
                            (models . (((id . "one"))
                                       ("two" . ((name . "Two"))))))))))))
    (should (equal (mapcar #'car models) '("p/one" "p/two")))
    (should (equal (mapcar #'car
                           (opencode-shell--normalize-models
                            '((providers . (((id . "q")
                                             (models . ((three . ((name . "Three")))))))))))
                   '("q/three")))
    (should (equal (aref (cadr (opencode-shell--session-row
                                '((id . "s") (model . ((providerID . "p")
                                                       (modelID . "one")))))) 3)
                   "p/one"))
    (should (equal (aref (cadr (opencode-shell--session-row
                                '((id . "s") (model . "p/two")))) 3)
                   "p/two"))))

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

(ert-deftest opencode-shell-async-queue-coalesces-and-drops-stale-work ()
  (with-temp-buffer
    (setq-local opencode-shell--generation 3)
    (let (values)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) 'timer))
                ((symbol-function 'timerp) (lambda (value) (eq value 'timer)))
                ((symbol-function 'cancel-timer) #'ignore))
        (opencode-shell-async-enqueue (current-buffer) 'messages 3
                                      (lambda (value) (push value values)) 'old)
        (opencode-shell-async-enqueue (current-buffer) 'messages 3
                                      (lambda (value) (push value values)) 'latest)
        (opencode-shell-async-enqueue (current-buffer) 'status 2
                                      (lambda (value) (push value values)) 'stale)
        (opencode-shell-async-drain (current-buffer))
        (should (equal values '(latest)))
        (should-not opencode-shell-async--queue)
        (should-not opencode-shell-async--idle-timer)))))

(ert-deftest opencode-shell-question-multiple-custom-semantics ()
  (let ((item '((id . "q")
                (questions . (((question . "Single choice")
                               (options . (((label . "A")) ((label . "B")))))
                              ((question . "Single custom") (custom . t)
                               (options . (((label . "C")))))
                              ((question . "Multiple choice") (multiple . t)
                               (options . (((label . "D")) ((label . "E")))))
                              ((question . "Multiple custom") (multiple . t)
                               (custom . t) (options . (((label . "F")))))))))
        requests completion-calls)
    (cl-letf (((symbol-function 'opencode-shell--choose-pending)
               (lambda (_ callback) (funcall callback item)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'completing-read)
               (lambda (prompt collection _predicate require-match &rest _)
                 (push (list 'single prompt collection require-match)
                       completion-calls)
                 (if require-match "B" "own single")))
              ((symbol-function 'completing-read-multiple)
               (lambda (prompt collection _predicate require-match &rest _)
                 (push (list 'multiple prompt collection require-match)
                       completion-calls)
                 (if require-match '("E" "D") '("own first" "F" "own last"))))
              ((symbol-function 'opencode-shell--request)
               (lambda (method path _callback &optional body &rest _)
                 (push (list method path body) requests))))
      (opencode-shell--questions)
      (should (equal (caddar requests)
                     '((answers . [["B"] ["own single"] ["E" "D"]
                                   ["own first" "F" "own last"]]))))
      (should (equal (json-serialize (caddar requests))
                     "{\"answers\":[[\"B\"],[\"own single\"],[\"E\",\"D\"],[\"own first\",\"F\",\"own last\"]]}"))
      (should
       (equal (nreverse completion-calls)
              '((single "Single choice [A, B]: " ("A" "B") t)
                (single "Single custom [C]: " ("C") nil)
                (multiple "Multiple choice [D, E]: " ("D" "E") t)
                (multiple "Multiple custom [F]: " ("F") nil)))))))

(ert-deftest opencode-shell-question-rejection ()
  (let ((item '((id . "q") (question . "Question"))) requests)
    (cl-letf (((symbol-function 'opencode-shell--choose-pending)
               (lambda (_ callback) (funcall callback item)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'opencode-shell--request)
               (lambda (method path _callback &optional body &rest _)
                 (push (list method path body) requests))))
      (opencode-shell--questions)
      (should (equal (cadar requests) "/question/q/reject")))))

(ert-deftest opencode-shell-renders-one-question-after-permissions ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "draft")
    (goto-char (+ opencode-shell--composer-start 2))
    (opencode-shell--receive-questions
     '(((id . "q1") (sessionID . "s") (header . "Choice")
        (question . "Choose"))
       ((id . "q2") (sessionID . "s") (question . "Later"))))
    (should (= 1 (how-many "┌─ QUESTION" (point-min) (point-max))))
    (should (string-match-p
             "│  Choice\n│\n│  Choose\n│\n│  RET/a answer  r reject"
             (buffer-string)))
    (should-not (string-match-p "Waiting for answer" (buffer-string)))
    (should-not (string-match-p "Later" (buffer-string)))
    (should (equal (opencode-shell--composer-text) "draft"))
    (should (= (- (point) opencode-shell--composer-start) 2))
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "read"))))
    (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))
    (should-not (string-match-p "┌─ QUESTION" (buffer-string)))
    (opencode-shell--receive-permissions nil)
    (should (= 1 (how-many "┌─ QUESTION" (point-min) (point-max))))))

(ert-deftest opencode-shell-question-card-does-not-duplicate-header ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-questions
     '(((id . "q") (sessionID . "s") (header . "Choose")
        (question . "Choose"))))
    (should (= 1 (how-many "│  Choose" (point-min) (point-max))))
    (should (eq (lookup-key opencode-shell-question-map (kbd "RET"))
                #'opencode-shell--questions))
    (should (eq (lookup-key opencode-shell-question-map (kbd "r"))
                #'opencode-shell--question-reject))))

(ert-deftest opencode-shell-inline-question-reply-resyncs-immediately ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--questions-pending
          '(((id . "q1") (sessionID . "s") (question . "Choose")
             (options . (((label . "A")))))))
    (let (requests resyncs)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "A"))
                ((symbol-function 'opencode-shell--resync)
                 (lambda (&optional full) (push full resyncs)))
                ((symbol-function 'opencode-shell--request)
                 (lambda (method path callback &optional body &rest _)
                   (push (list method path body) requests)
                   (funcall callback nil))))
        (opencode-shell--questions)
        (should (member '("POST" "/question/q1/reply" ((answers . [["A"]])))
                        requests))
        (should (equal resyncs '(nil)))
        (should-not opencode-shell--question-sending)
        (should-not opencode-shell--questions-pending)))))

(ert-deftest opencode-shell-permission-replies-and-presentation ()
  (let ((item '((id . "p1") (permission . "bash")
                (patterns . ("git status" "git diff"))
                (always . ("git *"))
                (tool . ((name . "shell") (command . "git status")))
                (metadata . ((cwd . "/work") (reason . "inspect")))))
        requests confirmations)
    (cl-letf (((symbol-function 'opencode-shell--permission-at-point)
               (lambda () item))
              ((symbol-function 'yes-or-no-p)
               (lambda (shown) (push shown confirmations) t))
              ((symbol-function 'opencode-shell--request)
               (lambda (method path _callback &optional body &rest _)
                 (push (list method path body) requests))))
      (opencode-shell--permission-allow-once)
      (should (equal (car requests)
                     '("POST" "/permission/p1/reply" ((reply . "once")))))
      (should (equal (opencode-shell--permission-description item)
                     "bash: git status"))
      (should-not confirmations)

      (setq requests nil opencode-shell--permission-sending nil)
      (opencode-shell--permission-reject)
      (should (equal (caddar requests) '((reply . "reject"))))
      (should-not confirmations)

      (setq requests nil opencode-shell--permission-sending nil)
      (opencode-shell--permission-allow-always)
      (should (equal (caddar requests) '((reply . "always"))))
      (should-not confirmations))))

(ert-deftest opencode-shell-api-log-uses-dedicated-read-only-buffer ()
  (let ((opencode-shell-log-buffer-name " *opencode-shell-test-log*")
        (opencode-shell-log-requests t))
    (unwind-protect
        (progn
          (opencode-shell--log "OpenCode API #%d → GET /session" 1)
          (with-current-buffer opencode-shell-log-buffer-name
            (should buffer-read-only)
            (should (string-match-p
                     "OpenCode API #1.*GET /session" (buffer-string)))))
      (kill-buffer opencode-shell-log-buffer-name))))

(ert-deftest opencode-shell-api-log-is-separated-by-session ()
  (with-temp-buffer
    (rename-buffer "*Opencode project shell*" t)
    (let ((opencode-shell-log-buffer-name " *opencode-shell-test-log*")
          (opencode-shell-log-requests t)
          (opencode-shell--session-id "session-1"))
      (unwind-protect
          (progn
            (opencode-shell--log "session request")
            (should (get-buffer "*Opencode project shell*-log"))
            (should-not (get-buffer opencode-shell-log-buffer-name)))
        (when-let ((buffer (get-buffer "*Opencode project shell*-log")))
          (kill-buffer buffer))))))

(ert-deftest opencode-shell-killing-session-buffer-kills-its-log ()
  (let ((shell (generate-new-buffer "*Opencode cleanup-log shell*"))
        log)
    (unwind-protect
        (progn
          (with-current-buffer shell
            (opencode-shell-mode)
            (setq opencode-shell--session-id "session-1")
            (opencode-shell--log "session request")
            (setq log (get-buffer (opencode-shell--log-buffer-name))))
          (should (buffer-live-p log))
          (kill-buffer shell)
          (should-not (buffer-live-p log)))
      (when (buffer-live-p shell) (kill-buffer shell))
      (when (buffer-live-p log) (kill-buffer log)))))

(ert-deftest opencode-shell-api-log-separates-identical-session-ids-by-profile ()
  (let ((one (generate-new-buffer "*Opencode project shell*"))
        (two (generate-new-buffer "*Opencode project shell*")))
    (unwind-protect
        (should-not
         (equal (with-current-buffer one
                  (setq opencode-shell--session-id "same")
                  (opencode-shell--log-buffer-name))
                (with-current-buffer two
                  (setq opencode-shell--session-id "same")
                  (opencode-shell--log-buffer-name))))
      (kill-buffer one)
      (kill-buffer two))))

(ert-deftest opencode-shell-lifecycle-summary-is-private-and-deterministic ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((secret "NEVER-LOG-PROMPT-TOOL-PERMISSION-DIRECTORY-SECRET")
           (turn (opencode-shell--make-turn
                  :id "local-1" :user secret :status 'receiving
                  :parts `(((id . "tool-1") (type . "tool") (tool . ,secret)
                            (state . ((status . "running") (input . ,secret)))))
                  :assistant-messages
                  `(("assistant-1" .
                     ((info . ((id . "assistant-1") (role . "assistant")
                               (finish . "stop") (time . ((completed . 2)))))
                      (parts . (((id . "tool-1") (type . "tool")
                                 (tool . ,secret)
                                 (state . ((status . "running")))))))))))
           (opencode-shell--turns (list turn))
           (opencode-shell--session-id "session-1")
           (opencode-shell--session-status
            '(("session-1" . (("type" . "idle")))))
           (opencode-shell--permissions `(((id . "permission-1") (description . ,secret))))
           (opencode-shell--permission-sending t)
           (opencode-shell--submit-in-flight "local-1")
           (opencode-shell--request-status "receiving")
           (summary (opencode-shell--lifecycle-summary)))
      (should (string-match-p "nonterminal=local-1:receiving" summary))
      (should (string-match-p "last-assistant=assistant-1" summary))
      (should (string-match-p "finish=stop completed=present" summary))
      (should (string-match-p "tool:running=1" summary))
      (should (string-match-p "session-status=idle" summary))
      (should (string-match-p "permissions=1 reply-in-flight=yes" summary))
      (should (string-match-p "submit-match=yes" summary))
      (should-not (string-match-p secret summary)))))

(ert-deftest opencode-shell-lifecycle-log-coalesces-unchanged-state ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((opencode-shell-log-requests t) lines)
      (cl-letf (((symbol-function 'opencode-shell--log)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) lines))))
        (opencode-shell--log-lifecycle "messages")
        (opencode-shell--log-lifecycle "status")
        (should (= (length lines) 1))
        (setq opencode-shell--request-status "waiting")
        (opencode-shell--log-lifecycle "messages")
        (should (= (length lines) 2))
        (opencode-shell--log-lifecycle "manual" t)
        (should (= (length lines) 3))))))

(ert-deftest opencode-shell-message-lifecycle-log-carries-sequence ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let (events)
      (cl-letf (((symbol-function 'opencode-shell--log-lifecycle)
                 (lambda (event &optional _force) (push event events))))
        (opencode-shell--render-messages nil 7)
        (should (member "messages:7" events))
        (should (member "poll-stop" events))))))

(ert-deftest opencode-shell-permission-lifecycle-log-follows-snapshot ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (let (events)
      (cl-letf (((symbol-function 'opencode-shell--log-lifecycle)
                 (lambda (event &optional _force) (push event events)))
                ((symbol-function 'opencode-shell--start-polling) #'ignore))
        (opencode-shell--receive-permissions
         '(((id . "p1") (sessionID . "s") (permission . "bash"))))
        (should (equal (car events) "permissions"))))))

(ert-deftest opencode-shell-permission-pending-label-shows-patterns ()
  (let (candidates)
    (cl-letf (((symbol-function 'opencode-shell--request)
               (lambda (_method _path callback &rest _)
                 (funcall callback
                          '(((id . "p2") (permission . "edit")
                             (patterns . ("src/a.el" "src/b.el")))))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (setq candidates (mapcar #'car table))
                 (caar table))))
      (opencode-shell--choose-pending "permission" #'ignore)
      (should (equal (car candidates) "p2: edit: src/a.el")))))

(ert-deftest opencode-shell-permission-context-is-bounded ()
  (let ((description
         (opencode-shell--permission-description
          `((permission . "read")
            (metadata . ((payload . ,(make-string 1000 ?x))))))))
    (should (< (length description) 250))
    (should (equal description "read"))))

(ert-deftest opencode-shell-permission-description-hides-transport-details ()
  (let* ((item '((permission . "bash")
                 (patterns . ("fallback command"))
                 (always . ("bash *" "scripts/check.sh *"))
                 (tool . ((messageID . "hidden-message")
                          (callID . "hidden-call")))
                 (metadata . ((command . "bash -n scripts/*.sh")
                              (cwd . "/hidden/workspace")))))
         (description (opencode-shell--permission-description item)))
    (should (equal description "bash: bash -n scripts/*.sh"))
    (should-not (string-match-p "hidden" description))))

(ert-deftest opencode-shell-permission-card-shows-only-actionable-context ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-permissions
     '(((id . "p") (sessionID . "s") (permission . "bash")
        (patterns . ("fallback")) (always . ("bash *"))
        (tool . ((messageID . "hidden-message") (callID . "hidden-call")))
        (metadata . ((command . "bash -n scripts/*.sh")
                     (cwd . "/hidden/workspace"))))))
    (let ((card (buffer-substring-no-properties
                 opencode-shell--permission-begin opencode-shell--permission-end)))
      (should (string-match-p
               "│  bash\n│\n│  bash -n scripts/\\*.sh\n│\n│  C-c C-y once  C-c C-l always  C-c C-n reject"
               card))
      (dolist (hidden '("Always scope" "hidden-message" "hidden-call"
                        "metadata" "patterns=" "tool="))
        (should-not (string-match-p hidden card))))))

(ert-deftest opencode-shell-permission-state-deduplicates-by-id ()
  (let* ((first '((id . "p1") (permission . "bash")))
         (duplicate '((id . "p1") (permission . "read")))
         (second '((id . "p2") (permission . "bash")))
         (items (opencode-shell--deduplicate-permissions
                 (list first duplicate '((permission . "missing")) second))))
    (should (equal items (list first second)))))

(ert-deftest opencode-shell-renders-only-first-pending-permission ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash")
        (patterns . ("first")))
       ((id . "p2") (sessionID . "s") (permission . "bash")
        (patterns . ("second")))))
    (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))
    (should (string-match-p "first" (buffer-string)))
    (should-not (string-match-p "second" (buffer-string)))
    (should (= 2 (length opencode-shell--permissions)))))

(ert-deftest opencode-shell-always-replies-without-emacs-confirmation ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash")
        (patterns . ("first")) (always . ("first")))
       ((id . "p2") (sessionID . "s") (permission . "bash")
        (patterns . ("second")))))
    (let (request callback)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (ert-fail "unexpected confirmation")))
                ((symbol-function 'opencode-shell--request)
                 (lambda (method path success &optional body &rest _)
                   (setq request (list method path body) callback success))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-always)
        (should (equal request
                       '("POST" "/permission/p1/reply" ((reply . "always")))))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
          (funcall callback nil)
          (opencode-shell-async-drain (current-buffer)))
        (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
        (let ((result-position (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "PERMISSION ALWAYS")
                                 (line-beginning-position))))
          (opencode-shell--receive-permissions opencode-shell--permissions)
          (should (= result-position
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward "PERMISSION ALWAYS")
                       (line-beginning-position)))))
        (setq opencode-shell--rendered-turns
              (list (opencode-shell--make-turn :id "not-a-prefix")))
        (opencode-shell--render-turns)
        (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
        (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))
        (should (string-match-p "second" (buffer-string)))
        (opencode-shell--receive-permissions nil)
        (should (= 0 (how-many "┌─ PERMISSION" (point-min) (point-max))))))))

(ert-deftest opencode-shell-inline-permission-preserves-composer-and-replies ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "draft")
    (goto-char (+ opencode-shell--composer-start 2))
    (opencode-shell--receive-permissions
     '(((id . "missing") (permission . "write"))
        ((id . "other") (sessionID . "x") (permission . "read"))
        ((id . "p") (sessionID . "s") (permission . "bash")
         (patterns . ("git status")) (always . ("git *")))))
    (should (= (length opencode-shell--permissions) 1))
    (should (equal (opencode-shell--composer-text) "draft"))
    (should (= (- (point) opencode-shell--composer-start) 2))
    (goto-char opencode-shell--permission-begin)
    (should (search-forward "PERMISSION" opencode-shell--permission-end t))
    (should (search-forward "git status" opencode-shell--permission-end t))
    (should-not (search-forward "Always scope" opencode-shell--permission-end t))
    (opencode-shell--receive-permissions opencode-shell--permissions)
    (goto-char (point-min))
    (should (= (how-many "PERMISSION" (point-min) (point-max)) 1))
    (should (= (how-many "Prompt>" (point-min) (point-max)) 0))
    (let (request callback)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (method path success &optional body &rest _)
                   (setq request (list method path body) callback success))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-once)
        (should (equal request '("POST" "/permission/p/reply" ((reply . "once")))))
        (should-error (opencode-shell--permission-reject) :type 'user-error)
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
          (funcall callback nil)
          (funcall callback nil)
          (opencode-shell-async-drain (current-buffer)))
        (should-not opencode-shell--permissions)
        (opencode-shell--receive-permissions
         '(((id . "p") (sessionID . "s") (permission . "bash")
            (patterns . ("git status")))))
        (should-not opencode-shell--permissions)
        (should (= (length opencode-shell--resolved-permissions) 1))
         (should (string-match-p "PERMISSION ONCE:.*bash.*git status" (buffer-string)))
         (should (equal (opencode-shell--composer-text) "draft"))))))

(ert-deftest opencode-shell-resolved-permission-survives-full-turn-rerender-at-anchor ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (let ((turn (opencode-shell--make-turn :id "turn-1" :user "run it"
                                            :assistant "" :status 'waiting)))
      (setq opencode-shell--turns (list turn))
      (opencode-shell--render-turns)
      (opencode-shell--receive-permissions
       '(((id . "p1") (sessionID . "s") (permission . "bash")
          (patterns . ("git diff --check")))))
      (let (callback)
        (cl-letf (((symbol-function 'opencode-shell--request)
                   (lambda (_method path success &rest _)
                     (when (equal path "/permission/p1/reply")
                       (setq callback success)))))
          (goto-char opencode-shell--permission-begin)
          (opencode-shell--permission-allow-always)
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
            (funcall callback nil)
            (opencode-shell-async-drain (current-buffer)))))
      (should (equal "turn-1"
                     (opencode-shell--get (car opencode-shell--resolved-permissions)
                                          'after-turn-id)))
      ;; The normal live callback path anchors in its coalesced idle render; it
      ;; must not depend on a later stale-marker or forced full rerender.
      (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
      (should (< (save-excursion
                   (goto-char (point-min))
                   (search-forward "PERMISSION ALWAYS"))
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "Waiting for response"))))
      (setq opencode-shell--rendered-turns
            (list (opencode-shell--make-turn :id "not-a-prefix")))
      (opencode-shell--render-turns)
      (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
       (should (< (save-excursion
                    (goto-char (point-min))
                    (search-forward "PERMISSION ALWAYS")
                    (point))
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "Waiting for response")
                    (point))))
       (setf (opencode-shell--turn-assistant turn) "finished"
             (opencode-shell--turn-status turn) 'complete)
       (dotimes (_ 2)
         (setq opencode-shell--rendered-turns
               (list (opencode-shell--make-turn :id "not-a-prefix")))
         (opencode-shell--render-turns))
       (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
       (should (< (save-excursion
                    (goto-char (point-min))
                    (search-forward "PERMISSION ALWAYS")
                    (point))
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "finished")
                    (point))))
       (should (< (opencode-shell--turn-user-end turn)
                  (opencode-shell--turn-response-begin turn)))
      (goto-char opencode-shell--composer-start)
      (insert "draft")
      (goto-char (+ opencode-shell--composer-start 2))
      (setq opencode-shell--turns nil
            opencode-shell--rendered-turns (list turn))
      (opencode-shell--render-turns)
      (should (= 1 (how-many "PERMISSION ALWAYS" (point-min) (point-max))))
      (should (equal "draft" (opencode-shell--composer-text)))
      (should (= 2 (- (point) opencode-shell--composer-start))))))

(ert-deftest opencode-shell-reply-success-refreshes-permissions-even-when-blocked ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash") (patterns . ("first")))
       ((id . "p2") (sessionID . "s") (permission . "bash") (patterns . ("second")))))
    (let (paths)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &rest _)
                   (push path paths)
                   (when (equal path "/permission/p1/reply") (funcall callback nil)))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-once)
        (should (member "/permission" paths))))))

(ert-deftest opencode-shell-reply-defers-refresh-when-permission-poll-in-flight ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--in-flight '((permissions . t)))
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash") (patterns . ("first")))))
    (let (paths)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &rest _)
                   (push path paths)
                   (when (equal path "/permission/p1/reply") (funcall callback nil)))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-once)
        (should-not (member "/permission" paths))
        (should opencode-shell--permission-refresh-pending)
        (setf (alist-get 'permissions opencode-shell--in-flight) nil)
        (opencode-shell--receive-permissions nil)
        (should (member "/permission" paths))
        (should-not opencode-shell--permission-refresh-pending)))))

(ert-deftest opencode-shell-reply-defers-refresh-until-failed-permission-poll-settles ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--in-flight '((permissions . t)))
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash") (patterns . ("first")))))
    (let (paths)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &rest _)
                   (push path paths)
                   (when (equal path "/permission/p1/reply") (funcall callback nil)))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-once)
        (should opencode-shell--permission-refresh-pending)
        (setf (alist-get 'permissions opencode-shell--in-flight) nil)
        (opencode-shell--consume-permission-refresh-pending)
        (should (member "/permission" paths))
        (should-not opencode-shell--permission-refresh-pending)))))

(ert-deftest opencode-shell-reply-failure-refreshes-permissions-and-reports ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "bash") (patterns . ("first")))))
    (let (paths reported)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path _callback &optional _body _params error-callback)
                   (push path paths)
                   (when (and error-callback (equal path "/permission/p1/reply"))
                     (funcall error-callback))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq reported (apply #'format fmt args)))))
        (goto-char opencode-shell--permission-begin)
        (opencode-shell--permission-allow-once)
        (should (member "/permission" paths))
        (should (string-match-p "failed" reported))))))

(ert-deftest opencode-shell-message-and-permission-renders-do-not-orphan-cards ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "draft")
    (goto-char (+ opencode-shell--composer-start 2))
    (let ((permission '(((id . "p1") (sessionID . "s")
                         (permission . "bash") (patterns . ("git status")))))
          (messages (list (opencode-shell-test--message "u1" "user" "question")
                          (opencode-shell-test--message "a1" "assistant" "answer" "u1"))))
      (dotimes (_ 5)
        (opencode-shell--receive-permissions permission)
        (opencode-shell--render-messages messages))
      (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))
      (should (= 1 (how-many "git status" (point-min) (point-max))))
      (goto-char opencode-shell--permission-begin)
      (should (search-forward "PERMISSION" opencode-shell--permission-end t))
      (should-not (text-property-any (point-min) opencode-shell--permission-begin
                                     'opencode-shell-permission permission))
      (should (equal (opencode-shell--composer-text) "draft"))
      (goto-char (+ opencode-shell--composer-start 2))
      (should (= (- (point) opencode-shell--composer-start) 2)))))

(ert-deftest opencode-shell-polling-generation-and-capabilities ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s" opencode-shell--generation 1)
    (let (requests stale)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_ path callback &optional _body _params error-callback)
                   (push (list path callback error-callback) requests))))
        (opencode-shell--resync t)
        (should (= (length requests) 7))
        (should opencode-shell--capabilities-loading)
        (should-not opencode-shell--capabilities-loaded)
        ;; A poll while the capability pair is pending must not overlap it.
        (opencode-shell--resync t)
        (should (= (length requests) 7))
        (funcall (cadr (assoc "/provider" requests)) '((providers . nil)))
        (should-not opencode-shell--capabilities-loaded)
        (funcall (cadr (assoc "/agent" requests)) nil)
        (should opencode-shell--capabilities-loaded)
        (should-not opencode-shell--capabilities-loading)
        (let ((old (cadr (assoc "/session/s/message" requests))))
          (setq opencode-shell--generation 2
                opencode-shell--in-flight '((messages . t)) requests nil)
          (cl-letf (((symbol-function 'opencode-shell--render-messages)
                     (lambda (_) (setq stale t))))
            (funcall old nil))
          (should-not stale)
          (should (alist-get 'messages opencode-shell--in-flight))
          (setq opencode-shell--in-flight nil))
        (opencode-shell--resync)
        (should (equal (mapcar #'car requests)
                         '("/question" "/permission" "/session/status"
                           "/session/s/message")))))))

(ert-deftest opencode-shell-periodic-resync-excludes-session-metadata ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--generation 1
          opencode-shell--capabilities-loaded nil)
    (let (paths)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path _callback &rest _)
                   (push path paths))))
        (opencode-shell--resync nil)
        (should (equal (sort paths #'string<)
                       '("/permission" "/question" "/session/s/message"
                         "/session/status")))
        (should-not (member "/session" paths))
        (should-not (member "/agent" paths))
        (should-not (member "/provider" paths))))))

(ert-deftest opencode-shell-question-snapshot-is-session-scoped-and-blocking ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--composer-visible nil)
    (opencode-shell--receive-questions
     '(((id . "q1") (sessionID . "other"))
       ((id . "q2") (sessionId . "s"))
       ((id . "q2") (sessionID . "s"))))
    (setq opencode-shell--submit-in-flight "user-1")
    (opencode-shell--render-messages
     (cadr opencode-shell-test--completion-polling-snapshots) 1)
    (should (equal (mapcar #'opencode-shell--question-id
                           opencode-shell--questions-pending)
                   '("q2")))
    (should opencode-shell--submit-in-flight)
    (should-not opencode-shell--composer-visible)
    (opencode-shell--receive-questions nil)
    (opencode-shell--render-messages
     (cadr opencode-shell-test--completion-polling-snapshots) 2)
    (should-not opencode-shell--submit-in-flight)
    (should opencode-shell--composer-visible)))

(ert-deftest opencode-shell-capabilities-retry-after-transient-failure ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s" opencode-shell--generation 1)
    (let (requests)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &optional _body _params error-callback)
                   (push (list path callback error-callback) requests))))
        (opencode-shell--resync t)
        (funcall (nth 2 (assoc "/provider" requests)))
        (should opencode-shell--capabilities-loading)
        (funcall (cadr (assoc "/agent" requests)) nil)
        (should-not opencode-shell--capabilities-loading)
        (should-not opencode-shell--capabilities-loaded)
        (setq requests nil)
        (opencode-shell--resync t)
        ;; The still-pending message poll remains guarded; only the failed
        ;; capability batch is retried.
        (should (= (length requests) 2))
        (should (assoc "/provider" requests))
        (should (assoc "/agent" requests))))))

(ert-deftest opencode-shell-poll-error-releases-guard ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--generation 7)
    (let (failed)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path _callback &optional _body _params error-callback)
                   (setq failed error-callback))))
        (opencode-shell--guarded-request 'messages "GET" "/message" #'ignore)
        (should (alist-get 'messages opencode-shell--in-flight))
        (funcall failed)
        (should-not (alist-get 'messages opencode-shell--in-flight))))))

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

(ert-deftest opencode-shell-reopen-keeps-one-timer ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (opencode-shell-async--animation-subscribers
         (make-hash-table :test #'eq :weakness 'key))
        (opencode-shell-async--animation-timer nil)
        timers cancelled opened)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq opened buffer)))
              ((symbol-function 'opencode-shell--resync) #'ignore)
              ((symbol-function 'opencode-shell-async--connect) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _) (let ((timer (list 'timer))) (push timer timers) timer)))
              ((symbol-function 'timerp) (lambda (value) (eq (car-safe value) 'timer)))
              ((symbol-function 'cancel-timer) (lambda (timer) (push timer cancelled))))
      (unwind-protect
          (progn
             (opencode-shell-open-session "timer-test")
             (opencode-shell-open-session "timer-test")
             ;; Reopening tears down the last subscription and recreates the
             ;; shared runtime timers without leaving duplicates active.
             (should (= (length timers) 4))
             (should (= (length cancelled) 2)))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest opencode-shell-animation-is-ui-only-resettable-and-deduplicated ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((turn (opencode-shell--make-turn :id "t" :user "q" :status 'waiting))
          (opencode-shell-async--runtimes (make-hash-table :test #'equal))
          (opencode-shell-async--animation-subscribers
           (make-hash-table :test #'eq :weakness 'key))
          (opencode-shell-async--animation-timer nil)
          timers cancelled network-calls)
      (setq opencode-shell--turns (list turn)
            opencode-shell--animation-frame 7)
      (opencode-shell--render-turns)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest args)
                   (let ((timer (cons 'timer args)))
                     (push timer timers)
                     timer)))
                ((symbol-function 'timerp)
                 (lambda (value) (eq (car-safe value) 'timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                ((symbol-function 'opencode-shell--resync)
                 (lambda (&rest _) (cl-incf network-calls)))
                ((symbol-function 'opencode-shell-async--connect) #'ignore))
        (opencode-shell--start-polling)
        (should (= opencode-shell--animation-frame 0))
        (should (= 2 (length timers)))
        (should (member opencode-shell-poll-interval (mapcar #'cadr timers)))
        (should (member opencode-shell-animation-interval (mapcar #'cadr timers)))
        (should (string-match-p (regexp-quote (make-string 1 opencode-shell--spinner-character))
                                (buffer-string)))
        (opencode-shell--start-polling)
        (should (= 2 (length timers)))
        (let ((animation (seq-find
                          (lambda (timer)
                            (= (cadr timer) opencode-shell-animation-interval))
                          timers)))
          (funcall (nth 3 animation)))
        (should (= opencode-shell--animation-frame 1))
        (should-not network-calls)
        (should (string-match-p (regexp-quote (make-string 2 opencode-shell--spinner-character))
                                (buffer-string)))
        (opencode-shell--stop-polling)
        (should-not opencode-shell--poll-timer)
        (should-not opencode-shell--animation-timer)
        (should (= 2 (length cancelled)))
        (setq timers nil opencode-shell--animation-frame 6)
        (opencode-shell--start-polling)
        (should (= opencode-shell--animation-frame 0))
        (opencode-shell--stop-polling)))))

(ert-deftest opencode-shell-animation-keeps-permission-before-status ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "q")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p1") (type . "text") (text . "partial")))))))
    (opencode-shell--receive-permissions
     '(((id . "p") (sessionID . "s") (permission . "bash"))))
    (goto-char opencode-shell--composer-start)
    (insert "draft")
    (goto-char (+ opencode-shell--composer-start 2))
    (dotimes (_ 20) (opencode-shell--animation-tick))
    (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))
    (should (= 1 (how-many "Receiving" (point-min) (point-max))))
    (should (equal (opencode-shell--composer-text) "draft"))
    (should (= 2 (- (point) opencode-shell--composer-start)))
    (goto-char (point-min))
    (should (search-forward "┌─ PERMISSION"))
    (should (search-forward "Receiving"))
    (opencode-shell--stop-polling)))

(ert-deftest opencode-shell-animation-preserves-anchored-result-markers ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((turn (opencode-shell--make-turn :id "t" :user "q" :status 'waiting)))
      (setq opencode-shell--turns (list turn)
            opencode-shell--resolved-permissions
            '(((id . "p") (reply . "once") (description . "bash")
               (after-turn-id . "t"))))
      (opencode-shell--render-turns)
      (let ((user-end (marker-position (opencode-shell--turn-user-end turn))))
        (goto-char opencode-shell--composer-start)
        (insert "draft")
        (goto-char (+ opencode-shell--composer-start 2))
        (dotimes (_ 20) (opencode-shell--animation-tick))
        (should (= user-end
                   (marker-position (opencode-shell--turn-user-end turn))))
        (should (< (opencode-shell--turn-user-end turn)
                   (opencode-shell--turn-response-begin turn)))
        (should (= 1 (how-many "PERMISSION ONCE" (point-min) (point-max))))
        (should (= 1 (how-many "Waiting for response" (point-min) (point-max))))
        (should (equal (opencode-shell--composer-text) "draft"))
        (should (= 2 (- (point) opencode-shell--composer-start)))))))

(ert-deftest opencode-shell-open-focuses-composer-in-selected-window ()
  (let (opened point insert-state)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq opened buffer) (set-buffer buffer)))
              ((symbol-function 'opencode-shell--resync) #'ignore)
              ((symbol-function 'opencode-shell--start-polling) #'ignore)
              ((symbol-function 'evil-insert-state)
               (lambda () (setq insert-state t))))
      (unwind-protect
          (progn
            (opencode-shell-open-session "focus-test" default-directory)
            (setq point (point))
            (should (= point (with-current-buffer opened (point-max))))
            (should insert-state))
        (when (buffer-live-p opened) (kill-buffer opened))))))

(ert-deftest opencode-shell-completion-stops-polling ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((timer (run-at-time 60 nil #'ignore))
          (animation (run-at-time 60 nil #'ignore)))
      (unwind-protect
          (progn
            (setq opencode-shell--poll-timer timer
                  opencode-shell--animation-timer animation)
            (opencode-shell--render-messages nil)
            (should-not opencode-shell--poll-timer)
            (should-not opencode-shell--animation-timer)
            (should-not (memq timer timer-list))
            (should-not (memq animation timer-list)))
        (when (timerp timer) (cancel-timer timer))
        (when (timerp animation) (cancel-timer animation))))))

(ert-deftest opencode-shell-evil-setup-load-orders ()
  (let (calls)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (&rest args) (push (cons 'state args) calls)))
              ((symbol-function 'evil-define-key*)
               (lambda (&rest args) (push (cons 'key args) calls))))
      (opencode-shell--setup-evil)
      (should (= (length calls) 6))
      (let ((insert-call
             (seq-find
              (lambda (call)
                (and (eq (car call) 'key)
                     (eq (cadr call) 'insert)
                     (eq (nth 2 call) opencode-shell-mode-map)))
              calls)))
        (should insert-call)
        (should (memq #'self-insert-command insert-call))
        (should (= 2 (cl-count #'newline insert-call))))
      (let ((sessions-call
             (seq-find
              (lambda (call)
                (and (eq (car call) 'key)
                     (eq (nth 3 call) opencode-shell-sessions-mode-map)))
              calls)))
        (let ((bindings (nthcdr 4 sessions-call)))
          (while bindings
            (should (eq (cadr bindings)
                        (pcase (key-description (car bindings))
                          ("RET" #'opencode-shell--open-at-point)
                          ("g r" #'opencode-shell--refresh)
                           ("c" #'opencode-shell--create-session)
                           ("/" #'opencode-shell--filter)
                           ("T" #'opencode-shell--toggle-child-sessions)
                           ("d" #'opencode-shell--delete-session)
                          ("?" #'opencode-shell-sessions-help))))
            (setq bindings (cddr bindings))))))))

(ert-deftest opencode-shell-uses-editable-base-with-native-character-input ()
  (with-temp-buffer
    (opencode-shell-mode)
    (should (derived-mode-p 'text-mode))
    (should-not (derived-mode-p 'special-mode))
    (should-not (lookup-key opencode-shell-mode-map (kbd "i")))
    (should-not (lookup-key opencode-shell-mode-map (kbd "한")))
    (should-not (lookup-key opencode-shell-mode-map (kbd "?")))
    (should-not (lookup-key opencode-shell-mode-map (kbd "RET")))
    (should-not (fboundp 'opencode-shell-self-insert))
    (should-not (fboundp 'opencode-shell-newline))))

(ert-deftest opencode-shell-selections-are-buffer-local ()
  (let ((a (generate-new-buffer " *oc-a*")) (b (generate-new-buffer " *oc-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (opencode-shell-mode)
                               (setq opencode-shell--selected-agent "build"))
          (with-current-buffer b (opencode-shell-mode)
                               (should-not opencode-shell--selected-agent)))
      (kill-buffer a) (kill-buffer b))))

(ert-deftest opencode-shell-selection-persists-from-ui-through-next-request ()
  (let ((a (generate-new-buffer " *oc-selection-a*"))
        (b (generate-new-buffer " *oc-selection-b*"))
        (provider-response
         '((connected . ("p"))
           (all . (((id . "p")
                    (models . (("one" . ((id . "one")))
                               ("two" . ((id . "two"))))))))))
        (agent-response
         '(((name . "build") (mode . "primary")
            (model . ((providerID . "p") (modelID . "one"))))
           ((name . "plan") (mode . "primary")))))
    (unwind-protect
        (progn
          (with-current-buffer b (opencode-shell-mode))
          (with-current-buffer a
            (opencode-shell-mode)
            (setq opencode-shell--session-id "s"
                  opencode-shell--models
                  (opencode-shell--normalize-models provider-response)
                  opencode-shell--agents
                  (opencode-shell--normalize-agents agent-response))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt &rest _)
                         (if (string-prefix-p "Agent" prompt) "plan" "p/two"))))
              (opencode-shell--select-agent)
              (opencode-shell--select-model))
            (should (string-match-p "agent:plan" (opencode-shell--header)))
            (should (string-match-p "model:p/two" (opencode-shell--header)))
            (let (requests payload)
              (cl-letf (((symbol-function 'opencode-shell--request)
                         (lambda (method path callback &optional body _params error-callback)
                           (if (equal method "POST")
                               (setq payload body)
                             (push (list path callback error-callback) requests)))))
                (opencode-shell--resync t)
                (funcall (cadr (assoc "/agent" requests)) agent-response)
                (funcall (cadr (assoc "/provider" requests)) provider-response)
                (should (equal opencode-shell--selected-agent "plan"))
                (should (equal opencode-shell--selected-model
                               '((providerID . "p") (modelID . "two"))))
                (insert "hello")
                (opencode-shell--submit)
                (should (equal (alist-get 'agent payload) "plan"))
                (should (equal (alist-get 'model payload)
                               '((providerID . "p") (modelID . "two")))))))
          (with-current-buffer b
            (should-not opencode-shell--selected-agent)
            (should-not opencode-shell--selected-model)))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest opencode-shell-selection-rejects-unavailable-completion-value ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--models
          (opencode-shell--normalize-models
           '((all . (((id . "p") (models . (("one" . ((id . "one"))))))))))
          opencode-shell--agents
          (opencode-shell--normalize-agents '(((name . "build")))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "stale")))
      (should-error (opencode-shell--select-model) :type 'user-error)
      (should-error (opencode-shell--select-agent) :type 'user-error))
    (should-not opencode-shell--selected-model)
    (should-not opencode-shell--selected-agent)))

(ert-deftest opencode-shell-markdown-preserves-raw-unicode-and-fences ()
  (with-temp-buffer
    (setq font-lock-defaults '(opencode-shell-render-font-lock-keywords t))
    (let ((raw "# 제목\n\n- 목록과 **굵게** 및 [링크](https://example.com)\n\n> 원문\n\n```elisp\n(message \"안녕\")\n```\n"))
      (opencode-shell-render-insert raw)
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) raw))
      (should (get-text-property (string-match "message" raw) 'face)))))

(ert-deftest opencode-shell-markdown-table-layout-fits-unicode-content ()
  (let* ((raw "앞\n| 책임 | 위치 |\n|---|---|\n| JSON 로그 생성 | `translator-api` |\n| OpenSearch 자격증명 | K8s Secret, 실제 값은 외부 주입 |\n뒤")
         (wide (opencode-shell-render-tables raw 72))
         (narrow (opencode-shell-render-tables raw 36)))
    (should (string-prefix-p "앞\n| 책임" wide))
    (should (string-suffix-p "\n뒤" wide))
    (dolist (line (split-string narrow "\n" t))
      (should (<= (string-width line) 36)))
    (should (string-match-p "실제 값" narrow))
    (should (string-match-p "외부" narrow))
    (should (string-match-p "주입" narrow))))

(ert-deftest opencode-shell-markdown-table-layout-leaves-other-input-exact ()
  (dolist (raw '("문장 | 그대로\n다음 문장\n"
                 "| header | value |\n| -- | --- |\n| incomplete | row |\n"
                 "```text\n| header | value |\n| --- | --- |\n| code | only |\n```\n"))
    (should (equal (opencode-shell-render-tables raw 30) raw))))

(ert-deftest opencode-shell-markdown-table-layout-keeps-empty-and-composed-cells ()
  (let* ((raw "| | Value |\n|---|---|\n| é | |\n")
         (rendered (opencode-shell-render-tables raw 20)))
    (should-not (equal rendered raw))
    (should (string-match-p "é" rendered))
    (dolist (line (split-string rendered "\n" t))
      (should (<= (string-width line) 20)))))

(ert-deftest opencode-shell-completed-response-adapts-table-without-changing-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((raw "| 책임 | 위치 |\n|---|---|\n| OpenSearch 자격증명 | K8s Secret, 실제 값은 외부 주입 |")
           (turn (opencode-shell--make-turn
                  :id "table" :assistant raw :status 'complete)))
      (setq opencode-shell--table-render-width 36)
      (let ((display (opencode-shell--response-display turn)))
        (dolist (line (split-string display "\n" t))
          (unless (string= line "ASSISTANT>")
            (should (<= (string-width line) 36))))
        (should (string-match-p "외부" display))
        (should (equal (opencode-shell--turn-assistant turn) raw))))))

(ert-deftest opencode-shell-table-resize-preserves-composer-and-point ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((raw "| 책임 | 위치 |\n|---|---|\n| OpenSearch 자격증명 | K8s Secret, 실제 값은 외부 주입 |")
           (turn (opencode-shell--make-turn
                  :id "table" :assistant raw :status 'complete)))
      (setq opencode-shell--turns (list turn)
            opencode-shell--table-render-width 72)
      (goto-char (point-max))
      (insert "작성 중")
      (let ((offset (- (point) opencode-shell--composer-start))
            restored-start)
        (cl-letf (((symbol-function 'opencode-shell--transcript-window)
                   (lambda () (selected-window)))
                  ((symbol-function 'window-body-width) (lambda (&rest _) 36))
                  ((symbol-function 'get-buffer-window-list)
                   (lambda (&rest _) (list (selected-window))))
                  ((symbol-function 'window-start) (lambda (&rest _) (point-min)))
                  ((symbol-function 'set-window-start)
                   (lambda (_window position &rest _)
                     (setq restored-start (marker-position position)))))
          (opencode-shell--refresh-table-layout))
        (should (= opencode-shell--table-render-width 36))
        (should (= restored-start (point-min)))
        (should (equal (opencode-shell--composer-text) "작성 중"))
        (should (= (- (point) opencode-shell--composer-start) offset))
        (should (equal (opencode-shell--turn-assistant turn) raw))
        (goto-char (point-min))
        (should (text-property-search-forward 'read-only t t))))))

(ert-deftest opencode-shell-table-resize-hook-has-buffer-lifecycle ()
  (with-temp-buffer
    (opencode-shell-mode)
    (should (memq #'opencode-shell--refresh-table-layout
                  window-configuration-change-hook))
    (opencode-shell--cleanup)
    (should-not (memq #'opencode-shell--refresh-table-layout
                      window-configuration-change-hook))))

(ert-deftest opencode-shell-keymaps-and-cleanup ()
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-y"))
              #'opencode-shell--permission-allow-once))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-l"))
              #'opencode-shell--permission-allow-always))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-n"))
              #'opencode-shell--permission-reject))
  (should-not (lookup-key opencode-shell-mode-map (kbd "p")))
  (should-not (fboundp 'opencode-shell-prompt))
  (should (eq (lookup-key opencode-shell-sessions-mode-map (kbd "RET"))
              #'opencode-shell--open-at-point))
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--poll-timer (run-at-time 60 nil #'ignore))
    (setq opencode-shell--generation 4
          opencode-shell--in-flight '((messages . t))
          opencode-shell--capabilities-loading t)
    (opencode-shell--cleanup)
    (should-not opencode-shell--poll-timer)
    (should-not opencode-shell--in-flight)
    (should-not opencode-shell--capabilities-loading)
    (should (= opencode-shell--generation 5))))

(ert-deftest opencode-shell-private-mode-commands-are-hidden-from-completion ()
  (dolist (command opencode-shell--mode-commands)
    (should (commandp command))
    (should-not (funcall (get command 'completion-predicate) command nil)))
  (dolist (command '(opencode-shell opencode-shell-status
                     opencode-shell-restart opencode-shell-reload))
    (should (commandp command))
    (should-not (get command 'completion-predicate)))
  (should (eq (lookup-key opencode-shell-sessions-mode-map (kbd "RET"))
              'opencode-shell--open-at-point))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-c"))
              'opencode-shell--submit)))

(defun opencode-shell-test--message (id role text &optional parent incomplete)
  "Build a minimal message envelope for conversation tests."
  `((info . ((id . ,id) (role . ,role) ,@(and parent `((parentID . ,parent)))
             ,@(and (equal role "assistant") (not incomplete)
                    '((finish . "stop") (time . ((completed . 2)))))))
    (parts . (((id . ,(concat id "-text")) (type . "text") (text . ,text))))))

(defun opencode-shell-test--tool-message (id parent &optional status)
  "Build a tool-only assistant envelope for conversation tests."
  `((info . ((id . ,id) (role . "assistant") (parentID . ,parent)))
    (parts . (((id . ,(concat id "-tool")) (type . "tool") (tool . "read")
               (state . ((status . ,(or status "completed")))))))))

(ert-deftest opencode-shell-reconciles-identical-local-prompts-in-order ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((first (opencode-shell--make-turn :id "local-1" :user "same" :status 'waiting))
          (second (opencode-shell--make-turn :id "local-2" :user "same" :status 'waiting)))
      (setq opencode-shell--turns (list first second))
      (opencode-shell--render-messages
       (list (opencode-shell-test--message "u1" "user" "same")
             (opencode-shell-test--message "u2" "user" "same")))
      (should (= (length opencode-shell--turns) 2))
      (should (eq (nth 0 opencode-shell--turns) first))
      (should (eq (nth 1 opencode-shell--turns) second))
      (should (equal (mapcar #'opencode-shell--turn-server-user-id opencode-shell--turns)
                     '("u1" "u2"))))))

(ert-deftest opencode-shell-superseded-history-orphan-stops-polling ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((timer (run-at-time 60 nil #'ignore)) events)
      (unwind-protect
          (progn
            (setq opencode-shell--poll-timer timer)
            (cl-letf (((symbol-function 'opencode-shell--log-lifecycle)
                       (lambda (event &optional _force) (push event events))))
              (opencode-shell--render-messages
               (list (opencode-shell-test--message "u1" "user" "orphan")
                     (opencode-shell-test--message "u2" "user" "new")
                     (opencode-shell-test--message "a2" "assistant" "answer" "u2")))
              (let ((orphan (car opencode-shell--turns)))
                (should (eq (opencode-shell--turn-status orphan) 'complete))
                (should (opencode-shell--turn-locally-settled orphan))
                (should (equal (opencode-shell--turn-terminal-error orphan)
                               "Interrupted: Superseded by a later prompt")))
              (should (equal opencode-shell--request-status "idle"))
              (should-not opencode-shell--poll-timer)
              (should (member "poll-stop" events))))
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest opencode-shell-submit-settles-existing-nonterminal-turns ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--selected-model '((providerID . "p") (modelID . "m"))
          opencode-shell--selected-agent "build"
          opencode-shell--turns
          (list (opencode-shell--make-turn :id "u1" :user "old" :status 'waiting)))
    (insert "new prompt")
    (cl-letf (((symbol-function 'opencode-shell--start-polling) #'ignore)
              ((symbol-function 'opencode-shell--request) (lambda (&rest _))))
      (opencode-shell--submit))
    (should (= (length opencode-shell--turns) 2))
    (let ((old (car opencode-shell--turns))
          (new (cadr opencode-shell--turns)))
      (should (eq (opencode-shell--turn-status old) 'complete))
      (should (opencode-shell--turn-locally-settled old))
      (should (eq (opencode-shell--turn-status new) 'sending))
      (should (equal opencode-shell--submit-in-flight
                     (opencode-shell--turn-id new))))))

(ert-deftest opencode-shell-abort-settlement-survives-stale-snapshot ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--submit-in-flight "u1"
          opencode-shell--composer-visible nil
          opencode-shell--turns
          (list (opencode-shell--make-turn :id "u1" :user "question" :status 'waiting)))
    (let (callback)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path cb &rest _) (setq callback cb)))
                ((symbol-function 'opencode-shell--resync) #'ignore))
        (opencode-shell--abort)
        (funcall callback nil)))
    (let ((turn (car opencode-shell--turns)))
      (should (eq (opencode-shell--turn-status turn) 'complete))
      (should (opencode-shell--turn-locally-settled turn))
      (should (equal (opencode-shell--turn-terminal-error turn)
                     "MessageAbortedError: Aborted")))
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")))
    (should (opencode-shell--turn-locally-settled (car opencode-shell--turns)))
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
    (let ((turn (car opencode-shell--turns)))
      (should (eq (opencode-shell--turn-status turn) 'complete))
      (should-not (opencode-shell--turn-locally-settled turn))
      (should-not (opencode-shell--turn-terminal-error turn)))))

(ert-deftest opencode-shell-abort-callback-does-not-settle-newer-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((target (opencode-shell--make-turn
                   :id "u1" :user "old" :status 'waiting))
          callback)
      (setq opencode-shell--session-id "s"
            opencode-shell--submit-in-flight "u1"
            opencode-shell--composer-visible nil
            opencode-shell--turns (list target))
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path cb &rest _) (setq callback cb)))
                ((symbol-function 'opencode-shell--resync) #'ignore))
        (opencode-shell--abort)
        (let ((new (opencode-shell--make-turn
                    :id "u2" :user "new" :status 'sending)))
          (setq opencode-shell--turns (append opencode-shell--turns (list new))
                opencode-shell--submit-in-flight "u2"
                opencode-shell--request-status "sending")
          (funcall callback nil)
          (should (eq (opencode-shell--turn-status target) 'complete))
          (should (opencode-shell--turn-locally-settled target))
          (should (eq (opencode-shell--turn-status new) 'sending))
          (should (equal opencode-shell--submit-in-flight "u2"))
          (should-not opencode-shell--composer-visible))))))

(ert-deftest opencode-shell-completed-tool-alone-does-not-complete-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((user (opencode-shell-test--message "u1" "user" "question")))
      (opencode-shell--render-messages
       (list user (opencode-shell-test--tool-message "a1" "u1")))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))
      (should (string-empty-p (opencode-shell--turn-assistant (car opencode-shell--turns))))
      (should (equal opencode-shell--request-status "receiving"))
      (opencode-shell--render-messages
       (list user
             (opencode-shell-test--tool-message "a1" "u1")
             (opencode-shell-test--message "a2" "assistant" "answer" "u1")))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))
      (should (equal (opencode-shell--turn-assistant (car opencode-shell--turns)) "answer")))))

(ert-deftest opencode-shell-renders-tool-names-without-payloads ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((messages
           (list
            (opencode-shell-test--message "u1" "user" "change files")
            '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
              (parts . (((id . "tool-1") (type . "tool") (tool . "edit")
                         (state . ((status . "running")
                                   (input . "SECRET EDIT PAYLOAD"))))
                        ((id . "tool-2") (type . "tool_use") (name . "write")
                         (input . "SECRET WRITE PAYLOAD"))))))))
      (dotimes (_ 3) (opencode-shell--render-messages messages))
      (should (= 1 (how-many "TOOL> edit" (point-min) (point-max))))
      (should (= 1 (how-many "TOOL> write" (point-min) (point-max))))
      (should-not (string-match-p "SECRET .* PAYLOAD" (buffer-string)))
      (should (= 1 (how-many "Receiving" (point-min) (point-max)))))))

(ert-deftest opencode-shell-partial-assistant-update-retains-known-parts ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((user (opencode-shell-test--message "u1" "user" "question"))
           (initial '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
                      (parts . (((id . "p1") (type . "text") (text . "answer"))
                                ((id . "p2") (type . "tool") (tool . "read")
                                 (state . ((status . "running"))))))))
           (partial '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
                      (parts . (((id . "p2") (type . "tool") (tool . "read")
                                 (state . ((status . "completed")))))))))
      (opencode-shell--render-messages (list user initial) 1)
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))
      (opencode-shell--render-messages (list user partial) 2)
      (let ((turn (car opencode-shell--turns)))
        (should (equal (opencode-shell--turn-assistant turn) "answer"))
        (should (= (length (opencode-shell--turn-parts turn)) 2))
        (should (eq (opencode-shell--turn-status turn) 'receiving))))))

(ert-deftest opencode-shell-completion-metadata-waits-for-running-tool ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((user (opencode-shell-test--message "u1" "user" "question"))
          (assistant '((info . ((id . "a1") (role . "assistant")
                                (parentID . "u1") (finish . "stop")
                                (time . ((completed . 2)))))
                       (parts . (((id . "p1") (type . "text") (text . "answer"))
                                 ((id . "p2") (type . "tool") (tool . "read")
                                  (state . ((status . "running")))))))))
      (opencode-shell--render-messages (list user assistant))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving)))))

(ert-deftest opencode-shell-step-finish-remains-terminal-evidence ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p1") (type . "text") (text . "answer"))
                       ((id . "p2") (type . "step-finish")))))))
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))))

(ert-deftest opencode-shell-tool-calls-finish-step-does-not-complete-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                      (finish . "tool-calls") (time . ((created . 1) (completed . 2)))))
             (parts . (((id . "p1") (type . "tool") (tool . "read")
                        (state . ((status . "completed"))))
                       ((id . "p2") (type . "step-finish")))))))
    (should-not (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))))

(ert-deftest opencode-shell-multi-step-turn-completes-only-at-final-stop-step ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--submit-in-flight "u1"
          opencode-shell--composer-visible nil)
    (let ((user (opencode-shell-test--message "u1" "user" "question"))
          (step1 '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                            (finish . "tool-calls") (time . ((created . 1) (completed . 2)))))
                   (parts . (((id . "p1") (type . "tool") (tool . "read")
                              (state . ((status . "completed"))))
                             ((id . "p2") (type . "step-finish"))))))
          (step2 '((info . ((id . "a2") (role . "assistant") (parentID . "u1")
                            (finish . "stop") (time . ((created . 3) (completed . 4)))))
                   (parts . (((id . "p3") (type . "text") (text . "answer"))
                             ((id . "p4") (type . "step-finish"))))))
          (timer (run-at-time 60 nil #'ignore)))
      (unwind-protect
          (progn
            (setq opencode-shell--poll-timer timer)
            (opencode-shell--render-messages (list user step1))
            (should-not (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))
            ;; The premature-completion bug would restore the composer and
            ;; cancel polling right after this first (non-final) step.
            (should (timerp opencode-shell--poll-timer))
            (should opencode-shell--submit-in-flight)
            (should-not opencode-shell--composer-visible)
            (opencode-shell--render-messages (list user step1 step2))
            (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))
            (should (equal (opencode-shell--turn-assistant (car opencode-shell--turns)) "answer"))
            (should-not opencode-shell--submit-in-flight)
            (should opencode-shell--composer-visible)
            (should-not opencode-shell--poll-timer))
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest opencode-shell-error-terminated-message-completes-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                      (time . ((created . 1) (completed . 2)))
                      (error . ((name . "MessageAbortedError")
                                (data . ((message . "Aborted")))))))
             (parts . nil))))
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))
    (should (equal (opencode-shell--turn-terminal-error (car opencode-shell--turns))
                   "MessageAbortedError: Aborted"))))

(ert-deftest opencode-shell-running-tool-blocks-error-completion ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                      (time . ((created . 1) (completed . 2)))
                      (error . ((name . "MessageAbortedError")
                                (data . ((message . "Aborted")))))))
             (parts . (((id . "p1") (type . "tool") (tool . "read")
                        (state . ((status . "running")))))))))
    (should-not (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))))

(ert-deftest opencode-shell-error-terminated-message-shows-reason ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                      (time . ((created . 1) (completed . 2)))
                      (error . ((name . "MessageAbortedError")
                                (data . ((message . "Aborted")))))))
             (parts . nil))))
    (should (string-match-p "MessageAbortedError: Aborted" (buffer-string)))))

(ert-deftest opencode-shell-normal-completion-has-no-error-suffix ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
    (should-not (opencode-shell--turn-terminal-error (car opencode-shell--turns)))
    (should-not (string-match-p "\\[" (buffer-string)))))

(ert-deftest opencode-shell-sanitized-polling-fixture-completes-authoritatively ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-1"
          opencode-shell--submit-in-flight "user-1"
          opencode-shell--composer-visible nil
          opencode-shell--permissions '(((id . "permission-1")
                                         (sessionID . "session-1"))))
    (let ((timer (run-at-time 60 nil #'ignore)))
      (unwind-protect
          (progn
            (setq opencode-shell--poll-timer timer)
            (opencode-shell--render-messages
             (car opencode-shell-test--completion-polling-snapshots) 1)
            (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                        'receiving))
            (should (timerp opencode-shell--poll-timer))
            (opencode-shell--render-messages
             (cadr opencode-shell-test--completion-polling-snapshots) 2)
            (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                        'complete))
            (should opencode-shell--submit-in-flight)
            (should-not opencode-shell--composer-visible)
            (should (timerp opencode-shell--poll-timer))
            (setq opencode-shell--permissions nil)
            (opencode-shell--render-messages
             (cadr opencode-shell-test--completion-polling-snapshots) 3)
            (should-not opencode-shell--submit-in-flight)
            (should opencode-shell--composer-visible)
            (should-not opencode-shell--poll-timer)
            (should (= 1 (how-many "Prompt> " (point-min) (point-max)))))
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest opencode-shell-partial-envelope-retains-completion-metadata ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((user (opencode-shell-test--message "u1" "user" "question"))
          (complete (opencode-shell-test--message "a1" "assistant" "answer" "u1"))
           (partial '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                               (time . ((created . 1)))))
                      (parts . (((id . "a1-text") (type . "text")
                                 (text . "answer")))))))
      (opencode-shell--render-messages (list user complete) 1)
      (opencode-shell--render-messages (list user partial) 2)
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete)))))

(ert-deftest opencode-shell-later-assistant-envelope-must-be-complete ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((user (opencode-shell-test--message "u1" "user" "question")))
      (opencode-shell--render-messages
       (list user
             (opencode-shell-test--message "a1" "assistant" "first" "u1")
             (opencode-shell-test--message "a2" "assistant" "second" "u1" t)))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving)))))

(ert-deftest opencode-shell-partial-text-does-not-complete-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((user (opencode-shell-test--message "u1" "user" "question"))
          (partial '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
                     (parts . (((id . "p1") (type . "text") (text . "partial")))))))
      (opencode-shell--render-messages (list user partial))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))
      (should (equal opencode-shell--request-status "receiving")))))

(ert-deftest opencode-shell-reasoning-only-response-is-thinking ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "r1") (type . "reasoning") (text . "thinking")))))))
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'thinking))
    (should (equal opencode-shell--request-status "thinking"))
    (should (string-match-p "Thinking" (buffer-string)))))

(ert-deftest opencode-shell-partial-text-keeps-visible-receiving-status ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p1") (type . "text") (text . "partial")))))))
    (should (string-match-p "Receiving" (buffer-string)))
    (should-not (string-match-p "ASSISTANT>" (buffer-string)))))

(ert-deftest opencode-shell-updating-local-turn-keeps-receiving-visible ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((turn (opencode-shell--make-turn :id "u1" :user "question" :status 'waiting)))
      (setq opencode-shell--turns (list turn))
      (opencode-shell--render-turns)
      (opencode-shell--render-messages
       (list (opencode-shell-test--message "u1" "user" "question")
             '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
               (parts . (((id . "p1") (type . "text") (text . "partial")))))))
      (should (string-match-p "Receiving" (buffer-string)))
      (should-not (string-match-p "ASSISTANT>" (buffer-string))))))

(ert-deftest opencode-shell-full-rerender-keeps-one-prompt-label ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
    (set-marker (opencode-shell--turn-user-begin (car opencode-shell--turns)) nil)
    (opencode-shell--render-turns)
    (should (= 1 (how-many "Prompt> " (point-min) (point-max))))))

(ert-deftest opencode-shell-stale-response-marker-triggers-full-rerender ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
    (let* ((turn (car opencode-shell--turns))
           (stale-end (copy-marker (1+ (point-max)))))
      (setf (opencode-shell--turn-response-end turn) stale-end)
      (opencode-shell--render-turns)
      (should (= 1 (how-many "ASSISTANT>" (point-min) (point-max))))
      (should (string-match-p "answer" (buffer-string)))
      (should (opencode-shell--turn-rendered-p turn)))))

(ert-deftest opencode-shell-hides-composer-label-until-response-completes ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((turn (opencode-shell--make-turn :id "u1" :user "hello"
                                            :status 'waiting)))
      (setq opencode-shell--turns (list turn)
            opencode-shell--submit-in-flight "u1"
            opencode-shell--composer-visible nil)
      (opencode-shell--render-turns)
      (should (= 1 (how-many "USER>" (point-min) (point-max))))
      (should (= 0 (how-many "Prompt> " (point-min) (point-max))))
      (setf (opencode-shell--turn-status turn) 'complete)
      (setq opencode-shell--submit-in-flight nil)
      (setq opencode-shell--composer-visible t)
      (opencode-shell--render-turns)
      (should (= 1 (how-many "Prompt> " (point-min) (point-max)))))))

(ert-deftest opencode-shell-identical-completed-poll-is-render-no-op ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((messages (list (opencode-shell-test--message "u1" "user" "question")
                          (opencode-shell-test--message "a1" "assistant" "answer" "u1"))))
      (opencode-shell--render-messages messages)
      (let* ((turn (car opencode-shell--turns))
             (begin (opencode-shell--turn-response-begin turn))
             (end (opencode-shell--turn-response-end turn))
             (before (buffer-string)))
        (let ((inhibit-read-only t))
          (add-text-properties begin end '(opencode-shell-render-token t)))
        (opencode-shell--render-messages messages)
        (should (equal before (buffer-string)))
        (should (get-text-property begin 'opencode-shell-render-token))
        (should (= 1 (how-many "Prompt> " (point-min) (point-max))))))))

(ert-deftest opencode-shell-hidden-response-reconciles-before-visible-idle-render ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((messages (list (opencode-shell-test--message "u1" "user" "question")
                           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
           (before (buffer-string))
           visible)
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) visible)))
        (opencode-shell--render-messages messages 1 t)
        (should (= (length opencode-shell--turns) 1))
        (should opencode-shell--render-dirty)
        (should (equal before (buffer-string)))
        (setq visible t)
        (opencode-shell--render-if-visible)
        (opencode-shell-async-drain (current-buffer))
        (should-not opencode-shell--render-dirty)
        (should (string-match-p "answer" (buffer-string)))))))

(ert-deftest opencode-shell-history-poll-advances-buffer-local-heartbeat ()
  (let ((first (generate-new-buffer " *heartbeat-1*"))
        (second (generate-new-buffer " *heartbeat-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'opencode-shell--guarded-request) #'ignore))
          (with-current-buffer first
            (opencode-shell-mode)
            (setq opencode-shell--session-id "s")
            (dotimes (_ 4) (opencode-shell--resync))
            (should (= opencode-shell--poll-heartbeat 1))
            (should (equal (opencode-shell--status-display "Waiting")
                           (format "Waiting %s\n\n"
                                   (make-string 1 opencode-shell--spinner-character)))))
          (with-current-buffer second
            (opencode-shell-mode)
            (should (= opencode-shell--poll-heartbeat 0))
            (should (= opencode-shell--animation-frame 0))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest opencode-shell-history-poll-restarts-growing-animation ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--animation-frame 7
          opencode-shell--turns
          (list (opencode-shell--make-turn :id "t" :user "q" :status 'waiting)))
    (opencode-shell--render-turns)
    (cl-letf (((symbol-function 'opencode-shell--guarded-request) #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
      (opencode-shell--resync)
      (opencode-shell-async-drain (current-buffer)))
    (should (= opencode-shell--animation-frame 0))
    (should (string-match-p
             (format "Waiting for response %s"
                     (regexp-quote (make-string 1 opencode-shell--spinner-character)))
             (buffer-string)))
    (opencode-shell--animation-tick)
    (should (string-match-p
             (format "Waiting for response %s"
                     (regexp-quote (make-string 2 opencode-shell--spinner-character)))
             (buffer-string)))))

(ert-deftest opencode-shell-animation-keeps-growing-until-next-poll ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--turns
          (list (opencode-shell--make-turn :id "t" :user "q" :status 'waiting)))
    (opencode-shell--render-turns)
    (dotimes (_ 12) (opencode-shell--animation-tick))
    (should (= opencode-shell--animation-frame 12))
    (should (string-match-p
             (format "Waiting for response %s"
                     (regexp-quote
                      (make-string 13 opencode-shell--spinner-character)))
             (buffer-string)))))

(ert-deftest opencode-shell-overlapping-resync-does-not-advance-heartbeat ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--in-flight '((messages . t)))
    (cl-letf (((symbol-function 'opencode-shell--guarded-request) #'ignore))
      (opencode-shell--resync)
      (should (= opencode-shell--poll-heartbeat 0)))))

(ert-deftest opencode-shell-hidden-resync-does-not-touch-buffer-text ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--turns
          (list (opencode-shell--make-turn :id "t" :user "q" :status 'waiting)))
    (opencode-shell--render-turns)
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'opencode-shell--guarded-request) #'ignore)
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
        (opencode-shell--resync)
        (should (equal before (buffer-string)))
        (should opencode-shell--render-dirty)))))

(ert-deftest opencode-shell-missing-status-does-not-complete-running-tool ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--tool-message "a1" "u1" "running")))
    (opencode-shell--complete-idle-turn)
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))))

(ert-deftest opencode-shell-idle-status-never-completes-text-response ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p1") (type . "text") (text . "answer")))))))
    (setq opencode-shell--session-status '((s . ((type . "idle"))))
          opencode-shell--submit-in-flight "u1"
          opencode-shell--composer-visible nil)
    (opencode-shell--complete-idle-turn)
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))
    (should opencode-shell--submit-in-flight)
    (opencode-shell--complete-idle-turn)
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving))
    (should opencode-shell--submit-in-flight)
    (should-not opencode-shell--composer-visible)))

(ert-deftest opencode-shell-permission-blocks-idle-completion-and-prompt ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--submit-in-flight "u1"
          opencode-shell--composer-visible nil
          opencode-shell--permissions '(((id . "p1") (sessionID . "s")))
          opencode-shell--session-status '((s . ((type . "idle")))))
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p") (type . "text") (text . "answer")))))))
    (opencode-shell--complete-idle-turn)
    (opencode-shell--complete-idle-turn)
    (should opencode-shell--submit-in-flight)
    (should-not opencode-shell--composer-visible)
    (should (= opencode-shell--idle-completion-count 0))))

(ert-deftest opencode-shell-permission-precedes-relocated-response-spinner ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((id . "p1") (type . "text") (text . "partial")))))))
    (opencode-shell--receive-permissions
     '(((id . "permission") (sessionID . "s") (permission . "read"))))
    (let ((permission (save-excursion (goto-char (point-min)) (search-forward "PERMISSION")))
          (spinner (save-excursion (goto-char (point-min)) (search-forward "Receiving"))))
      (should (< permission spinner))
      (should (= 1 (how-many "Receiving" (point-min) (point-max))))
      (should (<= opencode-shell--permission-status-begin spinner))
      (should (<= spinner opencode-shell--permission-status-end)))
    (opencode-shell--receive-permissions nil)
    (should (= 1 (how-many "Receiving" (point-min) (point-max))))
    (should-not (search-forward "PERMISSION" nil t))))

(ert-deftest opencode-shell-authoritative-completion-waits-for-permission-settlement ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--submit-in-flight "u1"
          opencode-shell--composer-visible nil
          opencode-shell--permissions '(((id . "p1") (sessionID . "s"))))
    (let ((messages (list (opencode-shell-test--message "u1" "user" "question")
                          (opencode-shell-test--message "a1" "assistant" "answer" "u1"))))
      (opencode-shell--render-messages messages)
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete))
      (should opencode-shell--submit-in-flight)
      (should-not opencode-shell--composer-visible)
      (setq opencode-shell--permissions nil)
      (opencode-shell--render-messages messages)
      (should-not opencode-shell--submit-in-flight)
      (should opencode-shell--composer-visible)
      (should (= 1 (how-many "Prompt> " (point-min) (point-max)))))))

(ert-deftest opencode-shell-part-field-merge-retains-omitted-fields ()
  (let* ((known '((id . "p1") (type . "tool") (tool . "read")
                  (state . ((status . "running"))) (metadata . ((path . "x")))))
         (incoming '((id . "p1") (state . ((status . "completed")))))
         (merged (car (opencode-shell--merge-parts (list known) (list incoming)))))
    (should (equal (opencode-shell--get merged 'tool) "read"))
    (should (equal (opencode-shell--get merged 'metadata) '((path . "x"))))
    (should (equal (opencode-shell--get (opencode-shell--get merged 'state) 'status)
                   "completed"))))

(ert-deftest opencode-shell-composer-boundary-is-multiline-and-transcript-read-only ()
  (with-temp-buffer
    (opencode-shell-mode)
    (insert "first\nsecond")
    (should (equal (opencode-shell--composer-text) "first\nsecond"))
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "sent")
           (opencode-shell-test--message "a1" "assistant" "reply" "u1")))
    (let ((turn (car opencode-shell--turns)))
      (should (marker-position (opencode-shell--turn-user-begin turn)))
      (should (marker-position (opencode-shell--turn-user-end turn)))
      (should (marker-position (opencode-shell--turn-response-begin turn)))
      (should (marker-position (opencode-shell--turn-response-end turn)))
      (should (eq (get-text-property
                   (opencode-shell--turn-user-begin turn) 'read-only) t))
      (should (eq (get-text-property
                   (opencode-shell--turn-response-begin turn) 'read-only) t)))
    (should-error (let ((inhibit-read-only nil))
                     (goto-char (point-min)) (insert "x"))
                   :type 'text-read-only)
    (should-error (let ((inhibit-read-only nil))
                    (delete-region (1- opencode-shell--composer-start)
                                   (1+ opencode-shell--composer-start)))
                  :type 'text-read-only)
    (goto-char (point-max))
    (insert "\nthird")
    (should (equal (opencode-shell--composer-text) "first\nsecond\nthird"))))

(ert-deftest opencode-shell--submit-commits-clears-and-restores-on-failure ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--selected-model '((providerID . "p") (modelID . "m"))
          opencode-shell--selected-agent "build")
    (insert "hello\nworld")
    (let (request failure)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (method path callback &optional body _params error-callback)
                   (setq request (list method path callback body)
                         failure error-callback))))
        (opencode-shell--submit)
        (should (equal (car request) "POST"))
        (should (equal (cadr request) "/session/s/prompt_async"))
        (should (equal (alist-get 'agent (nth 3 request)) "build"))
        (should (equal (alist-get 'model (nth 3 request)) opencode-shell--selected-model))
        (should (string-empty-p (opencode-shell--composer-text)))
        (should (equal (opencode-shell--turn-user (car opencode-shell--turns)) "hello\nworld"))
        (should (eq (get-text-property (point-min) 'read-only) t))
        (funcall failure)
        (insert "retry")
        (should-error (opencode-shell--submit) :type 'user-error)
        (should (equal (opencode-shell--composer-text) "retry"))
        (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'recovering))
         (should (equal opencode-shell--request-status "recovering"))))))

(ert-deftest opencode-shell-transcript-renders-preserve-composer-undo ()
  (with-temp-buffer
    (opencode-shell-mode)
    (buffer-enable-undo)
    (insert "draft")
    (undo-boundary)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "question")
           (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
    (opencode-shell--receive-permissions
     '(((id . "p1") (permission . "bash") (patterns . ("git status")))))
    (undo-only 1)
    (should (string-empty-p (opencode-shell--composer-text)))
    (should (= 1 (how-many "USER>" (point-min) (point-max))))
    (should (= 1 (how-many "ASSISTANT>" (point-min) (point-max))))
    (should (= 1 (how-many "┌─ PERMISSION" (point-min) (point-max))))))

(ert-deftest opencode-shell-mode-and-initial-transcript-have-no-undo-history ()
  (let ((buffer (generate-new-buffer " *oc-initial-undo*")))
    (unwind-protect
        (with-current-buffer buffer
          (buffer-enable-undo)
          (opencode-shell-mode)
          (should-not buffer-undo-list)
          (opencode-shell--render-messages
           (list (opencode-shell-test--message "u1" "user" "question")
                 (opencode-shell-test--message "a1" "assistant" "answer" "u1")))
          (should-not buffer-undo-list)
          (should-error (undo-only 1) :type 'user-error)
          (should (= 1 (how-many "USER>" (point-min) (point-max))))
          (should (= 1 (how-many "ASSISTANT>" (point-min) (point-max)))))
      (kill-buffer buffer))))

(ert-deftest opencode-shell-shifts-documented-composer-undo-entry-shapes ()
  (let* ((threshold 10)
         (delta 7)
         (property-entry '(nil face bold 10 . 12)))
    (should (= (opencode-shell--shift-undo-entry 11 threshold delta) 18))
    (should (equal (opencode-shell--shift-undo-entry '(10 . 13) threshold delta)
                   '(17 . 20)))
    (should (equal (opencode-shell--shift-undo-entry '("x" . -10) threshold delta)
                   '("x" . -17)))
    (should (equal (opencode-shell--shift-undo-entry property-entry threshold delta)
                   '(nil face bold 17 . 19)))
    (should (equal (opencode-shell--shift-undo-entry
                    '(apply 1 10 12 delete-region 10 12) threshold delta)
                   '(apply 1 17 19 delete-region 10 12)))
    (should (equal (opencode-shell--shift-undo-entry
                    '(apply function 10 12) threshold delta)
                   '(apply function 10 12)))))

(ert-deftest opencode-shell-submit-resets-undo-before-new-composer-edits ()
  (with-temp-buffer
    (opencode-shell-mode)
    (buffer-enable-undo)
    (setq opencode-shell--session-id "s")
    (insert "committed")
    (cl-letf (((symbol-function 'opencode-shell--request) #'ignore))
      (opencode-shell--submit))
    (should-error (undo-only 1) :type 'user-error)
    (insert "new draft")
    (undo-boundary)
    (undo-only 1)
    (should (string-empty-p (opencode-shell--composer-text)))
    (should (= 1 (how-many "committed" (point-min) (point-max))))
    (should (= 1 (how-many "USER>" (point-min) (point-max))))))

(ert-deftest opencode-shell-failure-does-not-overwrite-edited-composer ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "failed prompt")
    (let (failure)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path _callback &optional _body _params error-callback)
                   (setq failure error-callback))))
        (opencode-shell--submit)
        (insert "new draft")
        (goto-char (+ opencode-shell--composer-start 3))
        (funcall failure)
        (should (equal (opencode-shell--composer-text) "new draft"))
        (should (= (- (point) opencode-shell--composer-start) 3))))))

(ert-deftest opencode-shell-out-of-order-failures-do-not-restore-or-reorder-prompts ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (let (failures)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path _callback &optional _body _params error-callback)
                   (setq failures (append failures (list error-callback))))))
        (insert "first")
        (opencode-shell--submit)
        (insert "second")
        (should-error (opencode-shell--submit) :type 'user-error)
        (funcall (car failures))
        (should (equal (opencode-shell--composer-text) "second"))
        (should (equal (mapcar #'opencode-shell--turn-user opencode-shell--turns)
                       '("first")))))))

(ert-deftest opencode-shell-blank-submit-is-rejected-without-request ()
  (with-temp-buffer
    (opencode-shell-mode)
    (insert " \n ")
    (cl-letf (((symbol-function 'opencode-shell--request)
               (lambda (&rest _) (ert-fail "blank prompt requested"))))
      (should-error (opencode-shell--submit) :type 'user-error))
    (should (equal (opencode-shell--composer-text) " \n "))))

(ert-deftest opencode-shell-poll-insertion-preserves-composer-point-faces-and-id ()
  (with-temp-buffer
    (opencode-shell-mode)
    (insert "draft text")
    (goto-char (+ opencode-shell--composer-start 5))
    (let ((messages (list (opencode-shell-test--message "u1" "user" "question")
                          (opencode-shell-test--message "a1" "assistant" "answer" "u1"))))
      (opencode-shell--render-messages messages)
      (let ((id (opencode-shell--turn-id (car opencode-shell--turns))))
        (should (equal (opencode-shell--composer-text) "draft text"))
        (should (= (- (point) opencode-shell--composer-start) 5))
        (goto-char (point-min))
        (search-forward "ASSISTANT>")
        (let* ((turn (car opencode-shell--turns))
               (user-begin (opencode-shell--turn-user-begin turn))
               (user-end (opencode-shell--turn-user-end turn)))
          (opencode-shell--render-messages
           (list (opencode-shell-test--message "u1" "user" "question")
                 (opencode-shell-test--message "a1" "assistant" "updated" "u1")))
          (should (eq turn (car opencode-shell--turns)))
          (should (eq user-begin
                      (opencode-shell--turn-user-begin
                       (car opencode-shell--turns))))
          (should (eq user-end
                      (opencode-shell--turn-user-end
                       (car opencode-shell--turns)))))
        (opencode-shell--render-messages messages)
         (should (equal id (opencode-shell--turn-id (car opencode-shell--turns))))))))

(ert-deftest opencode-shell-stale-snapshot-cannot-regress-completed-response ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((complete (list (opencode-shell-test--message "u1" "user" "q")
                          (opencode-shell-test--message "a1" "assistant" "answer" "u1"))))
      (opencode-shell--render-messages complete 2)
      (opencode-shell--render-messages
       (list (opencode-shell-test--message "u1" "user" "q")) 1)
      (should (equal (opencode-shell--turn-assistant (car opencode-shell--turns)) "answer"))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns)) 'complete)))))

(ert-deftest opencode-shell-partial-snapshot-preserves-known-turn-order ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u1" "user" "first")) 1)
    (opencode-shell--render-messages
     (list (opencode-shell-test--message "u2" "user" "second")) 2)
    (should (equal (mapcar #'opencode-shell--turn-server-user-id
                           opencode-shell--turns)
                   '("u1" "u2")))))

(ert-deftest opencode-shell-sse-bursts-coalesce-runtime-wakes ()
  (let ((first (generate-new-buffer " *sse-first*"))
        (second (generate-new-buffer " *sse-second*"))
        (runtime (list :subscribers (make-hash-table :test #'eq)
                       :sse-input "")))
    (unwind-protect
        (progn
          (puthash first #'ignore (plist-get runtime :subscribers))
          (puthash second #'ignore (plist-get runtime :subscribers))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local opencode-shell--generation 1)))
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (&rest _) 'timer))
                    ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
            (opencode-shell-async--parse-sse
             runtime "data: {\"type\":\"session.updated\"}\n\n")
            (opencode-shell-async--parse-sse
             runtime "data: {\"type\":\"session.updated\"}\n\n")
            (dolist (buffer (list first second))
              (with-current-buffer buffer
                (should (= (length opencode-shell-async--queue) 1))))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest opencode-shell-runtime-shares-one-stream-cadence-and-cleans-up ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (first (generate-new-buffer " *runtime-first*"))
        (second (generate-new-buffer " *runtime-second*"))
        timers cancelled)
    (unwind-protect
        (cl-letf (((symbol-function 'opencode-shell-async--connect) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (let ((timer (cons 'timer args)))
                       (push timer timers)
                       timer)))
                  ((symbol-function 'timerp)
                   (lambda (value) (eq (car-safe value) 'timer)))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled))))
          (opencode-shell-async-subscribe-runtime
           'server first "http://localhost:4199/event" nil t 2 #'ignore)
          (opencode-shell-async-subscribe-runtime
           'server second "http://localhost:4199/event" nil t 2 #'ignore)
          (let ((runtime (opencode-shell-async-runtime-get 'server)))
            (should runtime)
            (should (= (hash-table-count (plist-get runtime :subscribers)) 2))
            (should (= (length timers) 1)))
          (opencode-shell-async-unsubscribe-runtime 'server first)
          (should (opencode-shell-async-runtime-get 'server))
          (opencode-shell-async-unsubscribe-runtime 'server second)
          (should-not (opencode-shell-async-runtime-get 'server))
          (should (= (length cancelled) 1)))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest opencode-shell-sse-reconnect-backoff-increases-after-rejection ()
  (let* ((key 'reconnect)
         (subscribers (make-hash-table :test #'eq))
         (runtime (list :subscribers subscribers :backoff 1
                        :reconnect-timer nil)))
    (puthash (current-buffer) #'ignore subscribers)
    (puthash key runtime opencode-shell-async--runtimes)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _) 'reconnect-timer))
                  ((symbol-function 'timerp)
                   (lambda (value) (eq value 'reconnect-timer))))
          (opencode-shell-async--schedule-reconnect key)
          (should (= (plist-get runtime :backoff) 2))
          (setf (plist-get runtime :reconnect-timer) nil)
          (opencode-shell-async--schedule-reconnect key)
          (should (= (plist-get runtime :backoff) 4)))
      (remhash key opencode-shell-async--runtimes))))

(ert-deftest opencode-shell-sse-validates-content-type-and-chunk-framing ()
  (let* ((key 'stream)
         (process 'stream-process)
         (runtime (list :subscribers (make-hash-table :test #'eq)
                        :process process :input "" :sse-input ""
                        :headers-done nil :connected nil :backoff 2))
         (deletes 0) (wakes 0))
    (puthash (current-buffer) (lambda () (cl-incf wakes))
             (plist-get runtime :subscribers))
    (puthash key runtime opencode-shell-async--runtimes)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) (lambda (_) (cl-incf deletes)))
              ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
              ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
      (opencode-shell-async--stream-filter
       key process
       "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n")
      (let* ((payload "data: {}\n\n")
             (chunk (format "%x;source=test\r\n%s\r\n" (length payload) payload)))
        (opencode-shell-async--stream-filter key process chunk))
      (opencode-shell-async-drain (current-buffer))
      (should (plist-get runtime :connected))
      (should (= wakes 1))
      (should (zerop deletes))
      (setf (plist-get runtime :headers-done) nil
            (plist-get runtime :connected) nil
            (plist-get runtime :input) ""
            (plist-get runtime :backoff) 8)
      (opencode-shell-async--stream-filter
       key process
       "HTTP/1.1 200 OK\r\nX-Reason: content-type: text/event-stream\r\nContent-Type: application/json\r\n\r\n{}")
      (should (= deletes 1))
      (should (= (plist-get runtime :backoff) 8))
      (setf (plist-get runtime :headers-done) t
            (plist-get runtime :chunked) t
            (plist-get runtime :input) "ZZ\r\n")
      (opencode-shell-async--parse-chunks runtime)
      (should (= deletes 2)))
    (remhash key opencode-shell-async--runtimes)))

(ert-deftest opencode-shell-submit-includes-stable-message-id ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "hello")
    (let (body)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path _callback &optional value &rest _)
                   (setq body value))))
        (opencode-shell--submit)
        (should (equal (alist-get 'messageID body)
                       (opencode-shell--turn-id (car opencode-shell--turns))))))))

(ert-deftest opencode-shell-hidden-submit-callback-defers-buffer-render ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (insert "hello")
    (let (success)
      (cl-letf (((symbol-function 'opencode-shell--start-polling) #'ignore)
                ((symbol-function 'opencode-shell--request)
                 (lambda (_method path callback &rest _)
                   (when (string-match-p "prompt_async" path)
                     (setq success callback)))))
        (opencode-shell--submit)
        (let ((before (buffer-string)))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
            (funcall success nil)
            (should (equal before (buffer-string)))
            (should opencode-shell--render-dirty)))))))

(ert-deftest opencode-shell-waiting-face-model-agent-keys-and-mode-line ()
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-c")) #'opencode-shell--submit))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "s-<return>")) #'opencode-shell--submit))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-v")) #'opencode-shell--select-model))
  (should (eq (lookup-key opencode-shell-mode-map (kbd "C-c C-m")) #'opencode-shell--select-agent))
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--directory "/work" opencode-shell--session-id "session"
          opencode-shell--models '(("p/m" . ((providerID . "p") (modelID . "m"))))
          opencode-shell--agents '(("build" . ((name . "build")))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _) (if (string-prefix-p "Model" prompt) "p/m" "build"))))
      (opencode-shell--select-model)
      (opencode-shell--select-agent))
    (should header-line-format)
    (should (string-match-p "build.*p/m" (opencode-shell--header)))
    (should-not (string-match-p "model\|agent\|OpenCode"
                                (opencode-shell--mode-line-status)))
    (setq opencode-shell--turns
          (list (opencode-shell--make-turn :id "local" :user "q" :status 'waiting)))
    (opencode-shell--render-turns)
    (goto-char (point-min)) (search-forward "Waiting")
    (should (eq (get-text-property (match-beginning 0) 'face)
                 'opencode-shell-waiting-face))))

(ert-deftest opencode-shell-sessions-help-overrides-evil-search-key ()
  (should (eq (lookup-key opencode-shell-sessions-mode-map (kbd "?"))
              #'opencode-shell-sessions-help))
  (require 'transient)
  (should (commandp 'opencode-shell-sessions-menu)))

(ert-deftest opencode-shell-transcript-help-and-header-metadata ()
  (should-not (lookup-key opencode-shell-mode-map (kbd "?")))
  (should (commandp 'opencode-shell-menu))
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-title "Generated title"
          opencode-shell--selected-agent "build"
          opencode-shell--selected-model '((providerID . "p") (modelID . "m"))
          opencode-shell--models '(("p/m" . ((providerID . "p") (modelID . "m")))))
    (setq opencode-shell--session-id "ses_123")
    (let* ((header (opencode-shell--header))
           (session-pos (string-match "session:ses_123" header)))
      (should (string-match-p "Generated title.*build.*p/m.*session:ses_123" header))
      (should (eq (lookup-key (get-text-property session-pos 'keymap header)
                              [header-line mouse-1])
                  #'opencode-shell-copy-session-id))
      (should (eq (get-text-property session-pos 'mouse-face header)
                  'mode-line-highlight))
      (should (string-match-p "Copy session ID"
                              (get-text-property session-pos 'help-echo header)))
      (should-not (get-text-property (+ session-pos (length "session:ses_123"))
                                     'keymap header))
      (should-not (get-text-property 1 'keymap header)))
    (should (equal (opencode-shell--mode-line-status) " [idle]"))))

(ert-deftest opencode-shell-pending-header-session-is-not-clickable ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-title nil
          opencode-shell--session-id nil)
    (let* ((header (opencode-shell--header))
           (session-pos (string-match "session:pending" header)))
      (should session-pos)
      (should-not (get-text-property session-pos 'keymap header))
      (should-not (get-text-property session-pos 'mouse-face header)))))

(ert-deftest opencode-shell-copy-session-id-copies-exact-id ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "ses_exact")
    (opencode-shell-copy-session-id)
    (should (equal (current-kill 0) "ses_exact"))))

(ert-deftest opencode-shell-defaults-require-available-build-model ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--agents
          '(("build" . ((name . "build") (model . "p/m"))))
          opencode-shell--models nil)
    (opencode-shell--initialize-server-defaults)
    (should-not opencode-shell--selected-agent)
    (should-not opencode-shell--selected-model)))

(ert-deftest opencode-shell-same-leaf-browsers-do-not-collide ()
  (let ((profile opencode-shell-test--local-profile) first second)
    (cl-letf (((symbol-function 'opencode-shell--refresh) #'ignore)
              ((symbol-function 'pop-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (opencode-shell--sessions "/one/project/" profile)
            (setq first (opencode-shell--sessions-buffer profile "/one/project/"))
            (opencode-shell--sessions "/two/project/" profile)
            (setq second (opencode-shell--sessions-buffer profile "/two/project/"))
            (should (buffer-live-p first))
            (should (buffer-live-p second))
            (should-not (eq first second))
            (should (string-match-p "<2>" (buffer-name second))))
        (when (buffer-live-p first) (kill-buffer first))
        (when (buffer-live-p second) (kill-buffer second))))))

(ert-deftest opencode-shell-session-browser-candidates-show-profile-path-and-state ()
  (let ((active (opencode-shell--session-browser-candidate
                 opencode-shell-test--local-profile "/work/" t))
        (recent (opencode-shell--session-browser-candidate
                 opencode-shell-test--remote-profile "/srv/work/" nil)))
    (should (string-match-p "local : /work/" (car active)))
    (should (string-match-p "remote : /srv/work/" (car recent)))
    (should (eq (get-text-property 0 'face (car active))
                'opencode-shell-active-session-face))
    (should (eq (get-text-property 0 'face (car recent))
                'opencode-shell-recent-session-face))))

(ert-deftest opencode-shell-switch-buffer-shows-title-and-buffer-name ()
  (let ((shell (generate-new-buffer "*Opencode project shell*")) prompt selected)
    (unwind-protect
        (progn
          (with-current-buffer shell
            (opencode-shell-mode)
            (setq opencode-shell--session-title "Generated title"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_ candidates &rest _)
                       (setq prompt (caar candidates))
                       prompt))
                    ((symbol-function 'switch-to-buffer)
                      (lambda (buffer &rest _) (setq selected buffer))))
            (opencode-shell-switch-buffer))
          (should (string-match-p "Generated title.*Opencode project shell" prompt))
          (should (eq selected shell)))
      (kill-buffer shell))))

(ert-deftest opencode-shell-sessions-installs-buffer-local-evil-bindings ()
  (let (bindings)
    (cl-letf (((symbol-function 'evil-local-set-key)
               (lambda (state key command)
                 (push (list state key command) bindings))))
      (with-temp-buffer
        (opencode-shell-sessions-mode))
      (dolist (expected `((normal ,(kbd "RET") opencode-shell--open-at-point)
                          (normal ,(kbd "g r") opencode-shell--refresh)
                          (normal ,(kbd "c") opencode-shell--create-session)
                          (normal ,(kbd "/") opencode-shell--filter)
                          (normal ,(kbd "d") opencode-shell--delete-session)
                          (normal ,(kbd "?") opencode-shell-sessions-help)))
        (should (member expected bindings))))))

(ert-deftest opencode-shell-reload-refreshes-existing-buffer-local-map ()
  (let (loaded)
    (with-temp-buffer
      (opencode-shell-sessions-mode)
      (use-local-map (copy-keymap opencode-shell-sessions-mode-map))
      (define-key (current-local-map) (kbd "?") #'ignore)
      (cl-letf (((symbol-function 'load)
                 (lambda (file &rest _) (push file loaded)))
              ((symbol-function 'opencode-shell--register-profile-commands) #'ignore)
              ((symbol-function 'locate-library)
               (lambda (library)
                 (expand-file-name
                  (if (equal library "opencode-shell-setting")
                      "opencode-shell-setting.el"
                    "opencode-shell.el")
                  default-directory))))
        (opencode-shell-reload)
        (should (seq-some (lambda (file)
                             (string-suffix-p "opencode-shell-setting.el" file))
                           loaded))
        (should (seq-some (lambda (file)
                            (string-suffix-p "opencode-shell-async.el" file))
                          loaded))
        (should (eq (lookup-key (current-local-map) (kbd "?"))
                    #'opencode-shell-sessions-help))))))

(ert-deftest opencode-shell-reload-resubscribes-active-transcript-runtime ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--runtime-key 'active)
    (let ((main (expand-file-name "opencode-shell.el" default-directory))
          (starts 0))
      (cl-letf (((symbol-function 'load) #'ignore)
                ((symbol-function 'locate-library)
                 (lambda (library) (and (equal library "opencode-shell") main)))
                ((symbol-function 'opencode-shell-async-reset) #'ignore)
                ((symbol-function 'opencode-shell--register-profile-commands) #'ignore)
                ((symbol-function 'opencode-shell--start-polling)
                 (lambda () (cl-incf starts))))
        (opencode-shell-reload)
        (should (= starts 1))
        (should-not opencode-shell--runtime-key)))))

(ert-deftest opencode-shell-directory-derived-buffer-names-and-reuse ()
  (should (equal (opencode-shell--directory-leaf "/work/project/") "project"))
  (let* ((profile opencode-shell-test--local-profile)
         (directory "/work/project/")
         (browser (generate-new-buffer " *oc-browser*"))
         (transcript (generate-new-buffer " *oc-transcript*")))
    (unwind-protect
        (progn
          (with-current-buffer browser
            (opencode-shell-sessions-mode)
            (setq-local opencode-shell--profile profile
                        opencode-shell--directory directory))
          (with-current-buffer transcript
            (opencode-shell-mode)
            (setq-local opencode-shell--profile profile
                        opencode-shell--directory directory
                        opencode-shell--session-id "s1"))
          (should (eq browser (opencode-shell--sessions-buffer profile directory)))
          (should (eq transcript
                      (opencode-shell--transcript-buffer profile directory "s1")))
          (should-not (opencode-shell--transcript-buffer profile directory "s2")))
      (kill-buffer browser)
      (kill-buffer transcript))))

(ert-deftest opencode-shell-conversation-state-is-independent-between-buffers ()
  (let ((a (generate-new-buffer " *oc-turn-a*"))
        (b (generate-new-buffer " *oc-turn-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a
            (opencode-shell-mode)
            (opencode-shell--render-messages
             (list (opencode-shell-test--message "same" "user" "one"))))
          (with-current-buffer b
            (opencode-shell-mode)
            (insert "two")
            (should-not opencode-shell--turns))
          (with-current-buffer a
            (should (= (length opencode-shell--turns) 1))
            (should (string-empty-p (opencode-shell--composer-text))))
          (with-current-buffer b (should (equal (opencode-shell--composer-text) "two"))))
      (kill-buffer a) (kill-buffer b))))

;; Profile and server lifecycle contract.  These tests intentionally use
;; complete profile plists: a profile is an immutable piece of configuration,
;; while resolved credentials and process ownership are transient state.

(defconst opencode-shell-test--local-profile
  '(:name "local" :base-url "http://127.0.0.1:7777/"
    :directory "/client/project" :workspace "/server/project"
    :start-command ("opencode" "serve") :server-directory "/tmp"
    :startup-timeout 2 :auth-source (:host "localhost" :port 7777)))

(defconst opencode-shell-test--remote-profile
  '(:name "remote" :base-url "https://code.example.test"
    :directory "/ssh:code.example.test:/srv/project"
    :workspace "/srv/project"
    :start-command ("opencode" "serve")))

(ert-deftest opencode-shell-project-directory-precedence-and-fallbacks ()
  (let ((projectile-mode t)
        (default-directory "/work/nested/"))
    (cl-letf (((symbol-function 'projectile-project-p) (lambda () t))
              ((symbol-function 'projectile-project-root) (lambda () "/projectile/root"))
              ((symbol-function 'project-current) (lambda (&rest _) 'project))
              ((symbol-function 'project-root) (lambda (_) "/project/root")))
      (should (equal (opencode-shell--project-directory) "/projectile/root/"))))
  (let ((projectile-mode nil)
        (default-directory "/work/nested/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'project))
              ((symbol-function 'project-root) (lambda (_) "/project/root")))
      (should (equal (opencode-shell--project-directory) "/project/root/"))))
  (let ((projectile-mode nil)
        (default-directory "/ssh:host.example:/srv/project/nested/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'remote-project))
              ((symbol-function 'project-root)
               (lambda (_) "/ssh:host.example:/srv/project/")))
      (should (equal (opencode-shell--project-directory)
                     "/ssh:host.example:/srv/project/"))))
  (dolist (marker '(directory file))
    (let* ((root (make-temp-file "opencode-shell-git-root-" t))
           (nested (expand-file-name "one/two/" root))
           (git (expand-file-name ".git" root)))
      (unwind-protect
          (progn
            (make-directory nested t)
            (if (eq marker 'directory)
                (make-directory git)
              (write-region "gitdir: elsewhere\n" nil git nil 'silent))
            (let ((projectile-mode nil)
                  (default-directory nested))
              (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
                (should (equal (opencode-shell--project-directory)
                               (file-name-as-directory root))))))
        (delete-directory root t))))
  (let* ((directory (make-temp-file "opencode-shell-no-project-" t))
         (projectile-mode nil)
         (default-directory directory))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
          (should (equal (opencode-shell--project-directory)
                         (file-name-as-directory directory))))
      (delete-directory directory t))))

(ert-deftest opencode-shell-current-server-directory-maps-project-root ()
  (let ((default-directory "/client/project/nested/"))
    (cl-letf (((symbol-function 'opencode-shell--project-directory)
               (lambda (&optional _) "/client/project/")))
      (should (equal (opencode-shell--current-server-directory
                      opencode-shell-test--local-profile)
                     "/server/project/")))))

(ert-deftest opencode-shell-profile-helpers-are-defined-before-public-commands ()
  (dolist (symbol '(opencode-shell--profile-key opencode-shell--profile-name
                    opencode-shell--default-profile opencode-shell--read-profile
                    opencode-shell--resolve-or-read-profile
                    opencode-shell--profile-remote-p))
    (should (fboundp symbol)))
  (should (boundp 'opencode-shell-profiles))
  (should (boundp 'opencode-shell--servers)))

(ert-deftest opencode-shell-default-local-endpoint-is-4199 ()
  (should (equal opencode-shell-base-url "http://127.0.0.1:4199")))

(ert-deftest opencode-shell-profile-key-name-default-and-read ()
  (let ((opencode-shell-profiles
         (list opencode-shell-test--local-profile
               opencode-shell-test--remote-profile))
        (default-directory "/client/project/src/") choice)
    (should (equal (opencode-shell--profile-name opencode-shell-test--local-profile)
                   "local"))
    (should (equal (opencode-shell--profile-key opencode-shell-test--local-profile)
                   (opencode-shell--profile-key (copy-tree opencode-shell-test--local-profile))))
    (should (equal (opencode-shell--default-profile)
                    (list :name "default" :base-url opencode-shell-base-url
                          :directory opencode-shell-directory)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (setq choice candidates) "remote")))
      (should (equal (opencode-shell--read-profile)
                     opencode-shell-test--remote-profile))
      (let ((names (mapcar (lambda (item) (if (consp item) (car item) item)) choice)))
        (should (member "local" names))
        (should (member "remote" names))))))

(ert-deftest opencode-shell-profile-local-and-tramp-directory-mapping ()
  (should-not (opencode-shell--profile-remote-p opencode-shell-test--local-profile))
  (should (opencode-shell--profile-remote-p opencode-shell-test--remote-profile))
  (should (equal (opencode-shell--server-directory
                  "/client/project/src/a.el" opencode-shell-test--local-profile)
                 "/server/project/src/a.el"))
  (should (equal (opencode-shell--server-directory
                   "/ssh:code.example.test:/srv/project/src/a.el"
                   opencode-shell-test--remote-profile)
                  "/srv/project/src/a.el"))
  (should (equal (opencode-shell--server-directory
                  "/client/project/src/./lib/../a.el"
                  opencode-shell-test--local-profile)
                 "/server/project/src/a.el"))
  (should (equal (opencode-shell--server-directory
                  "/client/project/../outside/a.el"
                  opencode-shell-test--local-profile)
                 "/client/outside/a.el"))
  (should (equal (opencode-shell--server-directory
                  "/client/project-sibling/a.el"
                  opencode-shell-test--local-profile)
                 "/client/project-sibling/a.el"))
  (should (equal (opencode-shell--server-directory
                  "/ssh:code.example.test:/srv/project/src/../a.el"
                  opencode-shell-test--remote-profile)
                 "/srv/project/a.el"))
  (should (equal (opencode-shell--server-directory
                  "/ssh:code.example.test:/srv/project/../outside/a.el"
                  opencode-shell-test--remote-profile)
                 "/srv/outside/a.el"))
  (let ((broad '(:name "workspace" :directory "/Workspace"
                 :workspace "/server/Workspace")))
    (should (equal (opencode-shell--server-directory
                    "/Workspace/personal/translator/" broad)
                   "/server/Workspace/personal/translator/"))
    (should (equal (opencode-shell--server-directory "/Workspace/" broad)
                   "/server/Workspace/")))
  (let ((nested '(:name "nested" :directory "/work"
                  :workspace "/work/server")))
    (should (equal (opencode-shell--server-directory
                    "/work/server/project/" nested)
                   "/work/server/project/"))))

(ert-deftest opencode-shell-session-buffers-use-client-default-directory ()
  (let ((profile opencode-shell-test--local-profile) browser transcript)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _)
                 (if (with-current-buffer buffer
                       (derived-mode-p 'opencode-shell-sessions-mode))
                     (setq browser buffer)
                   (setq transcript buffer))))
              ((symbol-function 'opencode-shell--request) #'ignore)
              ((symbol-function 'opencode-shell--resync) #'ignore)
              ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (opencode-shell--sessions "/server/project/src/" profile)
            (opencode-shell-open-session "session" "/server/project/src/" profile)
            (should (equal (buffer-local-value 'default-directory browser)
                           "/client/project/src/"))
            (should (equal (buffer-local-value 'default-directory transcript)
                           "/client/project/src/")))
        (when (buffer-live-p browser) (kill-buffer browser))
        (when (buffer-live-p transcript) (kill-buffer transcript))))))

(ert-deftest opencode-shell-profile-scopes-url-directory-workspace-and-auth ()
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

(ert-deftest opencode-shell-entry-opens-explicit-server-with-project-root ()
  (let ((default-directory "/client/project/current/") opened)
    (cl-letf (((symbol-function 'opencode-shell--start-server)
               (lambda (profile callback) (funcall callback profile)))
              ((symbol-function 'opencode-shell--project-directory)
               (lambda (&optional _) "/client/project/"))
              ((symbol-function 'opencode-shell--sessions)
               (lambda (&optional directory profile _current-window)
                   (setq opened (list directory profile)))))
      (opencode-shell opencode-shell-test--local-profile)
       (should (equal opened (list "/server/project/" opencode-shell-test--local-profile))))
    (let ((opencode-shell-poll-interval 60) buffers)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (push buffer buffers)))
               ((symbol-function 'opencode-shell--resync) #'ignore)
                ((symbol-function 'run-at-time) (lambda (&rest _) nil)))
        (unwind-protect
            (progn
              (opencode-shell-open-session "same-id" nil opencode-shell-test--local-profile)
              (opencode-shell-open-session "same-id" nil opencode-shell-test--remote-profile)
              (should (= (length (delete-dups buffers)) 2)))
          (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) buffers))))))

(ert-deftest opencode-shell-find-session-selects-live-and-recent-profile-paths ()
  (let ((opencode-shell-profiles
         (list opencode-shell-test--local-profile opencode-shell-test--remote-profile))
        (opencode-shell--recent-session-locations
         (list (cons (opencode-shell--profile-key opencode-shell-test--local-profile)
                     "/cached")))
        (browser (generate-new-buffer " *active-browser*")) requests prompt opened)
    (unwind-protect
        (progn
          (with-current-buffer browser
            (opencode-shell-sessions-mode)
            (setq-local opencode-shell--profile opencode-shell-test--local-profile
                        opencode-shell--directory "/work/"))
          (cl-letf (((symbol-function 'opencode-shell--request)
                     (lambda (method path callback &optional _body params _error)
                       (push (list opencode-shell--profile method path params callback) requests)))
                    ((symbol-function 'completing-read)
                     (lambda (text candidates &rest _)
                       (setq prompt (list text (mapcar #'car candidates)))
                       (car (seq-find
                             (lambda (candidate)
                               (string-match-p "local : /work/" (car candidate)))
                             candidates))))
                    ((symbol-function 'opencode-shell--open-sessions)
                     (lambda (profile directory &optional current-window)
                       (setq opened (list profile directory current-window)))))
            (opencode-shell-find-session)
            (should (= (length requests) 2))
            (dolist (request requests)
              (should (equal (cl-subseq request 1 4)
                             '("GET" "/session" ((limit . 1000)))))
              (funcall (nth 4 request)
                       (if (equal (opencode-shell--profile-name (car request)) "local")
                           '(((id . "live") (directory . "/work")
                              (time . ((updated . 1))))
                             ((id . "local-recent") (directory . "/z/")
                              (time . ((updated . 3)))))
                         '(((id . "recent") (directory . "/srv/recent/")
                            (time . ((updated . 2))))))))
            (should (equal (car prompt) "OpenCode profile : path: "))
            (should (= (length (seq-filter
                                (lambda (label) (string-match-p "local : /work/" label))
                                (cadr prompt)))
                       1))
            (should (seq-some (lambda (label) (string-match-p "local : /work/" label))
                              (cadr prompt)))
            (let ((labels (cadr prompt)))
              (should (< (cl-position-if (lambda (label) (string-match-p "local : /work/" label)) labels)
                         (cl-position-if (lambda (label) (string-match-p "inactive" label)) labels)
                         (cl-position-if (lambda (label) (string-match-p "local : /cached/" label)) labels)
                         (cl-position-if (lambda (label) (string-match-p "local : /z/" label)) labels)
                         (cl-position-if (lambda (label) (string-match-p "remote : /srv/recent/" label)) labels))))
            (should (equal opened (list opencode-shell-test--local-profile "/work/" t)))))
      (when (buffer-live-p browser) (kill-buffer browser)))))

(ert-deftest opencode-shell-remembers-session-browser-locations-without-slash-duplicates ()
  (let ((opencode-shell--recent-session-locations nil))
    (opencode-shell--remember-session-location opencode-shell-test--local-profile "/work")
    (opencode-shell--remember-session-location opencode-shell-test--local-profile "/work/")
    (should (equal opencode-shell--recent-session-locations
                   (list (cons (opencode-shell--profile-key opencode-shell-test--local-profile)
                               "/work/"))))))

(ert-deftest opencode-shell-persists-session-browser-locations ()
  (let ((opencode-shell-recent-locations-file (make-temp-file "opencode-shell-locations-"))
        (opencode-shell--recent-session-locations '(("local" . "/work/"))))
    (unwind-protect
        (progn
          (opencode-shell--save-recent-session-locations)
          (setq opencode-shell--recent-session-locations nil)
          (should (equal (opencode-shell--load-recent-session-locations)
                         '(("local" . "/work/")))))
      (when (file-exists-p opencode-shell-recent-locations-file)
        (delete-file opencode-shell-recent-locations-file)))))

(ert-deftest opencode-shell-persists-session-directory-overrides ()
  (let ((opencode-shell-session-directory-overrides-file
         (make-temp-file "opencode-shell-session-directories-"))
        (opencode-shell--session-directory-overrides
         '((("local" "session") . "/work/"))))
    (unwind-protect
        (progn
          (opencode-shell--save-session-directory-overrides)
          (setq opencode-shell--session-directory-overrides nil)
          (should (equal (opencode-shell--load-session-directory-overrides)
                         '((("local" "session") . "/work/")))))
      (when (file-exists-p opencode-shell-session-directory-overrides-file)
        (delete-file opencode-shell-session-directory-overrides-file)))))

(ert-deftest opencode-shell-find-session-quit-is-silent ()
  (let ((opencode-shell-profiles (list opencode-shell-test--local-profile)) callback)
    (cl-letf (((symbol-function 'opencode-shell--request)
               (lambda (_method _path success &rest _) (setq callback success)))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'opencode-shell--open-sessions)
               (lambda (&rest _) (ert-fail "quit must not open a browser"))))
      (opencode-shell-find-session)
      (should-not
       (funcall callback '(((id . "recent") (directory . "/work"))))))))

(ert-deftest opencode-shell-register-profile-commands-refreshes-and-isolates-profiles ()
  (let ((opencode-shell-profiles
         (list opencode-shell-test--local-profile opencode-shell-test--remote-profile))
        (opencode-shell--generated-profile-commands nil)
        opened)
    (let ((default-directory "/client/project/"))
      (unwind-protect
        (progn
          (opencode-shell--register-profile-commands)
          (should (commandp 'opencode-shell-local-sessions))
          (should (commandp 'opencode-shell-local-start))
          (should (commandp 'opencode-shell-remote-sessions))
          (should (commandp 'opencode-shell-remote-start))
          (cl-letf (((symbol-function 'opencode-shell--project-directory)
                     (lambda (&optional directory) (or directory default-directory)))
                    ((symbol-function 'opencode-shell--open-sessions)
                     (lambda (profile directory)
                       (push (list profile directory) opened))))
            (call-interactively 'opencode-shell-local-sessions)
            (let ((default-directory "/ssh:code.example.test:/srv/project/"))
              (call-interactively 'opencode-shell-remote-sessions)))
          (should (equal (mapcar (lambda (entry)
                                   (cons (opencode-shell--profile-name (car entry))
                                         (cdr entry)))
                                 opened)
                          '(("remote" "/srv/project/")
                            ("local" "/server/project/"))))
          (setq opencode-shell-profiles (list opencode-shell-test--local-profile))
          (opencode-shell--register-profile-commands)
          (should-not (fboundp 'opencode-shell-remote-sessions))
          (should-not (fboundp 'opencode-shell-remote-start)))
        (mapc (lambda (symbol) (when (fboundp symbol) (fmakunbound symbol)))
              opencode-shell--generated-profile-commands)))))

(ert-deftest opencode-shell-register-profile-commands-rejects-invalid-and-colliding-names ()
  (let ((opencode-shell--generated-profile-commands nil))
    (let ((opencode-shell-profiles '((:name "bad/name" :base-url "http://localhost:1"))))
      (should-error (opencode-shell--register-profile-commands) :type 'user-error))
    (let ((opencode-shell-profiles
           '((:name "foo bar" :base-url "http://localhost:1")
             (:name "foo_bar" :base-url "http://localhost:2"))))
      (should-error (opencode-shell--register-profile-commands) :type 'user-error))))

(ert-deftest opencode-shell-register-profile-commands-does-not-overwrite-existing-functions ()
  (let ((opencode-shell-profiles
         '((:name "reserved" :base-url "http://localhost:1")))
        (opencode-shell--generated-profile-commands nil))
    (cl-letf (((symbol-function 'opencode-shell-reserved-sessions) #'ignore))
      (should-error (opencode-shell--register-profile-commands) :type 'user-error)
      (should (eq (symbol-function 'opencode-shell-reserved-sessions) #'ignore)))))

(ert-deftest opencode-shell-directory-scoped-launch-isolates-buffers ()
  (let* ((profile '(:name "workspace" :base-url "http://127.0.0.1:4096"
                      :directory "/Workspace" :workspace "/server/Workspace"))
         (default-directory "/Workspace/personal/translator/")
         requests buffers)
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (push buffer buffers)))
              ((symbol-function 'opencode-shell--request)
               (lambda (method path _callback &optional _data query)
                  (push (list method path opencode-shell--directory query) requests)))
              ((symbol-function 'opencode-shell--project-directory)
               (lambda (&optional _) "/Workspace/")))
      (unwind-protect
          (progn
             (opencode-shell profile)
              (should (equal (cl-subseq (car requests) 0 3)
                             '("GET" "/session/status" "/server/Workspace/")))
            (opencode-shell--sessions "/Workspace/" profile)
             (should (equal (cl-subseq (car requests) 0 3)
                            '("GET" "/session/status" "/Workspace/")))
             (should (= (length (delete-dups buffers)) 2)))
        (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer))) buffers)))))

(ert-deftest opencode-shell--sessions-requires-and-stores-absolute-directory ()
  (let (seen)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'opencode-shell--refresh)
               (lambda () (push (list opencode-shell--directory opencode-shell--profile) seen))))
      (unwind-protect
          (progn
             (opencode-shell--sessions "/legacy/directory" opencode-shell-test--remote-profile)
             (should (equal (car seen)
                            (list "/legacy/directory/" opencode-shell-test--remote-profile)))
             (should-error (opencode-shell--sessions "relative" opencode-shell-test--local-profile)
                           :type 'user-error))
        (mapc (lambda (buffer)
                (when (string-prefix-p "*OpenCode Shell Sessions:" (buffer-name buffer))
                  (kill-buffer buffer)))
              (buffer-list))))))

(ert-deftest opencode-shell-current-directory-rejects-unmappable-profile-path ()
  (let ((default-directory "/unrelated/local/")
        (profile '(:name "remote" :directory "/client/root"
                    :workspace "/server/root")))
    (should-error (opencode-shell--current-server-directory profile)
                  :type 'user-error)))

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
              ((symbol-function 'opencode-shell--start-server)
               (lambda (value &rest _) (setq started value))))
      (opencode-shell--restart-server profile))
    (should (equal started profile))))

(ert-deftest opencode-shell-remote-profile-never-autostarts ()
  (let ((opencode-shell--servers (make-hash-table :test #'equal)) spawned)
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (setq spawned t))))
      (opencode-shell--start-server opencode-shell-test--remote-profile #'ignore))
    (should-not spawned)
    (should (= (hash-table-count opencode-shell--servers) 0))))

(ert-deftest opencode-shell-killing-transcript-does-not-stop-owned-server ()
  (let* ((opencode-shell--servers (make-hash-table :test #'equal))
         (profile opencode-shell-test--local-profile)
          (key (opencode-shell--server-key profile)) stopped)
    (puthash key (list :process 'owned :owned t) opencode-shell--servers)
    (with-temp-buffer
      (opencode-shell-mode)
      (setq-local opencode-shell--profile profile)
      (cl-letf (((symbol-function 'delete-process) (lambda (&rest _) (setq stopped t))))
        (opencode-shell--cleanup)))
    (should-not stopped)
    (should (gethash key opencode-shell--servers))))

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

(ert-deftest opencode-shell-shared-auth-selectors-use-effective-values ()
  (let* ((one '(:name "one" :base-url "http://localhost:80/"
                :start-command ("serve") :auth-host "auth.test"
                :auth-port 443 :auth-user "user"))
         (two '(:name "two" :base-url "http://127.0.0.1"
                :start-command ("serve")
                :auth-source (:host "auth.test" :port 443 :user "user")
                :auth-header "Authorization"))
         (opencode-shell-profiles (list one two)))
    (should (equal (opencode-shell--server-key one)
                   (opencode-shell--server-key two)))
    (should (equal (opencode-shell--server-lifecycle-config one)
                   (opencode-shell--server-lifecycle-config two)))
    (opencode-shell--validate-profiles))
  (let* ((one '(:base-url "http://localhost" :start-command ("serve")
                :auth-host "one.test"))
         (two '(:base-url "http://localhost:80" :start-command ("serve")
                :auth-source (:host "two.test"))))
    (should-error
     (opencode-shell--validate-server-profile
      two (list :config (opencode-shell--server-lifecycle-config one)))
     :type 'user-error)))

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
              ((symbol-function 'opencode-shell--start-server)
               (lambda (profile &rest _) (setq started profile))))
      (opencode-shell--restart-server two))
    (should (equal started two))))

(ert-deftest opencode-shell-browser-state-is-buffer-and-generation-isolated ()
  (let ((one (generate-new-buffer " *oc-one*"))
        (two (generate-new-buffer " *oc-two*")) requests)
    (unwind-protect
        (cl-letf (((symbol-function 'opencode-shell--request)
                   (lambda (_method path callback &rest _)
                     (push (list (current-buffer) path callback) requests))))
          (dolist (buffer (list one two))
            (with-current-buffer buffer
              (opencode-shell-sessions-mode)
              (setq-local opencode-shell--profile opencode-shell-test--local-profile
                          opencode-shell--directory "/work/")
              (opencode-shell--refresh)))
          (dolist (request (copy-sequence requests))
            (when (equal (cadr request) "/session/status")
              (with-current-buffer (car request)
                (funcall (nth 2 request)
                         `((,(if (eq (car request) one) "one" "two")
                            . ((type . "busy"))))))))
          (dolist (request requests)
            (when (equal (cadr request) "/session")
              (with-current-buffer (car request)
                (funcall (nth 2 request)
                         `(((id . ,(if (eq (car request) one) "one" "two"))
                             (directory . "/work/")))))))
          (dolist (buffer (list one two))
            (opencode-shell-async-drain buffer))
          (should (equal (with-current-buffer one (opencode-shell--get
                                                   opencode-shell--session-status "one"))
                         '((type . "busy"))))
          (should (equal (with-current-buffer one
                           (opencode-shell--get (car opencode-shell--sessions) 'id)) "one"))
          (should (equal (with-current-buffer two
                           (opencode-shell--get (car opencode-shell--sessions) 'id)) "two")))
      (kill-buffer one) (kill-buffer two))))

(ert-deftest opencode-shell-hidden-browser-defers-tabulated-render ()
  (with-temp-buffer
    (opencode-shell-sessions-mode)
    (setq-local opencode-shell--profile opencode-shell-test--local-profile
                opencode-shell--directory "/work/"
                opencode-shell--generation 4)
    (let (visible (prints 0))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) visible))
                ((symbol-function 'tabulated-list-print)
                 (lambda (&rest _) (cl-incf prints))))
        (opencode-shell--apply-session-browser-snapshot
         '(((id . "s") (directory . "/work/"))) nil 4)
        (should opencode-shell--browser-dirty)
        (should (zerop prints))
        (setq visible t)
        (opencode-shell--render-session-browser-if-visible)
        (should (= prints 1))
        (should-not opencode-shell--browser-dirty)))))

(ert-deftest opencode-shell-profile-validation-rejects-duplicate-identities ()
  (let ((opencode-shell-profiles
         '((:name "duplicate" :id "one") (:name "duplicate" :id "two"))))
    (should-error (opencode-shell--validate-profiles) :type 'user-error))
  (let ((opencode-shell-profiles
         '((:name "one" :id "same") (:name "two" :id "same"))))
    (should-error (opencode-shell--validate-profiles) :type 'user-error)))

(ert-deftest opencode-shell-browser-has-fixed-directory-and-no-project-column ()
  (with-temp-buffer
    (opencode-shell-sessions-mode)
    (setq-local opencode-shell--directory "/home/server"
                opencode-shell--sessions
                '(((id . "one") (title . "Same") (directory . "/work/one"))
                  ((id . "two") (title . "Same") (directory . "/work/two"))))
    (setq tabulated-list-entries (opencode-shell--session-entries))
    (should (equal (mapcar #'car tabulated-list-entries) '("one" "two")))
    (should (equal opencode-shell--directory "/home/server"))
    (should-not (assoc "Project" (append tabulated-list-format nil)))))

(ert-deftest opencode-shell-opened-session-keeps-row-directory-for-follow-up-requests ()
  (let ((profile opencode-shell-test--remote-profile)
        (opencode-shell-poll-interval 60) browser transcript requests)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _) (setq transcript buffer)))
              ((symbol-function 'run-at-time) (lambda (&rest _) nil))
              ((symbol-function 'opencode-shell--request)
               (lambda (_method path callback &rest _)
                 (push (list path opencode-shell--directory) requests)
                 (when (member path '("/session/s/message" "/permission" "/question"))
                   (funcall callback nil)))))
      (unwind-protect
          (progn
            (setq browser (generate-new-buffer " *oc-browser-directory*"))
            (with-current-buffer browser
              (opencode-shell-sessions-mode)
              (setq-local opencode-shell--profile profile
                          opencode-shell--directory "/home/remote"
                          opencode-shell--sessions
                          '(((id . "s") (directory . "/srv/other"))))
              (let ((inhibit-read-only t))
                (insert (propertize "s" 'tabulated-list-id "s")))
              (goto-char (point-min))
              (opencode-shell--open-at-point))
            (with-current-buffer transcript
              (opencode-shell--abort)
              (should-error (opencode-shell--permissions) :type 'user-error)
              (should-error (opencode-shell--questions) :type 'user-error))
            (should requests)
            (dolist (request requests)
              (should (equal (cadr request) "/srv/other"))))
        (when (buffer-live-p browser) (kill-buffer browser))
        (when (buffer-live-p transcript) (kill-buffer transcript))))))

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

(provide 'opencode-shell-test)
;;; opencode-shell-test.el ends here
