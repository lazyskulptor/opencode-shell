;;; opencode-shell-event-test.el --- Application event tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'opencode-shell-event)
(require 'opencode-shell-async)
(require 'opencode-shell)
(require 'completion-polling-regression)

(ert-deftest opencode-shell-event-decodes-v1-message-and-part-events ()
  (let ((message (opencode-shell-event-decode
                  (list :data opencode-shell-test--message-updated-event)))
        (part (opencode-shell-event-decode
               (list :data opencode-shell-test--part-updated-event))))
    (should (eq (plist-get message :kind) 'message-updated))
    (should (equal (plist-get message :session-id) "session-a"))
    (should (equal (plist-get message :message-id) "assistant-active"))
    (should (eq (plist-get part :kind) 'part-updated))
    (should (equal (plist-get part :session-id) "session-a"))
    (should (equal (plist-get part :message-id) "assistant-active"))
    (should (equal (plist-get part :part-id) "part-active"))))

(ert-deftest opencode-shell-event-reduces-unknown-and-malformed-data-to-snapshot-hints ()
  (let ((unknown
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.updated\",\"properties\":{\"info\":{\"sessionID\":\"session-a\"}}}")))
        (malformed (opencode-shell-event-decode '(:data "not json"))))
    (should (eq (plist-get unknown :kind) 'snapshot))
    (should (equal (plist-get unknown :session-id) "session-a"))
    (should (eq (plist-get unknown :reason) 'unsupported))
    (should (eq (plist-get unknown :resource) 'messages))
    (should (eq (plist-get malformed :kind) 'snapshot))
    (should-not (plist-get malformed :session-id))
    (should (eq (plist-get malformed :resource) 'all))
    (should (eq (plist-get malformed :reason) 'malformed))))

(ert-deftest opencode-shell-event-coalesces-update-and-removal-by-entity ()
  (should
   (equal (opencode-shell-event-identity
           '(:kind message-updated :session-id "s" :message-id "m"))
          (opencode-shell-event-identity
           '(:kind message-removed :session-id "s" :message-id "m"))))
  (should
   (equal (opencode-shell-event-identity
           '(:kind part-updated :session-id "s" :message-id "m" :part-id "p"))
          (opencode-shell-event-identity
           '(:kind part-removed :session-id "s" :message-id "m" :part-id "p"))))
  (should-not
   (equal (opencode-shell-event-identity
           '(:kind message-updated :session-id "s" :message-id "m1"))
          (opencode-shell-event-identity
           '(:kind message-updated :session-id "s" :message-id "m2"))))
  (should-not
   (equal (opencode-shell-event-identity
           '(:kind snapshot :session-id "s" :resource messages))
          (opencode-shell-event-identity
           '(:kind snapshot :session-id "s" :resource permissions)))))

(ert-deftest opencode-shell-event-scopes-unsupported-hints-by-resource ()
  (let ((permission
         (opencode-shell-event-decode
          '(:data "{\"type\":\"permission.updated\",\"properties\":{\"sessionID\":\"s\"}}")))
        (question
         (opencode-shell-event-decode
          '(:data "{\"type\":\"question.asked\",\"properties\":{\"sessionID\":\"s\"}}"))))
    (should (eq (plist-get permission :resource) 'permissions))
    (should (eq (plist-get question :resource) 'questions))))

(ert-deftest opencode-shell-event-queues-resource-specific-reconciliation ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--receive-application-event
     '(:kind snapshot :type "permission.updated" :reason unsupported
       :session-id "s" :resource permissions))
    (should (alist-get '(event-reconcile permissions)
                       opencode-shell-async--queue nil nil #'equal))
    (should-not (alist-get '(event-reconcile messages)
                           opencode-shell-async--queue nil nil #'equal))))

(ert-deftest opencode-shell-event-resource-reconciliation-avoids-unrelated-requests ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let (requests)
      (cl-letf (((symbol-function 'opencode-shell--guarded-request)
                 (lambda (key &rest _) (push key requests))))
        (opencode-shell--resync nil 'permissions))
      (should (equal requests '(permissions))))))

(ert-deftest opencode-shell-event-routes-session-scoped-events-only-to-their-subscriber ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (first (generate-new-buffer " *event-session-a*"))
        (second (generate-new-buffer " *event-session-b*"))
        (runtime (list :subscribers (make-hash-table :test #'eq)
                       :attempt 'current :backoff 8 :failures 2))
        first-events second-events)
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local opencode-shell--generation 1)))
          (puthash first
                   (list :callback #'ignore :session-id "session-a"
                         :event-callback (lambda (event) (push event first-events)))
                   (plist-get runtime :subscribers))
          (puthash second
                   (list :callback #'ignore :session-id "session-b"
                         :event-callback (lambda (event) (push event second-events)))
                   (plist-get runtime :subscribers))
          (puthash 'server runtime opencode-shell-async--runtimes)
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (&rest _) 'timer))
                    ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                    ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
            (opencode-shell-async--transport-event
             'server 'current
             (list :data opencode-shell-test--message-updated-event))
            (opencode-shell-async-drain first)
            (opencode-shell-async-drain second))
          (should (= (length first-events) 1))
          (should-not second-events))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (remhash 'server opencode-shell-async--runtimes))))

(ert-deftest opencode-shell-event-applies-message-and-part-deltas-without-snapshot ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (opencode-shell--render-messages
     '(((info . ((id . "user-active") (sessionID . "session-a")
                 (role . "user")))
        (parts . (((id . "user-text") (type . "text") (text . "question")))))
       ((info . ((id . "assistant-active") (sessionID . "session-a")
                 (role . "assistant") (parentID . "user-active")))
        (parts . nil))) 1)
    (let ((snapshot-count 0)
          (part-event
           (opencode-shell-event-decode
            (list :data opencode-shell-test--part-updated-event))))
      (cl-letf (((symbol-function 'opencode-shell--resync)
                 (lambda (&rest _) (cl-incf snapshot-count)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) t)))
        (opencode-shell--receive-application-event part-event)
        (opencode-shell-async-drain (current-buffer))
        (should (zerop snapshot-count))
        (should (opencode-shell--turn-parts (car opencode-shell--turns)))
        (should (string-match-p "TOOL> bash" (buffer-string)))
        (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                    'receiving))
        (opencode-shell--receive-application-event part-event)
        (should-not opencode-shell-async--queue)
        (opencode-shell--receive-application-event
         '(:kind part-updated :type "message.part.updated"
           :session-id "session-a" :message-id "missing" :part-id "p"
           :part ((id . "p") (messageID . "missing") (type . "text"))))
        (opencode-shell-async-drain (current-buffer))
        (should (= snapshot-count 1))))))

(ert-deftest opencode-shell-event-delta-completion-matches-snapshot-lifecycle ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (opencode-shell--render-messages
     '(((info . ((id . "user-active") (sessionID . "session-a")
                 (role . "user"))) (parts . nil))
       ((info . ((id . "assistant-active") (sessionID . "session-a")
                 (role . "assistant") (parentID . "user-active")))
        (parts . (((id . "part-active") (sessionID . "session-a")
                   (messageID . "assistant-active") (type . "tool")
                   (tool . "bash") (state . ((status . "running")))))))) 1)
    (opencode-shell--receive-application-event
     '(:kind message-updated :type "message.updated" :session-id "session-a"
       :message-id "assistant-active"
       :info ((id . "assistant-active") (sessionID . "session-a")
              (role . "assistant") (parentID . "user-active")
               (finish . "stop") (time . ((completed . 2))))))
    (opencode-shell--receive-application-event
     '(:kind part-updated :type "message.part.updated" :session-id "session-a"
       :message-id "assistant-active" :part-id "part-active"
       :part ((id . "part-active") (sessionID . "session-a")
              (messageID . "assistant-active") (type . "tool") (tool . "bash")
              (state . ((status . "completed"))))))
    (opencode-shell-async-drain (current-buffer))
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                'complete))
    (should (equal opencode-shell--request-status "idle"))))

(ert-deftest opencode-shell-event-recovers-a-live-transcript-with-a-nil-cache ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--message-envelopes nil
          opencode-shell--session-id "session-a")
    (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
      (opencode-shell--receive-application-event
       '(:kind message-updated :type "message.updated" :session-id "session-a"
         :message-id "user-live"
         :info ((id . "user-live") (sessionID . "session-a") (role . "user"))))
      (should (hash-table-p opencode-shell--message-envelopes))
      (should (gethash "user-live" opencode-shell--message-envelopes)))))

(ert-deftest opencode-shell-event-resets-growing-spinner-for-transcript-progress ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--animation-frame 6
          opencode-shell--session-id "session-a")
    (opencode-shell--receive-application-event
     '(:kind message-updated :type "message.updated" :session-id "session-a"
       :message-id "user-live"
       :info ((id . "user-live") (sessionID . "session-a") (role . "user"))))
    (should (zerop opencode-shell--animation-frame))
    (setq opencode-shell--animation-frame 4)
    (opencode-shell--receive-application-event
     '(:kind snapshot :type "permission.updated" :reason unsupported
       :session-id "session-a"))
    (should (= opencode-shell--animation-frame 4))))

(ert-deftest opencode-shell-event-drops-delivery-after-major-mode-transition ()
  (let ((buffer (generate-new-buffer " *event-mode-transition*")))
    (unwind-protect
        (with-current-buffer buffer
          (opencode-shell-mode)
          (setq opencode-shell--message-envelopes nil)
          (fundamental-mode)
          (opencode-shell--receive-application-event
           '(:kind message-updated :type "message.updated" :session-id "s"
             :message-id "stale"
             :info ((id . "stale") (sessionID . "s") (role . "user"))))
          (should-not opencode-shell--message-envelopes))
      (kill-buffer buffer))))

(ert-deftest opencode-shell-event-user-removal-cleans-only-child-message-cache ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (opencode-shell--render-messages
     '(((info . ((id . "u1") (sessionID . "session-a") (role . "user")))
        (parts . nil))
       ((info . ((id . "a1") (sessionID . "session-a")
                  (role . "assistant") (parentID . "u1")))
        (parts . nil))
       ((info . ((id . "u2") (sessionID . "session-a") (role . "user")))
        (parts . nil))
       ((info . ((id . "a2") (sessionID . "session-a")
                  (role . "assistant") (parentID . "u2")))
        (parts . nil))) 1)
    (opencode-shell--receive-application-event
     '(:kind message-removed :type "message.removed" :session-id "session-a"
       :message-id "u1"))
    (should-not (gethash "u1" opencode-shell--message-envelopes))
    (should-not (gethash "a1" opencode-shell--message-envelopes))
    (should (gethash "u2" opencode-shell--message-envelopes))
    (should (gethash "a2" opencode-shell--message-envelopes))
    (should (equal opencode-shell--message-order '("u2" "a2")))
    (should (equal (mapcar #'opencode-shell--turn-server-user-id
                           opencode-shell--turns)
                   '("u2")))))

(ert-deftest opencode-shell-event-interleaved-parent-removal-leaves-no-orphan ()
  (let ((user
         '(:kind message-updated :type "message.updated" :session-id "s"
           :message-id "u"
           :info ((id . "u") (sessionID . "s") (role . "user"))))
        (child
         '(:kind message-updated :type "message.updated" :session-id "s"
           :message-id "a"
           :info ((id . "a") (sessionID . "s") (role . "assistant")
                  (parentID . "u"))))
        (removal
         '(:kind message-removed :type "message.removed" :session-id "s"
           :message-id "u")))
    (dolist (events (list (list user child removal)
                          (list user removal child)))
      (with-temp-buffer
        (opencode-shell-mode)
        (setq opencode-shell--session-id "s")
        (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                  ((symbol-function 'timerp) (lambda (value) (eq value 'timer)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (dolist (event events)
            (opencode-shell-async-enqueue
             (current-buffer) (opencode-shell-event-identity event)
             opencode-shell--generation
             #'opencode-shell--receive-application-event event))
          (opencode-shell-async-drain (current-buffer))
          (should-not (gethash "u" opencode-shell--message-envelopes))
          (should-not (gethash "a" opencode-shell--message-envelopes))
          (should-not opencode-shell--message-order)
          (should (gethash "u" opencode-shell--removed-message-ids)))))))

(ert-deftest opencode-shell-event-decodes-and-coalesces-part-delta-events ()
  (let ((delta-1
         (opencode-shell-event-decode
          '(:data "{\"type\":\"message.part.delta\",\"properties\":{\"sessionID\":\"s1\",\"messageID\":\"a1\",\"partID\":\"p1\",\"field\":\"text\",\"delta\":\"안녕 \"}}")))
        (delta-2
         (opencode-shell-event-decode
          '(:data "{\"type\":\"message.part.delta\",\"properties\":{\"sessionID\":\"s1\",\"messageID\":\"a1\",\"partID\":\"p1\",\"field\":\"text\",\"delta\":\"세계\"}}"))))
    (should (equal delta-1
                   '(:kind part-delta :type "message.part.delta"
                     :session-id "s1" :message-id "a1" :part-id "p1"
                     :field "text" :delta "안녕 ")))
    (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
          (buffer (generate-new-buffer " *event-delta-coalesce*"))
          (runtime (list :subscribers (make-hash-table :test #'eq)
                         :attempt 'current :backoff 1 :failures 0))
          delivered)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local opencode-shell--generation 1))
            (puthash buffer
                     (list :callback #'ignore :session-id "s1"
                           :event-callback (lambda (ev) (push ev delivered)))
                     (plist-get runtime :subscribers))
            (puthash 'server runtime opencode-shell-async--runtimes)
            (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                      ((symbol-function 'timerp) (lambda (v) (eq v 'timer))))
              (opencode-shell-async--deliver-runtime runtime 'event delta-1)
              (opencode-shell-async--deliver-runtime runtime 'event delta-2)
              (opencode-shell-async-drain buffer))
            (should (= (length delivered) 1))
            (should (equal (plist-get (car delivered) :delta) "안녕 세계")))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest opencode-shell-event-scopes-session-updated-by-info-id-and-ignores-noise ()
  (let ((session-updated
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.updated\",\"properties\":{\"info\":{\"id\":\"session-a\"}}}")))
        (busy-status
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.status\",\"properties\":{\"sessionID\":\"session-a\",\"status\":{\"type\":\"busy\"}}}")))
        (idle-status
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.status\",\"properties\":{\"sessionID\":\"session-a\",\"status\":{\"type\":\"idle\"}}}")))
        (heartbeat
         (opencode-shell-event-decode
          '(:data "{\"type\":\"server.heartbeat\",\"properties\":{}}")))
        (file-edited
         (opencode-shell-event-decode
          '(:data "{\"type\":\"file.edited\",\"properties\":{\"file\":\"a.el\"}}")))
        (lsp-diag
         (opencode-shell-event-decode
          '(:data "{\"type\":\"lsp.client.diagnostics\",\"properties\":{}}")))
        (session-diff
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.diff\",\"properties\":{\"sessionID\":\"session-a\"}}"))))
    (should (equal (plist-get session-updated :session-id) "session-a"))
    (should (eq (plist-get session-updated :resource) 'messages))
    (should (eq (plist-get busy-status :resource) 'none))
    (should (eq (plist-get idle-status :resource) 'messages))
    (dolist (ignored (list heartbeat file-edited lsp-diag session-diff))
      (should (eq (plist-get ignored :resource) 'none)))
    (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
          (buffer (generate-new-buffer " *event-ignore-noise*"))
          (runtime (list :subscribers (make-hash-table :test #'eq)
                         :attempt 'current :backoff 1 :failures 0)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local opencode-shell--generation 1))
            (puthash buffer
                     (list :callback #'ignore :session-id "session-a"
                           :event-callback #'ignore)
                     (plist-get runtime :subscribers))
            (opencode-shell-async--deliver-runtime runtime 'event heartbeat)
            (opencode-shell-async--deliver-runtime runtime 'event busy-status)
            (with-current-buffer buffer
              (should-not opencode-shell-async--queue)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest opencode-shell-event-applies-user-part-and-part-delta-without-snapshot ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (let ((snapshot-count 0))
      (cl-letf (((symbol-function 'opencode-shell--resync)
                 (lambda (&rest _) (cl-incf snapshot-count)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
        (opencode-shell--receive-application-event
         '(:kind message-updated :type "message.updated"
           :session-id "session-a" :message-id "u-stream"
           :info ((id . "u-stream") (sessionID . "session-a") (role . "user"))))
        (opencode-shell--receive-application-event
         '(:kind part-updated :type "message.part.updated"
           :session-id "session-a" :message-id "u-stream" :part-id "p-user"
           :part ((id . "p-user") (sessionID . "session-a")
                  (messageID . "u-stream") (type . "text")
                  (text . "질문입니다"))))
        (opencode-shell--receive-application-event
         '(:kind message-updated :type "message.updated"
           :session-id "session-a" :message-id "a-stream"
           :info ((id . "a-stream") (sessionID . "session-a")
                  (role . "assistant") (parentID . "u-stream"))))
        (opencode-shell--receive-application-event
         '(:kind part-updated :type "message.part.updated"
           :session-id "session-a" :message-id "a-stream" :part-id "p-text"
           :part ((id . "p-text") (sessionID . "session-a")
                  (messageID . "a-stream") (type . "text")
                  (text . "답변: "))))
        (opencode-shell--receive-application-event
         '(:kind part-delta :type "message.part.delta"
           :session-id "session-a" :message-id "a-stream" :part-id "p-text"
           :field "text" :delta "성공"))
        (opencode-shell-async-drain (current-buffer))
        (should (zerop snapshot-count))
        (should (= (length opencode-shell--turns) 1))
        (should (equal (opencode-shell--turn-user (car opencode-shell--turns))
                       "질문입니다"))
        (should (equal (opencode-shell--turn-assistant (car opencode-shell--turns))
                       "답변: 성공"))))))

(ert-deftest opencode-shell-event-retains-orphan-part-when-message-updated-arrives-later ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (opencode-shell--render-messages
     '(((info . ((id . "u1") (sessionID . "session-a") (role . "user")))
        (parts . (((id . "pu1") (type . "text") (text . "hi")))))) 1)
    (let ((snapshot-count 0))
      (cl-letf (((symbol-function 'opencode-shell--resync)
                 (lambda (&rest _) (cl-incf snapshot-count)))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
        (opencode-shell--receive-application-event
         '(:kind part-updated :type "message.part.updated"
           :session-id "session-a" :message-id "a-late" :part-id "p-late"
           :part ((id . "p-late") (sessionID . "session-a")
                  (messageID . "a-late") (type . "text")
                  (text . "먼저 도착한 파트"))))
        (opencode-shell--receive-application-event
         '(:kind message-updated :type "message.updated"
           :session-id "session-a" :message-id "a-late"
           :info ((id . "a-late") (sessionID . "session-a")
                  (role . "assistant") (parentID . "u1"))))
        (opencode-shell-async-drain (current-buffer))
        (should (zerop snapshot-count))
        (should (equal (opencode-shell--turn-assistant (car opencode-shell--turns))
                       "먼저 도착한 파트"))))))

(ert-deftest opencode-shell-event-cancels-revision-reconcile-loop-when-streaming-continues ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--render-messages
     '(((info . ((id . "u1") (role . "user") (sessionID . "s")))
        (parts . (((id . "pu1") (type . "text") (text . "current")))))
       ((info . ((id . "a1") (role . "assistant")
                 (sessionID . "s") (parentID . "u1")))
        (parts . nil))) 1)
    (let (message-callback)
      (cl-letf (((symbol-function 'opencode-shell--guarded-request)
                 (lambda (key _method _path callback &rest _)
                   (when (eq key 'messages) (setq message-callback callback))))
                ((symbol-function 'opencode-shell--refresh-questions) #'ignore)
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
        (opencode-shell--resync)
        (opencode-shell--receive-application-event
         '(:kind part-updated :type "message.part.updated" :session-id "s"
           :message-id "a1" :part-id "p1"
           :part ((id . "p1") (sessionID . "s") (messageID . "a1")
                  (type . "text") (text . "stream-1"))))
        (funcall message-callback
                 '(((info . ((id . "u1") (role . "user") (sessionID . "s")))
                    (parts . (((id . "pu1") (type . "text") (text . "current")))))
                   ((info . ((id . "a1") (role . "assistant")
                             (sessionID . "s") (parentID . "u1")))
                    (parts . nil))))
        (should (alist-get '(event-reconcile messages)
                           opencode-shell-async--queue nil nil #'equal))
        (opencode-shell--receive-application-event
         '(:kind part-delta :type "message.part.delta" :session-id "s"
           :message-id "a1" :part-id "p1" :field "text" :delta "-stream-2"))
        (should-not (alist-get '(event-reconcile messages)
                               opencode-shell-async--queue nil nil #'equal))
        (should (equal (opencode-shell--turn-assistant
                        (car (last opencode-shell--turns)))
                       "stream-1-stream-2"))))))

(ert-deftest opencode-shell-event-session-updated-and-settled-idle-avoid-snapshot-storm ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (opencode-shell--render-messages
     '(((info . ((id . "u1") (sessionID . "session-a") (role . "user")))
        (parts . (((id . "pu1") (type . "text") (text . "hi")))))
       ((info . ((id . "a1") (sessionID . "session-a")
                 (role . "assistant") (parentID . "u1")
                 (finish . "stop") (time . ((completed . 2)))))
        (parts . (((id . "pa1") (type . "text") (text . "done")))))) 1)
    (let ((snapshot-count 0))
      (cl-letf (((symbol-function 'opencode-shell--resync)
                 (lambda (&rest _) (cl-incf snapshot-count)))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
                ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
        (opencode-shell--receive-application-event
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.updated\",\"properties\":{\"info\":{\"id\":\"session-a\",\"title\":\"새 제목\"}}}")))
        (opencode-shell--receive-application-event
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.status\",\"properties\":{\"sessionID\":\"session-a\",\"status\":{\"type\":\"idle\"}}}")))
        (opencode-shell--receive-application-event
         (opencode-shell-event-decode
          '(:data "{\"type\":\"session.idle\",\"properties\":{\"sessionID\":\"session-a\"}}")))
        (opencode-shell-async-drain (current-buffer))
        (should (zerop snapshot-count))
        (should (equal opencode-shell--session-title "새 제목"))
        (should opencode-shell--session-idle-p)))))

(ert-deftest opencode-shell-compacts-large-tool-and-reasoning-payloads-in-cache ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a")
    (let ((big-input (make-string 5000 ?x))
          (big-output (concat "<task id=\"child-sess-1\">" (make-string 5000 ?y)))
          (big-thought (make-string 5000 ?z)))
      (opencode-shell--render-messages
       `(((info . ((id . "u1") (sessionID . "session-a") (role . "user")))
          (parts . (((id . "pu1") (type . "text") (text . "run")))))
         ((info . ((id . "a1") (sessionID . "session-a")
                   (role . "assistant") (parentID . "u1")))
          (parts . (((id . "r1") (type . "reasoning") (text . ,big-thought))
                    ((id . "t1") (type . "tool") (tool . "task")
                     (state . ((status . "running")
                               (input . ,big-input)
                               (output . ,big-output)))))))) 1)
      (let* ((cached (gethash "a1" opencode-shell--message-envelopes))
             (parts (opencode-shell--get cached 'parts))
             (reasoning-part (car parts))
             (tool-part (cadr parts))
             (tool-state (opencode-shell--get tool-part 'state))
             (turn (car opencode-shell--turns)))
        (should-not (opencode-shell--get tool-state 'input))
        (should (<= (length (opencode-shell--get tool-state 'output)) 500))
        (should (opencode-shell--turn-spawned-session-p turn "child-sess-1"))
        (should (<= (length (or (opencode-shell--get reasoning-part 'text) "")) 256))))))

(ert-deftest opencode-shell-event-resets-idle-flag-on-new-turn-and-attaches-early-assistant ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "session-a"
          opencode-shell--session-idle-p t)
    (cl-letf (((symbol-function 'opencode-shell--refresh-session-metadata) #'ignore)
              ((symbol-function 'run-with-idle-timer) (lambda (&rest _) 'timer))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
              ((symbol-function 'timerp) (lambda (value) (eq value 'timer))))
      ;; Assistant arrives before user message; then user arrives and task tool starts.
      (opencode-shell--receive-application-event
       '(:kind message-updated :type "message.updated"
         :session-id "session-a" :message-id "a2"
         :info ((id . "a2") (sessionID . "session-a")
                (role . "assistant") (parentID . "u2"))))
      (opencode-shell--receive-application-event
       '(:kind part-updated :type "message.part.updated"
         :session-id "session-a" :message-id "a2" :part-id "t2"
         :part ((id . "t2") (sessionID . "session-a")
                (messageID . "a2") (type . "tool") (tool . "task")
                (state . ((status . "running"))))))
      (opencode-shell--receive-application-event
       '(:kind message-updated :type "message.updated"
         :session-id "session-a" :message-id "u2"
         :info ((id . "u2") (sessionID . "session-a") (role . "user"))))
      (should-not opencode-shell--session-idle-p)
      (should (= (length opencode-shell--turns) 1))
      ;; Turn must stay active (`receiving'), not prematurely settle from stale idle flag.
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                  'receiving))
      ;; Once session.idle arrives for this turn, the task-only turn settles.
      (opencode-shell--receive-application-event
       (opencode-shell-event-decode
        '(:data "{\"type\":\"session.idle\",\"properties\":{\"sessionID\":\"session-a\"}}")))
      (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                  'complete)))))

(provide 'opencode-shell-event-test)
;;; opencode-shell-event-test.el ends here
