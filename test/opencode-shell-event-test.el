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
    (should (eq (plist-get malformed :kind) 'snapshot))
    (should-not (plist-get malformed :session-id))
    (should (eq (plist-get malformed :reason) 'malformed))))

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

(provide 'opencode-shell-event-test)
;;; opencode-shell-event-test.el ends here
