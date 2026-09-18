;;; opencode-shell-pipeline-test.el --- Pipeline characterization tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Code:

(require 'ert)
(require 'opencode-shell)

(defun opencode-shell-pipeline-test--message (id role text &optional parent-id)
  "Return a minimal message envelope for ID, ROLE, TEXT, and PARENT-ID."
  `((info . ((id . ,id) (role . ,role)
             ,@(and parent-id `((parentID . ,parent-id)))))
    (parts . (((type . "text") (text . ,text))))))

(ert-deftest opencode-shell-pipeline-characterizes-tool-redaction-and-active-turn ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list
      (opencode-shell-pipeline-test--message "u1" "user" "inspect")
      '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
        (parts . (((type . "tool") (tool . "shell")
                   (state . ((status . "running")
                             (input . "PRIVATE TOOL PAYLOAD")))))))))
    (should (eq (opencode-shell--turn-status (car opencode-shell--turns))
                'receiving))
    (should (string-match-p "TOOL> shell" (buffer-string)))
    (should-not (string-match-p "PRIVATE TOOL PAYLOAD" (buffer-string)))))

(ert-deftest opencode-shell-pipeline-restored-receiving-turn-hides-composer ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list
      (opencode-shell-pipeline-test--message "u1" "user" "question")
      '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
        (parts . (((id . "p1") (type . "text") (text . "partial")))))))
    (should-not opencode-shell--submit-in-flight)
    (should-not (opencode-shell--composer-visible-p))
    (should (= 0 (how-many "Prompt>\n" (point-min) (point-max))))))

(ert-deftest opencode-shell-pipeline-characterizes-response-protection ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages
     (list
      (opencode-shell-pipeline-test--message "u1" "user" "question")
      '((info . ((id . "a1") (role . "assistant") (parentID . "u1")
                  (finish . "stop") (time . ((completed . 1)))))
        (parts . (((type . "text") (text . "answer")))))))
    (let* ((turn (car opencode-shell--turns))
           (begin (marker-position (opencode-shell--turn-response-begin turn)))
           (end (marker-position (opencode-shell--turn-response-end turn))))
      (should (< begin end))
      (should (eq (get-text-property begin 'read-only) t))
      (should (< end (marker-position opencode-shell--composer-start))))))

(ert-deftest opencode-shell-pipeline-characterizes-interaction-order-and-draft ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq-local opencode-shell--session-id "s")
    (goto-char opencode-shell--composer-start)
    (insert "draft")
    (opencode-shell--receive-questions
     '(((id . "q1") (sessionID . "s") (question . "Choose"))))
    (should (string-match-p "┌─ QUESTION" (buffer-string)))
    (should (equal (opencode-shell--composer-text) "draft"))
    (opencode-shell--receive-permissions
     '(((id . "p1") (sessionID . "s") (permission . "read"))))
    (should (string-match-p "┌─ PERMISSION" (buffer-string)))
    (should-not (string-match-p "┌─ QUESTION" (buffer-string)))
    (should (equal (opencode-shell--composer-text) "draft"))))

(ert-deftest opencode-shell-pipeline-stale-interaction-callback-cannot-settle-new-request ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let (first-callback second-callback completed)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path callback &rest _)
                   (if first-callback
                       (setq second-callback callback)
                     (setq first-callback callback)))))
        (opencode-shell--interaction-request
         'permission "p1" "POST" "/permission/p1/reply"
         (lambda (_) (push "p1" completed)))
        (funcall first-callback nil)
        (opencode-shell--interaction-request
         'permission "p2" "POST" "/permission/p2/reply"
         (lambda (_) (push "p2" completed)))
        (funcall first-callback nil)
        (should (equal completed '("p1")))
        (should (opencode-shell-interaction-matches-p
                 opencode-shell--interaction-state 'permission "p2"))
        (funcall second-callback nil)
        (should (equal completed '("p2" "p1")))
        (should-not opencode-shell--interaction-state)))))

(ert-deftest opencode-shell-pipeline-question-request-blocks-until-callback ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s"
          opencode-shell--questions-pending
          '(((id . "q1") (sessionID . "s") (question . "Choose")
             (options . (((label . "A")))))))
    (let (callback request)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "A"))
                ((symbol-function 'opencode-shell--resync) #'ignore)
                ((symbol-function 'opencode-shell--request)
                 (lambda (method path success &optional body &rest _)
                   (setq callback success request (list method path body)))))
        (opencode-shell--questions)
        (should (equal request
                       '("POST" "/question/q1/reply" ((answers . [["A"]])))))
        (should (opencode-shell-interaction-active-p
                 opencode-shell--interaction-state 'question))
        (setq opencode-shell--questions-pending nil)
        (should-not (opencode-shell--composer-visible-p))
        (funcall callback nil)
        (should-not opencode-shell--interaction-state)
        (should (opencode-shell--composer-visible-p))))))

(ert-deftest opencode-shell-pipeline-question-failure-clears-only-in-flight-state ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let* ((item '((id . "q1") (question . "Choose")))
           (opencode-shell--questions-pending (list item))
           error-callback resyncs)
      (cl-letf (((symbol-function 'opencode-shell--resync)
                 (lambda (&optional full) (push full resyncs)))
                ((symbol-function 'opencode-shell--request)
                 (lambda (_method _path _success &optional _body _params failure)
                   (setq error-callback failure))))
        (opencode-shell--question-reject item)
        (should (opencode-shell-interaction-active-p
                 opencode-shell--interaction-state 'question))
        (funcall error-callback)
        (should-not opencode-shell--interaction-state)
        (should (equal opencode-shell--questions-pending (list item)))
        (should (equal resyncs '(nil)))))))

(ert-deftest opencode-shell-pipeline-canonical-error-survives-every-render-path ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((turn (opencode-shell--make-turn :id "t" :user "q" :status 'waiting)))
      (setq opencode-shell--turns (list turn))
      (opencode-shell--render-turns)
      (setf (opencode-shell--turn-status turn) 'error)
      (opencode-shell--render-turns nil (list turn))
      (should (string-match-p
               "Request state is uncertain; resync with g r" (buffer-string)))
      (should-not (string-match-p "Request failed" (buffer-string)))
      (let ((incremental (buffer-string)))
        (opencode-shell--render-turns t)
        (should (equal incremental (buffer-string)))))))

(ert-deftest opencode-shell-pipeline-hydration-blocks-until-empty-snapshots-settle ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--begin-initial-hydration)
    (should-not (opencode-shell--composer-visible-p))
    (should (string-match-p "Loading session" (buffer-string)))
    (opencode-shell--receive-permissions nil)
    (opencode-shell--receive-questions nil)
    (opencode-shell--settle-hydration 'messages t)
    (opencode-shell--render-turns)
    (opencode-shell--render-permissions)
    (should (opencode-shell--composer-visible-p))
    (should (= 1 (how-many "Prompt>\n" (point-min) (point-max))))))

(ert-deftest opencode-shell-pipeline-hydration-failure-stays-blocked-until-retry ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--begin-initial-hydration)
    (dolist (resource '(permissions questions))
      (opencode-shell--settle-hydration resource t))
    (opencode-shell--settle-hydration 'messages nil)
    (opencode-shell--render-turns)
    (opencode-shell--render-permissions)
    (should-not (opencode-shell--composer-visible-p))
    (should (string-match-p "retry with g r" (buffer-string)))
    (opencode-shell--retry-hydration '(messages))
    (opencode-shell--settle-hydration 'messages t)
    (opencode-shell--render-turns)
    (opencode-shell--render-permissions)
    (should (opencode-shell--composer-visible-p))))

(ert-deftest opencode-shell-pipeline-receiving-reentry-remains-blocked-after-hydration ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--begin-initial-hydration)
    (opencode-shell--render-messages
     (list (opencode-shell-pipeline-test--message "u1" "user" "question")
           '((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
             (parts . (((type . "text") (text . "partial")))))))
    (dolist (resource '(messages permissions questions))
      (opencode-shell--settle-hydration resource t))
    (should-not (opencode-shell--composer-visible-p))))

(ert-deftest opencode-shell-pipeline-resync-settles-out-of-order-authoritative-callbacks ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "s")
    (opencode-shell--begin-initial-hydration)
    (let (callbacks failures)
      (cl-letf (((symbol-function 'opencode-shell--guarded-request)
                 (lambda (key _method _path success &optional _body failure)
                   (setf (alist-get key callbacks) success
                         (alist-get key failures) failure))))
        (opencode-shell--resync nil)
        (funcall (alist-get 'questions callbacks) nil)
        (funcall (alist-get 'permissions callbacks) nil)
        (should-not (opencode-shell--composer-visible-p))
        (funcall (alist-get 'messages callbacks) nil)
        (should (opencode-shell--composer-visible-p))
        (opencode-shell--begin-initial-hydration)
        (opencode-shell--resync nil)
        (funcall (alist-get 'messages failures))
        (should-not (opencode-shell--composer-visible-p))
        (opencode-shell--resync nil 'messages)
        (funcall (alist-get 'messages callbacks) nil)
        (funcall (alist-get 'permissions callbacks) nil)
        (funcall (alist-get 'questions callbacks) nil)
        (should (opencode-shell--composer-visible-p))))))

(provide 'opencode-shell-pipeline-test)
;;; opencode-shell-pipeline-test.el ends here
