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

(provide 'opencode-shell-pipeline-test)
;;; opencode-shell-pipeline-test.el ends here
