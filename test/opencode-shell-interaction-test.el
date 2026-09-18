;;; opencode-shell-interaction-test.el --- Interaction lifecycle tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Code:

(require 'ert)
(require 'opencode-shell-interaction)

(ert-deftest opencode-shell-interaction-begin-records-kind-and-id ()
  (should (equal (opencode-shell-interaction-begin nil 'permission "p1")
                 '((permission . "p1")))))

(ert-deftest opencode-shell-interaction-begin-rejects-duplicate-kind ()
  (should-error
   (opencode-shell-interaction-begin '((permission . "p1"))
                                     'permission "p2")
   :type 'user-error))

(ert-deftest opencode-shell-interaction-finish-removes-only-matching-request ()
  (let ((state '((permission . "p1") (question . "q1"))))
    (should (equal (opencode-shell-interaction-finish state 'permission "p1")
                   '((question . "q1"))))
    (should (eq (opencode-shell-interaction-finish state 'permission "stale")
                state))
    (should (eq (opencode-shell-interaction-finish state 'question "stale")
                state))))

(ert-deftest opencode-shell-interaction-matches-kind-and-id ()
  (let ((state '((permission . "p1") (question . "q1"))))
    (should (opencode-shell-interaction-matches-p state 'permission "p1"))
    (should-not (opencode-shell-interaction-matches-p state 'permission "q1"))
    (should (opencode-shell-interaction-active-p state))
    (should (opencode-shell-interaction-active-p state 'question))))

(ert-deftest opencode-shell-interaction-removes-pending-item-by-id ()
  (let ((pending '(((id . "p1") (permission . "read"))
                   ((id . "p2") (permission . "write")))))
    (should (equal
             (opencode-shell-interaction-remove-pending
              pending "p1" (lambda (item) (alist-get 'id item)))
             '(((id . "p2") (permission . "write")))))
    (should (eq
             (opencode-shell-interaction-remove-pending
              pending "missing" (lambda (item) (alist-get 'id item)))
             pending))))

(ert-deftest opencode-shell-interaction-allows-concurrent-different-kinds ()
  (should (equal
           (opencode-shell-interaction-begin
            '((permission . "p1")) 'question "q1")
           '((permission . "p1") (question . "q1")))))

(provide 'opencode-shell-interaction-test)
;;; opencode-shell-interaction-test.el ends here
