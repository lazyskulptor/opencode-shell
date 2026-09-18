;;; opencode-shell-state-test.el --- Lifecycle state tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Code:

(require 'ert)
(require 'opencode-shell-state)

(ert-deftest opencode-shell-state-turn-terminality-is-explicit ()
  (dolist (status '(sending waiting thinking receiving recovering aborting error))
    (should-not (opencode-shell-state-turn-terminal-p status)))
  (should (opencode-shell-state-turn-terminal-p 'complete)))

(ert-deftest opencode-shell-state-request-phase-prioritizes-active-turns ()
  (should (eq (opencode-shell-state-request-phase '(complete thinking) nil)
              'thinking))
  (should (eq (opencode-shell-state-request-phase '(complete receiving) nil)
              'receiving))
  (should (eq (opencode-shell-state-request-phase '(complete) nil) 'idle)))

(ert-deftest opencode-shell-state-polling-follows-unsettled-work ()
  (should (opencode-shell-state-polling-needed-p '(receiving) nil))
  (should (opencode-shell-state-polling-needed-p '(complete) "request"))
  (should-not (opencode-shell-state-polling-needed-p '(complete) nil)))

(ert-deftest opencode-shell-state-composer-readiness-truth-table ()
  (should (opencode-shell-state-composer-ready-p nil nil nil t))
  (should (opencode-shell-state-composer-ready-p '(complete) nil nil t))
  (dolist (status '(sending waiting thinking receiving recovering aborting error))
    (should-not
     (opencode-shell-state-composer-ready-p (list status) nil nil t)))
  (should-not
   (opencode-shell-state-composer-ready-p '(complete) "local" nil t))
  (should-not
   (opencode-shell-state-composer-ready-p '(complete) nil 'permission t))
  (should-not
   (opencode-shell-state-composer-ready-p '(complete) nil 'question t))
  (should-not
   (opencode-shell-state-composer-ready-p '(complete) nil nil nil)))

(provide 'opencode-shell-state-test)
;;; opencode-shell-state-test.el ends here
