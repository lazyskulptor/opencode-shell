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

(provide 'opencode-shell-state-test)
;;; opencode-shell-state-test.el ends here
