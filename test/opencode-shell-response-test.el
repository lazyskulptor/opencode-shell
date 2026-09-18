;;; opencode-shell-response-test.el --- Response normalization tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors

;;; Code:

(require 'ert)
(require 'opencode-shell-response)

(ert-deftest opencode-shell-response-normalizes-part-table ()
  (dolist (case
           '((((type . "tool") (tool . "read")
               (state . ((status . "running"))))
              tool "read" "running")
             (((type . "tool_use") (name . "write")
               (status . "completed"))
              tool "write" "completed")
             (((type . "tool-result") (tool . "shell")
               (state . ((status . "error"))))
              tool "shell" "error")
             (((type . "text") (text . "answer")) text nil nil)
             (((type . "reasoning") (text . "private")) reasoning nil nil)
             (((type . "step-finish")) step-finish nil nil)
             (((type . "future") (payload . "private")) unknown nil nil)))
    (pcase-let ((`(,part ,kind ,name ,status) case))
      (let ((model (opencode-shell-response-normalize-part part)))
        (should (eq (plist-get model :kind) kind))
        (should (equal (plist-get model :name) name))
        (should (equal (plist-get model :status) status))))))

(ert-deftest opencode-shell-response-tool-names-are-ordered-and-deduplicated ()
  (should
   (equal
    (opencode-shell-response-tool-names
     '(((type . "tool") (tool . "read"))
       ((type . "tool_use") (name . "write"))
       ((type . "tool-result") (tool . "read"))
       ((type . "tool"))
       ((type . "future") (name . "private"))))
    '("read" "write"))))

(ert-deftest opencode-shell-response-phase-follows-visible-progress ()
  (should (eq (opencode-shell-response-phase nil) 'waiting))
  (should (eq (opencode-shell-response-phase
               '(((type . "reasoning") (text . "thinking"))))
              'thinking))
  (should (eq (opencode-shell-response-phase
               '(((type . "reasoning")) ((type . "tool-result"))))
              'receiving))
  (should (eq (opencode-shell-response-phase
               '(((type . "text") (text . "answer"))))
              'receiving)))

(ert-deftest opencode-shell-response-running-tools-block-completion ()
  (dolist (part '(((type . "tool") (state . ((status . "running"))))
                  ((type . "tool_use") (status . "pending"))
                  ((type . "tool-result"))))
    (should (opencode-shell-response-running-tool-p part)))
  (dolist (part '(((type . "tool") (state . ((status . "completed"))))
                  ((type . "tool_use") (status . "error"))
                  ((type . "text") (status . "running"))))
    (should-not (opencode-shell-response-running-tool-p part))))

(ert-deftest opencode-shell-response-completion-evidence-is-normalized ()
  (should
   (opencode-shell-response-envelope-complete-p
    '((finish . "stop") (time . ((completed . 2))))
    '(((type . "text") (text . "done")))))
  (should-not
   (opencode-shell-response-envelope-complete-p
    '((finish . "tool-calls") (time . ((completed . 2))))
    '(((type . "tool") (state . ((status . "completed")))))))
  (should-not
   (opencode-shell-response-envelope-complete-p
    '((finish . "stop") (time . ((completed . 2))))
    '(((type . "tool") (state . ((status . "running")))))))
  (should
   (opencode-shell-response-envelope-complete-p
    nil '(((type . "step-finish"))))))

(ert-deftest opencode-shell-response-display-text-redacts-payloads ()
  (let* ((secret "NEVER-RENDER-RAW-PAYLOAD")
         (display
          (opencode-shell-response-part-display-text
           `((type . "tool_use") (name . "shell")
             (status . "running") (input . ,secret)))))
    (should (equal display "[tool shell: running]"))
    (should-not (string-match-p secret display)))
  (should (equal (opencode-shell-response-part-display-text
                  '((type . "text") (text . "answer")))
                 "answer"))
  (should (equal (opencode-shell-response-part-display-text
                  '((type . "unknown") (payload . "private")))
                 "")))

(provide 'opencode-shell-response-test)
;;; opencode-shell-response-test.el ends here
