;;; completion-polling-regression.el --- Sanitized lifecycle fixture -*- lexical-binding: t; -*-

(defconst opencode-shell-test--completion-polling-snapshots
  '((((info . ((id . "user-1") (role . "user")))
      (parts . nil))
     ((info . ((id . "assistant-1") (role . "assistant")
               (parentID . "user-1") (time . ((created . 1)))))
      (parts . (((id . "tool-1") (type . "tool")
                 (state . ((status . "running"))))))))
    (((info . ((id . "user-1") (role . "user")))
      (parts . nil))
     ((info . ((id . "assistant-1") (role . "assistant")
               (parentID . "user-1") (finish . "stop")
               (time . ((created . 1) (completed . 2)))))
      (parts . (((id . "tool-1") (type . "tool")
                 (state . ((status . "completed")))))))))
  "Metadata-only history snapshots for completion polling regression tests.")

(provide 'completion-polling-regression)
;;; completion-polling-regression.el ends here
