;;; completion-polling-regression.el --- Sanitized lifecycle fixture -*- lexical-binding: t; -*-

(require 'cl-lib)

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

(defconst opencode-shell-test--sse-wake-burst
  '("data: {\"type\":\"session.updated\"}\n\n"
    "data: {\"type\":\"message.updated\"}\n\n"
    "data: {\"type\":\"session.idle\"}\n\n")
  "Payload-free event burst used to verify wake-up coalescing.")

(defconst opencode-shell-test--message-updated-event
  "{\"type\":\"message.updated\",\"properties\":{\"info\":{\"id\":\"assistant-active\",\"sessionID\":\"session-a\",\"role\":\"assistant\",\"parentID\":\"user-active\"}}}"
  "Sanitized OpenCode 1.18.30 message.updated wire payload.")

(defconst opencode-shell-test--part-updated-event
  "{\"type\":\"message.part.updated\",\"properties\":{\"part\":{\"id\":\"part-active\",\"sessionID\":\"session-a\",\"messageID\":\"assistant-active\",\"type\":\"tool\",\"tool\":\"bash\",\"state\":{\"status\":\"running\"}}}}"
  "Sanitized OpenCode 1.18.30 message.part.updated wire payload.")

(defconst opencode-shell-test--active-transcript-snapshot
  '(((info . ((id . "user-active") (role . "user")))
     (parts . (((type . "text") (text . "질문")))))
    ((info . ((id . "assistant-active") (role . "assistant")
              (parentID . "user-active")))
     (parts . (((id . "assistant-active-text") (type . "text")
                (text . "응답 중"))))))
  "Active message snapshot used by cursor-stability acceptance tests.")

(defconst opencode-shell-test--changed-transcript-snapshot
  '(((info . ((id . "user-active") (role . "user")))
     (parts . (((type . "text") (text . "질문")))))
    ((info . ((id . "assistant-active") (role . "assistant")
              (parentID . "user-active")))
     (parts . (((id . "assistant-active-text") (type . "text")
                (text . "새 응답"))))))
  "Changed message snapshot used to verify one coalesced render.")

(defconst opencode-shell-test--pending-permission-snapshot
  '(((id . "permission-active") (sessionID . "acceptance-stability")
     (permission . "bash")))
  "Pending permission snapshot for stability acceptance tests.")

(defconst opencode-shell-test--pending-question-snapshot
  '(((id . "question-active") (sessionID . "acceptance-stability")
     (questions . (((question . "Continue?"))))))
  "Pending question snapshot for stability acceptance tests.")

(defun opencode-shell-test--large-message-snapshot (turn-count)
  "Return TURN-COUNT completed turns followed by one active turn."
  (append
   (apply #'append
          (cl-loop for index from 1 to turn-count
                   collect
                   `(((info . ((id . ,(format "large-user-%d" index))
                               (role . "user") (sessionID . "large-session")))
                      (parts . nil))
                     ((info . ((id . ,(format "large-assistant-%d" index))
                               (role . "assistant")
                               (sessionID . "large-session")
                               (parentID . ,(format "large-user-%d" index))
                               (finish . "stop") (time . ((completed . ,index)))))
                      (parts . (((id . ,(format "large-text-%d" index))
                                 (type . "text") (text . "done"))))))))
   `(((info . ((id . "large-active-user") (role . "user")
               (sessionID . "large-session"))) (parts . nil))
     ((info . ((id . "large-active-assistant") (role . "assistant")
               (sessionID . "large-session")
               (parentID . "large-active-user")))
      (parts . (((id . "large-active-part") (type . "reasoning")
                 (text . "working"))))))))

(provide 'completion-polling-regression)
;;; completion-polling-regression.el ends here
