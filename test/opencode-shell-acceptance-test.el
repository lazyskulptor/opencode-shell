;;; opencode-shell-acceptance-test.el --- Workflow acceptance tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'opencode-shell)
(require 'completion-polling-regression)

(defun opencode-shell-acceptance--messages (count)
  "Return COUNT complete chronological conversation turns."
  (apply #'append
         (cl-loop for n from 1 to count
                  collect `(((info . ((id . ,(format "u%d" n)) (role . "user")))
                             (parts . (((type . "text")
                                        (text . ,(format "질문 %d" n))))))
                            ((info . ((id . ,(format "a%d" n))
                                      (role . "assistant")
                                      (parentID . ,(format "u%d" n))
                                      (finish . "stop")
                                      (time . ((completed . ,n)))))
                             (parts . (((id . ,(format "a%d-text" n))
                                        (type . "text")
                                        (text . ,(format "응답 %d" n))))))))))

(ert-deftest opencode-shell-acceptance-ten-turn-conversation ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "acceptance")
    (let (success)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path callback &optional _body _params _error)
                   (setq success callback))))
        (dotimes (index 10)
          (goto-char (point-max))
          (insert (format "질문 %d" (1+ index)))
          (opencode-shell--submit)
          (funcall success nil)
          (opencode-shell--render-messages
           (opencode-shell-acceptance--messages (1+ index)))
          (should (= (length opencode-shell--turns) (1+ index)))
          (should (string-empty-p (opencode-shell--composer-text)))
          (should (= (point) opencode-shell--composer-start))
          (should (eq (get-text-property (point-min) 'read-only) t)))))))

(ert-deftest opencode-shell-acceptance-insert-entry-focuses-composer ()
  (dolist (_command '(i I a A o O))
    (with-temp-buffer
      (opencode-shell-mode)
      (let ((inhibit-read-only t))
        (goto-char (point-min)))
      (opencode-shell--evil-move-to-composer)
      (should (= (point) (point-max)))
      (should (opencode-shell--in-composer-p)))))

(ert-deftest opencode-shell-acceptance-queued-response-preserves-active-draft ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let (visible)
      (goto-char (point-max))
      (insert "작성 중인 초안")
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) visible)))
        (opencode-shell--render-messages
         (opencode-shell-acceptance--messages 2) 1 t)
        (should (equal (opencode-shell--composer-text) "작성 중인 초안"))
        (setq visible t)
        (opencode-shell--render-if-visible)
         (opencode-shell-async-drain (current-buffer))
         (should (equal (opencode-shell--composer-text) "작성 중인 초안"))))))

(ert-deftest opencode-shell-acceptance-identical-active-snapshot-is-a-no-op ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((messages
           '(((info . ((id . "u1") (role . "user")))
              (parts . (((type . "text") (text . "질문")))))
             ((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
              (parts . (((id . "a1-text") (type . "text") (text . "응답 중"))))))))
      (opencode-shell--render-messages messages 1)
      (goto-char opencode-shell--composer-start)
      (insert "작성 중")
      (goto-char (+ opencode-shell--composer-start 2))
      (let ((before (buffer-string))
            (tick (buffer-chars-modified-tick))
            (position (point)))
        (opencode-shell--render-messages messages 2 t)
        (opencode-shell-async-drain (current-buffer))
        (should (equal before (buffer-string)))
        (should (= tick (buffer-chars-modified-tick)))
        (should (= position (point)))
        (should-not opencode-shell--render-dirty)))))

(ert-deftest opencode-shell-acceptance-polling-preserves-editor-state-and-renders-changes-once ()
  (let* ((buffer (generate-new-buffer " *accept-stable-editor*"))
         (window (selected-window))
         (original-buffer (window-buffer window))
         (original-start (window-start window)))
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (setq opencode-shell--session-id "acceptance-stability")
            (opencode-shell--render-messages
             opencode-shell-test--active-transcript-snapshot 1)
            (opencode-shell--receive-permissions
             opencode-shell-test--pending-permission-snapshot)
            (opencode-shell--receive-questions
             opencode-shell-test--pending-question-snapshot)
            (goto-char opencode-shell--composer-start)
            (insert "작성 중인 초안")
            (goto-char (+ opencode-shell--composer-start 4))
            (set-window-start window (point-min))
            (let ((text (buffer-string))
                  (tick (buffer-chars-modified-tick))
                  (position (point))
                  (viewport (window-start window))
                  (undo (copy-tree buffer-undo-list))
                  (composer (marker-position opencode-shell--composer-start))
                  (response (marker-position
                             (opencode-shell--turn-response-begin
                              (car opencode-shell--turns)))))
              (dotimes (_ 3)
                (opencode-shell--render-messages
                 opencode-shell-test--active-transcript-snapshot 2 t)
                (opencode-shell--receive-permissions
                 opencode-shell-test--pending-permission-snapshot t)
                (opencode-shell--receive-questions
                 opencode-shell-test--pending-question-snapshot t)
                (opencode-shell--animation-tick))
              (opencode-shell-async-drain buffer)
              (should (equal text (buffer-string)))
              (should (= tick (buffer-chars-modified-tick)))
              (should (= position (point)))
              (should (= viewport (window-start window)))
              (should (equal undo buffer-undo-list))
              (should (= composer (marker-position opencode-shell--composer-start)))
              (should (= response
                         (marker-position
                          (opencode-shell--turn-response-begin
                           (car opencode-shell--turns))))))
            (opencode-shell--render-messages
             opencode-shell-test--changed-transcript-snapshot 3 t)
            (opencode-shell--receive-permissions nil t)
            (opencode-shell--receive-questions nil t)
            (should (= (length opencode-shell-async--queue) 1))
            (opencode-shell-async-drain buffer)
            (should (equal (opencode-shell--turn-assistant
                            (car opencode-shell--turns))
                           "새 응답"))
            (should (equal (opencode-shell--composer-text) "작성 중인 초안"))
            (should-not opencode-shell--permissions)
            (should-not opencode-shell--questions-pending)
            (let ((visible-text (buffer-string)))
              (set-window-buffer window original-buffer)
              (opencode-shell--receive-permissions
               opencode-shell-test--pending-permission-snapshot t)
              (should opencode-shell--render-dirty)
              (should (equal visible-text (buffer-string)))
              (set-window-buffer window buffer)
              (opencode-shell--render-if-visible)
              (opencode-shell-async-drain buffer)
              (should (equal opencode-shell--permissions
                             opencode-shell-test--pending-permission-snapshot))
              (should (string-match-p "bash" (buffer-string)))
              (should-not opencode-shell--render-dirty))))
      (set-window-buffer window original-buffer)
      (set-window-start window original-start)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest opencode-shell-acceptance-shared-runtime-coalesces-and-falls-back ()
  (let ((opencode-shell-async--runtimes (make-hash-table :test #'equal))
        (first (generate-new-buffer " *accept-runtime-1*"))
        (second (generate-new-buffer " *accept-runtime-2*"))
         (runtime (list :subscribers (make-hash-table :test #'eq)
                        :attempt 'accept-attempt :ticks 0
                        :poll-interval 2))
        (first-wakes 0) (second-wakes 0))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) 'idle-timer))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) 'idle-timer))
                  ((symbol-function 'timerp)
                   (lambda (value) (eq value 'idle-timer)))
                  ((symbol-function 'cancel-timer) #'ignore))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq-local opencode-shell--generation 1)))
          (puthash first (lambda () (cl-incf first-wakes))
                   (plist-get runtime :subscribers))
          (puthash second (lambda () (cl-incf second-wakes))
                   (plist-get runtime :subscribers))
          (puthash 'accept runtime opencode-shell-async--runtimes)
           (dolist (_frame opencode-shell-test--sse-wake-burst)
             (opencode-shell-async--transport-event
              'accept 'accept-attempt '(:data "wake")))
          (opencode-shell-async-drain first)
          (opencode-shell-async-drain second)
          (should (= first-wakes 1))
          (should (= second-wakes 1))
           (opencode-shell-async--poll-runtime 'accept)
          (opencode-shell-async-drain first)
          (opencode-shell-async-drain second)
          (should (= first-wakes 2))
          (should (= second-wakes 2)))
      (kill-buffer first)
      (kill-buffer second))))

(provide 'opencode-shell-acceptance-test)
;;; opencode-shell-acceptance-test.el ends here
