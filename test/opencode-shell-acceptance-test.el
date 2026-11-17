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

(ert-deftest opencode-shell-acceptance-caps-rendered-turns-to-recent-window ()
  "Only the most recent `opencode-shell-max-rendered-turns' are drawn; state keeps
all turns.  Capping keeps render/reconcile O(cap) so large sessions stay smooth."
  (with-temp-buffer
    (opencode-shell-mode)
    (let ((opencode-shell-max-rendered-turns 3))
      (opencode-shell--render-messages (opencode-shell-acceptance--messages 10))
      (should (= (length opencode-shell--turns) 10))
      (should (= (length opencode-shell--rendered-turns) 3))
      (let ((text (buffer-string)))
        (should (string-match-p "질문 10" text))
        (should (string-match-p "질문 8" text))
        (should (string-match-p "응답 9" text))
        (should-not (string-match-p "질문 7" text))
        (should-not (string-match-p "질문 5" text))))))

(ert-deftest opencode-shell-acceptance-response-cache-skips-unchanged-turns ()
  "A full rebuild reuses cached response projections for unchanged turns.
This keeps every resync/rebuild O(changed turns) instead of re-projecting the
whole transcript, which froze Emacs on each prompt in large sessions."
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages (opencode-shell-acceptance--messages 3))
    (let ((calls 0)
          (orig (symbol-function 'opencode-shell--response-display)))
      (cl-letf (((symbol-function 'opencode-shell--response-display)
                 (lambda (&rest a) (cl-incf calls) (apply orig a))))
        (opencode-shell--render-turns t)
        (should (= calls 0))
        ;; Changing a turn invalidates only that turn's cache entry.
        (setf (opencode-shell--turn-status (car opencode-shell--turns)) 'receiving)
        (opencode-shell--render-turns t)
        (should (= calls 1))))))

(ert-deftest opencode-shell-acceptance-appended-prompt-keeps-blank-line ()
  "A prompt appended on submit is separated from the prior response by a blank line.
Regression: the append path inserted at the shared composer-label newline, leaving
only one newline so the new prompt glued to the previous response."
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "sep")
    (cl-letf (((symbol-function 'opencode-shell--request)
               (lambda (_m _p _cb &optional _b _pa _e) nil)))
      (opencode-shell--render-messages (opencode-shell-acceptance--messages 1))
      (goto-char opencode-shell--composer-start)
      (insert "질문 2")
      (opencode-shell--submit)
      (should (string-match-p "응답 1\n\n질문 2" (buffer-string)))
      (should-not (string-match-p "응답 1\n질문 2" (buffer-string))))))

(ert-deftest opencode-shell-acceptance-ten-turn-conversation ()
  (with-temp-buffer
    (opencode-shell-mode)
    (setq opencode-shell--session-id "acceptance")
    (let (success)
      (cl-letf (((symbol-function 'opencode-shell--request)
                 (lambda (_method _path callback &optional _body _params _error)
                   (setq success callback))))
        (dotimes (index 10)
          (goto-char (1- (point-max)))
          (insert (format "질문 %d" (1+ index)))
          (opencode-shell--submit)
          (funcall success nil)
          (opencode-shell--render-messages
           (opencode-shell-acceptance--messages (1+ index)))
          (should (= (length opencode-shell--turns) (1+ index)))
          (should (string-empty-p (opencode-shell--composer-text)))
          (should (= (point) opencode-shell--composer-start))
          (should (eq (get-text-property (point-min) 'read-only) t)))))))

(ert-deftest opencode-shell-acceptance-protected-point-stays-put-across-render ()
  (with-temp-buffer
    (opencode-shell-mode)
    (opencode-shell--render-messages (opencode-shell-acceptance--messages 1))
    (goto-char (point-min))
    (let ((position (point)))
      (opencode-shell--render-turns)
      (should (= (point) position))
      (should (< (point) opencode-shell--composer-start))
      (should-error (insert "blocked") :type 'text-read-only))))

(ert-deftest opencode-shell-acceptance-empty-composer-point-survives-render ()
  (with-temp-buffer
    (opencode-shell-mode)
    (goto-char opencode-shell--composer-start)
    (should (= (point) (1- (point-max))))
    (opencode-shell--render-turns)
    (should (= (point) opencode-shell--composer-start))
    (should (= (point) (1- (point-max))))
    (should (get-text-property (point) 'opencode-shell-composer-sentinel))
    (should (= (line-number-at-pos (point))
               (line-number-at-pos (point-max))))
    (let ((filler (overlay-get opencode-shell--composer-overlay
                               'after-string)))
      (should (equal (get-text-property 0 'display filler)
                     '(space :align-to right-fringe)))
      (should (eq (get-text-property 0 'face filler)
                  'opencode-shell-composer-face)))
     (should (local-variable-p 'evil-move-beyond-eol))))

(ert-deftest opencode-shell-acceptance-empty-composer-backspace-is-a-no-op ()
  (declare-function evil-delete-backward-char-and-join "evil-commands")
  (with-temp-buffer
    (opencode-shell-mode)
    (buffer-enable-undo)
    (let ((before (buffer-string))
          (point-before (point))
          (start-before (marker-position opencode-shell--composer-start))
          (undo-before buffer-undo-list)
          deleted)
      (cl-letf (((symbol-function 'evil-delete-backward-char-and-join)
                 (lambda () (setq deleted t))))
        (dotimes (_ 3) (opencode-shell--evil-backspace)))
      (should-not deleted)
      (should (equal (buffer-string) before))
      (should (= (point) point-before))
      (should (= (marker-position opencode-shell--composer-start) start-before))
      (should (eq buffer-undo-list undo-before))
      (should (get-text-property (1- (point-max))
                                 'opencode-shell-composer-sentinel)))))

(ert-deftest opencode-shell-acceptance-queued-response-preserves-active-draft ()
  (with-temp-buffer
    (opencode-shell-mode)
    (let (visible)
      (goto-char (1- (point-max)))
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
      (goto-char opencode-shell--composer-start)
      (insert "작성 중")
      (goto-char (+ opencode-shell--composer-start 2))
      (opencode-shell--render-messages messages 1)
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
            (goto-char opencode-shell--composer-start)
            (insert "작성 중인 초안")
            (goto-char (+ opencode-shell--composer-start 4))
            (opencode-shell--render-messages
             opencode-shell-test--active-transcript-snapshot 1)
            (opencode-shell--receive-permissions
             opencode-shell-test--pending-permission-snapshot)
            (opencode-shell--receive-questions
             opencode-shell-test--pending-question-snapshot)
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

(ert-deftest opencode-shell-acceptance-long-session-delta-keeps-spinner-live ()
  (let* ((buffer (generate-new-buffer " *accept-long-delta*"))
         (window (selected-window))
         (original-buffer (window-buffer window))
         (original-update (symbol-function 'opencode-shell--update-turn-response))
         (updates 0))
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (setq opencode-shell--session-id "large-session")
            (opencode-shell--render-messages
             (opencode-shell-test--large-message-snapshot 40) 1)
            (let ((first-frame
                   (overlay-get (car opencode-shell--spinner-overlays) 'display)))
              (cl-letf (((symbol-function 'opencode-shell--update-turn-response)
                         (lambda (turn)
                           (cl-incf updates)
                           (funcall original-update turn))))
                (opencode-shell--receive-application-event
                 '(:kind part-updated :type "message.part.updated"
                   :session-id "large-session"
                   :message-id "large-active-assistant" :part-id "large-tool"
                   :part ((id . "large-tool") (sessionID . "large-session")
                          (messageID . "large-active-assistant") (type . "tool")
                          (tool . "bash") (state . ((status . "running"))))))
                (opencode-shell--animation-tick)
                (opencode-shell-async-drain buffer))
              (should (= updates 1))
              (should (string-match-p "TOOL> bash" (buffer-string)))
              (should opencode-shell--spinner-overlays)
              (should-not
               (equal first-frame
                      (overlay-get (car opencode-shell--spinner-overlays) 'display)))
              (opencode-shell--render-messages
               '(((info . ((id . "large-active-assistant")
                           (sessionID . "large-session")
                           (role . "assistant")
                           (parentID . "large-active-user")
                           (finish . "stop") (time . ((completed . 2)))))
                  (parts . (((id . "large-tool") (sessionID . "large-session")
                             (messageID . "large-active-assistant")
                             (type . "tool") (tool . "bash")
                             (state . ((status . "completed"))))))))
               2 t)
              (opencode-shell-async-drain buffer)
              (should (eq (opencode-shell--turn-status
                           (car (last opencode-shell--turns)))
                          'complete)))))
      (set-window-buffer window original-buffer)
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

(ert-deftest opencode-shell-acceptance-streamed-response-preserves-draft-and-spinner ()
  (let* ((buffer (generate-new-buffer " *accept-streamed-response*"))
         (window (selected-window))
         (previous (window-buffer window)))
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (setq opencode-shell--session-id "s")
            (goto-char opencode-shell--composer-start)
            (insert "draft")
            (goto-char (+ opencode-shell--composer-start 2))
            (opencode-shell--render-messages
             '(((info . ((id . "u1") (role . "user")))
                (parts . (((type . "text") (text . "question")))))
               ((info . ((id . "a1") (role . "assistant") (parentID . "u1")))
                (parts . (((id . "p1") (type . "text") (text . "partial")))))))
            (should (string-match-p "partial\n\nReceiving" (buffer-string)))
            (should opencode-shell--spinner-overlays)
            (should-not (opencode-shell--composer-visible-p))
            (should (equal (opencode-shell--composer-text) "draft"))
            (should (= 2 (- (point) opencode-shell--composer-start)))
            (opencode-shell--receive-permissions
             '(((id . "p") (sessionID . "s") (permission . "read"))))
            (should (string-match-p "partial\n\nReceiving" (buffer-string)))
            (should (= 1 (how-many "Receiving" (point-min) (point-max))))
            (should (equal (opencode-shell--composer-text) "draft"))))
      (set-window-buffer window previous)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest opencode-shell-acceptance-intermediate-burst-is-coalesced ()
  (let* ((buffer (generate-new-buffer " *accept-idle-burst*"))
         (window (selected-window))
         (previous (window-buffer window))
         (user '((info . ((id . "u") (role . "user")))
                 (parts . (((type . "text") (text . "question"))))))
         (real-timerp (symbol-function 'timerp))
         timers)
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (opencode-shell--render-messages
             (list user '((info . ((id . "a") (role . "assistant")
                                   (parentID . "u")))
                          (parts . (((id . "p") (type . "text")
                                     (text . "first")))))) 1)
            (cl-letf (((symbol-function 'run-with-idle-timer)
                       (lambda (delay _repeat function &rest args)
                         (let ((timer (list 'idle delay function args)))
                           (push timer timers) timer)))
                      ((symbol-function 'timerp)
                       (lambda (timer)
                         (or (and (consp timer) (eq (car timer) 'idle))
                             (funcall real-timerp timer))))
                      ((symbol-function 'cancel-timer) #'ignore))
              (dolist (value '("second" "latest"))
                (opencode-shell--render-messages
                 (list user `((info . ((id . "a") (role . "assistant")
                                         (parentID . "u")))
                              (parts . (((id . "p") (type . "text")
                                         (text . ,value)))))) nil t))
              (should (equal (mapcar #'cadr timers) '(0.25)))
              (should-not (string-match-p "latest" (buffer-string)))
              (opencode-shell--animation-tick)
              (should opencode-shell--spinner-overlays)
              (apply (nth 2 (car timers)) (nth 3 (car timers)))
              (should (string-match-p "latest\n\nReceiving" (buffer-string)))
              (should-not (opencode-shell--composer-visible-p))
              (opencode-shell--render-messages
               (list user '((info . ((id . "a") (role . "assistant")
                                     (parentID . "u") (finish . "stop")
                                     (time . ((completed . 2)))))
                            (parts . (((id . "p") (type . "text")
                                       (text . "done")))))) nil t)
              (opencode-shell-async-drain buffer)
              (should (opencode-shell--composer-visible-p))
              (should-not opencode-shell--spinner-overlays))))
      (set-window-buffer window previous)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun opencode-shell-acceptance--scroll-long-snapshot (count &optional active-last)
  "Chronological snapshot of COUNT turns; leave the last assistant active if
ACTIVE-LAST, so the renderer is in a Receiving state."
  (apply #'append
         (cl-loop for n from 1 to count
                  collect
                  `(((info . ((id . ,(format "u%d" n)) (role . "user")))
                     (parts . (((id . ,(format "pu%d" n)) (type . "text")
                                (text . ,(format "질문 %d\n%s" n
                                                 (mapconcat (lambda (k) (format "qfiller-%d-%d" n k))
                                                            (number-sequence 1 6) "\n")))))))
                    ((info . ((id . ,(format "a%d" n)) (role . "assistant")
                              (parentID . ,(format "u%d" n))
                              ,@(unless (and active-last (= n count))
                                  `((finish . "stop") (time . ((completed . ,n)))))))
                     (parts . (((id . ,(format "pa%d" n)) (type . "text")
                                (text . ,(format "응답 %d\n%s" n
                                                 (mapconcat (lambda (k) (format "line-%d-%d" n k))
                                                            (number-sequence 1 10) "\n")))))))))))

(ert-deftest opencode-shell-acceptance-receiving-scroll-issues-no-viewport-command ()
  "Receiving updates, force rebuild, table relayout, and completion of a long
transcript issue no explicit viewport command, keep the Composer draft/point,
and leave the view untouched across an unchanged update."
  (let* ((buffer (generate-new-buffer " *accept-scroll*"))
         (window (selected-window))
         (previous (window-buffer window))
         (moves 0)
         ws-before ws-after)
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (setq opencode-shell--session-id "accept-scroll")
            (goto-char opencode-shell--composer-start)
            (insert "초안")
            (let ((offset (- (point) opencode-shell--composer-start)))
              (cl-letf (((symbol-function 'set-window-start)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'set-window-point)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'recenter)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'recenter-top-bottom)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'scroll-up)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'scroll-down)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'scroll-other-window)
                         (lambda (&rest _) (cl-incf moves))))
                (opencode-shell--render-messages
                 (opencode-shell-acceptance--scroll-long-snapshot 8 t) 1)
                (opencode-shell--render-messages
                 (opencode-shell-acceptance--scroll-long-snapshot 8 t) 2 t)
                (opencode-shell--render-turns t)
                (setq opencode-shell--table-render-width nil
                      opencode-shell--render-dirty t
                      opencode-shell--render-event "accept-scroll"
                      opencode-shell--render-force nil
                      opencode-shell--render-dirty-turns nil)
                (opencode-shell--refresh-table-layout)
                (opencode-shell--render-messages
                 (opencode-shell-acceptance--scroll-long-snapshot 8) 3 t)
                (opencode-shell-async-drain buffer))
              (should (zerop moves))
              (should (equal (opencode-shell--composer-text) "초안"))
              (should (= (- (point) opencode-shell--composer-start) offset))
              ;; An unchanged update produces no text change, so the selected
              ;; window's start must stay exactly where the user left it.
              (goto-char (point-min))
              (forward-line 12)
              (set-window-start window (point) t)
              (setq ws-before (window-start window))
              (cl-letf (((symbol-function 'set-window-start)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'set-window-point)
                         (lambda (&rest _) (cl-incf moves)))
                        ((symbol-function 'recenter)
                         (lambda (&rest _) (cl-incf moves))))
                (opencode-shell--render-messages
                 (opencode-shell-acceptance--scroll-long-snapshot 8) 4 t)
                (opencode-shell-async-drain buffer))
              (setq ws-after (window-start window))
              (should (zerop moves))
              (should (= ws-before ws-after)))))
      (set-window-buffer window previous)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest opencode-shell-acceptance-receiving-scroll-two-windows-stay-put ()
  "Two windows with distinct read positions on one transcript receive no
viewport command on an unchanged update, keep their own positions, keep their
buffers, and manual navigation still moves point."
  (let* ((buffer (generate-new-buffer " *accept-scroll-2w*"))
         (window (selected-window))
         (previous (window-buffer window))
         (other (split-window window))
         (moves 0)
         ws-w ws-o)
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (set-window-buffer other buffer)
          (with-current-buffer buffer
            (opencode-shell-mode)
            (setq opencode-shell--session-id "accept-scroll-2w")
            (opencode-shell--render-messages
             (opencode-shell-acceptance--scroll-long-snapshot 8) 1)
            (goto-char (point-min))
            (forward-line 4)
            (set-window-start window (point) t)
            (goto-char (point-min))
            (forward-line 30)
            (set-window-start other (point) t)
            (setq ws-w (window-start window)
                  ws-o (window-start other))
            (cl-letf (((symbol-function 'set-window-start)
                       (lambda (&rest _) (cl-incf moves)))
                      ((symbol-function 'set-window-point)
                       (lambda (&rest _) (cl-incf moves)))
                      ((symbol-function 'recenter)
                       (lambda (&rest _) (cl-incf moves))))
              (opencode-shell--render-messages
               (opencode-shell-acceptance--scroll-long-snapshot 8) 2 t)
              (opencode-shell-async-drain buffer))
            (should (zerop moves))
            (should (eq (window-buffer window) buffer))
            (should (eq (window-buffer other) buffer))
            (should (= ws-w (window-start window)))
            (should (= ws-o (window-start other)))
            ;; Manual turn navigation is the user's to drive, not removed.
            (goto-char (point-max))
            (opencode-shell--previous-turn)
            (should (> (point) (point-min)))))
      (set-window-buffer window previous)
      (when (window-live-p other) (delete-window other))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun opencode-shell-acceptance--reading-anchor (window)
  "Capture actual native positions and a text token read in WINDOW."
  (with-current-buffer (window-buffer window)
    (let ((position (window-point window)))
      (list window (copy-marker (window-start window)) (copy-marker position)
            (buffer-substring-no-properties position (min (point-max) (+ position 10)))))))

(defun opencode-shell-acceptance--check-reading-anchors (anchors selected)
  "Check native positions and retained text, not merely absence of API calls."
  (unless noninteractive (redisplay t))
  (should (eq selected (selected-window)))
  (dolist (anchor anchors)
    (pcase-let ((`(,window ,start ,position ,text) anchor))
      (should (window-live-p window))
      (should (= start (window-start window)))
      (should (= position (window-point window)))
      (with-current-buffer (window-buffer window)
        (should (equal text (buffer-substring-no-properties
                             position (min (point-max) (+ position 10)))))))))

(ert-deftest opencode-shell-acceptance-live-recovery-scroll-changed-stream ()
  "SSE, local recovery, integrity resync and completion retain two reading views."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *accept-live-recovery*"))
          (opencode-shell-async--runtimes (make-hash-table :test #'equal))
          (opencode-shell-recovery--states (make-hash-table :test #'equal))
          (window (selected-window)) anchors)
      (unwind-protect
          (let ((other (split-window window nil 'right)))
            (set-window-buffer window buffer)
            (set-window-buffer other buffer)
            (with-current-buffer buffer
              (opencode-shell-mode)
              (let* ((profile (list :name "fixture" :base-url opencode-shell-base-url))
                     (key (opencode-shell--server-key profile))
                     (subscribers (make-hash-table :test #'eq))
                     (runtime (list :subscribers subscribers :paused nil :poll-interval 2))
                     (snapshot (opencode-shell-acceptance--scroll-long-snapshot 4 t))
                     (now 100.0) (snapshots 0) retry health)
                (setq opencode-shell--session-id "fixture"
                      opencode-shell--profile profile
                      opencode-shell--runtime-key key)
                (puthash buffer (list :callback #'ignore) subscribers)
                (puthash key runtime opencode-shell-async--runtimes)
                (goto-char opencode-shell--composer-start)
                (insert "draft")
                (opencode-shell--render-messages (copy-tree snapshot) 1)
                ;; Initial hydration normally starts through resync's sequence allocator.
                (setq opencode-shell--message-request-sequence opencode-shell--message-applied-sequence)
                (goto-char (opencode-shell--turn-response-begin (car opencode-shell--turns)))
                (forward-line 2)
                (set-window-start window (point) t)
                (forward-line 2)
                (let ((first-point (point)))
                  (goto-char (opencode-shell--turn-response-begin (car (last opencode-shell--turns))))
                  (forward-line 2)
                  (set-window-start other (point) t)
                  (forward-line 2)
                  (set-window-point other (point))
                  (goto-char first-point))
                (setq anchors (mapcar #'opencode-shell-acceptance--reading-anchor (list window other)))
                (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
                          ((symbol-function 'run-with-idle-timer) (lambda (&rest _) nil))
                          ((symbol-function 'run-at-time)
                           (lambda (_delay _repeat callback &rest args)
                             (setq retry (lambda () (apply callback args))) nil))
                          ((symbol-function 'opencode-shell--server-ready)
                           (lambda (_profile callback &rest _) (setq health callback) nil))
                          ((symbol-function 'opencode-shell--request)
                           (lambda (_method path callback &rest _)
                             (cond
                              ((string-suffix-p "/message" path)
                               (cl-incf snapshots) (funcall callback (copy-tree snapshot)))
                              ((equal path "/session/fixture")
                               (funcall callback '((time . ((updated . 2))))))
                              (t (funcall callback nil))))))
                  (opencode-shell--receive-application-event
                   '(:kind part-delta :type "message.part.delta" :session-id "fixture"
                     :message-id "a4" :part-id "pa4" :field "text" :delta "\nstream-tail"))
                  (should opencode-shell-async--queue)
                  (opencode-shell-async-drain buffer)
                  (should (string-match-p "stream-tail" (buffer-string)))
                  (opencode-shell-acceptance--check-reading-anchors anchors window)
                  ;; The server subsequently has content that was never sent as SSE.
                  (let ((part (car (alist-get 'parts (car (last snapshot))))))
                    (setf (alist-get 'text part) (concat (alist-get 'text part) "\nstream-tail\noffline-tail")))
                  (let ((server-current
                         (string-match-p "offline-tail"
                          (alist-get 'text (car (alist-get 'parts (car (last snapshot))))))))
                    (should server-current))
                  (opencode-shell--transport-failed profile)
                  (should (plist-get runtime :paused))
                  (funcall retry)
                  (funcall health t profile)
                  (should opencode-shell-async--queue)
                  (dotimes (_ 3) (opencode-shell-async-drain buffer))
                  (should (= snapshots 1))
                  (should-not (plist-get runtime :paused))
                  (let ((model-current
                         (string-match-p "offline-tail" (opencode-shell--turn-assistant
                                                         (car (last opencode-shell--turns))))))
                    (should model-current))
                  (should (string-match-p "offline-tail" (buffer-string)))
                  (setq opencode-shell--table-render-width 0
                        opencode-shell--render-dirty t opencode-shell--render-force t)
                  (opencode-shell--refresh-table-layout)
                  (opencode-shell-acceptance--check-reading-anchors anchors window)
                  ;; Metadata-only events must not postpone this newer body forever.
                  (let ((part (car (alist-get 'parts (car (last snapshot))))))
                    (setf (alist-get 'text part) (concat (alist-get 'text part) "\nintegrity-tail")))
                  (dotimes (_ 4)
                    (setq now (+ now 10))
                    (opencode-shell--receive-application-event
                     '(:kind session-updated :type "session.updated" :session-id "fixture"))
                    (opencode-shell--reconcile-quiet)
                    (opencode-shell-async-drain buffer))
                  (should (= snapshots 2))
                  (should (string-match-p "integrity-tail" (buffer-string)))
                  (opencode-shell--render-turns t)
                  (opencode-shell-acceptance--check-reading-anchors anchors window)
                  (opencode-shell--receive-application-event
                   '(:kind message-updated :type "message.updated" :session-id "fixture"
                     :message-id "a4" :info ((id . "a4") (sessionID . "fixture")
                     (role . "assistant") (parentID . "u4") (finish . "stop")
                     (time . ((completed . 999))))))
                  (dotimes (_ 2) (opencode-shell-async-drain buffer))
                  (should (eq (opencode-shell--turn-status (car (last opencode-shell--turns))) 'complete))
                  (should (opencode-shell--composer-visible-p))
                  (should (equal (opencode-shell--composer-text) "draft"))
                  (opencode-shell-acceptance--check-reading-anchors anchors window)))))
        (dolist (anchor anchors) (set-marker (nth 1 anchor) nil) (set-marker (nth 2 anchor) nil))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest opencode-shell-acceptance-live-recovery-scroll-hidden-update ()
  "A hidden transcript reconciles a changed stream without editing its text."
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *accept-hidden-stream*"))
          (window (selected-window)) (previous (window-buffer)))
      (unwind-protect
          (progn
            (set-window-buffer window buffer)
            (with-current-buffer buffer
              (opencode-shell-mode)
              (setq opencode-shell--runtime-key 'fixture opencode-shell--session-id "fixture")
              (opencode-shell--render-messages
               (opencode-shell-acceptance--scroll-long-snapshot 4 t) 1)
              (set-window-buffer window previous)
              (let ((tick (buffer-chars-modified-tick)))
                (opencode-shell--receive-application-event
                 '(:kind part-delta :type "message.part.delta" :session-id "fixture"
                   :message-id "a4" :part-id "pa4" :field "text" :delta "\nhidden-tail"))
                (opencode-shell-async-drain buffer)
                (should (= tick (buffer-chars-modified-tick)))
                (should opencode-shell--render-dirty)
                (set-window-buffer window buffer)
                (opencode-shell--render-if-visible)
                (opencode-shell-async-drain buffer)
                (should (string-match-p "hidden-tail" (buffer-string))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'opencode-shell-acceptance-test)
;;; opencode-shell-acceptance-test.el ends here
