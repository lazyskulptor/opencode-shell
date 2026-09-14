;;; opencode-shell-acceptance-test.el --- Workflow acceptance tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'opencode-shell)

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

(provide 'opencode-shell-acceptance-test)
;;; opencode-shell-acceptance-test.el ends here
