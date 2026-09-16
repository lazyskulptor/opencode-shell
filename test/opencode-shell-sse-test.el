;;; opencode-shell-sse-test.el --- SSE protocol tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'opencode-shell-sse)

(defun opencode-shell-sse-test--feed (parts &rest limits)
  "Feed PARTS through a parser configured with LIMITS."
  (let ((parser (apply #'opencode-shell-sse-parser-create limits)) events error)
    (dolist (part parts)
      (let ((result (opencode-shell-sse-parser-feed parser part)))
        (setq parser (plist-get result :parser)
              events (nconc events (plist-get result :events))
              error (or error (plist-get result :error)))))
    (list :parser parser :events events :error error)))

(defun opencode-shell-sse-test--wire ()
  "Return a representative chunked HTTP event stream."
  (let* ((first "event: update\ndata: one\ndata: two\n\n")
         (second ": heartbeat\ndata: three\n\n"))
    (concat "HTTP/1.1 200 OK\r\n"
            "Content-Type: text/event-stream; charset=utf-8\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            (format "%x;source=test\r\n%s\r\n" (length first) first)
            (format "%x\r\n%s\r\n" (length second) second)
            "0\r\nX-End: yes\r\n\r\n")))

(defun opencode-shell-sse-test--segments (wire seed)
  "Split WIRE deterministically into multiple chunks using SEED."
  (let ((offset 0) parts)
    (while (< offset (length wire))
      (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
      (let ((end (min (length wire) (+ offset 1 (% seed 17)))))
        (push (substring wire offset end) parts)
        (setq offset end)))
    (nreverse parts)))

(ert-deftest opencode-shell-sse-parser-is-independent-of-split-boundary ()
  (let* ((wire (opencode-shell-sse-test--wire))
         (whole (opencode-shell-sse-test--feed (list wire)))
         (expected (plist-get whole :events)))
    (should-not (plist-get whole :error))
    (should (eq (opencode-shell-sse-parser-phase (plist-get whole :parser)) 'done))
    (should (equal (mapcar (lambda (event) (plist-get event :data)) expected)
                   '("one\ntwo" "three")))
    (dotimes (boundary (1- (length wire)))
      (let ((result (opencode-shell-sse-test--feed
                     (list (substring wire 0 (1+ boundary))
                           (substring wire (1+ boundary))))))
        (should-not (plist-get result :error))
        (should (equal (plist-get result :events) expected))
        (should (eq (opencode-shell-sse-parser-phase
                     (plist-get result :parser))
                    'done))))))

(ert-deftest opencode-shell-sse-parser-does-not-mutate-prior-state ()
  (let* ((parser (opencode-shell-sse-parser-create))
         (result (opencode-shell-sse-parser-feed
                  parser "HTTP/1.1 200 OK\r\n")))
    (should (string-empty-p (opencode-shell-sse-parser-input parser)))
    (should (equal (opencode-shell-sse-parser-input (plist-get result :parser))
                   "HTTP/1.1 200 OK\r\n"))))

(ert-deftest opencode-shell-sse-parser-fixed-segmentations-match-whole-input ()
  (let* ((wire (opencode-shell-sse-test--wire))
         (expected (opencode-shell-sse-test--feed (list wire))))
    (dolist (seed '(1 7 42 8675309))
      (let ((actual (opencode-shell-sse-test--feed
                     (opencode-shell-sse-test--segments wire seed))))
        (should (equal (plist-get actual :events) (plist-get expected :events)))
        (should-not (plist-get actual :error))
        (should (eq (opencode-shell-sse-parser-phase (plist-get actual :parser))
                    'done))))))

(ert-deftest opencode-shell-sse-parser-finish-reports-incomplete-input ()
  (let* ((parser (opencode-shell-sse-parser-create))
         (partial (plist-get
                   (opencode-shell-sse-parser-feed
                    parser "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n")
                   :parser))
         (result (opencode-shell-sse-parser-finish partial)))
    (should (eq (plist-get (plist-get result :error) :reason)
                'incomplete-headers))
    (should (eq (opencode-shell-sse-parser-phase partial) 'headers))))

(ert-deftest opencode-shell-sse-parser-ignores-prefix-lookalike-fields ()
  (let ((result
         (opencode-shell-sse-test--feed
          '("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\ndatabase: no\ndata: yes\n\n"))))
    (should (equal (mapcar (lambda (event) (plist-get event :data))
                           (plist-get result :events))
                   '("yes")))))

(ert-deftest opencode-shell-sse-parser-bounds-header-not-coalesced-body ()
  (let* ((data (make-string 256 ?x))
         (wire (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
                       "data: " data "\n\n"))
         (result (opencode-shell-sse-test--feed
                  (list wire) :max-header-bytes 64 :max-frame-bytes 512)))
    (should-not (plist-get result :error))
    (should (equal (plist-get (car (plist-get result :events)) :data) data))))

(ert-deftest opencode-shell-sse-parser-rejects-invalid-limits ()
  (dolist (arguments '((:max-header-bytes 0)
                       (:max-chunk-bytes -1)
                       (:max-frame-bytes 0.5)))
    (should-error (apply #'opencode-shell-sse-parser-create arguments))))

(ert-deftest opencode-shell-sse-parser-validates-real-header-fields ()
  (let ((result
         (opencode-shell-sse-test--feed
          '("HTTP/1.1 200 OK\r\nX-Reason: content-type: text/event-stream\r\nContent-Type: application/json\r\n\r\n{}"))))
    (should (eq (plist-get (plist-get result :error) :reason) 'content-type))))

(ert-deftest opencode-shell-sse-parser-rejects-malformed-and-oversized-chunks ()
  (dolist (body '("ZZ\r\n" "5\r\nabcdeXX" "1;bad@=x\r\na\r\n"))
    (let ((result
           (opencode-shell-sse-test--feed
            (list (concat "HTTP/1.1 200 OK\r\n"
                          "Content-Type: text/event-stream\r\n"
                          "Transfer-Encoding: chunked\r\n\r\n" body)))))
      (should (plist-get result :error))))
  (let ((result
         (opencode-shell-sse-test--feed
          '("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n11\r\n")
          :max-chunk-bytes 16)))
    (should (eq (plist-get (plist-get result :error) :reason) 'chunk-too-large))))

(ert-deftest opencode-shell-sse-transport-handles-filter-before-return ()
  (let (events errors sent cancelled)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'header-timer))
              ((symbol-function 'timerp) (lambda (timer) (eq timer 'header-timer)))
              ((symbol-function 'cancel-timer) (lambda (timer) (push timer cancelled)))
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (funcall
                  (plist-get arguments :filter) 'stream
                  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\ndata: ready\n\n")
                 'stream))
              ((symbol-function 'process-live-p) (lambda (process) (eq process 'stream)))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'process-send-string)
               (lambda (_process request) (setq sent request))))
      (let ((connection
             (opencode-shell-sse-start
              "http://[::1]:4199/event" nil
              (lambda (event) (push event events))
              (lambda (error) (push error errors)))))
        (should (opencode-shell-sse-connected-p connection))
        (should (equal (plist-get (car events) :data) "ready"))
        (should-not errors)
        (should (string-match-p "Host: \\[::1\\]:4199" sent))
        (should (equal cancelled '(header-timer)))))))

(ert-deftest opencode-shell-sse-transport-handles-sentinel-before-return-once ()
  (let (errors cancelled)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'header-timer))
              ((symbol-function 'timerp) (lambda (timer) (eq timer 'header-timer)))
              ((symbol-function 'cancel-timer) (lambda (timer) (push timer cancelled)))
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (funcall (plist-get arguments :sentinel) 'dead "failed")
                 'dead))
              ((symbol-function 'process-status) (lambda (_) 'failed))
              ((symbol-function 'process-live-p) (lambda (_) nil))
              ((symbol-function 'delete-process) #'ignore))
      (let ((connection
             (opencode-shell-sse-start
              "http://localhost:4199/event" nil #'ignore
              (lambda (error) (push error errors)))))
        (should (eq (opencode-shell-sse-connection-state connection) 'disconnected))
        (should (= (length errors) 1))
        (should (= (length cancelled) 1))))))

(ert-deftest opencode-shell-sse-transport-ignores-nonterminal-sentinel ()
  (let (sentinel errors)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'header-timer))
              ((symbol-function 'timerp) (lambda (timer) (eq timer 'header-timer)))
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (setq sentinel (plist-get arguments :sentinel))
                 'stream))
              ((symbol-function 'process-status) (lambda (_) 'open))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'process-send-string) #'ignore))
      (let ((connection
             (opencode-shell-sse-start
              "http://localhost:4199/event" nil #'ignore
              (lambda (error) (push error errors)))))
        (funcall sentinel 'stream "open")
        (should (eq (opencode-shell-sse-connection-state connection) 'connecting))
        (should-not errors)))))

(ert-deftest opencode-shell-sse-transport-classifies-partial-eof-as-protocol-error ()
  (let (filter sentinel errors)
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'header-timer))
              ((symbol-function 'timerp) (lambda (timer) (eq timer 'header-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (setq filter (plist-get arguments :filter)
                       sentinel (plist-get arguments :sentinel))
                 'stream))
              ((symbol-function 'process-status) (lambda (_) 'closed))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process) #'ignore)
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'process-send-string) #'ignore))
      (let ((connection
             (opencode-shell-sse-start
              "http://localhost:4199/event" nil #'ignore
              (lambda (error) (push error errors)))))
        (funcall filter 'stream "HTTP/1.1 200 OK\r\nContent-Type: text/")
        (funcall sentinel 'stream "closed")
        (should (eq (plist-get (car errors) :type) 'protocol))
        (should (eq (plist-get (car errors) :reason) 'incomplete-headers))
        (should (eq (opencode-shell-sse-connection-state connection)
                    'disconnected))))))

(ert-deftest opencode-shell-sse-transport-stops-delivery-after-callback-close ()
  (let (connection delivered)
    (setq connection
          (opencode-shell-sse--make-connection
           :token 'token :state 'streaming :process 'stream
           :parser (let* ((parser (opencode-shell-sse-parser-create))
                          (result (opencode-shell-sse-parser-feed
                                   parser
                                   "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n")))
                     (plist-get result :parser))
           :on-event (lambda (event)
                       (push (plist-get event :data) delivered)
                       (opencode-shell-sse-stop connection))
           :on-error #'ignore))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil)))
      (opencode-shell-sse--filter
       connection 'token 'stream "data: first\n\ndata: second\n\n"))
    (should (equal delivered '("first")))
    (should (eq (opencode-shell-sse-connection-state connection) 'closed))))

(ert-deftest opencode-shell-sse-transport-stop-and-stale-filter-are-idempotent ()
  (let (filter errors (deleted 0))
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'header-timer))
              ((symbol-function 'timerp) (lambda (timer) (eq timer 'header-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (setq filter (plist-get arguments :filter))
                 'stream))
              ((symbol-function 'process-live-p) (lambda (process) (eq process 'stream)))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore)
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'delete-process) (lambda (_) (cl-incf deleted))))
      (let* ((connection
              (opencode-shell-sse-start
               "http://localhost:4199/event" nil #'ignore
               (lambda (error) (push error errors))))
             (old-token (opencode-shell-sse-connection-token connection)))
        (opencode-shell-sse-stop connection)
        (opencode-shell-sse-stop connection)
        (funcall filter 'stream "data: stale\n\n")
        (opencode-shell-sse--filter connection old-token 'stream "data: stale\n\n")
        (should (eq (opencode-shell-sse-connection-state connection) 'closed))
        (should (= deleted 1))
        (should-not errors)))))

(ert-deftest opencode-shell-sse-transport-rejects-unsafe-config-without-process ()
  (let (errors made)
    (cl-letf (((symbol-function 'make-network-process) (lambda (&rest _) (setq made t))))
      (let ((connection
             (opencode-shell-sse-start
              "https://localhost/event" nil #'ignore
              (lambda (error) (push error errors)))))
        (should (eq (opencode-shell-sse-connection-state connection) 'closed))
        (should (eq (plist-get (car errors) :type) 'config))
        (should-not made)))))

(provide 'opencode-shell-sse-test)
;;; opencode-shell-sse-test.el ends here
