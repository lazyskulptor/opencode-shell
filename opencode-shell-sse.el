;;; opencode-shell-sse.el --- Incremental SSE protocol handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Pure incremental parsing for an HTTP event stream.  The parser has no
;; process, timer, buffer, or runtime dependencies; transport lifecycle support
;; is layered below the parser in this module.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(cl-defstruct (opencode-shell-sse-parser
               (:constructor opencode-shell-sse--make-parser))
  phase input transfer chunk-state chunk-size sse-input sse-started
  max-header-bytes max-chunk-bytes max-frame-bytes)

(cl-defun opencode-shell-sse-parser-create
    (&key (max-header-bytes 65536)
          (max-chunk-bytes (* 4 1024 1024))
          (max-frame-bytes (* 1024 1024)))
  "Return a fresh incremental SSE parser.
MAX-HEADER-BYTES, MAX-CHUNK-BYTES, and MAX-FRAME-BYTES must be
positive integers."
  (dolist (limit (list max-header-bytes max-chunk-bytes max-frame-bytes))
    (unless (and (integerp limit) (> limit 0))
      (error "SSE parser limits must be positive integers")))
  (opencode-shell-sse--make-parser
   :phase 'headers :input "" :transfer 'identity
   :chunk-state 'size :chunk-size nil :sse-input "" :sse-started nil
   :max-header-bytes max-header-bytes
   :max-chunk-bytes max-chunk-bytes
   :max-frame-bytes max-frame-bytes))

(defun opencode-shell-sse--error (parser reason)
  "Move PARSER to an error state identified by REASON."
  (setf (opencode-shell-sse-parser-phase parser) 'error
        (opencode-shell-sse-parser-input parser) ""
        (opencode-shell-sse-parser-sse-input parser) "")
  (list :type 'protocol :reason reason))

(defun opencode-shell-sse--header-values (lines name)
  "Return values from LINES whose field name equals NAME."
  (let (values)
    (dolist (line lines (nreverse values))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
        (when (string-equal (downcase (match-string 1 line)) name)
          (push (match-string 2 line) values))))))

(defun opencode-shell-sse--parse-headers (parser block)
  "Validate HTTP header BLOCK and update PARSER.
Return nil on success or a protocol error plist."
  (let* ((lines (split-string block "\r\n"))
         (status (car lines))
         (fields (cdr lines))
         (content-types (opencode-shell-sse--header-values fields "content-type"))
         (encodings (opencode-shell-sse--header-values fields "transfer-encoding"))
         (content-encodings
          (opencode-shell-sse--header-values fields "content-encoding"))
         (transfer-codings
          (apply #'append
                 (mapcar (lambda (value)
                           (mapcar #'string-trim
                                   (split-string (downcase value) "," t)))
                         encodings))))
    (cond
     ((not (and status
                (string-match-p
                 "\\`HTTP/[0-9]+\\.[0-9]+ 200\\(?: [^\r\n]*\\)?\\'"
                 status)))
      (opencode-shell-sse--error parser 'http-status))
     ((seq-some
       (lambda (line)
         (not (string-match-p
               "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+:[ \t]*[^\r\n]*\\'" line)))
       fields)
      (opencode-shell-sse--error parser 'header-field))
     ((not (and (= (length content-types) 1)
                (string-equal
                 (downcase
                  (string-trim (car (split-string (car content-types) ";"))))
                 "text/event-stream")))
      (opencode-shell-sse--error parser 'content-type))
     ((or (not (or (null transfer-codings)
                   (equal transfer-codings '("chunked"))))
          (seq-some
           (lambda (value)
             (not (string-equal (downcase (string-trim value)) "identity")))
           content-encodings))
      (opencode-shell-sse--error parser 'unsupported-encoding))
     (t
      (setf (opencode-shell-sse-parser-phase parser) 'body
            (opencode-shell-sse-parser-transfer parser)
            (if transfer-codings
                'chunked
              'identity))
      nil))))

(defun opencode-shell-sse--field-value (line)
  "Return the SSE field value from LINE after its first colon."
  (if (string-match ":\\(.*\\)\\'" line)
      (let ((value (match-string 1 line)))
        (if (string-prefix-p " " value) (substring value 1) value))
    ""))

(defun opencode-shell-sse--token-character-p (character)
  "Return non-nil when CHARACTER is valid in an HTTP token."
  (or (and (>= character ?0) (<= character ?9))
      (and (>= character ?A) (<= character ?Z))
      (and (>= character ?a) (<= character ?z))
      (memq character '(?! ?# ?$ ?% ?& ?' ?* ?+ ?- ?. ?^ ?_ ?` ?| ?~))))

(defun opencode-shell-sse--chunk-size (line)
  "Return the hexadecimal chunk size from valid LINE, or nil."
  (when (string-match "\\`[[:xdigit:]]+" line)
    (let ((hex-end (match-end 0))
          (index (match-end 0))
          (length (length line))
          valid)
      (setq valid t)
      (while (and valid (< index length))
        (while (and (< index length) (memq (aref line index) '(?\s ?\t)))
          (setq index (1+ index)))
        (if (or (= index length) (/= (aref line index) ?\;))
            (setq valid nil)
          (setq index (1+ index))
          (while (and (< index length) (memq (aref line index) '(?\s ?\t)))
            (setq index (1+ index)))
          (let ((start index))
            (while (and (< index length)
                        (opencode-shell-sse--token-character-p
                         (aref line index)))
              (setq index (1+ index)))
            (when (= start index)
              (setq valid nil)))
          (while (and (< index length) (memq (aref line index) '(?\s ?\t)))
            (setq index (1+ index)))
          (when (and valid (< index length) (= (aref line index) ?=))
            (setq index (1+ index))
            (while (and (< index length) (memq (aref line index) '(?\s ?\t)))
              (setq index (1+ index)))
            (if (and (< index length) (= (aref line index) ?\"))
                (let ((closed nil))
                  (setq index (1+ index))
                  (while (and valid (< index length) (not closed))
                    (let ((character (aref line index)))
                      (cond
                       ((= character ?\")
                        (setq closed t index (1+ index)))
                       ((= character ?\\)
                        (setq index (1+ index))
                        (if (or (>= index length)
                                (let ((escaped (aref line index)))
                                  (not (or (= escaped ?\t)
                                           (and (>= escaped 32)
                                                (<= escaped 126))))))
                            (setq valid nil)
                          (setq index (1+ index))))
                       ((or (= character ?\t)
                            (= character 32)
                            (= character 33)
                            (and (>= character 35) (<= character 91))
                            (and (>= character 93) (<= character 126)))
                        (setq index (1+ index)))
                       (t (setq valid nil)))))
                  (unless closed (setq valid nil)))
              (let ((start index))
                (while (and (< index length)
                            (opencode-shell-sse--token-character-p
                             (aref line index)))
                  (setq index (1+ index)))
                (when (= start index)
                  (setq valid nil)))))))
      (when valid (substring line 0 hex-end)))))

(defun opencode-shell-sse--frame-event (frame)
  "Convert a complete SSE FRAME into an event plist, or nil."
  (let (data event id saw-data)
    (dolist (line (split-string frame "\r\n\\|\r\\|\n"))
      (cond
       ((or (string-empty-p line) (string-prefix-p ":" line)))
       ((string-match-p "\\`data\\(?:\\'\\|:\\)" line)
        (setq saw-data t)
        (push (opencode-shell-sse--field-value line) data))
       ((string-match-p "\\`event\\(?:\\'\\|:\\)" line)
        (setq event (opencode-shell-sse--field-value line)))
       ((string-match-p "\\`id\\(?:\\'\\|:\\)" line)
        (setq id (opencode-shell-sse--field-value line)))))
    (when saw-data
      (list :data (mapconcat #'identity (nreverse data) "\n")
            :event event :id id))))

(defun opencode-shell-sse--continuation-byte-p (byte)
  "Return non-nil when BYTE is a UTF-8 continuation byte."
  (and (>= byte #x80) (<= byte #xbf)))

(defun opencode-shell-sse--valid-utf8-p (bytes)
  "Return non-nil when unibyte string BYTES is canonical UTF-8."
  (let ((index 0) (length (length bytes)) valid)
    (setq valid t)
    (while (and valid (< index length))
      (let ((first (aref bytes index)))
        (cond
         ((<= first #x7f)
          (setq index (1+ index)))
         ((and (>= first #xc2) (<= first #xdf)
               (< (1+ index) length)
               (opencode-shell-sse--continuation-byte-p
                (aref bytes (1+ index))))
          (setq index (+ index 2)))
         ((and (or (and (= first #xe0)
                        (< (1+ index) length)
                        (>= (aref bytes (1+ index)) #xa0)
                        (<= (aref bytes (1+ index)) #xbf))
                   (and (or (and (>= first #xe1) (<= first #xec))
                            (and (>= first #xee) (<= first #xef)))
                        (< (1+ index) length)
                        (opencode-shell-sse--continuation-byte-p
                         (aref bytes (1+ index))))
                   (and (= first #xed)
                        (< (1+ index) length)
                        (>= (aref bytes (1+ index)) #x80)
                        (<= (aref bytes (1+ index)) #x9f)))
               (< (+ index 2) length)
               (opencode-shell-sse--continuation-byte-p
                (aref bytes (+ index 2))))
          (setq index (+ index 3)))
         ((and (or (and (= first #xf0)
                        (< (1+ index) length)
                        (>= (aref bytes (1+ index)) #x90)
                        (<= (aref bytes (1+ index)) #xbf))
                   (and (>= first #xf1) (<= first #xf3)
                        (< (1+ index) length)
                        (opencode-shell-sse--continuation-byte-p
                         (aref bytes (1+ index))))
                   (and (= first #xf4)
                        (< (1+ index) length)
                        (>= (aref bytes (1+ index)) #x80)
                        (<= (aref bytes (1+ index)) #x8f)))
               (< (+ index 3) length)
               (opencode-shell-sse--continuation-byte-p
                (aref bytes (+ index 2)))
               (opencode-shell-sse--continuation-byte-p
                (aref bytes (+ index 3))))
          (setq index (+ index 4)))
         (t (setq valid nil)))))
    valid))

(defun opencode-shell-sse--decode-utf8 (bytes)
  "Return strict UTF-8 decoding of BYTES, or nil when BYTES is invalid."
  (let ((raw (with-suppressed-warnings ((obsolete string-as-unibyte))
               (string-as-unibyte bytes))))
    (when (opencode-shell-sse--valid-utf8-p raw)
      (decode-coding-string raw 'utf-8))))

(defun opencode-shell-sse--consume-frames (parser bytes)
  "Append BYTES to PARSER's SSE input and return (EVENTS ERROR)."
  (let ((input (concat (opencode-shell-sse-parser-sse-input parser) bytes))
        events error delimiter)
    (while (and (not error)
                (setq delimiter
                      (string-match
                       "\\(?:\r\n\\|\r\\|\n\\)\\(?:\r\n\\|\r\\|\n\\)"
                       input)))
      (let ((frame (substring input 0 delimiter))
            (next-input (substring input (match-end 0))))
        (if (> (length frame) (opencode-shell-sse-parser-max-frame-bytes parser))
            (setq error (opencode-shell-sse--error parser 'frame-too-large))
          (if-let ((decoded (opencode-shell-sse--decode-utf8 frame)))
              (progn
                (unless (opencode-shell-sse-parser-sse-started parser)
                  (setf (opencode-shell-sse-parser-sse-started parser) t)
                  (when (string-prefix-p "\ufeff" decoded)
                    (setq decoded (substring decoded 1))))
                (when-let ((event (opencode-shell-sse--frame-event decoded)))
                  (push event events))
                (setq input next-input))
            (setq error (opencode-shell-sse--error parser 'invalid-utf8))))))
    (when (and (not error)
               (> (length input) (opencode-shell-sse-parser-max-frame-bytes parser)))
      (setq error (opencode-shell-sse--error parser 'frame-too-large)))
    (unless error
      (setf (opencode-shell-sse-parser-sse-input parser) input))
    (list (nreverse events) error)))

(defun opencode-shell-sse--consume-chunks (parser)
  "Consume complete chunked bytes in PARSER and return (EVENTS ERROR)."
  (let ((input (opencode-shell-sse-parser-input parser)) events error progress)
    (setq progress t)
    (while (and progress (not error))
      (setq progress nil)
      (pcase (opencode-shell-sse-parser-chunk-state parser)
        ('size
         (if-let ((end (string-match "\r\n" input)))
             (let* ((line (substring input 0 end))
                    (size-text (opencode-shell-sse--chunk-size line)))
               (cond
                ((> (length line) 8192)
                 (setq error (opencode-shell-sse--error parser 'chunk-line-too-large)))
                ((not size-text)
                 (setq error (opencode-shell-sse--error parser 'chunk-size)))
                (t
                 (let ((size (string-to-number size-text 16)))
                   (if (> size (opencode-shell-sse-parser-max-chunk-bytes parser))
                       (setq error (opencode-shell-sse--error parser 'chunk-too-large))
                     (setq input (substring input (+ end 2))
                           progress t)
                     (if (zerop size)
                         (setf (opencode-shell-sse-parser-chunk-state parser) 'trailers)
                       (setf (opencode-shell-sse-parser-chunk-size parser) size
                             (opencode-shell-sse-parser-chunk-state parser) 'data)))))))
           (when (> (length input) 8192)
             (setq error (opencode-shell-sse--error parser 'chunk-line-too-large)))))
        ('data
         (let ((size (opencode-shell-sse-parser-chunk-size parser)))
           (when (>= (length input) (+ size 2))
             (if (not (equal (substring input size (+ size 2)) "\r\n"))
                 (setq error (opencode-shell-sse--error parser 'chunk-terminator))
               (pcase-let ((`(,new-events ,frame-error)
                            (opencode-shell-sse--consume-frames
                             parser (substring input 0 size))))
                 (setq events (nconc events new-events)
                       error frame-error
                       input (substring input (+ size 2))
                       progress (not frame-error))
                 (unless frame-error
                   (setf (opencode-shell-sse-parser-chunk-size parser) nil
                         (opencode-shell-sse-parser-chunk-state parser) 'size)))))))
        ('trailers
         (cond
          ((string-prefix-p "\r\n" input)
           (setq input (substring input 2))
           (setf (opencode-shell-sse-parser-phase parser) 'done)
           (if (string-empty-p input)
               (setq progress nil)
             (setq error
                   (opencode-shell-sse--error parser 'input-after-terminal))))
          ((string-match "\r\n\r\n" input)
           (let ((trailers (substring input 0 (match-beginning 0)))
                 (next-input (substring input (match-end 0))))
             (if (or (> (length trailers) 8192)
                     (seq-some
                      (lambda (line)
                        (not (string-match-p
                              "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+:[ \t]*[^\r\n]*\\'"
                              line)))
                      (split-string trailers "\r\n" t)))
                 (setq error (opencode-shell-sse--error parser 'trailers))
               (setq input next-input)
               (setf (opencode-shell-sse-parser-phase parser) 'done)
               (if (string-empty-p input)
                   (setq progress nil)
                 (setq error
                       (opencode-shell-sse--error parser 'input-after-terminal))))))
          ((> (length input) 8192)
           (setq error (opencode-shell-sse--error parser 'trailers-too-large)))))))
    (unless error
      (setf (opencode-shell-sse-parser-input parser) input))
    (list events error)))

(defun opencode-shell-sse-parser-feed (parser bytes)
  "Feed binary string BYTES to PARSER.
Return a plist containing `:parser', `:events', and `:error'.  PARSER is
not mutated; the returned parser is the next state."
  (unless (and (opencode-shell-sse-parser-p parser) (stringp bytes))
    (signal 'wrong-type-argument (list 'opencode-shell-sse-parser-p parser)))
  (let ((next (copy-opencode-shell-sse-parser parser)) events error)
    (setf (opencode-shell-sse-parser-input next)
          (concat (opencode-shell-sse-parser-input next) bytes))
    (pcase (opencode-shell-sse-parser-phase next)
      ('headers
       (let* ((input (opencode-shell-sse-parser-input next))
              (end (string-match "\r\n\r\n" input)))
         (cond
          (end
           (if (> end (opencode-shell-sse-parser-max-header-bytes next))
               (setq error (opencode-shell-sse--error next 'headers-too-large))
             (let ((block (substring input 0 end))
                   (body (substring input (match-end 0))))
               (setf (opencode-shell-sse-parser-input next) body)
               (setq error (opencode-shell-sse--parse-headers next block)))))
          ((> (length input) (opencode-shell-sse-parser-max-header-bytes next))
           (setq error (opencode-shell-sse--error next 'headers-too-large))))))
      ((or 'error 'done)
       (when (not (string-empty-p bytes))
         (setq error (list :type 'protocol :reason 'input-after-terminal)))))
    (when (and (not error) (eq (opencode-shell-sse-parser-phase next) 'body))
      (pcase-let ((`(,new-events ,body-error)
                   (if (eq (opencode-shell-sse-parser-transfer next) 'chunked)
                       (opencode-shell-sse--consume-chunks next)
                     (let ((body (opencode-shell-sse-parser-input next)))
                       (setf (opencode-shell-sse-parser-input next) "")
                       (opencode-shell-sse--consume-frames next body)))))
        (setq events new-events error body-error)))
    (list :parser next :events events :error error)))

(defun opencode-shell-sse-parser-finish (parser)
  "Finish PARSER and report an incomplete protocol state without mutating it."
  (unless (opencode-shell-sse-parser-p parser)
    (signal 'wrong-type-argument (list 'opencode-shell-sse-parser-p parser)))
  (let ((next (copy-opencode-shell-sse-parser parser)) error)
    (pcase (opencode-shell-sse-parser-phase next)
      ('done nil)
      ('error
       (setq error '(:type protocol :reason prior-error)))
      ('headers
       (setq error (opencode-shell-sse--error next 'incomplete-headers)))
      ('body
       (unless
           (and (string-empty-p (opencode-shell-sse-parser-input next))
                (string-empty-p (opencode-shell-sse-parser-sse-input next))
                (or (eq (opencode-shell-sse-parser-transfer next) 'identity)
                    (and (eq (opencode-shell-sse-parser-transfer next) 'chunked)
                         (eq (opencode-shell-sse-parser-chunk-state next) 'size)
                         (null (opencode-shell-sse-parser-chunk-size next)))))
         (setq error (opencode-shell-sse--error next 'unexpected-eof)))))
    (list :parser next :events nil :error error)))

(cl-defstruct (opencode-shell-sse-connection
               (:constructor opencode-shell-sse--make-connection))
  token state process parser header-timer on-open on-event on-error)

(defun opencode-shell-sse--valid-request-p (host path headers)
  "Return non-nil when HOST, PATH, and HEADERS are safe for raw HTTP."
  (and (stringp host)
       (string-match-p "\\`[][0-9A-Za-z.:-]+\\'" host)
       (stringp path)
       (string-prefix-p "/" path)
       (seq-every-p (lambda (character) (<= 33 character 126)) path)
       (seq-every-p
        (lambda (header)
          (and (consp header)
               (string-match-p "\\`[!#$%&'*+.^_`|~0-9A-Za-z-]+\\'"
                               (format "%s" (car header)))
               (not (member (downcase (format "%s" (car header)))
                            '("host" "connection" "content-length"
                              "transfer-encoding")))
               (seq-every-p
                (lambda (character)
                  (or (= character 9) (<= 32 character 126)))
                (format "%s" (cdr header)))))
        headers)))

(defun opencode-shell-sse--cancel-header-timer (connection)
  "Cancel CONNECTION's header deadline."
  (when (timerp (opencode-shell-sse-connection-header-timer connection))
    (cancel-timer (opencode-shell-sse-connection-header-timer connection)))
  (setf (opencode-shell-sse-connection-header-timer connection) nil))

(defun opencode-shell-sse--finish (connection token state &optional error)
  "Finish CONNECTION identified by TOKEN in STATE and report ERROR once."
  (when (and (eq token (opencode-shell-sse-connection-token connection))
             (memq (opencode-shell-sse-connection-state connection)
                   '(connecting streaming)))
    (opencode-shell-sse--cancel-header-timer connection)
    (let ((process (opencode-shell-sse-connection-process connection)))
      (setf (opencode-shell-sse-connection-process connection) nil
            (opencode-shell-sse-connection-state connection) state)
      (when (process-live-p process)
        (delete-process process)))
    (when error
      (funcall (opencode-shell-sse-connection-on-error connection) error))
    t))

(defun opencode-shell-sse--filter (connection token process bytes)
  "Feed PROCESS BYTES into CONNECTION when TOKEN is current."
  (when (and (eq token (opencode-shell-sse-connection-token connection))
             (memq (opencode-shell-sse-connection-state connection)
                   '(connecting streaming))
             (or (null (opencode-shell-sse-connection-process connection))
                 (eq process (opencode-shell-sse-connection-process connection))))
    (when (null (opencode-shell-sse-connection-process connection))
      (setf (opencode-shell-sse-connection-process connection) process))
    (let* ((before (opencode-shell-sse-parser-phase
                    (opencode-shell-sse-connection-parser connection)))
           (result (opencode-shell-sse-parser-feed
                    (opencode-shell-sse-connection-parser connection) bytes))
           (parser (plist-get result :parser))
           (error (plist-get result :error)))
      (setf (opencode-shell-sse-connection-parser connection) parser)
      (when (and (eq before 'headers)
                 (eq (opencode-shell-sse-parser-phase parser) 'body))
        (opencode-shell-sse--cancel-header-timer connection)
        (setf (opencode-shell-sse-connection-state connection) 'streaming)
        (when (opencode-shell-sse-connection-on-open connection)
          (funcall (opencode-shell-sse-connection-on-open connection))))
      (dolist (event (plist-get result :events))
        (when (and (eq token (opencode-shell-sse-connection-token connection))
                   (eq (opencode-shell-sse-connection-state connection) 'streaming))
          (funcall (opencode-shell-sse-connection-on-event connection) event)))
      (cond
       (error
        (opencode-shell-sse--finish connection token 'disconnected error))
       ((eq (opencode-shell-sse-parser-phase parser) 'done)
        (opencode-shell-sse--finish
         connection token 'disconnected '(:type transport :reason eof)))))))

(defun opencode-shell-sse--sentinel (connection token process _event)
  "Handle PROCESS termination for CONNECTION when TOKEN is current."
  (when (and (eq token (opencode-shell-sse-connection-token connection))
             (memq (opencode-shell-sse-connection-state connection)
                   '(connecting streaming))
             (memq (process-status process) '(closed failed exit signal))
             (or (null (opencode-shell-sse-connection-process connection))
                 (eq process (opencode-shell-sse-connection-process connection))))
    (when (null (opencode-shell-sse-connection-process connection))
      (setf (opencode-shell-sse-connection-process connection) process))
    (let* ((finish (opencode-shell-sse-parser-finish
                    (opencode-shell-sse-connection-parser connection)))
           (error (or (plist-get finish :error)
                      '(:type transport :reason closed))))
      (setf (opencode-shell-sse-connection-parser connection)
            (plist-get finish :parser))
      (opencode-shell-sse--finish connection token 'disconnected error))))

(defun opencode-shell-sse--header-timeout (connection token)
  "Expire CONNECTION's HTTP header deadline for TOKEN."
  (when (eq token (opencode-shell-sse-connection-token connection))
    (setf (opencode-shell-sse-connection-header-timer connection) nil)
    (opencode-shell-sse--finish
     connection token 'disconnected '(:type timeout :reason headers))))

(defun opencode-shell-sse--request (host port path headers)
  "Return a raw HTTP SSE request for HOST, PORT, PATH, and HEADERS."
  (let ((header-host (if (and (string-match-p ":" host)
                              (not (string-prefix-p "[" host)))
                         (format "[%s]" host)
                       host)))
    (concat "GET " path " HTTP/1.1\r\nHost: " header-host ":"
            (number-to-string port)
            "\r\nAccept: text/event-stream\r\nCache-Control: no-cache\r\n"
            (mapconcat (lambda (header)
                         (format "%s: %s\r\n" (car header) (cdr header)))
                       headers "")
            "Connection: keep-alive\r\n\r\n")))

(cl-defun opencode-shell-sse-start
    (url headers on-event on-error &key (header-timeout 10) on-open)
  "Open URL as an SSE stream and return its connection object.
HEADERS is an alist of additional HTTP headers.  ON-EVENT receives parsed
event plists; ON-ERROR receives typed error plists."
  (unless (and (functionp on-event) (functionp on-error))
    (error "SSE callbacks must be functions"))
  (unless (or (null on-open) (functionp on-open))
    (error "SSE open callback must be nil or a function"))
  (unless (and (numberp header-timeout) (> header-timeout 0))
    (error "SSE header timeout must be positive"))
  (let* ((parsed (condition-case nil (url-generic-parse-url url) (error nil)))
         (scheme (and parsed (downcase (or (url-type parsed) ""))))
         (host (and parsed (url-host parsed)))
         (port (and parsed (or (url-port parsed) 80)))
         (path (and parsed (or (url-filename parsed) "/")))
         (token (make-symbol "opencode-sse-connection"))
         (connection
          (opencode-shell-sse--make-connection
           :token token :state 'connecting :process nil
           :parser (opencode-shell-sse-parser-create)
           :header-timer nil :on-open on-open
           :on-event on-event :on-error on-error)))
    (if (not (and parsed
                  (string-equal scheme "http")
                  (integerp port) (> port 0) (<= port 65535)
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (opencode-shell-sse--valid-request-p host path headers)))
        (progn
          (setf (opencode-shell-sse-connection-state connection) 'closed)
          (funcall on-error '(:type config :reason unsupported-or-unsafe))
          connection)
      (condition-case condition
          (let (process)
            (setf (opencode-shell-sse-connection-header-timer connection)
                  (run-at-time header-timeout nil
                               #'opencode-shell-sse--header-timeout
                               connection token))
            (setq process
                  (make-network-process
                   :name (format "opencode-sse-%s" (sxhash-eq token))
                   :host host :service port :coding 'binary :noquery t
                   :filter (lambda (stream bytes)
                             (opencode-shell-sse--filter
                              connection token stream bytes))
                   :sentinel (lambda (stream event)
                               (opencode-shell-sse--sentinel
                                connection token stream event))))
            (when (memq (opencode-shell-sse-connection-state connection)
                        '(connecting streaming))
              (unless (opencode-shell-sse-connection-process connection)
                (setf (opencode-shell-sse-connection-process connection) process))
              (set-process-query-on-exit-flag process nil)
              (if (process-live-p process)
                  (process-send-string
                   process (opencode-shell-sse--request host port path headers))
                (opencode-shell-sse--finish
                 connection token 'disconnected
                 '(:type transport :reason connect-failed)))))
        (error
         (opencode-shell-sse--finish
          connection token 'disconnected
          (list :type 'transport :reason 'connect-error
                :detail (car condition)))))
      connection)))

(defun opencode-shell-sse-stop (connection)
  "Close CONNECTION without reporting a transport error."
  (when (opencode-shell-sse-connection-p connection)
    (opencode-shell-sse--finish
     connection (opencode-shell-sse-connection-token connection) 'closed)))

(defun opencode-shell-sse-connected-p (connection)
  "Return non-nil when CONNECTION completed its SSE handshake."
  (and (opencode-shell-sse-connection-p connection)
       (eq (opencode-shell-sse-connection-state connection) 'streaming)))

(provide 'opencode-shell-sse)
;;; opencode-shell-sse.el ends here
