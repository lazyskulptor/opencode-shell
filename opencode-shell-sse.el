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

(cl-defstruct (opencode-shell-sse-parser
               (:constructor opencode-shell-sse--make-parser))
  phase input transfer chunk-state chunk-size sse-input
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
   :chunk-state 'size :chunk-size nil :sse-input ""
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
         (encodings (opencode-shell-sse--header-values fields "transfer-encoding")))
    (cond
     ((not (and status
                (string-match-p
                 "\\`HTTP/[0-9]+\\.[0-9]+ 2[0-9][0-9]\\(?: [^\r\n]*\\)?\\'"
                 status)))
      (opencode-shell-sse--error parser 'http-status))
     ((not (seq-some
            (lambda (value)
              (string-equal
               (downcase (string-trim (car (split-string value ";"))))
               "text/event-stream"))
            content-types))
      (opencode-shell-sse--error parser 'content-type))
     (t
      (setf (opencode-shell-sse-parser-phase parser) 'body
            (opencode-shell-sse-parser-transfer parser)
            (if (seq-some
                 (lambda (value)
                   (member "chunked"
                           (mapcar #'string-trim
                                   (split-string (downcase value) "," t))))
                 encodings)
                'chunked
              'identity))
      nil))))

(defun opencode-shell-sse--field-value (line)
  "Return the SSE field value from LINE after its first colon."
  (if (string-match ":\\(.*\\)\\'" line)
      (let ((value (match-string 1 line)))
        (if (string-prefix-p " " value) (substring value 1) value))
    ""))

(defun opencode-shell-sse--frame-event (frame)
  "Convert a complete SSE FRAME into an event plist, or nil."
  (let (data event id saw-data)
    (dolist (line (split-string frame "\r?\n"))
      (cond
       ((or (string-empty-p line) (string-prefix-p ":" line)))
       ((string-match-p "\\`data\\(?:\\|:\\)" line)
        (setq saw-data t)
        (push (opencode-shell-sse--field-value line) data))
       ((string-match-p "\\`event\\(?:\\|:\\)" line)
        (setq event (opencode-shell-sse--field-value line)))
       ((string-match-p "\\`id\\(?:\\|:\\)" line)
        (setq id (opencode-shell-sse--field-value line)))))
    (when saw-data
      (list :data (decode-coding-string
                   (mapconcat #'identity (nreverse data) "\n") 'utf-8)
            :event event :id id))))

(defun opencode-shell-sse--consume-frames (parser bytes)
  "Append BYTES to PARSER's SSE input and return (EVENTS ERROR)."
  (let ((input (concat (opencode-shell-sse-parser-sse-input parser) bytes))
        events error delimiter)
    (while (and (not error)
                (setq delimiter (string-match "\r?\n\r?\n" input)))
      (let ((frame (substring input 0 delimiter))
            (next-input (substring input (match-end 0))))
        (if (> (length frame) (opencode-shell-sse-parser-max-frame-bytes parser))
            (setq error (opencode-shell-sse--error parser 'frame-too-large))
          (when-let ((event (opencode-shell-sse--frame-event frame)))
            (push event events))
          (setq input next-input))))
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
             (let ((line (substring input 0 end)))
               (cond
                ((> (length line) 8192)
                 (setq error (opencode-shell-sse--error parser 'chunk-line-too-large)))
                ((not (string-match
                       "\\`\\([[:xdigit:]]+\\)\\(?:;[^\r\n]*\\)?\\'" line))
                 (setq error (opencode-shell-sse--error parser 'chunk-size)))
                (t
                 (let ((size (string-to-number (match-string 1 line) 16)))
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
           (setq progress nil))
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
               (setq progress nil))))
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

(provide 'opencode-shell-sse)
;;; opencode-shell-sse.el ends here
