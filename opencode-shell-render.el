;;; opencode-shell-render.el --- Safe Markdown presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes
;;; Commentary:

;; Small, safe Markdown fontification for `opencode-shell'.  Source text is
;; preserved exactly; only text properties are added.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defface opencode-shell-markdown-code-face
  '((t :inherit fixed-pitch))
  "Face for Markdown code." :group 'opencode-shell)

(defconst opencode-shell-render-font-lock-keywords
  '(("^\\(#{1,6}\\)\\s-+\\(.+\\)$" (2 'font-lock-function-name-face))
    ("^\\s-*\\(?:[-+*]\\|[0-9]+[.]\\)\\s-+" . font-lock-keyword-face)
    ("^\\s-*>\\s-*" . font-lock-comment-face)
    ("\\[\\([^]\n]+\\)\\](\\([^ )\n]+\\))"
     (1 'link t) (2 'font-lock-string-face t))
    ("`\\([^`\n]+\\)`" (1 'opencode-shell-markdown-code-face t)))
  "Conservative Markdown font-lock rules.")

(defconst opencode-shell-render-table-max-columns 20
  "Maximum number of columns handled by the simple table renderer.")

(defun opencode-shell-render--table-cells (line)
  "Return trimmed cells from a conventional pipe table LINE, or nil."
  (when (and (string-match-p "\\`[ \t]*|.*|[ \t]*\\'" line)
             (not (string-match-p "\\\\|" line)))
    (let* ((trimmed (string-trim line))
           (inner (substring trimmed 1 -1))
           (cells (mapcar #'string-trim (split-string inner "|" nil))))
      (when (and (> (length cells) 0)
                 (<= (length cells) opencode-shell-render-table-max-columns))
        cells))))

(defun opencode-shell-render--table-separator-p (cells)
  "Return non-nil when CELLS form a Markdown table separator row."
  (and cells
       (seq-every-p (lambda (cell)
                      (string-match-p "\\`:?-\\{3,\\}:?\\'" cell))
                    cells)))

(defun opencode-shell-render--fit-column-widths (rows width)
  "Return column widths for ROWS fitting total WIDTH, or nil."
  (let* ((columns (length (car rows)))
         (available (- width (1+ (* 3 columns))))
         (widths (make-vector columns 3)))
    (when (>= available (* 3 columns))
      (dolist (row rows)
        (cl-loop for cell in row
                 for column from 0
                 do (aset widths column
                          (max (aref widths column) (string-width cell)))))
      (while (> (apply #'+ (append widths nil)) available)
        (let ((widest 0))
          (dotimes (column columns)
            (when (> (aref widths column) (aref widths widest))
              (setq widest column)))
          (aset widths widest (1- (aref widths widest)))))
      widths)))

(defun opencode-shell-render--wrap-cell (cell width)
  "Split CELL into strings whose display width is at most WIDTH."
  (if (string-empty-p cell)
      '("")
    (let ((rest cell) chunks)
      (while (not (string-empty-p rest))
        (let ((end 0))
          (while (and (< end (length rest))
                      (<= (string-width (substring rest 0 (1+ end))) width))
            (setq end (1+ end)))
          (when (= end 0) (setq end 1))
          (let ((next end))
            (when (< end (length rest))
              (when-let ((space (cl-position-if
                                 (lambda (char) (memq char '(?\s ?\t)))
                                 rest :end end :from-end t)))
                (when (> space 0)
                  (setq end space next (1+ space)))))
            (push (substring rest 0 end) chunks)
            (setq rest (string-trim-left (substring rest next))))))
      (nreverse chunks))))

(defun opencode-shell-render--pad-cell (cell width)
  "Pad CELL on the right to display WIDTH."
  (concat cell (make-string (max 0 (- width (string-width cell))) ?\s)))

(defun opencode-shell-render--layout-table (rows width)
  "Lay out parsed table ROWS within WIDTH, wrapping cell contents."
  (when-let ((widths (opencode-shell-render--fit-column-widths rows width)))
    (let (lines)
      (cl-loop for row in rows
               for row-number from 0
               do
               (if (= row-number 1)
                   (push (concat "| "
                                 (mapconcat (lambda (column-width)
                                              (make-string column-width ?-))
                                            (append widths nil) " | ")
                                 " |")
                         lines)
                 (let* ((wrapped
                         (cl-loop for cell in row
                                  for column from 0
                                  collect (opencode-shell-render--wrap-cell
                                           cell (aref widths column))))
                        (height (apply #'max (mapcar #'length wrapped))))
                   (dotimes (line-number height)
                     (push
                      (concat "| "
                              (cl-loop for chunks in wrapped
                                       for column from 0
                                       collect
                                       (opencode-shell-render--pad-cell
                                        (or (nth line-number chunks) "")
                                        (aref widths column))
                                       into cells
                                       finally return (mapconcat #'identity cells " | "))
                              " |")
                      lines)))))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun opencode-shell-render-tables (text width)
  "Return TEXT with complete pipe tables laid out within WIDTH.
Text outside recognized tables, including fenced code blocks, is unchanged."
  (let ((lines (split-string text "\n" nil))
        (in-fence nil)
        result)
    (while lines
      (let ((line (car lines)))
        (cond
         ((string-match-p "\\`[ \t]*\\(```\\|~~~\\)" line)
          (setq in-fence (not in-fence))
          (push line result)
          (setq lines (cdr lines)))
         ((or in-fence (< (length lines) 3))
          (push line result)
          (setq lines (cdr lines)))
         (t
          (let* ((header (opencode-shell-render--table-cells line))
                 (separator (opencode-shell-render--table-cells (cadr lines))))
            (if (and header separator
                     (= (length header) (length separator))
                     (opencode-shell-render--table-separator-p separator)
                     (let ((body (opencode-shell-render--table-cells (caddr lines))))
                       (and body (= (length body) (length header)))))
                (let ((table (list header separator))
                      (raw-table (list (cadr lines) line))
                      body)
                  (setq lines (cddr lines))
                  (while (let ((row (and lines
                                         (opencode-shell-render--table-cells (car lines)))))
                           (when (and row (= (length row) (length header)))
                             (push row body)
                             (push (car lines) raw-table)
                             (setq lines (cdr lines))
                             t)))
                  (setq table (append table (nreverse body)))
                  (let ((rendered (opencode-shell-render--layout-table
                                   table width)))
                    (if rendered
                        (dolist (rendered-line (split-string rendered "\n" nil))
                          (push rendered-line result))
                      (dolist (raw-line (nreverse raw-table))
                        (push raw-line result)))))
              (push line result)
              (setq lines (cdr lines))))))))
    (mapconcat #'identity (nreverse result) "\n")))

(defun opencode-shell-render-markdown-region (beg end)
  "Fontify Markdown between BEG and END without changing its text."
  (unless font-lock-keywords
    (font-lock-set-defaults))
  (save-excursion
    (font-lock-append-text-property beg end 'fontified t)
    (font-lock-fontify-keywords-region beg end)
    (goto-char beg)
    (let ((open nil))
      (while (re-search-forward "^```.*$" end t)
        (if open
            (progn
              (add-face-text-property open (line-end-position)
                                      'opencode-shell-markdown-code-face t)
              (setq open nil))
          (setq open (line-beginning-position))))
      (when open
        (add-face-text-property open end 'opencode-shell-markdown-code-face t)))))

(defun opencode-shell-render-insert (text)
  "Insert TEXT verbatim and apply safe Markdown presentation properties."
  (let ((beg (point)))
    (insert text)
    (opencode-shell-render-markdown-region beg (point))))

(provide 'opencode-shell-render)
;;; opencode-shell-render.el ends here
