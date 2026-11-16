;;; opencode-shell-render.el --- Safe Markdown presentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: opencode-shell contributors
;; Keywords: tools, processes
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Small, safe Markdown fontification for `opencode-shell'.  Source text is
;; preserved exactly; only text properties are added.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Each Markdown element gets a visibly distinct hue (Zenburn-family palette,
;; matching the role faces defined in `opencode-shell').

(defface opencode-shell-markdown-code-face
  '((((class color) (background dark))
     :inherit fixed-pitch :background "#35352b" :foreground "#dfcf9f")
    (((class color) (background light))
     :inherit fixed-pitch :background "#f0ead6" :foreground "#5f5f00")
    (t :inherit fixed-pitch))
  "Face for Markdown inline and fenced code." :group 'opencode-shell)

(defface opencode-shell-markdown-bold-face
  '((((class color) (background dark)) :foreground "#dfaf8f" :weight bold)
    (((class color) (background light)) :foreground "#a6460f" :weight bold)
    (t :weight bold))
  "Face for Markdown **strong** text (orange)." :group 'opencode-shell)

(defface opencode-shell-markdown-emphasis-face
  '((((class color) (background dark)) :foreground "#dc8cc3" :slant italic)
    (((class color) (background light)) :foreground "#8b1a7a" :slant italic)
    (t :slant italic))
  "Face for Markdown *emphasized* text (magenta)." :group 'opencode-shell)

(defface opencode-shell-markdown-heading-face
  '((((class color) (background dark)) :foreground "#94bff3" :weight bold)
    (((class color) (background light)) :foreground "#1f5bb6" :weight bold)
    (t :inherit font-lock-function-name-face :weight bold))
  "Face for Markdown headings (blue)." :group 'opencode-shell)

(defface opencode-shell-markdown-list-face
  '((((class color) (background dark)) :foreground "#f0dfaf" :weight bold)
    (((class color) (background light)) :foreground "#8a6d00" :weight bold)
    (t :inherit font-lock-keyword-face))
  "Face for Markdown list markers (yellow)." :group 'opencode-shell)

(defface opencode-shell-markdown-quote-face
  '((((class color) (background dark)) :foreground "#7f9f7f" :slant italic)
    (((class color) (background light)) :foreground "#3a663a" :slant italic)
    (t :inherit font-lock-comment-face))
  "Face for Markdown blockquote markers (green)." :group 'opencode-shell)

(defface opencode-shell-markdown-link-face
  '((((class color) (background dark)) :foreground "#8cd0d3" :underline t)
    (((class color) (background light)) :foreground "#00688b" :underline t)
    (t :inherit link))
  "Face for Markdown link text (cyan)." :group 'opencode-shell)

(defface opencode-shell-markdown-url-face
  '((((class color) (background dark)) :foreground "#93e0e3" :underline t)
    (((class color) (background light)) :foreground "#3a7a7c" :underline t)
    (t :inherit font-lock-string-face))
  "Face for Markdown link URLs." :group 'opencode-shell)

(defun opencode-shell-render--match-fenced-code (limit)
  "Match the body of a closed ``` or ~~~ fenced code block before LIMIT."
  (let (matched)
    (while (and (not matched)
                (< (point) limit)
                (re-search-forward "^\\(```\\|~~~\\).*\n" limit t))
      (let* ((open-beg (match-beginning 0))
             (fence (match-string 1))
             (body-beg (point))
             (close-re (concat "^" (regexp-quote fence) "[ \t]*$")))
        (if (re-search-forward close-re limit t)
            (let ((body-end (match-beginning 0))
                  (fence-end (match-end 0)))
              (set-match-data (list open-beg fence-end body-beg body-end))
              (goto-char (min limit (1+ fence-end)))
              (setq matched t))
          (goto-char limit))))
    matched))

(defconst opencode-shell-render-font-lock-keywords
  '(;; Shade the body of a ``` / ~~~ fenced code block linearly.
    (opencode-shell-render--match-fenced-code
     (1 'opencode-shell-markdown-code-face t))
    ("^\\(#\\{1,6\\}\\).*$" (0 'opencode-shell-markdown-heading-face))
    ("^\\s-*\\(?:[-+*]\\|[0-9]+[.]\\)\\s-+" (0 'opencode-shell-markdown-list-face))
    ("^\\s-*>\\s-*" (0 'opencode-shell-markdown-quote-face))
    ("\\[\\([^]\n]+\\)\\](\\([^ )\n]+\\))"
     (1 'opencode-shell-markdown-link-face t) (2 'opencode-shell-markdown-url-face t))
    ("\\*\\*\\([^*\n]+\\)\\*\\*" (1 'opencode-shell-markdown-bold-face))
    ("\\(?:^\\|[^*]\\)\\(\\*[^*\n]+\\*\\)" (1 'opencode-shell-markdown-emphasis-face))
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
      (let ((total (apply #'+ (append widths nil))))
        (while (> total available)
          (let ((widest 0))
            (cl-loop for col from 1 below columns
                     when (> (aref widths col) (aref widths widest))
                     do (setq widest col))
            (aset widths widest (1- (aref widths widest)))
            (setq total (1- total)))))
      widths)))

(defun opencode-shell-render--wrap-cell (cell width)
  "Split CELL into strings whose display width is at most WIDTH."
  (if (string-empty-p cell)
      '("")
    (let ((rest cell) chunks)
      (while (not (string-empty-p rest))
        (let ((end (if (<= (string-width rest) width)
                       (length rest)
                     (let ((low 0)
                           (high (length rest)))
                       (while (< low high)
                         (let ((mid (/ (+ low high 1) 2)))
                           (if (<= (string-width (substring rest 0 mid)) width)
                               (setq low mid)
                             (setq high (1- mid)))))
                       low))))
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
                              (mapconcat
                               #'identity
                               (cl-loop for chunks in wrapped
                                        for column from 0
                                        collect (opencode-shell-render--pad-cell
                                                 (or (nth line-number chunks) "")
                                                 (aref widths column)))
                               " | ")
                              " |")
                      lines)))))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun opencode-shell-render-tables (text width)
  "Return TEXT with complete pipe tables laid out within WIDTH.
Text outside recognized tables, including fenced code blocks, is unchanged."
  (if (not (string-match-p "|.*---" text))
      text
    (let ((lines (split-string text "\n" nil))
          (in-fence nil)
          result)
      (while lines
        (let ((line (car lines)))
          (cond
           ((string-match-p "\\`[ \t]*\\(```\\|~~~\\)" line)
            (setq in-fence (not in-fence))
            (push (pop lines) result))
           ((or in-fence (null (cddr lines)))
            (push (pop lines) result))
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
                               (push (pop lines) raw-table)
                               t)))
                    (setq table (append table (nreverse body)))
                    (let ((rendered (opencode-shell-render--layout-table
                                     table width)))
                      (dolist (output-line (if rendered
                                               (split-string rendered "\n" nil)
                                             (nreverse raw-table)))
                        (push output-line result))))
                (push (pop lines) result)))))))
      (mapconcat #'identity (nreverse result) "\n"))))

(provide 'opencode-shell-render)
;;; opencode-shell-render.el ends here
