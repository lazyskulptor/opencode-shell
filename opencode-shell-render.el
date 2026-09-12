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
