;;; hocl-mode.el --- HOCL mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar hocl-syntax-table
  (let ((hocl-table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" hocl-table)
    (modify-syntax-entry ?\n "> b" hocl-table)
    hocl-table)
  "Syntax table for `hocl-mode'.")

(defun hocl-comment-dwim (arg)
  "Comment or uncomment current line or region (ARG) in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "//")
        (comment-end ""))
    (comment-dwim arg)))

(defconst hocl-keywords
  '("let" "replace" "replace-one" "by" "if" "in"))

(defconst hocl-types
  '("String" "int" "boolean" "float" "Object"))

;; (defconst hocl-constants
;;   '("SRC" "IN" "PAR" "RES" "SRV" "DST"))

(defvar hocl-keyword-regexp
  (regexp-opt hocl-keywords 'symbols))

(defvar hocl-type-regexp
  (regexp-opt hocl-types 'words))

;; (defvar hocl-constant-regexp
;;   (regexp-opt hocl-constants 'words))

(defvar hocl-font-lock-keywords
  `(("<\\|>\\|?\\|\\[\\|\\]" . font-lock-builtin-face)
    ("\\_<[a-z]\\{2\\}_[[:alnum:]_]+\\_>" . font-lock-function-name-face)
    (,hocl-type-regexp . font-lock-type-face)
    ;; (,hocl-constant-regexp . font-lock-constant-face)
    ;; ("\\(\\?\\)?[a-z]_[[:alnum:]]\\{1,3\\}". font-lock-variable-name-face)
    (,hocl-keyword-regexp . font-lock-keyword-face)
    ;; ("\\_<\\(let\\|replace\\([\\-]one\\)?\\|by\\|if\\|in\\)\\_>" . font-lock-keyword-face)
    ))

(add-to-list 'auto-mode-alist '("\\.hocl\\'" . hocl-mode))

(define-derived-mode hocl-mode fundamental-mode "HOCL-mode" nil
  :syntax-table hocl-syntax-table

  (setq-local font-lock-keywords-only t)
  (setq font-lock-defaults '((hocl-font-lock-keywords)))
  (define-key hocl-mode-map [remap comment-dwim] 'hocl-comment-dwim))

(provide 'hocl-mode)

;;; hocl-mode.el ends here
