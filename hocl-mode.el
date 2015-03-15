;;; hocl-mode.el --- HOCL mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  Simple mode for HOCL files.
;;  Site: https://github.com/jrbalderrama/hocl-mode
;;
;;; Code:

(defconst hocl-types
  '("String" "int" "boolean" "float" "Object")
  "HOCL reserved words for data types.")

;; (defconst hocl-constants
;;   '("SRC" "IN" "PAR" "RES" "SRV" "DST"))

(defconst hocl-keywords
  '("let" "replace" "replace-one" "by" "if" "in")
  "HOCL reserved words for language keywords.")

(defvar hocl-buildin-regexp
  "<\\|>\\|?\\|\\[\\|\\]"
  "HOCL build-in symbols.
Normally non [:alnum:] characters.")

(defvar hocl-function-regexp
  "\\<[a-z]\\{2\\}_[[:alnum:]_]+\\>"
  "HOCL function name pattern.
The accepted pattern is associated to the string xx_y[y]+ where
'x' is an alphabet char and 'y' is an alphanumeric char.  For
example: 'gw_pass'.")

(defvar hocl-type-regexp
  (regexp-opt hocl-types 'words)
  "HOCL variable type pattern.")

;; (defvar hocl-constant-regexp
;;   (regexp-opt hocl-constants 'words)
;;   "HOCL constant pattern.")

;; (defvar hocl-variable-regexp
;;   "\\(\\?\\)?[a-z]_[[:alnum:]]\\{1,3\\}"
;;   "HOCL variable name pattern.
;; The suggested pattern is associated to the string format
;; 'x_y[yy]' where 'x' is an alphabet char and 'y' is a alphanumeric
;; char.  For example: 'w_src'.")

(defvar hocl-keyword-regexp
  ;; "\\_<\\(let\\|replace\\([\\-]one\\)?\\|by\\|if\\|in\\)\\_>"
  (regexp-opt hocl-keywords 'symbols)
  "HOCL keywords pattern.")

(defvar hocl-syntax-table
  (let ((hocl-table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" hocl-table)
    (modify-syntax-entry ?\n "> b" hocl-table)
    hocl-table)
  "Syntax table for `hocl-mode'.")

(defvar hocl-font-lock-keywords
  `((,hocl-buildin-regexp . font-lock-builtin-face)
    (,hocl-function-regexp . font-lock-function-name-face)
    (,hocl-type-regexp . font-lock-type-face)
    ;; (,hocl-constant-regexp . font-lock-constant-face)
    ;; (,hocl-variable-regexp . font-lock-variable-name-face)
    (,hocl-keyword-regexp . font-lock-keyword-face)))

(defun hocl-comment-dwim (arg)
  "Comment or uncomment current line or region (ARG) in a smart way.
For details see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "//")
        (comment-end ""))
    (comment-dwim arg)))

(define-derived-mode hocl-mode fundamental-mode "HOCL-mode"
  "Simple mode to highlight HOCL keywords and support comments."
  :syntax-table hocl-syntax-table

  (setq font-lock-defaults '((hocl-font-lock-keywords)))
  (define-key hocl-mode-map [remap comment-dwim] 'hocl-comment-dwim))

;; associate files with 'hocl' extension with this mode
(add-to-list 'auto-mode-alist '("\\.hocl\\'" . hocl-mode))

(provide 'hocl-mode)

;;; hocl-mode.el ends here
