;;; funcdoc-popup.el --- Floating docs for the selected function name -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (posframe "1.4.0"))
;; Keywords: convenience, docs, tools
;; URL: https://example.com/funcdoc-popup

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Select the name of a function (double-click, `C-SPC' + movement, `M-@'...)
;; and a floating child frame appears at the selection showing the
;; documentation written inside that function: its docstring or, failing
;; that, the block of comments at the top of its body.
;;
;; The name is resolved through the xref backend (Eglot, lsp-mode, ...), so
;; definitions in other files work.  Without a language server, only
;; definitions in the current buffer are found.
;;
;; Python is supported out of the box.  Other languages can be added with
;; `funcdoc-popup-register-language'.
;;
;; Usage:
;;
;;   (add-hook 'python-mode-hook #'funcdoc-popup-mode)
;;
;; or enable it everywhere with `funcdoc-popup-global-mode'.
;;
;; Requires a graphical Emacs (child frames).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'posframe)

(declare-function python-nav-end-of-statement "python")

;;;; Customization

(defgroup funcdoc-popup nil
  "Floating documentation popup for selected function names."
  :group 'convenience
  :prefix "funcdoc-popup-")

(defcustom funcdoc-popup-max-width 80
  "Maximum width of the popup, in columns."
  :type 'integer)

(defcustom funcdoc-popup-max-height 25
  "Maximum height of the popup, in lines."
  :type 'integer)

(defcustom funcdoc-popup-border-width 1
  "Border width of the popup, in pixels."
  :type 'integer)

(defcustom funcdoc-popup-internal-border-width 6
  "Padding between the popup text and its border, in pixels."
  :type 'integer)

(defcustom funcdoc-popup-border-color "gray50"
  "Color of the popup border."
  :type 'color)

(defcustom funcdoc-popup-use-xref t
  "Non-nil means resolve the selected name through the xref backend.
This is what makes definitions in other files work."
  :type 'boolean)

(defface funcdoc-popup-face
  '((t :inherit tooltip))
  "Face used for the popup's text and background.")

;;;; Internals

(defconst funcdoc-popup--buffer " *funcdoc-popup*"
  "Name of the buffer shown in the popup frame.")

(defvar-local funcdoc-popup--last-key nil
  "Cons (NAME . POSITION) of the last selection that was looked up.")

(defvar-local funcdoc-popup--visible nil
  "Non-nil when the popup is currently shown from this buffer.")

;;;; Language registry

(defvar funcdoc-popup-languages nil
  "Alist of language definitions.
Each element is (MODE :def-regexp FORMAT :extract FUNCTION), where
MODE is a major mode (derived modes match too), FORMAT is a `format'
string with one %s for the quoted name, and FUNCTION is called with
point at the start of a definition and returns the documentation
string or nil.")

(defun funcdoc-popup-register-language (mode def-regexp extract)
  "Register support for major MODE.
DEF-REGEXP is a format string with one %s that receives the
`regexp-quote'd function name and must match the start of its
definition.  EXTRACT is a function called with point on that
definition; it returns the documentation string or nil."
  (setf (alist-get mode funcdoc-popup-languages nil t #'eq)
        (list :def-regexp def-regexp :extract extract)))

(defun funcdoc-popup--language ()
  "Return the language entry for the current buffer, or nil."
  (cl-loop for (mode . plist) in funcdoc-popup-languages
           when (derived-mode-p mode) return plist))

;;;; Python

(defun funcdoc-popup--indent (line)
  "Return the width of the leading whitespace of LINE."
  (if (string-match "\\`[ \t]*" line) (match-end 0) 0))

(defun funcdoc-popup--dedent (text)
  "Strip the common indentation from the lines of TEXT after the first."
  (let* ((lines (split-string text "\n"))
         (rest (cdr lines))
         (indent (apply #'min 1000
                        (mapcar #'funcdoc-popup--indent
                                (seq-filter (lambda (l) (string-match-p "[^ \t]" l))
                                            rest)))))
    (string-join
     (cons (string-trim (car lines))
           (mapcar (lambda (l)
                     (if (>= (length l) indent) (substring l indent) (string-trim l)))
                   rest))
     "\n")))

(defun funcdoc-popup--python-extract ()
  "Return the docs of the Python def/class at point, or nil.
Prefers the docstring; falls back to the leading `#' comment block."
  (beginning-of-line)
  (when (looking-at "[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|class\\)[ \t]")
    (python-nav-end-of-statement)
    (forward-line 1)
    (skip-chars-forward " \t\n")
    (cond
     ((looking-at "[rRuUbBfF]*\\(\"\"\"\\|'''\\|\"\\|'\\)")
      (let ((start (point)))
        (forward-sexp)
        (let* ((raw (buffer-substring-no-properties start (point)))
               (raw (replace-regexp-in-string
                     "\\`[rRuUbBfF]*\\(\"\"\"\\|'''\\|\"\\|'\\)" "" raw))
               (raw (replace-regexp-in-string
                     "\\(\"\"\"\\|'''\\|\"\\|'\\)\\'" "" raw)))
          (string-trim (funcdoc-popup--dedent raw)))))
     ((looking-at "[ \t]*#")
      (let (lines)
        (while (looking-at "[ \t]*#+[ \t]?\\(.*\\)$")
          (push (match-string-no-properties 1) lines)
          (forward-line 1))
        (string-join (nreverse lines) "\n"))))))

(defconst funcdoc-popup--python-def-regexp
  "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|class\\)[ \t]+%s[ \t]*[(:]")

(funcdoc-popup-register-language
 'python-mode funcdoc-popup--python-def-regexp #'funcdoc-popup--python-extract)
(funcdoc-popup-register-language
 'python-ts-mode funcdoc-popup--python-def-regexp #'funcdoc-popup--python-extract)

;;;; Looking up the documentation

(defun funcdoc-popup--extract-here ()
  "Extract docs at point using the current buffer's language entry."
  (when-let* ((lang (funcdoc-popup--language)))
    (save-excursion
      (funcall (plist-get lang :extract)))))

(defun funcdoc-popup--local (name)
  "Look for a definition of NAME in the current buffer."
  (when-let* ((lang (funcdoc-popup--language)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format (plist-get lang :def-regexp) (regexp-quote name)) nil t)
        (goto-char (match-beginning 0))
        (funcall (plist-get lang :extract))))))

(defun funcdoc-popup--xref (pos)
  "Resolve the symbol at POS via xref and return the docs of its definition."
  (let ((backend (xref-find-backend)))
    ;; The etags fallback would prompt for a TAGS file; skip it.
    (unless (memq backend '(nil etags))
      (condition-case nil
          (save-excursion
            (goto-char pos)
            (when-let* ((id (xref-backend-identifier-at-point backend))
                        (defs (xref-backend-definitions backend id)))
              (cl-some
               (lambda (item)
                 (let ((marker (xref-location-marker (xref-item-location item))))
                   (with-current-buffer (marker-buffer marker)
                     (save-excursion
                       (goto-char marker)
                       (funcdoc-popup--extract-here)))))
               defs)))
        (error nil)))))

(defun funcdoc-popup--doc-for (name pos)
  "Return documentation for the function NAME selected at POS."
  (or (and funcdoc-popup-use-xref (funcdoc-popup--xref pos))
      (funcdoc-popup--local name)))

;;;; Popup

(defun funcdoc-popup--hide ()
  "Hide the popup and forget the last lookup."
  (when funcdoc-popup--visible
    (setq funcdoc-popup--visible nil)
    (posframe-hide funcdoc-popup--buffer))
  (setq funcdoc-popup--last-key nil))

(defun funcdoc-popup--show (doc pos)
  "Show DOC in a child frame anchored at POS."
  (setq funcdoc-popup--visible t)
  (posframe-show funcdoc-popup--buffer
                 :string doc
                 :position pos
                 :poshandler #'posframe-poshandler-point-bottom-left-corner
                 :foreground-color (face-foreground 'funcdoc-popup-face nil t)
                 :background-color (face-background 'funcdoc-popup-face nil t)
                 :border-width funcdoc-popup-border-width
                 :border-color funcdoc-popup-border-color
                 :internal-border-width funcdoc-popup-internal-border-width
                 :max-width funcdoc-popup-max-width
                 :max-height funcdoc-popup-max-height))

(defun funcdoc-popup--update ()
  "Show or hide the popup according to the current selection.
Runs from `post-command-hook'."
  (if (and (use-region-p)
           (= (line-number-at-pos (region-beginning))
              (line-number-at-pos (region-end))))
      (let* ((beg (region-beginning))
             (name (string-trim
                    (buffer-substring-no-properties beg (region-end))))
             (key (cons name beg)))
        ;; Look up once per selection so the language server isn't queried
        ;; after every command.
        (unless (equal key funcdoc-popup--last-key)
          (setq funcdoc-popup--last-key key)
          (let ((doc (and (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)
                          (funcdoc-popup--doc-for name beg))))
            (if (and doc (not (string-empty-p doc)))
                (funcdoc-popup--show doc beg)
              (when funcdoc-popup--visible
                (setq funcdoc-popup--visible nil)
                (posframe-hide funcdoc-popup--buffer))))))
    (when (or funcdoc-popup--visible funcdoc-popup--last-key)
      (funcdoc-popup--hide))))

;;;; Minor modes

;;;###autoload
(define-minor-mode funcdoc-popup-mode
  "Show a function's documentation in a floating frame when its name is selected."
  :lighter " FDoc"
  :group 'funcdoc-popup
  (if funcdoc-popup-mode
      (if (and (display-graphic-p) (posframe-workable-p))
          (add-hook 'post-command-hook #'funcdoc-popup--update nil t)
        (setq funcdoc-popup-mode nil)
        (message "funcdoc-popup: child frames are not available here"))
    (remove-hook 'post-command-hook #'funcdoc-popup--update t)
    (funcdoc-popup--hide)))

(defun funcdoc-popup--maybe-enable ()
  "Enable `funcdoc-popup-mode' in buffers whose major mode is supported."
  (when (funcdoc-popup--language)
    (funcdoc-popup-mode 1)))

;;;###autoload
(define-globalized-minor-mode funcdoc-popup-global-mode
  funcdoc-popup-mode funcdoc-popup--maybe-enable
  :group 'funcdoc-popup)

(provide 'funcdoc-popup)

;;; funcdoc-popup.el ends here
