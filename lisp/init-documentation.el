;; disable annoying resize of minibuffer on long docstrings.
;; prefer looking up documentation at point in the *eldoc* buffer via
;; M-x eldoc-doc-buffer (bound to K)
(setq eldoc-echo-area-use-multiline-p nil)

;; NOTE also useful for syntax highlighting in eldoc docstrings
(use-package markdown-mode)

;; https://emacs.stackexchange.com/a/82952
(defvar rb--eldoc-html-patterns
  '(("&nbsp;" " ")
    ("&lt;" "<")
    ("&gt;" ">")
    ("&amp;" "&")
    ("&quot;" "\"")
    ("&apos;" "'"))
  "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

(defun rb--string-replace-all (patterns in-string)
  "Replace all cars from PATTERNS in IN-STRING with their pair."
  (mapc (lambda (pattern-pair)
          (setq in-string
                (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
        patterns)
  in-string)

(defun rb--eldoc-preprocess (orig-fun &rest args)
  "Preprocess the docs in ARGS to be displayed by eldoc via ORIG-FUN to replace HTML escapes."
  (let ((doc (car args)))
    ;; The first argument is a list of (STRING :KEY VALUE ...) entries
    ;; we replace the text in each such string
    ;; see docstring of `eldoc-display-functions'
    (when (listp doc)
      (setq doc (mapcar
                 (lambda (doc)
                   (cons
                    (rb--string-replace-all rb--eldoc-html-patterns (car doc))
                    (cdr doc)))
                 doc)))
    (apply orig-fun (cons doc (cdr args)))))

(advice-add 'eldoc-display-in-buffer :around #'rb--eldoc-preprocess)


(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '(
          "\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          apheleia-mode
          flycheck-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


(provide 'init-documentation)
