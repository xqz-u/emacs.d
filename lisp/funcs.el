(defconst *opacity* 90)
(defconst *opacity-step* 10)
;; the variable modified to control transparency
(defvar opacity *opacity*)


(defun xqz-u/reset-font-size ()
  "Reset the font size."
  (interactive)
  (text-scale-increase 0))


(defun xqz-u/on-python-save ()
  "Run isort and `elpy-format-code` on save for Python files."
  (when (eq major-mode 'python-mode)
    (py-isort-buffer)
    (elpy-format-code)))


(defun xqz-u/toggle-workon ()
  "Toggle `pyvenv-workon`"
  (interactive)
  (if pyvenv-virtual-env
      (pyvenv-deactivate)
    (call-interactively #'pyvenv-workon)))


(defun xqz-u/delete-current-buffer-file ()
  "Deletes current file and associated buffer, asking for confirmation."
  (interactive)
  (let ((bname (buffer-name))
        (fname (buffer-file-name)))
    (when (y-or-n-p (format "Delete file %s:" fname))
      (delete-file fname)
      (evil-delete-buffer bname))))


(defun spacemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (featurep 'projectile)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))


;; TODO do nothing if on whitespace outside form? rn evaluates
;; previous form
(defun xqz-u/eval-defun-ielm ()
  "Evaluates outermost lisp form in ielm, creating a new ielm process
if none is available. Point is moved after the evaluated form
in th original buffer."
  (interactive)
  (let ((working-buffer (current-buffer))
        (sexp (progn
                (end-of-defun)
                (elisp--preceding-sexp))))
    (switch-to-buffer-other-window "*ielm*")
    (with-current-buffer (progn
                           (ielm)
                           "*ielm*")
      (cl-prettyprint `,sexp)
      (ielm-return))
    (switch-to-buffer-other-window working-buffer)))


(defun xqz-u/elpy-project-find-egg ()
  (locate-dominating-file default-directory ".xqz-u-egg"))



;; https://www.emacswiki.org/emacs/TransparentEmacs
(defun emacs-wiki/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun xqz-u/scale-opacity (inc &optional reset-val)
  "Scales frame opacity [0-100] of selected frame by INC."
  (interactive "nOpacity scale factor: ")
  (let ((new-val (or reset-val (+ inc opacity))))
    (if (or (< new-val 0) (> new-val 100))
        (user-error "Invalid transparency value %s [0-100]" new-val)
      (progn
        (setq opacity new-val)
        (emacs-wiki/transparency new-val)
        (message "New opacity: %s" opacity)))))


(defun xqz-u/copy-file-name ()
  "Pushes the visited buffer's file name onto the kill ring."
  (interactive)
  (progn
    (kill-new buffer-file-name)
    (message "(p)aste: `%s'" buffer-file-name)))


(defun open-dot-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))


(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))


(provide 'funcs)
