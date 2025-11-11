;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/purcell/emacs.d/blob/6c7fa676a547f83bd4e9c3a41b6b20a84236eaa7/lisp/init-whitespace.el
;; with small modifications

(setq-default show-trailing-whitespace nil)

;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))


(use-package whitespace-cleanup-mode)
(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(with-eval-after-load 'whitespace-cleanup-mode
  (diminish 'whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)


;; global before-save hooks
(dolist (hook '(whitespace-cleanup
                delete-trailing-whitespace))
  (add-hook 'before-save-hook hook))


(provide 'init-whitespace)
;;; init-whitespace.el ends here
