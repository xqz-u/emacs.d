;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'recentf-mode)

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/"
                   "/ssh:"
                   ,package-user-dir
                   ;; ,(concat package-user-dir "/.*-autoloads\\.el\\'")
                   ;; ,(concat user-emacs-directory ".cache")
                   ,(recentf-expand-file-name no-littering-var-directory)
                   ,(recentf-expand-file-name no-littering-etc-directory)))


(provide 'init-recentf)
;;; init-recentf.el ends here
