;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; https://github.com/emacscollective/no-littering?tab=readme-ov-file#native-compilation-cache
(when (and (fboundp 'startup-redirect-eln-cache)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; perform some operations here for startup time optimization
;; ref: https://www.omarpolo.com/dots/emacs.html

;; disable some modes & indent via spaces, not tabs (default)
(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                tool-bar-mode
                tooltip-mode
                menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; load custom code
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(provide 'early-init)
;;; early-init.el ends here
