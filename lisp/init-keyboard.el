;;; init-keyboard.el --- Setup personal keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(define-key emacs-lisp-mode-map (kbd "<C-return>") #'xqz-u/eval-defun-ielm)
;; (define-key evil-motion-state-map (kbd "g r") #'xref-find-references)

(use-package hydra :defer t) ;; submenu-like keybindings management

(defhydra hydra-text-scale ()
  "Scale text."
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("*" xqz-u/reset-font-size "reset")
  ("q" nil "quit" :exit t))

(defhydra hydra-opacity-scale ()
  "Adjust transparency."
  ("+" (xqz-u/scale-opacity *opacity-step*))
  ("-" (xqz-u/scale-opacity (- *opacity-step*)))
  ("*" (xqz-u/scale-opacity 0 *opacity*))
  ("q" nil "quit" :exit t))

(use-package general
  :after evil
  :config
  (general-create-definer xqz-u/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(xqz-u/leader-key
  "b"   '(:ignore t :which-key "buffer")
  "ds"  '(kill-sexp :which-key "delete sexp")
  "f"   '(:ignore t :which-key "files")
  "gs"  '(magit-status :which-key "git status")
  "p"   '(:ignore t :which-key "project")
  "q"   '(:ignore t :which-key "quit")
  "t"   '(:ignore t :which-key "toggle")
  "w"   '(:ignore t :which-key "window")
  "z"   '(hydra-text-scale/body :which-key "zoom")
  "SPC" '(counsel-M-x :which-key "commands menu")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to last buffer")
  ";"   '(evilnc-comment-or-uncomment-lines :which-key "toggle comment"))

(xqz-u/leader-key
  :infix "b"
  "b" '(counsel-switch-buffer :which-key "switch to buffer")
  "c" '(xqz-u/copy-file-name :which-key "copy file name")
  "d" '(kill-current-buffer :which-key "kill buffer")
  "h" '(lambda () (interactive) (switch-to-buffer (get-buffer-create dashboard-buffer-name)) :which-key "home"))

(xqz-u/leader-key
  :infix "f"
  "s"  '(save-buffer :which-key "save buffer")
  "f"  '(counsel-find-file :which-key "find file")
  "r"  '(counsel-recentf :which-key "find recent file")
  "D"  '(xqz-u/delete-current-buffer-file :which-key "delete buffer")
  "R"  '(spacemacs/rename-current-buffer-file :which-key "rename buffer")
  "ed" '(open-dot-file :which-key "open dotemacs"))

(xqz-u/leader-key
  :infix "p"
  "f" '(projectile-find-file :which-key "find file in project")
  "g" '(projectile-grep :which-key "grep in project"))

(xqz-u/leader-key
  :infix "q"
  "f" '(delete-frame :which-key "delete frame")
  "q" '(kill-emacs :which-key "quit Emacs"))

(xqz-u/leader-key
  :infix "t"
  "o" '(hydra-opacity-scale/body :which-key "scale transparency")
  "s" '(treemacs-select-directory :which-key "open Treemacs in")
  "t" '(treemacs :which-key "toggle Treemacs")
  "T" '(counsel-load-theme :which-key "load theme"))

(xqz-u/leader-key
  :infix "w"
  "s" '(split-window-vertically :which-key "split-window-vertically")
  "v" '(split-window-horizontally :which-key "split-window-horizontally")
  "d" '(delete-window :which-key "delete-window")
  "h" '(evil-window-left :which-key "evil-window-left")
  "j" '(evil-window-down :which-key "evil-window-down")
  "k" '(evil-window-up :which-key "evil-window-up")
  "l" '(evil-window-right :which-key "evil-window-right")
  "H" '(evil-window-move-far-left :which-key "evil-window-move-far-left")
  "L" '(evil-window-move-far-right :which-key "evil-window-move-far-right")
  "J" '(evil-window-move-very-bottom :which-key "evil-window-move-very-bottom")
  "K" '(evil-window-move-very-top :which-key "evil-window-move-very-top"))


(provide 'init-keyboard)
