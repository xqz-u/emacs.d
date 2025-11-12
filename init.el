;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; TODO use treesitter properly: look into treesit-auto
;; (or manual treesitter https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)
;; & combobulate
;; TODO checkout the whole vertico + corfu etc stack instead of ivy + company
;; https://www.reddit.com/r/emacs/comments/1fc6igt/what_is_a_completion_framework_what_are_the_pros/



;; ---- INIT ----


;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq jit-lock-defer-time 0)

;; General performance tuning
;; (when (require-package 'gcmh)
;;   (setq gcmh-high-cons-threshold (* 128 1024 1024))
;;   (add-hook 'after-init-hook (lambda ()
;;                                (gcmh-mode)
;;                                (diminish 'gcmh-mode))))


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(setq package-native-compile t
      package-install-upgrade-built-in t)
;; load and activate packages in `packages-load-list`
(package-initialize)
;; refresh ELPA packages info for packages downloaded from `package-archives`
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; `use-package` will download a package if it is not already available
(setq use-package-always-ensure t)

;; load this package ASAP: do not leak secrets across the fs & keep the fs clean
(use-package no-littering
  :config
  (no-littering-theme-backups))


(defconst *is-a-mac* (eq system-type 'darwin))

;; my own lisp/ folder is added to the load-path in early-init.el
(require 'funcs)
;; show startup time in minibuffer
(add-hook 'emacs-startup-hook 'efs/display-startup-time)

;; load shell variables asap
(require 'init-exec-path)

(setq custom-file (locate-user-emacs-file "custom.el"))

(setq user-full-name "Marco A. Gallo"
      user-mail-address "marco.gallo0530@gmail.com")


;; ---- BASIC UI ----


(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)

(setq inhibit-startup-message t
      visible-bell nil
      display-line-numbers-type 'relative
      require-final-newline t
      global-auto-revert-non-file-buffers t
      vc-follow-symlinks t
      help-window-select t)

(setq-default indent-tabs-mode nil)

;; built in theme
(load-theme 'modus-vivendi-deuteranopia)

;; enable the column number in the modeline, together with the line number
(column-number-mode)

(global-auto-revert-mode)

(global-display-line-numbers-mode t)
(global-hl-line-mode)

;; Disable line numbers for some modes
(dolist (mode-hook '(org-mode-hook
                     term-mode-hook
                     shell-mode-hook
                     treemacs-mode-hook
                     eshell-mode-hook
                     pdf-view-mode-hook))
  (add-hook mode-hook
            (lambda () (display-line-numbers-mode -1))))

;; startup position & size of window frame (daemon too)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html
;; also check with (frame-parameters)
(setq default-frame-alist
      (append default-frame-alist
              `((fullscreen . maximized)
                ;; (top . -30)
                ;; (left . 100)
                ;; (width . 150)
                ;; (height . 45)
                (alpha . ,*opacity*)
                ;; FIXME not working on X with i3?
                ;; (mouse-color . "goldenrod3")
                )))


;; Use a column indicator at position 80
(setq-default fill-column 80)
;; show boundaries of content in buffer, like delimiters
(setq-default indicate-buffer-boundaries 'left)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)

;; prefer a sensible abbreviaton of file path as title for GUI frames
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; enable y/n answers in prompts
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))


;; ---- PACKAGES ----

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


;; ---- ADDITIONAL UI/UX ----


(when *is-a-mac*
  ;; if emacs starts up behind all other open applications
  ;; https://emacs.stackexchange.com/a/83155
  (select-frame-set-input-focus (selected-frame))
  ;; gls is part of coreutils (brew install coreutils)
  (setq-default insert-directory-program (or (executable-find "gls") "ls")))


;; vim keybindings
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  ;; :custom
  ;; (evil-lookup-func #'helpful-at-point)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil in other modes
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))


;; ivy and counsel help with emacs commands completion generally (completion-read)
;; https://www.reddit.com/r/emacs/comments/1bz0ekn/company_vertico_corfu_corfusion/
(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1))

;; show annotations next to commands in minibuffer
(use-package ivy-rich
  :init
  (ivy-rich-mode))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; uncomment the following line to have sorting remembered across sessions
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package undo-fu
  :after evil
  :custom
  (evil-undo-system 'undo-fu))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; use this code to enable keycast-mode-line-mode when using doom-modeline
;; https://github.com/tarsius/keycast/issues/7#issuecomment-2387310012
(use-package keycast)

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map ("C-c e" . macrostep-expand)))

;; pop up a completion menu after partial keybinding insertion. built-in, just activate the mode
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode))


(global-prettify-symbols-mode 1)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq prettify-symbols-alist '(("lambda" . ?λ)
                                           ("&&" . ?∧)
                                           ("and" . ?∧)
                                           ("||" . ?∨)
                                           ("or" . ?∨)
                                           ("not" . ?¬)))))

(use-package diminish)
(diminish 'abbrev-mode)

;; then, M-x nerd-icons-install-fonts
(use-package nerd-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-enable-word-count t)
  (doom-modeline-minor-modes t))

;; display enabled minor modes as a small menu in the modeline
;; (use-package minions
;;   :config (minions-mode))

(use-package reveal-in-osx-finder :if *is-a-mac*)

(use-package windresize)

(use-package page-break-lines
  :diminish page-break-lines-mode
  :hook
  (after-init-hook . global-page-break-lines-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside t)
  (sp-use-paredit-bindings)
  (smartparens-global-mode))


(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "dimgrey")
  (set-face-background 'highlight-indentation-current-column-face "darkgrey"))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-status-show-untracked-files 'all))

;; to highlight VC changes in the fringe / show diffs inline
(use-package diff-hl
  :hook
  (after-init-hook . global-diff-hl-mode))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden -0")))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package evil-nerd-commenter)

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (org-mode . hl-todo-mode)
         (text-mode . hl-todo-mode)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; Prevent killing the dashboard buffer as in Doom Emacs or Spacemacs,
  ;; from https://www.emacswiki.org/emacs/ProtectingBuffers.
  (add-hook 'dashboard-after-initialize-hook
            (lambda () (emacs-lock-mode 'kill)))
  (emacs-lock-mode 'kill)
  :custom
  ;; for daemon frames
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-banner-logo-title "Welcome back!")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-navigator t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'projectile)
  ;; default has org bookmarks and agenda, not using atm
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (after-init-hook . global-flycheck-mode)
  :bind
  (:map flycheck-mode-map ("C-c ! !" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :defer t
  :hook
  (python-mode    . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (c-mode         . eglot-ensure)
  :bind
  (:map eglot-mode-map ("C-c r" . eglot-rename)))

(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))


(use-package yasnippet
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (yas-reload-all))


;; (setq xref-search-program 'ripgrep
;;       grep-command "rg -nS --noheading")


(use-package company
  :hook (after-init-hook . company-mode)
  :bind
  ;; (:map company-active-map
  ;;	("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.25)

  :config
  (global-company-mode))

;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode)
;;   :custom
;;   (company-box-doc-delay 0.25))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))


(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package apheleia
  :custom
  (apheleia-log-debug-info t)
  :hook
  (prog-mode . apheleia-mode)
  :config

  ;; add clang-format style argument for C-like languages
  (let ((c-format-def (alist-get 'clang-format apheleia-formatters)))
    (setf (alist-get 'clang-format apheleia-formatters)
          (append c-format-def '("-style" "file")))

    ;; run isort + black on python files
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(isort black))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          '(isort black))))


(use-package treemacs
  :custom
  (treemacs-follow-after-init t)
  (treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc)
  (treemacs-indent-guide-mode nil)

  :config
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode)
    (treemacs-git-mode 'deferred)))

(use-package treemacs-evil
  :after (treemacs evil)
  :bind
  (:map evil-treemacs-state-map ("SPC w l" . treemacs-select-window)))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :after treemacs
  :hook
  (dired-mode . treemacs-icons-dired-enable-once))


(require 'prelude-c)


(use-package python
  :config
  (define-key python-mode-map (kbd "C-<return>") 'python-shell-send-statement))

(use-package uv-mode
  :hook
  (python-mode-hook . uv-mode-auto-activate-hook)
  (python-ts-mode-hook . uv-mode-auto-activate-hook))

;; (use-package ein
;;   :custom
;;   (ein:output-area-inlined-images t)
;;   (ein:worksheet-enable-undo t))


(use-package yaml-mode)


(use-package markdown-mode)


;; TODO maximize opacity in PDF frames
(use-package pdf-tools
  :config
  (pdf-tools-install)

  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  (add-hook 'pdf-view-mode-hook '(lambda () (blink-cursor-mode -1))))


(require 'init-whitespace)
(require 'init-recentf)
(require 'init-uniquify)

(require 'init-ligatures)
(require 'init-documentation)

(require 'init-keyboard)


(use-package gptel)


;; variables configuerd through 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


;; (with-eval-after-load 'tex
;;   (dolist (option TeX-view-program-selection)
;;     (if (equal (car option) 'output-pdf)
;;	(setf (cadr option) "PDF Tools"))))


;; (connection-local-set-profile-variables
;;   'peregrine-remote-path
;;   '((tramp-remote-path . ("/software/software/git/2.33.1-GCCcore-11.2.0-nodocs/bin"
;;                           tramp-default-remote-path))))

;; (connection-local-set-profiles
;;   '(:application tramp :machine "peregrine_thesis") 'peregrine-remote-path)

;; (connection-local-set-profiles
;;   '(:application tramp :machine "peregrine_thesis_gpu") 'peregrine-remote-path)

;; (use-package org
;;   :custom
;;   (org-confirm-babel-evaluate nil)
;;   (org-ellipsis " ▾")
;;   (org-list-allow-alphabetical t)

;;   :config
;;   (require 'ox-beamer)
;;   ;; (require 'ox-latex)
;;   ;; (add-to-list 'org-latex-classes
;;   ;;            '("beamer" "\\documentclass[presentation]{beamer}"
;;   ;;              ("\\section{%s}" . "\\section*{%s}")
;;   ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
;;   ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;   (add-to-list 'org-latex-default-packages-alist '("" "relsize" t))
;;   )

;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (shell . t)))

;; (with-eval-after-load 'org
;;   ;; This is needed as of Org 9.4.4
;;   (require 'org-tempo)
;;   (dolist (org-src-block-abbr '(("el" . "src emacs-lisp")
;;                                 ("sh" . "src shell")))
;;     (add-to-list 'org-structure-template-alist org-src-block-abbr)))

;; (defun efs/org-mode-visual-fill ()
;;   "Center text in .org files."
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   ;; :hook (org-mode . efs/org-mode-visual-fill)
;;   )

;; (use-package org-ref)

;; (setq doc-view-continuous t)

;; ;; to show latex in ein notebooks and other places
;; (use-package math-preview)

;; (define-key org-mode-map (kbd "M-}") nil)

;; ;; auto fill and syntax-check every text mode
;; (dolist (hook '(flyspell-mode
;;                 auto-fill-mode))
;;   (add-hook 'text-mode-hook hook))

;; ;; (setq org-latex-pdf-process (list "latexmk -xelatex -shell-escape -bibtex -f -pdf %f"))
;; (setq org-latex-pdf-process
;;       '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -bibtex -pdf -f %f"))

;; (defun org-babel-tangle-config ()
;;   "Automatically tangle your literate .org config file on save."
;;   (when (string= (buffer-file-name) "<<dot-file-name()>>")
;;     ;; disable confirmation tooltip if enabled
;;     (let ((org-confirm-babel-evaluate nil))
;;       (org-babel-tangle))))

;; (add-hook 'org-mode-hook (lambda ()
;;                            (add-hook 'after-save-hook #'org-babel-tangle-config)))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode))

;; (use-package parrot
;;   :config
;;   (global-set-key (kbd "C-c p") 'parrot-rotate-prev-word-at-point)
;;   (global-set-key (kbd "C-c n") 'parrot-rotate-next-word-at-point)
;;   (global-set-key (kbd "C-c s") 'parrot-start-animation)

;;   (parrot-set-parrot-type 'thumbsup)

;;   (parrot-mode)

;;   (setq parrot-rotate-dict
;;         '((:rot ("yes" "no") :caps t :upcase t))))

;; (add-hook 'parrot-click-hook #'xqz-u/on-python-save)


;; (use-package pkgbuild-mode)
;; (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))

(provide 'init)
;;; init.el ends here
