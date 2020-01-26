(setq custom-file (locate-user-emacs-file "custom"))

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda ()
                           (garbage-collect)))

(if (<= emacs-major-version 26)
    (global-linum-mode t)
  (global-display-line-numbers-mode))

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)

(setq-default tab-width 2)
(setq-default indent-tabs-mode t)
(defvaralias
  'c-basic-offset
  'tab-width
  )
(defvaralias
  'cperl-indent-level
  'tab-width
  )

(electric-indent-mode t)
(electric-pair-mode t)
(show-paren-mode 1)

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    ;; :family "Roboto Mono"
                    :height (if (eq system-type 'darwin) 140 120))
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Cica"))
(set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji") nil 'prepend)

(set-language-environment "Japanese")

(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq-default ispell-program-name "~/.emacs.d/aspell.cmd")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load
   bootstrap-file
   nil
   'nomessage))

(straight-use-package 'use-package)

;; Look and feel

(use-package all-the-icons
  :straight t)

(use-package delight
  :straight t)

(use-package dracula-theme
  :straight t)

(use-package kaolin-themes
  :straight t
  ;; :init
  ;; (load-theme 'kaolin-valley-dark t)
  )

(use-package spacemacs-theme
  :defer t
  :straight t
  :init
  (load-theme 'spacemacs-light t)
  )

;; (use-package srcery-theme
;;  :disabled
;;  :straight t
;;  :init
;;  (load-theme 'srcery t)
;;  )

;; (use-package doom-modeline
;;   :straight t
;;  :init
;;  (doom-modeline-mode 1))

;; And everything else
(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window))

(use-package aggressive-indent
  :straight t
  ;; :hook
  ;; (c++-mode . aggressive-indent-mode)
  )

(use-package anzu
  :straight t
  :delight
  :config
  (global-anzu-mode 1))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package company
  :straight t
  :config
  (global-company-mode)
  ;; (company-tng-configure-default)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd" "--clang-tidy")))
  ;; :hook
  ;; (before-save . eglot-format-buffer)
  :custom
  (eglot-autoreconnect t)
  )

(use-package flycheck
  :straight t
  )

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1))

;; (use-package google-c-style
;;   :straight (google-c-style
;;              :type git
;;              :host github
;;              :repo "google/styleguide"
;;              :branch "gh-pages")
;;   :hook
;;   (c++-mode . google-set-c-style)
;;   (c-mode-common-hook . google-set-c-style)
;;   (c++-mode-common-hook . google-set-c-style)
;;   (c-mode-common-hook . google-make-newline-indent))

(use-package helm
  :straight t
  :config
  (helm-mode 1)
  :custom
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x C-r" . helm-recentf))

(use-package highlight-indent-guides
  :straight t
  :delight
  :hook
  ((emacs-lisp-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package magit
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  :config
  (use-package helm-projectile
    :straight t
    :requires helm
    )
  :bind
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute	'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))

(use-package undo-tree
  :straight t
  :delight
  :config
  (global-undo-tree-mode))

(use-package verilog-mode
  :straight (verilog-mode
             :type git
             :host github
             :repo "veripool/verilog-mode"
             :branch "master")
  :custom
  (indent-tabs-mode nil)
  (verilog-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-declaration 2)
  (verilog-indent-level-behavioral  2)
  (verilog-case-indent 2)
  (verilog-auto-newline nil)
  (verilog-auto-indent-on-newline t)
  (verilog-tab-always-indent t)
  (verilog-auto-endcomments t)
  (verilog-minimum-comment-distance 20)
  (verilog-indent-begin-after-if t)
  (verilog-auto-lineup '(all))
  )

(use-package volatile-highlights
  :straight t
  :delight
  :hook
  (after-init . volatile-highlights-mode))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.php\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (web-mode-use-tabs))

(use-package which-key
  :straight t
  :delight
  :hook
  (after-init . which-key-mode))

(use-package yasnippet
  :delight
  :straight t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :requires yasnippet
  :after yasnippet)
