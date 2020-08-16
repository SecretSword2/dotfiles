(setq custom-file (locate-user-emacs-file "custom"))

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda ()
                           (garbage-collect)))

;; display line numbers
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      ))

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
                    :height (if (eq system-type 'darwin) 140 120))
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "MotoyaLMaru"))
;; 日本語のサンプルです
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

(use-package delight
  :straight t)

(use-package kaolin-themes
  :straight t
  :init
  (load-theme 'kaolin-valley-dark t)
  )

(use-package spacemacs-theme
  :straight t
  )

(use-package srcery-theme
  :straight t
 )

;; And everything else

(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window))

(use-package aggressive-indent
  :straight t
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

(use-package counsel
  :straight t
  :delight
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-r" . counsel-recentf)
  ("C-x C-f" . counsel-find-file)
  )

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-package flycheck
  :straight t
  )

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1))

(use-package highlight-indent-guides
  :straight t
  :delight
  :hook
  ((emacs-lisp-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package lsp-mode
  :straight t
  :custom
  ;; enable log only for debug
  (lsp-log-io nil)
  ;; turn off for better performance
  (lsp-enable-symbol-highlighting nil)
  (lsp-auto-configure t)

  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (create-lockfiles nil)
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  )

(use-package lsp-treemacs
  :straight t
  :requires (lsp-mode tree-sitter-langs)
  :init
  (lsp-treemacs-sync-mode 1)
  )

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :custom (scroll-margin 0)
  :hook   (lsp-mode . lsp-ui-mode))

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
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

(use-package tree-sitter
  :straight (tree-sitter
             :host github
             :repo "ubolonton/emacs-tree-sitter"
             :files ("lisp/*.el")))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs
             :host github
             :repo "ubolonton/emacs-tree-sitter"
             :files ("langs/*.el" "langs/queries"))
  :requires tree-sitter
  :hook
  (javascript-mode . tree-sitter-mode)
  (javascript-mode . tree-sitter-hl-mode)
  )

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
  :straight t
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
