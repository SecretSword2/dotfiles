(setq custom-file (locate-user-emacs-file "custom"))

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda ()
													 (garbage-collect)))

(if (<= emacs-major-version 26)
		(global-linum-mode t)
	(global-display-line-numbers-mode))

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

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
										:family "IBM Plex Mono"
										;; :family "Roboto Mono"
										:height (if (eq system-type 'darwin) 150 130))
(set-fontset-font t 'unicode (font-spec :family "Cica"))

(set-language-environment "Japanese")

(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

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
	:init
	(load-theme 'kaolin-valley-dark t)
	)

;; (use-package srcery-theme
;; 	:disabled
;; 	:straight t
;; 	:init
;; 	(load-theme 'srcery t)
;; 	)

;; (use-package doom-modeline
;;   :straight t
;; 	:init
;; 	(doom-modeline-mode 1))

;; And everything else
(use-package ace-window
	:straight t
	:bind
	("M-o" . ace-window))

(use-package aggressive-indent
	:straight t
	:hook
	(c++-mode . aggressive-indent-mode))

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
	:init
	(use-package company-lsp
		:straight t
		:config
		(push 'company-lsp company-backends)
		:custom
		(company-lsp-async t)
		(company-lsp-enable-snippet t)
		(company-lsp-enable-recompletion t))
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
	:diminish
	:config
	(use-package amx
		:straight t)
	(use-package ivy
		:straight t
		:custom
		(ivy-use-virtual-buffers t)
		(enable-recursive-minibuffers t)
		(ivy-use-selectable-prompt t)
		:config
		(ivy-mode 1))
	(use-package ivy-rich
		:straight t
		:after ivy
		:config
		(ivy-rich-mode t))
  (counsel-mode 1)
	:bind
	(("M-s M-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-M-r" . counsel-recentf)
	 ("C-x C-b" . counsel-ibuffer)
	 ("C-M-f" . counsel-ag))
	)

(use-package diff-hl
	:straight t
	:config
	(global-diff-hl-mode)
	(diff-hl-margin-mode))

(use-package google-c-style
	:straight (google-c-style
						 :type git
						 :host github
						 :repo "google/styleguide"
						 :branch "gh-pages")
	:hook
	(c++-mode . google-set-c-style)
	(c-mode-common-hook . google-set-c-style)
	(c++-mode-common-hook . google-set-c-style)
	(c-mode-common-hook . google-make-newline-indent))

(use-package highlight-indent-guides
	:straight t
	:delight
	:hook
	((emacs-lisp-mode) . highlight-indent-guides-mode)
	:custom
	(highlight-indent-guides-auto-enabled t)
	(highlight-indent-guides-responsive t)
	(highlight-indent-guides-method 'character))

(use-package eglot
  :straight t)

(use-package projectile
	:straight t
	:config
	(defun projectile-project-find-function (dir)
		(let* ((root (projectile-project-root dir)))
			(and root
					 (cons 'transient root))))
	(with-eval-after-load
			'project
		(add-to-list
		 'project-find-functions
		 'projectile-project-find-function)))

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
