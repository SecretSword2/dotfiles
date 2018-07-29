(setq custom-file (locate-user-emacs-file "custom"))

(global-linum-mode t)

(setq inhibit-startup-screen t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(show-paren-mode 1)

(setq ispell-program-name "aspell")

(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(set-face-attribute 'default nil :family "Source Han Code JP" :height 110)
(set-language-environment "Japanese")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
										(not (gnutls-available-p))))
			 (proto (if no-ssl "http" "https")))
	(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
	(when (< emacs-major-version 24)
		(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless package-archive-contents
	(package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(eval-when-compile
	(require 'use-package))

(use-package ido
	:init
	(ido-mode 1)
	(ido-everywhere 1)
	(setq ido-enable-flex-matching t))

(use-package cyberpunk-theme
	:ensure t
	:init
	(load-theme 'cyberpunk t))

(use-package company
	:ensure t
	:init
	(global-company-mode))
;; (use-package auto-complete
;; 	:ensure t
;; 	:config
;; 	(ac-config-default)
;; 	(ac-flyspell-workaround)
;; 	(add-to-list 'ac-modes 'tex-mode))

(use-package flycheck
	:ensure t)

(use-package anzu
	:ensure t
	:config
	(global-anzu-mode +1))

(use-package undo-tree
	:ensure t
	:config
	(global-undo-tree-mode))

(use-package smartparens
	:ensure t
	:hook (prog-mode-hook . smartparens-mode))

;; (use-package helm
;; 	:ensure t
;; 	:config
;; 	(helm-mode 1)
;; 	(helm-autoresize-mode t)
;; 	:bind
;; 	(("M-x" . helm-M-x)
;; 	 ("M-y" . helm-show-kill-ring)
;; 	 ("C-x C-f" . helm-find-files)
;; 	 ("C-x b" . helm-buffers-list)))

(use-package smex
	:bind
	(("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

(use-package web-mode
	:mode (("\\.html?\\'" . web-mode))
	:config
	(setq web-mode-markup-indent-offset 2)
	(web-mode-use-tabs))

(use-package magit
	:ensure t)

(use-package markdown-mode
	:ensure t)

(use-package flyspell
	:init
	(add-hook 'prog-mode-hook 'flyspell-prog-mode)
	(add-hook 'text-mode-hook 'flyspell-mode)
	(add-hook 'markdown-mode-hook 'flyspell-mode)
	:commands
	(flyspell-prog-mode flyspell-mode))

 (use-package auctex
	:init
	(add-hook 'LaTeX-mode-hook
						(lambda ()
							(setq indent-tabs-mode t))))
