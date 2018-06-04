;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/init-packages")
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Show numbers
(global-linum-mode t)
;; Theme
(load-theme 'cyberpunk)
;; Disable welocme screen
(setq inhibit-startup-screen t)
;; Set tab width
(setq-default tab-width 2)
(setq-default indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(eval-when-compile
	(require 'use-package))

(use-package cyberpunk-theme
	:ensure t
	:config
	(load-theme 'cyberpunk))

(use-package auto-complete
	:ensure t
	:config
	(ac-config-default)
	(add-to-list 'ac-modes 'tex-mode))

(use-package flycheck
	:ensure t)

(use-package anzu
	:ensure t
	:config
	(global-anzu-mode +1))

(use-package undo-tree
	:config
	(global-undo-tree-mode))

(use-package smartparens
	:hook (prog-mode-hook . smartparens-mode))

(show-paren-mode 1)

;; (require 'magit)

(setq ispell-program-name "aspell")

(use-package web-mode
	:mode (("\\.html?\\'" . web-mode))
	:config
	(setq web-mode-markup-indent-offset 2)
	(web-mode-use-tabs))
