;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; Code:

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

(set-face-attribute 'default nil :family "M+ 1mn" :height 115)
(set-language-environment "Japanese")


;;; Commentary:
;; 

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless package-archive-contents
	(package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(eval-when-compile
	(require 'use-package))

(use-package cyberpunk-theme
	:ensure t
	:init
	(load-theme 'cyberpunk t))

(use-package company
	:ensure t
	:init
	(global-company-mode))

(use-package counsel
	:ensure t
	:init
	(counsel-mode 1)
	(ivy-mode 1)
	:config
	(global-set-key "\C-s" 'swiper)
	(global-set-key (kbd "C-c C-r") 'ivy-resume)
	(global-set-key (kbd "<f6>") 'ivy-resume)
	(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
	(global-set-key (kbd "C-c g") 'counsel-git)
	(global-set-key (kbd "C-c j") 'counsel-git-grep)
	(global-set-key (kbd "C-c k") 'counsel-ag)
	(global-set-key (kbd "C-x l") 'counsel-locate)
	(global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

(use-package rainbow-delimiters
	:ensure t
	:init
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	:config
	(set-face-attribute 'rainbow-delimiters-unmatched-face nil
											:foreground 'unspecified
											:inherit 'error
											:strike-through t))

(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode))

(use-package yasnippet
	:ensure t
	:init
	(yas-global-mode 1))

(use-package yasnippet-snippets
	:ensure t
	:requires yasnippet
	:after yasnippet)

(use-package anzu
	:disabled
	:ensure t
	:config
	(global-anzu-mode +1))

(use-package undo-tree
	:ensure t
	:config
	(global-undo-tree-mode))

(use-package smartparens
	:disabled
	:ensure t
	:hook prog-mode)

(use-package web-mode
	:ensure t
	:mode ("\\.html?\\'" "\\.php\\'")
	:config
	(setq web-mode-markup-indent-offset 2)
	(web-mode-use-tabs))

(use-package magit
	:ensure t)

(use-package markdown-mode
	:ensure t)

(use-package flyspell
	:ensure t
	:hook (((text-mode markdown-mode) . flyspell-mode)
				 (prog-mode . flyspell-prog-mode)))

(provide 'init)

;;; init.el ends here
