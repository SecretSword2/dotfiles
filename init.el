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

(set-face-attribute 'default nil :family "M+ 1mn" :height 115)
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
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package cyberpunk-theme
	:straight t
	:init
	(load-theme 'cyberpunk t))

(use-package powerline
	:straight t
	:init
	(powerline-default-theme))

(use-package yasnippet
	:straight t
	:init
	(yas-global-mode 1))

(use-package yasnippet-snippets
	:straight t
	:requires yasnippet
	:after yasnippet)

(use-package company
	:requires yasnippet
	:straight t
	:init
	(global-company-mode)
	(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
	(defun company-mode/backend-with-yas (backend)
		(if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
				backend
			(append (if (consp backend) backend (list backend))
							'(:with company-yasnippet))))
	(defun set-yas-as-company-backend ()
		(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
	(add-hook 'company-mode-hook 'set-yas-as-company-backend)
	:config
	(setq company-show-numbers nil)
	(setq company-transformers '(company-sort-by-backend-importance))
	(setq company-idle-delay 0)
	(setq company-minimum-prefix-length 2)
	(setq company-selection-wrap-around t)
	(setq company-dabbrev-downcase nil))

(use-package counsel
	:straight t
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
	:straight t
	:init
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	:config
	(set-face-attribute 'rainbow-delimiters-unmatched-face nil
											:foreground 'unspecified
											:inherit 'error
											:strike-through t))

(use-package flycheck
	:straight t
	:init
	(global-flycheck-mode))

(use-package undo-tree
	:straight t
	:config
	(global-undo-tree-mode))

(use-package web-mode
	:mode ("\\.html?\\'" "\\.php\\'")
	:config
	(setq web-mode-markup-indent-offset 2)
	(web-mode-use-tabs))

(use-package magit
	:straight t)

(use-package markdown-mode
	:straight t)

(use-package flyspell
	:straight t
	:hook (((text-mode markdown-mode) . flyspell-mode)
				 (prog-mode . flyspell-prog-mode)))

(use-package groovy-mode
	:straight t)

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
