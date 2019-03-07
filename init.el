;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;; Code:

(setq custom-file (locate-user-emacs-file "custom"))

(global-linum-mode t)

(setq ring-bell-function 'ignore)
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

(use-package spaceline
	:straight t
	:config
	(spaceline-emacs-theme)
	(spaceline-helm-mode))

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

(use-package helm
	:straight t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y"   . helm-show-kill-ring)
         ("C-c m"   . helm-man-woman)
         ("C-c o"   . helm-occur)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char))
	:config
	(helm-mode 1)
	(helm-autoresize-mode t)
	(setq helm-M-x-fuzzy-match t)
	(setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t))

(use-package helm-swoop
	:straight t
	:requires helm
	:config
	(setq helm-swoop-use-fuzzy-match t))

(use-package anzu
	:straight t
	:diminish anzu-minor-mode
	:config
	(global-anzu-mode +1)
	(setq anzu-cons-mode-line-p nil))

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
