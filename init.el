(setq custom-file (locate-user-emacs-file "custom"))

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(if (<= emacs-major-version 26)
		(global-display-line-numbers-mode)
	(global-linum-mode t))

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(show-paren-mode 1)

;; (setq ispell-program-name "docker.exe run --rm -it -v ${PWD}:/workdir hunspell")

;;(set-face-attribute 'default nil :family "M+ 1mn" :height 115)
(set-face-attribute 'default nil :family "Ocami" :height 140)
;; (set-fontset-font (frame-parameter nil 'font)
;; 									'japanese-jisx0208
;; 									(font-spec :family "BIZ UDGothic" :size 15)))

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

;; Themes
(use-package dracula-theme
	:straight t
	:init
	(load-theme 'dracula t))

(use-package all-the-icons
	:straight t
  :custom
  (all-the-icons-scale-factor 0.83))

(use-package doom-modeline
  :straight t
	:init
	(doom-modeline-mode 1))

(use-package projectile
	:straight t
  :config
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  )

(use-package eglot
	:straight t
	:config
	(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
	:bind
	(:map eglot-mode-map
				("C-c C-d" . eglot-help-at-point)
				("C-c C-r" . eglot-code-actions))
	:hook
	((c-mode-common . eglot-ensure))
	)

(use-package yasnippet
	:delight yas-minor-mode
	:straight t
	:init
	(yas-global-mode 1))

(use-package yasnippet-snippets
	:straight t
	:requires yasnippet
	:after yasnippet)

(use-package company
	:straight t
	:delight company-mode
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
	:custom
	(company-begin-commands '(self-insert-command))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t)
	(company-transformers '(company-sort-by-backend-importance))
	(company-selection-wrap-around t)
	(company-dabbrev-downcase nil))

(use-package company-box
	:straight t
  :after (company all-the-icons)
  :hook ((company-mode . company-box-mode))
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package hydra
	:straight t)

(use-package counsel
	:straight t
	:config
  (counsel-mode 1))

(use-package ivy
	:straight t
	:config
	(setq ivy-use-virtual-buffers t)
	(setq enable-recursive-minibuffers t)
	(setq ivy-use-selectable-prompt t)
	(ivy-mode 1)
	:bind
	(("M-s M-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-M-r" . counsel-recentf)
	 ("C-x C-b" . counsel-ibuffer)
	 ("C-M-f" . counsel-ag)))

(use-package ivy-hydra
	:straight t
	:after (ivy hydra)
	:requires (ivy hydra))

(use-package anzu
	:straight t
	:diminish anzu-mode
	:config
	(global-anzu-mode +1))

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

(use-package flycheck-vale
	:straight t
	:init
	(flycheck-vale-setup))

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

(use-package groovy-mode
	:straight t)

(provide 'init)
