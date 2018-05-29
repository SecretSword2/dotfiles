
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/init-packages")
; MELPA BEGIN
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("9fe1540491fcf692b8c639a3abacd32b29233bc4cb834a12a0fd1e01cbd0a128" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" default)))
 '(package-selected-packages
	 (quote
		(rainbow-delimiters smartparens anzu expand-region undo-tree flycheck markdown-mode cyberpunk-theme color-theme-modern auto-complete async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
; MELPA END

; Look and feel
;; Show numbers
(global-linum-mode t)
;; Theme
(load-theme 'cyberpunk)
;; Disable welocme screen
(setq inhibit-startup-screen t)
;; Set tab width
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'tex-mode)

(global-anzu-mode +1)

(global-undo-tree-mode)

(add-hook 'prog-mode-hook #'smartparens-mode)

(show-paren-mode 1)

(setq ispell-program-name "aspell")
