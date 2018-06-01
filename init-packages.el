(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    0  (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

; list the packages you want
(setq package-list
      '(cyberpunk-theme
	auto-complete
	flycheck
	undo-tree
	anzu
	smartparens
	magit
	web-mode
	))


; activate all the packages
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
