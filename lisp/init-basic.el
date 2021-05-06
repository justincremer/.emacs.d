;; init-basic.el --- Initialize basic configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Basic configurations.
;;

;;; Code:

;; Sets a buffered backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
	  backup-by-copying t
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)

(setq disabled-command-function nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
	  url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings out of sight (thanks Ambrevar)
(setq custom-file
	  (if (boundp 'server-socket-dir)
		  (expand-file-name "custom.el" server-socket-dir)
		(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(provide 'init-basic)

;;; init-basic.el ends here
