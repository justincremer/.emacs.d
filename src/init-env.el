;;; init-env.el --- Initialize environment        -*- lexical-binding: t no-byte-compile: t-*-

;;; Commentary:
;;
;; Initialize environment
;;

;;; Code:

(if (not (version< emacs-version "28.0.0"))
	"Disables obsolete function messages in `native-comp'."
  (define-advice define-obsolete-function-alias
	  (:filter-args (ll) fix-obsolete)
	(let ((obsolete-name (pop ll))
		  (current-name (pop ll))
		  (when (if ll (pop ll) "1"))
		  (docstring (if ll (pop ll) nil)))
	  (list obsolete-name current-name when docstring))))

;; Enables disabled commands without a prompt
(setq disabled-command-function nil)

;; Localizes auto-backups under a single directory
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
	  backup-by-copying t
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)

;; Keep customization settings out of sight (thanks Ambrevar)
(setq custom-file
	  (if (boundp 'server-socket-dir)
		  (expand-file-name "custom.el" server-socket-dir)
		(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Change the user-emacs-directory to keep unwanted things out of config directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
	  url-history-file (expand-file-name "url/history" user-emacs-directory))

(provide 'init-env)

;;; init-env.el ends here
