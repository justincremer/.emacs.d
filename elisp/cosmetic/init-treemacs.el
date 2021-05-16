;; init-treemacs.el --- Initialize treemacs configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)

(use-package treemacs
  :commands (treemacs-follow-mode
			 treemacs-filewatch-mode
			 treemacs-fringe-indicator-mode
			 treemacs-git-mode)
  :bind (([f2]        . treemacs)
		 ("M-o"       . treemacs-select-window)
		 ("C-x 1"     . treemacs-delete-other-windows)
		 ("C-x t 1"   . treemacs-delete-other-windows)
		 ("C-x t t"   . treemacs)
		 ("C-x t b"   . treemacs-bookmark)
		 ("C-x t C-t" . treemacs-find-file)
		 ("C-x t M-t" . treemacs-find-tag)
		 :map treemacs-mode-map
		 ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
		treemacs-missing-project-action  'remove
		treemacs-is-never-other-window   t
		treemacs-sorting                 'alphabetic-asc
		treemacs-follow-after-init       t
		treemacs-width                   30
		treemacs-no-png-images           nil)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
			   (not (null (executable-find "python3"))))
	(`(t . t)
	 (treemacs-git-mode 'deferred))
	(`(t . _)
	 (treemacs-git-mode 'simple)))

  (use-package treemacs-projectile
	:after (treemacs projectile)
	:ensure t)

  (use-package treemacs-magit
	:after (treemacs magit)
	:ensure t)

  (use-package treemacs-perspective
	:after (treemacs perspective)
	:ensure t
	:config
	(treemacs-set-scope-type 'Perspectives))

  (use-package treemacs-icons-dired
	:after (treemacs dired)
	:ensure t
	:config
	(treemacs-icons-dired-mode)))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
