;; init-tabs.el --- Initialize tabs configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Centaur tabs configurations.
;;

;;; Code:

(defun xiu/centaur-tabs-mode--config ()
  "Configuration for `centaur-tabs-mode'."
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-bar 'over
		centaur-tabs-set-icons t
		centaur-tabs-set-gray-out-icons 'buffer
		centaur-tabs-height 24
		centaur-tabs-set-modified-marker t
		centaur-tabs-modifier-marker "*"))

(use-package centaur-tabs
  :ensure t
  :init (centaur-tabs-mode t)
  :bind (("C-<tab>" . centaur-tabs-forward-tab)
		 ("C-<iso-lefttab>" . centaur-tabs-backward-tab))
  :hook
  (projectile-mode . centaur-tabs-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  :config (xiu/centaur-tabs-mode--config))

(provide 'init-tabs)

;;; init-tabs.el ends here
