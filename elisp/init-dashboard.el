;; init-dashboard.el --- Initialize basic configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(defun xiu/emacs-version-number ()
  "Displays current Emacs version number as a string."
  (caddr (split-string (version))))

(defun xiu/load-time-message ()
  "Return a string displaying the init time and garbage colletions."
  (format "Loaded in %s with %d garbage collections"
		  (format "%.2f seconds"
				  (float-time
				   (time-subtract after-init-time before-init-time)))
		  gcs-done))

(defun xiu/dashboard-config ()
  "Configuration for startup `dashboard'."
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title
		(format "Welcome to Xiumacs %s" (xiu/emacs-version-number)))
  (setq dashboard-init-info (xiu/load-time-message))
  (setq dashboard-footer-icon
		(all-the-icons-octicon "dashboard"
							   :height 1.1
							   :v-adjust -0.05
							   :face 'font-lock-keyword-face))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items'((bookmarks . 5)
						 (recents . 5)
						 (projects . 20)
						 (agenda . 5))))
;; (setq dashboard-projects-switch-function 'projectile-persp-switch-project)
;; (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name))

(use-package dashboard
  :after projectile
  :ensure t
  :config
  (xiu/dashboard-config))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
