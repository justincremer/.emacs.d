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
  (setq dashboard-set-heading-icons t
		dashboard-set-file-icons t
		dashboard-center-content t
		dashboard-show-shortcuts t
		dashboard-set-navigator t
		dashboard-startup-banner 'logo
		dashboard-banner-logo-titlfe (format "Welcome to Xiumacs %s" (xiu/emacs-version-number))
		dashboard-init-info (xiu/load-time-message)
		dashboard-items '((projects . 20)
						  (bookmarks . 5)
						  (agenda . 5)
						  (recents . 5))
		dashboard-footer-icon (all-the-icons-octicon "dashboard"
													 :height 1.1
													 :v-adjust -0.05
													 :face 'font-lock-keyword-face)
		initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package dashboard
  :after (projectile)
  :ensure t
  :config
  (xiu/dashboard-config))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
