;; init-projectile.el --- Initialize projectile configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Projectile configurations.
;;

;;; Code:

(defun xiu/switch-project-hook ()
  "Switch to a workspace with the project name."
  (persp-switch (projectile-project-name)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/development")
	(setq projectile-project-search-path '("~/development")))
  (setq projectile-switch-project-action #'xiu/switch-project-hook))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-f" . counsel-projectile-find-file))
  :config (counsel-projectile-mode))

(xiu/leader-key-def
  "pp"  'counsel-projectile
  "pf"  'counsel-projectile-find-file
  "ps"  'counsel-projectile-switch-project
  "pF"  'counsel-projectile-rgc
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)

(provide 'init-projectile)

;;; init-projectile.el ends here
