;; init-treemacs.el --- Initialize treemacs configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs configurations.
;;

;;; Code:

(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-perspective
  :after (treemacs perspective)
  :ensure t
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package lsp-treemacs
  :after (lsp treemacs)
  :ensure t)

(xiu/leader-key-def
  "tt" '(treemacs :which-key "treemacs"))

(provide 'init-treemacs)

;;; init-treemacs.el ends here
