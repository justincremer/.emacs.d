;; init-flycheck.el --- Initialize flycheck configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
