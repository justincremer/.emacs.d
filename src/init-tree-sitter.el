;;; init-tree-sitter.el --- Initialize Tree-Sitter configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Tree-Sitter configurations.
;;

;;; Code:

(use-package tree-sitter
  :ensure t
  :straight t
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :straight t)

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here
