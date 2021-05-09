;; init-markdown.el --- Initialize markdown configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown configurations.
;;

;;; Code:

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun xiu/set-markdown-header-font-sizes ()
	(dolist (face '((markdown-header-face-1 . 1.2)
					(markdown-header-face-2 . 1.1)
					(markdown-header-face-3 . 1.0)
					(markdown-header-face-4 . 1.0)
					(markdown-header-face-5 . 1.0)))
	  (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
  (defun xiu/markdown-mode-hook ()
	(xiu/set-markdown-header-font-sizes))
  (add-hook 'xiu/markdown-mode-hook 'xiu/markdown-mode-hook))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(provide 'init-markdown)

;;; init-markdown.el ends here
