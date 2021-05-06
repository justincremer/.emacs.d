;; init-web.el --- Initialize web development configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Web development configurations.
;;

;;; Code:

(use-package web-mode
  :straight t
  :mode (("\\.jsx?\\'" . web-mode)
		 ("\\.tsx?\\'" . web-mode)
		 ("\\.html?\\'" . web-mode)
		 ("\\.ejs\\'" . web-mode)
		 ("\\.vue\\'" . web-mode)
		 ("\\.svelte\\'" . web-mode)
		 ("\\.php\\'" . web-mode)
		 ("\\.json\\'" . web-mode))
  :commands web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :config
  (setq-default web-mode-code-indent-offset 4)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2)
  (setq web-mode-content-types-alist
		'(("jsx" . "\\.js[x]?\\'")
		  ("tsx" . "\\.ts[x]?\\'"))))

(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

(defun xiu/set-js-indentation ()
  "Set the default indentation width for newlines."
  (setq js-indent-level 4)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 4))

(defun xiu/js2-config ()
  "Set environment variables for js2-mode."
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'xiu/set-js-indentation)
  (add-hook 'json-mode-hook #'xiu/set-js-indentation))

(use-package js2-mode
  :straight t
  :mode "\\.jsx?\\'"
  :config
  (xiu/js2-config))

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\'"
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
		 (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
		 (css-mode . emmet-mode)))

(provide 'init-web)

;;; init-web.el ends here
