;; init-web.el --- Initialize web development configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Web development configurations.
;;

;;; Code:

(require 'init-custom)

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(use-package scss-mode
  :init
  (setq scss-compile-at-save nil))

(use-package less-css-mode)

(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
		 ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
				("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extra-mode)
		 (js2-mode . js2-highlight-unused-variables-mode))
  :config
  (with-eval-after-load 'flycheck
	(when (or (executable-find "eslint_d")
			  (executable-find "eslint")
			  (executable-find "jshint"))
	  (setq js2-mode-show-strict-warnings nil))
	(when (executable-find "eslint_d")
	  ;; https://github.com/mantoni/eslint_d.js
	  ;; npm -i -g eslint_d
	  (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
	:diminish
	:hook (js2-mode . js2-refactor-mode)
	:config (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package typescript-mode
  :mode ("\\.tsx?\'" . typescript-mode))

(defun xiu/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
			 (string-match "\\.tsx?\\'" buffer-file-name))
	(tide-setup)
	(tide-hl-identifier-mode)))

(use-package tide
  :hook (web-mode . xiu/activate-tide-mode)
  :ensure t)

(use-package web-mode
  :mode "\\.\\(html?\\|[jt]sx\\|vue\\|svelte\\)$"
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4))

(use-package rjsx-mode
  :after (js2-mode)
  :mode ("\\.[jt]sx\'" . rjsx-mode))

(use-package mocha
  :after (js2-mode typescript-mode)
  :hook ((js2-mode . jest-minor-mode)
		 (typescript-mode . jest-minor-mode))
  :config (use-package mocha-snippets))

(use-package jest
  :after (js2-mode typescript-mode)
  :hook ((js2-mode typescript-mode)
		 . jest-minor-mode))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(use-package prettier-js
  :diminish
  :hook ((js-mode typescript-mode js2-mode rjsx-mode  json-mode web-mode css-mode scss-mode html-mode)
		 .
		 prettier-js-mode))

;; (use-package emmet-mode
;;   :hook ((web-mode . emmet-mode)
;;		 (css-mode . emmet-mode)))

(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode) . skewer-mode)
		 (css-mode . skewer-css-mode)
		 (web-mode . skewer-html-mode)
		 (html-mode . skewer-html-mode))
  :init
  (with-eval-after-load 'skewer-css
	(diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
	(diminish 'skewer-html-mode))
  :config (use-package impatient-mode))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
	:diminish
	:hook (restclient-mode . restclient-test-mode))

  (with-eval-after-load 'company
	(use-package company-restclient
	  :defines company-backends
	  :init (add-to-list 'company-backends 'company-restclient))))

(provide 'init-web)

;;; init-web.el ends here
