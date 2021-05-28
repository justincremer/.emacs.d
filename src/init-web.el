;; init-web.el --- Initialize web development configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Web development configurations.
;;

;;; Code:

(require 'init-custom)

(use-package web-mode
  :mode ("\\.\\(html?\\|[jt]sx\\|vue\\|svelte\\)$" . web-mode)
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66ccff"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :config
  (setq web-mode-markup-indent-offset 4
		web-mode-css-indent-offset 2
		web-mode-code-indent-offset 4))

(use-package js2-mode
  :disabled
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
		 ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
				("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extra-mode)
		 (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;;   (use-package js2-refactor
  ;;	:diminish
  ;;	:hook (js2-mode . js2-refactor-mode)
  ;;	:config (js2r-add-keybindings-with-prefix "C-c C-m")))

  (with-eval-after-load 'flycheck
	(when (or (executable-find "eslint_d")
			  (executable-find "eslint")
			  (executable-find "jshint"))
	  (setq js2-mode-show-strict-warnings nil))
	(when (executable-find "eslint_d") ;; yarn global add eslint_d
	  (setq flycheck-javascript-eslint-executable "eslint_d"))))

(use-package rjsx-mode
  :after (js2-mode)
  :mode ("\\.\\(jsx\\|tsx\\)$" . rjsx-mode))

;; (use-package typescript-mode
;;   :mode ("\\.tsx?\\'" . typescript-mode))
;; :config
;; (with-eval-after-load 'flycheck
;;   (when (executable-find "eslint_d") ;; yarn global add eslint_d
;;	(setq flycheck-javascript-eslint-executable "eslint_d"))))

(defun xiu/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (interactive)
  (when (and (stringp buffer-file-name)
			 (string-match "\\.tsx?\\'" buffer-file-name))
	(tide-setup)
	(tide-hl-identifier-mode)))

(use-package tide
  ;; :disabled
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (web-mode . xiu/activate-tide-mode))

;; (use-package mocha
;;   :after (js2-mode typescript-mode)
;;   :hook ((js2-mode . jest-minor-mode)
;;		 (typescript-mode . jest-minor-mode))
;;   :config (use-package mocha-snippets))

;; (use-package jest
;;   :after (js2-mode typescript-mode)
;;   :hook ((js2-mode typescript-mode) . jest-minor-mode))

;; Install: npm -g install prettier
(use-package prettier-js
  :hook (web-mode . prettier-js-mode))

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(use-package scss-mode
  :init
  (setq scss-compile-at-save nil))

(use-package less-css-mode)

(use-package skewer-mode
  :disabled
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
  :disabled
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
;
