;; init-clisp.el --- Initialize clisp configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Clisp configurations.
;;

;;; Code:

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq auto-mode-alist (append '(("\\.lisp" . common-lisp-mode))
							  auto-mode-alist))

(use-package sly
  :mode "\\.lisp\\'")

(provide 'init-clisp)

;;; init-clisp.el ends here
