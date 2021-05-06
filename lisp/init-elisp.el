;; init-elisp.el --- Initialize elisp configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Elisp configurations.
;;

;;; Code:

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(xiu/leader-key-def
  "e"  '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "er" '(eval-region :which-key "eval region"))

(provide 'init-elisp)

;;; init-elisp.el ends here
