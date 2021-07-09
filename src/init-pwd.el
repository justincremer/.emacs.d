;;; init-pwd.el --- Initialize password manager        -*- lexical-binding: t no-byte-compile: t-*-

;;; Commentary:
;;
;; Initialize pwd
;;

;;; Code:

(use-package password-store
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(xiu/leader-key-def
  "ap" '(:ignore t :which-key "pass")
  "app" 'password-store-copy
  "api" 'password-store-insert
  "apg" 'password-store-generate)

(provide 'init-pwd)

;;; init-pwd.el ends here
