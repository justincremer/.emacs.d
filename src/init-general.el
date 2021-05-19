;; init-general.el --- Initialize general configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; General configurations.
;;

;;; Code:

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-unset-key (kbd "C-/"))

(use-package general
  :config
  (general-create-definer xiu/leader-key-def
	:prefix "C-/"
	:global-prefix "C-/")
  (general-create-definer ctrl-c-keys
	:prefix "C-c"))

(xiu/leader-key-def
 "s" '(ace-swap-window :which-key "swap windows")
 "t" '(:ignore t :which-key "toggle")
 "tf" '(counsel-load-theme :which-key "choose theme"))

(provide 'init-general)

;;; init-general.el ends here
