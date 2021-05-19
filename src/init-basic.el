;;; init-basic.el --- Initialize basic configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Basic configurations.
;;

;;; Code:

;; Automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(provide 'init-basic)

;;; init-basic.el ends here
