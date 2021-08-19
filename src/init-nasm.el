;; init-nasm.el --- Initialize nasm development configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nasm development configurations.
;;

;;; Code:

(use-package nasm-mode
  :ensure t
  :config (add-hook 'asm-mode-hook 'nasm-mode))

(provide 'init-nasm)

;;; init-nasm.el ends here
