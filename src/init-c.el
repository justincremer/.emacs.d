;; init-c.el --- Initialize C configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; C configurations.
;;

;;; Code:


;; :mode (("\\.js\\'" . js2-mode)


(use-package ccls
  :hook ("\\.c\\" . c-mode))

(provide 'init-c)

;;; init-c.el ends here
