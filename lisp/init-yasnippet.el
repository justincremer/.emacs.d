;; init-yasnippet.el --- Initialize yasnippet configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Yasnippet configurations.
;;

;;; Code:

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
