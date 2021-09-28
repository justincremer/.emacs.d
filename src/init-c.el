;;; init-c.el --- Initialize C configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; C configurations.
;;

;;; Code:

(use-package glsl-mode :ensure t)

(use-package clang-format :ensure t)

(defun clang-format-current-buffer-on-save-hook ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
			(lambda ()
			  (when (locate-dominating-file "." ".clang-format")
				(clang-format-buffer))
			  nil)  ;; Continue to save.
			nil t)) ;; Buffer local hook.

(provide 'init-c)

;;; init-c.el ends here
