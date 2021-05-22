;; init-perspective.el --- Initialize perspective configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Perspective configurations.
;;

;;; Code:



(use-package perspective
  :demand t
  :bind (("C-M-j" . persp-counsel-switch-buffer)
		 ("C-M-k" . persp-switch)
		 ("C-M-l" . persp-next)
		 ("C-M-h" . persp-prev)
		 ("C-x k" . persp-kill-buffer*)
		 ("C-M-k" . persp-kill))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (global-unset-key (kbd "C-M-l"))
  (unless (equal persp-mode t)
	(persp-mode)))

(provide 'init-perspective)

;;; init-perspective.el ends here
