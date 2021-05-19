;; init-funcs.el --- Initialize funcs configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Custom function configurations.
;;

;;; Code:

(defun xiu/set-exec-path-from-shell-path ()
  "Set up Emacs symbol `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
	(setenv "PATH" path-from-shell)
	(setq exec-path (split-string path-from-shell path-separator))))

(provide 'init-funcs)

;;; init-funcs.el ends here
