;;; init.el --- Start this puppy up      -*- lexical-binding: t no-byte-compile: t-*-

;; -----------------------------------------------------------------------------
;; ----------------------- XIUMACS --- by: Justin cremer -----------------------
;; ------------------ https://github.com/justincremer/.emacs.d -----------------
;; -----------------------------------------------------------------------------

;;; Commentary:
;;
;; Turns words into syscalls baby
;;

;;; Code:

;; Load path -------------------------------------------------------------------

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages  -------------------------------------------------------------------

(require 'init-package)
(require 'init-basic)
(require 'init-ui)
(require 'init-dashboard)

(require 'init-eshell)
(require 'init-shell)

(require 'init-perspective)
(require 'init-general)
(require 'init-treemacs)
(require 'init-ivy)
(require 'init-yasnippet)
(require 'init-evil)

(require 'init-projectile)
(require 'init-magit)
(require 'init-org)
(require 'init-lsp)
(require 'init-company)
(require 'init-dap)
(require 'init-docker)
(require 'init-lisp)
(require 'init-elisp)
(require 'init-clisp)
(require 'init-markdown)
(require 'init-web)
(require 'init-dart)
(require 'init-go)
(require 'init-haskell)

;;; init.el ends here
