;;; early-init.el --- Compiler and elisp configurations.  -*- lexical-binding: t -*-
;;; commentary:

;;; code:

;; Disable compiler warning for deprecated packages

(if (not (version< emacs-version "28.0.0"))
	  "Disables obsolete function messages in `native-comp'."
	  (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
		(let ((obsolete-name (pop ll))
			  (current-name (pop ll))
			  (when (if ll (pop ll) "1"))
			  (docstring (if ll (pop ll) nil)))
		  (list obsolete-name current-name when docstring))))

(setq byte-compile-warnings '(cl-functions))

(setq gc-cons-threshold most-positive-fixnum)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
