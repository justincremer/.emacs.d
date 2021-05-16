;;; early-init.el --- Compiler and load path configurations.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Garbage Collector configuration
(defvar xiu/gc-cons-threshold (if (display-graphic-p) 64000000 1600000))
(defvar xiu/gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000))
(defvar xiu/gc-timer (run-with-idle-timer 10 t #'garbage-collect))
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
	  gc-cons-threshold xiu/gc-cons-upper-limit
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist
				  gc-cons-threshold xiu/gc-cons-threshold
                  gc-cons-percentage 0.1)

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold xiu/gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold xiu/gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Disable compiler warning for deprecated packages
(if (not (version< emacs-version "28.0.0"))
	"Disables obsolete function messages in `native-comp'."
  (define-advice define-obsolete-function-alias
	  (:filter-args (ll) fix-obsolete)
	(let ((obsolete-name (pop ll))
		  (current-name (pop ll))
		  (when (if ll (pop ll) "1"))
		  (docstring (if ll (pop ll) nil)))
	  (list obsolete-name current-name when docstring))))

(setq byte-compile-warnings '(cl-functions)
	  gc-cons-threshold most-positive-fixnum)

;; Load path

;; This is inelegant and should be fixed sooner rather than later
(let ((default-directory "~/.config/emacs/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (defun update-load-path (&rest _)
  ;; "Update `load-path'."
;;   (dolist (dir '("site-lisp" "elisp"))
;;     (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

;; (update-load-path)

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
