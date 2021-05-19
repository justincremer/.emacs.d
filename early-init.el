
;;; early-init.el --- Compiler and load path configurations.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar xiu/gc-cons-threshold (if (display-graphic-p) 64000000 1600000))
(defvar xiu/gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000))
(defvar xiu/gc-timer (run-with-idle-timer 10 t #'garbage-collect))
(defvar xiu/default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
	  gc-cons-threshold xiu/gc-cons-upper-limit
      gc-cons-percentage 0.5
	  package-enable-at-startup nil
	  frame-inhibit-implied-resize t
	  byte-compile-warnings '(cl-functions)
	  gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
		  (lambda ()
			(defun xiu/minibuffer-setup-hook ()
			  (setq gc-cons-threshold xiu/gc-cons-upper-limit))
			
			(defun xiu/minibuffer-exit-hook ()
			  (setq gc-cons-threshold xiu/gc-cons-threshold))
			
			(setq file-name-handler-alist xiu/default-file-name-handler-alist
				  gc-cons-threshold xiu/gc-cons-threshold
				  gc-cons-percentage 0.1)
			
			(if (boundp 'after-focus-change-function)
				(add-function :after after-focus-change-function
							  (lambda ()
								(unless (frame-focus-state)
								  (garbage-collect))))
			  (add-hook 'focus-out-hook 'garbage-collect))
			
			(add-hook 'minibuffer-setup-hook #'xiu/minibuffer-setup-hook)
			(add-hook 'minibuffer-exit-hook #'xiu/minibuffer-exit-hook)))

;; (defun update-load-path (&rest _)
;;   "Update `load-path'."
;;   (dolist (dir '("src" "site-lisp"))
;;     (push (expand-file-name dir user-emacs-directory) load-path)))

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("src"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
