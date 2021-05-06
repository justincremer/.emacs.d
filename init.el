;;; init.el --- Start this puppy up      -*- lexical-binding: t no-byte-compile: t-*-

;; -----------------------------------------------------------------------------
;; ----------------------- XIUMACS --- by: Justin cremer -----------------------
;; ------------------ https://github.com/justincremer/.emacs.d -----------------
;; -----------------------------------------------------------------------------

;;; Commentary:
;;
;; Turns some numbers into other numbers
;;

;;; Code:

;; Startup ---------------------------------------------------------------------

;; Set garbage collection threshold for performance reasons
;; (setq gc-cons-threshold (* 50 1000 1000))

(defvar xiu/gc-cons-threshold (if (display-graphic-p) 64000000 1600000))

(defvar xiu/gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000))

(defvar xiu/gc-timer (run-with-idle-timer 10 t #'garbage-collect))

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold xiu/gc-cons-upper-limit
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold xiu/gc-cons-threshold
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
(require 'init-perspective)
(require 'init-general)
(require 'init-treemacs)
(require 'init-ivy)

;; Evil -------------------------------------------------------------------

(defun xiu/evil-hook ()
  "Determines which modes evade evil-mode."
  (dolist (mode '(custom-mode
				  shell-mode
				  eshell-mode
				  term-mode
				  git-rebase-mode
				  erc--mode
				  circe-server-mode
				  circe-chat-mode
				  circe-query-mode
				  sauron-mode))
	(add-to-list 'evil-emacs-state-modes mode)))

(defun xiu/dont-use-arrows ()
  "Yells at you for using arrow keys in evil mode."
  (interactive)
  (message "arrow keys make fingers go brrrrrr"))

(use-package evil
  :disabled
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'xiu/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<left>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<right>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<down>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<up>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<left>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<right>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<down>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<up>") 'xiu/dont-use-arrows)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :disabled
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
		(remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

;; Projectile ------------------------------------------------------------------

(defun xiu/switch-project ()
  "Switch to a workspace with the project name."
  (persp-switch (projectile-project-name)))
  ;; (projectile-find-file))
  
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Development")
    (setq projectile-project-search-path '("~/Development")))
  (setq projectile-switch-project-action #'xiu/switch-project))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-f" . counsel-projectile-find-file))
  :config (counsel-projectile-mode))

(xiu/leader-key-def
  "pp"  'counsel-projectile
  "pf"  'counsel-projectile-find-file
  "ps"  'counsel-projectile-switch-project
  "pF"  'counsel-projectile-rgc
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)

;; Magit -----------------------------------------------------------------------

(use-package magit
  :bind ("C-M-g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(xiu/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package forge
  :disabled)

(use-package magit-todos
  :defer t)

;; Org -------------------------------------------------------------------------

(defun xiu/org-mode-setup ()
  "Syntax styling in `org-mode'."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun xiu/org-mode-font-setup ()
  "Face styling in `org-mode'."
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (set-face-attribute 'org-document-title nil :font "Fira Code Retina:antialias=subpixel" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font "Fira Code Retina:antialias=subpixel" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun xiu/org-mode-visual-fill ()
  "Line wrapping in `org-mode'."
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :hook (org-mode . xiu/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (xiu/org-mode-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode . xiu/org-mode-visual-fill))

;; LSP Mode --------------------------------------------------------------------

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
			  ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable t))

(xiu/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package hover
  :ensure t)

;; Company ---------------------------------------------------------------------

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection))
  (:map lsp-mode-map
		("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Debug Adapter ---------------------------------------------------------------

(use-package dap-mode
  :straight t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))


;; Meta Lisp -------------------------------------------------------------------

;; (use-package lispy
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)))

;; (use-package lispyville
;;   :hook ((lispy-mode . lispyville-mode))
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional
;; 										additional-movement slurp/barf-cp
;; 										prettify)))

;; Emacs Lisp ------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(xiu/leader-key-def
  "e"  '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "er" '(eval-region :which-key "eval region"))

;; Common Lisp -----------------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq auto-mode-alist (append '(("\\.lisp" . common-lisp-mode))
                              auto-mode-alist))

(use-package sly
  :mode "\\.lisp\\'")

;; Markdown --------------------------------------------------------------------

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun xiu/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
  (defun xiu/markdown-mode-hook ()
    (xiu/set-markdown-header-font-sizes))
  (add-hook 'xiu/markdown-mode-hook 'xiu/markdown-mode-hook))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Docker ----------------------------------------------------------------------

(use-package docker
  :defer t)

(use-package dockerfile-mode
  :defer t)

;; Web Mode ------------------------------------------------------------------------

(use-package web-mode
  :straight t
  :mode (("\\.jsx?\\'" . web-mode)
		 ("\\.tsx?\\'" . web-mode)
		 ("\\.html?\\'" . web-mode)
		 ("\\.ejs\\'" . web-mode)
		 ("\\.vue\\'" . web-mode)
		 ("\\.svelte\\'" . web-mode)
		 ("\\.php\\'" . web-mode)
		 ("\\.json\\'" . web-mode))
  :commands web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :config
  (setq-default web-mode-code-indent-offset 4)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2)
  (setq web-mode-content-types-alist
		'(("jsx" . "\\.js[x]?\\'")
		  ("tsx" . "\\.ts[x]?\\'"))))

(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

;; Javascript -------------------------------------------------------

(defun xiu/set-js-indentation ()
  "Set the default indentation width for newlines."
  (setq js-indent-level 4)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 4))

(defun xiu/js2-config ()
  "Set environment variables for js2-mode."
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'xiu/set-js-indentation)
  (add-hook 'json-mode-hook #'xiu/set-js-indentation))

(use-package js2-mode
  :straight t
  :mode "\\.jsx?\\'"
  :config
  (xiu/js2-config))

;; Typescript ------------------------------------------------------------------

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\'"
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

;; Emmet -----------------------------------------------------------------------

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; Golang ----------------------------------------------------------------------

;; Haskell ---------------------------------------------------------------------

(use-package haskell-mode
  :straight t
  :ensure t
  :mode "\\.hs\\'"
  :ensure t)

;; Yasnippet -------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

;; Hover -----------------------------------------------------------------------



;;; init.el ends here

