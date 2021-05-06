;; init-ui.el --- Initialize ui configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; UI configurations.
;;

;;; Code:

;; Font ------------------------------------------------------------------------

;; Make sure Windows doesn't wreck your char sets (thanks Bill)
(set-default-coding-systems 'utf-8)

(defvar default-font-size 120)

(setq-default tab-width 4)

(set-face-attribute 'default nil
					:font "Iosevka:regular:antialias=subpixel:hinting=true"
					:height default-font-size)

(use-package unicode-fonts
  :ensure t
  :custom (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config (unicode-fonts-setup))

(use-package emojify
  :ensure t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Not working in Emacs 28.0.50
(use-package ligature
  :disabled
  :ensure t
  :config (xiu/ligature-config))

;; Assign ligation mapping
(let ((alist `((?! . ,(regexp-opt '("!!" "!=" "!==")))
			   (?# . ,(regexp-opt '("##" "###" "####" "#(" "#?" "#[" "#_" "#_(" "#{")))
			   (?$ . ,(regexp-opt '("$>")))
			   (?% . ,(regexp-opt '("%%")))
			   (?& . ,(regexp-opt '("&&")))
			   (?* . ,(regexp-opt '("*" "**" "***" "**/" "*/" "*>")))
			   (?+ . ,(regexp-opt '("+" "++" "+++" "+>")))
			   (?- . ,(regexp-opt '("--" "---" "-->" "-<" "-<<" "->" "->>" "-}" "-~")))
			   (?. . ,(regexp-opt '(".-" ".." "..." "..<" ".=")))
			   (?/ . ,(regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>")))
			   (?: . ,(regexp-opt '(":" "::" ":::" ":=")))
			   (?\; . ,(regexp-opt '(";;")))
			   (?< . ,(regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<~" "<~~")))
			   (?= . ,(regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>")))
			   (?> . ,(regexp-opt '(">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>")))
			   (?= . ,(regexp-opt '("?=")))
			   (?? . ,(regexp-opt '("??")))
			   (?\[ . ,(regexp-opt '("[]")))
			   (?\\ . ,(regexp-opt '("\\\\" "\\\\\\")))
			   (?^ . ,(regexp-opt '("^=")))
			   (?w . ,(regexp-opt '("www")))
			   (?x . ,(regexp-opt '("x")))
			   (?{ . ,(regexp-opt '("{-")))
			   (?| . ,(regexp-opt '("|=" "|>" "||" "||=")))
			   (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))))))
  (dolist (char-regexp alist)
	(set-char-table-range composition-function-table (car char-regexp)
						  `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Style -----------------------------------------------------------------------

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq use-dialog-box nil)
(setq visible-bell t)
(setq inhibit-startup-message t)

(dolist (mode '(org-mode-hook
				shell-mode-hook
				eshell-mode-hook
				term-mode-hook
				treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))
;; :init (load-theme 'doom-dark+ t))
;; :init (load-theme 'doom-acario-dark t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :after web-mode
  :defer t
  :hook (org-mode
		 emacs-lisp-mode
		 web-mode
		 typescript-mode
		 js2-mode))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(use-package whitespace-cleanup-mode
  :defer t
  :hook
  (org-mode
   emacs-lisp-mode
   web-mode
   js2-mode
   typescript-mode))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package diminish)

(provide 'init-ui)

;;; init-ui.el ends here
