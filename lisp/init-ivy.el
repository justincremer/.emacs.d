;; init-ivy.el --- Initialize ivy configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ivy configurations.
;;

;;; Code:

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
		 ("C-;" . comment-line)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-l" . ivy-alt-done)
		 ("C-j" . ivy-next-line)
		 ("C-k" . ivy-previous-line)
		 :map ivy-switch-buffer-map
		 ("C-k" . ivy-previous-line)
		 ("C-l" . ivy-done)
		 ("C-d" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-k" . ivy-previous-line)
		 ("C-d" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t) (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(xiu/leader-key-def
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
		(plist-put ivy-rich-display-transformers-list
				   'ivy-switch-buffer
				   '(:columns
					 ((ivy-rich-candidate (:width 40))
					  (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
					  (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
					  (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
					  (ivy-rich-switch-buffer-path ; return file path relative to project root or `default-directory' if project is nil
					   (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
					 :predicate
					 (lambda (cand)
					   (if-let ((buffer (get-buffer cand)))
						   ;; Don't mess with EXWM buffers
						   (with-current-buffer buffer
							 (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 ("C-M-l" . counsel-imenu)
		 :map minibuffer-local-map
		 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (setq ivy-initial-inputs-alist nil))

(use-package flx ;; Imroves sorting for fuzzy-matched results
  :defer t
  :after ivy
  :init (setq ivy-flx-limit 10000))

(use-package wgrep)
(use-package ripgrep)

(use-package ivy-posframe
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
								  (left-fringe . 8)
								  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(xiu/leader-key-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file")
  "fj"  '(counsel-file-jump :which-key "jump to file"))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(xiu/leader-key-def
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

(provide 'init-ivy)

;;; init-ivy.el ends here
