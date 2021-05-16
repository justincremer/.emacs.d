;; init-org.el --- Initialize org configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

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

(provide 'init-org)

;;; init-org.el ends here
