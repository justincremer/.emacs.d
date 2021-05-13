;; init-evil.el --- Initialize evil configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; EviI configurations.
;;

;;; Code:

(defun xiu/evil-hook ()
  "Determines which modes evade `evil-mode'."
  (dolist (mode '(custom-mode
				  shell-mode
				  eshell-mode
				  term-mode
				  git-rebase-mode
				  erc--mode))
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
  (evil-mode t)
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

(provide 'init-evil)

;;; init-evil.el ends here
