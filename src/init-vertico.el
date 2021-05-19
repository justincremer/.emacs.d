;; init-vertico.el --- Initialize vertico configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Vertico configurations.
;;

;;; Code:

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :straight '(vertico :host github
					  :repo "minad/vertico"
					  :branch "main")
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package marginalia
  :disabled
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package corfu
  :straight '(corfu :host github
					:repo "minad/corfu")
  :bind (:map corfu-map
			  ("C-j" . corfu-next)
			  ("C-k" . corfu-previous)
			  ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
		completion-category-defaults nil
		completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-vertico)

;;; init-vertico.el ends here
