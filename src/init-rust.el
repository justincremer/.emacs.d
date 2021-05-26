;; init-rust.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;  Rust configurations.

;;; Code:

(use-package rust-mode
  :bind (:map rust-mode-map
			  (("C-c C-t" . racer-describe)
			   ([?\t] . company-indent-or-complete-common)))
  :config
  (use-package rustic
	:ensure
	:bind (:map rustic-mode-map
				("M-j" . lsp-ui-imenu)
				("M-?" . lsp-find-references)
				("C-c C-c l" . flycheck-list-errors)
				("C-c C-c a" . lsp-execute-code-action)
				("C-c C-c r" . lsp-rename)
				("C-c C-c q" . lsp-workspace-restart)
				("C-c C-c Q" . lsp-workspace-shutdown)
				("C-c C-c s" . lsp-rust-analyzer-status))
	:config
	(defun xiu/rustic-mode-hook ()
	  "So that `C-c', `C-c', and `C-r' work without having to confirm."
	  (setq-local buffer-save-without-query t))

	;; uncomment for less flashiness
	;; (setq lsp-eldoc-hook nil)
	;; (setq lsp-enable-symbol-highlighting nil)
	;; (setq lsp-signature-auto-activate nil)

	;; comment to disable rustfmt on save
	(setq rustic-format-on-save t)
	(add-hook 'rustic-mode-hook 'xiu/rustic-mode-hook)
	(add-hook	'rustic-mode '(lsp-rust-analyzer-cargo-watch-command "clippy")
				'rustic-mode 'lsp-rust-analyzer-server-display-inlay-hints t))

  (progn
	(use-package flycheck-rust)

	(use-package cargo
	  :hook (rust-mode . cargo-minor-mode)
	  :bind ("C-c C-c C-n" . cargo-process-new))

	(use-package separedit
	  :straight (separedit :type git :host github :repo "idcrook/separedit.el")
	  :config
	  (progn
		(define-key prog-mode-map (kbd "C-c '") #'separedit)
		(setq separedit-default-mode 'markdown-mode)))

	(use-package racer
	  :hook (rust-mode . racer-mode)
	  :config
	  (progn
		(defun xiu/racer-mode-hook ()
		  (set (make-local-variable 'company-backends)
			   '((company-capf company-files)))
		  (setq company-minimum-prefix-length 1)
		  (setq indent-tabs-mode nil))

		(add-hook 'racer-mode-hook 'xiu/racer-mode-hook)
		(add-hook 'racer-mode-hook #'company-mode)
		(add-hook 'racer-mode-hook #'eldoc-mode)))

	(add-hook 'rust-mode-hook 'flycheck-mode)
	(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

	(add-hook 'before-save-hook
			  (lambda ()
				(when (eq major-mode 'rust-mode)
				  (rust-format-buffer))))))

(provide 'init-rust)

;;; init-rust.el ends here
