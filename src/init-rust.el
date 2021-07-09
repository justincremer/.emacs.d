;; init-rust.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;  Rust configurations.

;;; Code:

(defun xiu/rustic--modules ()
  "Install additional rust modules for `rustic-mode'."

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
	(defun xiu/racer-mode-hook ()
	  (set (make-local-variable 'company-backends)
		   '((company-capf company-files)))
	  (setq company-minimum-prefix-length 1)
	  (setq indent-tabs-mode nil))
	(add-hook 'racer-mode-hook 'xiu/racer-mode-hook)
	(add-hook 'racer-mode-hook #'company-mode)
	(add-hook 'racer-mode-hook #'eldoc-mode))

  (use-package rustic
	:ensure
	:straight t
	:bind (:map rust-mode-map
				("M-j" . lsp-ui-imenu)
				("M-?" . lsp-find-references)
				("C-c C-t" . racer-describe)
				("C-c C-c l" . flycheck-list-errors)
				("C-c C-c a" . lsp-execute-code-action)
				("C-c C-c r" . lsp-rename)
				("C-c C-c q" . lsp-workspace-restart)
				("C-c C-c Q" . lsp-workspace-shutdown)
				("C-c C-c s" . lsp-rust-analyzer-status))
	:config
	(setq rustic-format-on-save t
		  rustic-lsp-server 'rust-analyzer))
  (add-hook 'rust-mode-hook '(setq-local buffer-save-without-query t))
  (add-hook	'rust-mode '(lsp-rust-analyzer-cargo-watch-command "clippy"))
  (add-hook 'rust-mode '(lsp-rust-analyzer-server-display-inlay-hints t)))

(use-package rust-mode
  :config
  (progn
	(use-package flycheck-rust)
	(xiu/rustic--modules)))

(provide 'init-rust)

;;; init-rust.el ends here
