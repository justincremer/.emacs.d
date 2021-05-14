;; init-go.el --- Initialize Golang configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:

(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :bind (:map go-mode-map
			  ("C-c R" . go-remove-unused-imports)
			  ("<f1>" . godoc-at-point))
  :config
  (with-eval-after-load 'exec-path-from-shell
	(exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (defvar go--tools-no-update '("golang.org/x/tools/gopls@latest"))
  (defvar go--tools '("golang.org/x/tools/cmd/goimports"
					  "github.com/go-delve/delve/cmd/dlv"
					  "github.com/josharian/impl"
					  "github.com/cweill/gotests/..."
					  "github.com/fatih/gomodifytags"
					  "github.com/davidrjenni/reftools/cmd/fillstruct"))

  (defun go-update-tools ()
	"Install or update go tools."
	(interactive)
	(unless (executable-find "go")
	  (user-error "Unable to find `go' in `exec-path'!"))

	(message "Installing go tools...")
	(let ((proc-name "go-tools")
		  (proc-buffer "*Go Tools*"))
	  (dolist (pkg go--tools-no-update)
		(set-process-sentinel
		 (start-process proc-name proc-buffer "go" "get" "-v" pkg)
		 (lambda (proc _)
		   (let ((status (process-exit-status proc)))
			 (if (= 0 status)
				 (message "Installed %s" pkg)
			   (message "Failed to install %s: %d" pkg status))))))

	  (dolist (pkg go--tools)
		(set-process-sentinel
		 (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
		 (lambda (proc _)
		   (let ((status (process-exit-status proc)))
			 (if (= 0 status)
				 (message "Installed %s" pkg)
			   (message "Failed to install %s: %d" pkg status))))))))

  (unless (executable-find "gopls")
	(go-update-tools))

  (use-package go-fill-struct)
  (use-package go-impl)

  (use-package flycheck-golangci-lint
	:if (executable-find "golangci-lint")
	:after flycheck
	:defines flycheck-disabled-checkers
	:hook (go-mode . (lambda ()
					   "Enable golangci-lint."
					   (setq flycheck-disabled-checkers '(go-gofmt
														  go-golint
														  go-vet
														  go-build
														  go-test
														  go-errcheck))
					   (flycheck-golangci-lint-setup))))

  (use-package go-tag
	:bind (:map go-mode-map
		   ("C-c t t" . go-tag-add)
		   ("C-c t T" . go-tag-remove))
	:init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
	:bind (:map go-mode-map
		   ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
	:bind (:map go-mode-map
		   ("C-c t a" . go-test-current-project)
		   ("C-c t m" . go-test-current-file)
		   ("C-c t ." . go-test-current-test)
		   ("C-c t x" . go-run))))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)

;;; init-go.el ends here
