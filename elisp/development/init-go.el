;; init-go.el --- Initialize Golang configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:

;; (defconst xiu/go--root-path "/usr/local/go/bin")
(defconst xiu/go--tools-no-update
  '("golang.org/x/tools/gopls@latest"))
(defconst xiu/go--tools-update
  '("golang.org/x/tools/cmd/goimports"
	"golang.org/x/tools/cmd/godoc"
	"github.com/rogpeppe/godef"
	"github.com/josharian/impl"
	"github.com/cweill/gotests/..."
	"github.com/fatih/gomodifytags"
	"github.com/go-delve/delve/cmd/dlv"
	"github.com/davidrjenni/reftools/cmd/fillstruct"))

(defun xiu/go-update-tools (no-update-list update-list)
  "Install or update `go' dev tools."
  (interactive)
  (unless (executable-find "go")
	(user-error "Unable to find `go' in `exec-path'!"))
  (message "Installing go tools...")
  (let ((proc-name "go-tools")
		(proc-buffer "*Go Tools*"))
	(dolist (pkg no-update-list)
	  (set-process-sentinel
	   (start-process proc-name proc-buffer "go" "get" "-v" pkg)
	   (lambda (proc _)
		 (let ((status (process-exit-status proc)))
		   (if (= 0 status)
			   (message "Installed %s" pkg)
			 (message "Failed to install %s: %d" pkg status))))))
	(dolist (pkg update-list)
	  (set-process-sentinel
	   (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
	   (lambda (proc _)
		 (let ((status (process-exit-status proc)))
		   (if (= 0 status)
			   (message "Installed %s" pkg)
			 (message "Failed to install %s: %d" pkg status))))))))

(defun xiu/go-mode-config ()
  (with-eval-after-load 'exec-path-from-shell
	(exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; (unless (member xiu/go--root-path load-path)
	;; (add-to-list 'load-path xiu/go--root-path))

  (unless (executable-find "gopls")
	(xiu/go-update-tools xiu/go--tools-no-update xiu/go--tools-update))

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
				("C-c t x" . go-run)))

  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :bind (:map go-mode-map
			  ("C-c R" . go-remove-unused-imports)
			  ("<f1>" . godoc-at-point))
  :config (xiu/go-mode-config))

(use-package go-playground
  :diminish
  :commands (go-playground-mode))

(provide 'init-go)

;;; init-go.el ends here
