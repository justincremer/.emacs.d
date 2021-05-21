;; init-haskell.el --- Initialize haskell configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Haskell configurations.
;;

;;; Code:

;; (use-package haskell-mode
;;   :hook ((haskell-mode-hook . interactive-haskell-mode)
;;		 (haskell-mode-hook . turn-on-haskell-indentation))
;;   :bind (:map haskell-mode-map
;;			  ("C-c C-g" . haskell-interactive-bring)
;;			  ("C-c C-l" . haskell-process-load-or-reload)
;;			  ("C-c C-t" . haskell-process-do-type)
;;			  ("C-c C-i" . haskell-process-do-info)
;;			  ("C-c C-c" . haskell-process-cabal-build)
;;			  ("C-c C-k" . haskell-interactive-mode-clear)
;;			  ("C-c c" . haskell-process-cabal)
;;			  ([f7] . haskell-navigate-imports))
;;   :custom ((haskell-process-suggest-remove-import-lines t)
;;		   (haskell-process-auto-import-loaded-modules t)
;;		   (haskell-process-type 'cabal-repl)
;;		   (haskell-stylish-on-save t)
;;		   (haskell-process-log t))
;;   :init (eval-after-load "haskell-mode"
;;		  '(progn
;;			 (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
;;			 (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))
;;   :config (setq haskell-ghci-program-name "cabal"
;;				haskell-ghci-program-args '("repl")))

;; (use-package lsp-haskell
;;   :hook ((haskell-mode-hook haskell-literate-mode-hook-hook) . lsp))

(use-package haskell-mode)
(use-package lsp-haskell
	:hook ((haskell-mode-hook haskell-literate-mode-hook-hook) . lsp))

(provide 'init-haskell)

;;; init-haskell.el ends here
