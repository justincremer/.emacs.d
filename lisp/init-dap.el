;; init-dap.el --- Initialize dap configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dap configurations.
;;

;;; Code:

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(provide 'init-dap)

;;; init-dap.el ends here
