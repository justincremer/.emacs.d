;; init-tabs.el --- Initialize tabs configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Centaur tabs configurations.
;;

;;; Code:

(use-package centaur-tabs
  :disabled
  :ensure t
  :bind ("C-TAB" . tabs-cycle)
  :commands (tabs-cycle centaur-tabs-forward-tab-text)
  :config
  (setq centaur-tabs-set-bar 'over
  	centaur-tabs-set-icons t
  	centaur-tabs-set-gray-out-icons 'buffer
  	centaur-tabs-height 24
  	centaur-tabs-set-modified-marker t
  	centaur-tabs-modifier-marker "*")
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(provide 'init-tabs)

;;; init-tabs.el ends here
