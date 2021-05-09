;; init-package.el --- Initialize package management configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Package management configurations.
;;

;;; Code:

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'init-package)

;;; init-package.el ends here
