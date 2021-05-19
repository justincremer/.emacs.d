;; init-custom.el --- Initialize custom configurations.      -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Custom configurations.
;;

;;; Code:

(defgroup xiu nil
  "Xiu Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/justincremer/.emacs.d"))

(defcustom xiu/full-name "Justin Cremer"
  "Set user full name."
  :group 'xiu
  :type 'string)

(defcustom xiu/mail-address "jacremer@live.com"
  "Set user email address."
  :group 'xiu
  :type 'string)

(defcustom xiu/org-directory (expand-file-name "~/org/")
  "Set org directory."
  :group 'xiu
  :type 'string)

(defcustom xiu/prettify-symbols-alist
  '(("lambda" . ?λ)
	("<-" . ?←)
	("->" . ?→)
	("->>" . ?↠)
	("=>" . ?⇒)
	("map" . ?↦)
	("/=" . ?≠)
	("!=" . ?≠)
	("==" . ?≡)
	("<=" . ?≤)
	(">=" . ?≥)
	("=<<" . (?= (Br . Bl) ?≪))
	(">>=" . (?≫ (Br . Bl) ?=))
	("<=<" . ?↢)
	(">=>" . ?↣)
	("&&" . ?∧)
	("||" . ?∨)
	("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'xiu
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom xiu/prettify-org-symbols-alist
  '(("[ ]" . ?☐)
	("[X]" . ?☑)
	("[-]" . ?⛝)
	("#+ARCHIVE:" . ?📦)
	("#+AUTHOR:" . ?👤)
	("#+CREATOR:" . ?💁)
	("#+DATE:" . ?📆)
	("#+DESCRIPTION:" . ?⸙)
	("#+EMAIL:" . ?📧)
	("#+OPTIONS:" . ?⛭)
	("#+SETUPFILE:" . ?⛮)
	("#+TAGS:" . ?🏷)
	("#+TITLE:" . ?📓)
	("#+BEGIN_SRC" . ?✎)
	("#+END_SRC" . ?□)
	("#+BEGIN_QUOTE" . ?»)
	("#+END_QUOTE" . ?«)
	("#+HEADERS" . ?☰)
	("#+RESULTS:" . ?💻))
  "Alist of symbol prettifications for `org-mode'."
  :group 'xiu
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;; init-custom.el ends here
