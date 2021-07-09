;;; init-erc.el --- Initialize erc client        -*- lexical-binding: t no-byte-compile: t-*-

;;; Commentary:
;;
;; Initialize erc
;;

;;; Code:


(defun xiu/on-erc-track-list-changed ()
  (dolist (buffer erc-modified-channels-alist)
	(tracking-add-buffer (car buffer))))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(use-package erc
  :commands erc
  :hook (erc-track-list-changed . xiu/on-erc-track-list-changed)
  :config
  (setq
	  erc-nick "daviwil"
	  erc-user-full-name "David Wilson"
	  erc-prompt-for-nickserv-password nil
	  erc-auto-query 'bury
	  erc-join-buffer 'bury
	  erc-interpret-mirc-color t
	  erc-rename-buffers t
	  erc-lurker-hide-list '("JOIN" "PART" "QUIT")
	  erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE")
	  erc-track-enable-keybindings nil
	  erc-track-visibility nil ; Only use the selected frame for visibility
	  erc-fill-column 80
	  erc-fill-function 'erc-fill-static
	  erc-fill-static-center 20
	  erc-track-exclude '("#twitter_daviwil")
	  erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#guix"))
	  erc-quit-reason (lambda (s) (or s "Fading out..."))
	  erc-modules
	  '(autoaway autojoin button completion fill irccontrols keep-place
		  list match menu move-to-prompt netsplit networks noncommands
		  readonly ring stamp track hl-nicks))

  (add-hook 'erc-join-hook 'bitlbee-identify)
  (defun bitlbee-identify ()
	"If we're on the bitlbee server, send the identify command to the &bitlbee channel."
	(when (and (string= "127.0.0.1" erc-session-server)
			   (string= "&bitlbee" (buffer-name)))
	  (erc-message "PRIVMSG" (format "%s identify %s"
									 (erc-default-target)
									 (password-store-get "IRC/Bitlbee"))))))

(defun xiu/connect-irc ()
  "Connect to an libera.chat."
  (interactive)
  (erc-tls
	 :server "irc.libera.chat" :port 6667
	 :nick "xiuxiu62" :password (password-store-get "IRC/Freenode")))
  ;; (erc
  ;;    :server "127.0.0.1" :port 6667
  ;;    :nick "daviwil" :password (password-store-get "IRC/Bitlbee")))

(provide 'init-erc)

;;; init-erc.el ends here
