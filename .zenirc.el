;;;
;;;	~/.zenirc.el -- stuff for ZenIRC
;;;

;;;#ident	"@(#)HOME:.zenirc.el	20.1	98/07/20 11:23:48 (woods)"

;; A good way to use this is to add something like
;;(autoload 'zenirc
;;  (expand-file-name "~/.zenirc")
;;  "Major mode to waste time" t nil)

(require 'zenirc)

;;; Code:

;; the following is an example of how to do something during initializing a
;; server connection. 001 is the first thing the server sends to a client
;; after the client sends USER and NICK.
;;
;; :pfawww.pp.se 001 Omnion :Welcome to the Internet Relay Network Onion
;;
(defvar zenirc-startup-channels "&internex,#NetBSD,#secrets,#Planix"
  "*Comma separated string of channels to join during startup")

(defun zenirc-startup-join (proc parsedmsg)
  (process-send-string proc
		       (concat "JOIN " zenirc-startup-channels "\n"))

  (process-send-string proc
		       "AWAY :Sleeping, working, playing, eating, drinking, whatever....\n")

  ;; obviously this only works sometimes
  ;;
  (process-send-string proc "TOPIC #secrets\n")
  (process-send-string proc
		       "TOPIC #secrets :.... The Secrets of the *WEIRD* ....\n")
  (process-send-string proc "MODE #secrets +ip\n")
  (process-send-string proc "WHO #secrets\n")

  ;; obviously this only works sometimes
  ;;
  (process-send-string proc "TOPIC #Planix\n")
  (process-send-string proc
		       "TOPIC #Planix :Thoze PlanIX Dudes....\n")
  (process-send-string proc "MODE #Planix +is\n")
  (process-send-string proc "WHO #Planix\n")

  (zenirc-dotowho "*weird.com" '(process-send-string proc
						     (concat "INVITE "
							     (aref whoreply 7)
							     " #secrets\n")))
  (zenirc-dotowho "*planix.com" '(process-send-string proc
						      (concat "INVITE "
							     (aref whoreply 7)
							      " #Planix\n")))
)

(zenirc-add-hook 'zenirc-server-001-hook 'zenirc-startup-join)

;;; This is a big startup hook for zenirc...

(defun zenirc-custom-startup ()
  "Private startup hook for zenirc"
  ;; this is a list of IRC servers you use
  ;; it consists of servername, portnumber, password, nickname, username
  ;;(setq zenirc-server-alist 
  ;;      '(("flinch.io.org" 6667)	; 198.133.36.153
  ;;	("irc.sdsc.edu" 6667)
  ;;	("cs-pub.bu.edu" 6666)
  ;;    ("irc.rift.com"))		; 205.150.226.4
  (setq zenirc-server-default "205.150.226.4") ; emacs vs. resolver problems...

  (setq case-fold-search t)		; K.I.S.S.

  ;; this is your default nickname
  (setq zenirc-nick-default "RoboHack")

  ;; this is what you reply to CTCP USERINFO
  (setq zenirc-userinfo "The Robo-Hacker!(tm)")

  ;; this is a list of annoying things to ignore. This list ignores messages
  ;; from nickserv, anything with the word "fnord" in it, and messages from
  ;; the major dweeb craig.
  (setq zenirc-ignorance-list 
	'("^:NickServ!Nickserv@hpsystem2.informatik.tu-muenchen.de" "fnord"
	  "^:craig!craig@netsys1.netsys.com" ".*.*.*.*"))

  ;; ZenIRC can beep when it notices something
  (setq zenirc-beep-on-signal t)

  ;; fancy PRIVMSG formatting
  (require 'zenirc-signal)

  ;; yow-ness
  (require 'zenirc-yow)

  ;; this is how you want ZenIRC to send confirmations
  ;; "nil" is no confirmation
  ;; "t" is confirmation in buffer
  ;; "'message" is confirmation in echo area
  (setq zenirc-send-confirmation t)

  ;; if you want timestamps on PRIVMSG/NOTICE or not
  ;; with default prefix and suffix it looks like
  ;;	*ben[13:31]* lets have some fun
  ;;	<ben#twilight_zone[13:32]> SLUGS AND KNIGHTS! SLUGS AND KNIGTS!
  (setq zenirc-timestamp t
	zenirc-timestamp-prefix "["
	zenirc-timestamp-suffix "]")

  ;; if you want ZenIRC to send out ERRMSG on bogus CTCP queries
  (setq zenirc-send-ctcp-errmsg-on-unknown t)

  ;; if you awnt ZenIRC to send out ERRMSG on unbalanced CTCP queries
  (setq zenirc-send-ctcp-errmsg-on-unbalanced t)

  ;; if you want ZenIRC to tell you when send out CTCP replies
  (setq zenirc-verbose-ctcp t)

  ;; what ZenIRC replies on CTCP FINGER
  (setq zenirc-fingerdata
	(format "%s <%s@%s>" (user-full-name) (user-real-login-name) (system-name)))

  ;; commandkey in ZenIRC
  (setq zenirc-command-char ?/)

  ;; use the following to surpress AWAY info if seen more then once
  (require 'zenirc-away)

  ;; use the following to make ZenIRC queue commands for you, to get around
  ;; stupid flood controls
  (require 'zenirc-command-queue)

  ;; just for fun....
  (require 'zenirc-doto)		; also requires 'zenirc-command-queue

  ;; use the following to have tab-completion in ZenIRC
  (require 'zenirc-complete)

  ;; use the following to make ZenIRC fill incoming messages for you
  ;;
  (require 'zenirc-fill)
  ;;
  ;; if ZenIRC should fill things
  (set-default 'zenirc-fill-mode nil)	; don't show this in all buffers...
  (setq zenirc-fill-mode t)		; ... just in this one.
  ;;
  ;; how ZenIRC should fill things
  ;; "'prefix" adds zenirc-fill-prefix on each line
  ;; "'dynamic" adds spaces at beginning of line, depending on length of 
  ;;            first word
  (setq zenirc-fill-type 'dynamic)
  ;;(setq zenirc-fill-prefix " | ")

  ;; this causes messages to be formatted similar to ircII. the default is to
  ;; show nick!user@host instead of just nick, which I find eliminates the need
  ;; for nickserv, but you might be more used to ircII-style.
  ;; *** NOTE ***
  ;; also check out zenirc-format.el for a better-than-any-other-client nick
  ;; formatting package.
  (setq zenirc-user-format "%s")

  ;; use the following to make ZenIRC format things like no-other-client 
  ;; is able to
  (require 'zenirc-format)

  ;; use the following to get history functions on C-cC-p and C-cC-n
  (require 'zenirc-history)

  ;; use the following to get an ircII like /ignore command
  ;;(require 'zenirc-ignore)

  ;; use the following to make ZenIRC output netsplits nicer
  ;;(require 'zenirc-netsplit)

  ;; use the following to get an ircII like /notify command
  (require 'zenirc-notify)
  (setq zenirc-notify-list		; a list of notificated people
	'("dreamzz" "hmmmm" "robo2" "robohacker" "robotester" "pope13"))

  ;; allow CTCP "iwantop <channel>" commands to work automatically....
  (require 'zenirc-iwantop)
  (setq zenirc-iwantop-alist
	'(("\#secrets"	".*weird.com" ".*planix.com" "dreamzz!.*")
	  ("\#planix"	".*planix.com" ".*weird.com" ".*!peter@.*passport.ca" ".*!yoda@.*passport.ca")))

  ;; use the following to make ZenIRC popup buffers when things happen
  (require 'zenirc-popup)
  (setq zenirc-popup-available-frames t)
)

(zenirc-add-hook 'zenirc-startup-hook 'zenirc-custom-startup)

(require 'zenirc-iwantop)		; use the same alist....

(defun zenirc-auto-op-on-join (proc parsedmsg)
  (let* ((who (aref parsedmsg 1))
	 (their-nick (zenirc-extract-nick who))
	 ;; Do a zenirc-downcase-name even though case-fold-search is t
	 ;; because extra characters are translated to conform with
	 ;; RFC1459.
	 (channel (zenirc-downcase-name (aref parsedmsg 2)))
	 (alist zenirc-iwantop-alist)
	 (list nil)
	 (fmt-mode   "MODE %s +o %s\n"))
    (if (not (zenirc-names-equal-p their-nick zenirc-nick))
	(progn
	  (while alist
	    (and (string-match (zenirc-downcase-name (car (car alist)))
			       channel)
		 (progn
		   ;; skip the first elt of the car of alist, since
		   ;; that's just the channel name regexp
		   (setq list (cdr (car alist)))
		   (setq alist nil)))
	    (setq alist (cdr alist)))
	  (if (and list (zenirc-string-match-list who list))
	      (process-send-string proc (format fmt-mode
						channel their-nick)))))))
		 
(zenirc-add-hook 'zenirc-server-JOIN-hook 'zenirc-auto-op-on-join)

;;; TODO: set zenirc-exit-hook to try re-connecting after a pause....

;; this is the second last line of the file, the next line is the last one
;; The End.
