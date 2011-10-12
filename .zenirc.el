;;;
;;;	~/.zenirc.el -- stuff for ZenIRC
;;;

;;;#ident	"@(#)HOME:.zenirc.el	34.1	11/10/12 16:54:51 (woods)"

;; A good way to use this is to add something like to .emacs(.el)
;;(autoload 'zenirc (expand-file-name "~/.zenirc") "Major mode to waste time" t nil)

(require 'zenirc)

;;; Code:

;; the following is an example of how to do something during initializing a
;; server connection. 001 is the first thing the server sends to a client
;; after the client sends USER and NICK.
;;
;; :pfawww.pp.se 001 Omnion :Welcome to the Internet Relay Network Onion
;;
(defvar zenirc-startup-channels nil
  "*Comma separated string of channels to join during startup")

;; Use "/query <channel>" to switch between these ones...
;(setq zenirc-startup-channels "#srh,#NetBSD,#weird,#Planix")
(setq zenirc-startup-channels "#NetBSD,#weird,#Planix")

(defun zenirc-startup-join (proc parsedmsg)
  "*Hook function run in zenirc-server-001-hook."

  ;; pause and wait for any auto-invites to take effect.....
  (message "pausing and waiting for possible invites....")
  (sleep-for 10)

  (process-send-string proc
		       (concat "JOIN " zenirc-startup-channels "\n"))

  (process-send-string proc
		       "AWAY :Sleeping, working, playing, eating, drinking, whatever....\n")

  ;; obviously this only works sometimes
  ;;
  (process-send-string proc "TOPIC #weird\n")
  (process-send-string proc
		       "TOPIC #weird :.... The Secrets of the *WEIRD* ....\n")
  (process-send-string proc "MODE #weird +ip\n")
  (process-send-string proc "WHO #weird\n")

  ;; obviously this only works sometimes
  ;;
  (process-send-string proc "TOPIC #Planix\n")
  (process-send-string proc
		       "TOPIC #Planix :Thoze PlanIX Dudes....\n")
  (process-send-string proc "MODE #Planix +is\n")
  (process-send-string proc "WHO #Planix\n")

  ;; Now we invite anyone at weird.com to join #weird
  (zenirc-dotowho "*weird.com" '(process-send-string proc
						     (concat "INVITE "
							     (aref whoreply 7)
							     " #weird\n")))
  ;; Now we invite anyone at planix.com to join #Planix
  (zenirc-dotowho "*planix.com" '(process-send-string proc
						      (concat "INVITE "
							     (aref whoreply 7)
							      " #Planix\n")))
)

(zenirc-add-hook 'zenirc-server-001-hook 'zenirc-startup-join)

(defvar zenirc-auto-invite-weird-list nil
  "*List of nicknames to invite to #weird.  Must be lower-case.")

(setq zenirc-auto-invite-weird-list '("ghoti"
				      "hmmm-"
				      "EhloKitty"
				      "vambo"
				      "robo2"
				      "robohack"
				      "robotest"))

(defun zenirc-auto-invite-weird (proc nick)
  "*For use on zenirc-notify-is-on-hook."
  (if (member (downcase nick) zenirc-auto-invite-weird-list)
      (progn
	(zenirc-message proc "[debug] Auto-inviting %s to #weird...." nick)
	(zenirc-dotowho nick '(process-send-string proc
						 (concat "INVITE "
							 (aref whoreply 7)
							 " #weird\n"))))))

(defvar zenirc-auto-invite-planix-list nil
  "*List of nicknames to invite to #planix.  Must be lower-case.")

(setq zenirc-auto-invite-planix-list '("robohack"
				       "robo2"
				       "robotest"
				       "whome"
				       "who-me"))

(defun zenirc-auto-invite-planix (proc nick)
  "*For use on zenirc-notify-is-on-hook."
  (if (member (downcase nick) zenirc-auto-invite-planix-list)
      (progn
	(zenirc-message proc "[debug] Auto-inviting %s to #planix...." nick)
	(zenirc-dotowho nick '(process-send-string proc
						   (concat "INVITE "
							   (aref whoreply 7)
							   " #planix\n"))))))

;;; This is a big startup hook for zenirc...

(defun zenirc-custom-startup ()
  "Private startup hook for zenirc"
  ;;
  ;; From a recent list of EFnet servers:
  ;;
  ;; [AB] irc.powersurfr.com         24.108.60.60            6660-6669,7000
  ;; [MB] irc.magic.ca               207.161.152.101         6660-6669
  ;; [MB] irc.mbnet.mb.ca            204.112.54.14           6660-6669
  ;; [NB] irc.nbnet.nb.ca            198.164.211.2           6667
  ;; [ON] irc.idirect.ca             205.210.36.2            6667
  ;; [ON] irc.rift.com               205.150.226.4           6667
  ;; [QC] irc.mlink.net              205.236.182.76          6667
  ;; [QC] irc.total.net              205.236.175.138         6667
  ;;
  ;; servername, portnumber, password, nickname, username
  ;;(setq zenirc-server-alist 
  ;;      '(("flinch.io.org" 6667)	; 198.133.36.153
  ;;	("irc.sdsc.edu" 6667)
  ;;	("cs-pub.bu.edu" 6666)
  ;;	("irc.rift.com")		; 205.150.226.4
  ;;	("irc.lagged.com"))
  ;;
  ;; WARNING: this is over-ridden by $IRCSERVER in the environment!
  ;;
  ;; WARNING: not also that if zenirc-server-alist is not set then it will be
  ;; set from this variable and further attempts to change it must set that
  ;; variable instead.
  (setq zenirc-server-default "irc.srh.org")

  (setq case-fold-search t)		; K.I.S.S.

  ;; this is your default nickname
  (setq zenirc-nick-default "Robo2")
  ;(setq zenirc-nick-default "RoboHack")

  ;; this is what you reply to CTCP USERINFO
  (setq zenirc-userinfo "The Robo-Hacker!(tm)")

  ;; this is a list of annoying things to ignore. This list ignores messages
  ;; from nickserv, anything with the word "fnord" in it, and messages from
  ;; the major dweeb craig.
  (setq zenirc-ignorance-list 
	'("^:NickServ!Nickserv@hpsystem2.informatik.tu-muenchen.de"
	  "fnord"
	  "^:craig!craig@netsys1.netsys.com"
	  ".*.*.*.*"))

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

  ;; if WHOIS returns no-such-nick, setting this variable to t will make 
  ;; the client automaticall issue an WHOWAS command
  (setq zenirc-whowas-on-401 nil)

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
  (require 'zenirc-ignore)

  ;; use the following to make ZenIRC output netsplits nicer
  (require 'zenirc-netsplit)

  ;; use the following to get an ircII like /notify command
  (require 'zenirc-notify)
  (setq zenirc-notify-list		; a list of notificated people
	'("hmmm-"
	  "vambo"
	  "punchy"
	  "EhloKitty"
	  "pope13"
	  "RoboHack"
	  "robo2"
	  "robotest"
	  "whome"
	  "who-me"))
  (zenirc-add-hook 'zenirc-notify-is-on-hook 'zenirc-auto-invite-weird)
  (zenirc-add-hook 'zenirc-notify-is-on-hook 'zenirc-auto-invite-planix)

  ;; allow CTCP "iwantop <channel>" commands to work automatically....
  (require 'zenirc-iwantop)
  (setq zenirc-iwantop-alist
	'(("\#weird"	".*weird.com" ".*planix.com")
	  ("\#planix"	".*planix.com" ".*weird.com")))

  ;; use the following to make ZenIRC popup buffers when things happen
  (require 'zenirc-popup)
  (setq zenirc-popup-available-frames t)

  ;; This ZenIRC extensions allows you to colourise input from specific
  ;; sources.  Use the "/color #victim <COLOR>" command to start
  ;; colourizing a certain victim's output, "/uncolor #victim" to stop.
  (setq zenirc-color-mode t)

  ;; TODO: set up faces so we can use new `zenirc-color-alist' to choose default colors
;  (setq zenirc-color-alist
;	'(("^<robo2" face-variable-color-yellow)
;	  ("#planix" face-variable-color-red)
;	  ("#srh" face-variable-color-darkgreen)
;	  ("#weird" face-variable-color-purple)))

;  (require 'zenirc-color)
;  (zenirc-color-mode)
;  (zenirc-add-color-victim "purple" "#weird")
;  (zenirc-add-color-victim "red" "#planix")
;  (zenirc-add-color-victim "orange" "#netbsd")
;  (zenirc-add-color-victim "blue" "&internex")
;  (zenirc-add-color-victim "darkgreen" "#srh")
;  (zenirc-add-color-victim "green" "robotest")
;  (zenirc-add-color-victim "yellow" "robo2")
;  (zenirc-add-color-victim "steelblue" "hmmm-")

  ;; TODO: modify "/color" to print the list when given no args...

  ;; end of zenirc-custom-startup
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

;; this is effectively an alias for /part
(defvar zenirc-command-leave-hook nil
  "Hook run for /leave command.")
(zenirc-add-hook 'zenirc-command-leave-hook 'zenirc-command-part)

;; /myaway
;; set my custom away message
(defvar zenirc-command-myaway-hook '(zenirc-command-myaway))
(defun zenirc-command-myaway (proc parsedcmd)
  (process-send-string proc
		       "AWAY :Sleeping, working, playing, eating, drinking, whatever....\n"))


;;; TODO: set zenirc-exit-hook to try re-connecting after a pause....

;; this is the second last line of the file, the next line is the last one
;; The End.
