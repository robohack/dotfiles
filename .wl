;;;;
;;;;	.wl.el - Wanderlust custom configuration
;;;;
;;;;#ident	"@(#)HOME:.wl	1.3	05/09/29 18:14:16 (woods)"
;;;;

;; one can apparently use `mime-preview-toggle-content' with C-c C-t C-c to
;; show the text part if desired, and presumably to hide the HTML crap....
;;
;; this apparently has to be done after mime-view is loaded, and so the
;; following was also once mentioned on the mailing list:
;;
;;	To disable rendering text/html, please try the following configuration:
;;
;(eval-after-load "mime-view"
;  '(progn
;     (ctree-set-calist-strictly
;      'mime-preview-condition
;      '((type . text)
;	(subtype . html)
;	(body . visible)
;	(body-presentation-method . mime-display-text/plain)))
;     (set-alist 'mime-view-type-subtype-score-alist
;		'(text . html) 0)
;     ;;
;     ))
;;
;; however for now the following seems to do the trick all by itself:
;;
(setq mime-setup-enable-inline-html nil)

;; don't leave my passwords sitting in memory too long!
;;
(setq elmo-passwd-life-time 3600)

;; Use SSL connection
;(setq elmo-imap4-default-stream-type 'starttls)
;(setq elmo-imap4-default-port 993)
(setq elmo-imap4-default-stream-type nil)
(setq elmo-imap4-default-port 143)

;; password always in raw format for my servers
;;
(setq elmo-imap4-default-authenticate-type 'clear)

;; XXX this doesn't quite work right to turn on automatic signing, especially
;; with mutiple accounts.
;;
;(setq mime-edit-pgp-processing '(sign))

;(setq mime-setup-enable-pgp t)		; it is the default
(setq pgg-default-scheme 'pgp5)		; for composing
(setq pgg-scheme 'pgp5)			; for verify/decrypt
;(setq pgg-cache-passphrase t)		; it is the default
(setq pgg-passphrase-cache-expiry 3600)	; 60 min

;; Directory where icons are placed.
;; XXX should be set by package install, but seems not to be)
;;
(setq wl-icon-directory "/usr/pkg/share/wl")

;; SMTP server for mail posting. Default: `nil'
(setq wl-smtp-posting-server "mail")

;; NNTP server for news posting. Default: `nil'
;;
;(setq wl-nntp-posting-server "news.weird.com")

;; fancier summaries.  Default: ugly  :-)
;;
(setq wl-stay-folder-window t)
(setq wl-summary-default-number-column 6)
(setq wl-summary-width nil)
(setq wl-summary-line-format (concat "%n %T" "%P %[%20(%c %f%) %] %Y/%M/%D(%W)%h:%m %-8S %t%~\"%s\""))
(setq wl-summary-default-view 'sequence)

;; show recipient in summary %f column of all folders when sender is me
;;
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)
 
(setq wl-summary-move-order nil)	; just go to the next message...
(setq wl-summary-move-direction-toggle nil) ; and don't waffle!!!
(setq wl-summary-move-direction-downward t) ; just always go DOWN

(setq wl-summary-exit-next-move nil)	; don't move the Folder pointer on quit

(defun my-wl-summary-turn-off-disp-msg ()
  "Unconditionally turn off message display so that I don't fat-finger myself
into too much confusion."
  (interactive)
  (wl-summary-toggle-disp-msg 'off))

(define-key wl-summary-mode-map "\C-x1" 'my-wl-summary-turn-off-disp-msg)

(define-key wl-summary-mode-map "c" 'wl-jump-to-draft-buffer)
(define-key wl-summary-mode-map "b" 'wl-summary-prev-page)
(define-key wl-summary-mode-map "g" 'wl-summary-sync-force-update)
(define-key wl-summary-mode-map "G" 'wl-summary-goto-folder)
;(define-key wl-summary-mode-map "s" 'wl-summary-save)

(defun my-wl-summary-exec-and-rescan ()
  "Run `wl-summary-exec' and then immediately run `wl-summary-sync-force-update'."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  (wl-summary-exec)
  (wl-summary-sync-force-update))

;; turn off dangerous commands with too-simple-to-hit keys
;;
(define-key wl-summary-mode-map "x" nil)
(define-key wl-summary-mode-map "X" 'my-wl-summary-exec-and-rescan)

;; XXX GRRR!  It seems this is impossible to do from here!
;; 
;(define-key mime-view-mode-map "c" 'mime-preview-toggle-content)
;(define-key mime-view-mode-map "h" 'mime-preview-toggle-header)

;(setq wl-thread-insert-opened t)	; XXX do we want to see the opened threads?

;(setq wl-auto-prefetch-first nil)	; is the default
;(setq wl-auto-select-first nil)	; is the default
(setq wl-auto-select-next 'skip-no-unread)
(setq wl-message-buffer-prefetch-depth 0)
(setq wl-message-buffer-prefetch-threshold 1000000)

(setq elmo-message-fetch-threshold 1000000)

;; default is blank (white on white) on monochrome displays!
;;
(setq wl-summary-flag-alist
      '((important "black" "I")
	(private "black" "P")
	(todo "black" "T")
	(business "black" "W")))

;; XXX or maybe I should make a link between .xface and .face?
;;
(setq wl-x-face-file "~/.face")

;; enable WL as the defautl mail composer
(if (boundp 'mail-user-agent)	; from simple.el in emacs-21
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; keep all my sent mail in one place for now....
;;
(setq wl-fcc "%inbox/Sent@mail.weird.com")

;; this should work for me, but it won't work for everyone
;; (and it especially won't work for mobile laptop users!)
;;
(setq wl-envelope-from (concat (user-login-name) "@" (system-name)))

;; a good default, but may be adapted by wl-draft-config-alist as below
;;
(setq wl-from "\"Greg A. Woods\" <woods@weird.com>")

;; wl-draft-config-exec really should be done as early as possible, even with
;; the "reply" hack, not as late as possible!
;;
(remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

;; the next is probably useless for me, though it may help if I ever learn to
;; use drafts much...
;;
(add-hook 'wl-draft-reedit-hook 'wl-draft-config-exec)

;; add a (pgp-sign . BOOL)
(unless (assq 'pgp-sign wl-draft-config-sub-func-alist)
  (wl-append wl-draft-config-sub-func-alist
	     '((pgp-sign . mime-edit-set-sign))))

;; add a (pgp-encrypt . BOOL)
(unless (assq 'pgp-encrypt wl-draft-config-sub-func-alist)
  (wl-append wl-draft-config-sub-func-alist
	     '((pgp-encrypt . mime-edit-set-encrypt))))

;; try to predict who I should be....
;; (XXX there's probably a more efficient way to write this)
;;
;; Note there's also a hack to set wl-smtp-posting-server using this...
;;
(setq wl-draft-config-alist
      '((reply "From: Andreas Wrede"
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Organization" . "Planix, Inc."))
	(reply "From: .*@.*aci\\.on\\.ca"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%inbox/list-archive/wl-en@" wl-draft-parent-folder)
         ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>"))
	(reply "Delivered-To: wl-en@lists.airs.net"
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>"))
	((string-match "^%inbox/planix.*@mail\\.weird\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*@mail\\.planix\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*:woods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@aci.on.ca>"))
	((string-match "^%.*:gwoods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@aci.on.ca>"))
	((or t)
	 (pgp-sign . t)
	 mime-edit-insert-signature)))

(setq wl-insert-message-id nil)		; let our MTA do it....

(setq signature-insert-at-eof t)
(setq signature-file-alist
      '((("From" . "@planix\\.") . "~/.signature-planix.com")
	(("From" . ".") . "~/.signature")))

;; mail-sent-via is a big useless pile of crap.
;; 
(define-key wl-draft-mode-map "\C-c\C-v" nil)

(setq wl-default-folder "^%INBOX@mail.weird.com")

;; since we do (set-language-environment "Latin-1") in .emacs....
;;
(setq default-mime-charset-for-write 'latin-1)

;; try to keep deleted messages in the Trash folder on the same host
;;
(setq wl-dispose-folder-alist
      '(("^%inbox.Trash@" . remove)	; this one must come first
	("^%INBOX@mail.weird.com" . "%inbox/Trash@mail.weird.com")
	("^%inbox.*@mail.weird.com" . "%inbox/Trash@mail.weird.com")
	("^%INBOX@building.weird.com" . "%inbox/Trash@building.weird.com")
	("^%inbox.*@building.weird.com" . "%inbox/Trash@building.weird.com")
	("^%INBOX@mail.planix.com" . "%inbox.Trash@mail.planix.com:993!")
	("^%inbox.*@mail.planix.com" . "%inbox.Trash@mail.planix.com:993!")
	("^%INBOX@mailbox.aci.on.ca" . "%inbox/Trash@mailbox.aci.on.ca:993!")
	("^%inbox.*@mailbox.aci.on.ca" . "%inbox/Trash@mailbox.aci.on.ca:993!")
	("^%INBOX:gwoods@mailbox.aci.on.ca" . "%inbox/Trash:gwoods@mailbox.aci.on.ca:993!")
	("^%inbox.*:gwoods@mailbox.aci.on.ca" . "%inbox/Trash:gwoods@mailbox.aci.on.ca:993!")
	("^-" . remove)
	("^@" . remove)
	("^\\+trash" . remove)
	("^\\+" . trash)))

;; Swap function of 'n' ('p') and 'N' ('P') in Summary and Folder modes
;;
;(define-key wl-folder-mode-map "n" 'wl-folder-next-unread)
;(define-key wl-folder-mode-map "N" 'wl-folder-next-entity)
;(define-key wl-folder-mode-map "p" 'wl-folder-prev-unread)
;(define-key wl-folder-mode-map "P" 'wl-folder-prev-entity)
;;
;(define-key wl-summary-mode-map "n" 'wl-summary-down)
;(define-key wl-summary-mode-map "N" 'wl-summary-next)
;(define-key wl-summary-mode-map "p" 'wl-summary-up)
;(define-key wl-summary-mode-map "P" 'wl-summary-prev)
