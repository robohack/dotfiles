;;;;
;;;;	.wl.el - Wanderlust custom configuration
;;;;
;;;;#ident	"@(#)HOME:.wl	28.2	08/07/14 17:10:36 (woods)"
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

;; turn off scoring for speed -- I never use it anyway
;;
(setq wl-use-scoring nil)

;; don't leave my passwords sitting in memory too long!
;;
(setq elmo-passwd-life-time 14400)	; 4 hrs

;; let's try this for use with wl-refile-guess-by-from and use of "%inbox/from"
;; as wl-refile-default-from-folder in particular since it doesn't seem to be
;; able to do anything any smarter than to concatonate the mailbox string onto
;; this prefix, thus there's no way to specify a server name in the default.
;;
;; a better solution would be to use a call to `format' to expand
;; wl-refile-default-from-folder, with "%s" in the position where the folder
;; name should be placed
;;
(setq elmo-imap4-default-server "mailbox.weird.com")

;; Use SSL connection
;(setq elmo-imap4-default-stream-type 'starttls)
;(setq elmo-imap4-default-port 993)
(setq elmo-imap4-default-stream-type nil)
(setq elmo-imap4-default-port 143)

;; password always in raw format for my servers
;;
(setq elmo-imap4-default-authenticate-type 'clear)

;; XXX this doesn't quite work right to turn on automatic signing, especially
;; with multiple keys.
;;
;(setq mime-edit-pgp-processing '(sign))

;; XXX almost no modern GUI-based reader and re-assemble split messages!
;;
(setq mime-edit-split-message nil)

;; 
;;
;;(setq filename-filters '(filename-special-filter))

(setq mime-setup-enable-pgp t)		; it is the default
(setq pgg-default-scheme 'pgp5)		; for composing
(setq pgg-scheme 'pgp5)			; for verify/decrypt
(setq pgg-cache-passphrase t)		; it is the default
(setq pgg-passphrase-cache-expiry 14400); 4 hrs
;(setq pgg-read-passphrase 'read-passwd)	; it is the default?
(setq pgg-read-passphrase 'read-string)	; XXX for debugging

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
(setq wl-summary-persistent-mark-priority-list '(deleted
						 flag
						 new
						 answered
						 forwarded
						 unread))

;; show recipient in summary %f column of all folders when sender is me
;;
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

(setq wl-summary-move-direction-toggle nil) ; and don't waffle!!!
(setq wl-summary-move-direction-downward t) ; just always go DOWN

(setq wl-summary-exit-next-move nil)	; don't move the Folder pointer on quit

(defun my-wl-summary-turn-off-disp-msg ()
  "Unconditionally turn off message display so that I don't fat-finger myself
into too much confusion."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  (delete-other-windows))

(define-key wl-summary-mode-map "\C-x1" 'my-wl-summary-turn-off-disp-msg)

(require 'advice)
(defadvice wl-summary-sync-force-update (before my-wl-summary-sync-force-update activate)
  "Turn off message display before updating the summary."
  (wl-summary-toggle-disp-msg 'off))

(define-key wl-summary-mode-map "c" 'wl-jump-to-draft-buffer)
(define-key wl-summary-mode-map "b" 'wl-summary-prev-page)
(define-key wl-summary-mode-map "g" 'wl-summary-sync-force-update)
(define-key wl-summary-mode-map "G" 'wl-summary-goto-folder)
;(define-key wl-summary-mode-map "s" 'wl-summary-save)

(define-key wl-summary-mode-map "\M-n" 'wl-summary-down)
(define-key wl-summary-mode-map "\M-p" 'wl-summary-up)

;; make 'q' in a virtual folder (created by 'V') return to the folder from
;; which it was created, just as 'C-u V' does, instead of quitting to the
;; Folder buffer  (from Yoichi NAKAYAMA on the wl-en list)
;;
(add-hook 'wl-summary-prepared-hook
	  (lambda ()
	    (setq wl-summary-buffer-exit-function
		  (when (eq 'filter
			    (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
		    'wl-summary-unvirtual))))

;; Just setting wl-summary-subject-function to point to the identity function
;; will work, but perhaps other callers of the munging version of this function
;; would also appreciate just seeing the original text un-touched.  At worst
;; this might botch subject-based threading,
;;
(setq wl-summary-subject-function 'identity)
(defun wl-summary-default-subject (subject-string)
  subject-string)

(defun my-wl-summary-exec-and-rescan ()
  "Run `wl-summary-exec' and then immediately run `wl-summary-sync-force-update'."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  ;;
  ;; XXX this is only a tiny example of what I really want
  ;;
  ;; I would like to see something more like the little confirmation display
  ;; created by wl-draft-send-confirm such that the number of deletes, moves,
  ;; copies, raw deletes, etc. can be shown, and optionally detailed, before
  ;; their execution is confirmed.
  ;;
  (if (yes-or-no-p (format "Execute %d marks? " (length wl-summary-buffer-temp-mark-list)))
      (wl-summary-exec))
  (wl-summary-sync-force-update))

;; turn off dangerous commands with too-simple-to-hit keys
;;
(define-key wl-summary-mode-map "x" nil)
(define-key wl-summary-mode-map "X" 'my-wl-summary-exec-and-rescan)

;; XXX GRRR!  It seems this is impossible to do from here!
;;
;(define-key mime-view-mode-map "c" 'mime-preview-toggle-content)
;(define-key mime-view-mode-map "h" 'mime-preview-toggle-header)

(setq wl-thread-insert-opened t)	; XXX do we want to see the opened threads?

;(setq wl-auto-prefetch-first nil)	; is the default
;(setq wl-auto-select-first nil)	; is the default
(setq wl-auto-select-next 'skip-no-unread)
(setq wl-message-buffer-prefetch-depth 0)
(setq wl-message-buffer-prefetch-threshold 1000000)

(setq elmo-message-fetch-threshold 1000000)

;; additional fields to retrieve when fetching headers
;;
;; (also appear in the '?' and 'V' lists, though arbitrary fields can be given
;; to those functions to do a direct search on the server)
;;
;; The following list is automatically prepended:
;;
;;	    '("Subject" "From" "To" "Cc" "Date"
;;	      "Message-Id" "References" "In-Reply-To")
;;
(setq elmo-msgdb-extra-fields '("Delivered-To"
				"List-Id"
				"Precedence"
				"Received"
				"Reply-To"
				"Return-Path"
				"Sender"
				"X-Priority"))

;; XXX over-ride the search function to give a better prompt:
;;
(defun wl-read-search-condition (default)
  "Read search condition string interactively."
  (wl-read-search-condition-internal "Search by (arbitrary header allowed)" default))

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

(setq wl-message-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq mime-view-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq wl-message-ignored-field-list
      '(".*Received:"
	".*Path:"
	".*Sender:"			; include X-Sender, X-X-Sender, etc.
	".*Host:"
	"^[cC]ontent.*:"		; irrelevant!  :-)
	"^Content-Type:"
	"^DomainKey.*:"			; bogus junk
	"^Errors-To:"
	"^In-Reply-To:"			; just another message-id
	"^Lines:"
	"^List-.*:"			; rfc????
	"^Message-I[dD]:"		; RFC 2036 too!
	"^[mM][iI][mM][eE]-[vV]ersion:"	; irrelevant!  :-)
;;;	"^Precedence:"
	"^References:"
	"^Replied:"
	"^Status:"
	"^Thread-Index:"
	"^X-Accept-Language:"
	"^X-BeenThere:"			; mailman?
	"^X-Cam.*:"			; some stupid virus scanner
	"^X-CanItPRO.*:"
	"^X-Greylist.*:"
	"^X-Hashcash.*:"		; ???
	"^X-MAil-Count:"		; fml?
	"^X-MIME-Autoconverted:"
	"^X-ML.*:"			; fml
	"^X-MS-.*:"
	"^X-Mailman.*:"			; mailman
	"^X-OriginalArrivalTime:"
	"^X-PMAS-.*:"
	"^X-PMX.*:"
	"^X-RPI.*:"
	"^X-SKK:"
	"^X-Scanned.*:"
	"^X-Sieve:"			; cyrus
	"^X-Spam.*:"
	"^X-VM-.*:"
	"^X-Virus.*:"
	"^Xref:"
;	"^X-Original-To:"		; fml?
	))

;; we do want all the received headers on forwarded messages, but not any other
;; locally added headers
;;
(setq wl-ignored-forwarded-headers
      "\\(return-path\\|x-sieve\\|x-uidl\\)")

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

;; delete myself from the recipient list(s?) in draft messages
;;
;; One or the other of the following will also have to be set if you use
;; multiple e-mail addresses:
;;
;; `wl-user-mail-address-list'
;; `wl-user-mail-address-regexp' (supersedes the first one)
;;
(setq wl-draft-always-delete-myself t)

;;
;; modified version of default body citation func
;;
(defun my-wl-default-draft-cite ()
  (let ((mail-yank-ignored-headers "[^:]+:")
	(mail-yank-prefix "> ")
	date from cite-title)
    (save-restriction
      (if (< (mark t) (point))
	  (exchange-point-and-mark))
      (narrow-to-region (point)(point-max))
      (setq date (std11-field-body "date")
	    subject (std11-field-body "subject")
	    from (std11-field-body "from")))
    (when (or date from)
      (insert (format "At %s, %s wrote:\nSubject: %s\n"
		      (or date "some time ago")
		      (if wl-default-draft-cite-decorate-author
			  (funcall wl-summary-from-function
				   (or from "you"))
			(or from "you"))
		      (or subject "(unkown)"))))
    (mail-indent-citation)))

(setq wl-draft-cite-function 'my-wl-default-draft-cite)

; (setq wl-draft-reply-without-argument-list
;       '(("Reply-To" ("Reply-To") nil nil)
;        ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
;        ("From" ("From") nil nil)))
;
; (setq wl-draft-reply-with-argument-list
;       '(("Followup-To" nil nil ("Followup-To"))
; 	("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
; 	("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
; 	("From" ("From") ("To" "Cc") ("Newsgroups"))))

;; pull in all the MIME stuff
(require 'elmo-mime)
(require 'mel)

;; keep all my sent mail in one place for now....
;;
;(setq wl-fcc "%inbox/Sent@mailbox.weird.com")
(setq wl-fcc "+sent")

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

;; also turn on flyspell explicitly
;;
(add-hook 'wl-mail-setup-hook (lambda ()
				(flyspell-mode)))


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

;(defun my-mime-edit-set-1st-text-qp-encoding ()
;  "Try setting the default encoding of the first text tag to quoted-printable"
;  ;; XXX first find the first MIME tag which should have been inserted by
;  ;; mime-edit-insert-signature
;  (mime-edit-define-encoding "quoted-printable"))

(defadvice mime-edit-insert-signature (after my-mime-edit-signature-set-qp-encoding activate)
  "Add quoted-printable encoding to the MIME tag for the signature file."
  (mime-edit-define-encoding "quoted-printable"))

;; try to use some hand-coded rules instead of relying entirely on BBDB
;;
;; first though make sure these rules come after the auto-learned subject and
;; msgid (thread-like) rules, but after the from and tocc auto-learned so that
;; the auto-learning rules (that make sense) have precedence
;;
(setq wl-refile-guess-functions
      '(wl-refile-guess-by-subject
	wl-refile-guess-by-msgid
	wl-refile-guess-by-rule
	wl-refile-guess-by-from
	wl-refile-guess-by-history))
;;
;; Sadly this is a prefix, not a format pattern, so no server name can be included
;;
(setq wl-refile-default-from-folder "%inbox/from")
;;
(setq wl-refile-rule-alist
      '(("Subject"
	 ("^\\[Acct Event\\] " . "%inbox/planix/aci/db-trouble-tickets@mailbox.weird.com"))
	("Subject"
	 ("Aurora Cable Abuse Department" . "%inbox/planix/aci/abuse@mailbox.weird.com"))
	("Subject"
	 ("\\[SpamCop " . ("To"
			     (".*@.*aci\\.on\\.ca" . "%inbox/planix/aci/abuse@mailbox.weird.com"))))
	(("To" "Cc" "From" "Sender")
	 ("\\(jennifer@.*\\(wrede\\|planix\\).*\\|jen\\(nifer\\)?\\.wrede@.*\\)"
	  . "%inbox/from/jennifer@mailbox.weird.com"))
	(("To" "Cc")
	 ("\\(abuse\\|[hp]ostmaster\\|info\\|support\\)@.*aci\\.on\\.ca"
	  . "%inbox/planix/aci/\\1@mailbox.weird.com"))
	("From"
	 ("\\(abuse\\|[hp]ostmaster\\|info\\|support\\)@.*aci\\.on\\.ca"
	  . "%inbox/planix/aci/\\1@mailbox.weird.com"))))

;; try to predict who I should be....
;; (XXX there's probably a more efficient way to write this)
;;
;; Note there's also a hack to set wl-smtp-posting-server using this...
;;
(setq wl-draft-config-alist
      '((reply "From: Andreas Wrede"
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply "From: .*@.*planix\\."
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply "From: .*@.*\\(aci\\|opc\\)\\.on\\.ca"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("X-Priority" . "2")
	 ("Organization" . "Planix, Inc."))
	(reply "From: .*@.*teloip\\.com"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply "From: .*@.*\\(lawyermediator\\|gelmanlaw\\)\\.ca"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("X-Priority" . "1"))
	((string-match "^%inbox/planix.*@mail\\.weird\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*@mail\\.planix\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*:woods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@aci.on.ca>")
	 ("Precedence" . "first-class"))
	((string-match "^%.*:gwoods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
	 ("Precedence" . "first-class"))
	;; mailing list:  info-cyrus
	((or (string-match "^%inbox/Lists-In/cyrus-lists@"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/info-cyrus@"
			   wl-draft-parent-folder))
         ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>"))
	(reply "Sender: info-cyrus-bounces@lists.andrew.cmu.edu"
	 ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>"))
	(reply "To: woods-cyrus@weird.com"
	 ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>"))
	;; mailing list:  emacs-mime-en
	((or (string-match "^%inbox/Lists-In/emacs-mime-en-l@"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/emacs-mime-en@"
			   wl-draft-parent-folder))
         ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>"))
	(reply "Delivered-To: emacs-mime-en@m17n.org"
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>"))
	(reply "To: woods-emacs-mime-en-l@weird.com"
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>"))
	;; mailing list:  wl-en
	((or (string-match "^%inbox/Lists-In/wl-en-l@"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/wl-en@"
			   wl-draft-parent-folder))
         ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>"))
	(reply "Delivered-To: wl-en@lists.airs.net"
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>"))
	(reply "To: woods-wl-en-l@planix.com"
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>"))
	;; defaults for everything
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

;; do the right thing....
;;
(define-key wl-draft-mode-map "\C-xk" 'wl-draft-kill)

(setq wl-default-folder "^%INBOX@mailbox.weird.com")

;; since we do (set-language-environment "Latin-1") in .emacs....
;;
(setq default-mime-charset-for-write 'latin-1)

;; try to keep deleted messages in the Trash folder on the same host
;;
;; it would be nice if there were some nifty way to simply say something
;; like "everywhere I have an INBOX, assume 'd' should refile into the
;; "Trash" folder living directly under that INBOX, including for all
;; other sub-folders of that INBOX" without having to enumerate them all
;; (and take special consideration of the ones where I have to use a
;; different port or whatever)
;;
(setq wl-dispose-folder-alist
      '(("^%inbox.*Trash@" . remove)	; this one must come first
	("^%INBOX$" . "%inbox/Trash")
	("^%inbox[^@]*$" . "%inbox/Trash")
	("^%INBOX@mailbox.weird.com" . "%inbox/Trash@mailbox.weird.com")
	("^%inbox.*@mailbox.weird.com" . "%inbox/Trash@mailbox.weird.com")
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

;;; For junk handling
;;;
;;; see also wl-spam.el
;;;
;;;	(setq elmo-spam-scheme 'sa)
;;;	(require 'wl-spam)
;;;	(setq wl-spam-folder "%INBOX.spam")
;;;

;; first set up the local junk folder name
;;
(setq wl-junk-folder "+junk")

;; in the style of wl-dispose-folder-alist
;;
;; (see the comment about manual enumeration for wl-dispose-folder-alist too)
;;
;; XXX ideally what I want to do when I press 'J' inside a Junk folder is to
;; move the message back up to the parent INBOX (undoing any IMAP flags I might
;; someday set at the same time)
;;
(setq wl-junk-folder-alist
      '(("^%inbox.*Junk@" . null)	; this one must come first
	("^+junk" . null)		; this one too?
	("^\\(/[^/]*/\\)?%INBOX$" . "%inbox/Junk")
	("^%inbox[^@]*$" . "%inbox/Junk")
	("^%INBOX@mailbox.weird.com" . "%inbox/Junk@mailbox.weird.com")
	("^%inbox.*@mailbox.weird.com" . "%inbox/Junk@mailbox.weird.com")
	("^%INBOX@mail.planix.com" . "%inbox.Junk@mail.planix.com:993!")
	("^%inbox.*@mail.planix.com" . "%inbox.Junk@mail.planix.com:993!")
	("^%INBOX@mailbox.aci.on.ca" . "%inbox/Junk@mailbox.aci.on.ca:993!")
	("^%inbox.*@mailbox.aci.on.ca" . "%inbox/Junk@mailbox.aci.on.ca:993!")
	("^%INBOX:gwoods@mailbox.aci.on.ca" . "%inbox/Junk:gwoods@mailbox.aci.on.ca:993!")
	("^%inbox.*:gwoods@mailbox.aci.on.ca" . "%inbox/Junk:gwoods@mailbox.aci.on.ca:993!")))

;; essentially a copy of wl-summary-get-dispose-folder (they could probably be
;; merged together into something more generic)
;;
;; XXX I'm not sure I got this right, but so far it seems to work well enough...
;;
(defun wl-summary-get-junk-folder (folder)
  (if (string= folder wl-junk-folder)
      'null
    (let* ((type (or (wl-get-assoc-list-value wl-junk-folder-alist folder)
		     'trash)))
      (cond ((stringp type)
	     type)
	    ((or (equal type 'remove) (equal type 'null))
	     'null)
	    (t;; (equal type 'junk)
	     (let ((junk-folder (wl-folder-get-elmo-folder wl-junk-folder)))
	       (unless (elmo-folder-exists-p junk-folder)
		 (if (y-or-n-p
		      (format "Junk Folder %s does not exist, create it? "
			      wl-junk-folder))
		     (elmo-folder-create junk-folder)
		   (error "Junk Folder is not created"))))
	     wl-junk-folder)))))

;; the Junk action.
;;
;; For now we just move them into the appropriate Junk folder, but eventually
;; we might at least add IMAP flags so that they appear as junk to other types
;; of clients, and also we might do some post-processing and perhaps possibly
;; forward some of them off to SpamCop and/or the source's contacts, etc.
;;
(defun wl-summary-exec-action-junk (mark-list)
  (wl-summary-move-mark-list-messages mark-list
				      (wl-summary-get-junk-folder
				       (wl-summary-buffer-folder-name))
				      "Junking messages..."))

;; a font face for Junk lines in the Summary
(wl-defface wl-highlight-summary-junk-face
  '((((type tty)
      (background dark))
     (:foreground "blue"))
    (((class color))
     (:foreground "LightSlateGray")))
  "Face used for displaying messages mark as Junk."
  :group 'wl-summary-faces
  :group 'wl-faces)

(defvar wl-junk-mark-action-list
  '(("J"
     junk
     nil
     wl-summary-register-temp-mark
     wl-summary-exec-action-junk
     wl-highlight-summary-junk-face
     "Mark messages as Junk."))
  "A variable to define a Mark & Action for Junk handling.
Append this value to `wl-summary-mark-action-list'.

See `wl-summary-mark-action-list' for the details of each element.")

(setq wl-summary-mark-action-list (append
				   wl-summary-mark-action-list
				   wl-junk-mark-action-list))

;; call this to re-define all the wl-summary-exec-action-* (and
;; wl-summary-*) functions, including our new one
;;
(wl-summary-define-mark-action)

;; and hook our new junker into the summary keymap
;;
(define-key wl-summary-mode-map "J" 'wl-summary-junk)

;; XXX is there some really _EASY_ way to hook into the menus too?


;; try to create sub-folders for every level in IMAP using a recursive RE
;;
;; this doesn't properly take into account the actual hierarchy separator
;; specified by the IMAP server (it just allows either, and would no doubt be
;; royally messed up if the char that's not the true separator appears in a
;; folder name).
;;
;; XXX this recursive RE doesn't exactly work right.  It has the desired
;; effect, but only _after_ WL has learned the underlying structure, which it
;; seems can only currently be done by commenting out the recursive RE and
;; leaving just the non-recursive non-hierarchical match for IMAP folders
;; (i.e. the line below marked "XXX is this necessary?") while re-opening
;; all folders with "ESC RET", then putting it back and doing it again.
;;
(setq wl-folder-hierarchy-access-folders
      '("^%\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
	"^%[^/.]+\\(:\\|@\\|$\\)"	; XXX is this necessary?
	"^-[^.]*\\(:\\|@\\|$\\)"
	"^@$"
	"^'$"))

;; no sense normally reading e-mail on a narrow screen! :-)
;;
(setq wl-folder-window-width 30)	; default is just 20
;;
;; or maybe this would be better, but does it work???
;;
;; XXX really wish I could visit multiple folders, perhaps one per frame!
;;
;(setq wl-folder-use-frame t)

;; also do this to avoid having groups show up before the mailbox with
;; the same name as the group
;;
;; I wish wl-folder-update-recursive-current-entity would start with
;; things in the same order as wl-fldmgr-sort does, though I suppose I
;; could wrap the former with some advice to call the latter after it is
;; done to achieve the same effect.
;;
(setq wl-fldmgr-sort-group-first nil)

;; it seems "difficult" to get emacs to always show the *Completions* buffer in
;; the most appropriate place (it all too often seems to want to use the narrow
;; Folder window on the left side of the frame), so we give up and pop a new
;; frame for it....
;;
;(setq special-display-regexps
;      (cons '(".*\\*Completions\\*.*"
;	      '((top . 0)
;		(left . -1)
;		(height . 20)
;		(width . 80)
;		(tool-bar-lines . 0)
;		(menu-bar-lines . 0)))
;	    special-display-regexps))
;
;(setq special-display-buffer-names
;      (cons "*Completions*" special-display-buffer-names))

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

;(defface wl-highlight-summary-deleted-face
;  '((((class color)
;      (background dark))
;     (:foreground "Red" :bold t :italic t :strike-through "OrangeRed"))
;    (((class color)
;      (background light))
;     (:foreground "IndiaRed3" :bold t :italic t :strike-through "black"))
;    (t
;     (:bold t :italic t :strike-through t)))
;  "Mail deleted face")

;; Cc: WanderLust Users Mailing List (English) <wl-en@lists.airs.net>
;; Subject: Re: colorization/fontification of draft-mode buffer contents
;; From: Ron Isaacson <Ron.Isaacson@morganstanley.com>
;; Date: Sun, 22 Apr 2007 14:13:31 -0400
;; Message-Id: <3ggwt04xook.wl_Ron.Isaacson@morganstanley.com>
;;
;; This is likely to be resource-intensive on large draft buffers, of
;; course.
;;
;; XXX maybe it should only do something if the buffer has been modified by the
;; previous command?
;;
;(defun my-wl-draft-highlight ()
;  "wl-draft-highlight-and-recenter without the recenter"
;  (interactive "P")
;  (when wl-highlight-body-too
;    (let ((modified (buffer-modified-p)))
;      (unwind-protect
;	  (progn
;	    (put-text-property (point-min) (point-max) 'face nil)
;	    (wl-highlight-message (point-min) (point-max) t))
;	(set-buffer-modified-p modified)))))
;
;(defun my-wl-draft-turn-on-real-time-highlight ()
;  "add a buffer local hook on post-command-hook to call my-wl-draft-highlight"
;  (add-hook 'post-command-hook 'my-wl-draft-highlight nil t))
;
;(add-hook 'wl-mail-setup-hook 'my-wl-draft-turn-on-real-time-highlight)


;;; Wanderlust draft-mode highligting
;;;
(defun wl-draft-inside-header-p (pos)
  "POS"
  (save-excursion
    (goto-char pos)
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$")
     nil t)))

(defun wl-draft-highlight-region (start end)
  "START END"
  (interactive "r")
  (let ((modified (buffer-modified-p))
	(goto-next-invisible-change
	 (lambda (end)
	   (goto-char (or (next-single-property-change
			   (point) 'invisible nil end)
			  end)))))
    (unwind-protect
	(save-excursion
	  (setq start
		(progn (goto-char start) (line-beginning-position)))
	  (setq end
		(progn (goto-char end) (forward-line) (point)))
	  (goto-char start)
	  (when (get-text-property (point) 'invisible)
	    (funcall goto-next-invisible-change end))
	  (while (< (point) end)
	    (let ((sub-start (point))
		  (sub-end (funcall goto-next-invisible-change end)))
	      (put-text-property sub-start sub-end 'face nil)
	      (wl-highlight-message sub-start sub-end t
				    (not (wl-draft-inside-header-p sub-start)))
	      (funcall goto-next-invisible-change end))))
      (set-buffer-modified-p modified))))

(defun wl-draft-highlight-buffer ()
  (interactive)
  (wl-draft-highlight-region (point-min) (point-max)))

;; jit-lock enabled
(add-hook 'wl-draft-mode-hook
	  (lambda ()
	    (jit-lock-register 'wl-draft-highlight-region)))
