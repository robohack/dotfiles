;;;;
;;;;	.wl.el - Wanderlust custom configuration
;;;;
;;;;#ident	"@(#)HOME:.wl	32.3	11/06/04 14:06:07 (woods)"
;;;;

;; XXX look for ideas in <URL:http://triaez.kaisei.org/~kaoru/emacsen/startup/init-mua.el>

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
;; (and it must be set _before_ mime-view (or really semi-setup) is loaded,
;; which is why it's up here at the top of ~/.wl)
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
;; able to do anything any smarter than to concatenate the mailbox string onto
;; this prefix, thus there's no way to specify a server name in the default.
;;
;; a better solution would be to use a call to `format' to expand
;; wl-refile-default-from-folder, with "%s" in the position where the folder
;; name should be placed
;;
(setq elmo-imap4-default-server "mailbox.weird.com")

;; dunno why this is suddenly necessary
;; `mail-local-domain-name' comes from my ~/.emacs.el
;;
(setq wl-local-domain (or mail-local-domain-name "weird.com"))

;; Use SSL connection
;(setq elmo-imap4-default-stream-type 'starttls)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-default-port 993)
;; ... else don't use SSL
;(setq elmo-imap4-default-stream-type nil)
;(setq elmo-imap4-default-port 143)

;; password always in raw format for my servers
;;
(setq elmo-imap4-default-authenticate-type 'clear)

;; this is needed to make sure filenames created to save attachments are sane
;;
(setq filename-filters '(filename-special-filter))

;; Directory where icons are placed.
;; XXX should be set by package install, but seems not to be)
;;
(setq wl-icon-directory "/usr/pkg/share/wl")

;; SMTP server for mail posting. Default: `nil'
(setq wl-smtp-posting-server "localhost")

;; NNTP server for news posting. Default: `nil'
;;
;(setq wl-nntp-posting-server "news.weird.com")

;; prefetch everything that's uncached, not just unread-uncached (U) and
;; new-uncached (N)
;;
(setq wl-summary-incorporate-marks
      (list wl-summary-uncached-mark
	    wl-summary-new-uncached-mark
	    wl-summary-unread-uncached-mark
	    wl-summary-answered-uncached-mark))

(setq wl-stay-folder-window t)

;; support for marking messages addressed "to-me" in the Summary buffer.
;;
;; By Ron Isaacson with thanks to Erik Hetzner for some fixes:
;;
(defun wl-summary-line-to-me ()
  "Return `*' if current message is addressed to me, else ` '."
  (let ((all-addresses (append
			(elmo-message-entity-field wl-message-entity 'to t)
			(elmo-message-entity-field wl-message-entity 'cc t)))
	(to-me nil))
    (while (and all-addresses
		(not to-me))
      (setq to-me (wl-address-user-mail-address-p (car all-addresses)))
      (setq all-addresses (cdr all-addresses)))
    (if to-me "*" " ")))

;; add "%E" to `wl-summary-line-format' to invoke `wl-summary-line-to-me'
;; 
(setq wl-summary-line-format-spec-alist
      (put-alist '?E
		 '((wl-summary-line-to-me))
		 wl-summary-line-format-spec-alist))

;; fancier summaries.  Default: ugly  :-)
;;
(setq wl-summary-default-number-column 6) ; message numbers are often 6 digits with Cyrus IMAP
(setq wl-summary-width nil)
;; after changing `wl-summary-line-format' you need to exit and re-enter the
;; Summary buffer to update the displayed format.
(setq wl-summary-line-format (concat "%n %T"
				     "%P %E %[%20(%c %f%) %] %Y/%M/%D(%W)%h:%m %-8S %t%~\"%s\" \t"))
(setq wl-summary-default-view 'sequence)
(setq wl-summary-persistent-mark-priority-list '(killed
						 deleted
						 junk
						 flag ; user-defined flag!?!?!?
						 important
						 private
						 todo
						 business
						 ingore
						 new
						 answered
						 forwarded
						 unread))

;; default is blank (white on white) on monochrome displays!
;;
;; XXX for this to work correctly (i.e. be display-independent), the COLOR
;; field must be a full display face created by `defface'.
;;
;; XXX in the mean time maybe we should also check the result of:
;;
;;	(frame-parameter nil 'display-type)
;;
(if (display-color-p)
    (setq wl-summary-flag-alist '((ignore "black" " ")
				  (important "black" "I")
				  (private "blue" "P")
				  (todo "red" "T")
				  (business "green" "W")
				  (junk "orange" "J")
				  (killed "grey" "K")
				  (forwarded "grey" "F")
				  (redirected "grey" "R")))
  (setq wl-summary-flag-alist '((ignore "black" " ")
				(important "black" "I")
				(private "black" "P")
				(todo "black" "T")
				(business "black" "W")
				(junk "black" "J")
				(killed "black" "K")
				(forwarded "black" "F")
				(redirected "black" "R"))))

;; XXX unfortunately just calling `defface' again to try to redefine a face
;; does not work.
;;
;(wl-defface wl-highlight-summary-deleted-face
;  '((((type tty)
;      (background dark))
;     (:foreground "red"))
;    (((class grayscale)
;      (background dark))
;     (:foreground "grey"))
;    (((class mono)
;      (background dark))
;     (:foreground "white" :strike-through t))
;    (((class mono)
;      (background light))
;     (:foreground "black" :strike-through t))
;    (((class color)
;      (background dark))
;     (:foreground "red" :strike-through "OrangeRed"))
;    (((class color)
;      (background light))
;     (:foreground "IndiaRed3" :strike-through "black"))
;    (t
;     (:strike-through t)))
;  "Face used for displaying messages that have been marked to be deleted."
;  :group 'wl-summary-faces
;  :group 'wl-faces)

(require 'wl-highlight)
(if (display-color-p)
    (if (eq (frame-parameter nil 'background-mode) 'light)
	(set-face-attribute 'wl-highlight-summary-deleted-face nil
			    :foreground "red"
			    :strike-through "OrangeRed")
      (set-face-attribute 'wl-highlight-summary-deleted-face nil
			  :foreground "IndiaRed3"
			  :strike-through "black"))
  (set-face-attribute 'wl-highlight-summary-deleted-face nil
		      :strike-through t))

;; show recipient in summary %f column of all folders when sender is me
;;
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

(setq wl-summary-move-direction-toggle nil) ; and don't waffle!!!
(setq wl-summary-move-direction-downward t) ; just always go DOWN

(setq wl-summary-exit-next-move nil)	; don't move the Folder pointer on quit

;; to decode encoded words within quoted strings in headers....
;;
;; NOTE:  normally this is contrary to the standards, I think, so it should not
;; be necessary, but of course some stupid mailers always quote every display
;; name regardless of whether it needs quoting or not, and perhaps sometimes
;; even when it must not be quoted.
;;
;;(setq mime-header-accept-quoted-encoded-words t)

(defun my-wl-summary-turn-off-disp-msg ()
  "Unconditionally turn off message display so that I don't fat-finger myself
into too much confusion."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  ;; this used to effectively turn off the Folder window too by calling
  ;; (delete-other-windows), but with a wide display that's NOT what I want to do
  ;; any more.
  )

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


(setq wl-thread-insert-opened t)	; XXX do we want to see the opened threads?

;(setq wl-auto-prefetch-first nil)	; is the default
;(setq wl-auto-select-first nil)	; is the default

;; using `skip-no-unread' with the following is has unfortunate side effects if
;; the space bar is held down in auto-repeat mode as the confirmation character
;; is in fact "SPC".
;;
(setq wl-auto-select-next nil)		; is the default

;; found this on a Japanese mailing list with hints that this is the proper way
;; to use "always buffer local" `wl-summary-buffer-*-folder-function' variables
;; which according to the code will completely avoid triggering anything to do
;; with auto-selecting the next or previous folder when navigating past the end
;; or beginning of a "Summary" buffer
;;
(add-hook 'wl-summary-mode-hook 
	  '(lambda () 
		   (setq wl-summary-buffer-prev-folder-function 'ignore 
			 wl-summary-buffer-next-folder-function 'ignore)))

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

;; the following would require an .xbm filename in `x-face-default-xbm-file'
;;
;;	(add-hook 'wl-mail-setup-hook 'x-face-insert)
;;
;; so instead we use the old "X-Face:" literal text content file created by
;; compface...
;;
(setq wl-x-face-file "~/.face")
(add-hook 'wl-draft-insert-x-face-field-hook
	  (lambda nil
	    (x-face-insert wl-x-face-file)))

;; ~/.emacs.el should find and load x-face-e21 if it is available....
;;
(if (fboundp 'x-face-decode-message-header)
    (setq wl-highlight-x-face-function 'x-face-decode-message-header))

(if (fboundp 'x-face-save)
    (define-key wl-summary-mode-map "\C-x4s" 'x-face-save))
(if (fboundp 'x-face-ascii-view)
    (define-key wl-summary-mode-map "\C-x4a" 'x-face-ascii-view))

(if (fboundp 'x-face-insert)
    (define-key wl-draft-mode-map "\C-x4i" 'x-face-insert))
(if (fboundp 'x-face-show)
    (define-key wl-draft-mode-map "\M-\C-t" 'x-face-show))

;; for header effects
;;
;(setq wl-highlight-message-header-alist ...)

;; show all the headers except those we know we don't care about...
;;
(setq wl-message-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq mime-view-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq wl-message-ignored-field-list
      '("[^:]*Received:"
	"[^:]*Path:"
	"[^:]*Sender:"			; include X-Sender, X-X-Sender, etc.
	"[^:]*Host:"
	"^[cC]ontent[^:]*:"		; irrelevant!  :-)
	"^Content-Type:"
	"^DomainKey[^:]*:"		; bogus junk
	"^Errors-To:"
	"^In-Reply-To:"			; just another message-id
	"^Lines:"
	"^List-[^:]*:"			; rfc????
	"^Message-I[dD]:"		; RFC 2036 too!
	"^[mM][iI][mM][eE]-[vV]ersion:"	; irrelevant!  :-)
	"^References:"
	"^Replied:"
	"^Status:"
	"^Thread-Index:"
	"^X-Accept-Language:"
	"^X-BeenThere:"			; mailman?
	"^X-Cam[^:]*:"			; some stupid virus scanner
	"^X-CanItPRO[^:]*:"
	"^X-CHA:"
	"^X-CSC:"
	"^X-GMX[^:]*:"
	"^X-Greylist[^:]*:"
	"^X-Hashcash[^:]*:"		; ???
	"^X-IronPort[^:]*:"		; some silly AV crapware
	"^X-Junkmail[^:]*:"		; mirapoint???
	"^X-MAil-Count:"		; fml?
	"^X-MIME-Autoconverted:"
	"^X-Mirapoint[^:]*:"		; mirapoint???
	"^X-ML[^:]*:"			; fml
	"^X-MS-[^:]*:"
	"^X-Mailman[^:]*:"		; mailman
	"^X-OriginalArrivalTime:"
	"^X-PMAS-[^:]*:"
	"^X-PMX[^:]*:"
	"^X-Provags-[^:]*:"
	"^X-RPI[^:]*:"
	"^X-SKK:"
	"^X-SMTP-Spam-[^:]*:"
	"^X-Scanned[^:]*:"
	"^X-Sieve:"			; cyrus
	"^X-Spam[^:]*:"
	"^X-VM-[^:]*:"
	"^X-Virus[^:]*:"
	"^Xref:"
;	"^X-Original-To:"		; fml?
	))

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

;; we do want all the received headers on forwarded messages, but not any other
;; locally added headers
;;
(setq wl-ignored-forwarded-headers
      "\\(return-path\\|x-sieve\\|x-uidl\\)")

;; enable WL as the default mail composer
(if (boundp 'mail-user-agent)	; from simple.el in emacs-21
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; pull in all the MIME stuff (why does it seem we must do this?)
(require 'elmo-mime)
(require 'mel)
(require 'mime-edit)
(require 'mime-view)

;; Make MIME understand HTML while preferring the text version if one is
;; provided.
;;
(if (elisp-file-in-loadpath-p "w3m")	; could use (require 'mime-w3m nil t) instead
    (progn
      (require 'mime-w3m)))

(setq mime-view-type-subtype-score-alist
      '(((text . plain) . 4)
	((text . enriched) . 3)		; RFC 1896
	((text . richtext) . 2)		; RFC 1341/1521 (deprecated/obsolete)
	((text . html) . 1)		; Gak!
	(t . 0)))

;; to have text flowing automatically in display of emails in wanderlust
(autoload 'fill-flowed "flow-fill")
(add-hook 'mime-display-text/plain-hook
 	  (lambda ()
 	    (when (string= "flowed"
 			   (cdr (assoc "format"
 				       (mime-content-type-parameters
 					(mime-entity-content-type entity)))))
 	      (fill-flowed))))

;; XXX this doesn't quite work right to turn on automatic signing globally...
;;
;(setq mime-edit-pgp-processing '(sign))

(setq mime-setup-enable-pgp t)		; it is the default
;(setq pgg-default-scheme 'pgp5)		; for composing
;(setq pgg-scheme 'pgp5)			; for verify/decrypt
(setq pgg-default-scheme 'gpg)		; for composing
(setq pgg-scheme 'gpg)			; for verify/decrypt

;(setq pgg-read-passphrase 'read-passwd)	; it is the default?
(setq pgg-read-passphrase 'read-string)	; XXX for debugging
(setq pgg-cache-passphrase t)		; it is the default
(setq pgg-passphrase-cache-expiry 14400); 4 hrs

(setq pgg-default-keyserver-address "pool.sks-keyservers.net")

;; set up a way for pgg to fetch text from a URL such that it appears on stdout
;;
(setq pgg-insert-url-function  (function pgg-insert-url-with-program))
(setq pgg-insert-url-program "ftp")
(setq pgg-insert-url-extra-arguments '("-o" "-"))

;; XXX almost no modern GUI-based reader and re-assemble split messages!
;;
(setq mime-edit-split-message nil)

;; XXX this function is a copy of the original done simply to change the
;; default value for the encoding to be quoted-printable instead of base64
;;
(defun mime-encode-region (start end encoding)
  "Encode region START to END of current buffer using ENCODING.
ENCODING must be string."
  (interactive
   (list (region-beginning)(region-end)
	 (completing-read "Encoding: "
			  (mime-encoding-alist)
			  nil t "quoted-printable")))
  (funcall (mel-find-function 'mime-encode-region encoding) start end))

;; XXX GRRR!  It seems this is impossible to do from here!
;; (error on startup: "eval-buffer: Symbol's value as variable is void: mime-view-mode-map")
;;
;; xxx these wouldn't be right anywya -- what I want are keys to show the raw
;; message headers and body as it would be transmitted
;;
;(define-key mime-view-mode-map "c" 'mime-preview-toggle-content)
;(define-key mime-view-mode-map "h" 'mime-preview-toggle-header)

; FIXME: Doesn't work(?). Should add `text-mode' settings
;
;; (e.g. word wrapping) to WL message viewing mode.
;;
;(add-hook 'mime-view-mode-hook 'my-text-mode-init)

;; delete myself from the recipient list(s?) in draft messages
;;
;; One or the other of the following will also have to be set if you use
;; multiple e-mail addresses:
;;
;; `wl-user-mail-address-list'
;; `wl-user-mail-address-regexp' (supersedes the first one)
;;
(setq wl-draft-always-delete-myself t)

;; DO use the pet name, if defined, or the RFC display-name, in summary lines
;;
;; (turned off for drafts below in `my-wl-default-draft-cite' because I do want
;; to use `wl-default-draft-cite-decorate-author'!)
;;
(setq wl-use-petname t)

;; modified version of default body citation func
;;
(defun my-wl-default-draft-cite ()
  (let ((mail-yank-ignored-headers "[^:]+:")
	(mail-yank-prefix "> ")
	(wl-use-petname nil)
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

;; note: 'split-horiz is a feature of some private patches
;;
(setq wl-draft-buffer-style 'split-horiz)
(setq wl-draft-reply-buffer-style 'split-horiz)

;; By default Wanderlust uses Reply-to-All; but that is usually not what we
;; (well, I) want.  The code below makes Reply-to-Sender the default, with
;; Reply-to-All the action when there is an argument prefix; i.e. `A' or `a'
;; will reply to sender, `C-u A' and `C-u a' reply to all.
;;
;; (Note, the uppercase 'A' is for replying with quoting of the original
;;  message, while the lowercase `a' starts the reply with an empty message)
;;
;; from a mailing list post by David Bremner
;;
;; Invert behaviour of with and without argument replies.
;;
;; ... reply-to-sender, but just standard Mail RFC headers!
(setq wl-draft-reply-without-argument-list
      '(("Reply-To" . (("Reply-To")
		       nil 
		       nil))
	("From" . (("From")
		   nil
		   nil))))
;;                                                                                                                                    
;; ... reply-to-all, but just standard Mail RFC headers!
;;
(setq wl-draft-reply-with-argument-list
      '(("Reply-To" . (("Reply-To")
		       ("To" "Cc")
		       nil))
	(wl-draft-self-reply-p . (("To")
				  ("Cc")
				  nil))
	("From" . (("From")
		   ("To" "Cc")
		   nil))))

;; by default keep all my sent mail in one place....
;;
;; Note this may be adjusted at draft buffer creation time by settings in
;; `wl-draft-config-alist'.
;;
(setq wl-fcc "%inbox/Sent@mailbox.weird.com")
;;
;; for network-free offline support
;;(setq wl-fcc "+sent")

;; this should work for me, but it won't work for everyone
;; (and it especially won't work for mobile laptop users!)
;;
(setq system-fqdn (if (string-match "\\." (system-name))
		      (system-name)
		    (concat (system-name) "." wl-local-domain)))
(setq wl-envelope-from (concat (user-login-name) "@" system-fqdn))

;; a good default, but may be adapted by wl-draft-config-alist as below
;; (if unset the full local hostname is used)
;;
(setq wl-from "\"Greg A. Woods\" <woods@weird.com>")

;; used to show "To: recip" in summary lines for messages sent by user
;;
;; also used to eliminate alternate addresses from destination fields in draft
;; buffers
;;
;; note: supersedes `wl-user-mail-address-list' so only this one can be used if
;; we want to match the same wildcard mailbox forms used in the aliases file(s)
;;
(setq wl-user-mail-address-regexp
      "woods\\(-[^@]*\\)?@\\(weird\\.\\(com\\|ca\\)\\)\\|\\(planix\\.\\(ca\\|com\\|net\\)\\)")

;; also turn on flyspell explicitly
;;
(add-hook 'wl-mail-setup-hook (lambda ()
				(flyspell-mode)))
(add-hook 'wl-mail-setup-hook (lambda ()
				(mail-abbrevs-setup)))

;; more MIME file types
;;
(mime-view-read-mailcap-files)		; from `mime-view-mailcap-files'
;;
(add-to-list 'mime-file-types
	     '("\\.pdf$" "application"
		"pdf"
		nil
		"base64" "attachment"
		(("filename" . file))))
	     
;; Unfortunately this `defadvice' is not quite sufficient on its own.
;;
;; XXX The only work-around I know for now is to mark the entire body of the
;; message as as region just before I'm ready to send and then invoke
;;`mime-encode-region' with the now-default value of "quoted-printable".
;;
;;	C-t M-<SPACE> M-> M-x mime-encode-region <RETURN> <RETURN>
;;
;; It doesn't seem like WL/SEMI/FLIM implements quoted-printable encoding
;; properly, or maybe even not at all (i.e. doing this seems to be a no-op, as
;; will be evident from this message).
;; 
;; I do see the MIME tag as "[[text/plain][quoted-printable]]", and this
;; message will have a "Content-Transfer-Encoding: quoted-printable"
;; header, but there will be no such encoding performed before it is sent.
;;
;; It may be that `mime-edit-translate-body' doesn't do any encoding work, but
;; if not, how to make it do so and yet avoid encoding files, etc. that have
;; been inserted with their encoding already done?  It probably doesn't do any
;; encoding because it assumes the encoding was already done when the file was
;; inserted, but this is not true for `mime-edit-insert-text' or the equivalent
;; since that's likely just text typed by the user.
;;
;; Maybe when we're about to send a message we should first run through all the
;; MIME parts (instead of the following) and transform any [[text/*]] tags
;; without a specified encoding to add the [quoted-printable] encoding and of
;; course then do the necessary quoted-printable encoding automatically as well
;; at that time.  Optionally we could do this only when we're using PGG to sign
;; a message, but really all text will be more robust through e-mail if it is
;; encoded somehow, and quoted-printable is the least intrusive, only showing
;; its ugly head if it is absolutely necessary.
;;
(defadvice mime-edit-insert-signature (after my-mime-edit-signature-set-qp-encoding activate)
  "Add quoted-printable encoding to the MIME tag for the message."
  (mime-edit-define-encoding "quoted-printable"))

;; The default draft folder, first set up the local draft folder name.
;;
;; Someday this may be adjusted at draft buffer creation time by settings in
;; `wl-draft-config-alist'.
;;
;; WARNING:  currently `wl-summary-reedit' does a plain `string=' comparison
;; against the value of `wl-draft-folder' to decide whether or not the folder
;; in question is a "draft" folder and thus do the right magic to supersede the
;; original message with the one being edited, and again to delete the draft
;; message once it has been successfully sent.
;;
(setq wl-draft-folder "+draft")

;; wl-draft-config-exec really MUST be done as early as possible, not as late
;; as possible!!!
;; 
;; So, move it at least to the mail-setup stage...
;;
(remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
;(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
(add-hook 'wl-mail-setup-hook
	  '(lambda ()
	     (unless wl-draft-reedit ; don't apply when reedit.
	       (wl-draft-config-exec wl-draft-config-alist))))

;; hmmm, this seems counter-productive
(remove-hook 'wl-draft-reedit-hook 'wl-draft-remove-text-plain-tag)

;; suggested by Masaru Nomiya on the WL mailing list
;;
(defun my-wl-draft-subject-check ()
  "Check whether the message has a subject before sending."
  (if (and (< (length (std11-field-body "Subject")) 1)
	   (null (y-or-n-p "No subject!  Send current draft?")))
      (error "Abort.")))

;; note, this check could cause some false positives; anyway, better safe than
;; sorry...
;;
(defun my-wl-draft-attachment-check ()
  "If attachment is mention but none included, warn the the user."
  (save-excursion
    (goto-char 0)
    (unless ;; don't we have an attachment?
	(re-search-forward "^Content-Disposition: attachment" nil t)
      (when ;; no attachment; did we mention an attachment?
	  (re-search-forward "attachment" nil t)
	(unless (y-or-n-p "Possibly missing an attachment.  Send current draft?")
	  (error "Abort."))))))

(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-subject-check)
(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-attachment-check)

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
;; add ("FCC" . "%inbox/Sent@mailbox.domain") to set FCC...
;;
;; What about using this too:
;;
;;	;; If non-nil, applied only one element of `wl-draft-config-alist'.
;;	(setq wl-draft-config-matchone t)
;;
(setq wl-draft-config-alist
      '((reply
	 "From: [\"]?Andreas Wrede[\"]?"
	 ("From" . "\"Greg A. Woods\" <woods@weird.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@weird.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: .*@.*planix\\."
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
;	(reply
;	 "From: .*@.*teloip\\."
;	 ("From" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("FCC" . "%inbox.Sent:gwoods@mail.teloip.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "TELoIP Inc."))
	(reply
	 "From: .*@.*clasix\\.net"
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: .*@.*seawellnetworks\\.com"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: [\"]?Scott Lindsay[\"]?"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: .*Ted Gray.*"
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: .*@.*\\(aci\\|opc\\)\\.on\\.ca"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("X-Priority" . "2")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "From: .*@.*\\(lawyermediator\\|gelmanlaw\\)\\.ca"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("X-Priority" . "1"))
	((string-match "^%inbox/planix.*@mailbox\\.weird\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*@mail\\.planix\\.com" wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
;	((string-match "^%.*@mail\\.teloip\\.com" wl-draft-parent-folder)
;	 ("From" . "\"Greg A. Woods\" <woods@teloip.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <woods@teloip.com>")
;	 ("FCC" . "%inbox.Sent:gwoods@mail.teloip.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "TELoIP, Inc."))
	((string-match "^%.*:woods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@aci.on.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*:gwoods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  emacs-mime-en
	((or (string-match "^%inbox/Lists-IN/emacs-mime-en-l"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/emacs-mime-en"
			   wl-draft-parent-folder))
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "Delivered-To: emacs-mime-en@m17n.org"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "To: woods-emacs-mime-en-l@weird.com"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  info-cyrus
	((or (string-match "^%inbox/Lists-IN/cyrus-lists"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/info-cyrus"
			   wl-draft-parent-folder))
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "Sender: info-cyrus-bounces"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "To: woods-cyrus@weird.com"	; XXX is this one necessary?
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  git
	((string-match "^%inbox/Lists-IN/git-list"
		       wl-draft-parent-folder)
         ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Reply-To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "List-Id: .*<git.vger.kernel.org>"
         ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Reply-To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  nsd-users
	((string-match "^%inbox/Lists-IN/nsd-users"
		       wl-draft-parent-folder)
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Reply-To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "List-Id: .*<nsd-users.NLnetLabs.nl>"
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Reply-To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Organization" . "Planix, Inc."))
	;; mailing lists:  netbsd
	;; XXX the (let ((case-fold-search t))) in wl-draft.el:wl-draft-config-exec doesn't seem to work....
	;; XXX how to set reply-to correctly in a dynamic way without having to
	;; write some complex editing function?
	;; mailing list:  nsd-users
	(reply
	 "Delivered-To: .*@[Nn][Ee][Tt][Bb][Ss][Dd]\\.[Oo][Rr][Gg]"
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%inbox/Lists-IN/netbsd-lists/"
		       wl-draft-parent-folder)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "List-Id: .*\\.[Nn][Ee][Tt][Bb][Ss][Dd]\\.[Oo][Rr][Gg]>"
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  unbound-users
	((string-match "^%inbox/Lists-IN/unbound-users"
		       wl-draft-parent-folder)
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Reply-To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "List-Id: .*<unbound-users.unbound.net>"
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Reply-To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  wl-en
	((or (string-match "^%inbox/Lists-IN/wl-en-l"
			   wl-draft-parent-folder)
	     (string-match "^%inbox/list-archive/wl-en@"
			   wl-draft-parent-folder))
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "Delivered-To: wl-en@lists.airs.net"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "To: woods-wl-en-l@planix.com"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@lists.airs.net>")
	 ("Organization" . "Planix, Inc."))
	; defaults for everything
	((or t)
	 (pgp-sign . t)
	 mime-edit-insert-signature)))

(setq wl-insert-message-id nil)		; let our MTA do it....

(setq signature-insert-at-eof t)
(setq signature-file-alist
      '((("From" . "@planix\\.") . "~/.signature-planix.com")
;	(("From" . "@teloip\\.") . "~/.signature-teloip.com")
	(("From" . "@aci\\.") . "~/.signature-aci-postmaster")
	(("From" . ".") . "~/.signature")))

;; mail-sent-via is a big useless pile of crap.
;; 
;; luckily it seems we can modify `wl-draft-mode-map' at ~/.wl load time, but
;; otherwise it could be done in a `wl-draft-mode-hook' function
;;
(define-key wl-draft-mode-map "\C-c\C-v" nil)

;; do the right thing....
;;
(define-key wl-draft-mode-map "\C-xk" 'wl-draft-kill)

;; For some reason, `wl-draft-save' is not bound in `wl-draft-mode' even though
;; it should rightfully override the default save operation.
;;
(define-key wl-draft-mode-map (kbd "C-x C-s") 'wl-draft-save)

;; fix gratuitous overriding of `M-t' in draft mode.
;;
(define-key wl-draft-mode-map (kbd "M-t") nil)

;; just for `wl-summary-goto-folder', which oddly enough is only bound to a key
;; (`G') in the Summary buffer of all places!
;;
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
;; different port, different heirarchy separator, or whatever)
;;
(setq wl-dispose-folder-alist
      '(("^%inbox.*Trash@" . remove)	; this one must come first
	("^%INBOX$" . "%inbox/Trash")
	("^%inbox[^@]*$" . "%inbox/Trash")
	("^%INBOX@mailbox.weird.com" . "%inbox/Trash@mailbox.weird.com")
	("^%inbox.*@mailbox.weird.com" . "%inbox/Trash@mailbox.weird.com")
;	("^%INBOX@mail.teloip.com" . "%inbox.Trash@mail.teloip.com:993!")
;	("^%inbox.*@mail.teloip.com" . "%inbox.Trash@mail.teloip.com:993!")
	("^%INBOX:gwoods@mailbox.aci.on.ca" . "%inbox/Trash:gwoods@mailbox.aci.on.ca:993!")
	("^%inbox.*:gwoods@mailbox.aci.on.ca" . "%inbox/Trash:gwoods@mailbox.aci.on.ca:993!")
	("^%INBOX@mailbox.aci.on.ca" . "%inbox/Trash@mailbox.aci.on.ca:993!")
	("^%inbox.*@mailbox.aci.on.ca" . "%inbox/Trash@mailbox.aci.on.ca:993!")
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

;; in the style of `wl-dispose-folder-alist'
;;
;; (see the comment about manual enumeration for `wl-dispose-folder-alist' too)
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
	("^%INBOX:gwoods@mailbox.aci.on.ca" . "%inbox/Junk:gwoods@mailbox.aci.on.ca:993!")
	("^%inbox.*:gwoods@mailbox.aci.on.ca" . "%inbox/Junk:gwoods@mailbox.aci.on.ca:993!")
	("^%INBOX@mailbox.aci.on.ca" . "%inbox/Junk@mailbox.aci.on.ca:993!")
	("^%inbox.*@mailbox.aci.on.ca" . "%inbox/Junk@mailbox.aci.on.ca:993!")))

;; essentially a copy of `wl-summary-get-dispose-folder' (they could probably
;; be merged together into something more generic)
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
		   (error "Junk Folder has not been created"))))
	     wl-junk-folder)))))

;; ensure we have a `junk' flag type defined for elmo that can translate into
;; an appropriate IMAP flag.  (or is it the other way around?)
;;
;; Currently my mail server reports the following:
;;
;; * FLAGS (\Answered \Flagged \Draft \Deleted \Seen NonJunk Junk $NotJunk
;; $Junk MessageJunkMailLevel JunkRecorded MessageHasBeenViewed $Label5 $Label1
;; $Label2 $Label3 $Label4 $Forwarded Redirected NotJunk Forwarded MyStuff)
;;
;; After playing with `wl-summary-set-flags' and "registering" some of the ones
;; that were given as completions (and thus would presumably have already been
;; registered!), specifically 'business, 'todo, and 'private, we get:
;;
;; * FLAGS (\Answered \Flagged \Draft \Deleted \Seen NonJunk Junk $NotJunk
;; $Junk MessageJunkMailLevel JunkRecorded MessageHasBeenViewed $Label5 $Label1
;; $Label2 $Label3 $Label4 $Forwarded Redirected NotJunk Forwarded MyStuff
;; Business Todo Private)
;;
(defconst elmo-imap4-flag-specs '((important "\\Flagged")
				  (read "\\Seen")
				  (unread "\\Seen" 'remove)
				  (answered "\\Answered")
				  ;; draft-melnikov-imap-keywords-03.txt
				  (forwarded "$Forwarded")
				  (work "$Work")
				  (personal "$Personal")
				  (shouldreply "$ShouldReply")
				  ;; more!
				  (deleted "\\Deleted")	; XXX should this now be 'killed?
				  (forwarded "Forwarded")
				  (ignore "JunkRecorded")
				  (ignore "NotJunk")
				  (ignore "$NotJunk")
				  (junk "Junk")
				  (junk "$Junk")))

;; the Junk action.
;;
;; For now we just move them into the appropriate Junk folder, but eventually
;; we might at least add IMAP (persistent) flags so that they appear as junk to other types
;; of clients, and also we might do some post-processing and perhaps possibly
;; forward some of them off to SpamCop and/or the source addr's contacts, etc.
;;
(defun wl-summary-exec-action-junk (mark-list)
  (message (princ-list mark-list))
;  (elmo-folder-set-flag wl-summary-buffer-elmo-folder
;			(wl-summary-make-destination-numbers-list mark-list)
;			'junk nil)
  (wl-summary-move-mark-list-messages mark-list
				      (wl-summary-get-junk-folder
				       (wl-summary-buffer-folder-name))
				      "Junking messages..."))

;; a font face for Junk lines in the Summary
(wl-defface wl-highlight-summary-junk-face
  '((((type tty))
     (:foreground "grey"))
    (((class grayscale))
     (:foreground "grey" :slant italic))
    (((class mono))
     (:foreground "black" :slant italic))
    (((class color))
     (:foreground "LightSlateGray" :slant italic)))
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
     "Move messages marked as Junk to the appropriate Junk folder."))
  "A variable to define a Mark & Action for Junk handling.
Append its value to `wl-summary-mark-action-list'.

See `wl-summary-mark-action-list' for the details of each element.")

(setq wl-summary-mark-action-list (append
				   wl-summary-mark-action-list
				   wl-junk-mark-action-list))

(add-to-list 'wl-summary-skip-mark-list "J")

(defcustom wl-summary-junk-cached-mark "j"
  "Character for persistend mark for junked and cached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

(defcustom wl-summary-junk-uncached-mark "J"
  "Character for persistent mark for junked and uncached message."
  :type '(string :tag "Mark")
  :group 'wl-summary-marks)

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
;; Note: if you change the hierarchy and want to rebuild the tree do:
;;
;;	rm -rf ~/.elmo/folder
;;
(setq wl-folder-hierarchy-access-folders
      '(
	"^%\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)" ; for IMAP (recursive)
	"^%[^/.]+\\(:\\|@\\|$\\)"	; for IMAP (XXX is this necessary?)
	"^-[^.]*\\(:\\|@\\|$\\)"	; for NNTP
	"^@$"				; for Shimbun (?)
	"^'$"				; for internal (?)
	))

;; set the folder-window's width to some decent percentage of the window width.
;;
(setq wl-folder-window-width (max wl-folder-window-width
				  (round (* (window-width) 0.25))))
;;
;; or maybe this could be better, but it doesn't work the way I expected it to
;; -- i.e. it creates a new frame for the Folder window, but then procedes to
;; just use this frame for all other windows as well.
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
