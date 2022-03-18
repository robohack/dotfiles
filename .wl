;;;;
;;;;	.wl.el - Wanderlust custom configuration
;;;;
;;;;#ident	"@(#)HOME:.wl	37.5	22/03/18 12:22:57 (woods)"
;;;;

;; XXX look for ideas in <URL:http://triaez.kaisei.org/~kaoru/emacsen/startup/init-mua.el>
;;
;; See also https://www.emacswiki.org/emacs/WlFaq

;; N.B.:  the following packages are effectively required:
;;
;;	graphics/compface
;;	security/gnupg2		; with security/pinentry
;;	textproc/aspell		; or...
;;	textproc/ispell
;;
;; OpenSSL is also required (for "openssl s_client:)
;;
;; Some emacs packages listed in ~/.emacs.el will also be needed.

;; FixMe:
;;
;; `undo' should mark a message as unread if the last action was to view it.

;; do not display diary at midnight (it messes with window configurations!)
(if (boundp 'appt-display-diary)
    (setq appt-display-diary nil))

;; Make the mouse/trackpad scroll the window more "smoothly"
;;
;; DO NOT EVER move the cursor with scroll input (unless doing so to keep it
;; within the current window.
;;
(eval-after-load "wl-summary"
  '(progn
     (define-key wl-summary-mode-map [mouse-4] 'mwheel-scroll)
     (define-key wl-summary-mode-map [mouse-5] 'mwheel-scroll)
     (define-key wl-summary-mode-map [S-mouse-4] 'mwheel-scroll)
     (define-key wl-summary-mode-map [S-mouse-5] 'mwheel-scroll)))

;; same for mime-view, but maybe this doesn't work so well?
;; xxx maybe setting `mime-view-mode-default-map' doesn't work???
;;
(eval-after-load "mime-view"
  '(progn
     (define-key mime-view-mode-default-map [mouse-4] 'mwheel-scroll)
     (define-key mime-view-mode-default-map [mouse-5] 'mwheel-scroll)
     (define-key mime-view-mode-default-map [S-mouse-4] 'mwheel-scroll)
     (define-key mime-view-mode-default-map [S-mouse-5] 'mwheel-scroll)))

;; make scrolling back work in the message view
;; xxx maybe setting `mime-view-mode-default-map' doesn't work???
;;
(define-key mime-view-mode-default-map "b" 'mime-preview-scroll-down-entity)

;; turn off scoring for speed -- I never use it anyway
;;
(setq wl-use-scoring nil)

;; don't leave my passwords sitting in memory too long!
;;
;; (use `elmo-passwd-alist-clear' to manually clear cache and start over)
;;
(setq elmo-passwd-life-time 14400)	; 4 hrs

;; let's try this for use with wl-refile-guess-by-from and use of "%INBOX/from"
;; as wl-refile-default-from-folder in particular since it doesn't seem to be
;; able to do anything any smarter than to concatenate the mailbox string onto
;; this prefix, thus there's no way to specify a server name in the default.
;;
;; a better solution would be to use a call to `format' to expand
;; wl-refile-default-from-folder, with "%s" in the position where the folder
;; name should be placed
;;
(setq elmo-imap4-default-server "mailbox.weird.com")

;(setq elmo-imap4-debug t)	;; for tracing the IMAP session
(setq elmo-imap4-force-login t)	;; hmmm...  is this necessary with my Cyrus IMAPd?
;(setq elmo-imap4-debug-inhibit-login-logging-default nil) ;; for trying to trace login, but using my own still-unpublished hacks


;; Check these folders for new mail
;;
;(setq wl-biff-check-folder-list
;      '("%INBOX:\"user@gmail.com\"/clear@imap.gmail.com:993"
;	"%INBOX:\"user@other.domain\"/clear@imapserver.other.domain:993"))
;;
;; Use strict diff so wl-biff works with Gmail and others
;;
;(setq wl-strict-diff-folders wl-biff-check-folder-list)

;; Check for mail every 60 seconds
;;
(setq wl-biff-check-interval 60)

;; Check only when idle
;;
(setq wl-biff-use-idle-timer t)


;; `mail-local-domain-name' comes from my ~/.emacs.el
;;
(setq wl-local-domain (or mail-local-domain-name "example.org"))

;; Use SSL connection
;;
;; N.B.:  You MUST install WanderLust with `wl-install-utils' set in WL-CFG
;;
;(setq elmo-imap4-default-stream-type 'starttls)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-default-port 993)
;; ... else don't use SSL...
;; XXX WARNING XXX:  only safe if IMAP host & network to it is 100% secure!
;(setq elmo-imap4-default-stream-type nil)
;(setq elmo-imap4-default-port 143)

;; This is required for some reason in order for imap.gmail.com connections to
;; work (note that it would work with `1' ("Verification required"), but this
;; seems safer.
;;
;; `3' means "Reject connection if verification fails"
;;
(setq ssl-certificate-verification-policy 3)

;    ;; https://github.com/wanderlust/wanderlust/issues/166
;   (setq gnutls-verify-error nil)	; xxx ???
;   (setq gnutls-min-prime-bits 1024)
;   (setq gnutls-algorithm-priority "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3")

(setq ssl-program-arguments
      '("s_client"
	"-tls1"			; new mailbox.weird.com requires TLSv1 (or SSLv3)
	"-quiet"
	"-host" host
	"-port" service
	"-verify" (int-to-string ssl-certificate-verification-policy)
	"-CApath" ssl-certificate-directory))

;; password always sent in the clear for my servers (over TLS, of course)
;;
(setq elmo-imap4-default-authenticate-type 'clear)

;; this is needed to make sure filenames created to save attachments are sane
;;
(setq filename-filters '(filename-special-filter))

(defun string-matched (search strings)
  (while (and strings (not (string-match search (car strings))))
    (setq strings (cdr strings)))
  (car strings))

;; Directory where icons are placed (XXX should be set by configuration!)
;;
(eval-and-compile
  (defvar wl-icon-directory-ORIGINAL wl-icon-directory
    "original value at startup")
  )
(setq wl-icon-directory
      (cond ((let ((icons
		    (expand-file-name "icons/"
				      (string-matched "/wanderlust" load-path))))
	       (if (file-directory-p icons)
		   icons)))
	    ;; n.b.:  the first will almost certainly always win, but keep these
	    ;; for posterity:
	    ((let ((icons
		    (expand-file-name "../../wl/icons/"
				      (cond ((boundp 'local-site-lisp-dir)
					     local-site-lisp-dir)
					    ((boundp 'pkg-site-lisp-dir)
					     pkg-site-lisp-dir)))))
	       (if (file-directory-p icons)
		   icons)))
	    ((let ((icons
		    (expand-file-name "icons/"
				      (cond ((and (boundp 'package-alist)
						  (fboundp 'package-desc-dir))
					     (package-desc-dir
					      (cadr (assq 'wanderlust
							  package-alist))))
					    (t
					     nil)))))
	       (if (file-directory-p icons)
		   icons)))
	    ((let ((icons
		    (expand-file-name "wl/icons/"
				      data-directory)))
	       (if (file-directory-p icons)
		   icons))
	     (t
	      nil))))

;; prefetch everything that's uncached, not just unread-uncached (U) and
;; new-uncached (N)
;;
;; aka: (setq wl-summary-incorporate-marks '("N" "U" "!" "A" "F" "$"))
;;
;; Visiting any folder will now pre-fetch all messages.
;;
;; Also one can explicity call `wl-folder-prefetch-current-entity' (bound to I
;; in the Folder buffer).
;;
;(setq wl-summary-incorporate-marks
;      (list wl-summary-uncached-mark
;	    wl-summary-new-uncached-mark
;	    wl-summary-unread-uncached-mark
;	    wl-summary-answered-uncached-mark))
;;
;; XXX _HOWEVER_ prefetching is extremely slow!  but this does not stop it!
;;
;(setq wl-summary-incorporate-marks nil)

;(setq wl-summary-force-prefetch-folder-list nil) ; is the default

(setq wl-stay-folder-window t)

;; support for marking messages addressed "to-me" in the Summary buffer.
;;
;; (Uses `wl-address-user-mail-address-p', thus `wl-user-mail-address-regexp' if
;; non-nil, else `wl-user-mail-address-list'.)
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
				     "%P %E %[%20(%c %f%) %] %Y/%M/%D(%W)%h:%m %-8S%-2@ %t%~\"%s\" \t"))
(setq wl-summary-default-view 'sequence)

;; never search for thread parent messages by subject!
;;
(setq wl-summary-search-parent-by-subject-regexp nil)

;; xxx unfortunately this is results in an either-or list, with no way to
;; combine various flags and statuses to, for example, show an "Important" but
;; answered message with the same background as the "important" flag, and also
;; with the grey strike-through of an answered message (IFF answered is above
;; important)
(setq wl-summary-persistent-mark-priority-list '(killed
						 deleted
						 draft
						 answered
						 forwarded
						 redirected
						 flag ; user-defined flag!?!?!? (XXX standin for all of them?)
						 shouldreply
						 junk
						 important
						 special
						 private
						 todo
						 personal
						 work
						 new
						 unread
						 notjunk
						 nonjunk))

;; default is blank (white on white) on monochrome displays!
;;
;; XXX for this to work correctly (i.e. be display-independent), the COLOR
;; field must be a full display face created by `defface'.
;;
;; XXX in the mean time maybe we should also check the result of:
;;
;;	(frame-parameter nil 'display-type) ; (or does `display-color-p' just do that?)
;;
(if (display-color-p)
    (setq wl-summary-flag-alist '((ignore "dim gray" " ")
				  (important "black" "I") ; see later set-face-attribute
				  (private "blue" "P")
				  (todo "dark red" "T")
				  (personal "dark blue" "p")
				  (business "forest green" "W")
				  (work "dark green" "w")
				  (forwarded "medium blue" "F")
				  (redirected "dark cyan" "R")
				  (killed "grey" "K")
				  (junk "SlateGray" "J")
				  (unread "black" "O") ; xxx hmmm, probably not what I think
				  (flag "black" "?") ; xxx doesn't seem to work completely (still get `wl-summary-flag-mark')
				  (junkrecorded "black" " ") ; xxx actually should be whatever is in force without the flag!
				  (notjunk "black" " ")
				  ))
  ;; else not colour...
  (setq wl-summary-flag-alist '((ignore "black" " ")
				(important "black" "I")
				(private "black" "P")
				(todo "black" "T")
				(personal "black" "p")
				(business "black" "W")
				(work "black" "w")
				(forwarded "black" "F")
				(redirected "black" "R")
				(killed "black" "K")
				(unread "black" "O") ; xxx hmmm, probably not what I think
				(junk "black" "J")
				(flag "black" "?")
				;;(junkrecorded "black" " ")
				;;(notjunk "black" " ")
				)))

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

(defun my-wl-init-stuff ()
  "Setup stuff run at the end of `wl-init'."
  (require 'wl-highlight)
  ;; some faces are not defined until after `wl-init'...
  (if (display-color-p)
      ;; xxx hmmm... some themes do wl stuff, others do not...
      (if (eq (frame-parameter nil 'background-mode) 'light)
	  (progn
	    ;; message view
	    (set-face-attribute 'wl-highlight-message-headers
				nil
				:foreground "black")
	    ;; Summary
	    (set-face-attribute 'wl-highlight-summary-new-face
				nil
				:foreground "dark red")
	    (set-face-attribute 'wl-highlight-summary-unread-face
				nil
				:foreground "black")
	    (set-face-attribute 'wl-highlight-summary-answered-face
				nil
				:foreground "sea green"
				:strike-through "grey")
	    (set-face-attribute 'wl-highlight-summary-resend-face
				nil
				:foreground "blue")
	    ;; here "flagged" means _ANY_ flag!
	    (set-face-attribute 'wl-highlight-summary-flagged-face
				nil
				:foreground "magenta"
				:background "white")
	    (set-face-attribute 'wl-highlight-summary-deleted-face
				nil
				:foreground "red"
				:strike-through "orange red")
	    (set-face-attribute 'wl-highlight-summary-disposed-face
				nil
				:foreground "saddle brown"
				:strike-through "orange red")
	    ;; Flagged (see wl-summary-flag-alist for primary settings -- the
	    ;; corresponding entry in wl-summary-flag-alist must exist for these
	    ;; to work)
	    (set-face-attribute 'wl-highlight-summary-important-flag-face
				nil
				:background "lemon chiffon")
	    (set-face-attribute 'wl-highlight-summary-personal-flag-face
				nil
				:background "cornsilk")
	    (set-face-attribute 'wl-highlight-summary-work-flag-face
				nil
				:background "light cyan")
	    (set-face-attribute 'wl-highlight-summary-redirected-flag-face
				nil
				:foreground "brown") ; xxx doesn't work?
	    ;; xxx this is not the same as `wl-highlight-summary-junk-face'
	    (set-face-attribute 'wl-highlight-summary-junk-flag-face
				nil
				:foreground "SlateGray"
				:strike-through "LightGray")
;;; XXX grrr.....
;;;	    (set-face-attribute 'wl-highlight-summary-notjunk-flag-face
;;;				nil
;;;				:foreground "black")
	    ;; Folder
	    (set-face-attribute 'wl-highlight-folder-few-face
				nil
				:foreground "firebrick")
	    (set-face-attribute 'wl-highlight-folder-many-face
				nil
				:foreground "red")
	    (set-face-attribute 'wl-highlight-folder-opened-face
				nil
				:foreground "dark green")
	    (set-face-attribute 'wl-highlight-folder-unknown-face
				nil
				:foreground "blue")
	    (set-face-attribute 'wl-highlight-folder-unread-face
				nil
				:foreground "black")
	    )
        ;; else background-mode dark(?)
	(progn
	  ;; message view
	  (set-face-attribute 'wl-highlight-message-headers
			      nil
			      :foreground "white")
	  ;; Summary
	  (set-face-attribute 'wl-highlight-summary-new-face
			      nil
			      :background "black"
			      :foreground "white")
	  (set-face-attribute 'wl-highlight-summary-flagged-face
			      nil
			      :background "black"
			      :foreground "yellow")
	  (set-face-attribute 'wl-highlight-summary-important-flag-face
			      nil
			      :background "black"
			      :foreground "white")
;;; XXX How to make this always the same as "normal" where normal might be dynamic???
;;;	  (set-face-attribute 'wl-highlight-summary-junkrecorded-flag-face
;;;			      nil
;;;			      :background "black"
;;;			      :foreground "white")
	  (set-face-attribute 'wl-highlight-summary-ignore-flag-face
			      nil
			      :background "black"
			      :foreground "blue")
	  (set-face-attribute 'wl-highlight-summary-deleted-face
			      nil
			      :foreground "orange red"
			      :strike-through "red")
	  (set-face-attribute 'wl-highlight-summary-disposed-face
			      nil
			      :foreground "dark orange"
			      :strike-through "orange red")
	    )
	)
    ;; else not color display:
    (set-face-attribute 'wl-highlight-summary-deleted-face nil
			:strike-through t)))

(add-hook `wl-init-hook `my-wl-init-stuff)

;; show recipient in summary %f column of all folders when sender is me
;;
(setq wl-summary-showto-folder-regexp ".*")
(setq wl-summary-from-function 'wl-summary-default-from)

(setq wl-summary-move-direction-toggle nil) ; and don't waffle!!!
(setq wl-summary-move-direction-downward t) ; just always go DOWN

(setq wl-summary-exit-next-move nil)	; don't move the Folder pointer on quit

;; don't automatically try to sync all "marks", as this causes enormous delays
;; when loading large infrequently visited folders.  Use "s mark <return>" to do
;; it intentionally.
;;
(setq wl-summary-auto-sync-marks nil)

;; to decode encoded words within quoted strings in headers....
;;
;; NOTE:  normally this is contrary to the standards, I think, so it should not
;; be necessary, but of course some stupid mailers always quote every display
;; name regardless of whether it needs quoting or not, and perhaps sometimes
;; even when it must not be quoted, such as when it is encoded.
;;
;; Also, you have to refresh the summary after you change this if the header is
;; shown in a summary column.
;;
;; XXX This still doesn't allow for improperly encoded headers, such as when
;; spaces are not encoded:
;;
;;	Subject: =?UTF-8?Q?DNEvents.com Inc. invites you to 8th Toronto Domainer Dinner (Apr 23, 2009)?=
;;
;; XXX these are not decoded either:
;;
;;	Subject: =?utf-8?Q?We=e2=80=99re_updating_our_Privacy_Policy_and_tools?=
;;	From: "=?utf-8?Q?feedback=40slack=2ecom?=" <feedback@slack.com>
;;	From: =?UTF-8?Q?Isma=c3=abl_Tanguy?= <ismael.tanguy@univ-brest.fr>
;;
;; N.B.:  `rfc2047-decode-string' handles all of the above just fine.
;;
(setq mime-header-accept-quoted-encoded-words t)

(defun my-wl-summary-turn-off-disp-msg ()
  "Unconditionally turn off message display so that I don't fat-finger myself
into too much confusion (use this for bindings to `delete-other-windows')."
  (interactive)
  (wl-summary-toggle-disp-msg 'off)
  ;; the default effectively turned off the Folder window too because it called
  ;; `delete-other-windows', but with a wide display that's NOT what I want to
  ;; do any more.
  ;;
  ;; note:  long ago there was code in `wl-summary-toggle-disp-msg' that hid the
  ;; folder window when displaying the summary, but it was commented out.
  )

(define-key wl-summary-mode-map "\C-x1" 'my-wl-summary-turn-off-disp-msg)

(require 'advice)
(defadvice wl-summary-sync-force-update (before my-wl-summary-sync-force-update activate)
  "Turn off message display before updating the summary."
  (wl-summary-toggle-disp-msg 'off))

;; maybe this could probably be done with `defadvice'?
;; xxx this is not exactly a good mimic to `wl-summary-mark-as-unread' as it
;; does not really follow the API for wl-summary commands
;; xxx should also wrap `wl-summary-mark-as-unread-region'
(defun my-wl-summary-mark-as-unread (&optional arg)
  "Mark the current message as unread.
If ARG is non-nil, forget everything about the message."
  (interactive "P")
  (cond
   ((null (wl-summary-message-number))
    (message "No message."))
   (arg
    (wl-summary-toggle-disp-msg 'off)
    (wl-message-buffer-cache-clean-up)
    (wl-summary-delete-cache)))
  (wl-summary-mark-as-unread))

(define-key wl-summary-mode-map "!" 'my-wl-summary-mark-as-unread)

(define-key wl-summary-mode-map "c" 'wl-jump-to-draft-buffer) ; 'c'ontinue
(define-key wl-summary-mode-map "b" 'wl-summary-prev-page)
(define-key wl-summary-mode-map "g" 'wl-summary-sync-force-update)
(define-key wl-summary-mode-map "G" 'wl-summary-goto-folder)
;; 's' is currently bound to `wl-summary-sync', which is infinitely more useful!!!
;(define-key wl-summary-mode-map "s" 'wl-summary-save)

(define-key wl-summary-mode-map "\M-n" 'wl-summary-down)
(define-key wl-summary-mode-map "\M-p" 'wl-summary-up)

(define-key wl-folder-mode-map "\M-n" 'wl-folder-next-unread)
(define-key wl-folder-mode-map "\M-p" 'wl-folder-prev-unread)

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
(define-key wl-summary-mode-map "\C-c\C-c" 'my-wl-summary-exec-and-rescan) ; complete selection

(setq wl-thread-insert-opened t)	; XXX do we want to see the opened threads?

;; xxx this is a copy just to add `interactive'
(defun wl-summary-goto-top-of-current-thread ()
  (interactive)
  (wl-summary-jump-to-msg
   (wl-thread-entity-get-number
    (wl-thread-entity-get-top-entity (wl-thread-get-entity
				      (wl-summary-message-number))))))
(define-key wl-summary-mode-map "tt" 'wl-thread-goto-top-of-current-thread)
(define-key wl-summary-mode-map "\C-\M-a" 'wl-summary-goto-top-of-current-thread)
(define-key wl-summary-mode-map "tn" 'wl-thread-goto-bottom-of-sub-thread)
(define-key wl-summary-mode-map (kbd "C-M-)") 'wl-thread-goto-bottom-of-sub-thread)

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

(setq wl-message-buffer-prefetch-idle-time 10)
;(setq wl-message-buffer-prefetch-depth 1) ; is the default
;(setq wl-prefetch-confirm t)	; is the default
(setq wl-message-buffer-prefetch-threshold 1000000)

;(setq elmo-message-fetch-confirm t)	; is the default
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

;; xxx necessary for now to support "%@" in `wl-summary-line-format'
;; (rebuild message db for all folders after adding this:  s all <RETURN>)
;;
(add-to-list 'elmo-msgdb-extra-fields "Content-Type")

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

;; show all the headers except those we know we don't care about...  (Note that
;; any `*-view-visible-field-list' value overwhelm's the `*-ignored-field-list'
;; value)
;;
(setq wl-message-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq mime-view-visible-field-list nil) ; was '("^Dnas.*:" "^Message-Id:")
(setq wl-message-ignored-field-list
      '("[^:]*Received:"
	"[^:]*Path:"
	"[^:]*Sender:"			; include X-Sender, X-X-Sender, etc.
	"[^:]*Host:"
	"^ARC-Authentication-Results:"
	"^ARC-Message-Signature:"
	"^ARC-Seal:"
	"^Authentication-Results:"
	"^Autocrypt:"			; interesting, but contains huge key data
	"^Content[^:]*:"		; irrelevant!  :-)
	"^DKIM-Signature:"		; useless junk
	"^DomainKey[^:]*:"		; bogus junk
	"^Errors-To:"
	"^In-Reply-To:"			; just another message-id
	"^IronPort-[^:]*:"		; some silly AV crapware
	"^Lines:"
	"^List-[^:]*:"			; rfc????
	"^Message-I[dD]:"		; RFC 2036 too!
	"^MIME-Version:"		; irrelevant!  :-)
	"^Received-SPF:"		; supid and meaningless!
	"^References:"
	"^Replied:"
	"^Status:"
	"^Thread-Index:"
	"^Topicbox-Delivery-ID:"
	"^Topicbox-Message-UUID:"
	"^X-Accept-Language:"
	"^x-authority-analysis:"
	"^X-Barracuda[^:]*:"		; some stupid virus scanner
	"^X-BeenThere:"			; mailman?
	"^X-Brightmail-Tracker:"
	"^X-Cam[^:]*:"			; some stupid virus scanner
	"^X-CanItPRO[^:]*:"
	"^X-CSC:"
	"^X-CHA:"
	"^X-CMAE-Envelope:"
	"^X-CTCH-[^:]*:"
	"^X-ImunifyEmail-Filter-Info:"
	"^X-Exchange[^:]*:"		; M$-Exchange
	"^X-Filter-ID:"
	"^X-Forefront[^:]*:"
	"^X-GMX[^:]*:"
	"^X-Gm-Message-State:"
	"^X-Google-DKIM-Signature:"
	"^X-Google-Smtp-Source:"
	"^X-Greylist[^:]*:"
	"^X-Hashcash[^:]*:"		; ???
	"^X-IPAS-Result:"
	"^X-IronPort[^:]*:"		; some silly AV crapware
	"^X-Junkmail[^:]*:"		; mirapoint???
	"^X-MAil-Count:"		; fml?
	"^X-ME-[^:]*:"
	"^X-MIME-Autoconverted:"
	"^X-Microsoft[^:]*:"		; M$-Exchange
	"^X-Mirapoint[^:]*:"		; mirapoint???
	"^X-ML[^:]*:"			; fml
	"^X-MS-[^:]*:"
	"^X-MSAMetaData:"
	"^X-MSAPipeline:"
	"^X-Mailman[^:]*:"		; mailman
	"^X-OriginalArrivalTime:"
	"^X-PMAS-[^:]*:"
	"^X-PMX[^:]*:"
	"^X-Provags-[^:]*:"
	"^X-OQ-[^:]*:"
	"^X-Received-Authentication-Results:"
	"^X-RPI[^:]*:"
	"^X-SG-EID:"
	"^X-SKK:"
	"^X-SMTP-Spam-[^:]*:"
	"^X-SONIC-DKIM-SIGN:"
	"^X-Scanned[^:]*:"
	"^X-Sieve:"			; cyrus
	"^X-Spam[^:]*:"
	"^X-UI-Out-Filterresults:"
	"^X-VADE-[^:]*:"
	"^X-VM-[^:]*:"
	"^X-Virus[^:]*:"
	"^X-YMail-OSG:"			; some Mozilla mailer?
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
(setq wl-refile-default-from-folder "%INBOX/from")
;;
(setq wl-refile-rule-alist
      '(("Subject"
	 ("^\\[Acct Event\\] " . "%INBOX/planix/aci/db-trouble-tickets@mailbox.weird.com"))
	("Subject"
	 ("Aurora Cable Abuse Department" . "%INBOX/planix/aci/abuse@mailbox.weird.com"))
	("Subject"
	 ("\\[SpamCop " . ("To"
			     (".*@.*aci\\.on\\.ca" . "%INBOX/planix/aci/abuse@mailbox.weird.com"))))
	(("To" "Cc" "From" "Sender")
	 ("\\(jennifer@.*\\(wrede\\|planix\\).*\\|jen\\(nifer\\)?\\.wrede@.*\\)"
	  . "%INBOX/from/jennifer@mailbox.weird.com"))
	(("To" "Cc")
	 ("\\(abuse\\|[hp]ostmaster\\|info\\|support\\)@.*aci\\.on\\.ca"
	  . "%INBOX/planix/aci/\\1@mailbox.weird.com"))
	("From"
	 ("\\(abuse\\|[hp]ostmaster\\|info\\|support\\)@.*aci\\.on\\.ca"
	  . "%INBOX/planix/aci/\\1@mailbox.weird.com"))))

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

;; pull in all the MIME stuff (why does it seem we must do this here?)
(require 'elmo-mime)
(require 'mel)
(require 'mime-edit)
(require 'mime-view)

;; Under Emacs 24.4 and later, you can force using `shr' (Emacs’ built-in HTML
;; formatter) with the following:
;;
(if (or (> emacs-major-version 24)
	(and (= emacs-major-version 24)
	     (>= emacs-minor-version 4)))
    (progn
      (require 'shr)
      (setq mime-view-text/html-previewer 'shr)
      ;; add very useful bindings not there by default
      (define-key shr-map "c" 'shr-copy-url)
      ;; n.b.:  shr-next-link should also be on TAB, but that is apparently
      ;; overridden by mime-preview-move-to-next...
      (define-key shr-map "n" 'shr-next-link)
      (define-key shr-map "p" 'shr-previous-link))
  ;; otherwise try for w3m, iff we have mime-w3...
  ;;
  (if (elisp-file-in-loadpath-p "mime-w3")
      (require 'mime-w3)		; not really necessary here?
    (message "Please install `w3m' or `shr' for HTML formatting.")))

;; Make MIME understand HTML while preferring the text part, if one is
;; provided.
;;
;; (note:  `mime-view' "requires" `w3' or `shr'...)
;;
;; one can use `mime-preview-toggle-content', normally C-c C-t C-c to show the
;; content of a mime part.  See also `mime-preview-show-header'.
;;
;; Sometimes one may even need to do this to show the text/plain part if
;; desired, and presumably to hide the HTML crap....
;;
;; Ideally though I would like to always have the text/plain part previewed!
;;
;; Setting the text/plain score high enough usually seems to do this, but
;; apparently it has to be done after mime-view is loaded.
;;
(eval-after-load "mime-view"
  '(progn
     (setq mime-view-type-subtype-score-alist
	   '(((text . plain) . 6)	; ALWAYS preferred!!!
	     ((text . enriched) . 5)	; RFC 1896
	     ((text . richtext) . 4)	; RFC 1341/1521 (deprecated/obsolete)
	     ((text . html) . 3)	; Gak!
	     (t . 2)))			; everything else
     ;; Perhaps this might also help
     ;;
     (set-modified-alist 'mime-view-type-subtype-score-alist
			 '(((multipart . mixed) . 3)))
     (set-modified-alist 'mime-view-type-subtype-score-alist
			 '(((multipart . related) . 3)))
     (set-modified-alist 'mime-view-type-subtype-score-alist
			 '(((multipart . alternative) . 3)))
     )
  )

;; these were added more recently and when set they fix a glaring mistake in
;; showing of MIME "buttons" for each MIME part in a message
;;
(setq mime-view-multipart/alternative-show-all-children t) ; XXX the default is nil!  WHY???
(setq mime-view-multipart/related-show-all-children t) ; XXX the default is nil!  WHY???

;; "content-type: plain/text; format=flowed" are automatically re-filled by
;; wanderlust (well, by SEMI) to the following width (with the default width
;; taken from `fill-column').
;;
(setq mime-display-text/plain-flowed-fill-column 42)

;; However some mailers also send whole paragraphs as single fixed lines!
;;
(defvar mime-display-text/plain-hook-fill-column 66
  "Fill column for plain/text in messages matching reflow-for-buggy-mailers
  or reflow-for-senders.  Default is `fill-column'")

(defun mime-display-text/plain-hook-fill-lines ()
  "Helper to re-flow plain/text parts in mail messages."
  (let ((fill-column
	 (cond
	  ((and (integerp mime-display-text/plain-hook-fill-column)
		(< mime-display-text/plain-hook-fill-column 1))
	   (+ (window-width) mime-display-text/plain-hook-fill-column))
	  ((integerp mime-display-text/plain-hook-fill-column)
	   mime-display-text/plain-hook-fill-column)
	  ((numberp mime-display-text/plain-hook-fill-column)
	   (floor
	    (* (window-width) mime-display-text/plain-hook-fill-column)))
	  (mime-display-text/plain-hook-fill-column
	   (eval mime-display-text/plain-hook-fill-column))
	  (t fill-column)))
	(count 0))
    ;; XXX this is good enough, but not ideal as it doesn't preserve blank lines
    ;; in quoted text.
    (fill-individual-paragraphs (point-min)(point-max) nil
				mail-citation-prefix-regexp))) ; requires 20.3?

;; N.B.:  Some mailers, notably newer versions of Apple Mail (e.g. some time
;; after version "(2.919.2)" and before "(2.3445.104.11)"), don't insert the
;; "format=flowed" attribute even though they normally spit out one-line
;; paragraphs that really do need re-flowing.
;;
;; This is currently used with `string-prefix-p', which works for the Apple set,
;; but maybe it should be a list of regexs?  Also maybe they should be matched
;; against the contents of the "User-Agent" field as well? (though for now the
;; known troublmakers are using X-Mailer)
;;
(defcustom mime-display-text/plain-reflow-for-buggy-mailers
  '("Apple Mail" "iPhone Mail" "iPad Mail")
  "A list of mailers that don't set format=flowed properly.
These should be just the first part of the 'X-Mailer' header
value, without any version info."
  :group 'mime-view
  :type '(repeat string))

;; Similarly some senders don't use format=flowed and also send one-line
;; paragraphs, making for difficult reading
;;
;; XXX this doesn't work quite so well for technews as they send some parts that
;; look like paragraphs, but which should have been kept as a list of lines,
;; e.g. the subject summary.
;;
(defcustom mime-display-text/plain-reflow-for-senders
  '("technews-editor@acm.org" "learning@acm.org")
  "A list of regexps for senders that don't set format=flowed properly.
These should match the 'from' header value."
  :group 'mime-view
  :type '(repeat regexp))

(defun my-mime-display-text/plain-helper ()
  "Re-reflow a text/plain part."
  (let ((e-beg (point-min))
	(e-end (point-max)))
    (save-restriction
      (widen)
      (let* ((x-mailer (std11-field-body "x-mailer"))
	     (is-buggy (car
			(memq t (mapcar
				 (lambda (s)
				   (string-prefix-p ; xxx use string-match-p too???
                                    s (or x-mailer "")))    ; xxx pass t for IGNORE-CASE too???
				 mime-display-text/plain-reflow-for-buggy-mailers))))
	     (sender (std11-field-body "from"))
	     (bad-sender (not (null
			       (delete nil (mapcar
                                            (lambda (s)
                                              (string-match-p s sender))
                                            mime-display-text/plain-reflow-for-senders))))))
	(narrow-to-region e-beg e-end)
	(message "Would we reflow message by %s 'X-Mailer: %s' from %s '%s'?"
;;; XXX once upon a time the caller's parameters were in scope, but no longer in
;;; emacs 26.1?
;;;
;;;		 (if (boundp 'situation)
;;;		     (or (cdr (assoc "format" situation)) "fixed")
;;;		   "<UNKNOWN>(fixed?)")
		 (if is-buggy "BUGGY" "normal")
		 (or x-mailer "<NIL>")
		 (if bad-sender "BAD sender" "sender")
		 sender)
	(if (or is-buggy bad-sender)
	    (mime-display-text/plain-hook-fill-lines))))))

(add-hook 'mime-display-text/plain-hook 'my-mime-display-text/plain-helper)

;; To write format=flowed messages we add “--[[text/plain; format=flowed]]" at
;; the beginning of the message, using this hook, then to truly do the right
;; thing when composing one might use the equivalent of something like this:
;;
;; (require "http://www.pps.univ-paris-diderot.fr/~jch/software/files/format-flowed.el")
;; (add-hook 'mime-edit-translate-hook 'format-flowed-translate-message)
;;
;; However I haven't enabled that part yet as I don't want it to happen
;; automatically behind the scenes while sending!  I (think I) want an editing
;; mode which does proper "format=flowed" encoding on the fly.
;;
(defun my-mail-setup-format-flowed ()
  (save-excursion
    (mail-text)				; xxx hmmm... not working here?
    (mime-edit-insert-tag "text" "plain" "; format=flowed")))
;;
;; XXX I also don't want this always, i.e. without having some editing mode
;; which does proper "format=flowed" encoding on the fly....
;;
;(add-hook 'wl-mail-setup-hook 'my-mail-setup-format-flowed)

;; hmmm....  automatic word-wrap is great, but there are some aspects of
;; `visual-line-mode' might not always be desiable.
;;
(require 'simple)
(if (fboundp 'visual-line-mode)
    (add-hook 'mime-view-mode-hook '(lambda () (visual-line-mode t))))

;; XXX setting this is a bit annoying -- it then requires the key (and if
;; necessary the key passphrase) be given before beginning to edit the draft
;; buffer, *AND* every time the draft buffer is saved!
;;
;(setq-default mime-edit-pgp-processing '(sign))
(setq-default mime-edit-pgp-processing nil) ; it is the default

(setq mime-edit-pgp-verbose t)
;; XXX this was necessary to see any keys in the *Keys* buffer when signing
;; (it should not be necessary -- just list secret keys if this is nil!!!)
;; xxx ideally of course the desired key would be chosen from the From line!
(setq mime-edit-pgp-signers '("woods@planix.com" "woods@robohack.ca"))

(setq mime-edit-pgp-encrypt-to-self t)	; hmmm....

(setq mime-setup-enable-egp t)		; it is the default if egp is installed

;; n.b.:  when replying to an encrypted message, if you want to quote from the
;; encrpyted text, you will need to decrypt it by typing C-c C-v C-c (a.k.a.
;; M-x mime-preview-show-content) with the cursor on the PGP/MIME part (i.e. on
;; the entity-button for the application/pgp-encrypted MIME part) to decrypt the
;; content into the message buffer (i.e. instead of just pressing 'v' (i.e. M-x
;; mime-preview-play-current-entity) to "play" then MIME part).

;; the default uses:  (if (executable-find "gpg2") "gpg2" "gpg")
;(setq epg-gpg-program "gpg2")		; for NetBSD pkgsrc
;(setq epg-debug t)	;; for tracing epg things
;; debug output goes to the " *epg-debug*" buffer.
;; Note that the buffer name starts with a space.

(when (elisp-file-in-loadpath-p "pinentry")
  ;; you must add the line "allow-emacs-pinentry" to "~/.gnupg/gpg-agent.conf"
  ;; (and maybe "allow-loopback-pinentry" too, see `epa-pinentry-mode' below)
  ;;
  (require 'pinentry)
  ;;
  ;; xxx this may not be necessary in emacs-25.3, but it doesn't seem to hurt, and
  ;; it supposedly will be necessary in 26.x:
  ;;
  ;; N.B.:  it now requires "allow-loopback-pinentry" in ~/.gnupg/gpg-agent.conf
  ;;
  (setq epa-pinentry-mode 'loopback)
  ;;
  ;; XXX sometimes the wrong emacs instance will ask for the passphrase, and
  ;; unfortunately just re-running the following won't fix that problem, though it
  ;; may make it possible to at least use the other instance and get on with
  ;; sending a message...
  ;;
  ;; The real solution seems to be to run `pinentry-stop' in all instances
  ;; (including the desired one) and then run `pinentry-start' in the desired
  ;; instance.
  ;;
  (pinentry-start))

(when (elisp-file-in-loadpath-p "pgg.el")
  (if (not (fboundp 'pgg-display-error-buffer))
      (defun pgg-display-error-buffer ()
        "Pop up an error buffer indicating the reason for an en/decryption failure."
        (let ((temp-buffer-show-function
               (function pgg-temp-buffer-show-function)))
          (with-output-to-temp-buffer pgg-echo-buffer
            (set-buffer standard-output)
            (insert-buffer-substring pgg-errors-buffer))))))

;; more handy stuff for ~/.gnupg/gpg-agent.conf:
;;
;;	max-cache-ttl 14400
;;	default-cache-ttl 14400
;;
;; remember:  gpgconf --reload gpg-agent

;; XXX unfortunately we have to re-define this whole monster to avoid having it
;; hide the typing.  Stupid policies should not be implemented in code!
;; Mechanisms to allow them, but never enforce them!  Ideally this would only be
;; done for passphrases, not for IMAP passwords, but it is too blunt a hammer
;; given the many current stupidities in epa
;;
;; XXX XXX XXX this version only works with 
;;
;; (defun read-passwd (prompt &optional confirm default)
;;   "Read a password, prompting with PROMPT, and return it.
;; If optional CONFIRM is non-nil, read the password twice to make sure.
;; Optional DEFAULT is a default password to use instead of empty input.

;; Once the caller uses the password, it can erase the password
;; by doing (clear-string STRING)."
;;   (if confirm
;;       (let (success)
;;         (while (not success)
;;           (let ((first (read-passwd prompt nil default))
;;                 (second (read-passwd "Confirm password: " nil default)))
;;             (if (equal first second)
;;                 (progn
;;                   (and (arrayp second) (not (eq first second)) (clear-string second))
;;                   (setq success first))
;;               (and (arrayp first) (clear-string first))
;;               (and (arrayp second) (clear-string second))
;;               (message "Password not repeated accurately; please start over")
;;               (sit-for 1))))
;;         success)
;;     (let (minibuf)
;;       (minibuffer-with-setup-hook
;;           (lambda ()
;;             (setq minibuf (current-buffer))
;;             ;; Turn off electricity.
;;             (setq-local post-self-insert-hook nil)
;;             (setq-local buffer-undo-list t)
;;             (setq-local select-active-regions nil)
;;             (use-local-map read-passwd-map)
;;             (setq-local inhibit-modification-hooks nil) ;bug#15501.
;; 	    (setq-local show-paren-mode nil)		;bug#16091.
;; ;; XXX no!  (add-hook 'post-command-hook 'read-password--hide-password nil t)
;;             )
;;         (unwind-protect
;;             (let ((enable-recursive-minibuffers t)
;; 		  (read-hide-char (or read-hide-char ?.)))
;;               (read-string prompt nil t default)) ; t = "no history"
;;           (when (buffer-live-p minibuf)
;;             (with-current-buffer minibuf
;;               ;; Not sure why but it seems that there might be cases where the
;;               ;; minibuffer is not always properly reset later on, so undo
;;               ;; whatever we've done here (bug#11392).
;; ;; XXX        (remove-hook 'after-change-functions 'read-password--hide-password
;; ;; XXX                     'local)
;;               (kill-local-variable 'post-self-insert-hook)
;;               ;; And of course, don't keep the sensitive data around.
;;               (erase-buffer))))))))

;; xxx this thing has stupid help text, no docstring, and no way to improve it!
;; redefine the whole thing until it can be improved properly.
;;
(defun epa--select-keys (prompt keys)
  (unless (and epa-keys-buffer
               (buffer-live-p epa-keys-buffer))
    (setq epa-keys-buffer (generate-new-buffer "*Keys*")))
  (with-current-buffer epa-keys-buffer
    (epa-key-list-mode)
    ;; C-c C-c is the usual way to finish the selection (bug#11159).
    (define-key (current-local-map) "\C-c\C-c" 'exit-recursive-edit)
    (let ((inhibit-read-only t)
	  buffer-read-only)
      (erase-buffer)
      (insert prompt "\n"
	      (substitute-command-keys "\
- `\\[epa-mark-key]' to mark a key on the line
- `\\[epa-unmark-key]' to unmark a key on the line\n
- `\\[exit-recursive-edit]' or `C-cC-c' to finish (or `\\[abort-recursive-edit]' or `q' to cancel)\n"))
      (widget-create 'link
		     :notify (lambda (&rest _ignore) (abort-recursive-edit))
		     :help-echo
		     "Click here or \\[abort-recursive-edit] to cancel"
		     "Cancel")
      (widget-create 'link
		     :notify (lambda (&rest _ignore) (exit-recursive-edit))
		     :help-echo
		     "Click here or \\[exit-recursive-edit] to finish"
		     "OK")
      (insert "\n\n")
      (epa--insert-keys keys)
      (widget-setup)
      (set-keymap-parent (current-local-map) widget-keymap)
      (setq epa-exit-buffer-function #'abort-recursive-edit)
      (goto-char (point-min))
      (let ((display-buffer-mark-dedicated 'soft))
        (pop-to-buffer (current-buffer))))
    (unwind-protect
	(progn
	  (recursive-edit)
	  (epa--marked-keys))
      (kill-buffer epa-keys-buffer))))

;; XXX almost no modern GUI-based reader can re-assemble split messages!
;;
(setq mime-edit-split-message nil)

;; XXX this function is a copy of the original done simply to change the
;; default value for the encoding to be quoted-printable instead of base64
;;
;; (quoted-printable is infinitely easier for humans to read than base64!)
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

;; Mailing List managers all too often break BASE64 encoded entities by
;; _BLINDLY_ adding plain text to the tail of each message (i.e. in a
;; non-MIME-compatible way)
;;
;; This is a workaround for BASE64 with trailing garbage
;;
(require 'mime-def)
(mel-define-method mime-decode-string (string (nil "base64"))
		   (condition-case error
		       (base64-decode-string string)
		     (error
		      (catch 'done
			(when (string-match "\\([A-Za-z0-9+/ \t\r\n]+\\)=*" string)
			  (let ((tail (substring string (match-end 0)))
				(string (match-string 1 string)))
			    (dotimes (i 3)
			      (condition-case nil
				  (progn
				    (setq string (base64-decode-string string))
				    (throw 'done (concat string tail)))
				(error))
			      (setq string (concat string "=")))))
			(signal (car error) (cdr error))))))

;; XXX GRRR!  It seems this is impossible to do from here!
;; (error on startup: "eval-buffer: Symbol's value as variable is void: mime-view-mode-map")
;;
;; xxx these wouldn't be right anyway -- what I want are keys to show the raw
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

;; note: 'msg-split-horiz and 'split-horiz are features only recently pushed
;; upstream from my private patches
;;
(setq wl-draft-buffer-style 'msg-split-horiz)
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
(setq wl-fcc "%INBOX/Sent@mailbox.weird.com")
;;
;; for network-free offline support
;;(setq wl-fcc "+sent")

;; this should work for me, but it won't work for everyone
;; (and it especially won't work for mobile laptop users!)
;; XXX should probably use variable `mail-host-address' instead of system-name!
;;
(setq system-fqdn (if (string-match "\\." (system-name))
		      (system-name)
		    (concat (system-name) "." wl-local-domain)))
;;(setq wl-envelope-from (concat user-login-name "@" system-fqdn))
;;
;; Actually.... this might work for many uses....
;;
;(setq wl-envelope-from (concat user-login-name "@" wl-local-domain))
(setq wl-envelope-from (concat user-login-name "@mail.weird.com"))

;; a good default, but may be adapted by wl-draft-config-alist as below
;; (if unset the `user-full-name' and `user-mail-address' are used)
;;
;(setq wl-from "\"Greg A. Woods\" <Greg.A.Woods@avoncote.ca>")

;; SMTP server for mail posting.  Default: `nil'
;;
;; Used by `wl-draft-send-mail-with-smtp', the default value for
;;`wl-draft-send-mail-function'.
;;
;; With "localhost" in both we assume a local SMTP server on port#25 that can
;; properly route to the world....
;;
(setq smtp-fqdn "more.local")		; used in FLIM's smtp.el
(setq wl-smtp-posting-server "more.local") ; used in wl-draft.el

;; NNTP server for news posting.  Default: `nil'
;;
;; (only for cross-posting mail to Usenet?)
;;
;(setq wl-nntp-posting-server "news.weird.com")
;(setq wl-nntp-posting-server (let ((envvalue (getenv "NNTPSERVER")))
;			       (if (or (null envvalue)
;				       (string-equal envvalue ""))
;				   nil
;				 (if (string-equal "." (substring envvalue 0 1))
;				     (substring envvalue 1)
;				   envvalue))))

;; used to show "To: recip" in summary lines for messages sent by user
;;
;; also used to eliminate alternate addresses from destination fields in draft
;; buffers
;;
;; note: supersedes `wl-user-mail-address-list' so only this one can be used if
;; we want to match the same wildcard mailbox forms used in the aliases file(s)
;;
(setq wl-user-mail-address-regexp
      "^\\(\\(woods\\([-+][^@]*\\)?@\\(\
\\(avoncote\\.\\ca\\)\\|\
\\(weird\\.\\(com\\|ca\\)\\)\\|\
\\(weird\\.toronto\\.ca\\)\\|\
\\(planix\\.\\(ca\\|com\\|net\\)\\)\\|\
\\(robohack\\.planix\\.\\(ca\\|com\\|net\\)\\)\\|\
\\(robohack\\.\\(ca\\|org\\)\\)\
\\(robo-hack\\.\\(ca\\|com\\)\\)\
\\)\
\\)\
\\|greg\\(\\.a\\)?\\.woods\\([-+][^@]*\\)?@avoncote\\.\\ca\\|\
\\|gwoods@acm\\.\\org\\|\
\\|woods\\.greg\\.a\\(+[^@]*\\)?@gmail\\.com\\)$")

;; also turn on flyspell explicitly
;;
(add-hook 'wl-mail-setup-hook (lambda ()
				(turn-on-flyspell)))

;; and enable mail-abbrevs too!
;;
(add-hook 'wl-mail-setup-hook (lambda ()
				(mail-abbrevs-setup)))

;; also incorporate aliases from ~/.mailrc:
;;
(setq wl-address-init-function 'my-wl-address-init)
(defun my-wl-address-init ()
  (wl-local-address-init)
  (setq wl-address-completion-list
	(append wl-address-completion-list (build-mail-aliases))))

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
;; XXX or maybe it is....
;;
;; XXX The only work-around I know for now is to mark the entire body of the
;; message as as region just before I'm ready to send and then invoke
;;`mime-encode-region' with the now-default value of "quoted-printable".
;;
;;	C-t M-<SPACE> M-> M-x mime-encode-region <RETURN> <RETURN>
;;
;; It doesn't seem like WL/SEMI/FLIM implements quoted-printable encoding
;; properly, or maybe even not at all (i.e. setting the following advice seems
;; to only half work).
;;
;; I do see the MIME tag as "[[text/plain][quoted-printable]]", and messages
;; will have a "Content-Transfer-Encoding: quoted-printable" header, but no
;; such encoding is performed automatically before they are sent, as one might
;; hope and expect!
;;
;; It may be that `mime-edit-translate-body' doesn't do any encoding work, but
;; if not, how to make it do so and yet avoid encoding files, etc. that have
;; been inserted with their encoding already done?  It probably doesn't do any
;; encoding because it assumes the encoding was already done when the file was
;; inserted, but this is not true for `mime-edit-insert-text' or the equivalent
;; since that's likely just text typed by the user.
;;
;; Maybe when we're about to send a message we should first run through all the
;; MIME parts (instead of setting the following advice) and transform any
;; [[text/*]] tags without a specified encoding to add the [quoted-printable]
;; encoding and of course actually do the necessary quoted-printable encoding
;; automatically as well at that time.  All text will be more robustly
;; transmitted through e-mail if it is encoded somehow, and quoted-printable is
;; the least intrusive, showing its ugly head only if it is absolutely
;; necessary.
;;
;; XXX it would be nice too if we could put the MIME tag before the cited text,
;; if any, and not just in front of the newly inserted signature....
;;
(defadvice mime-edit-insert-signature (after my-mime-edit-signature-set-qp-encoding activate)
  "Add a charset to the MIME tag for the signature of the message."
  (progn
    ;; xxx even if the buffer is edited in utf-8, the act of running
    ;; `mime-encode-region' seems to cause any non-ASCII characters to be
    ;; encoded as ISO-8859-1 characters, but then for any mailer to interpret
    ;; them properly it has to be told what they are in the content-type
    ;; header.
    ;;
    (mime-edit-define-charset 'iso-8859-1)
;;
;; hmmmm.... this _isn't_ necessary! (maybe because the charset is set)
;;
;; (maybe we could change the charset to utf-8 eventually now too....)
;;
;;  (mime-edit-define-encoding "quoted-printable")
    ))

(setq wl-auto-save-drafts-interval 5)	; default was 1, will be 300

;; The default draft folder, first set up the local draft folder name.
;;
;; Someday this may be adjusted at draft buffer creation time by settings in
;; `wl-draft-config-alist', but see the note there about how the new draft
;; buffer is already associated with the original draft folder and the new
;; value is only set in a buffer-local copy in the new draft buffer.
;;
;; WARNING:  currently `wl-summary-reedit' does a plain `string=' comparison
;; against the value of `wl-draft-folder' to decide whether or not the folder
;; in question is a "draft" folder and thus do the right magic to supersede the
;; original message with the one being edited, and again to delete the draft
;; message once it has been successfully sent (or when it is killed).
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

(defun my-wl-draft-set-send-via-gmail ()
  "Set message up to be relayed through Gmail."
  (interactive)
  (setq-local wl-smtp-posting-user "Woods.Greg.A@gmail.com")
  (setq-local wl-smtp-posting-server "smtp.gmail.com")
  (setq-local wl-smtp-posting-port 587)
  (setq-local wl-smtp-connection-type 'starttls)
  (setq-local wl-smtp-authenticate-type "plain"))
(define-key wl-draft-mode-map "\C-cG" 'my-wl-draft-set-send-via-gmail)

;; add a (pgp-sign . BOOL) action for wl-draft-config-alist
(unless (assq 'pgp-sign wl-draft-config-sub-func-alist)
  (wl-append wl-draft-config-sub-func-alist
	     '((pgp-sign . mime-edit-set-sign))))

;; add a (pgp-encrypt . BOOL) action for wl-draft-config-alist
(unless (assq 'pgp-encrypt wl-draft-config-sub-func-alist)
  (wl-append wl-draft-config-sub-func-alist
	     '((pgp-encrypt . mime-edit-set-encrypt))))

;; try to predict who I should be....
;;
;; XXX there's probably a more efficient way to write this
;;
;; xxx similar things can also be done with "templates", which are set in
;; `wl-template-alist'
;;
;; Note that with `wl-template-alist' set, another template can be chosen while
;; composing the message with C-c C-j
;;
;; add ("FCC" . "%INBOX/Sent@mailbox.domain") to set FCC...
;;
;; What about using this too:
;;
;;	;; If non-nil, applied only one element of `wl-draft-config-alist'.
;;	(setq wl-draft-config-matchone t)
;;
;; N.B.:  All matching conditions are applied!  There doesn't seem to be any way
;; to define a condition that prevents any other condition from matching, nor is
;; there any way to define a condition that only matches for messages that
;; didn't match any other condition -- so, the actions for the "default"
;; condition at the bottom "(or t)" are also always applied.
;;
;; XXX When set here, `wl-draft-folder' is properly set inside the draft
;; buffer, but the draft is still not saved to the proper draft folder!!!
;;
(setq wl-draft-config-alist
      '((reply
	 "^[Ff]rom: [\"]?Andreas Wrede[\"]?"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@weird.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@weird.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	;; XXX the following are perhaps not ideal REs....
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\\|[fF]rom\\): .*@.*planix\\."
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\\|[fF]rom\\): .*@.*robohack\\.ca"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@robohack.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@robohack.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Robo-Hacker"))
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\\|[fF]rom\\): .*@.*avoncote\\.ca"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <Greg.A.Woods@avoncote.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <Greg.A.Woods@avoncote.ca>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Avoncote Farms"))
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\\|[fF]rom\\): .*gwoods@acm\\.org"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <gwoods@acm.org>")
	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@acm.org>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
;	(reply
;	 "^From: .*@.*citrix\\."
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <greg.woods@citrix.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <greg.woods@citrix.com>")
;	 ("FCC" . "%Sent Messages:t_gregwo@mail.citrix.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Citrix Systems"))
;	(reply
;	 "^From: .*@.*teloip\\."
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("FCC" . "%INBOX.Sent:gwoods@mail.teloip.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "TELoIP Inc."))
;	(reply
;	 "^From: .*@.*clasix\\.net"
;	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
;	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
;	 ("X-Priority" . "2")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Planix, Inc."))
;	(reply
;	 "^From: .*@.*seawellnetworks\\.com"
;	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
;	 ("X-Priority" . "2")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Planix, Inc."))
	(reply
	 "^[Ff]rom: [\"]?Scott Lindsay[\"]?"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^[Ff]rom: .*Ted Gray.*"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("X-Priority" . "2")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
;	(reply
;	 "^[Ff]rom: .*@.*\\(aci\\|opc\\)\\.on\\.ca"
;	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
;	 ("Precedence" . "first-class")
;	 ("X-Priority" . "2")
;	 ("Organization" . "Planix, Inc."))
	(reply
	 "^[Ff]rom: .*@.*\\(lawyermediator\\|gelmanlaw\\)\\.ca"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("X-Priority" . "1"))
	((string-match "^%INBOX/planix.*@mailbox\\.weird\\.com"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%.*@mail\\.planix\\.com"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Precedence" . "first-class")
	 ("Organization" . "Planix, Inc."))
;	((string-match "^%.*@mail\\.citrix\\.com"
;		       wl-draft-parent-folder)
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <greg.woods@citrix.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <greg.woods@citrix.com>")
;	 ("FCC" . "%Sent Messages:t_gregwo@mail.citrix.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Citrix Systems"))
;	((string-match "^%.*@mail\\.teloip\\.com" wl-draft-parent-folder)
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@teloip.com>")
;	 ("FCC" . "%inbox.Sent:gwoods@mail.teloip.com:993!")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "TELoIP, Inc."))
	((string-match "^%.*@imap\\.gmail\\.com"
		       wl-draft-parent-folder)
	 (pgp-sign . nil)
;XXX	 (wl-draft-folder . "%[Gmail]/Drafts:Woods.Greg.A@imap.gmail.com:993!")
	 ("From" . "\"Greg A. Woods\" <Woods.Greg.A@gmail.com>")
	 ("Reply-To" . "\"Greg A. Woods\" <Woods.Greg.A@gmail.com>")
	 ;; there is no need to use Fcc in gmail if you used the gmail SMTP
	 ;; server. gmail saves a copy of all sent messages automatically.
;	 ("FCC" . "%[Gmail]/Sent Mail:Woods.Greg.A@imap.gmail.com:993!")
	 ("FCC" . "")
	 ("Precedence" . "first-class")
	 ("Organization" . "Me, Myself, and I")
	 (wl-smtp-posting-user . "Woods.Greg.A@gmail.com")
	 (wl-smtp-posting-server . "smtp.gmail.com")
	 (wl-smtp-posting-port . 587)
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-authenticate-type . "plain"))
; 	((string-match "^%.*@klervi\\.com"
; 		       wl-draft-parent-folder)
; 	 (pgp-sign . nil)
; ;XXX	 (wl-draft-folder . "%[Gmail]/Drafts:\"g.woods@klervi.com\"@imap.gmail.com:993!")
; 	 ("From" . "\"klervi - Greg A. Woods\" <g.woods@klervi.com>")
; 	 ("Reply-To" . "\"klervi - Greg A. Woods\" <g.woods@klervi.com>")
; 	 ("Precedence" . "first-class")
; 	 ("Organization" . "GIR Nord Amerique Inc.")
; ;;;	 (wl-smtp-posting-user . "gaw")
; ;;;	 (wl-smtp-posting-server . "celcius.klervi.com")
; ;;;	 (wl-smtp-posting-port . 465)
; ;;;	 (wl-smtp-connection-type . 'ssl)
; ;;;	 (wl-smtp-authenticate-type . "plain"))
; ;;;	 ;; there is no need to use Fcc in gmail if you used the gmail SMTP
; ;;;	 ;; server. gmail saves a copy of all sent messages automatically.
; ;;;	 ;; however since we now use celcius....
; ;;;	 ("FCC" . "%[Gmail]/Sent Mail:\"g.woods@klervi.com\"@imap.gmail.com:993!")
; ;;; or with gmail....
; 	 (wl-smtp-posting-user . "g.woods@klervi.com")
; 	 (wl-smtp-posting-server . "smtp.gmail.com")
; 	 (wl-smtp-posting-port . 587)
; 	 (wl-smtp-connection-type . 'ssl)
; 	 ("FCC" . "")
; 	 )
;	((string-match "^%.*:woods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <woods@aci.on.ca>")
;	 ("Reply-To" . "\"Greg A. Woods\" <woods@aci.on.ca>")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Planix, Inc."))
;	((string-match "^%.*:gwoods@mailbox\\.aci\\.on\\.ca" wl-draft-parent-folder)
;	 (pgp-sign . nil)
;	 ("From" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
;	 ("Reply-To" . "\"Greg A. Woods\" <gwoods@aci.on.ca>")
;	 ("Precedence" . "first-class")
;	 ("Organization" . "Planix, Inc."))
	;; mailing list:  emacs-mime-en
	((or (string-match "^%INBOX/Lists-IN/emacs-mime-en-l"
			   wl-draft-parent-folder)
	     (string-match "^%INBOX/list-archive/emacs-mime-en"
			   wl-draft-parent-folder))
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^Delivered-To: emacs-mime-en@m17n.org"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^[Tt]o: woods-emacs-mime-en-l@weird.com"
	 (pgp-sign . nil)
	 ("From" . "\"Greg A. Woods\" <woods-emacs-mime-en-l@weird.com>")
	 ("To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Reply-To" . "EMACS-MIME Users Mailing List (English) <emacs-mime-en@m17n.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  cyrus-devel
	(reply
	 "^List-Id: [^<]*<cyrus-devel.lists.andrew.cmu.edu>"
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <cyrus-devel@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <cyrus-devel@lists.andrew.cmu.edu>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  info-cyrus
	(reply
	 "^List-Id: .*<info-cyrus.lists.andrew.cmu.edu>"
	 (pgp-sign . nil)
         ("From" . "\"Greg A. Woods\" <woods-cyrus@weird.com>")
	 ("To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Reply-To" . "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  ctwm
	((string-match "^%INBOX/Lists-IN/ctwm-list"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("To" . "The CTWM Mailing List <ctwm@ctwm.org>")
	 ("Reply-To" . "The CTWM Mailing List <ctwm@ctwm.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  git
	((string-match "^%INBOX/Lists-IN/git-list"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Reply-To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^List-Id: .*<git.vger.kernel.org>"
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Reply-To" . "The Git Mailing List <git@vger.kernel.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  nsd-users
	((string-match "^%INBOX/Lists-IN/nsd-users"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Reply-To" . "The NSD User's Mailing List <nsd-users@NLnetLabs.nl>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^List-Id: .*<nsd-users.NLnetLabs.nl>"
	 (pgp-sign . t)
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
	 "^[Dd]elivered-[Tt]o: .*@[Nn][Ee][Tt][Bb][Ss][Dd]\\.[Oo][Rr][Gg]"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.com>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	((string-match "^%INBOX/Lists-IN/netbsd-lists"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^[Ll]ist-[Ii]d: .*\\.[Nn][Ee][Tt][Bb][Ss][Dd]\\.[Oo][Rr][Gg]>"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("Reply-To" . "")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  tuhs
	((string-match "^%INBOX/Lists-IN/tuhs"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@robohack.ca>")
	 ("To" . "The Unix Heritage Society mailing list <tuhs@tuhs.org>")
	 ("Reply-To" . "The Unix Heritage Society mailing list <tuhs@tuhs.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  unbound-users
	((string-match "^%INBOX/Lists-IN/unbound-users"
		       wl-draft-parent-folder)
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Reply-To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Organization" . "Planix, Inc."))
	(reply
	 "^List-Id: .*<unbound-users.unbound.net>"
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Reply-To" . "The Unbound User's Mailing List <unbound-users@unbound.net>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list: u-u (unix unanimous)
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\|[Ss]ender\\): .*u-u@[^.]*\\.*unixunanimous\\.org"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods@planix.ca>")
	 ("To" . "Unix Unanimous Mailing List <u-u@unixunanimous.org>")
	 ("Reply-To" . "Unix Unanimous Mailing List <u-u@unixunanimous.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list:  wl-en
	((or (string-match "^%INBOX/Lists-IN/wl-en-l"
			   wl-draft-parent-folder)
	     (string-match "^%INBOX/list-archive/wl-en@"
			   wl-draft-parent-folder))
	 (pgp-sign . t)
         ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@ml.gentei.org>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@ml.gentei.org>")
	 ("Organization" . "Planix, Inc."))
	;; mailing list wl-en
	(reply
	 "^\\([Tt][Oo]\\|[Cc][Cc]\\): wl-en@"
	 (pgp-sign . t)
	 ("From" . "\"Greg A. Woods\" <woods-wl-en-l@planix.com>")
	 ("To" . "WanderLust Users Mailing List (English) <wl-en@ml.gentei.org>")
	 ("Reply-To" . "WanderLust Users Mailing List (English) <wl-en@ml.gentei.org>")
	 ("Organization" . "Planix, Inc."))
	; defaults for everything (unfortunately this also always matches)
	((or t)
	 (pgp-sign . t)
	 mime-edit-insert-signature)))

(setq wl-insert-message-id nil)		; let our MTA do it....

(setq mime-setup-use-signature t)	;xxx hmmmm..... doesn't work?
(setq signature-insert-at-eof t)
(setq signature-file-alist
      '((("From" . "@planix\\.ca") . "~/.signature-planix.ca")
	(("From" . "@planix\\.com") . "~/.signature-planix.com")
	(("From" . "@klervi\\.com") . "~/.signature-klervi.com")
;	(("From" . "@citrix\\.") . "~/.signature-citrix.com")
;	(("From" . "@teloip\\.") . "~/.signature-teloip.com")
;	(("From" . "@aci\\.") . "~/.signature-aci-postmaster")
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

;; note `mime-edit-insert-signature' just inserts the signature boundary
(define-key wl-draft-mode-map "\C-cs" 'insert-signature)

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

;; since we do (set-language-environment "UTF-8") in .emacs....
;;
;; NOTE: for Latin-1 the correct mime-charset name is "ISO-8859-1"
;;
;; XXX the default value is "Q", and that seems to work for UTF-8
;;
;; XXX on the other hand since `mime-encode-region' seems to insist on
;; converting to ISO-8859-1 anyway....
;;
(setq default-mime-charset-for-write 'iso-8859-1)

;; try to keep deleted messages in the Trash folder on the same host
;;
;; it would be nice if there were some nifty way to simply say something
;; like "everywhere I have an INBOX, assume 'd' should refile into the
;; "Trash" folder living directly under that INBOX, including for all
;; other sub-folders of that INBOX" without having to enumerate them all
;; (and take special consideration of the ones where I have to use a
;; different port, different heirarchy separator, or whatever)
;;
;; NOTE:  On Gmail the default system folders (Sent, Trash, etc.) are locale
;; specific, i.e. the name depends on your language settings in gmail, and
;; these folders are placed in the top level folder called "%[Gmail]/".  Normal
;; folders (called "filters" by Gmail) have no prefix, and sub-folders are
;; delimited by "/".
;;
;; Note also that Gmail has a magic "%[Gmail]/All Mail" sub-folder in which all
;; mail is always stored, and this is the only place one can ever really truly
;; delete any mail from on Gmail.  (i.e. removing from Trash doesn't delete!)
;;
;; For reference:  Gmail config in ~/.folders (multiple accounts repeat this
;;section with different prefix and addresses):
;;
;;	Gmail IMAP folders{
;;		%:Woods.Greg.A@imap.gmail.com:993!/
;;	}
;;
;; Press C-RET to expand them all.
;;
(setq wl-dispose-folder-alist
      '(("^\\(/[^/]*/\\)?%.*[/.]Trash:.*$" . remove) ; this one must come first!!!
	("^\\(/[^/]*/\\)?%.*Deleted Messages:.*$" . remove) ; must appear before use as targett!!!
	("^\\(/[^/]*/\\)?%INBOX$" . "%INBOX/Trash")
	("^\\(/[^/]*/\\)?%INBOX[^@]*$" . "%INBOX/Trash")
	("^\\(/[^/]*/\\)?%.*:woods@mailbox.weird.com" . "%INBOX/Trash:woods@mailbox.weird.com:993!")
	("^\\(/[^/]*/\\)?%.*:Woods.Greg.A@imap.gmail.com" . "%[Gmail]/Trash:Woods.Greg.A@imap.gmail.com:993!")
	("^\\(/[^/]*/\\)?%[Gmail]/All Mail:Woods.Greg.A@imap.gmail.com" . remove)
	("^\\(/[^/]*/\\)?%.*:\"g.woods@klervi.com\"@imap.gmail.com" . "%[Gmail]/Trash:\"g.woods@klervi.com\"@imap.gmail.com:993!")
	("^\\(/[^/]*/\\)?%[Gmail]/All Mail:\"g.woods@klervi.com\"@imap.gmail.com" . remove)
;	("^\\(/[^/]*/\\)?%.*:t_gregwo@mail.citrix.com" . "%Deleted Messages:t_gregwo@mail.citrix.com:993!")
;	("^\\(/[^/]*/\\)?%.*:woods@mail.teloip.com" . "%inbox.Trash:woods@mail.teloip.com:993!")
;	("^\\(/[^/]*/\\)?%.*:gwoods@mailbox.aci.on.ca" . "%INBOX/Trash:gwoods@mailbox.aci.on.ca:993!")
;	("^\\(/[^/]*/\\)?%.*:woods@mailbox.aci.on.ca" . "%INBOX/Trash:woods@mailbox.aci.on.ca:993!")
	("^-" . remove)
	("^@" . remove)
	("^\\+trash$" . remove)
	("^\\+" . trash)))

;;; For junk handling
;;;
;;; see also wl-spam.el
;;;
;;;	(setq elmo-spam-scheme 'sa)
;;;	(require 'wl-spam)
;;;	(setq wl-spam-folder "%INBOX/spam")
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
      '(("^%INBOX.*Junk@" . null)	; this one must come first
	("^+junk" . null)		; this one too?
	("^\\(/[^/]*/\\)?%INBOX$" . "%INBOX/Junk")
	("^\\(/[^/]*/\\)?%INBOX[^@]*$" . "%INBOX/Junk")
	("^\\(/[^/]*/\\)?%.*:woods@mailbox.weird.com" . "%INBOX/Junk:woods@mailbox.weird.com:993!")
	("^\\(/[^/]*/\\)?%.*:Woods.Greg.A@imap.gmail.com" . "%[Gmail]/Spam:Woods.Greg.A@imap.gmail.com:993!")
	("^\\(/[^/]*/\\)?%.*:\"g.woods@klervi.com\"@imap.gmail.com" . "%[Gmail]/Spam:\"g.woods@klervi.com\"@imap.gmail.com:993!")
	))

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
;; Currently my mail server reports the following (from ". SELECT INBOX"):
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
;; Wed Jan  2 18:32:11 PST 2019
;;
;; * FLAGS (\Answered \Flagged \Draft \Deleted \Seen NonJunk Junk $NotJunk $Junk
;; MessageJunkMailLevel JunkRecorded MessageHasBeenViewed $Label5 $Label1
;; $Label2 $Label3 $Label4 $Forwarded Redirected NotJunk Forwarded proxy
;; proxy.net MyStuff Business Todo Private tectrol.com Jenny Ignore $MDNSent
;; $Pending $Label7 $MailFlagBit0 $MailFlagBit1 $MailFlagBit2 $Personal $Work)
;;
;; * OK [PERMANENTFLAGS (\Answered \Flagged \Draft \Deleted \Seen NonJunk Junk
;; $NotJunk $Junk MessageJunkMailLevel JunkRecorded MessageHasBeenViewed
;; $Label5 $Label1 $Label2 $Label3 $Label4 $Forwarded Redirected NotJunk
;; Forwarded proxy proxy.net MyStuff Business Todo Private tectrol.com Jenny
;; Ignore $MDNSent $Pending $Label7 $MailFlagBit0 $MailFlagBit1 $MailFlagBit2
;; $Personal $Work \*)]
;;
;; N.B.:  This is used with `assq' as in (assq flag elmo-imap4-flag-specs), so
;; the internal keyword can be repeated, but this will result in conversion of
;; message flags on the server to match the desired flag (this is done by
;; `elmo-imap4-flags-to-imap'), and that may not be desirable for good
;; interoperability.  On the other hand...
;;
;; Note: reusing `defconst' here hides the location of the original variable,
;; which was in elmo-imap4.el.
;;
(require 'elmo-imap4)
(defconst elmo-imap4-flag-specs '((important "\\Flagged") ; cannot change
				  (read "\\Seen")	  ; cannot change
				  (unread "\\Seen" 'remove) ; cannot change
				  (answered "\\Answered")   ; cannot change
				  (deleted "\\Deleted") ; not necessary here???
				  (draft "\\Draft")
				  ;;(recent "\\Recent") ; XXX per session, not STOREable
				  ;; draft-melnikov-imap-keywords-03.txt
				  ;; XXX see also RFC-5788 (was draft-melnikov-imap-keywords-10)
				  ;; and https://www.iana.org/assignments/imap-keywords/
				  (forwarded "$Forwarded")
				  (work "$Work")
				  (personal "$Personal")
				  (shouldreply "$ShouldReply")
				  ;; more!
				  (todo "Todo")
				  (private "Private")
				  (special "$Important")
				  ;(forwarded "Forwarded")
				  (redirected "Redirected")
				  (junkrecorded "JunkRecorded")
				  (phishing "$Phishing")
				  (notjunk "$NotJunk")
				  ;(notjunk "NotJunk")
				  (nonjunk "NonJunk")
				  (junk "$Junk")
				  ;(junk "Junk")
				  ))

;; the Junk action.
;;
;; For now we just move them into the appropriate Junk folder, but eventually
;; we might at least add IMAP (persistent) flags so that they appear as junk to
;; other types of clients, and also we might do some post-processing and
;; perhaps possibly forward some of them off to SpamCop and/or the source
;; addr's contacts, etc.
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

;; a font face for Junk lines in the Summary (those marked to be junked)
;; N.b.:  not the same as `wl-highlight-summary-junk-flag-face'
(wl-defface wl-highlight-summary-junk-face
  '((((type tty))
     (:foreground "grey"))
    (((class grayscale))
     (:foreground "DarkSlateGray" :strike-through "LightGray" :slant italic))
    (((class mono))
     (:foreground "grey" :strike-through "grey" :slant italic))
    (((class color))
     (:foreground "DarkSlateGray" :strike-through "LightGray" :slant italic)))
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
;; Folder window on the left side of the frame), so should we give up and pop a
;; new frame for it?
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

;; from https://idiomdrottning.org/svolternet
;;
;; Re: SV: Re: SV: Re: SV: Your misbehaved email
;;
;; This is something for Emacs’ message-mode that I made years and years ago but
;; forgot to publish. It’s just been working so well that I forgot about it
;; until now.
;;
;; When dealing with Scandinavian or Icelandic people they often have misbehaved
;; email clients that add “SV:” to a subject even if there is already a
;; “Re:”. And then if your own client is also misbehaved, it’ll add another Re
;; back on there, resulting in a ridic subject with a long chain of alternating
;; Re and SV.
;;
;; Obviously the fault is with the Scandinavian clients for not being aware of
;; basics of RFC 5322, which specifies Re:.
;;
;; When used in a reply, the field body MAY start with the string “Re: “ (an
;; abbreviation of the Latin “in re”, meaning “in the matter of”) followed by
;; the contents of the “Subject:” field body of the original message. If this is
;; done, only one instance of the literal string “Re:” ought to be used since
;; use of other strings or more than one instance can lead to undesirable
;; consequences.
;;
;; Them localizing is fine, localization is a good thing, but they should then
;; be aware of “Re:” and be responsible for cleaning up the chains. But OK. We
;; have Lisp so let’s be the adult in the room.
;;
;; Obvs for German, sub in AW instead of SV etc.
;;
(defun svolternet ()
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (while (save-excursion (re-search-forward "^subject:\\\( re:\\\| sv:\\\)\\\{2,\\\}" nil t))
          (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "^subject:\\\( re:\\\| sv:\\\) re:" nil t)
                  (replace-match "Subject: Re:" nil nil)))
          (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "^subject:\\\( re:\\\| sv:\\\) sv:" nil t)
                  (replace-match "Subject: SV:" nil nil))))))

;; not that I encounter this often, but I need equiv for WL
;(add-hook 'message-mode-hook
;          (lambda () (add-hook 'first-change-hook 'svolternet nil 'make-it-local)))

;;; Local Variables:
;;; emacs-lisp-docstring-fill-column: 77
;;; End:
