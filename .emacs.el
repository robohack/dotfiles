;;;;
;;;;	.emacs.el
;;;;
;;;;#ident	"@(#)HOME:.emacs.el	29.4	09/05/22 16:47:28 (woods)"
;;;;
;;;; per-user start-up functions for GNU-emacs v19.34 or newer
;;;;
;;;; primarily tested on v21.2
;;;;

;;; This file should be stored in "~/.emacs.el".
;;; Saving it will normally compile it into "~/.emacs.elc".
;;; Make a (symbolic) link from "~/.emacs" to "~/.emacs.elc" to use it.

;;; to debug, eval (^X^E) these after starting with "emacs -q":
;;;
;;; (setq debug-on-error t)
;;; (load-file "~/.emacs.el")
;;;
;;; more goodies for debug:
;;;
;;; (setq debug-on-error nil)
;;; (setq stack-trace-on-error t)
;;; (setq stack-trace-on-error nil)
;;; (setq debug-on-quit t)
;;; (setq debug-on-quit nil)

;;; helpers for calendar session
;;;
;;; eval this to stop appt-check:
;;; (cancel-timer appt-timer)
;;;
;;; eval this to re-start appt-check:
;;; (setq appt-timer (run-at-time t 60 'appt-check))

;; I don't want that annoying startup message.
(setq inhibit-startup-message t)

;;;; ----------
;;;; Let's make sure we're "home"....
;;;; if that's where we should be.
(if (<= (safe-length command-line-args) 1)
    (cd "~"))
;;;; ----------
;;;; stolen from cl.el -- find out where we are!

(eval-and-compile
  (defvar init-emacs-type
    (cond ((boundp 'emacs-major-version)	; first available in 19.23
	   emacs-major-version)
	  ((or (and (fboundp 'epoch::version)
		    (symbol-value 'epoch::version))
	       (string-lessp emacs-version "19"))
	   18)				; was there ever anything less?
	  (t 19))				; what else could it be?
    "Emacs major version for testing compatibility."))

(if (<= init-emacs-type 19)
    (progn
      (message "Not running emacs v20 or newer I see -- you may have trouble with this .emacs!")
      (sit-for 5)))

;; XXX string-to-int is apparently obsolete since 22.1
(if (not (fboundp 'string-to-number))
    (defalias 'string-to-number 'string-to-int))

;;; stolen by way of Len Tower from Noah Freidman from /home/fsf/friedman/etc/init/emacs/init.el
(defun emacs-version-get-component (component)
  (let ((old-match-data (match-data))
        (version 0)
        (regexp (cond
                 ((eq 'major component) "^\\([0-9]+\\)")
                 ((eq 'minor component) "^[0-9]+\\.\\([0-9]+\\)")
                 ((eq 'build component) "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"))))
    (unwind-protect
        (and (string-match regexp emacs-version)
             (setq version
                   (string-to-number (substring emacs-version
						(match-beginning 1)
						(match-end 1)))))
      (store-match-data old-match-data))
    version))

;; note the "official" variables are named like emacs-*-version, but they
;; didn't appear until 19.23
(defconst emacs-version-major (emacs-version-get-component 'major)
  "Major version number for this Emacs.")
(defconst emacs-version-minor (emacs-version-get-component 'minor)
  "Minor version number for this Emacs.")
(defconst emacs-version-build (emacs-version-get-component 'build)
  "Build number for this Emacs.")
;;; end by Noah Freidman from /home/fsf/friedman/etc/init/emacs/init.el

;;;; ----------
;;;; things to do for coding systems, MULE, etc.

(if (>= init-emacs-type 20)
    (setq inhibit-eol-conversion t))	; show M$ crap for what it is....

(if (>= init-emacs-type 20)
    (set-language-environment "Latin-1")
  (standard-display-european 1))

(if (and (>= init-emacs-type 20)
	 (not window-system))
    (set-terminal-coding-system 'iso-8859-1)) ; force the issue

;; XXX this may rely on v20 or even v21 features....
(if (not window-system)
    (set-input-mode nil nil t))		; turn on 8'th-bit META handling

;;;; ----------
;;;; What to do after this file has been loaded...
;;;;
(add-hook 'after-init-hook
	  (function
	   (lambda ()
	     "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'."
	     (progn
	       ;;;(setq debug-on-error t)	; need this to debug in here...
	       ;; (require 'time)	; this isn't provided by time.el!
	       (let ((process-connection-type nil)) ;pty's are limited, pipes aren't
		 (display-time))	; also autoload'ed
	       ;; XXX for some reason in 22.3 this appears to be on but doesn't
	       ;; work unless it's forced on like this.
	       (if (fboundp 'auto-compression-mode)
		 (auto-compression-mode 1))
	       ;;
	       ;; Message-Id: <9601081816.AA07579@alex.x.org>
	       ;; From: Stephen Gildea <gildea@x.org>
	       ;; To: bug-gnu-emacs@prep.ai.mit.edu
	       ;; Date: Mon, 08 Jan 1996 13:16:19 EST
	       ;; Subject: resize-minibuffer-mode should be on by default
	       ;;
	       ;; I just stumbled upon resize-minibuffer-mode (in Emacs 19.26-19.30);
	       ;; it is very nice.
	       ;;
	       ;; XXX but it's gone (and mostly automatic) in Emacs 22 and newer
	       ;;
	       (eval-when-compile
		 (defvar resize-minibuffer-mode))
	       (if (and (fboundp 'resize-minibuffer-mode)
			(not resize-minibuffer-mode))
		   (resize-minibuffer-mode)))))) ; also autoload'ed

;;;; ----------
;;;; get ready to load stuff

(defvar original-load-path load-path "The value of load-path at startup.")

;; prepend our private (even if it does not exist!) elisp library
(defvar private-lisp-dir
  (expand-file-name "~/lib/elisp")
  "The location of the user's private e-lisp library.")
;; add-to-list in 19.29 and newer
(add-to-list 'load-path private-lisp-dir)

;; prepend the $LOCAL version's site-lisp dir
;;
;; Note this may already be included with defaults specified in the file
;; `epaths.h' used when Emacs was built.  `add-to-list' avoids duplicates.
;;
(if (file-exists-p (concat (getenv "LOCAL")
			   "/share/emacs/site-lisp/default.el"))
    (progn
      (defvar local-site-lisp-dir 
	(concat (getenv "LOCAL") "/share/emacs/site-lisp")
	"Location of the site-lisp directory for local packages.")
      (add-to-list 'load-path local-site-lisp-dir)))

;; append the $PKG version's site-lisp dir if we're running the local version.
;;
;; Note this may already be included with defaults specified in the file
;; `epaths.h' used when Emacs was built.  `add-to-list' avoids duplicates.
;;
(if (and (file-exists-p (concat (getenv "PKG")
				"/share/emacs/site-lisp/default.el"))
	 (not (fboundp 'local-site-lisp-dir)))
    (progn
      (defvar pkg-site-lisp-dir 
	(concat (getenv "PKG") "/share/emacs/site-lisp")
	"Location of the site-lisp directory for add-on packages.")
      (add-to-list 'load-path pkg-site-lisp-dir 'prepend)))

;; This is only done in `normal-top-level' before ~/.emacs is loaded!
;; 
;; Look in each dir in load-path for a subdirs.el file.
;; If we find one, load it, which will add the appropriate subdirs
;; of that dir into load-path,
;; Look for a leim-list.el file too.  Loading it will register
;; available input methods.
(let ((tail load-path) dir)
  (while tail
    (setq dir (car tail))
    (let ((default-directory dir))
      (message (concat "~/.emacs.el: loading " (expand-file-name "subdirs.el")))
      (load (expand-file-name "subdirs.el") t t t))
    (let ((default-directory dir))
      (message (concat "~/.emacs.el: loading " (expand-file-name "leim-list.el")))
      (load (expand-file-name "leim-list.el") t t t))
    ;; We don't use a dolist loop and we put this "setq-cdr" command at
    ;; the end, because the subdirs.el files may add elements to the end
    ;; of load-path and we want to take it into account.
    (setq tail (cdr tail))))

(defvar original-vc-path vc-path "The value of vc-path at startup.")
(setq vc-path
      (if (file-directory-p "/usr/sccs")
	  '("/usr/sccs")
	(if (file-directory-p "/usr/local/libexec/cssc")
	    '("/usr/local/libexec/cssc")
	  (if (file-directory-p "/usr/pkg/libexec/cssc")
	      '("/usr/pkg/libexec/cssc")
	    nil))))

;; colour is nice, but on a monochrome screen vc-annotate shows invisble text
;;
;; unfortunately there's no vc-annotate-hook to run to first test if the
;; current frame is colour-capable or not (eg. using `display-color-p') so this
;; is all-or-nothing....
;;
(require 'vc)
(setq vc-annotate-background nil)
(setq vc-annotate-color-map
   '(( 26.3672 . "black")
    ( 52.7344 . "black")
    ( 79.1016 . "black")
    (105.4688 . "black")
    (131.8359 . "grey3")
    (158.2031 . "grey3")
    (184.5703 . "grey3")
    (210.9375 . "grey3")
    (237.3047 . "grey3")
    (263.6719 . "grey3")
    (290.0391 . "grey3")
    (316.4063 . "grey3")
    (342.7734 . "grey3")
    (369.1406 . "grey3")
    (395.5078 . "grey3")
    (421.8750 . "grey3")
    (448.2422 . "grey3")))
(setq vc-annotate-very-old-color "grey1")

(eval-and-compile
  (defun file-in-pathlist-p (file-name path-list)
    "Returns t if the string FILENAME is a file name which occurs in a
directory in the list PATHLIST, otherwise nil."
    (let (try-path (file-found-in-path-p nil))
      (while (not (or file-found-in-path-p (null path-list)))
	(setq try-path (car path-list)
	      path-list (cdr path-list))
	(if (file-exists-p (concat try-path "/" file-name)) ; path-separator :-)
	    (setq file-found-in-path-p t)))
      (eval 'file-found-in-path-p))))

(eval-and-compile
  (defun file-in-loadpath-p (file-name)
    "Returns t if the string argument FILENAME is a file name present in a
directory in the load-path list, otherwise returns nil."
    (file-in-pathlist-p file-name load-path)))

;;; This could probably be rewritten to use mapcar
;;;
(eval-and-compile
  (defun elisp-file-in-loadpath-p (file-name)
    "Returns t if there is an emacs lisp-library of the name FILENAME in the
load-path list. Matching is first done by looking for the file with an .elc
extension, an .el extension, and finally with no extension at all, and
returning t if any of the three are found. Nil is returned otherwise."
    (let ((extension-list (list ".elc" ".el" ""))
	  (file-found-p nil)
	  name-to-try)
      (while (and (not file-found-p) (not (null extension-list)))
	(setq name-to-try (concat file-name (car extension-list)))
	(setq extension-list (cdr extension-list))
	(setq file-found-p (file-in-loadpath-p name-to-try)))
      (eval 'file-found-p))))

;;;; ----------
;;;; some default packages we'd like, if we can get them...

(if (elisp-file-in-loadpath-p "newcomment")
    (progn
      (require 'newcomment)))

(if (elisp-file-in-loadpath-p "filladapt")
    (progn
      (require 'filladapt)
      (eval-when-compile
	(defvar filladapt-mode))
      (setq-default filladapt-mode t)	; it is a "Good Thing(tm)!"
      (add-hook 'text-mode-hook 'turn-on-filladapt-mode) ; it is a "Really Good Thing(tm)!"
      (add-hook 'sh-mode-hook 'turn-on-filladapt-mode)  ; ... especially in sh-mode :-)
      (add-hook 'c-mode-hook 'turn-on-filladapt-mode))) ; ... even in c-mode :-)

(if (and (elisp-file-in-loadpath-p "func-menu")
	 window-system)
    (progn
      (require 'func-menu)
      (define-key global-map [S-down-mouse-2]
	'function-menu)))

;; This is an ancient hack by Joe Wells is really only necessary on emacs-18
;; and very early versions of emacs-19 since the appearance of the new
;; `resize-temp-buffer-window' function.
;(if (elisp-file-in-loadpath-p "shwtmpbuf")
;    (progn
;      (load "shwtmpbuf")
;      ;; FIXME: need an undo-temp-buffers to revert frame composition....
;      (global-set-key "\C-xH" 'hide-temp-buffers))) ; defaults to C-x t in shwtmpbuf

;; ksh-mode is handy if you don't have sh-script....
(if (and (elisp-file-in-loadpath-p "ksh-mode")
	 (not (elisp-file-in-loadpath-p "sh-script")))
    (autoload 'ksh-mode "ksh-mode" "Major mode for editing Korn Shell scripts." t))

;; not autoload'ed in 19.28, but it is there....
(if (elisp-file-in-loadpath-p "sh-script")
    (progn
      (autoload 'shell-script-mode "sh-script" "Major mode for editing shell
scripts (alias)." t)
      (autoload 'sh-mode "sh-script" "Major mode for editing shell scripts." t)))

;; This handy extension to `outline-mode' has been available in the default
;; distribution since 19.27....
(if (elisp-file-in-loadpath-p "foldout")
    (eval-after-load "outline" '(load "foldout")))

;;;; ----------
;;;; some property defintions...

(put 'eval-expression 'disabled nil)	; allow ESC ESC
(put 'narrow-to-region 'disabled nil)	; allow C-x n
(put 'rmail 'disabled t)		; avoid mbox destruction

;;;; ----------
;;;; handling of abbrev files...

(condition-case ()
    (read-abbrev-file nil t)
  (file-error nil))

;;;; ----------
;;;; Set defaults of other buffer-local variables

(setq-default case-fold-search nil)	; unless set, don't ignore case
(setq-default indent-tabs-mode t)	; allow tabs in indentation
(setq-default require-final-newline 1)	; needed by some unix programs
(eval-when-compile
  (if (< init-emacs-type 21)
      (defvar indicate-empty-lines)))
(setq-default indicate-empty-lines t)	; show which lines are past the EOF

;;;; ----------
;;;; some new global variable settings...

;;; Date: Wed, 2 Feb 1994 12:49:31 GMT
;;; Message-Id: <1994Feb2.124931.19715@nessie.mcc.ac.uk>
;;; Organization: Manchester Computing Centre, Manchester, England
;;; From: ehgasm2@uts.mcc.ac.uk (Simon Marshall)
;;; Subject: Re: Quick routine to BOLDFACE directories in DIRED buffers
;;;
;;; XXX probably should use \\' instead of $ to match end of string
;;;
(if window-system
    (defvar dired-font-lock-keywords
      '(("\\S +\\([~%#]\\)$" . font-lock-variable-name-face) ; font-lock-doc-string-face
	("\\S +\\.\\([oszZ]\\|elc\\|gz\\)$" . font-lock-string-face)
	("^  \\(/.+\\)$" 1 font-lock-type-face)
	("[^ ]+ -> [^ ]+$" . font-lock-function-name-face)
	("^..\\(.....w....\\|........w.\\)" 1 font-lock-comment-face)
	("^[^ ].*$" 0 font-lock-comment-face t)
	("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face))))

(defvar orig-default-frame-font
  nil
  "The original default frame font.")

(defvar preferred-frame-font
  "fixed"
  "My preferred font.")

;; The Bitstream Courier font is very clean but doesn't seem to have a matching
;; size italic (and has no oblique) font (at least not on some stock X11's).
;;
;;	"-bitstream-courier-medium-r-*-*-*-120-*-*-m-*-iso8859-1"
;;
;; This is the next best thing for a default X11 installation:
;;
;;	"-adobe-*-medium-r-normal--*-120-*-*-m-*-iso8859-1"
;;
;; It matches the default, at least as of 21.1 on a 100dpi display, which is:
;;
;;	"-adobe-courier-medium-r-normal--17-120-100-100-m-100-ISO8859-1"
;;
;; A pxlsz of '0' might force the use of Type-1 fonts (if your Xserver is
;; capable of displaying them)...
;;
;;	"-adobe-*-medium-r-normal--0-120-*-*-m-*-iso8859-1"
;;
;; ... but since they don't always have a proper back-tick, we're hosed and we
;; must stay with this slightly bigger (about 30% wider, but maybe 2% shorter)
;; and much uglier font.
;;
;; ... assuming it's X, that is!  ;-)
;;
;; Ideally you can install a custom font with all unique glyphs (and a
;; complete set of glyphs).
;;
;; Indeed I use just such a font.
;;
;;	    "-etl-fixed-medium-r-normal--16-*-*-*-c-*-iso8859-1"
;;
;; It's one of the fonts from the GNU intlfonts distribution.  These are by far
;; the very best all-round complete fonts I've ever seen for X11.
;;
(if (eq window-system 'x)
    (progn
      (setq orig-default-frame-font (frame-parameter nil 'font))
      (setq preferred-frame-font
	    "-etl-fixed-medium-r-normal--16-*-*-*-c-*-iso8859-1")))

(require 'frame)
(defun set-frame-face-to-preferred-frame-font (frame)
  "Set FRAME's faces to those for preferred-frame-font.  This is a replacement
for set-frame-font (formerly set-default-font) which can be used as an
after-make-frame-functions hook.  It is not just a wrapper but a
re-implementation so that we can catch the error from modify-frame-parameters
when our preferred font is not available."
  (condition-case nil
      (progn
	(modify-frame-parameters frame
				 (list (cons 'font
					     preferred-frame-font))))
    (error (modify-frame-parameters frame
				    (list (cons 'font
						orig-default-frame-font)))))
  (run-hooks 'after-setting-font-hook 'after-setting-font-hooks)
  ;; Update faces that want a bold or italic version of the default font.
  ;; unnecessary in 21.1 and newer.  Ignore the "no longer necessary" warning.
  ;; (XXX can't use eval-and-compile here because of local variable "frame")
  (if (fboundp 'frame-update-faces)
      (frame-update-faces frame)))
(add-hook 'after-make-frame-functions 'set-frame-face-to-preferred-frame-font)

;; this fixes up the current frame, which already exists, and already has its
;; font parameter set....
(set-frame-face-to-preferred-frame-font (selected-frame))

;; where is that damn cursor anyway?!?!?!?
(if (and window-system
	 (fboundp 'blink-cursor-mode))
    (blink-cursor-mode 1))

;; we like fancy faces!
(global-font-lock-mode t)

(setq auto-save-timeout 300)		; 30 seconds is insane!
(setq backup-by-copying nil)		; rename is safer and faster...
(setq backup-by-copying-when-linked t)	; ... but when files are linked...
(if (= (user-uid) 0)
    (setq backup-by-copying-when-mismatch t)) ; also if root and not owner so
					      ; as not to change the ownership
(setq colon-double-space t)		; ah ha!  this should mirror sentence-end-double-space!
(setq default-tab-width 8)		; a tab is a tab is a tab is a tab....
(setq delete-auto-save-files t)		; delete auto-save file when saved
(setq enable-local-variables 1)		; non-nil & non-t means query...
(setq file-name-handler-alist nil)	; turn off ange-ftp entirely
(setq make-backup-files t)		; better safe than sorry!
(setq message-log-max 1000)		; default of 50 loses too much!
(setq next-line-add-newlines nil)	; I hate it when it does that!  ;-)
(setq search-highlight 1)		; not sure when this begins to work
(setq sentence-end-double-space t)	; just to be absolutely sure!
(eval-when-compile
  (if (< init-emacs-type 21)
      (defvar tab-always-indent)))
(setq tab-always-indent nil)		; silly
(setq track-eol nil)			; too hard to control (it's sticky!)
(setq window-min-height 1)		; don't be snobbish
(setq window-min-width 1)

(setq sentence-end
      "[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*") ; also to make sure!

(require 'compile)
(setq compilation-window-height 10)	; default height for a compile window
(setq compilation-scroll-output t)	; where, oh where, has this been!!!

(require 'font-lock)
(setq font-lock-maximum-size nil)	; don't worry about the buffer size...

;; (require 'time)			; this isn't provided by time.el!
(setq display-time-day-and-date t)	; what day is it again?
(eval-when-compile
  (defvar display-time-24hr-format))	; not defvar'ed!
(setq display-time-24hr-format t)	; time in 24hour-mode
;(if (or (string-equal (system-name) "robohack")
;	(string-equal (system-name) "almost.weird.com")
;	(string-equal (system-name) "always.weird.com"))
;    (setq display-time-interval 300)) ; poor little machines....

(if (fboundp 'column-number-mode)
    (column-number-mode 1))		; XXX does this stick?  I hope so!

;; note the v20.* compiler will bitch about tool-bar-mode being undefined...
(if (elisp-file-in-loadpath-p "tool-bar")
    (progn
      (require 'tool-bar)
      (if (fboundp 'tool-bar-mode)
	  (tool-bar-mode -1))))		; major screen-space waster!

(if window-system
    (progn
      ; mouse wheel support, at least for (eq window-system 'x)
      (global-set-key [mouse-4] 'scroll-one-line-down)
      (global-set-key [mouse-5] 'scroll-one-line-up)
      (setq mouse-yank-at-point t)	; yank at click is DANGEROUS!!!!
      (setq baud-rate 153600)))		; let's make things more efficient

(if (elisp-file-in-loadpath-p "browse-url")
    (progn
      (require 'browse-url)
      (if window-system
	  (progn
	    (global-set-key [S-mouse-2] 'browse-url-at-mouse)))
      
      ;;(setq browse-url-netscape-arguments '("-install"))

      ;; Always save modified buffers before displaying the file in a browser:
      ;;
      (setq browse-url-save-file t)

      ;; Bind the browse-url commands to keys with the `C-c C-z' prefix
      ;; (as used by html-helper-mode):
      ;;
      (global-set-key "\C-c\C-z." 'browse-url-at-point)
      (global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
      (global-set-key "\C-c\C-zr" 'browse-url-of-region)
      (global-set-key "\C-c\C-zu" 'browse-url)
      (global-set-key "\C-c\C-zv" 'browse-url-of-file)
      (add-hook 'dired-mode-hook
		(lambda ()
		  (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))

      ;; To just use the Emacs w3 browser when not running under X11:
      ;;(or (eq window-system 'x)
      ;;    (setq browse-url-browser-function 'browse-url-w3))

      (setq browse-url-browser-function '(("^mailto:" . browse-url-mail)))

      (setq browse-url-new-window-flag t)	; always a good idea??? (or just for mail?)

      (eval-when-compile
	(if (elisp-file-in-loadpath-p "browse-url")
	    (require 'browse-url)))
      (defvar browse-url-links-program nil
	"A web browser program to use in an xterm window")
      (defun browse-url-links-xterm (url &optional new-window)
	;; new-window ignored
	"Ask the Links WWW browser to load URL.
Default to the URL around or before point.  A new Links process is run
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'."
	(interactive (browse-url-interactive-arg "Links URL: "))
	(apply #'start-process `(,(concat "links-" url) nil ,browse-url-xterm-program
				 ,@browse-url-xterm-args "-T" ,(concat "WWW-Browser:" url) "-e" ,browse-url-links-program
				 ,url)))

      ;; NOTE:  all custom settings for browse-url-browser-function must be done
      ;; prior to this point!
      ;;
      (if (eq window-system 'x)
	  (progn
	    (setq browse-url-browser-function
		  (append browse-url-browser-function '(("." . browse-url-links-xterm))))
	    (setq browse-url-xterm-args '("-cn" "-rw" "-sb" "-si" "-sk" "-ls" "-ziconbeep" "1" "-n" "WWW" ))
	    (setq browse-url-links-program "links"))
	(setq browse-url-browser-function
	      (append browse-url-browser-function '(("." . browse-url-lynx-emacs)))))
      )) ; (require 'browse-url) END

(if (and (string-match "-sunos4" system-configuration)
	 (or (string-match "\\`/bin/sh\\'" shell-file-name)
	     (string-match "\\`/usr/bin/sh\\'" shell-file-name)))
    (setq cannot-suspend t))		; no jobs support!  ;-)

;; Something more detailed, like this, really should be the default!
;;
;; note that the octal escapes are for iso-8859-1 encodings....
;; note also that \240, the Icelandic "eth", doesn't always appear in
;; iso-8859-1 faces
;;
(setq list-faces-sample-text
      "abcdefghijklmnopqrstuvwxyz\n\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\n\
0123456789\n\
!@\#$%^&*()_+-=\\[];'`,./|{}:\"~<>?\n\
\240\241\242\243\244\245\246\247\
\250\251\252\253\254\255\256\257\
\260\261\262\263\264\265\266\267\
\270\271\272\273\274\275\276\277\n\
\340\341\342\343\344\345\346\347\
\350\351\352\353\354\355\356\357\
\360\361\362\363\364\365\366\367\
\370\371\372\373\374\375\376\377\n\
\300\301\302\303\304\305\306\307\
\310\311\312\313\314\315\316\317\
\320\321\322\323\324\325\326\327\
\330\331\332\333\334\335\336\337")

(add-to-list 'completion-ignored-extensions '(".out"))

(setq frame-title-format
      '("" mode-line-process " %b [%f] %F@" system-name))

(setq icon-title-format frame-title-format)

(setq same-window-buffer-names
      '("*shell*"
	"*mail*"
	"*inferior-lisp*"
	"*ielm*"
	"*scheme*")) ; *info* nixed

(setq special-display-regexps
      '((".*\\*Apropos\\*.*"
	 '((top . 0)
	   (left . -1)
	   (height . 50)
	   (width . 80)
	   (tool-bar-lines . 0)
	   (menu-bar-lines . 0)))
	(".*\\*Help\\*.*"
	 '((top . 0)
	   (left . -1)
	   (height . 30)
	   (width . 80)
	   (tool-bar-lines . 0)
	   (menu-bar-lines . 0)))
	(".*\\*info\\*.*"
	 '((top . 0)
	   (left . -1)
	   (height . 50)
	   (width . 80)
	   (tool-bar-lines . 0)
	   (menu-bar-lines . 0)))
	(".*\\*scratch\\*.*"
	 '((top . 300)
	   (left . -0)
	   (height . 44)
	   (width . 90)
	   (tool-bar-lines . 0)
	   (menu-bar-lines . 0)))
;	(".*"
;	 '((top . 0)
;	   (left . -1)
;	   (height . 16)
;	   (width . 80)
;	   (unsplittable . t)		; ?????  Should they all be?
;	   (tool-bar-lines . 0)
;	   (menu-bar-lines . 0)))
	))

;; frame parameters for plain buffer names are supplied by
;; special-display-frame-alist, set below
;;
(setq special-display-buffer-names
      '("*compilation*"
	"*grep*"
	"*tex-shell*"))

;; special buffers also shouldn't have a menu-bar either....
;;
;; These apply to `special-display-buffer-names' and `special-display-regexps'
;;
;; Hmmm....
;;
;; Actually these seem to completely override the frame parameters that are
;; explicitly set in special-display-regexps above.  That must be a bug!
;;
(setq special-display-frame-alist
      '((top . 0)
	(left . -1)
	(height . 16)
	(width . 80)
	(unsplittable . t)
	(tool-bar-lines . 0)
	(menu-bar-lines . 0)))

;; Format string for PR summary text.
(eval-when-compile
  (defvar gnats::format-string))
(setq gnats::format-string
      "%5n %-14,14c %,1e%,1p %-8,8r %,6L %,4s %-4*4S %-12*-12R %j\n")

;;; From Len Tower again:
(cond
 ((eq system-type 'unix-SVR4)
  (setq dired-listing-switches "-lba"
        list-directory-verbose-switches "-lbaF"
        list-directory-brief-switches "-abCF"))
 ((or (eq system-type 'netbsd)
      (eq system-type 'berkeley-unix)
      (equal (getenv "ARCH") "symmetry"))
  (setq dired-listing-switches "-lag"
        list-directory-verbose-switches "-laFg"
        list-directory-brief-switches "-aCFg"))
 (t
  (setq dired-listing-switches "-lbag"
        list-directory-verbose-switches "-lbaFg"
        list-directory-brief-switches "-abCFg")))

(eval-when-compile
  (defvar tags-revert-without-query))	; avoid needing (require 'etags)
(setq tags-revert-without-query t)	; always revert to a newer TAGS file

;;;; ----------
;;;; auto-mode-alist setup

(defun add-to-auto-mode-alist (element)
  "Add ELEMENT to `auto-mode-alist' if it isn't there yet."
  (add-to-list 'auto-mode-alist element t)) ; add-to-list in 19.29 and newer

;; The REs in each element of `auto-mode-alist' match the visited file name so
;; you normally only want to match names at the end and not include any
;; directory portion unless you always visit the file using the directory
;; pathname explicitly.
;;
;; note: no cvs backup files listed, they match "\\.#.*\\.[.0-9]+\\'"
;;
;; note "\\'" matches the empty string, but only at the end of the buffer or
;; string being matched against.
;;
(mapcar 'add-to-auto-mode-alist
	(list
	 '("[Cc]onfig[^/\\.]*\\'" . sh-mode)	; sh-mode, in 19.28 and newer
	 '("[^/]*rc\\'" . sh-mode)
	 '("/rc\\.[^/]*\\'" . sh-mode)
	 '("/rc\\.[^/]+/[^/]*\\'" . sh-mode)	; anything in an rc.* directory
	 '("[-\\.]ash[^/\\.]*\\'" . sh-mode)
	 '("[-\\.]ksh[^/\\.]*\\'" . sh-mode)
	 '("[-\\.]sh[^/\\.]*\\'" . sh-mode)
	 '("\\.[^/]*profile" . sh-mode)
	 '("DEINSTALL\\'" . sh-mode)		; NetBSD pkgsrc
	 '("DESCR\\'" . indented-text-mode)	; NetBSD pkgsrc
	 '("INSTALL\\'" . sh-mode)		; NetBSD pkgsrc (clashes with
						;   GNU standard INSTALL doc)
	 '("MESSAGE\\'" . indented-text-mode)	; NetBSD pkgsrc
	 '("PLIST\\'" . sh-mode)			; NetBSD pkgsrc (not sh, but...)
	 '("REQUIRE\\'" . sh-mode)		; NetBSD pkgsrc
	 '("\\.java\\'" . java-mode)		; cc-mode
	 '("\\.[0-9][a-z]?\\'" . nroff-mode)	; man page
	 '("\\.[0-9][a-z]?\\.in\\'" . nroff-mode) ; man page
	 '("\\.[mM]?an\\'" . nroff-mode)	; man page
	 '("\\.mdoc\\'" . nroff-mode)		; mdoc(7) document
	 '("\\.t[imes]*\\'" . nroff-mode)	; nroff+tbl
	 '("\\.t[imes]*\\.in\\'" . nroff-mode)	; nroff+tbl
	 '("[rR][eE][lL][eE][aA][sS][eE]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[cC][hH][aA][nN][gG][eE][sS]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[iI][nN][sS][tT][aA][lL][lL]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[aA][uU][tT][hH][oO][rR][sS]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[bB][uU][gG][sS]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[cC][oO][pP][yY][^/.]*\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[nN][eE][wW][sS]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[tT][oO][dD][oO]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[tT][hH][aA][nN][kK][^/.]*\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("[rR][eE][aA][dD]\\([-.]\\)?[mM][eE]\\([-.][^/]*\\)?\\'" . indented-text-mode)
	 '("\\.te?xt\\'" . indented-text-mode)
	 '("\\.notes?\\'" . indented-text-mode)
	 '("\\.article\\'" . indented-text-mode)
	 '("\\.letter\\'" . indented-text-mode)
	 '("\\.mail[^/]*\\'" . mail-mode)
	 '("\\.wl\\'" . emacs-lisp-mode)		; WL init file
	 '("\\.vm\\'" . emacs-lisp-mode)		; VM init file
	 '("/tmp/[^/]*\\.ed[^/]*\\'" . indented-text-mode) ; mail edit buffer
	 '("/tmp/[^/]*nf[^/]*\\'" . indented-text-mode))) ; notesfile compose buf

(add-to-auto-mode-alist
 (cons (concat "\\`" (getenv "HOME") "/notes/.+\\'") 'indented-text-mode))

;; assume the autoloads are done for this...
(if (elisp-file-in-loadpath-p "diff-mode")
    (add-to-auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)" . diff-mode)))

;; assume the autoloads are done for this...
(if (or (elisp-file-in-loadpath-p "makefile")
	(elisp-file-in-loadpath-p "make-mode"))
    (mapcar 'add-to-auto-mode-alist
	    (list
	     '("[Mm]ake[^/]*\\.am\\'" . makefile-mode)
	     '("[Mm]ake[^/]*\\.inc\\'" . makefile-mode)
	     '("[Mm]ake[^/]*\\'" . makefile-mode)
	     '("[Pp]\\.[Mm]ake[^/]*\\'" . makefile-mode)
	     '("[Mm]\\.include\\'" . makefile-mode)
	     '("[^/]*mk.conf[^/]*\\'" . makefile-mode)
	     '("[^/]+\\.mk\\'" . makefile-mode)
	     '("[^/]+\\.mk\\.in\\'" . makefile-mode))))

;; assume the autoloads are done for this...
(if (elisp-file-in-loadpath-p "lout-mode")
    (add-to-auto-mode-alist '("\\.lout\\'" . lout-mode)))

;; assume the autoloads are done for this...
(if (elisp-file-in-loadpath-p "autoconf")
    (progn
      (add-to-auto-mode-alist '("/configure\\.in\\'" . autoconf-mode))
      (add-to-auto-mode-alist '("\\.ac\\'" . autoconf-mode)))
  (if (elisp-file-in-loadpath-p "m4-mode")
      (progn
	(add-to-auto-mode-alist '("/configure\\.in\\'" . m4-mode))
	(add-to-auto-mode-alist '("\\.ac\\'" . m4-mode)))))

;; assume the autoloads are done for this...
(if (elisp-file-in-loadpath-p "vm")
    (progn
      (if (and window-system
	       (boundp 'menu-bar-tools-menu))
	  (progn
	    ;; to quiet the v19 byte compiler
	    (eval-when-compile
	      (defvar menu-bar-tools-menu))
	    (define-key menu-bar-tools-menu [rmail] '("Read Mail" . vm))
	    (define-key-after menu-bar-tools-menu [smail] '("Send Mail" . vm-mail) 'rmail)))
      (mapcar 'add-to-auto-mode-alist
	      (list
	       '("/Letter\\'" . vm-mode)
	       '("mbox\\'" . vm-mode)
	       '("/Mail/.*\\'" . vm-mode)
	       '("/News/.*\\'" . vm-mode)
	       '("\\.shar[^/]*\\'" . vm-mode)))))


(if (elisp-file-in-loadpath-p "wl")
    (progn
      ;; autoload configuration
      ;; (Not required if you have installed Wanderlust as XEmacs package)
      (autoload 'wl "wl" "Wanderlust" t)
      (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
      (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

      ;; enable wl-draft mode
      (autoload 'wl-user-agent-compose "wl-draft" nil t)))

;;;; ----------
;;;; special setup!
;;
;; XXX should this stuff be done in the after-init-hook too?

(eval-and-compile
  (progn
    ;; Set the PATH environment variable from the exec-path so that child
    ;; processes will inherit anything emacs uses.
    (setenv "PATH"
	    (mapconcat
	     '(lambda (string) string)
	     exec-path
	     ":"))
    ;; So that subprocesses will use emacs for editing.
    (setenv "EDITOR" "emacsclient")
    (setenv "VISUAL" "emacsclient")))

;;;; ----------
;;;; some useful functions....

(defun show-text-prop ()
  (interactive)
  (princ (text-properties-at (point))))
(global-set-key "\C-xP" 'show-text-prop)

;;; I prefer if already capitalized characters stay that way....
;;;
(defun upcase-initials-word (arg)
  "Capitalize the character after the point (and the initial character on the
next ARG-1 words if ARG is greater than 1), moving over."
  (interactive "*p")
  (let (lastpoint)
    (save-excursion
      (mark-word arg)
      (setq lastpoint (mark))
      (upcase-initials-region (point) lastpoint))
    (goto-char lastpoint)))
(global-set-key "\ec" 'upcase-initials-word)
(global-set-key "\eC" 'capitalize-word)

(defun get-buffer-file-name ()
  "Echo the current buffer's file name and put it into the kill ring"
  (interactive)
  (let ((bn (buffer-file-name nil)))
   (kill-new bn)
   (message "%s" bn)))

(global-set-key "\M-?-B" 'get-buffer-file-name)

;; Message-ID: <14865.20524.263889.867670@vegemite.chem.nottingham.ac.uk>
;; Date: Tue, 14 Nov 2000 14:46:04 +0000
;; From: Matt Hodges <pczmph@unix.ccc.nottingham.ac.uk>
;; To: GNU Emacs bugs <bug-gnu-emacs@gnu.org>
;; Subject: Killing a named process.
;;
;; [I posted this to gnu.emacs.bug but it seems to have got lost]
;;
;; Feature request: occasionally I want to be able to kill a process (as
;; seen in the *Process List* buffer) and no convenience command (eg
;; ispell-kill-ispell) exists.
;;
;; Did I miss a way of easily doing this? If not, the following can be
;; used.
;;
(defun kill-named-process ()
  "Kill a process chosen from a list.
The list is built from the function `process-list'."
  (interactive)
  (if (equal (length (process-list)) 0)
      (error "No processes exist"))
  (let* ((process-list
          (mapcar
           (function (lambda (w)
                       (list (process-name w) w)))
           (process-list)))
         (process-name
          (completing-read
           "Choose process to kill: " process-list nil t))
         process)
    (cond
     ((> (length process-name) 0)
      (setq process (cadr (assoc process-name process-list)))
      (kill-process process)
      ;; Update the *Process List* buffer
      (if (buffer-live-p (get-buffer "*Process List*"))
          (progn
            ;; Wait until process disappears off list
            (while (member process (process-list))
              (sit-for 0 100))
            (save-window-excursion
              (list-processes))))
      (message "Process \"%s\" killed" process-name))
     (t
      (message "No process chosen")))))

;; Message-ID: <1996Nov14.092737.1@psiclu.psi.ch>
;; From: badii@cvax.psi.ch
;; Organization: Paul Scherrer Institute
;; Reply-To: badii@psi.ch
;; To: gnu-emacs-sources@prep.ai.mit.edu
;; Date: 14 Nov 96 09:27:37 +0200
;; Subject: Improved, fast page-up page-down
;;
;; Dear (X)emacs users,
;;
;; The three functions below (a basic one and two which call it to perform
;; different actions) allow page-down, page-up movement in such a way that the
;; original line is always recovered when inverting the action and the cursor
;; remains all the time at the same line on the screen (window): that is, no
;; automatic recentering is performed.
;;
;; In my opinion, recentering is annoying in a few occasions. For example, when
;; the current line is close to the bottom of the window and page-down is
;; pressed, because of the recentering, the lines which were visible at the
;; bottom of the window are scrolled up outside of the user's view.  The
;; default mapping of the pg-up, pg-dn keys with scroll-down scroll-up, on the
;; other hand, has the problem of the noninvertibility of the action mentioned
;; above (i.e., after a pg-dn, pg-up, the current line may be changed).
;;
;; Those who are interested in trying this simple variant should assign the
;; keys pg-dn, pg-up in .emacs to the functions rb-page-down, rb-page-up. The
;; code is for (X)emacs-19.14: slight modifications for Emacs are necessary.
;;
;; Remo Badii
;; Paul Scherrer Institute
;; Nonlinear dynamics and
;; stochastic processes
;; CH-5232 Villigen
;; Switzerland
;; badii @ psi.ch
;;
; (defvar rb-up nil
;   "Set true by rb-page-up and false by rb-page-down.")
;
; (defun rb-page-move ()
;   "Called by rb-page-down (up).  Moves the current line down (up)
; window-displayed-height lines, depending on whether rb-up is nil or t.
; The inverse operation brings back to the previous line.
; No recentering takes place, except close to the end of the buffer,
; so that the cursor remains on the same displayed line on the screen."
;   (interactive "_")
;   (let ((wdh (window-displayed-height))  ; Variables: window-start, current
; 	  ws curr sh trgt)                 ; line, shift, target line
;     (if rb-up (setq wdh (- wdh)))
;     (setq curr (count-lines 1 (point)))  ; Get current line
;     (if (bolp) (setq curr (1+ curr)))    ; Correct for beg-of-line
;     (setq ws (window-start))
;     (save-excursion
; 	(goto-char ws)                     ; Go to beg of window
; 	(setq ws (count-lines 1 (point)))  ; and evaluate line number
; 	(if (bolp) (setq ws (1+ ws)))      ; Correct for beg-of-line
; 	)
;     (setq sh (- curr ws))                  ; Compute the shift
;     (save-excursion
; 	(forward-line (- wdh sh))          ; Compute target point
; 	(beginning-of-line)
; 	(setq trgt (point))
; 	)
;     (forward-line wdh)                        ; Move and
;     (set-window-start (selected-window) trgt) ; reposition.
;     (if (> (point) (- (point-max) 20))        ; If close to end
; 	  (progn                              ; of buffer, recenter.
; 	    (setq wdh (abs wdh))              ; Take abs(wdh)
; 	    (recenter (/ wdh 2))))))
;
; (defun rb-page-up ()
;   "Moves the text up window-displayed-height lines."
;   (interactive "_")
;   (setq rb-up t)
;   (rb-page-move))
;
; (defun rb-page-down ()
;   "Moves the text up window-displayed-height lines."
;   (interactive "_")
;   (setq rb-up nil)
;   (rb-page-move))

;;Message-Id: <199704040329.AA197244558@martigny.ai.mit.edu>
;;In-Reply-To: <199704040014.QAA17341@june.cs.washington.edu>
;;	(dyaitskov@insystems.com)
;;From: Bill Dubuque <wgd@martigny.ai.mit.edu>
;;To: dyaitskov@insystems.com
;;Cc: ntemacs-users@cs.washington.edu, help-gnu-emacs@prep.ai.mit.edu,
;;        bug-gnu-emacs@prep.ai.mit.edu, rms@gnu.ai.mit.edu,
;;        wgd@martigny.ai.mit.edu
;;Date: Thu, 3 Apr 1997 22:29:16 -0500
;;Subject: Re: undo question
;;
;;Try something like the following for a 'redo' command.
;;
;;WARNING: just like 'undo', this will only work if you bind it
;;to a keychord -- "M-x redo" won't do the trick. The problem
;;is that these commands test 'last-command' to determine their
;;state, but when you invoke them via "M-x...", 'last-command'
;;is clobbered by the commands you execute in the minibuffer
;;after typing in "M-x..." (so 'last-command' will end up being
;;'self-insert-command'). This is a bug/misfeature in Emacs.
;;
;;RMS: Perhaps minibuffer invocation should preserve 'last-command'.
;;
(defun redo (&optional arg)
  "Redo some previous changes.
Repeat this command to redo more changes.
Cannot be invoked from the minibuffer -- must be bound.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (cond ((eq last-command 'redo)
         (let ((last-command 'undo))
	   (undo arg)))
        ((eq last-command 'undo)
	 (let ((last-command 'nosuch))
	   (undo arg)))
        (t (error "Can't redo: last command was not undo but %s"
                  last-command)))
  (setq this-command 'redo))
(global-set-key "\C-xc" 'redo)

;;Message-Id: <9507291206.AA01499@owl.hq.ileaf.com>
;;From: karl@owl.hq.ileaf.com (Karl Berry)
;;Subject: what-line enhancement
;;Date: Sat, 29 Jul 95 08:06:35 EDT
;;
;;How about having what-line print the total number of lines in the
;;buffer, as well as the current line?
;;
;; merged with enhancements by Michael D. Prange to print line in a narrowed
;; region too.
;;
(defun what-line ()
  "Print the current line number of point, and total lines in buffer (and
similarly the line in the narrowed region and the number of lines in the
region, if there is one).

Note that a buffer always has one more line in it than the file has."
  (interactive)
  (save-restriction
    (save-excursion
      (beginning-of-line)
      (let* ((narrowed-point-min (point-min))
	     (narrowed-lines-after-point (count-lines (point) (point-max)))
	     (lines-after-narrowed-point-min (count-lines narrowed-point-min (point))))
	(widen)
	(if (= narrowed-point-min (point-min)) ;test for narrowing
	    (message "Line %d of %d"
		     (1+ lines-after-narrowed-point-min)
		     (+ 1 lines-after-narrowed-point-min (count-lines (point) (point-max))))
	  (let* ((lines-after-point (count-lines (point) (point-max)))
		 (lines-before-region (progn (goto-char narrowed-point-min)
					     (beginning-of-line)
					     (count-lines 1 (point)))))
	    (message "Line %d of %d in buffer.  Line %d of %d in narrowed region."
		     (+ 1 lines-after-narrowed-point-min lines-before-region)
		     (+ 1 lines-after-narrowed-point-min lines-before-region lines-after-point)
		     (+ 1 lines-after-narrowed-point-min)
		     (+ 1 lines-after-narrowed-point-min narrowed-lines-after-point))))))))
(global-set-key "\C-xl" 'what-line)
(global-set-key "\C-xL" 'count-lines-page)

;;From: friedman@gnu.ai.mit.edu (Noah Friedman)
;;Message-Id: <9502130229.AA10679@tepui.cli.com>
;;Subject: nuke-trailing-whitespace
;;Date: Sun, 12 Feb 95 20:29:02 CST
;;
(defvar nuke-trailing-whitespace-p 'ask
  "If `nil', the function `nuke-trailing-whitespace' is disabled.
If `t', `nuke-trailing-whitespace' unreservedly strips trailing whitespace
from the current buffer.  If not `nil' and not `t', a query is made for each
instance of trailing whitespace.")
;;
;;(add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;;
(defun nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

If the variable `nuke-trailing-whitespace-p' is `nil', this function is
disabled.  If `t', unreservedly strip trailing whitespace.
If not `nil' and not `t', query for each instance."
  (interactive)
  (and nuke-trailing-whitespace-p
       (save-match-data
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (cond ((eq nuke-trailing-whitespace-p t)
                    (while (re-search-forward "[ \t]+$" (point-max) t)
                      (delete-region (match-beginning 0) (match-end 0))))
                   (t
                    (query-replace-regexp "[ \t]+$" "")))))))
  ;; always return nil, in case this is on write-file-hooks.
  nil)

(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun line-to-top-of-window ()
  "Scroll the selected window up so that the current line is at the top."
  (interactive)
  (recenter 0))

(defun top-of-window ()
  "Move the point to the top line in the current window."
  (interactive)
  (move-to-window-line 0))

(defun bottom-of-window ()
  "Move the point to the bottom line in the current window."
  (interactive)
  (move-to-window-line -1))

(defun forward-screen ()
  "Move point down by the height of the window"
  (interactive)
  (next-line (/ (* (window-height) 3) 5)))

(defun backward-screen ()
  "Move point down by the height of the window"
  (interactive)
  (previous-line (/ (* (window-height) 3) 5)))

;; cycle through buffers, ignoring uninteresting ones
(defun z-backward-buffer () (interactive)
  "Switch to previously selected buffer."
  (let* ((list (cdr (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (bury-buffer)
    (switch-to-buffer buffer)))

(defun z-forward-buffer () (interactive)
  "Opposite of backward-buffer."
  (let* ((list (reverse (buffer-list)))
	 (buffer (car list)))
    (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
      (progn
	(setq list (cdr list))
	(setq buffer (car list))))
    (switch-to-buffer buffer)))

;;; like not-modified, but silent, and can toggle
;;;
(defun toggle-buffer-modified ()
  "Toggle the buffer-modified status."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

;;; Snarfed from Steve Humble
(defun ascii-table (new)
  "Show the buffer *Ascii Table* or make one.
Make a new one if NEW (or prefix arg) is non-nil."
  (interactive "P")
  (let ((buf "*Ascii Table*")
        (c 0)
        (header "char  8  10  16%s")
        c64 c128 c192)
    (if (and (not new) (get-buffer buf))
        (display-buffer buf)
      (with-output-to-temp-buffer buf
        (message "Building ascii table...")
        ;;Silly spacing, but it looks better that way.
        (princ (format header "    "))
        (princ (format header "       "))
        (princ (format header "      "))
        (princ (format header "\n"))
        (while (< c 64)
          (setq c64 (+ c 64)
                c128 (+ c 128)
                c192 (+ c 192))
          (princ (format
                  ;;Yes, it's possible to take out the remaining
                  ;;spaces in the format string by changing the field
                  ;;widths, but that would make it even more illegible.
                  "%3s%4o%4d%4x %6s%4o%4d%4x %9s%4o%4d%4x %8s%4o%4d%4x\n"
                  (key-description (char-to-string c)) c c c
                  (key-description (char-to-string c64)) c64 c64 c64
                  (key-description (char-to-string c128)) c128 c128 c128
                  (key-description (char-to-string c192)) c192 c192 c192))
          (setq c (1+ c)))
        (message "Building ascii table...done.")))))

(defun date (&optional insert)
  "Display today's date and the current time in the echo area.
If the optional argument INSERT (prefix-arg, in interactive) is given and its
value is non-nil, then insert at point today's date and the current time,
advancing point."
  (interactive "P")
  (if insert
      (progn
        (barf-if-buffer-read-only)
        (insert (current-time-string)))
    (message (current-time-string))))

(defun insert-date-in-current-buffer ()
  "Insert the date (as from date(1)) in current buffer at point."
  (interactive)
  (insert (format-time-string "%a %b %e %T %Z %Y")))

(defconst rfc822-month-alist
  '(("jan" "January" "1")
    ("feb" "February" "2")
    ("mar" "March" "3")
    ("apr" "April" "4")
    ("may" "May" "5")
    ("jun" "June" "6")
    ("jul" "July" "7")
    ("aug" "August" "8")
    ("sep" "September" "9")
    ("oct" "October" "10")
    ("nov" "November" "11")
    ("dec" "December" "12"))
  "Valid month names for RFC 822 format dates.")
(defconst rfc822-weekday-alist
  '(("sun" "Sunday" "0")
    ("mon" "Monday" "1")
    ("tue" "Tuesday" "2")
    ("wed" "Wednesday" "3")
    ("thu" "Thursday" "4")
    ("fri" "Friday" "5")
    ("sat" "Saturday" "6"))
  "Valid weekday names for RFC 822 format dates.")

(defun insert-rfc822-date-in-current-buffer ()
  ""
  (interactive)
  (let* ((timezone (car (current-time-zone)))
	 (hour (/ timezone 3600))
	 (min (/ (- timezone (* hour 3600)) 60))
	 (time (current-time))
	 (resent nil))
    (insert (capitalize
	     (car (nth (string-to-number (format-time-string "%w" time))
		       rfc822-weekday-alist)))
	    ", "
	    ;; %e generated " 2".  Go from string to int
	    ;; to string to get rid of the blank.
	    (int-to-string
	     (string-to-number
	      (format-time-string "%e" time)))
	    " "
	    (capitalize
	     (car (nth
		   (1- (string-to-number (format-time-string "%m" time)))
		   rfc822-month-alist)))
	    (format-time-string " %Y %H:%M:%S" time)
	    (format " %s%02d%02d"
		    (if (< timezone 0) "-" "+")
		    (abs hour)
		    (abs min))
	    "\n")))

(defun shell-command-to-buffer (process-name buffer-name &rest args)
  "Runs a command string PROCESS-NAME and puts it in buffer
BUFFER-NAME.  Optional command args to process supplied by ARGS"
  (set-buffer (get-buffer-create buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (apply 'call-process process-name nil t nil args)
  (setq buffer-read-only t)
  (goto-char (point-min))
  (pop-to-buffer (current-buffer)))

;;; borrowed from <johnw@gnu.org>
;(defun edit-variable (variable)
;  "Edit the value of VARIABLE."
;  (interactive (list (completing-read "Edit variable: " obarray 'boundp)))
;  (let* ((symbol (intern variable))
;         (value (symbol-value symbol))
;         (buffer (current-buffer)))
;    (with-current-buffer (get-buffer-create (format "*var %s*" variable))
;      (erase-buffer)
;      (emacs-lisp-mode)
;;; XXX these variables need to be pulled out and defvar'ed
;      (setq edit-variable-buffer buffer
;            edit-variable-symbol symbol
;            edit-variable-windows (current-window-configuration))
;      (insert (pp-to-string value))
;      (goto-char (point-min))
;      (select-window (display-buffer (current-buffer)))
;;; XXX this function needs to be pulled out and made top-level in scope
;      (define-key (current-local-map) [(control ?c) (control ?c)]
;        (function
;         (lambda ()
;           (interactive)
;           (goto-char (point-min))
;           (let ((symbol edit-variable-symbol)
;                 (value (read (current-buffer))))
;             (with-current-buffer edit-variable-buffer
;               (set symbol value)))
;           (set-window-configuration edit-variable-windows)))))))

;;; More stuff stolen from Roland.
;(defun make-interactive (symbol &rest interactive-args)
;  "Make the function definition of SYMBOL an interactive command.
;Remaining arguments, if any, are passed to interactive in the function."
;  (let ((func (symbol-function symbol))
;	interactive)
;    (if (commandp func)
;	(let ((msg (format "%s is already interactively callable." symbol)))
;	  (or (null interactive-args)
;	      (y-or-n-p (concat msg "  Continue? "))
;	      (error msg))))
;    (setq interactive (cons 'interactive interactive-args))
;    (if (subrp func)
;	(setq func
;	      (list 'lambda '(&rest args) (documentation func) interactive
;		    (cons 'eval
;			  (list
;;;; the 19.34 bytecompiler complains that this cons has one argument:
;			   (cons '` (list (list 'funcall func '(,@ args))))
;			   ))))
;      (let ((funcar (car func))
;	    (args (car (cdr func)))
;	    doc body)
;	(setq doc (car (cdr (cdr func))))
;	(if (stringp doc)
;	    (setq body (cdr (cdr (cdr func))))
;	  (setq doc nil
;		body (cdr (cdr func))))
;	(setq func
;	      (cons funcar
;		    (if doc (cons args (cons doc (cons interactive body)))
;		      (cons args (cons args (cons interactive body))))))
;	))
;    (fset symbol func)))

;; These ones I dreampt up myself!
;;
;; (I would also like to have a similar function that deletes all the trailing
;; whitespace from every line in a buffer.  See rm-tspaces.el by Paul D. Smith.)
;;
(defun buffer-trim-trailing-whitespace (&optional buff-nm)
  "Trim the trailing whitespace from the end of a buffer.
If BUFF-NM is not given, use the current buffer."
  (interactive)
  (if (not (bufferp buff-nm))
      (setq buff-nm (read-buffer "Buffer to trim: "
				 (buffer-name (current-buffer)))))
  (buffer-trim-trailing-chars buff-nm))

(defun buffer-trim-trailing-chars (buff-nm &optional chars-to-trim)
  "Trim trailing characters from the end of a buffer.
If CHARS-TO-TRIM is not given, default to ``\ \t\n'' (i.e. whitespace)."
  (save-window-excursion
    (set-buffer buff-nm)
    (goto-char (point-max))
    (let ((end-pt (point)))
      (skip-chars-backward (or chars-to-trim
			       "\ \t\n"))
      (delete-region (point) end-pt))))

;;; From: terra@diku.dk (Morten Welinder)
;;; To: gnu-emacs-sources@prep.ai.mit.edu
;;; Subject: Making TAB scroll completions
;;; Date: Sat, 12 Mar 1994 12:43:48 GMT
;;;
;;; Make multiple TABs scroll completions
(defun minibuf-tab ()
  "Like `minibuffer-complete', but if you use this repeatedly it will scroll
the window showing completions."
  (interactive)
  (or (eq last-command this-command) (setq minibuffer-scroll-window nil))
  (if minibuffer-scroll-window
      (save-excursion
	(set-buffer (window-buffer minibuffer-scroll-window))
	(if (pos-visible-in-window-p (point-max) minibuffer-scroll-window)
	    (set-window-start minibuffer-scroll-window (point-min))
	  (scroll-other-window)))
    (minibuffer-complete)))

(define-key minibuffer-local-must-match-map "\t" 'minibuf-tab)
(define-key minibuffer-local-completion-map "\t" 'minibuf-tab)

;;; From: jimb@totoro.bio.indiana.edu (Jim Blandy)
;;; To: gnu-emacs-sources@prep.ai.mit.edu
;;; Subject: Finding function sources
;;; Date: Fri, 18 Mar 94 12:49:11 -0500
;;;
;;; These two functions show the name of the file from which a given Emacs
;;; lisp function was loaded.  They use the `load-history' and `load-path'
;;; variables to figure out where the function definition at hand came
;;; from.
;;;
;;; find-load-file.el --- Which file did a given function come from?
;;;
;;; Author: Jim Blandy <jimb@gnu.ai.mit.edu>
;;; Created: 18 Mar 1994
;;; Keywords: lisp
;;;
(defun load-file-defining-function (function)
  "Return the name of the source file from which FUNCTION was loaded.
If FUNCTION was defined by reading from a buffer, return 'buffer.
If FUNCTION is a subr, or a lisp function dumped with Emacs, return nil."
  (interactive
   (let ((fn (function-called-at-point)) ; note, this requires help.el to have
					 ; been loaded!
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Find function (default %s): " fn)
				  "Find function: ")
				obarray 'fboundp t))
     (list (if (equal val "")
	       fn
	     (intern val)))))
  (symbol-function function)		; XXX compiler says just called for effect!
  (let ((hist load-history)
        (file nil))
    (while (consp hist)
      (let ((name (car (car hist)))
            (functions (cdr (car hist))))
        (if (memq function functions)
            (setq file (if name name 'buffer)
                  hist nil)
          (setq hist (cdr hist)))))
    (if file
	(setq file (load-file-name file)))
    (if (interactive-p)
        (message
         (cond
          ((stringp file)
           (format "Function %s loaded from \"%s\"." function file))
          ((null file)
           (format "Function %s loaded from a buffer without a file."
                   function))
          (t
           (format "Function %s loaded when Emacs was built."
                   function)))))
    file))

(define-key help-map "F" 'load-file-defining-function)

(defun load-file-name (filename &optional nosuffix)
  "Expand FILENAME, searching in the directories listed in `load-path'.
This returns the name of the file `load-library' and `load' would
process if passed FILENAME as the name of the file to load.

If optional arg NOSUFFIX is non-nil, don't try adding
suffixes `.elc' or `.el' to the specified name FILE."
  (if (file-name-absolute-p filename)
      filename
    (let ((path load-path)
          pathified-name
          expanded
          extended)
      (while (and (null pathified-name)
                  (consp path))
        (setq expanded (expand-file-name filename (car path)))
        (if (not (file-name-absolute-p expanded))
            (setq expanded (expand-file-name filename)))
        (if (file-name-absolute-p expanded)
            (setq pathified-name
                  (cond
                   ((and (not nosuffix)
                         (file-readable-p
                          (setq extended (concat expanded ".elc")))
                         (not (file-directory-p extended)))
                    extended)
                   ((and (not nosuffix)
                         (file-readable-p
                          (setq extended (concat expanded ".el")))
                         (not (file-directory-p extended)))
                    extended)
                   ((and (file-readable-p expanded)
                         (not (file-directory-p expanded)))
                    expanded))))
        (setq path (cdr path)))
      (if pathified-name
          pathified-name
          (signal 'file-error (list "Cannot find load file"
                                    filename))))))

;;; Message-Id: <m0tGNOF-0003dDC@fly.CNUCE.CNR.IT>
;;; Organization: CNUCE-CNR, Via S.Maria 36, Pisa - Italy +39-50-593211
;;; From: Francesco Potorti` <pot@cnuce.cnr.it>
;;; Subject: negative argument for indent-for-comment
;;; Date: Fri, 17 Nov 1995 10:47 +0100 (MET)
;;;
;;; indent-for-comment ignores a negative prefix argument, and
;;; kill-comment is not bound to anything by default, so it seems natural
;;; to call kill-comment with M-- M-;.
;;
;;; I have been using this in my .emacs for a long time.  Why not make it
;;; part of the distribution?
;;
;; Redefine indent-for-comment to kill the comment with negative
;; prefix
(require 'advice)
(defadvice indent-for-comment (around kill-comment activate)
  "Kill the comment with negative prefix."
  (if (eq current-prefix-arg '-)
      (kill-comment nil)
    ad-do-it))

(defun override-default-variable-settings ()
  "User defined function.  Intended to be called within various hooks to
override the value of buffer-local variables whose default values
might have been overridden by the major mode."
  (setq case-fold-search t		; allow case-insensitive searches
        indent-tabs-mode t		; allow tabs in indentation
        selective-display nil))		; don't allow selective display

(defun override-local-key-settings ()
  "User defined function.  Intended to be called within various hooks to
override the value of buffer-local key map settings which may have been
overridden without consideration by the major mode."
  (local-set-key "\C-?" 'delete-char)	; many modes
  (local-set-key "\C-h" 'delete-backward-char)	; sh-mode
  ;; the rest are *not* overridden by cc-mode, but are by c-mode
  (local-set-key "\e\C-h" 'backward-kill-word) ; text-mode
  (local-set-key "\e?" 'help-command)	; nroff-mode
  (local-set-key "\eh" 'mark-c-function)
  (local-set-key "\e\C-?" 'kill-word)
  (local-set-key "\e\C-e" 'compile)
  ;; try this on for size...
  (local-set-key "\C-x\e\C-e" 'recompile)
  )

;; 1999-07-12 Noah Friedman <friedman@splode.com>
;; Public domain
;;
(defun make-buffer-file-executable-if-script-p ()
  "If a file looks like it is an executable script, and not a source file (has
no `.'-separated extension), then make it executable with
`make-buffer-file-executable'."
  (if (and (save-excursion
	     (save-restriction
	       (widen)
	       (goto-char (point-min))
	       (save-match-data
		 (looking-at "^#!"))))
	   (string-match "/[^/.]+\\'" (buffer-file-name)))
      (make-buffer-file-executable)))

(defun make-buffer-file-executable ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (let* ((current-mode (file-modes (buffer-file-name)))
	 (add-mode (logand ?\111 (default-file-modes))))
    (or (/= (logand ?\111 current-mode) 0)
	(zerop add-mode)
	(set-file-modes (buffer-file-name)
			(logior current-mode add-mode)))))

;; these are probably always a good idea....
;;
(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)
(add-hook 'vc-checkout-hook 'make-buffer-file-executable-if-script-p)
(add-hook 'vc-checkin-hook 'make-buffer-file-executable-if-script-p)

;; himark -- by Ehud Karni <ehud@unix.simonwiesel.co.il>
;;
;; This code is public domain.
;;
(defvar himark-overlay-list nil
  "list of high-mark overlays (himark-unset deletes them).")
(defvar himark-overlay-face 'highlight
  "Name of face (quoted symbol) to use for himark.
e.g. (setq himark-overlay-face 'modeline)
Use `list-faces-display' to see all available faces")

(defun himark-unset ()
  "Remove himark overlay"
  (interactive)
       (while himark-overlay-list
           (delete-overlay (car himark-overlay-list))
           (setq himark-overlay-list (cdr himark-overlay-list))))

(defun himark-set (regexp)
  "Highlight all occurrence of REGEXP in this buffer"
  (interactive "sEnter regexp to himark: ")
  (let (oc-src ov
	       (pos (point)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq ov (make-overlay (match-beginning 0)
			     (match-end 0)))
      (overlay-put ov 'face himark-overlay-face)
      (overlay-put ov 'priority 98)
      (add-to-list 'himark-overlay-list ov))
    (goto-char pos)))

;; end of himark stuff

(global-set-key (kbd "C-c d") 'delete-line)

;; for some reason Emacs lacks delete-line, implementing it with the source
;; from kill-line is, however, trivial
;;
;; from:
;; <URL:http://lists.gnu.org/archive/html/help-gnu-emacs/2004-09/msg00178.html>
;;
(defun delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru
newline. With prefix argument, delete that many lines from point.  Negative
arguments delete lines backward.

When calling from a program, nil means \"no arg\", a number counts as a prefix
arg.

 To delete a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[delete-line] \\[delete-line].

If `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line with no
argument.  As a consequence, you can always delete a whole line by typing
\\[beginning-of-line] \\[delete-line]."
   (interactive "P")
   (delete-region (point)
                ;; It is better to move point to the other end of the delete
                ;; before deleting. That way, in a read-only buffer, point
                ;; moves across the text that is to be delete. The choice has
                ;; no effect on undo now that undo records the value of point
                ;; from before the command was run.
                (progn
                  (if arg
                      (forward-visible-line (prefix-numeric-value arg))
                    (if (eobp)
                        (signal 'end-of-buffer nil))
                    (if (or (looking-at "[ \t]*$") (and kill-whole-line
							(bolp)))
			(forward-visible-line 1)
                      (end-of-visible-line)))
                  (point))))

;;;; ----------
;;;; some special hooks.....

(if (and (<= (safe-length command-line-args) 1)
	 (or (string-equal (getenv "EDITOR") "emacsclient")
	     (string-equal (getenv "VISUAL") "emacsclient")))
    (progn
      ;; to quiet the v19 byte compiler
      (eval-when-compile
	(defvar server-process)
	(defvar server-temp-file-regexp))
      (eval-and-compile
	(autoload 'server-buffer-done "server"
	  "Mark BUFFER as \"done\" for its client(s)."
	  nil nil))
      (require 'server)
      (setq server-temp-file-regexp
	    "/tmp/Re\\|/draft$\\|/\\.letter$\\|/\\.article$/\\|/tmp/[^/]*\\.ed\\|/tmp/[^/]*nf")

;;;      ;; From: qhslali@aom.ericsson.se (Lars Lindberg EHS/PBE 80455 2122 { tom
;;;      ;;	-> 940531  ansv. EHS/PBE Christer Nilsson })
;;;      ;; Message-Id: <9402170914.AA18291@aom.ericsson.se>
;;;      ;; Subject: [19.22] emacsclient server should have a hook for kill-buffer
;;; This doesn't seem to work right -- it causes an infinite loop....
;;;      (add-hook 'server-visit-hook
;;;		(function
;;;		 (lambda ()
;;;		   (add-hook 'kill-buffer-hook
;;;			     (function
;;;			      (lambda ()
;;; perhaps this should be wrapped with something that returns nil....
;;;				(server-buffer-done
;;;				 (current-buffer))))))))

      (if (not (boundp 'server-started))
	  (progn
	    (defvar server-started 1 "If defined we've called server-start.")
	    (server-start)))))

;;;; ----------
;;;; some major-mode hooks...

;;; gdb (aka gud -- Grand Unified Debugger mode) wants to use ^X^A as a key-map
;;; prefix, but since we do that in here, it just doesn't work!
(eval-when-compile
  (defvar gud-key-prefix))
(setq gud-key-prefix "\C-x\C-g")	; this makes more sense anyway....

;;; Date: Wed, 2 Feb 1994 12:49:31 GMT
;;; Message-Id: <1994Feb2.124931.19715@nessie.mcc.ac.uk>
;;; Organization: Manchester Computing Centre, Manchester, England
;;; From: ehgasm2@uts.mcc.ac.uk (Simon Marshall)
;;; Subject: Re: Quick routine to BOLDFACE directories in DIRED buffers
;;;
(progn
  ;; to quiet the v19 byte compiler
  (add-hook 'dired-mode-hook
	    (function
	     (lambda ()
	       (font-lock-mode t)	; not needed with `global-font-lock-mode'?
	       (eval-when-compile
		 (defvar dired-font-lock-keywords))
	       (setq font-lock-keywords
		     dired-font-lock-keywords)))))

(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda ()
	     "Private emacs-lisp-mode-hook."
	     (override-local-key-settings)
	     (override-default-variable-settings)
	     (local-set-key "\eJ" 'indent-sexp)
	     (local-set-key "\ej" 'lisp-fill-paragraph))))

(add-hook 'lisp-interaction-mode-hook
	  (function
	   (lambda ()
	     "Private lisp-interaction-mode-hook."
	     (setq mode-name "LispInteraction")
	     (override-local-key-settings)
	     (override-default-variable-settings))))

(add-hook 'makefile-mode-hook		; mostly to get `compile' binding
	  (function
	   (lambda ()
	     "Private makefile-mode-hook."
	     (override-local-key-settings)
	     (override-default-variable-settings))))

;; note that once upon a time this hook variable was called `pcl-cvs-load-hook'
;;
(add-hook 'cvs-mode-hook
	  (function
	   (lambda ()
	     "Private cvs-mode hook to get rid of that horrid `z' key binding!"
	     (eval-when-compile
	       (defvar cvs-mode-map))
	     (define-key cvs-mode-map "z" 'nil))))

;;; GNU-Emacs' ideas about formatting C code really suck!  Let's stick to doing
;;; things the good old standard K&R way!!!!
;;;
;;; For reference my .indent.pro (for BSD indent) says:
;;;
;;;  -bad -bap -bc -br -nbs -c49 -cd33 -ncdb -ce -cli0 -d0 -di16 -ndj -ei -neei
;;;  -nfc1 -i8 -ip -l256 -lp -npcs -psl -sc -nsob -Tptrdiff_t -Tsize_t
;;;  -Tssize_t -Toff_t -Ttime_t -Tclock_t -Tsocklen_t -Tbool_t -Tenum_t
;;;  -Tu_char -Tu_short -Tu_int -Tu_long -Tuchar -Tushort -Tuint -Tulong
;;;  -Tunchar -Tquad_t -Tu_quad_t -Tqaddr_t -Tlonglong_t -Tu_longlong_t
;;;  -Tregoff_t -Twchar_t -Tint8_t -Tuint8_t -Tu_int8_t -Tint16_t -Tuint16_t
;;;  -Tu_int16_t -Tint32_t -Tuint32_t -Tu_int32_t -Tint64_t -Tuint64_t
;;;  -Tu_int64_t -Tintptr_t -Tuintptr_t -Tva_list -Tmode_t -Toff_t -Tpid_t
;;;  -Tuid_t -Tgid_t -Trlim_t
;;;
;;; NOTE: Someday I _think_ I want a simple flag I can toggle in a file's local
;;; variables to turn off use of tab characters and do all indentation and
;;; alignment with spaces only.

(require 'cc-mode)

(setq c-font-lock-extra-types
      '("FILE"
	"fd_set"
	"jmp_buf"
	"va_list"
	"\\sw+_t"
	"t_\\sw+"
	"u_\\sw+"
	"uchar"
	"uint"
	"ulong"
	"unchar"
	"ushort"))

(defconst my-c-style
  '((c-backslash-column . 78)
    (c-basic-offset . 8)
    (c-block-comment-prefix . "* ")
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       scope-operator)) ; (scope-operator)
    (c-comment-continuation-stars . "* ")
    (c-comment-only-line-offset . (0 . 0))
    ;; ACTION can be either a function symbol or a list containing any
    ;; combination of the symbols `before' or `after'.  If the list is empty,
    ;; no newlines are inserted either before or after the brace.
    (c-hanging-braces-alist . ((block-open . (after))
			       (block-close . (before))
			       (brace-list-open . nil)
			       (brace-list-close . nil)
			       (brace-list-intro . nil)
			       (brace-list-entry . nil)
			       (class-open . (after))
			       (class-close . nil)
			       (defun-open . (before after))
			       (defun-close . (before))
			       (inline-open . nil)
			       (inline-close . nil)
			       (statement-case-open . nil)
			       (substatement-open . nil)))
    (c-hanging-colons-alist . ((member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after)))
    (c-label-minimum-indentation . 0)
    ;; an OFFSET is nil; an inteter (usually zero); one of the symbols:  `+',
    ;; `-', `++', `--', `*', or `/' (a positive or negative multiple of
    ;; `c-basic-offset' is added; 1, -1, 2, -2, 0.5, and -0.5, respectively); a
    ;; vector; a function; or a list.
    (c-offsets-alist . ((arglist-close . c-lineup-close-paren) ; +
			(arglist-cont-nonempty . c-lineup-arglist) ; +
			(arglist-intro . c-lineup-arglist-intro-after-paren) ; +
			(block-open . -) ; 0
			(func-decl-cont . 0) ; +
			(inline-open . 0) ; +
                        (statement-case-open . *) ; 0
			(statement-cont . c-lineup-math) ; +
			(substatement-open . 0)))) ; +
  "My PERSONAL C Style, similar to NetBSD KNF.")
(c-add-style "PERSONAL" my-c-style nil)

(defconst smail-c-style
  (append my-c-style '((c-basic-offset . 4)))
  "Smail C Style; my personal style, but at offset 4.")
(c-add-style "SMAIL" smail-c-style nil)

(defconst my-awk-style
  '((c-backslash-column . 78)
    (c-basic-offset . 8)
    (c-block-comment-prefix . "# ")
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       scope-operator)) ; (scope-operator)
    (c-comment-continuation-stars . "# ")
    (c-comment-only-line-offset . (0 . 0))
    (c-comment-prefix-regexp . "# *")
    (c-comment-start-regexp . "#[ 	]*")
    ;; ACTION can be either a function symbol or a list containing any
    ;; combination of the symbols `before' or `after'.  If the list is empty,
    ;; no newlines are inserted either before or after the brace.
    (c-hanging-braces-alist . ((block-open . nil)
			       (block-close . (before))
			       (brace-list-open . nil)
			       (brace-list-close . nil)
			       (brace-list-intro . nil)
			       (brace-list-entry . nil)
			       (class-open . (after))
			       (class-close . nil)
			       (defun-open . (after))
			       (defun-close . (before))
			       (defun-block-intro . (after))
			       (inline-open . nil)
			       (inline-close . nil)
			       (statement-case-open . nil)
			       (substatement-open . nil)))
    (c-hanging-colons-alist . ((member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after)))
    (c-label-minimum-indentation . 0)
    ;; an OFFSET is nil; an inteter (usually zero); one of the symbols:  `+',
    ;; `-', `++', `--', `*', or `/' (a positive or negative multiple of
    ;; `c-basic-offset' is added; 1, -1, 2, -2, 0.5, and -0.5, respectively); a
    ;; vector; a function; or a list.
    (c-offsets-alist . ((arglist-close . c-lineup-close-paren) ; +
			(arglist-cont-nonempty . c-lineup-arglist) ; +
			(arglist-intro . c-lineup-arglist-intro-after-paren) ; +
			(block-open . -) ; 0
			(func-decl-cont . 0) ; +
			(inline-open . 0) ; +
                        (statement-case-open . *) ; 0
			(statement-cont . c-lineup-math) ; +
			(substatement-open . 0)))) ; +
  "My PERSONAL AWK Style, similar to my-c-style.")
(c-add-style "PERSONAL-AWK" my-awk-style nil)

;; This is how Dave Mills likes to see the code formatted.
;;
(defconst ntp-c-style
  '((c-basic-offset . 8)
    (c-offsets-alist . ((arglist-intro        . +)
                        (case-label           . *)
                        (statement-case-intro . *)
                        (statement-cont       . *)
                        (substatement-open    . 0))))
  "Dave L. Mills; programming style for use with ntp")
(c-add-style "ntp" ntp-c-style nil)

;; NetBSD "knf" style (ala /usr/share/misc/style)
;;
(defconst netbsd-knf-c-style
  '((c-auto-newline . nil)
    (c-tab-always-indent . nil)
    (c-recognize-knr-p . t)
    (c-basic-offset . 8)
    (c-comment-only-line-offset . 0)
    (c-cleanup-list . (brace-else-brace
		       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    (c-hanging-braces-alist . ((defun-open . (before after))
			       (defun-close . (before))
			       (class-open . (after))
			       (class-close . nil)
			       (inline-open . nil)
			       (inline-close . nil)
			       (block-open . (after))
			       (block-close . (before))
			       (substatement-open . nil)
			       (statement-case-open . nil)
			       (brace-list-open . nil)
			       (brace-list-close . nil)
			       (brace-list-intro . nil)
			       (brace-list-entry . nil)
			       ))
    (c-offsets-alist . ((knr-argdecl-intro . +)
			(arglist-cont-nonempty . 4)
			(knr-argdecl . 0)
			(block-open . -)
			(label . -)
			(member-init-intro . '++)
			(statement-cont . 4)
			)))
  "NetBSD KNF C Style.")
(c-add-style "netbsd" netbsd-knf-c-style nil)
;; these settings are also important to KNF....
(defun netbsd-knf-c-mode-hook ()
  "Other stuff for NetBSD-KNF"
  (setq tab-width 8
	indent-tabs-mode t))

;; XXX c-default-style does not work properly for awk-mode in cc-mode 5.28
;; (e.g. in 21.3) because awk-mode is just a derived mode.  See the setting of
;; awk-mode-hook below for the attempt to hack around this bug.
;;
(setq c-default-style
      '((awk-mode . "PERSONAL-AWK")
	(c-mode . "PERSONAL")
	(other . "PERSONAL")))

(defun my-c-mode-common-hook ()
  "My setup hook to be called by all CC Mode modes for common initializations."

  ;; other customizations
  (setq tab-width 8)			; normal, standard, default TAB chars
  (setq fill-column 79)
  (setq comment-column 40)
  (eval-when-compile
    (if (< init-emacs-type 21)
	(defvar comment-style)))
  (setq comment-style 'extra-line)	; not used, but maybe someday?
  (setq indent-tabs-mode t)		; only use tabs

  (if (elisp-file-in-loadpath-p "filladapt")
      (progn
	(require 'filladapt)
	(c-setup-filladapt)
	;; we supposedly can't autoload this thing, yet that means this
	;; function will not be defined at compile time...
	(turn-on-filladapt-mode)))

  ;; CC Mode things that are not style variables...
  (setq c-echo-syntactic-information-p nil)
  (setq c-electric-pound-behavior '(alignleft)) ; nil
  (setq c-recognize-knr-p t)		; yes, PLEASE!
  (setq c-tab-always-indent nil)	; insert tabs if not in left margin

  (if (fboundp 'c-toggle-auto-state)
      (c-toggle-auto-state 1))		; try this on for size!
  (if (fboundp 'c-toggle-auto-newline)
      (c-toggle-auto-newline 1))	; ... or under its new name

  ;; keybindings for all of the supported languages.  We can put these in
  ;; c-mode-base-map because awk-mode-map, c-mode-map, c++-mode-map,
  ;; objc-mode-map, java-mode-map, idl-mode-map, pike-mode-map, and so on
  ;; inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map "\ej" 'c-fill-paragraph)

  ;; even cc-mode is sometimes too over-bearing.  It seems to
  ;; insist re-setting some key bindings without regard to the
  ;; global key map.
  (override-local-key-settings)
  (override-default-variable-settings))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Derived modes don't have their major-mode (or mode-name) set until after the
;; parent mode has been initialized.  For example this causes c-default-style
;; to be useless with any modes derived from c-mode.  This silly function
;; attempts to work around that bug and can be used in the initialization hook
;; for any such mode derived from c-mode (such as awk-mode).
;;
(defun my-derived-c-mode-hook ()
  "Silly setup hook to be called by modes derived from c-mode."
  (let ((style (if (stringp c-default-style)
		   c-default-style
		 (or (cdr (assq major-mode c-default-style))
		     (cdr (assq 'other c-default-style))
		     "gnu"))))
    (c-set-style style 't)))

;; In 21.3 awk-mode is a derived mode of c-mode.
;;
(add-hook 'awk-mode-hook 'my-derived-c-mode-hook)

;;;
;;; version-control (VC) stuff....
;;;

(setq diff-switches "-u")		; defaults to "-c" for unknown reasons

;; to quiet the v19 byte compiler
(eval-when-compile
  (defvar vc-command-messages)
  (defvar vc-initial-comment)
  (defvar vc-checkout-carefully))
(add-hook 'vc-mode-hook
	  (function
	   (lambda ()
	     "Private vc-mode stuff."
	     (require 'vc)
	     (eval-and-compile
	       (if (< init-emacs-type 21)
		   (setq vc-checkout-carefully t)))
	     (setq vc-command-messages t)
	     (setq vc-initial-comment t)
	     (eval-when-compile
	       (defvar vc-maximum-comment-ring-size)) ; not defvar'ed!
	     (setq vc-maximum-comment-ring-size 64) ; 32 is too small!
	     (add-hook 'vc-checkin-hook
		       'vc-comment-to-change-log))))

;;;
;;; misc other hooks
;;;

(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda ()
	     "Private emacs-lisp-mode stuff."
	     (setq fill-column 79)
	     (turn-on-auto-fill))))

(add-hook 'isearch-mode-hook
	  (function
	   (lambda ()
	     "Private isearch-mode stuff."
	     ;;(define-key isearch-mode-map "\C-t" 'isearch-toggle-case-fold)
	     (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
	     ;;(define-key isearch-mode-map "\C-e" 'isearch-edit-string)
	     (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
	     (define-key isearch-mode-map "\C-\\" 'isearch-repeat-forward)
	     (define-key isearch-mode-map "\C-^" 'isearch-quote-char))))

(add-hook 'text-mode-hook
	  (function
	   (lambda ()
	     "Private text-mode and indented-text-mode stuff."
	     ;; If for some reason the *scratch* buffer was killed earlier
	     ;; and is recreated here because all other buffers have been
	     ;; killed, then reset the major mode to emacs-lisp-mode.
	     ;; One disadvantage to this is that you can't put the
	     ;; *scratch* buffer in text mode without disabling this hook.
	     (if (equal (buffer-name) "*scratch*")
		 (emacs-lisp-mode)
	       (progn
		 (override-local-key-settings)
		 (override-default-variable-settings)
		 (if (elisp-file-in-loadpath-p "ispell")
		     (local-set-key "\eS" 'ispell-buffer)
		   (local-set-key "\eS" 'spell-buffer))
		 ;; XXX should we also turn on the flyspell minor mode?
		 (setq abbrev-mode t)
		 (setq fill-column 72)
		 (setq require-final-newline t)	; needed by some unix programs
		 (turn-on-auto-fill))))))

;; dont' need this now...
;;(add-hook 'nroff-mode-hook
;;	  (function
;;	   (lambda ()
;;	     "Private nroff-mode stuff."
;;	     (local-set--key "\e?" 'help-command)))) ; argh!

(require 'view)
(add-hook 'view-mode-hook
	  (function
	   (lambda ()
	     "Private view-mode stuff."
	     (define-key view-mode-map "b" 'View-scroll-page-backward)
	     (define-key view-mode-map "\C-h" 'View-scroll-page-backward))))

;; the real thing, in 19.30(?) and above
(if (elisp-file-in-loadpath-p "sh-script")
    (progn
      ;; to quiet the v19 bytecompiler...
      (eval-when-compile
	(defvar sh-alias-alist)
	(defvar sh-indentation)
	(defvar sh-basic-offset)
	(defvar sh-learn-basic-offset)
	(defvar sh-indent-comment)
	(defvar sh-indent-for-case-label)
	(defvar sh-indent-for-case-alt)
	(defvar sh-indent-for-do)
	(defvar sh-indent-for-then)
	(defvar sh-indent-after-switch)
	(defvar sh-indent-after-case)
	(defvar sh-indent-after-do))
      (setq sh-alias-alist
	    '((ksh . posix)		; most shells are really posix
	      (bash2 . posix)
	      (ash . posix)
	      (sh . posix)
	      (sh5 . sh)))
      (setq sh-indentation 8)
      (setq sh-basic-offset 8)
      (setq sh-learn-basic-offset nil)	; never....
      (setq sh-indent-comment t)
      ;; 	+   Indent right by sh-basic-offset
      ;; 	-   Indent left  by sh-basic-offset
      ;; 	++  Indent right twice sh-basic-offset
      ;; 	--  Indent left  twice sh-basic-offset
      ;; 	*   Indent right half sh-basic-offset
      ;; 	/   Indent left  half sh-basic-offset.
      (setq sh-indent-for-case-label 0)
      (setq sh-indent-for-case-alt '+)
      (setq sh-indent-for-do 0)
      (setq sh-indent-after-do '+)
      (setq sh-indent-for-then 0)
      (setq sh-indent-after-case 0)		; for rc
      (setq sh-indent-after-switch 0)		; for rc
      (add-hook 'sh-mode-hook
		(function
		 (lambda ()
		   "Private sh-mode-hook."
		   (override-local-key-settings)
		   (override-default-variable-settings))))))

(if (elisp-file-in-loadpath-p "perl-mode")
    (progn
      (add-hook 'perl-mode-hook
		(function
		 (lambda ()
		   "Private perl-mode-hook."
		   (override-default-variable-settings)
		   (override-local-key-settings))))))

(if (elisp-file-in-loadpath-p "tcl")
    (progn
      ;; to quiet the v19 bytecompiler...
      (eval-when-compile
	(defvar tcl-indent-level)
	(defvar tcl-continued-indent-level))
      (setq tcl-indent-level 8)
      (setq tcl-continued-indent-level 8)
      (add-hook 'tcl-mode-hook
		(function
		 (lambda ()
		   "Private tcl-mode-hook."
		   (override-default-variable-settings)
		   (override-local-key-settings))))))


;;;; ----------
;;;; more hooks for non-default packages

(if (and (elisp-file-in-loadpath-p "ksh-mode")
	 (not (elisp-file-in-loadpath-p "sh-script")))
    (progn
      ;; to quiet the v19 byte compiler
      (eval-when-compile
	(defvar ksh-indent)
	(defvar ksh-group-indent)
	(defvar ksh-brace-indent)
	(defvar ksh-case-item-indent)
	(defvar ksh-case-indent)
	(defvar ksh-match-and-tell))
      (add-hook 'ksh-mode-hook
		(function
		 (lambda ()
		   "Private ksh-mode stuff."
		   (setq ksh-indent 8)
		   (setq ksh-group-indent -8)
		   (setq ksh-brace-indent 0)
		   (setq ksh-case-item-indent 0)
		   (setq ksh-case-indent 8)
		   (setq ksh-match-and-tell t))))))

;;;; ----------
;;;; some default key re-binding....

;;; first off, we do some fancy stuff to make C-h work "properly," but still
;;; have good access to the help functions!
;;
;; NOTE: this *should* work by simply reading termio for current erase char.
;; As of emacs-21.2 there's a note in the NEWS file which says "** On terminals
;; whose erase-char is ^H (Backspace), Emacs now uses
;; normal-erase-is-backspace-mode."  Unfortunately this does exactly the wrong
;; thing, and in a totally bizzare and stupid way.
;;
;; Remember to call override-local-key-settings in the appropriate hooks to fix
;; up modes which violate global user preferences....
;;
(if (and (>= init-emacs-type 21)
	 (>= emacs-version-minor 2))
    ;; grrr.... do something with that stupid broken poor useless excuse for
    ;; a feature, normal-erase-is-backspace-mode....
    (setq keyboard-translate-table nil))
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-?" 'delete-char)
(global-set-key "\e\C-h" 'backward-kill-word)
(global-set-key "\e\C-?" 'kill-word)


;;; OK, now we diddle with help....
;;
;; Oddly, the help interface in emacs is extremely scatter-brained, with
;; several slightly different ways of doing the same thing.  This is probably
;; due to the fact that several different programmers have implemented various
;; bits and pieces of the help systems.  See help.el and help-macro.el, but try
;; not to tear your hair out when you find out help-event-list in 19.34 is
;; essentially bogus, since it is simply an extension to a "standard" list.
;;
;; Remember to call override-local-key-settings in the appropriate hooks to fix
;; up modes which violate global user preferences....
;;
(global-set-key [f1] 'help-command)	; first do this for 19.28.
(global-set-key "\e?" 'help-command)	; this is the first step to set up help
(global-set-key "\e?F" 'view-emacs-FAQ)	; in 19.34 it needs more help...
;; should help-char be just ? instead?
(setq help-char ?\M-?)			; this should "fix" the rest.

;;; I USUALLY EXPECT THE BACKSPACE KEY TO WORK LIKE AN ASCII BACKSPACE!
;;
;; For some entirely un-fathomable reason the default function bindings make
;; the 'backspace' and 'delete' keys synonymous!
;;
(define-key function-key-map [backspace] [?\C-h])
(define-key function-key-map [M-backspace] [?\M-\C-h])
;;(define-key function-key-map [C-backspace] [?\C-h]) ; sometimes *is* DEL....

;;; OK, that's the end of the stuff to fix GNU Emacs' C-h brain damage.

;;;Message-Id: <3063228438513258@naggum.no>
;;;References: <199701241330.IAA26620@mks.com>
;;;From: Erik Naggum <erik@naggum.no>
;;;To: gnu-emacs-bug@cis.ohio-state.edu
;;;Date: 26 Jan 1997 00:47:18 +0000
;;;Subject: Re: y-or-n-p should accept RET as a positive acknowlegement
;;;
;;;* David J. Fiander
;;;| Why does it accept SP but not RET as a positive acknowlegement?
;;;
;;;add these lines somewhere in your setup files:
;;;
;;;#\Erik
;;;-- 
;;;1,3,7-trimethylxanthine -- a basic ingredient in quality software.
;;
(define-key query-replace-map [return] 'act)
(define-key query-replace-map "\C-m" 'act)

;; these are really just for Sun-style keyboards....
(global-set-key [cancel] 'keyboard-quit)
(global-set-key [redo] 'repeat)
(global-set-key [M-redo] 'repeat-complex-command)
(global-set-key [redo] 'repeat)
(global-set-key [copy] 'kill-ring-save)
(global-set-key [open] 'find-file)
(global-set-key [paste] 'yank)
(global-set-key [cut] 'kill-region)

;;; for fingers that forget and terminals that are brain-dead....
(global-set-key "\C-\\" 'isearch-forward)
(global-set-key "\C-x\C-\\" 'save-buffer)
(global-set-key "\C-^" 'quoted-insert)
(global-set-key "\C-x\C-^" 'toggle-read-only)

;; rumour has it C-x C-q was toggle-read-only and then vc-toggle-read-only
;; this was suggested by Kevin Rodgers <kevinr@ihs.com>:
(global-set-key "\C-xq" 'toggle-read-only)
(global-set-key "\C-xvq" 'vc-toggle-read-only)

;; Francesco Potorti` <pot@gnu.org> also advises this:
;;
;; Ask before using version control: maybe toggle-read-only is enough
(defadvice vc-toggle-read-only (around vc-ask activate)
  (if (and (vc-backend buffer-file-name)
	   (y-or-n-p "Toggle version control status? "))
      ad-do-it
    (toggle-read-only)))

;;; some of the remainder is to get back some Jove/Gosmacs comaptability, but
;;; without getting it all....
;;;
(global-set-key "\e\C-r" 'isearch-backward-regexp)
(global-set-key "\eq" 'query-replace-regexp)
(global-set-key "\eQ" 'query-replace)
(global-set-key "\er" 'replace-regexp)
(global-set-key "\eR" 'replace-string)

(global-set-key "\ej" 'fill-paragraph)
(global-set-key "\eJ" 'fill-region)

(global-set-key "\C-x\C-i" 'insert-file)
(global-set-key "\C-xI" 'insert-buffer)
(global-set-key "\e " 'set-mark-command)
(global-set-key "\C-x " 'fixup-whitespace)
(global-set-key "\C-xt" 'goto-line)

(global-set-key [?\C-\M-\(] 'beginning-of-defun)
(global-set-key [?\C-\M-\)] 'end-of-defun)

(global-set-key "\e!" 'shell)
(global-set-key "\C-x!" 'shell-command)
(global-set-key "\C-x\C-d" 'insert-date-in-current-buffer)

(global-set-key "\C-x\C-v" 'find-file)	; I never liked "visit"....
(global-set-key "\C-xV" 'find-alternate-file)

(global-set-key "\ez" 'scroll-one-line-down)
(global-set-key "\C-z" 'scroll-one-line-up)
(global-set-key "\e\C-l" 'line-to-top-of-window)

(global-set-key "\C-xz" 'enlarge-window)
(global-set-key "\C-x\C-z" 'shrink-window)

(global-set-key "\e\C-z" 'suspend-emacs)

(global-set-key "\C-x?" 'describe-key-briefly)
(if (fboundp 'super-apropos)
    (global-set-key "\C-x\C-a" 'super-apropos)
  (if (fboundp 'apropos-documentation)
      (global-set-key "\C-x\C-a" 'apropos-documentation)))


(global-set-key "\e," 'top-of-window)		; mirror M-<
(global-set-key "\e." 'bottom-of-window)	; mirror M->

(global-set-key "\C-xT" 'find-tag)
(global-set-key "\C-x4T" 'find-tag-other-window)

(global-set-key "\eS" 'spell-buffer)

(if window-system
    (progn
      (global-set-key "\C-xp" 'previous-multiframe-window)
      (global-set-key "\C-x51" 'delete-other-frames)
      (global-set-key "\C-x5i" 'iconify-frame)
      (global-set-key "\C-x5l" 'lower-frame)
      (global-set-key "\C-x5u" 'raise-frame)
      (global-set-key "\C-x5T" 'find-tag-other-frame)
      (if (fboundp 'make-frame-on-display)
	  (global-set-key "\C-x5O" 'make-frame-on-display))))

;;; Message-Id: <199504171641.KAA21020@async.cs.utah.edu>
;;; Original-To: bug-gnu-emacs@prep.ai.mit.edu
;;; From: willrich@async.cs.utah.edu (William F Richardson)
;;; Subject: GNU Emacs suggestions/contributions
;;; Date: Mon, 17 Apr 1995 10:41:54 -0600 (MDT)
;;;
;; This is only useful under X windows.
(if (not (fboundp 'delete-other-frames))
    (defun delete-other-frames ()
      "Delete all frames other than the currently selected one."
      (interactive)
      (let ((me (selected-frame))
	    (list (frame-list)))
	(while (car list)
	  (if (not (eq me (car list)))
	      (delete-frame (car list)))
	  (setq list (cdr list))))))

;;; Bindings to make it look like Jove (or old Emacs :-)
;;; (courtesy Mark Moraes)
;;;(defun prev-window ()
;;;  (interactive)
;;;  (other-window -1)) ; this does not deal with argument
;;;(define-key global-map "\C-xn" 'other-window)
;;;(define-key global-map "\C-xp" 'prev-window)
;;;(define-key global-map "\C-xq" 'quoted-insert)
;;;(define-key global-map "\C-z" 'one-scroll-up)
;;;(define-key global-map "\ez" 'one-scroll-down)
;;;(define-key global-map "\C-r" 'search-backward)
;;;(define-key global-map "\eq" 'query-replace-regexp)
;;;(define-key global-map "\er" 'replace-regexp)
;;;(define-key global-map "\eg" 'goto-line)
;;;(define-key global-map "\ej" 'fill-paragraph)
;;;(define-key global-map "\e\C-z" 'suspend-emacs)
;;;(define-key global-map "\C-\\" 'search-forward)
;;;(define-key global-map "\C-x\C-\\" 'save-buffer)
;;;(define-key global-map "\C-x\C-i" 'insert-file)
;;;(define-key global-map "\C-h" 'delete-backward-char)
;;;(define-key global-map "\e\C-h" 'backward-kill-word)
;;;(define-key global-map "\C-x!" 'shell-command)
;;;(define-key global-map "\e\e" 'keyboard-quit)
;;;(define-key global-map "\e " 'set-mark-command)
;;;(define-key global-map "\eC-M" 'set-mark-command)

;;;;-------
;;;; more goodies

;;;(if (= init-emacs-type '19)
;;;      (dont-compile
;;;	(defun display-buffer-in-frame-or-window (buf)
;;;	  "Try to find buffer BUF in another (visible) frame, otherwise call
;;;display-buffer for it"
;;;	  (or (get-buffer-window buf t)
;;;	      (display-buffer buf)))
;;;	(setq temp-buffer-show-function 'display-buffer-in-frame-or-window)))

;;; From: dsmith@spam.maths.adelaide.edu.au (David Smith)
;;; Subject: framepop.el: Display temporary buffers in dedicated frame
;;; Date: 08 Oct 1993 09:17:05 GMT
;;; Organization: The University of Adelaide
;;; Message-Id: <DSMITH.93Oct8184705@spam.maths.adelaide.edu.au>
;;;
;;;
;;; let's leave this until frame management is a wee bit more mature
;;;
;;;(if (and window-system
;;;	 (elisp-file-in-loadpath-p "framepop"))
;;;    (dont-compile
;;;      (setq framepop-prefix-map (lookup-key global-map "\C-c\C-f"))
;;;      (if (not (keymapp framepop-prefix-map))
;;;	  (progn
;;;	    (setq framepop-prefix-map (make-sparse-keymap))
;;;	    (define-key global-map "\C-c\C-f" framepop-prefix-map)
;;;	    (define-key global-map "\C-c\C-fz" 'framepop-toggle-frame)
;;;	    (define-key global-map "\C-c\C-fv" 'framepop-scroll-frame)
;;;	    (define-key global-map "\C-c\C-fs" 'framepop-show-frame)
;;;	    (define-key global-map "\C-c\C-fx" 'framepop-iconify-frame)
;;;	    (define-key global-map "\C-c\C-fr" 'framepop-raise-frame)
;;;	    (define-key global-map "\C-c\C-fl" 'framepop-lower-frame)
;;;	    (cond (window-system (require 'framepop)))))))

;;; From: ca@cs.umd.edu (Cengiz Alaetinoglu)
;;; Newsgroups: gnu.emacs.sources
;;; Subject: compile-frame.el version 1.1
;;; Date: 08 Oct 1993 20:28:22 GMT
;;; Organization: University of Maryland, Computer Science Department
;;; Lines: 126
;;; Distribution: world
;;; Message-Id: <CA.93Oct8162822@yangtze.cs.umd.edu>
;;;
(if (and (elisp-file-in-loadpath-p "compile-frame")
	 window-system)
    (progn
      ;; to quiet the v19 byte compiler
      (eval-when-compile
	(defvar compilation-frame-id))
      (require 'compile-frame)
      (eval-and-compile
	(autoload 'raise-frame "frame"	; actually in frame.c
	  "Bring FRAME to the front, so it occludes any frames it overlaps."
	  nil nil))
      (add-hook 'compilation-frame-selected-hook
		(function
		 (lambda ()
		   "Private compilation-frame stuff."
		   (raise-frame compilation-frame-id))))))

(if (or window-system (and (fboundp 'server-process)
			   server-process))
    (progn
      (setq kill-emacs-query-functions '(ask-really-exit-emacs))))

;;; for those times we forget...
;;;
(defun ask-really-exit-emacs ()
  "Query user if he really wants to exit since this will destroy the
current emacs server process..."
  (interactive)
  (beep)
  (yes-or-no-p "Are you sure you *really* want to exit? "))

;;; From: kfogel@occs.cs.oberlin.edu (Karl Fogel)
;;; Date: Mon, 1 Nov 1993 10:23:04 -0500
;;; Message-Id: <9311011523.AA18545@occs.cs.oberlin.edu>
;;; To: gnu-emacs-sources@prep.ai.mit.edu
;;; Subject: frame hopping from the keyboard
;;;
;;;         I wanted a something to move among frames without using the
;;; mouse.  Emacs 19 apparently has no native function to do this
;;; (corrections?  I couldn't find it, at least...), so here is one.  I
;;; bind it to C-c o, myself.  Another candidate was C-x 5 o; although
;;; it's too many keystrokes for me, it seems as though this is sort of
;;; what C-x 5 o was originally meant to do (again, not sure).  Please let
;;; me know if you find it useful or have any suggestions/fixes.  As you
;;; can see, it's quite short (shorter than this paragraph, okay), but I
;;; have hardly touched my mouse since I started using it :-)
;;;
;;; (if window-system
;;;     (progn
;;;       (global-set-key "\C-co" 'keyboard-focus-next-or-previous-frame)
;;;       ;;
;;;       (defun keyboard-focus-next-or-previous-frame (parg)
;;; 	"Switch the focus to the next logical frame (and raise that frame to
;;; the front).  Keyboard input will go to the newly selected frame.
;;; Prefix ARG means go to previous frame, not next frame.
;;; The mouse cursor will not follow you, which is kind of a weird
;;; feeling, but you'll get used to it."
;;; 	(interactive "P")
;;; 	(let* ((nowframe (selected-frame))
;;; 	       (nextframe (if parg (previous-frame) (next-frame)))
;;; 	       (visip (frame-visible-p nextframe)))
;;; 	  (and visip
;;; 	       (progn
;;; 		 (select-frame nextframe)
;;; 		 (if (eq visip 'icon) (iconify-or-deiconify-frame))
;;; 		 (redirect-frame-focus nowframe nextframe)
;;; 		 (raise-frame nextframe)))))))

;;; Based on suggestions by David G. Grubbs <dgg@ksr.com> and Paul Palmer
;;; <palmerp@math.orst.edu>.
;;;
;;; Assuming the use of detex 2.3 by Daniel Trinkle:
;;; -w means one word per line.
;;; -n means don't expand \input or \include commands.
;;; -l means force LaTeX mode.
;;;
(if (and (elisp-file-in-loadpath-p "ispell")
	 (file-in-pathlist-p "ispell" exec-path))
    (progn
      ;; to quiet the v19 byte compiler
      (eval-when-compile
	(defvar ispell-dictionary)
	(defvar ispell-filter-hook)
	(defvar ispell-filter-hook-args)
	(defvar plain-TeX-mode-hook)
	(defvar LaTeX-mode-hook)
	(defvar nroff-mode-hook))
      (require 'ispell)
      (setq ispell-dictionary (getenv "DICTIONARY"))
      (if (not ispell-dictionary)
	  (setq ispell-dictionary "british")) ; that's what's best!!!
      (define-key global-map "\M-S" 'ispell-buffer)
      (setq plain-TeX-mode-hook
	    (function
	     (lambda ()
	       (setq ispell-filter-hook "detex")
	       (setq ispell-filter-hook-args '("-nw")))))
      (setq LaTeX-mode-hook
	    (function
	     (lambda ()
	       (setq ispell-filter-hook "detex")
	       (setq ispell-filter-hook-args '("-lnw")))))
      (setq nroff-mode-hook
	    (function
	     (lambda ()
	       (setq ispell-filter-hook "deroff")
	       (setq ispell-filter-hook-args '("-w")))))))

;;; unix "spell" knows to use "deroff", so only use this if you use a speller
;;; other than it.
;;;
;;;(defun filter-through-deroff ()
;;;  "Magic!"
;;;  (setq spell-command (concat "deroff | " spell-command)))

;;; GNU Aspell has custom filter modes built into it
;;;
(if (and (elisp-file-in-loadpath-p "ispell")
	 (file-in-pathlist-p "aspell" exec-path))
    (setq-default ispell-program-name "aspell"))

;;;;
;;;; calendar and appointment stuff
;;;;

(require 'calendar)
(require 'appt)
(if (and (boundp 'appt-timer) appt-timer)
    (cancel-timer appt-timer))		; turn off appt-check immediately

;;; eval this to re-start appt-check:
;;; (setq appt-timer (run-at-time t 60 'appt-check))

;; For 61 Lorraine Drive:
;; This is according to mapblast.com: 43.77681 N  79.420865 W
;; the GPS essentially agrees:  43 46 36.9 N  79 25 15.1 W
(setq calendar-latitude 43.77681)
(setq calendar-longitude -79.420865)

(setq calendar-time-display-form
      '(24-hours ":" minutes
		 (if time-zone " (") time-zone (if time-zone ")")))
(setq diary-date-forms
      '((month "/" day "[^/0-9]")
	(monthname "/" day "[^/0-9]")
	(year "/" month "/" day "[^0-9]")
	(year "/" monthname "/" day "[^0-9]")
	(month "-" day "[^/0-9]")
	(monthname "-" day "[^/0-9]")
	(year "-" month "-" day "[^0-9]")
	(year "-" monthname "-" day "[^0-9]")
	(monthname " *" day "[^,0-9]")
	(monthname " *" day ", *" year "[^0-9]")
	(dayname "\\W")))
(setq american-date-diary-pattern
      '((month "/" day "[^/0-9]")
	(monthname "/" day "[^/0-9]")
	(year "/" month "/" day "[^0-9]")
	(year "/" monthname "/" day "[^0-9]")
	(month "-" day "[^/0-9]")
	(monthname "-" day "[^/0-9]")
	(year "-" month "-" day "[^0-9]")
	(year "-" monthname "-" day "[^0-9]")
	(monthname " *" day "[^,0-9]")
	(monthname " *" day ", *" year "[^0-9]")
	(dayname "\\W")))

(setq appt-audible t)			; beep to warn of appointments
(setq appt-display-mode-line t)		; show minutes to appt time in mode line
(setq appt-display-duration 60)		; seconds to display appointment msg
					; (NOTE: must be less than appt-display-interval!)
(setq appt-display-interval 2)		; minutes between checks of appointment list (def. 3)

;; NOTE: this is really also the minimum interval allowed between appointments.
;; If appointments are made any more dense than this interval then they will
;; clobber each other as only one can be shown at a time.
(setq appt-message-warning-time 15)	; minutes of warning prior to appt

;; XXX these still need rewriting for 22.1 and newer
(setq appt-msg-window t)		; do not show appt message in a separate window!
(setq appt-visible t)			; show appt msg in echo area (only if appt-msg-window is nil)

;; keep appointment messages visible in their wee window.
;;
(setq appt-delete-window-function 'ignore) ; never delete the window automatically

;; Theory has it that you can re-define (appt-disp-window) to have it create
;; pop-up frames, but so long as we run a separate emacs for calendaring then
;; appts will cause it's frame to be raised.

;; In theory we could also (setq calendar-setup 'one-frame) to have it
;; automaticaly open a dedicated frame when it starts; however if we run a
;; separate emacs for calendaring then we can use it's main frame.

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(add-hook 'list-diary-entries-hook 'sort-diary-entries)

(if (<= (safe-length command-line-args) 1)
    (progn
      ;; rebuild appt-time-msg-list every time the diary display is rebuilt
      ;; NOTE: this has to be appended after fancy-diary-display so that
      ;; it is executed last
      (add-hook 'diary-display-hook 'appt-make-list t)
      ;; only do appt stuff if we're running a long-term session....
      (setq appt-display-diary t)	; display diary at midnight
      (setq appt-issue-message t))	; enable appt msgs
  (progn
    (setq appt-display-diary nil)
    (setq appt-issue-message nil)))

(setq mark-diary-entries-in-calendar t) ; quite CPU expensive....
(setq mark-holidays-in-calendar t)

(setq view-diary-entries-initially t)	; do diary-display on first invocation
					; and at midnight....
(setq diary-list-include-blanks t)	; include holidays in diary even if
					; there is no diary entry for that day

(eval-when-compile
  (defvar number-of-diary-entries))
;; XXX still in docs in 22.3, but apparently not used
(setq number-of-diary-entries [4 4 4 4 4 5 5]) ; always enough to span a long weekend

(setq all-christian-calendar-holidays t)
(setq other-holidays
      '((holiday-sexp			; abs-easter stolen from holidays.el
	 '(let* (; (year (car (cdr (cdr (cdr (cdr (cdr (decode-time))))))))
		(century (1+ (/ year 100)))
		(shifted-epact        ;; Age of moon for April 5...
		 (% (+ 14 (* 11 (% year 19));;     ...by Nicaean rule
		       (-           ;; ...corrected for the Gregorian century rule
			(/ (* 3 century) 4))
		       (/    ;; ...corrected for Metonic cycle inaccuracy.
			(+ 5 (* 8 century)) 25)
		       (* 30 century));;              Keeps value positive.
		    30))
		(adjusted-epact       ;;  Adjust for 29.5 day month.
		 (if (or (= shifted-epact 0)
			 (and (= shifted-epact 1) (< 10 (% year 19))))
		     (1+ shifted-epact)
		   shifted-epact))
		(paschal-moon       ;; Day after the full moon on or after March 21.
		 (- (calendar-absolute-from-gregorian (list 4 19 year))
		    adjusted-epact))
		(abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))
	   (calendar-gregorian-from-absolute (+ abs-easter 1))) "Easter Monday (Canada)")
	(holiday-float 5 1 -2 "Victoria Day (Canada)") ; second last Monday [or
						       ; is it the 3rd Monday?]
	(holiday-fixed 7 1 "Canada Day")
	(holiday-float 8 1 1 "Civic Holiday (Canada)") ; first Monday
	(holiday-float 9 1 1 "Labour Day (Canada)") ; first Monday
	(holiday-float 10 1 2 "Thanksgiving Day (Canada)") ; second Monday
	(holiday-fixed 12 26 "Boxing Day (Canada & UK)")
	;; the rest are pseudo-holidays or non-local holidays...
	(holiday-fixed 1 11 "Sir John A. Macdonald's birthday")
	(holiday-fixed 1 25 "Robby Burns Day")
	(holiday-float 2 1 3 "Heritage Day") ; (unoff.) third Monday
	(holiday-fixed 4 1 "April Fool's Day")
	(holiday-fixed 4 21 "Queen Elizabeth's birthday")
	(holiday-fixed 4 22 "Earth Day")
	(holiday-float 5 1 -1 "Memorial Day Spring Holiday (U.K.)") ; last Monday (?)
	(holiday-fixed 6 8 "Queen's Birthday (N.Z.)") ; (?)
	(holiday-float 6 1 2 "Queen's Birthday (A.C.T., NSW, N.T., Qld., S.A., Tas., Vic., Aust.)") ; (?)
	(holiday-float 6 1 -1 "Fete National (Quebec)") ; last Monday (?)
	(holiday-fixed 7 14 "Bastille Day")
	(holiday-float 8 1 -1 "Summer Bank Holiday (U.K.") ; last Monday(?)
	(holiday-float 10 1 1 "Labour Day (A.C.T, N.S.W, S.A., Aust.)") ; first Monday
	(holiday-fixed 10 16 "World Food Day") ; ????
	(holiday-float 10 1 -1 "Labour Day (N.Z.)") ; last Monday
	(holiday-fixed 11 11 "Remembrance Day (Canada)")
	(holiday-fixed 12 6 "National Day of Remembrance and Action on Violence Against Women")))

(defadvice calendar (before calendar-toggle-appt-check-advice activate)
  "Ask if `appt-check' should be run every sixty seconds."
  (if (y-or-n-p "Enable 60-sec timer for `appt-check'? ")
      (setq appt-timer (run-at-time t 60 'appt-check))))

;; this may not be necessary when view-diary-entries-initially is not nil
;;
(defadvice calendar (after calendar-do-diary-advice activate)
  "Run `diary' every time `calendar' is started."
  (diary))

;;;;
;;;; timeclock.el stuff
;;;;

(if (elisp-file-in-loadpath-p "timeclock")
    (progn
      ;; You'll probably want to bind the timeclock commands to some handy
      ;; keystrokes.  At the moment, C-c t is unused in Emacs 21 (and here in
      ;; ~/.emacs.el):
      ;;
      (require 'timeclock)

      (eval-when-compile
	(defvar timeclock-use-display-time))	; quiet the compiler
      (setq timeclock-use-display-time t)

      ;; NOTE:  you must have a ~/.timelog file or this will crap out...
      ;;
      (if (file-exists-p "~/.timelog")
	  (timeclock-modeline-display))

      ;; There's probably a better way to do this....
      ;;
      (defun timeclock-bindings-help ()
	"Show help for timeclock bindings"
	(interactive)
	(message
	 "i - in, c - change, o - out, r - reread-log, u - update-mode, v - visit-log, w - when-to-leave"))
      
      (defun my-timeclock-generate-report (&optional html-p)
	"Show help for timeclock bindings"
	(interactive "p")
	(setq html-p (> html-p 1))
	(set-buffer (get-buffer-create (if html-p
					   "*TimeClock HTML Report*"
					 "*TimeClock Report*")))
	(setq buffer-read-only nil)
	(erase-buffer)
	(eval-when-compile
	  (if (elisp-file-in-loadpath-p "timeclock")
	      (require 'timeclock)))
	(timeclock-generate-report html-p)
	(pop-to-buffer (current-buffer)))
      
      (define-key global-map "\C-ct?" 'timeclock-bindings-help) ; XXX should use help-char
      (define-key global-map "\C-cti" 'timeclock-in)
      (define-key global-map "\C-cto" 'timeclock-out)
      (define-key global-map "\C-ctc" 'timeclock-change)
      (define-key global-map "\C-cts" 'timeclock-status-string)
      (define-key global-map "\C-ctr" 'timeclock-reread-log)
      (define-key global-map "\C-ctu" 'timeclock-update-modeline)
      (define-key global-map "\C-ctv" 'timeclock-visit-timelog)
      (define-key global-map "\C-ctw" 'timeclock-when-to-leave-string)
      ))

;;;;
;;;; todo stuff....
;;;;

(if (elisp-file-in-loadpath-p "todo-mode")
    (progn
      ;; todo-mode autoloads itself and may even be autoloaded by calendar....

      (eval-when-compile
	(require 'todo-mode))
      ;; 
      (setq todo-prefix "&%%(equal (calendar-current-date) date)")

      (global-set-key "\C-cTs" 'todo-show) ;; switch to TODO buffer
      (global-set-key "\C-cTi" 'todo-insert-item) ;; insert new item
      ))

;;;;
;;;; mail-mode stuff
;;;;

;(setq read-mail-command 'vm)
(setq read-mail-command 'wl)

;;;;
;;;; for sendmail.el et al....
;;;;

(require 'sendmail)

;; With Smail's content filtering we will get an error if we try to post
;; something that's forbidden -- however the current stupid `sendmail-send-it'
;; seems to ignore errors from the mailer unless `mail-interactive' is set.
;;
;; Unfortunately interactive delivery will appear to fail if the mailer returns
;; EX_TEMPFAIL even though the message may have been successfully queued for
;; later delivery.  Arguably the mailer shouldn't return EX_TEMPFAIL in this
;; situation, but some do (eg. some versions Smail).
;;
;; The right thing to do is to rewrite sendmail-send-it to properly handle all
;; errors all the time.
;;
(setq mail-interactive nil)

(setq mail-specify-envelope-from nil)	; not ususually permitted anyway

(setq mail-yank-prefix "> ")
(setq mail-header-separator ".")	; as close to nothing as possible

(setq mail-archive-file-name "~/Mail/.outgoing") ; for non-VM use

(defconst mail-default-domains-completion-alist
  '(
    ("weird.com")
    ("weird.ca")
    ("weird.toronto.on.ca")
    ("robohack.ca")
    ("planix.com")
    ("robohack.planix.com")
    ("planix.ca")
    ("planix.net")
    ("reptiles.org")
    ("vex.net")
    ("aci.on.ca")
    ("proxy.net")
    ("accumarkpg.com")
    ("protagon.com")
   )
  "*Default list of domains for mail-local-domain-name.")

(eval-and-compile
  (defvar mail-local-domain-name (or (let ((envvalue (getenv "DOMAINNAME")))
				       (if (or (null envvalue)
					       (string-equal envvalue ""))
					   nil
					 (if (string-equal "." (substring envvalue 0 1))
					     (substring envvalue 1)
					   envvalue)))
				     (if (string-match "\\." (system-name))
					 (substring (system-name) (match-end 0)))
				     (completing-read "Domain name (without host part): "
						      mail-default-domains-completion-alist))
    "*Local domain name to be used for mail purposes."))

(defun mail-reset-mail-local-domain-name-users ()
  "Run after chaning `mail-local-domain-name'."
  (setq mail-default-reply-to (concat "\"" (user-full-name) "\" <"
				      (user-login-name) "@"
				      mail-local-domain-name ">"))
  (my-mail-signature-selector))

(add-hook 'after-init-hook 'mail-reset-mail-local-domain-name-users)

(defun set-new-mail-local-domain-name (new-domain)
  "Set a new value for `mail-local-domain-name'."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 new-domain)
     (setq new-domain (completing-read "Domain name (without host part): "
				       mail-default-domains-completion-alist))
     (list new-domain)))
  ;; strip any leading dot...
  (if (string-equal "." (substring new-domain 0 1))
      (setq new-domain (substring new-domain 1)))
  (setq mail-local-domain-name new-domain)
  (mail-reset-mail-local-domain-name-users))

;; xxx need to do something to set mail-host-address correctly!!!
;; (doing that should give user-mail-address the "right" value)

;; mailcrypt --- a simple interface to message encryption with PGP.
;;
(if (and (fboundp 'elisp-file-in-loadpath-p)
	 (elisp-file-in-loadpath-p "mailcrypt"))
    (progn
      ;; to quiet the v19 byte compiler
      (eval-when-compile
	(defvar mc-encrypt-for-me)
	(defvar mc-passwd-timeout)
	(defvar mc-pgp-always-sign)
	(defvar mc-pgp50-user-id)
	(defvar mc-pgp50-fetch-timeout)
	(defvar mc-pgp50-keyserver-address)
	(defvar mc-pgp50-hkpserver-address))

      (setq mc-encrypt-for-me t)
      (setq mc-passwd-timeout 1800)
      (setq mc-pgp-always-sign t)
      ;; XXX this should be auto-set using the current 'From:'
      (setq mc-pgp50-user-id (concat (user-login-name) "@" mail-local-domain-name))
      (setq mc-pgp50-fetch-timeout 60)
      (setq mc-pgp50-keyserver-address "pgpkeys.mit.edu")
      (setq mc-pgp50-hkpserver-address "pgpkeys.mit.edu")))

(if (elisp-file-in-loadpath-p "ispell")
    (progn
      (define-key mail-mode-map "\C-ci" 'ispell-message)
      (define-key mail-mode-map "\M-S" 'ispell-message)))

(define-key mail-mode-map "\C-i" 'mail-goto-next-header-or-insert)
(define-key mail-mode-map [S-tab] 'mail-goto-previous-header)

(defun mail-goto-previous-header (&optional count)
  "Call mail-goto-next-header-or-insert with (- COUNT)"
  (interactive "p")
  (mail-goto-next-header-or-insert (- count)))

(defun mail-goto-next-header-or-insert (&optional count)
  "If in header area, go to beginning of next header.
If point is not in the header area, just insert the character which
invoked this command.

With numeric prefix arg, skip forward that many headers.
If prefix arg is negative, skip backward that many headers.

If either the head or tail of the headers are reached, wrap around
to the other end and continue."
  (interactive "p")
  (let* ((headers-end
	  (save-excursion
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "$"))
	    (match-beginning 0)))
	 (forwardp (or (null count) (> count 0)))
	 (fn (if forwardp 're-search-forward 're-search-backward))
	 (la (if forwardp 2 1))
	 (nla (if forwardp 1 2))
	 (re "^[^:\n\t ]+:")
	 (i (abs count)))
    (cond ((>= (point) headers-end)
	   (call-interactively 'self-insert-command))
	  (t
	   (save-restriction
	     (narrow-to-region (point-min) headers-end)
	     (while (not (zerop i))
	       (cond
		((funcall fn re nil t (if (looking-at re) la nla)))
		(t
		 (goto-char (point-min))
		 (funcall fn re nil t)))
	       (setq i (1- i)))
	     (goto-char (match-end 0))
	     (and (looking-at "[ \t]")
		  (forward-char 1)))))))

;; mail-x-face-file thanks to John Owens <owens@graphics.stanford.edu>
;;
(defvar mail-x-face-file "~/.face"
  "The name of a file containing the content for your `X-Face' header in
`compface' output format.")

(defvar mail-default-x-face
  " *Vqm<\"ErD\"t5{*PViZ^>nLZo*Td+y(r1f3)jj\\>&1O=,q?:pt6cR]Hi6h|*f#
 8nBBgD@\\+D7Cf5&(Wg'RA|/+Ee`$\"V>se7P7j2O-MG?rIJW=EBcaQf7k\\KnmT0^375!h
 \\._mc>~9'd_
"
  "The default x-face content in `compface' output format (i.e. with a leading
space on every line, and a trailing newline).")

(defvar mail-alternate-x-face
  " G=jn*S]P-JmPX0[GAK;)7Yo0p?#U/0m{g!*j3XGvT80*#5pX0kPN$4+azk{O#@ZE
 ZV9BS:4y;\9utXK@+?.mCT.k%G&Ix2XEj-`bBt{TituWYrQ5npZb+:ERfmRt-((lW:itQr$
 C|B~;vhJ:>2,{tA}#)P'g3h6eE8JT|Qfcm50pUoy{zb8=jvof2?lY}EYTEt4z=5*i%OJ136
 \?S8^g~^>,s&,jBb'=K|ryeVtUX5
"
  "An alternate x-face file -- this one is a cute little BSD daemon.
Use it by evaluating `(setq mail-default-x-face mail-alternate-x-face)'")

(defun mail-insert-x-face ()
  "Insert an X-Face header containing the contents of mail-x-face-file."
  (if (file-exists-p mail-x-face-file)
      (save-excursion 
	(goto-char (point-min))
	(search-forward mail-header-separator)
	(beginning-of-line nil)
	(insert "X-Face:")
	(insert-file-contents mail-x-face-file))))
;;
(add-hook 'mail-setup-hook 'mail-insert-x-face)

(if (elisp-file-in-loadpath-p "x-face-e21")
    (progn
      (load-library "x-face-e21")
      ;; (lambda nil (memq major-mode '(message-mode wl-draft-mode)))
      (eval-when-compile
	(defvar x-face-auto-image))
      (setq x-face-auto-image t)))

;; always a good thing!
;;
;; hmmmm... at the top of mailabbrev.el it says to use mailabbrev do:
;;
;;	(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
;;
;; however in sendmail.el it suggests (properly?) that `mail-abbrevs-setup' is
;; an appropriate option for `mail-setup-hook'
;;
(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

;; it would be nice to have mail-abbrev-next-header and maybe
;; mail-abbrev-goto-mail-text and mail-abbrev-comma too....
;; mail-abbrev-end-of-buffer is useless with signature files.

(defun my-mail-abbrevs-fix-next-line ()
  "Substitute the mail-abbrev-next-line for next-line in mail-mode-map wherever
the same key is used in global-map."
  (substitute-key-definition 'next-line 'mail-abbrev-next-line
			     mail-mode-map global-map))
(add-hook 'mail-setup-hook 'my-mail-abbrevs-fix-next-line)

(require 'mailabbrev)
(defun mail-abbrev-expand-hook ()
  "The default definition of this function tries to do magic for auto-fill-mode
but it's seriously brain damaged so we re-define it as nothing."
  t)

;;;
;;; mail aliases
;;;
;;; Note:  the `phrase' token is defined as consisting one or more `word's, and
;;; they are either `atom's or `quoted-string's:
;;;
;;; specials        =       "(" / ")" /     ; Special characters used in
;;;                         "<" / ">" /     ;  other parts of the syntax
;;;                         "[" / "]" /
;;;                         ":" / ";" /
;;;                         "@" / "\" /
;;;                         "," / "." /
;;;                         DQUOTE
;;; 
;;; atext           =       ALPHA / DIGIT / ; Any character except controls,
;;;                         "!" / "#" /     ;  SP, and specials.
;;;                         "$" / "%" /     ;  Used for atoms
;;;                         "&" / "'" /
;;;                         "*" / "+" /
;;;                         "-" / "/" /
;;;                         "=" / "?" /
;;;                         "^" / "_" /
;;;                         "`" / "{" /
;;;                         "|" / "}" /
;;;                         "~"
;;;
;;; atom            =       [CFWS] 1*atext [CFWS]
;;;
;;;
;;; However note in particular that `dot-atom' tokens are _NOT_ allowed in a
;;; `phrase':
;;;
;;; dot-atom        =       [CFWS] dot-atom-text [CFWS]
;;; 
;;; dot-atom-text   =       1*atext *("." 1*atext)
;;; 

;; aliases for managing identity in from and reply-to headers
;;
(define-mail-abbrev "me"
  "\"Greg A. Woods\" <woods@weird.com>")
(define-mail-abbrev "me-at-weird.ca"
  "\"Greg A. Woods\" <woods@weird.ca>")
(define-mail-abbrev "me-at-weird.toronto.on.ca"
  "\"Greg A. Woods\" <woods@weird.toronto.on.ca>")
(define-mail-abbrev "robohack"
  "\"Greg A. Woods\" <woods@robohack.ca>")
(define-mail-abbrev "me-at-aci"
  "\"Greg A. Woods\" <woods@aci.on.ca>")
(define-mail-abbrev "woods"
  "\"Greg A. Woods; Planix, Inc.\" <woods@planix.com>")
(define-mail-abbrev "me-at-planix"
  "\"Greg A. Woods; Planix, Inc.\" <woods@planix.com>")
(define-mail-abbrev "me-at-planix.ca"
  "\"Greg A. Woods; Planix, Inc.\" <woods@planix.ca>")
(define-mail-abbrev "me-at-planix.net"
  "\"Greg A. Woods; Planix, Inc.\" <woods@planix.net>")
(define-mail-abbrev "postmaster"
  "The Weird PostMaster <postmaster@weird.com>")
(define-mail-abbrev "postmaster-weird"
  "The Weird PostMaster <postmaster@weird.com>")
(define-mail-abbrev "postmaster-weird.ca"
  "The Weird Canadian PostMaster <postmaster@weird.ca>")
(define-mail-abbrev "postmaster-weird.toronto.on.ca"
  "The Weird Toronto PostMaster <postmaster@weird.toronto.on.ca>")
(define-mail-abbrev "postmaster-robohack"
  "The RoboHack PostMaster <postmaster@robohack.ca>")
(define-mail-abbrev "postmaster-planix"
  "The RoboHack PostMaster <postmaster@planix.com>")
(define-mail-abbrev "postmaster-planix.ca"
  "The RoboHack PostMaster <postmaster@planix.ca>")
(define-mail-abbrev "postmaster-planix.net"
  "The RoboHack PostMaster <postmaster@planix.net>")
(define-mail-abbrev "postmaster-robohack.planix"
  "The Planix RoboHack PostMaster <postmaster@robohack.planix.com>")
(define-mail-abbrev "postmaster-robohack.planix.ca"
  "The Planix Canadian RoboHack PostMaster <postmaster@robohack.planix.ca>")
(define-mail-abbrev "postmaster-robohack.planix.net"
  "The Planix Network RoboHack PostMaster <postmaster@robohack.planix.net>")
(define-mail-abbrev "woods-host"
  "\"Greg A. Woods\" <woods-host@planix.com> (host support alias)")

;; aliases for mailing lists
;;
(define-mail-abbrev "aegis-users"
  "Aegis User's Mailing List <aegis-users@canb.auug.org.au>")
(define-mail-abbrev "amanda-hackers"
  "Amanda Hackers Mailing List <amanda-hackers@amanda.org>")
(define-mail-abbrev "amanda-users"
  "Amanda User's Mailing List <amanda-users@amanda.org>")
(define-mail-abbrev "autoconf"
  "GNU AUtoconf Mailing List <autoconf@gnu.org>")
(define-mail-abbrev "automake"
  "GNU Automake Mailing List <automake@gnu.org>")
(define-mail-abbrev "bind-workers"
  "BIND Workers Mailing List <bind-workers@isc.org>")
(define-mail-abbrev "bitkeeper-users"
  "BitKeeper Discussion List <bitkeeper-users@bitmover.com>")
(define-mail-abbrev "bug-cvs"
  "CVS-II Bugs Mailing List <bug-cvs@gnu.org>")
(define-mail-abbrev "bug-gnu-emacs"
  "GNU Emacs Bugs Mailing List <bug-gnu-emacs@gnu.org>")
(define-mail-abbrev "bug-vm"
  "ViewMail Bugs Mailing List <bug-vm@UUnet.UU.net>")
(define-mail-abbrev "bugtraq"
  "\"BUGTRAQ: Full Disclosure Security Mailing List\" <bugtraq@SecurityFocus.com>")
(define-mail-abbrev "conserver"
  "ConServer User's Mailing List <users@conserver.com>")
(define-mail-abbrev "conserver-users"
  "ConServer User's Mailing List <users@conserver.com>")
(define-mail-abbrev "cricket-users"
  "Cricket User's Mailing List <cricket-users@lists.sourceforge.net>")
(define-mail-abbrev "csas-incidents"
  "CSAS Incidents List <incidents@csas.com>")
(define-mail-abbrev "cssc-users"
  "GNU CSSC Discussion List <cssc-users@gnu.org>")
(define-mail-abbrev "current-users"
  "NetBSD-current Discussion List <current-users@NetBSD.ORG>")
(define-mail-abbrev "cyrus-bugs"
  "Cyrus Bugs List <cyrus-bugs@andrew.cmu.edu>")
(define-mail-abbrev "cyrus-devel"
  "Cyrus Developer's List <cyrus-devel@lists.andrew.cmu.edu>")
(define-mail-abbrev "cyrus-users"
  "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
(define-mail-abbrev "info-cyrus"
  "Cyrus User's Mailing List <info-cyrus@lists.andrew.cmu.edu>")
(define-mail-abbrev "datacenter"
  "Data Centre Discussion List <datacenter@shorty.com>")
(define-mail-abbrev "datacentre"
  "Data Centre Discussion List <datacenter@shorty.com>")
(define-mail-abbrev "devel-cvs"
  "CVS Developer's Mailing List <devel-cvs@cyclic.com>")
(define-mail-abbrev "emacs-pretest-bug"
  "GNU Emacs Pre-Test Bugs Mailing List <emacs-pretest-bug@gnu.org>")
(define-mail-abbrev "exim-users"
  "Exim User's Mailing List <exim-users@exim.org>")
(define-mail-abbrev "geeks"
  "Sun Geeks List <geeks@sunhelp.org>")
(define-mail-abbrev "gnats-bugs"
  "NetBSD GNATS submissions and followups <gnats-bugs@gnats.netbsd.org>")
(define-mail-abbrev "gnu-emacs-sources"
  "GNU Emacs Sources Mailing List <gnu-emacs-sources@gnu.org>")
(define-mail-abbrev "gnu-emacs-pretest"
  "GNU Emacs Pre-Test Bugs Mailing List <emacs-pretest-bug@gnu.org>")
(define-mail-abbrev "gnu-emacs-pretest-bug"
  "GNU Emacs Pre-Test Bugs Mailing List <emacs-pretest-bug@gnu.org>")
(define-mail-abbrev "info-cvs"
  "CVS-II Discussion Mailing List <info-cvs@gnu.org>")
(define-mail-abbrev "info-vm"
  "ViewMail Info Mailing List <info-vm@UUnet.UU.net>")
(define-mail-abbrev "ipfilter"
  "IP-Filter Mailing List <ipfilter@postbox.anu.edu.au>")
(define-mail-abbrev "ip-filter"
  "IP-Filter Mailing List <ipfilter@postbox.anu.edu.au>")
(define-mail-abbrev "lout"
  "Old Lout Mailing List <lout@ptc.spbu.ru>")
(define-mail-abbrev "lout-users"
  "Lout Users Mailing List <lout-users@lists.planix.com>")
(define-mail-abbrev "mush-users"
  "Mush User's Mailing List <mush-users-request@garp.mit.edu>")
(define-mail-abbrev "nanog"
  "North America Network Operators Group Mailing List <nanog@merit.edu>")
(define-mail-abbrev "netbsd-bugs"
  "NetBSD Bugs and PR posting List <netbsd-bugs@NetBSD.ORG>")
(define-mail-abbrev "netbsd-gnats"
  "NetBSD GNATS submissions and followups <gnats-bugs@gnats.netbsd.org>")
(define-mail-abbrev "netbsd-current"
  "NetBSD-current Discussion List <current-users@NetBSD.ORG>")
(define-mail-abbrev "netbsd-help"
  "NetBSD Questions List <netbsd-help@NetBSD.ORG>")
(define-mail-abbrev "netbsd-ports"
  "NetBSD Ports Discussion List <netbsd-ports@NetBSD.ORG>")
(define-mail-abbrev "netbsd-source-changes"
  "NetBSD CVS Logs <source-changes@NetBSD.ORG>")
(define-mail-abbrev "netbsd-users"
  "NetBSD User's Discussion List <netbsd-users@NetBSD.ORG>")
(define-mail-abbrev "port-alpha"
  "NetBSD/alpha Discussion List <port-alpha@NetBSD.ORG>")
(define-mail-abbrev "port-i386"
  "NetBSD/i386 Discussion List <port-i386@NetBSD.ORG>")
(define-mail-abbrev "port-ofppc"
  "NetBSD/pmax Discussion List <port-ofppc@NetBSD.ORG>")
(define-mail-abbrev "port-pmax"
  "NetBSD/pmax Discussion List <port-pmax@NetBSD.ORG>")
(define-mail-abbrev "port-sparc"
  "NetBSD/sparc Discussion List <port-sparc@NetBSD.ORG>")
(define-mail-abbrev "port-sun3"
  "NetBSD/sun3 Discussion List <port-sun3@NetBSD.ORG>")
(define-mail-abbrev "port-vax"
  "NetBSD/vax Discussion List <port-vax@NetBSD.ORG>")
(define-mail-abbrev "postfix-testers"
  "Postfix Tester's Mailing List <postfix-testers@postfix.org>")
(define-mail-abbrev "postfix-users"
  "Postfix User's Mailing List <postfix-users@postfix.org>")
(define-mail-abbrev "qotd"
  "Quotation of the Day List <quotationoftheday@yahoo.ca>")
(define-mail-abbrev "rescue"
  "Sun Rescue List <rescue@sunhelp.org>")
(define-mail-abbrev "secureshell"
  "SSH User's Mailing List <secureshell@securityfocus.com>")
(define-mail-abbrev "shape-l"
  "ShapeTools Discussion Mailing List <SHAPE-L%DB0TUI11.BITNET@vm.gmd.de>")
(define-mail-abbrev "smail-bugs"
  "Smail PR Reports and followups <smail-bugs@planix.com>")
(define-mail-abbrev "smail3-devel"
  "Smail-3 Developers Mailing List <smail3-devel@weird.com>")
(define-mail-abbrev "smail3-wizards"
  "Smail-3 Wizards Mailing List <smail3-wizards@athabascau.ca>")
(define-mail-abbrev "smail3-users"
  "Smail-3 User's Mailing List <smail3-users@athabascau.ca>")
(define-mail-abbrev "spamtools"
  "Spam Tools Mailing List <spamtools@abuse.net>")
(define-mail-abbrev "squeak"
  "Squeak Mailing List <squeak@cs.uiuc.edu>")
(define-mail-abbrev "sungeeks"
  "Sun Geeks List <geeks@sunhelp.org>")
(define-mail-abbrev "sun-geeks"
  "Sun Geeks List <geeks@sunhelp.org>")
(define-mail-abbrev "sunrescue"
  "Sun Rescue List <rescue@sunhelp.org>")
(define-mail-abbrev "sun-rescue"
  "Sun Rescue List <rescue@sunhelp.org>")
(define-mail-abbrev "suns-at-home"
  "Suns-at-Home Mailing List <suns-at-home@net-kitchen.com>")
(define-mail-abbrev "tech-embed"
  "NetBSD for Embedded Systems Technical Discussion List <tech-embed@NetBSD.ORG>")
(define-mail-abbrev "tech-install"
  "NetBSD Install Process Technical Discussion List <tech-install@NetBSD.ORG>")
(define-mail-abbrev "tech-kern"
  "NetBSD Kernel Technical Discussion List <tech-kern@NetBSD.ORG>")
(define-mail-abbrev "tech-misc"
  "NetBSD Miscellaneous Technical Discussion List <tech-misc@NetBSD.ORG>")
(define-mail-abbrev "tech-net"
  "NetBSD Networking Technical Discussion List <tech-net@NetBSD.ORG>")
(define-mail-abbrev "tech-perform"
  "NetBSD Performance Technical Discussion List <tech-perform@NetBSD.ORG>")
(define-mail-abbrev "tech-pkg"
  "NetBSD Packages Technical Discussion List <tech-pkg@NetBSD.ORG>")
(define-mail-abbrev "tech-ports"
  "NetBSD Porting Technical Discussion List <tech-ports@NetBSD.ORG>")
(define-mail-abbrev "tech-security"
  "NetBSD Security Technical Discussion List <tech-security@NetBSD.ORG>")
(define-mail-abbrev "tech-smp"
  "NetBSD SMP Technical Discussion List <tech-smp@NetBSD.ORG>")
(define-mail-abbrev "tech-toolchain"
  "NetBSD Toolchain Technical Discussion List <tech-toolchain@NetBSD.ORG>")
(define-mail-abbrev "tech-userlevel"
  "NetBSD Userlevel Technical Discussion List <tech-userlevel@NetBSD.ORG>")
(define-mail-abbrev "tech-x11"
  "NetBSD X11 Technical Discussion List <tech-x11@NetBSD.ORG>")
(define-mail-abbrev "tkined"
  "Scotty Mailing List <tkined@ibr.cs.tu-bs.de>")
(define-mail-abbrev "tpc-rp"
  "TPC-Int <tpc-rp@aarnet.edu.au>")
(define-mail-abbrev "trn-test"
  "TRN Beta Test List <trn-test@borland.com>")
(define-mail-abbrev "unix-virus"
  "Unix Virus Mailing List <unix-virus@virus.beergrave.net>")
(define-mail-abbrev "vmailer-testers"
  "VMailer Testers List <vmailer-testers@porcupine.org>")

;; local mailing list aliases
;;
(define-mail-abbrev "jet-fuel"
  "Caffeine is fuel for your mind <jet-fuel@weird.com>")

;; ACI aliases
;;
(define-mail-abbrev "aci-abuse"
  "ACI Abuse Contact <abuse@aci.on.ca>")
(define-mail-abbrev "abuse-aci"
  "ACI Abuse Contact <abuse@aci.on.ca>")

(define-mail-abbrev "aci-postmaster"
  "ACI PostMaster <postmaster@aci.on.ca>")
(define-mail-abbrev "postmaster-aci"
  "ACI PostMaster <postmaster@aci.on.ca>")

(define-mail-abbrev "aci-hostmaster"
  "ACI HostMaster <hostmaster@aci.on.ca>")
(define-mail-abbrev "hostmaster-aci"
  "ACI HostMaster <hostmaster@aci.on.ca>")

(define-mail-abbrev "aci-support"
  "ACI Technical Support <support@aci.on.ca>")
(define-mail-abbrev "support-aci"
  "ACI Technical Support <support@aci.on.ca>")

(define-mail-abbrev "aci-netadmin"
  "ACI Network Administrator <netadmin@aci.on.ca>")
(define-mail-abbrev "netadmin-aci"
  "ACI Network Administrator <netadmin@aci.on.ca>")

(define-mail-abbrev "aci-sysadmin"
  "ACI Systems Administrator <sysadmin@aci.on.ca>")
(define-mail-abbrev "sysadmin-aci"
  "ACI Systems Administrator <sysadmin@aci.on.ca>")

(define-mail-abbrev "aci-uucp"
  "ACI UUCP Administrator <uucp@admin.aci.on.ca>")
(define-mail-abbrev "uucp-aci"
  "ACI UUCP Administrator <uucp@admin.aci.on.ca>")

(define-mail-abbrev "csas-incidents"
  "CSAS Incidents List <incidents@csas.com>")

;; friendly aliases
;;
(define-mail-abbrev "adb"
  "Anthony DeBoer <adb@onramp.ca>")
(define-mail-abbrev "alex"
  "Alex von Tiesenhausen <alex@corelan.com>")
(define-mail-abbrev "sandu"
  "Alexandru Sburlan <sandu@mail.on.rogers.wave.ca>")
(define-mail-abbrev "andreas"
  "Andreas Wrede <andreas@planix.com>")
(define-mail-abbrev "andy"
  "Andy Mills <andy@weird.com>")
(define-mail-abbrev "andy-work"
  "Andy Mills <andy@nationalfibre.net> (work)")
(define-mail-abbrev "aunt-linda"
  "Barry & Linda Edge <bledge@telusplanet.net>")
(define-mail-abbrev "bdb"
  "Bruce Becker <bdb@gts.net>")
(define-mail-abbrev "bruce"
  "Bruce Becker <bdb@gts.net>")
(define-mail-abbrev "bernie"
  "Bernard Becker <becker@proxy.net>")
(define-mail-abbrev "blatte"
  "Blattidae Cucaracha <blatte@cock.roach.org>")
(define-mail-abbrev "bryan"
  "Bryan Challenger <bryan@aci.on.ca>")
(define-mail-abbrev "cousin-linda"
  "Gordon & Linda Fisk <circlef@sk.sympatico.ca>") ; also <circle.f@sk.sympatico.ca> ?
(define-mail-abbrev "cousin-tim"
  "Tim Edge <EDGEIF22@aol.com>")
(define-mail-abbrev "dan"
  "Dan Tomlinson <dan@compus.ca>")
(define-mail-abbrev "darcy"
  "\"D'Arcy J.M. Cain\" <darcy@druid.net>")
(define-mail-abbrev "dave"
  "Dave Mason <mason@tmsoftware.ca>")
(define-mail-abbrev "david"
  "David Maxwell <david@maxwell.net>")
(define-mail-abbrev "dennis"
  "Dennis Breckenridge VE6TCP <dennis@nebulus.net>")
(define-mail-abbrev "drek"
  "Agent Drek <drek@smashpow.net>")
(define-mail-abbrev "dtw"
  "Deirdre Taylor-Wright <dtw@rom.on.ca>")
(define-mail-abbrev "dumais"
  "\"Paul E. Dumais\" <dumais@mad.scientist.com>")
(define-mail-abbrev "dwm"
  "David Maxwell <david@vex.net>")
(define-mail-abbrev "ecarroll"
  "\"Eric M. Carroll\" <ecarroll@rogers.wave.ca>")
(define-mail-abbrev "eric.carroll"
  "\"Eric M. Carroll\" <eric.carroll@acm.org>")
(define-mail-abbrev "evan"
  "Evan Leibovitch <evan@telly.ca>")
(define-mail-abbrev "georgn"
  "Georg Nikodym <georgn@somanetworks.com>")
(define-mail-abbrev "gil"
  "Gil Hauer <gilh@somanetworks.com>")
(define-mail-abbrev "grant"
  "Grant Officer <grant@slick.net>")
(define-mail-abbrev "heison"
  "Heison Chak <heison@somanetworks.com>")
(define-mail-abbrev "henry"
  "Henry Spencer <henry@utzoo.cs.utoronto.ca>")
(define-mail-abbrev "henryp"
  "Henry Potgieter <henryp@aci.on.ca>")
(define-mail-abbrev "hugh"
  "\"Hugh D. Gamble\" <hugh@phaedrav.com>")
(define-mail-abbrev "hughg"
  "\"Hugh D. Gamble\" <hugh@phaedrav.com>")
(define-mail-abbrev "hughr"
  "Hugh Redelmeier <hugh@mimosa.com>")
(define-mail-abbrev "jeff"
  "Jeff Royle <jeff@aci.on.ca>")
(define-mail-abbrev "jen"
  "Jennifer Wrede <jennifer@wrede.ca>")
(define-mail-abbrev "jenny"
  "Jennifer Wrede <jennifer@wrede.ca>")
(define-mail-abbrev "jennifer"
  "Jennifer Wrede <jennifer@wrede.ca>")
(define-mail-abbrev "jenny"
  "Jennifer Wrede <jennifer@wrede.ca>")
(define-mail-abbrev "jerqs"
  "jerq users <jerqs@clsc.utoronto.ca>")
(define-mail-abbrev "jim"
  "Jim Mercer <jim@reptiles.org>")
(define-mail-abbrev "jmm"
  "John Macdonald <jmm@Elegant.COM>")
(define-mail-abbrev "strange"
  "\"Kristofer P. Cox\" <strange@strange.com>")
(define-mail-abbrev "kris"
  "Kris Cox (not at corelan!) <kris@corelan.com>")
(define-mail-abbrev "kyle"
  "Kyle Jones <kyle_jones@wonderworks.com>")
(define-mail-abbrev "lcis"
  "Library of Computer and Information Sciences <professionals@bookclubservices.com>")
(define-mail-abbrev "marc"
  "Marc Staveley <marc@somanetworks.com>")
(define-mail-abbrev "nik"
  "Nik Habermel <habern@caledon.org>")
(define-mail-abbrev "norman"
  "Norman Wilson <norman@cs.yorku.ca>")
(define-mail-abbrev "paul"
  "\"Paul E. Dumais\" <dumais@mad.scientist.com>")
(define-mail-abbrev "ped"
  "\"Paul E. Dumais\" <dumais@moosefactory.com>")
(define-mail-abbrev "peter"
  "Peter Renzland <peter@renzland.org>")
(define-mail-abbrev "peterm"
  "\"Peter G. Marshall\" <peterm@openphase.com>")
(define-mail-abbrev "planix"
  "\"Planix, Inc.\" <planix@planix.com>")
(define-mail-abbrev "postmaster-proxy"
  "PROXY Postmaster <postmaster@proxy.net>")
(define-mail-abbrev "proxy-postmaster"
  "PROXY Postmaster <postmaster@proxy.net>")
(define-mail-abbrev "rayan"
  "Rayan Zachariassen <rayan@uunet.ca>")
(define-mail-abbrev "reg"
  "Reg Coppicus <regcoppicus@westman.wave.ca>")
(define-mail-abbrev "rhl"
  "\"R. H. Lathwell\" <rhl@the-wire.com>")
(define-mail-abbrev "rob"
  "Rob Ellis <rob@web.ca>")
(define-mail-abbrev "stever"
  "Steve Rapaport <steve@petabit.com>")
(define-mail-abbrev "timo"
  "Timo Janhunen <timo@aci.on.ca>")
(define-mail-abbrev "all-planix"
  "Those Planix Dudes <all@planix.com>")
(define-mail-abbrev "planix-all"
  "Those Planix Dudes <all@planix.com>")
(define-mail-abbrev "ops"
  "Weird NOC <ops@weird.com>")
(define-mail-abbrev "wiznet"
  "Wiznet Support <support@wiznet.ca>")

(define-mail-abbrev "dduffey"
  "dduffey@slb.com (sandra's friend sending baby pictures)")

;; aliases for commonly used addresses when sending spam complaints
;;
(define-mail-abbrev "easynet"	"abuse-silent@easynet.nl (defunct)")
(define-mail-abbrev "orbz"	"spam@orbz.org (defunct)")
(define-mail-abbrev "osir"	"relays@relays.osirusoft.com (defunct)")
(define-mail-abbrev "osirusoft"	"relays@relays.osirusoft.com (defunct)")
(define-mail-abbrev "spamcop"	"spam@cmds.spamcop.net")
(define-mail-abbrev "old-spamcop" "spamcop@spamcop.net (defunct)")
(define-mail-abbrev "wirehub"	"abuse-silent@wirehub.net (defunct)")

;; If t, it means to insert the contents of the file `mail-signature-file'.
;; If a string, that string is inserted.
;;
;; Otherwise, it should be an expression; which will be evaluated by
;; `mail-signature' and that expression should return a string containing
;; whatever text you want to insert.
;;
;; Note the expression magic is not currently documented.
;;
(defvar default-mail-signature-file mail-signature-file
  "Original value of `mail-signature-file' for safe keeping.")
(setq mail-signature t)

(defun my-mail-signature-selector ()
  "Select an appropriate signature file"
  (let ((custom-sigfile (concat mail-signature-file "-" mail-local-domain-name)))
    (if (file-exists-p custom-sigfile)
	(setq mail-signature-file custom-sigfile)
      (setq mail-signature-file default-mail-signature-file))))

(add-hook 'after-init-hook 'my-mail-signature-selector)

(eval-and-compile
  (if (not (boundp 'message-signature-separator))
      (defvar message-signature-separator  "^-- *$"
	"Regexp matching the signature separator.")))

;; more fancy .sig handling adapted from:
;; <URL:http://www-xray.ast.cam.ac.uk/~gmorris/dotvm.txt>
;;
(defun my-mail-goto-signature ()
  "Move point to the signature separator, if present.  Otherwise go to the end
and return nil."
  (goto-char (point-max))
  (re-search-backward message-signature-separator (mail-text-start) t))

(defun my-mail-signature-start ()
  "Return start position of signature in current buffer, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (and (re-search-backward message-signature-separator nil t)
         (point))))

(defface my-signature-face
  '((((class color)
      (background dark))
     (:foreground "light pink"))
    (((class color)
      (background light))
     (:background "light pink"))
    (t
     (:italic t)))
  "My signature face"
  :group 'basic-faces)

(defun my-signature-highlight ()
  "Highlight the signature in an article with `my-signature-face'."
  (interactive)
  (save-excursion
    (let ((start (my-mail-signature-start)))
      (and start
           (facep 'my-signature-face)
           (overlay-put (make-overlay start (point-max))
                        'face 'my-signature-face)))))

(defun my-signature-insert (sig &optional replace highlight)
  "Insert SIG as signature at end of current buffer, unless a signature is
already present, in which case REPLACE must be supplied.  If a file matching
string SIG exists, its contents will be inserted, otherwise SIG is treated as a
literal string. With HIGHLIGHT, highlight the new signature."
  (save-excursion
    (let ((sigsep "\n-- \n")	; should match message-signature-separator
	  (exists (my-mail-goto-signature))) ; moves point
      (when (and exists replace)
	(delete-region (point) (point-max))
	(delete-char -1))
      (when (or (not exists) (and exists replace))
	(insert sigsep)
	(if (file-exists-p sig) (insert-file-contents sig)
	  (insert sig))
	(and highlight (my-signature-highlight))))))

(defun my-signature-kill ()
  "Remove signature in current buffer."
  (interactive)
  (save-excursion
    (and (my-mail-goto-signature) (delete-region (point) (point-max)))))

(defvar my-randomsig-file "~/quotes"
  "")
(defvar my-randomsig-delimiter-pattern "\n\n"
  "")
(defvar my-randomsig-comment-pattern "#c#"
  "")
(defvar my-randomsig-sigflag-pattern ""	; "#s#"
  "")

(defun my-get-random-quote (&optional include-commented lines sigflag)
  "Return a random quote from `my-randomsig-file'.
If INCLUDE-COMMENTED is non-nil, include private quotes.
If LINES is non-nil, do not return a quote with more than LINES lines.
If SIG is non-nil, only return quotes marked as sig-suitable, ignoring
any line-limit."
  (interactive)
  (or (file-exists-p my-randomsig-file)
      (error "File `%s' not found" my-randomsig-file))
    (with-temp-buffer
      (insert-file-contents my-randomsig-file)
      (goto-char (point-min))
      (let ((sigcount 0)
            (try 0)
            (maxtry 100)
            found sigstring selected)
	;; Count number of signatures.
	(while (re-search-forward my-randomsig-delimiter-pattern nil t)
	  (setq sigcount (1+ sigcount)))
	;; search for a suitable signature...
        (while (not sigstring)
          (while (and (not found) (< try maxtry))
            ;; Select random signature.
            (setq try (1+ try))
	    (setq selected (1+ (random sigcount)))
            (goto-char (point-min))
            ;; Note this expects a separator before first quote.
            (or (re-search-forward my-randomsig-delimiter-pattern
                                   (point-max) t selected)
                (error "Failed to find selected quote"))
            (setq found (if (looking-at my-randomsig-comment-pattern)
			    include-commented
			  t))
            ;; If requested, reject those not explicitly marked as sigs.
            ;; Ugly.
            (and found
                 (setq found
		       (or (not sigflag) 
			   (re-search-forward 
			    my-randomsig-sigflag-pattern 
			    (line-end-position) t)))))
          (or found (error "Tried %s quotes and failed" maxtry))
          ;; Cut signature and return it.
          (let ((start (point))
                (end (save-excursion
                       (and (re-search-forward
                             my-randomsig-delimiter-pattern (point-max) 'move)
                            (beginning-of-line))
                       (point)))
                thislines)
            ;; Move to end, or comment section.
            (and (re-search-forward my-randomsig-comment-pattern end 'move)
                 (beginning-of-line))
            (setq end (point)
                  sigstring (buffer-substring start end)
                  thislines (count-lines start end))
            ;; Reject signature if too long.
            (and lines (not sigflag) (> thislines lines)
                 (setq found nil sigstring nil))))
        (if (interactive-p) (message "%s" sigstring)
          sigstring))))

;; This is bogus, but serves as an example.
;;
;; what we really want is just to replace the quotation part, not the whole .sig
;;
(defun my-sig-replace-random-and-highlight (&optional prefix)
  "Insert and highlight random signature, from those marked OK.
With PREFIX, select from all quotes."
  (interactive "*P")			; suitable for keybinding
  (my-signature-insert (my-get-random-quote prefix nil nil) t t))

;;;;
;;;; maybe someday some smtpmail.el stuff....
;;;;

;;; Message-ID: <u7ybihq0jg.fsf@wmperry.oz.net>
;;; X-Face: O~Rn;(l][/-o1sALg4A@xpE:9-"'IR[%;,,!m7</SYF`{vYQ(&RI1&EiH[FvT;J}@f!4kfz
;;;  x_!Y#=y{Uuj9GvUi=cPuajQ(Z42R[wE@{G,sn$qGr5g/wnb*"*ktI+,CD}1Z'wxrM2ag-r0p5I6\nA
;;;  [WJopW_J.WY;
;;; From: William Perry <wmperry@aventail.com>
;;; To: info-vm@uunet.uu.net
;;; Date: 10 Sep 1996 18:54:11 -0700
;;; Subject: Re: configuring SMPT server
;;;
;(if (elisp-file-in-loadpath-p "smtpmail")
;    (progn
;      (require 'smtpmail)
;      (setq send-mail-function 'smtpmail-send-it)
;      (setq smtpmail-default-smtp-server (concat "mail" mail-local-domain-name))
;      (setq smtpmail-smtp-service "smtp")
;      (setq smtpmail-local-domain mail-local-domain-name)))

;;;
;;; GNUS specific stuff
;;;

(eval-when-compile
  (defvar message-from-style)
  (defvar message-sendmail-f-is-evil)
  (defvar message-generate-headers-first)
  (defvar message-interactive)
  (defvar message-user-organization)
  (defvar gnus-read-active-file))

(setq message-from-style 'angles)	; true RFC-[2]822
(setq message-sendmail-f-is-evil t)	; indeed it is!
(setq message-generate-headers-first t)	; seems much nicer

;; With Smail's content filtering we will get an error if we try to post
;; something that's forbidden -- however the current stupid
;; `message-send-mail-with-sendmail' ignores errors from the mailer unless
;; `message-interactive' is set.
;;
;; Unfortunately interactive delivery will appear to fail if the mailer returns
;; EX_TEMPFAIL even though the message may have been successfully queued for
;; later delivery.  Arguably the mailer shouldn't return EX_TEMPFAIL in this
;; situation, but some do (eg. some versions Smail).
;;
;; Unfortunately there's not even any other way to specify other custom options
;; to `sendmail-program' or else we could just add our own "-odb -oem" without
;; having to turn off `message-interactive'.
;;
(setq message-interactive t)

(setq message-user-organization t)
(if (file-exists-p "~/.organization")
    (setq message-user-organization-file "~/.organization"))

(setq gnus-read-active-file t)		; default of 'some causes it to hang


;;;;-------
;;;; the closing comments.....

;;; From: nickel@cs.tu-berlin.de (Juergen Nickelsen)
;;; Newsgroups: gnu.emacs.help,comp.emacs
;;; Subject: Re: model for .emacs file
;;; Date: 18 Jan 1993 18:48:18 GMT
;;; Organization: STONE Project, Technical University of Berlin, Germany
;;; Message-ID: <NICKEL.93Jan18194816@tempest.cs.tu-berlin.de>
;;;
;;;; Once upon a time this used write-file-hooks and the function looked
;;;; like this, but since I use SCCS vc-checkin-hook made more sense:
;;;;
;;;;(defun byte-compile-this-file ()
;;;;  (write-region (point-min) (point-max) buffer-file-name nil 't)
;;;;  (byte-compile-file buffer-file-name)
;;;;  nil)
;;;

;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (byte-compile-file buffer-file-name) nil)
;;; vc-checkin-hook: (byte-compile-this-file)
;;; End:
