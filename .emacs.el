;;;;
;;;;	.emacs.el
;;;;
;;;;#ident	"@(#)HOME:.emacs.el	15.1	94/12/20 14:38:46 (woods)"
;;;;
;;;; per-user start-up functions for GNU-emacs v19 only
;;;;

;;; to debug, eval (^X^E) these after "emacs -q":
;;;
;;; (setq debug-on-error t)
;;; (load-file "~/.emacs.el")
;;;
;;; more goodies for debug:
;;;
;;; (setq stack-trace-on-error t)
;;; (setq debug-on-quit t)
;;; (setq debug-on-error nil)

;; I don't want that annoying startup message.
(setq inhibit-startup-message t)

;;;; ----------
;;;; stolen from cl.el -- find out where we are!

(defvar init-emacs-type (cond ((or (and (fboundp 'epoch::version)
					(symbol-value 'epoch::version))
				   (string-lessp emacs-version "19")) 18)
			      ((string-match "Lucid" emacs-version) 'lucid)
			      (t 19)))

(if (/= init-emacs-type '19)
    (message "Not running emacs v19 I see -- you'll have trouble with this .emacs!"))

;;;_  - add-hook definition for divergent emacsen
;;;_   > add-hook (hook function &optional append)
(load-library "subr")
(if (not (fboundp 'run-hooks))
    ;; Hook manipulation functions. (borrowed from newer versions of subr.el)
    (progn
      (defun run-hooks (&rest hooklist)
	"Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments."
	(while hooklist
	  (let ((sym (car hooklist)))
	    (and (boundp sym)
		 (symbol-value sym)
		 (let ((value (symbol-value sym)))
		   (if (and (listp value) (not (eq (car value) 'lambda)))
		       (let ((functions value))
			 (while value
			   (funcall (car value))
			   (setq value (cdr value))))
		     (funcall value)))))
	  (setq hooklist (cdr hooklist))))

      ;; Tell C code how to call this function.
      (defconst run-hooks 'run-hooks
	"Variable by which C primitives find the function `run-hooks'.
Don't change it.")))

(if (not (fboundp 'run-hook-with-args))
    (defun run-hook-with-args (hook &rest args)
      "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value return by `run-hook-with-args',
as that may change."
      (and (boundp hook)
	   (symbol-value hook)
	   (let ((value (symbol-value hook)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (mapcar '(lambda (foo) (apply foo args))
			 value)
	       (apply value args))))))

(if (not (fboundp 'add-hook))
    (defun add-hook (hook function &optional append)
      "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
      (or (boundp hook) (set hook nil))
      ;; If the hook value is a single function, turn it into a list.
      (let ((old (symbol-value hook)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (set hook (list old))))
      (or (if (consp function)
	      (member function (symbol-value hook))
	    (memq function (symbol-value hook)))
	  (set hook
	       (if append
		   (nconc (symbol-value hook) (list function))
		 (cons function (symbol-value hook)))))))

(if (not (fboundp 'remove-hook))
    (defun remove-hook (hook function)
      "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'."
      (if (or (not (boundp hook))	;unbound symbol, or
	      (null (symbol-value hook)) ;value is nil, or
	      (null function))		;function is nil, then
	  nil				;Do nothing.
	(let ((hook-value (symbol-value hook)))
	  (if (consp hook-value)
	      (setq hook-value (delete function hook-value))
	    (if (equal hook-value function)
		(setq hook-value nil)))
	  (set hook hook-value)))))

;;;; ----------
;;;; What to do after this file has been loaded...
;; to quiet the v19 byte compiler
(defvar display-time-24hr-format)
(defvar display-time-interval)
(add-hook 'after-init-hook
	  (function
	   (lambda ()
	     "Functions to call after loading the init file (`~/.emacs').
The call is not protected by a condition-case, so you can set `debug-on-error'
in `.emacs', and put all the actual code on `after-init-hook'."
,	     (progn
	       ;; (require 'time)	; this isn't provided by time.el!
	       (setq display-time-day-and-date t) ; autoload'ed though
	       (setq display-time-24hr-format t)
	       (if (or (string-equal (system-name) "robohack")
		       (string-equal (system-name) "web"))
		   (setq display-time-interval 300)) ; poor little machines....
	       (display-time)))))	; also autoload'ed

;;;; ----------
;;;; get ready to load stuff

(defvar local-gnu-path (cond
		      ((getenv "GNU")
		       (getenv "GNU"))
		      ((getenv "LOCAL")
		       (concat (getenv "LOCAL") "/gnu"))
		      (t
		       "/gnu")))

(defvar original-load-path load-path)
(setq load-path (cons (expand-file-name "~/lib/elisp") load-path))

;;; This could probably be rewritten to use mapcar
(defun elisp-file-in-loadpath-p (file-name)
  "Returns t if there is an emacs lisp-library of the name FILENAME in
the load-path list. Matching is first done by looking for the file
with an .elc extension, an .el extension, and finally with no
extension at all, and returning t if any of the three are found. Nil
is returned otherwise."
  (let ((extension-list (list ".elc" ".el" ""))
        (file-found-p nil)
        name-to-try)
    (while (and (not file-found-p) (not (null extension-list)))
      (setq name-to-try (concat file-name (car extension-list)))
      (setq extension-list (cdr extension-list))
      (setq file-found-p (file-in-loadpath-p name-to-try)))
    (eval 'file-found-p)))

(defun file-in-loadpath-p (file-name)
  "Returns t if the string argument FILENAME is a file name present in a
directory in the load-path list, otherwise returns nil."
  (file-in-pathlist-p file-name load-path))

(defun file-in-pathlist-p (file-name path-list)
  "Returns t if the string FILENAME is a file name which occurs in a
directory in the list PATHLIST, otherwise nil."
  (let (try-path (file-found-in-path-p nil))
    (while (not (or file-found-in-path-p (null path-list)))
      (setq try-path (car path-list)
            path-list (cdr path-list))
      (if (file-exists-p (concat try-path "/" file-name))
          (setq file-found-in-path-p t)))
    (eval 'file-found-in-path-p)))

;;;; ----------
;;;; some default packages we'd like...

;; hyperbole auto-loading
(if (elisp-file-in-loadpath-p "hyperbole")
    (progn
      (setq load-path (append load-path
			      (list (concat (car original-load-path)
					    "/hyperbole"))))
      (load-library "hyperbole")))

(if (elisp-file-in-loadpath-p "c-boxes")
    (autoload 'reindent-c-comment "c-boxes" "Function for boxing C comments." t))

(if (and (elisp-file-in-loadpath-p "func-menu")
	 window-system)
    (progn
      (require 'func-menu)
      (define-key global-map [S-down-mouse-1]
	'function-menu)))

(if (elisp-file-in-loadpath-p "shwtmpbuf")
    (progn
      (load "shwtmpbuf")
      (global-set-key "\C-xH" 'hide-temp-buffers))) ; defaults to C-x t in shwtmpbuf

(if (elisp-file-in-loadpath-p "ksh-mode")
    (autoload 'ksh-mode "ksh-mode" "Major mode for editing sh Scripts." t))

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
;;;; If running as root, don't make backup files.  This should be default!!!

(cond ((eq (user-uid) 0)
       (setq make-backup-files nil)
       (setq auto-save-default nil)))

;;;; ----------
;;;; Set defaults of other buffer-local variables

(setq-default case-fold-search nil)	; V-19 will fix this even better
(setq-default indent-tabs-mode t)	; allow tabs in indentation
(setq-default require-final-newline 1)	; needed by some unix programs

;;;; ----------
;;;; some new global variable settings...

;;; Date: Wed, 2 Feb 1994 12:49:31 GMT
;;; Message-Id: <1994Feb2.124931.19715@nessie.mcc.ac.uk>
;;; Organization: Manchester Computing Centre, Manchester, England
;;; From: ehgasm2@uts.mcc.ac.uk (Simon Marshall)
;;; Subject: Re: Quick routine to BOLDFACE directories in DIRED buffers
;;;
(if window-system
    (defvar dired-font-lock-keywords
      '(("\\S +\\([~%#]\\)$" . font-lock-doc-string-face)
	("\\S +\\.\\([oszZ]\\|elc\\|gz\\)$" . font-lock-string-face)
	("^  \\(/.+\\)$" 1 font-lock-type-face)
	("[^ ]+ -> [^ ]+$" . font-lock-function-name-face)
	("^..\\(.....w....\\|........w.\\)" 1 font-lock-comment-face)
	("^[^ ].*$" 0 font-lock-comment-face t)
	("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face))))

(setq auto-save-timeout 600)		; 30 seconds is insane!
(setq backup-by-copying t)		; copy, thus preserving modes and owner
(setq compilation-window-height 10)	; default height for a compile window
(setq default-tab-width 8)		; a tab is a tab is a tab is a tab....
(setq delete-auto-save-files t)		; delete auto-save file when saved
(setq enable-local-variables t)		; (is this the default in v19?)
(setq make-backup-files nil)		; too much clutter
(setq next-line-add-newlines nil)	; I hate it when it does that!  ;-)
(setq track-eol nil)			; too hard to control (it's sticky!)
(setq window-min-height 1)		; don't be snobbish
(setq window-min-width 1)
(setq completion-ignored-extensions
      (append '(".out")
	      completion-ignored-extensions))

(if window-system
    (setq search-highlight t))		; i-search hightlight match

(if window-system
    (setq baud-rate 153600))		; let's make things a bit more efficient

;;;; ----------
;;;; auto-mode-alist setup

(setq auto-mode-alist
      (append
       '(("/[^/]+\.vm$" . emacs-lisp-mode))	; VM customisation file
       '(("/\.vm$" . emacs-lisp-mode))		; VM init file
       '(("/[^/]+\\.[chtly].[.0-9]+$" . c-mode)) ; cvs backup file
       '(("/[^/]+\\.t$" . c-mode))		; APC "ling" file
       '(("/[^/chtly]+\\.[0-9][a-z]?$" . nroff-mode)) ; man page
       '(("/[^/]+\\.d.[.0-9]+$" . nroff-mode))	; cvs backup file
       '(("/[^/]+\\.d$" . nroff-mode))		; documentation file
       '(("/[^/]+\\.m[mes]?$" . nroff-mode))	; mm, me, ms docs
       '(("/[^/]+\\.t[imes]*$" . nroff-mode))	; as above, but with leading 't'
       '(("/[^/]*[rR][eE][aA][dD]" . indented-text-mode))
       '(("/[^/]*\\.article.*$" . indented-text-mode))
       '(("/[^/]*\\.letter.*$" . indented-text-mode))
       '(("^.*/tmp/[^/]*\\.ed.*$" . indented-text-mode)) ; mail edit buffer
       '(("^.*/tmp/[^/]*nf.*$" . indented-text-mode)) ; notesfile compose buffer
       '(("/tmp/\.mail.*$" . mail-mode))
       auto-mode-alist))

(if (elisp-file-in-loadpath-p "ksh-mode")
    (setq auto-mode-alist
	  (append
	   '(("\\.sh.[.0-9]+$" . ksh-mode))
	   '(("\\.sh$" . ksh-mode))
	   '(("\\.ksh.*$" . ksh-mode))
	   '(("\\..*profile" . ksh-mode))
	   auto-mode-alist)))

(if (elisp-file-in-loadpath-p "vm")
    (setq auto-mode-alist
	  (append
	   '(("/Letter[^/]*$" . vm-mode))
	   '(("mbox$" . vm-mode))
	   '(("/Mail/.*$" . vm-mode))
	   '(("/News/.*$" . vm-mode))
	   auto-mode-alist)))

;;;; ----------
;;;; special setup!

(eval-and-compile
  (and (fboundp 'setenv)
       ;; Set the PATH environment variable from the exec-path so
       ;; that child processes will inherit anything emacs uses.
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

;;;; For mail reading or looking at man pages (courtesy Mark Moraes)
(defun remove-nroff-bs ()
  "remove all nroff overstriking from a buffer"
  (interactive "*")
  ;; call removebs from start to end, replacing input with output, display
  ;; after command completion. Removebs is a simple program in my bin -
  ;; could have used col -b, but it is slower, and the equivalent emacslisp
  ;; (like nuke-nroff-bs from man.el) is much slower.
  (call-process-region (point-min) (point-max) "removebs" t t nil)
  (goto-char (point-min)))

;;; So I can conveniently do this from MH mode (also Mark Moraes)
(defun remove-nroff-bs-in-other-window ()
  "remove all nroff overstriking from the buffer in the other window"
  (interactive)
  (let ((thiswin (selected-window)))
    (other-window 1)
    (remove-nroff-bs)
    (select-window thiswin)))
(global-set-key "\C-x4z" 'remove-nroff-bs-in-other-window)

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

;;; from the FAQ
;;;
;;; use:	(swap-keys ?\C-h ?\C-?)
;;;
(defun swap-keys (key1 key2)
  "Swap keys KEY1 and KEY2 using map-key."
  (map-key key1 key2)
  (map-key key2 key1))

(defun map-key (from to)
  "Make key FROM behave as though key TO was typed instead."
  (setq keyboard-translate-table
	(concat keyboard-translate-table
		(let* ((i (length keyboard-translate-table))
		       (j from)
		       (k i)
		       (str (make-string (max 0 (- j (1- i))) ?X)))
		  (while (<= k j)
		    (aset str (- k i) k)
		    (setq k (1+ k)))
		  str)))
  (aset keyboard-translate-table from to)
  (let ((i (1- (length keyboard-translate-table))))
    (while (and (>= i 0) (eq (aref keyboard-translate-table i) i))
      (setq i (1- i)))
    (setq keyboard-translate-table
	  (if (eq i -1)
	      nil
	    (substring keyboard-translate-table 0 (1+ i))))))

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
  "Insert output of date process in current buffer at point."
  (interactive)
  (call-process "date" nil t t))

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

;;; for orthogonality (thx to john@xanth.UUCP (John Owens))
(defun find-file-read-only-other-window (filename)
  "Like find-file-read-only, but does it in another window."
  (interactive "Find file read-only in other window: ")
  (switch-to-buffer-other-window (find-file-noselect filename))
  (setq buffer-read-only t))
(global-set-key "\^x4\^r" 'find-file-read-only-other-window)

;;; More stuff stolen from Roland.
(defun make-interactive (symbol &rest interactive-args)
  "Make the function definition of SYMBOL an interactive command.
Remaining arguments, if any, are passed to interactive in the function."
  (let ((func (symbol-function symbol))
	interactive)
    (if (commandp func)
	(let ((msg (format "%s is already interactively callable." symbol)))
	  (or (null interactive-args)
	      (y-or-n-p (concat msg "  Continue? "))
	      (error msg))))
    (setq interactive (cons 'interactive interactive-args))
    (if (subrp func)
	(setq func
	      (list 'lambda '(&rest args) (documentation func) interactive
		    (cons 'eval
			  (list
			   (cons '` (list (list 'funcall func '(,@ args))))
			   ))))
      (let ((funcar (car func))
	    (args (car (cdr func)))
	    doc body)
	(setq doc (car (cdr (cdr func))))
	(if (stringp doc)
	    (setq body (cdr (cdr (cdr func))))
	  (setq doc nil
		body (cdr (cdr func))))
	(setq func
	      (cons funcar
		    (if doc (cons args (cons doc (cons interactive body)))
		      (cons args (cons args (cons interactive body))))))
	))
    (fset symbol func)))

(defun override-default-variable-settings ()
  "User defined function.  Intended to be called within various hooks to
override the value of buffer-local variables whose default values
might have been overridden by the major mode."
  (setq case-fold-search t		; allow case-insensitive searches
        indent-tabs-mode t		; allow tabs in indentation
        selective-display nil))		; don't allow selective display

;;; From: simonm@plod.ai.mit.edu (Simon Marshall)
;;; Reply-To: Simon.Marshall@mail.esrin.esa.it
;;; Date: Fri, 21 Jan 94 08:54:28 GMT
;;; To: bug-gnu-emacs@prep.ai.mit.edu
;;; Subject: [19.22]: `match-string': Short but sweet function
;;;
;;; In GNU Emacs 19.22.1 of Tue Nov 30 1993 on tracy (berkeley-unix)
;;;
;;; I think I got some version from someone else, but here's a nice function
;;; to alleviate the (substring string (match-beginning 1) (match-end 1))
;;; blues.  Now you can just (match-string 1 string) to your heart's delight...
;;;
(defun match-string (n &optional string)
  "Return the matched grouping N from STRING.
If STRING is not given, use the current buffer.  See `string-match'."
  (if (stringp string)
      (substring string (match-beginning n) (match-end n))
    (buffer-substring (match-beginning n) (match-end n))))

;; These ones I dreampt up myself!
;;
;; (I would also like to have a similar function that deletes all the trailing
;; whitespace from every line in a buffer.)
;;
(defun buffer-trim-trailing-whitespace (&optional buff-nm)
  "Trim the trailing whitespace a buffer.
If BUFF-NM is not given, use the current buffer."
  (interactive)
  (if (not (bufferp buff-nm))
      (setq buff-nm (read-buffer "Buffer to trim: "
				 (buffer-name (current-buffer)))))
  (buffer-trim-trailing-chars buff-nm))

(defun buffer-trim-trailing-chars (buff-nm &optional chars-to-trim)
  "Trim trailing characters from a buffer.
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
  (symbol-function function)
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

;;;; ----------
;;;; some special hooks.....

(if (or (string-equal (getenv "EDITOR") "emacsclient")
	(string-equal (getenv "VISUAL") "emacsclient"))
    (progn
      ;; to quiet the v19 byte compiler
      (defvar server-temp-file-regexp)
      (defvar server-process)
      (eval-and-compile
	(autoload 'server-buffer-done "server"
	  "Mark BUFFER as \"done\" for its client(s)."
	  nil nil))
      (require 'server)
      (setq server-temp-file-regexp
	    "/tmp/Re\\|/draft$\\|/\\.letter$\\|/\\.article$/\\|/tmp/[^/]*\\.ed\\|/tmp/[^/]*nf")
      ;; From: qhslali@aom.ericsson.se (Lars Lindberg EHS/PBE 80455 2122 { tom
      ;;	-> 940531  ansv. EHS/PBE Christer Nilsson })
      ;; Message-Id: <9402170914.AA18291@aom.ericsson.se>
      ;; Subject: [19.22] emacsclient server should have a hook for kill-buffer
      (add-hook 'server-visit-hook
		(function
		 (lambda ()
		   (add-hook 'kill-buffer-hook
			     (function
			      (lambda ()
				(server-buffer-done
				 (current-buffer))))))))
      (defun server-really-exit ()	; for those times we forget
	"Query user if he really wants to exit since this will destroy the
current emacs server process..."
	(interactive)
	(if server-process
	    (if (yes-or-no-p
		 "Are you sure you *really* want to exit? ")
		(save-buffers-kill-emacs))))
      (global-set-key "\C-x\C-c" 'server-really-exit)
      (server-start)))

;;; Reset mail-aliases to t when ~/.mailrc is written, so that mail-aliases
;;; will get updated before next send."
;;; From: mike@inform.co.nz (Mike Williams)From: mike@inform.co.nz (Mike Williams)
(defvar mail-aliases)
(add-hook 'write-file-hooks
	  (function
	   (lambda ()
	     (if (string-equal (buffer-file-name)
			       (expand-file-name "~/.mailrc"))
		 (setq mail-aliases t))
	     nil)			; Must return nil for file to be written
	   ))

;;;; ----------
;;;; some major-mode hooks...

;;; Date: Wed, 2 Feb 1994 12:49:31 GMT
;;; Message-Id: <1994Feb2.124931.19715@nessie.mcc.ac.uk>
;;; Organization: Manchester Computing Centre, Manchester, England
;;; From: ehgasm2@uts.mcc.ac.uk (Simon Marshall)
;;; Subject: Re: Quick routine to BOLDFACE directories in DIRED buffers
;;;
(if window-system
    (progn
      ;; to quiet the v19 byte compiler
      (defvar font-lock-keywords)
      (require 'font-lock)
      (add-hook 'dired-mode-hook
		(function
		 (lambda ()
		   (font-lock-mode t)
		   (setq font-lock-keywords
			 dired-font-lock-keywords))))))

(add-hook 'display-time-hook
	  (function
	   (lambda ()
	     (if (elisp-file-in-loadpath-p "vm")
		 nil			; we want mail checking....
	       (progn
		 ;; display-time can't check "Status:" headers or "Forward
		 ;; to" files so if not running vm or something else that
		 ;; cleans out the spool files, disable the mail checking
		 ;; feature
		 ;;
		 ;; must appear after display-time is invoked (thus after
		 ;; time.el is loaded)
		 ;;
		 (defun display-time-file-nonempty-p (file)
		   "This function returns 'nil, as it would only be useful if
it could check Status: headers for O, or Forward to in mailboxes."
		   nil))))))

(add-hook 'lisp-interaction-mode-hook
	  (function
	   (lambda ()
	     "Private lisp-interaction-mode-hook."
	     (setq mode-name "LispInteraction")
	     (override-default-variable-settings))))

;;; GNU-Emacs' (Stallman's?) ideas about formatting C code suck!  Let's stick to
;;; doing things the good old K&R standard way!!!!
;;;
(add-hook 'c-mode-hook
	  (function
	   (lambda ()
	     "Private c-mode stuff."
	     ;; damn c-mode is too over-bearing!  It seems to insist re-setting
	     ;; these bindings without regard to the global key map.
	     (local-set-key "\eh" 'mark-c-function)
	     (local-set-key "\e\C-h" 'backward-kill-word)
	     (local-set-key "\C-?" 'delete-char)
	     (local-set-key "\e\C-?" 'kill-word)
	     (local-set-key "\e\C-e" 'compile)
	     (override-default-variable-settings)
	     (setq fill-column 79)
	     (setq comment-column 48)
	     (setq comment-multi-line t)
	     (setq c-auto-newline nil)
	     (setq c-argdecl-indent 8)
	     (setq c-auto-newline nil)
	     (setq c-brace-offset 0)
	     (setq c-brace-imaginary-offset 0)
	     (setq c-continued-statement-offset 8)
	     (setq c-continued-brace-offset -8)
	     (setq c-indent-level 8)
	     (setq c-label-offset -8)
	     (setq c-tab-always-indent nil))))

;; to quiet the v19 byte compiler
(defvar vc-command-messages)
(defvar vc-initial-comment)
(defvar vc-checkout-carefully)
(add-hook 'vc-mode-hook
	  (function
	   (lambda ()
	     "Private vc-mode stuff."
	     (setq vc-command-messages t)
	     (setq vc-initial-comment t)
	     (setq vc-checkout-carefully t)
	     (add-hook 'vc-checkin-hook
		       'vc-comment-to-change-log))))

(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda ()
	     "Private emacs-lisp-mode stuff."
	     (setq fill-column 79)
	     (turn-on-auto-fill))))

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
		 (override-default-variable-settings)
		 (local-set-key "\e\C-h" 'backward-kill-word)
		 (local-set-key "\eS" 'spell-buffer)
		 (local-set-key "\e\C-e" 'compile)
		 (setq abbrev-mode t)
		 (setq fill-column 72)
		 (setq require-final-newline t)		; needed by some unix programs
		 (turn-on-auto-fill))))))

(add-hook 'nroff-mode-hook
	  (function
	   (lambda ()
	     "Private nroff-mode stuff."
	     (run-hooks 'text-mode-hook))))

;;;; ----------
;;;; more hooks for non-default packages

(if (elisp-file-in-loadpath-p "pcl-cvs")
    (progn
      ;; to quiet the v19 byte compiler
      (defvar cvs-diff-flags)
      (defvar cvs-status-flags)
      (defvar cvs-diff-ignore-marks)
      (add-hook 'cvs-mode-hook
		(function
		 (lambda ()
		   "Private cvs-mode stuff."
		   ;; List of strings to use as  flags to pass to
		   ;; ``diff'' and ``cvs diff''.
		   (setq cvs-diff-flags '("-u"))
		   ;; List of strings to pass to ``cvs status''
		   (setq cvs-status-flags '("-Q"))
		   ;; Non-nil if cvs-diff and cvs-mode-diff-backup
		   ;; should ignore any marked files.
		   (setq cvs-diff-ignore-marks t))))))

(if (elisp-file-in-loadpath-p "ksh-mode")
    (progn
      ;; to quiet the v19 byte compiler
      (defvar ksh-indent)
      (defvar ksh-group-indent)
      (defvar ksh-brace-indent)
      (defvar ksh-case-item-indent)
      (defvar ksh-case-indent)
      (defvar ksh-match-and-tell)
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

(if (elisp-file-in-loadpath-p "c-boxes")
    (progn
      ;; to quiet the v19 byte compiler
      (defvar c-comment-starting-blank)
      (add-hook 'c-mode-hook
		(function
		 (lambda ()
		   "Private c-boxes stuff."
		   (local-set-key "\eq" 'reindent-c-comment)
		   (setq c-comment-starting-blank t))))))

;;;; ----------
;;;; some default key re-binding....

;;; first off, we do some fancy stuff to make C-h work "properly," but still
;;; have good access to the help functions!
;;;
;;; NOTE: this *should* work by simply reading termio for current erase char.
;;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-?" 'delete-char)
(global-set-key "\e\C-h" 'backward-kill-word)
(global-set-key "\e\C-?" 'kill-word)
(global-set-key "\e?" 'help-command)		; smart enough to set itself up

;; I *USUALLY* EXPECT THE BACKSPACE KEY TO GENERATE AN ASCII BACKSPACE!
(define-key function-key-map [backspace] [8])
(define-key function-key-map [backspace] [?\C-h])
(define-key function-key-map [C-backspace] [?\C-h])
(define-key function-key-map [M-backspace] [?\M-\C-h])

;;; for fingers that forget....
(global-set-key "\C-\\" 'search-forward)
(global-set-key "\C-x\C-\\" 'save-buffer)

;;; much of the remainder is to get back some Jove/Gosmacs comaptability, but
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

(global-set-key "\e!" 'shell)
(global-set-key "\C-x!" 'shell-command)
(global-set-key "\C-x\C-d" 'insert-date-in-current-buffer)

(global-set-key "\C-x\C-v" 'find-file)	; I never liked "visit"....
;;;(global-set-key "\C-xv" 'view-file)	; this is now the prefix for vc
(global-set-key "\C-xV" 'find-alternate-file)

(global-set-key "\ez" 'scroll-one-line-down)
(global-set-key "\C-z" 'scroll-one-line-up)
(global-set-key "\e\C-l" 'line-to-top-of-window)

(global-set-key "\C-xz" 'enlarge-window)
(global-set-key "\C-x\C-z" 'shrink-window)

(global-set-key "\e\C-z" 'suspend-emacs)

(global-set-key "\C-x?" 'describe-key-briefly)
(global-set-key "\C-x\C-a" 'super-apropos)

(global-set-key "\e," 'top-of-window)		; mirror M-<
(global-set-key "\e." 'bottom-of-window)	; mirror M->

(global-set-key "\C-xT" 'find-tag)
(global-set-key "\C-x4T" 'find-tag-other-window)

(global-set-key "\eS" 'spell-buffer)

(if window-system
    (progn
      (global-set-key "\C-x5i" 'iconify-frame)
      (global-set-key "\C-x5T" 'find-tag-other-frame)))

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
      (defvar compilation-frame-id)
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
(if (elisp-file-in-loadpath-p "ispell")
    (progn
      ;; to quiet the v19 byte compiler
      (defvar ispell-filter-hook)
      (defvar ispell-filter-hook-args)
      (defvar plain-TeX-mode-hook)
      (defvar LaTeX-mode-hook)
      (defvar nroff-mode-hook)
      (require 'ispell)
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

;;; From: kifer@sbkifer.cs.sunysb.edu (Michael Kifer)
;;; Subject: Re: calendar tool in Emacs?
;;; Organization: SUNY at Stony Brook
;;; Date: 15 Nov 1993 20:53:02 GMT
;;; Message-Id: <KIFER.93Nov15155303@sbkifer.cs.sunysb.edu>
;;;
;;;(if (= init-emacs-type '19)
;;;    (progn
;;;      (setq
;;;       view-diary-entries-initially t
;;;       mark-diary-entries-in-calendar t
;;;       mark-holidays-in-calendar t
;;;       diary-display-hook (list 'appt-make-list 'fancy-diary-display)
;;;       appt-display-duration 14		; seconds to display appointment message
;;;       appt-issue-message t)
;;;      (autoload 'appt-make-list "appt.el" nil t)
;;;      (add-hook 'initial-calendar-window-hook 'display-time)
;;;      (calendar)))
(require 'advice)
(defadvice appt-disp-window (around kn-appt-disp-win compile)
  (if (or (= min-to-app 20)
	  (and (<= min-to-app 6) (= (mod min-to-app 2) 0)))
      ad-do-it))
(eval-after-load "appt" '(ad-activate 'appt-disp-window))
(setq calendar-latitude 43.75)
(setq calendar-longitude -79.45)
(setq today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-time-display-form
      '(24-hours ":" minutes
		 (if time-zone " (") time-zone (if time-zone ")")))
(setq appt-message-warning-time 20) ; minutes of warning prior to appt
(setq view-diary-entries-initially t)
;;;   (setq mark-diary-entries-in-calendar t) ; too expensive....
(setq mark-holidays-in-calendar t)
(setq diary-display-hook (list 'appt-make-list 'fancy-diary-display))
(setq appt-display-duration 60)	; seconds to display appointment message
(setq appt-issue-message t)
(setq number-of-diary-entries [3 3 3 3 3 4 3])
(setq all-christian-calendar-holidays t)
(setq other-holidays
      '((holiday-fixed 1 11 "Sir John A. Macdonald's birthday")
	(holiday-float 2 1 3 "Heritage Day") ; (unoff.) third Monday
	(holiday-fixed 4 21 "Queen Elizabeth's birthday")
	(holiday-fixed 4 22 "Earth Day")
	(holiday-float 5 1 -2 "Victoria Day") ; second last Monday
	(holiday-fixed 7 14 "Bastille Day")
	(holiday-fixed 7 1 "Canada Day")
	(holiday-float 8 1 1 "Civic Holiday") ; first Monday
	(holiday-float 9 1 1 "Labour Day") ; first Monday
	(holiday-float 10 1 2 "Thanksgiving Day (Canada)")
	(holiday-fixed 10 16 "World Food Day") ; ????
	(holiday-fixed 12 6 "National Day of Remembrance and Action on
Violence Against Women")
	(holiday-fixed 12 26 "Boxing Day")))
;;; don't need to do this -- was done above
;;;      (add-hook 'initial-calendar-window-hook 'display-time)
(autoload 'appt-make-list "appt.el" nil t)

;;; ;; Appointments every 3 minutes not every 1 minute!
;;; (defadvice appt-check (around my-appt-advice activate)
;;;    "Notify about appointments only if time is multiple of 3."
;;;    (let ((cur-min (string-to-int
;;; 		   (substring (current-time-string) 14 16))))
;;;      (if (eq 0 (mod cur-min 3))
;;; 	 ad-do-it)))

;;;;-------
;;;; the closing comments.....

;;; From: nickel@cs.tu-berlin.de (Juergen Nickelsen)
;;; Newsgroups: gnu.emacs.help,comp.emacs
;;; Subject: Re: model for .emacs file
;;; Date: 18 Jan 1993 18:48:18 GMT
;;; Organization: STONE Project, Technical University of Berlin, Germany
;;; Message-ID: <NICKEL.93Jan18194816@tempest.cs.tu-berlin.de>
;;;
;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
