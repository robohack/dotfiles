;;;
;;;	.emacs.el
;;;
;;;#ident	"@(#)HOME:.emacs.el	4.1	93/12/15 15:13:21 (woods)"
;;;
;;; per-user start-up functions for GNU-emacs v18 or v19
;;;

; to debug, eval these after "emacs -q":
; (setq debug-on-error t)
; (load-file "~/.emacs.el")

(setq inhibit-startup-message t)

;; ----------
;; stolen from cl.el -- find out where we are!
(defvar init-emacs-type (cond ((or (and (fboundp 'epoch::version)
					(symbol-value 'epoch::version))
				   (string-lessp emacs-version "19")) 18)
			      ((string-match "Lucid" emacs-version) 'lucid)
			      (t 19)))

;; ----------
;; get ready to load stuff

(setq load-path (cons (expand-file-name "~/lib/elisp") load-path))

(if (= init-emacs-type '19)
    (setq load-path (append load-path
			    (list (concat
				   (cond 
				    ((getenv "GNU")
				     (getenv "GNU"))
				    ((getenv "LOCAL")
				     (concat (getenv "LOCAL") "/gnu")))
				   "/lib/emacs/site-lisp")))))

;; emacs-18 doesn't have these...
(if (= init-emacs-type '18)
    (defmacro dont-compile (&rest body)
      "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
      (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body))))))

(if (= init-emacs-type '18)
    (defmacro eval-when-compile (&rest body)
      "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant."
      ;; Not necessary because we have it in b-c-initial-macro-environment
      ;; (list 'quote (eval (cons 'progn body)))
      (cons 'progn body)))

(if (= init-emacs-type '18)
    (defmacro eval-and-compile (&rest body)
      "Like `progn', but evaluates the body at compile time and at load time."
      ;; Remember, it's magic.
      (cons 'progn body)))

;; This could probably be rewritten to use mapcar
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

;; ----------
;; some default packages we'd like...

(if (or (= init-emacs-type '19)
	(elisp-file-in-loadpath-p "display-time"))
    (dont-compile
      (setq display-time-mail-file "/dev/null") ; it can't check "Status:" headers
      (setq display-time-day-and-date t)
      (setq display-time-24hr-format t)
      (display-time)))

(if (and (/= init-emacs-type '19)
	 (elisp-file-in-loadpath-p "mode-line"))
    (dont-compile
      (require 'mode-line)
      (if (boundp 'file-name-abbreviation-alist)
	  (setq file-name-abbreviation-alist
		(append (list '("^/big/web/work/apc/" . "APC|")
			      '("^/big/web/work/" . "WEB|")
			      '("^/big/web/update.d/" . "UPD|")
			      '((concat "^" (getenv "LOCAL") "/src") . "LSRC|")
			      '((concat "^" (getenv "LOCAL")) . "LOCAL|")
			      (cons (concat "^" (expand-file-name "~") "/src/work.d/")
				    "~WRK|")
			      (cons (concat "^" (expand-file-name "~") "/src/")
				    "~SRC|"))
			file-name-abbreviation-alist)))
      (require 'mode-line)))

(if (elisp-file-in-loadpath-p "c-boxes")
    (autoload 'reindent-c-comment "c-boxes" nil t))

(if (elisp-file-in-loadpath-p "ksh-mode")
    (autoload 'ksh-mode "ksh-mode" "Major mode for editing sh Scripts." t))

;; ----------
;; some property defintions...

(put 'eval-expression 'disabled nil)	; allow ESC ESC
(put 'narrow-to-region 'disabled nil)	; allow C-x n
(put 'rmail 'disabled t)		; avoid mbox destruction

;; ----------
;; handling of abbrev files...
(condition-case ()
    (read-abbrev-file nil t)
  (file-error nil))

;; ----------
;; If running as root, don't make backup files.  This should be default!!!
(cond ((eq (user-uid) 0)
       (setq make-backup-files nil)
       (setq auto-save-default nil)))

;; ----------
;; Set defaults of other buffer-local variables

(setq-default case-fold-search nil)	; V-19 will fix this even better
(setq-default indent-tabs-mode t)	; allow tabs in indentation
(setq-default require-final-newline 1)	; needed by some unix programs

;; ----------
;; some new global variable settings...

(dont-compile
  (if (= init-emacs-type 18)
      (setq ask-about-buffer-names t)))	;
(setq backup-by-copying t)		; copy, thus preserving modes and owner
(dont-compile
  (if (= init-emacs-type 18)
      (setq completion-auto-exit t)))	; have completing-reads exit when unique
(setq compilation-window-height 10)	; default height for a compile window
(setq default-tab-width 8)		; a tab is a tab is a tab is a tab....
(setq delete-auto-save-files t)		; delete auto-save file when saved
;(setq enable-recursive-minibuffers t)	; do we really want this?  No, probably not
(dont-compile
  (if (= init-emacs-type 18)
      (setq inhibit-local-variables t)	; confirm modes, etc. (security!)
    (progn
      (if (eq window-system 'x)
	  (setq search-highlight t))	; i-search hightlight match
      (setq enable-local-variables t)))) ; (is this the default in v19?)
(setq make-backup-files nil)		; too much clutter
(dont-compile
  (if (= init-emacs-type 18)
      (setq search-delete-char 8)))	; C-h, same as delete-backward-char
(dont-compile
  (if (= init-emacs-type 18)
      (setq spell-command "spell -b")))	; we're British, you know! :-)
(setq track-eol nil)			; too hard to control (it's sticky!)
(setq window-min-height 1)
(setq window-min-width 1)

(setq auto-mode-alist
      (append
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
       auto-mode-alist))

(if (elisp-file-in-loadpath-p "ksh-mode")
    (setq auto-mode-alist
	  (append
	   '(("\\.sh.[.0-9]+$" . ksh-mode))
	   '(("\\.sh$" . ksh-mode))
	   '(("\\.ksh.*$" . ksh-mode))
	   '(("\\..*profile" . ksh-mode))
	   auto-mode-alist)))

(setq completion-ignored-extensions
      (append '(".out")
	      completion-ignored-extensions))

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

;; unix "spell" knows to use "deroff", so only use this if you use a speller
;; other than it.
;;
;(defun filter-through-deroff ()
;  "Magic!"
;  (setq spell-command (concat "deroff | " spell-command)))

;; ----------
;; some useful functions....

; I hate it running away off the end of the file - next line should stop at
; the end of the file  (courtesy Mark Moraes)
; [I wish they'd both ring the bell when the run into BOF or EOF....]
(defun next-line (count)
  (interactive "p")
  (previous-line (- count)))

;; For mail reading or looking at man pages (courtesy Mark Moraes)
(defun remove-nroff-bs ()
  "remove all nroff overstriking from a buffer"
  (interactive "*")
  ;; call removebs from start to end, replacing input with output, display
  ;; after command completion. Removebs is a simple program in my bin - 
  ;; could have used col -b, but it is slower, and the equivalent emacslisp
  ;; (like nuke-nroff-bs from man.el) is much slower.
  (call-process-region (point-min) (point-max) "removebs" t t nil)
  (goto-char (point-min)))

;; So I can conveniently do this from MH mode (also Mark Moraes)
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

;; from the FAQ
;;
;; use:	(swap-keys ?\C-h ?\C-?)
;;
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

;; Snarfed from Steve Humble
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

(defun insert-date-in-current-buffer ()
  "Insert time and date in current buffer at point."
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

(or (fboundp 'start-process-shell-command)
    ;; From version 19 subr.el.
    (defun start-process-shell-command (name buffer &rest args)
      "Start a program in a subprocess.  Return the process object for it.
Args are NAME BUFFER COMMAND &rest COMMAND-ARGS.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer or (buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is command name, the name of a shell command.
Remaining arguments are the arguments for the command.
Wildcards and redirection are handle as usual in the shell."
      (if (eq system-type 'vax-vms)
          (apply 'start-process name buffer args)
        (start-process name buffer shell-file-name "-c"
                       (concat "exec " (mapconcat 'identity args " "))))))

;; for orthogonality (thx to john@xanth.UUCP (John Owens))
(defun find-file-read-only-other-window (filename)
  "Like find-file-read-only, but does it in another window."
  (interactive "Find file read-only in other window: ")
  (switch-to-buffer-other-window (find-file-noselect filename))
  (setq buffer-read-only t))
(global-set-key "\^x4\^r" 'find-file-read-only-other-window)

;; More stuff stolen from Roland. 
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

(or (fboundp 'add-hook)
    ;; From version 19 subr.el
    (defun add-hook (hook function)
      "Add to the value of HOOK the function FUNCTION unless already present.
HOOK should be a symbol, and FUNCTION may be any valid function.
HOOK's value should be a list of functions, not a single function.
If HOOK is void, it is first set to nil."
      (or (boundp hook) (set hook nil))
      (or (if (consp function)
              ;; Clever way to tell whether a given lambda-expression
              ;; is equal to anything in the hook.
              (let ((tail (assoc (cdr function) (symbol-value hook))))
                (equal function tail))
            (memq function (symbol-value hook)))
          (set hook (cons function (symbol-value hook))))))

;; ----------
;; some special hooks.....

(dont-compile
  (if (and (elisp-file-in-loadpath-p "server")
	   (string-equal (getenv "VISUAL") "emacsclient"))
      (progn
	(require 'server)
	;; I *USUALLY* EXPECT THE BACKSPACE KEY TO GENERATE AN ASCII BACKSPACE!
	(define-key function-key-map [backspace] [8])
	(define-key function-key-map [backspace] [?\C-h])
	(define-key function-key-map [C-backspace] [?\C-h])
	(define-key function-key-map [M-backspace] [?\M-\C-h])
	(defun server-really-exit ()	; for those times we forget
	  "Query user if he really wants to exit since this will destroy the
current emacs server process..."
	  (interactive)
	  (if server-process
	      (if (yes-or-no-p "Are you sure you *really* want to exit? ")
		  (save-buffers-kill-emacs))))
	(global-set-key "\C-x\C-c" 'server-really-exit)
	(server-start))))

; (enable-arrow-keys) must be done by this hook, since the .emacs file
; is loaded and executed before the terminal code is loaded...
;
(setq term-setup-hook
      (function
       (lambda ()
	 "Private term-setup-hook."
	 (if (fboundp 'enable-arrow-keys) ; byte-compile-file may complain
	     (enable-arrow-keys)))))	  ; that enable-arrow-keys is not defined

;; ----------
;; some major-mode hooks...

(add-hook 'lisp-interaction-mode-hook
	  (function 
	   (lambda () 
	     "Private lisp-interaction-mode-hook."
	     (setq mode-name "LispInteraction")
	     (override-default-variable-settings))))

; Stallman's ideas about formatting C code suck!
;
(add-hook 'c-mode-hook
	  (function
	   (lambda ()
	     "Private c-mode stuff."
	     (local-set-key "\eh" 'mark-c-function)
	     (local-set-key "\e\C-h" 'backward-kill-word)
	     (local-set-key "\e\C-e" 'compile)
	     (if (elisp-file-in-loadpath-p "c-boxes")
		 (local-set-key "\eq" 'reindent-c-comment))
	     (override-default-variable-settings)
	     (setq fill-column 79)
	     (setq comment-column 48)
	     (setq comment-multi-line t)
	     (setq c-auto-newline nil)
	     (setq c-comment-starting-blank t)
	     (setq c-argdecl-indent 8)
	     (setq c-auto-newline nil)
	     (setq c-brace-offset 0)
	     (setq c-brace-imaginary-offset 0)
	     (setq c-continued-statement-offset 8)
	     (setq c-continued-brace-offset -8)
	     (setq c-indent-level 8)
	     (setq c-label-offset -8)
	     (setq c-tab-always-indent nil))))

(dont-compile
  (if (elisp-file-in-loadpath-p "ksh-mode")
      (add-hook 'cvs-mode-hook
		(function
		 (lambda ()
		   "Private cvs-mode stuff."
		   (setq cvs-diff-flags '("-u")) ; List of strings to use as flags to pass to ``diff'' and ``cvs diff''.
		   (setq cvs-status-flags '("-Q")) ; List of strings to pass to ``cvs status''
		   (setq cvs-diff-ignore-marks t)))))) ; Non-nil if cvs-diff and cvs-mode-diff-backup should ignore any marked files. 

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

(dont-compile
  (if (elisp-file-in-loadpath-p "ksh-mode")
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

;; ----------
;; some default key re-binding....

; first off, we do some fancy stuff to make C-h work "properly," but still
; have good access to the help functions!
;
; NOTE: this *should* work by simply reading termio for current erase char.
;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-?" 'delete-char)
(global-set-key "\e\C-h" 'backward-kill-word)
(global-set-key "\e\C-?" 'kill-word)
(global-set-key "\e?" 'help-command)		; smart enough to set itself up

; for fingers that forget....
(global-set-key "\C-\\" 'search-forward)
(global-set-key "\C-x\C-\\" 'save-buffer)

; much of the remainder is to get back some Jove/Gosmacs comaptability, but
; without getting it all....
;
(global-set-key "\e\C-r" 'isearch-backward-regexp)
(global-set-key "\eq" 'query-replace-regexp)
(global-set-key "\eQ" 'query-replace)
(global-set-key "\er" 'replace-regexp)
(global-set-key "\eR" 'replace-string)

(global-set-key "\ej" 'fill-paragraph)
(global-set-key "\eJ" 'fill-region)

(global-set-key "\C-x\C-i" 'insert-file)
(global-set-key "\e " 'set-mark-command)
(global-set-key "\C-x " 'fixup-whitespace)
(global-set-key "\C-xt" 'goto-line)

(global-set-key "\e!" 'shell)
(global-set-key "\C-x!" 'shell-command)
(global-set-key "\C-x\C-d" 'insert-date-in-current-buffer)

(global-set-key "\C-x\C-v" 'find-file)	; I never liked "visit"....
;;(global-set-key "\C-xv" 'view-file)	; this is now the prefix for vc
(global-set-key "\C-xV" 'find-alternate-file)

(global-set-key "\ez" 'scroll-one-line-down)
(global-set-key "\C-z" 'scroll-one-line-up)
(global-set-key "\e\C-l" 'line-to-top-of-window)

(global-set-key "\C-xz" 'enlarge-window)
(global-set-key "\C-x\C-z" 'shrink-window)

(global-set-key "\e\C-z" 'suspend-emacs)

(global-set-key "\C-x?" 'describe-key-briefly)

(global-set-key "\e," 'top-of-window)		; mirror M-<
(global-set-key "\e." 'bottom-of-window)	; mirror M->

(global-set-key "\C-xT" 'find-tag)
(global-set-key "\C-x4T" 'find-tag-other-window)

(global-set-key "\eS" 'spell-buffer)

(if (= init-emacs-type '19)
    (dont-compile
      (global-set-key "\C-x\C-a" 'super-apropos)
      (global-set-key "\C-x5i" 'iconify-frame)
      (global-set-key "\C-x5T" 'find-tag-other-frame)))

;; Bindings to make it look like Jove (or old Emacs :-)
;; (courtesy Mark Moraes)
;(defun prev-window ()
;  (interactive)
;  (other-window -1)) ; this does not deal with argument
;(define-key global-map "\C-xn" 'other-window)
;(define-key global-map "\C-xp" 'prev-window)
;(define-key global-map "\C-xq" 'quoted-insert)
;(define-key global-map "\C-z" 'one-scroll-up)
;(define-key global-map "\ez" 'one-scroll-down)
;(define-key global-map "\C-r" 'search-backward)
;(define-key global-map "\eq" 'query-replace-regexp)
;(define-key global-map "\er" 'replace-regexp)
;(define-key global-map "\eg" 'goto-line)
;(define-key global-map "\ej" 'fill-paragraph)
;(define-key global-map "\e\C-z" 'suspend-emacs)
;(define-key global-map "\C-\\" 'search-forward)
;(define-key global-map "\C-x\C-\\" 'save-buffer)
;(define-key global-map "\C-x\C-i" 'insert-file)
;(define-key global-map "\C-h" 'delete-backward-char)
;(define-key global-map "\e\C-h" 'backward-kill-word)
;(define-key global-map "\C-x!" 'shell-command)
;(define-key global-map "\e\e" 'keyboard-quit)
;(define-key global-map "\e " 'set-mark-command)
;(define-key global-map "\eC-M" 'set-mark-command)

;;-------
;; more goodies

(if (= init-emacs-type '19) 
      (dont-compile
	(defun display-buffer-in-frame-or-window (buf)
	  "Try to find buffer BUF in another (visible) frame, otherwise call
display-buffer for it"
	  (or (get-buffer-window buf t)
	      (display-buffer buf)))
	(setq temp-buffer-show-function 'display-buffer-in-frame-or-window)))

;; From: dsmith@spam.maths.adelaide.edu.au (David Smith)
;; Subject: framepop.el: Display temporary buffers in dedicated frame
;; Date: 08 Oct 1993 09:17:05 GMT
;; Organization: The University of Adelaide
;; Message-Id: <DSMITH.93Oct8184705@spam.maths.adelaide.edu.au>
;;
;;
; let's leave this until frame management is a wee bit more mature
;
;(if (elisp-file-in-loadpath-p "framepop")
;    (progn
;      (setq framepop-prefix-map (lookup-key global-map "\C-c\C-f"))
;      (if (not (keymapp framepop-prefix-map))
;	  (progn
;	    (setq framepop-prefix-map (make-sparse-keymap))
;	    (define-key global-map "\C-c\C-f" framepop-prefix-map)
;	    (define-key global-map "\C-c\C-fz" 'framepop-toggle-frame)
;	    (define-key global-map "\C-c\C-fv" 'framepop-scroll-frame)
;	    (define-key global-map "\C-c\C-fs" 'framepop-show-frame)
;	    (define-key global-map "\C-c\C-fx" 'framepop-iconify-frame)
;	    (define-key global-map "\C-c\C-fr" 'framepop-raise-frame)
;	    (define-key global-map "\C-c\C-fl" 'framepop-lower-frame)
;	    (cond (window-system (require 'framepop)))))))

;; From: ca@cs.umd.edu (Cengiz Alaetinoglu)
;; Newsgroups: gnu.emacs.sources
;; Subject: compile-frame.el version 1.1
;; Date: 08 Oct 1993 20:28:22 GMT
;; Organization: University of Maryland, Computer Science Department
;; Lines: 126
;; Distribution: world
;; Message-Id: <CA.93Oct8162822@yangtze.cs.umd.edu>
;; 
(dont-compile
  (if (elisp-file-in-loadpath-p "compile-frame")
      (progn
	(require 'compile-frame)
	(add-hook 'compilation-frame-selected-hook
		  '(lambda () (raise-frame compilation-frame-id))))))

;; From: kfogel@occs.cs.oberlin.edu (Karl Fogel)
;; Date: Mon, 1 Nov 1993 10:23:04 -0500
;; Message-Id: <9311011523.AA18545@occs.cs.oberlin.edu>
;; To: gnu-emacs-sources@prep.ai.mit.edu
;; Subject: frame hopping from the keyboard
;; 
;;         I wanted a something to move among frames without using the
;; mouse.  Emacs 19 apparently has no native function to do this
;; (corrections?  I couldn't find it, at least...), so here is one.  I
;; bind it to C-c o, myself.  Another candidate was C-x 5 o; although
;; it's too many keystrokes for me, it seems as though this is sort of
;; what C-x 5 o was originally meant to do (again, not sure).  Please let
;; me know if you find it useful or have any suggestions/fixes.  As you
;; can see, it's quite short (shorter than this paragraph, okay), but I
;; have hardly touched my mouse since I started using it :-)
;; 
(if (= init-emacs-type '19) 
    (dont-compile
      (global-set-key "\C-co" 'keyboard-focus-next-or-previous-frame)
      ;;
      (defun keyboard-focus-next-or-previous-frame (parg)
	"Switch the focus to the next logical frame (and raise that frame to
the front).  Keyboard input will go to the newly selected frame.
Prefix ARG means go to previous frame, not next frame.
The mouse cursor will not follow you, which is kind of a weird
feeling, but you'll get used to it."
	(interactive "P")
	(let* ((nowframe (selected-frame))
	       (nextframe (if parg (previous-frame) (next-frame)))
	       (visip (frame-visible-p nextframe)))
	  (and visip
	       (progn
		 (select-frame nextframe)
		 (if (eq visip 'icon) (iconify-or-deiconify-frame))
		 (redirect-frame-focus nowframe nextframe)
		 (raise-frame nextframe)))))))

;; Based on suggestions by David G. Grubbs <dgg@ksr.com> and Paul Palmer
;; <palmerp@math.orst.edu>.
;;
;; Assuming the use of detex 2.3 by Daniel Trinkle:
;; -w means one word per line.
;; -n means don't expand \input or \include commands.
;; -l means force LaTeX mode.
;
(if (and (= init-emacs-type '19) 
	 (elisp-file-in-loadpath-p "ispell"))
    (dont-compile
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

;; From: kifer@sbkifer.cs.sunysb.edu (Michael Kifer)
;; Subject: Re: calendar tool in Emacs?
;; Organization: SUNY at Stony Brook
;; Date: 15 Nov 1993 20:53:02 GMT
;; Message-Id: <KIFER.93Nov15155303@sbkifer.cs.sunysb.edu>
;;
;(if (= init-emacs-type '19) 
;    (progn
;      (setq 
;       view-diary-entries-initially t
;       mark-diary-entries-in-calendar t
;       mark-holidays-in-calendar t
;       diary-display-hook (list 'appt-make-list 'fancy-diary-display)
;       appt-display-duration 14		; seconds to display appointment message
;       appt-issue-message t)
;      (autoload 'appt-make-list "appt.el" nil t)
;      (add-hook 'initial-calendar-window-hook 'display-time)
;      (calendar)))
(if (= init-emacs-type '19) 
    (dont-compile
      (require 'advice)
      (defadvice appt-disp-window (around kn-appt-disp-win act)
	(if (or (= min-to-app 20)
		(and (<= min-to-app 6) (= (mod min-to-app 2) 0)))
	    ad-do-it))
      (eval-after-load "appt" '(ad-activate 'appt-disp-window))
      (setq calendar-latitude 43.75)
      (setq calendar-longitude -79.45)
      (setq calendar-time-display-form 
	    '(24-hours ":" minutes
		       (if time-zone " (") time-zone (if time-zone ")")))
      (setq appt-message-warning-time 20)
      (setq view-diary-entries-initially t)
      (setq mark-diary-entries-in-calendar t)
      (setq mark-holidays-in-calendar t)
      (setq diary-display-hook (list 'appt-make-list 'fancy-diary-display))
      (setq appt-display-duration 60)	; seconds to display appointment message
      (setq appt-issue-message t)
      (setq number-of-diary-entries [2 2 2 2 2 4 2])
      (setq all-christian-calendar-holidays t)
      (setq other-holidays
	    '((holiday-fixed 1 11 "Sir John A. Macdonald's birthday")
	      (holiday-fixed 2 17 "Heritage Day") ; ?????
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
      (autoload 'appt-make-list "appt.el" nil t)
      (add-hook 'initial-calendar-window-hook 'display-time)))

; ;; Appointments every 3 minutes not every 1 minute!
; (defadvice appt-check (around my-appt-advice activate)
;    "Notify about appointments only if time is multiple of 3."
;    (let ((cur-min (string-to-int 
; 		   (substring (current-time-string) 14 16))))
;      (if (eq 0 (mod cur-min 3))
; 	 ad-do-it)))

;; From: nickel@cs.tu-berlin.de (Juergen Nickelsen)
;; Newsgroups: gnu.emacs.help,comp.emacs
;; Subject: Re: model for .emacs file
;; Date: 18 Jan 1993 18:48:18 GMT
;; Organization: STONE Project, Technical University of Berlin, Germany
;; Message-ID: <NICKEL.93Jan18194816@tempest.cs.tu-berlin.de>
;;
;;; Local Variables:
;;; eval: (defun byte-compile-this-file () (write-region (point-min) (point-max) buffer-file-name nil 't) (byte-compile-file buffer-file-name) nil)
;;; write-file-hooks: (byte-compile-this-file)
;;; End:
