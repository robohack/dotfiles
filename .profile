#
#	.profile - for either sh, ksh, bash, or ash (if type is defined).
#
#ident	"@(#)HOME:.profile	20.8	99/06/27 17:35:17 (woods)"

#
# Assumptions that may cause breakage:
#
#	- standard environment has been set by login(1)
#	- $argv0 is `basename $0` from .xinitrc or .xsession
#	- test(1), aka "[", supports '-h' for testing symlinks
#

# Files referenced [all optional]:
#
#	$HOME/.ashtype	- sourced once, if running ash(1)
#	$HOME/.ashlogin	- sourced once, if running ash(1)
#	$HOME/.bashlogin - sourced once, if running bash(1)
#	$HOME/.editor	- name of prefered text editor command
#	$HOME/.kshlogin	- sourced once, if running ksh(1)
#	$HOME/.kshlogout - set on trap 0, if running ksh(1)[, or bash(1)?]
#	$HOME/.localprofile - sourced once early in here to set system-local prefs.
#	$HOME/.mailer	- name of prefered MUA command
#	$HOME/.shell	- mktable'd and exec'ed as shell (see end of this file)
#	$HOME/.shlogin	- sourced once, if running sh(1)
#	$HOME/.shlogout	- set on trap 0, if running sh(1) or ash(1)
#	$HOME/.shrc	- sourced once from .shlogin, and pathname used in $ENV
#	$HOME/.stty	- sourced for stty command(s), etc. just before tset(1)
#	$HOME/.trninit	- pathname set as value for $TRNINIT

# Notes:
#
#	.localprofile may set $PATH_IS_OKAY to "true" if it is so.
#

umask 022
ulimit -S -p 99999		# make it equal the hard limit

if [ -r $HOME/.bashlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} -a -n "${BASH}" ] ; then
	trap '. $HOME/.bashlogout ; exit $?' 0
elif [ -r $HOME/.kshlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} -a -z "${BASH}" ] ; then
	trap '. $HOME/.kshlogout ; exit $?' 0
elif [ -r $HOME/.shlogout ] ; then
	trap '. $HOME/.shlogout ; exit $?' 0
fi

if [ "`echo ~`" = "$HOME" -a -r $HOME/.ashtype -a ${RANDOM:-0} -eq ${RANDOM:-0} ] ; then
	# TODO: actually, maybe this should be considered a Posix shell environment...
	. $HOME/.ashtype
fi

if [ -z "$LOGNAME" ] ; then
	LOGNAME="$USER"
	export LOGNAME
fi

if [ -z "$UUNAME" ] ; then
	if expr "`type uuname`" : '.* is .*/uuname$' >/dev/null 2>&1 ; then
		UUNAME="`uuname -l`"
	else
		UUNAME="`hostname`"
	fi
	export UUNAME
fi

if [ -z "$HOSTNAME" ] ; then
	if expr "`type hostname`" : '.* is .*/hostname$' >/dev/null 2>&1 ; then
		HOSTNAME=`hostname`
	else
		HOSTNAME=$UUNAME
	fi 
	export HOSTNAME
fi

if [ -z "$DOMAINNAME" ] ; then
	if [ -r /etc/resolv.conf ] && fgrep domain /etc/resolv.conf >/dev/null 2>&1; then
		eval `sed -n 's/domain[ 	]*/DOMAINNAME=./p' /etc/resolv.conf`
	elif expr "`type domainname`" : '.* is .*/domainname$' >/dev/null 2>&1 ; then
		DOMAINNAME="`domainname`"
	elif expr "$HOSTNAME" : '^[^\.]*\.' >/dev/null 2>&1 ; then
		DOMAINNAME="."`expr "$HOSTNAME" : '^[^\.]*\.\(.*\)$'`
	else
		# these cases for machines without domainname,
		# and a short hostname....
		#
		case "$UUNAME" in
		weirdo )
			DOMAINNAME=".weird.com"
			;;
		* )
			DOMAINNAME=".UUCP"
			;;
		esac
	fi
	export DOMAINNAME
fi

if [ -z "$TTY" ] ; then
	TTY="`tty`" ; export TTY
fi
if [ -z "$TTYN" ] ; then
	TTYN="`basename "$TTY"`" ; export TTYN
fi

dirappend ()
{
	if [ $# -le 1 ] ; then
		echo "Usage: dirappend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'$varname
	while [ $# -gt 0 ] ; do
		if [ -d "$1" -a `expr ":$varvalue:" : ".*:$1:.*"` -eq 0 ] ; then
			eval $varname='$'"$varname"'":$1"'
		fi
		shift
	done
	unset varname varvalue
}

dirprepend ()
{
	if [ $# -le 1 ] ; then
		echo "Usage: dirprepend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'$varname
	while [ $# -gt 0 ] ; do
		if [ -d "$1" -a `expr ":$varvalue:" : ".*:$1:.*"` -eq 0 ] ; then
			eval $varname='"$1:"$'"$varname"
		fi
		shift
	done
	unset varname varvalue
}

# system-local user preferences go in here
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile
fi

if "${PATH_IS_OKAY:-false}" ; then
	: # we trust $PATH has been initialized correctly on these machines....
else
	OPATH=$PATH
	PATH="/bin" ; export PATH	# otherwise start fresh...
	dirappend PATH /usr/bin /usr/lbin
fi
export PATH

if [ -z "$LOCAL" ] ; then
	if [ -d /local -a ! -h /local -a -d /local/bin ] ; then
		LOCAL="/local"
	elif [ -d /usr/local -a -d /usr/local/bin ] ; then
		LOCAL="/usr/local"
	else
		LOCAL="/NO-local-FOUND"
	fi
fi
export LOCAL

if [ -z "$CONTRIB" ] ; then
	if [ -d /contrib -a ! -h /contrib -a -d /contrib/bin ] ; then
		CONTRIB="/contrib"
	elif [ -d /usr/contrib -a -d /usr/contrib/bin ] ; then
		CONTRIB="/usr/contrib"
	else
		CONTRIB="/NO-contrib-FOUND"
	fi
fi
export CONTRIB

if [ -z "$PKG" ] ; then
	if [ -d /pkg -a ! -h /pkg -a -d /pkg/bin ] ; then
		PKG="/pkg"
	elif [ -d /usr/pkg -a -d /usr/pkg/bin ] ; then
		PKG="/usr/pkg"
	else
		PKG="/NO-pkg-FOUND"
	fi
fi
export PKG

if [ -z "$OPT" ] ; then
	if [ -d /opt -a ! -h /opt ] ; then
		OPT="/opt"
	elif [ -d /usr/opt ] ; then
		OPT="/usr/opt"
	else
		OPT="/NO-opt-FOUND"
	fi
fi
export OPT

if [ -z "$GNU" ] ; then
	if [ -d /local/gnu -a ! -h /local/gnu -a -d /local/gnu/bin ] ; then
		GNU="/local/gnu"
	elif [ -d /usr/gnu -a -d /usr/gnu/bin ] ; then
		GNU="/usr/gnu"
	elif [ -d /usr/local/gnu -a -d /usr/local/gnu/bin ] ; then
		GNU="/usr/local/gnu"
	else
		GNU="/NO-gnu-FOUND"
	fi
fi
export GNU

if [ -z "$PROJECT" ] ; then
	PROJECT="SCCS"
fi
export PROJECT

if [ -z "$WORKPATH" ] ; then
	WORKPATH="$HOME/work.d"
	dirappend WORKPATH /work/$LOGNAME $LOCAL/work.d/$LOGNAME
fi
export WORKPATH

# TODO: explore more options for this....  (xmkmf?)
# TODO: what if there's more than one?
#
# don't worry about openwin -- it's handled in the ISSUN case below
#
if [ -z "$X11PATH" ] ; then
	# FIXME: this won't work very well if X11R? is multiple names....
	if [ -d /local/X11R? -a ! -h /local/X11R? ] ; then
		X11PATH="`echo /local/X11R?`"
	elif [ -d /usr/X11 -a ! -h /usr/X11 ] ; then
		X11PATH="/usr/X11"
	elif [ -d /usr/X11R? -a ! -h /usr/X11? ] ; then
		X11PATH="`echo /usr/X11R?`"
	elif [ -d /usr/X??? -a ! -h /usr/X??? ] ; then	# X386, for example
		X11PATH="`echo /usr/X???`"
	elif [ -d /usr/local/X11R? -a ! -h /usr/local/X11R? ] ; then
		X11PATH="`echo /usr/local/X11R?`"
	else
		X11PATH="/NO-X11-FOUND"
	fi
	export X11PATH
fi
if [ -z "$X11BIN" ] ; then
	# TODO: this is a best guess that might fail for remote hosts
	if [ -d /usr/bin/X11 -a ! -h /usr/bin/X11 ] ; then
		X11BIN=/usr/bin/X11
	else
		X11BIN=$X11PATH/bin
	fi
	export X11BIN
fi

dirappend PATH /usr/ccs/bin $X11BIN $LOCAL/bin $GNU/bin $CONTRIB/bin $PKG/bin /usr/ucb /usr/bsd
dirappend PATH /usr/games $LOCAL/games $OPT/games/bin

# CDPATH isn't supported in all shells, but it won't hurt....
#
# make sure these directories are fixed in even if they are not
# present at login time.
#
CDPATH=":$HOME:$WORKPATH:$HOME/src:$HOME/src/lib:$HOME/lib"

dirappend CDPATH /usr/src /usr/src/lib /usr/src/cmd /usr/src/add-on /usr/src/uts
dirappend CDPATH /usr/src/bin /usr/src/distrib /usr/src/domestic /usr/src/etc
dirappend CDPATH /usr/src/games /usr/src/gnu /usr/src/include /usr/src/lib
dirappend CDPATH /usr/src/libexec /usr/src/regress /usr/src/sbin /usr/src/share
dirappend CDPATH /usr/src/sys /usr/src/usr.bin /usr/src/usr.sbin
dirappend CDPATH /usr/src/local /usr/src/local/lib /usr/src/local/cmd
dirappend CDPATH /usr/src/gnu/usr.bin /usr/src/gnu/lib /usr/src/gnu/libexec
dirappend CDPATH /usr/pkgsrc
dirappend CDPATH /usr/src/ucbcmd /usr/src/ucblib
dirappend CDPATH $LOCAL/src $LOCAL/src/lib $LOCAL/src/gnu $LOCAL/src/bsd
dirappend CDPATH $LOCAL $LOCAL/lib /opt /usr/lib /usr/spool / /usr

export CDPATH

OMANPATH="$MANPATH" ; export OMANPATH

# don't set MANPATH with 4.4BSD man....
#
if [ -z "$MANPATH" -a ! -r /etc/man.conf ] ; then
	if [ -d /usr/share/man ] ; then
		MANPATH="/usr/share/man"
	else
		MANPATH="/usr/man"
	fi
	export MANPATH
fi
case "$UUNAME" in
web | robohack )
	dirprepend MANPATH $LOCAL/man
	;;
esac
if [ ! -d $LOCAL/share/man ] ; then
	dirappend MANPATH $LOCAL/man
fi
dirprepend MANPATH $LOCAL/share/man $GNU/man $CONTRIB/man $PKG/man $X11PATH/man

ISSUN=false; export ISSUN
if [ -x /usr/bin/sun ] ; then
	if sun ; then
		ISSUN=true
		PATH=`echo $PATH | sed 's/^\/bin://'`
		if [ "`uname -r | sed 's/^\([0-9]*\).*$/\1/'`" -lt 5 ] ; then
			if [ "X$LOGNAME" != "Xroot" ] ; then
				dirprepend PATH /usr/5bin
			else
				dirappend PATH /usr/5bin
			fi
		else
			dirprepend PATH /opt/SUNWspro/bin
		fi
		# XXX FIXME: should use OPENWINHOME ???
		# XXX FIXME: should only do this if DISPLAY set???
		dirappend PATH /usr/openwin/bin /usr/openwin/demo
		dirappend MANPATH /usr/openwin/share/man
	fi
fi

if [ -d $LOCAL/dmdlayers/bin -a "X$TERM" = "Xdmd" ] ; then
	DMD=$LOCAL/dmdlayers ; export DMD
	TOOLS=$DMD/local ; export TOOLS
	dirappend PATH $DMD/bin $TOOLS/bin
	dirprepend MANPATH $DMD/man $TOOLS/man
fi

if [ "X$HOME" != "X/" ] ; then
	dirprepend PATH $HOME/bin
	PATH="${PATH}:"
fi

#
# PATH should finally be set properly!  Just Mh and X11 set below
#

# turn this off by default, turn it on by hand?
mesg n

if [ -r /var/log/smail/logfile ] ; then
	MAILLOG="/var/log/smail/logfile"
elif [ -r /var/spool/smail/log/logfile ] ; then
	MAILLOG="/var/spool/smail/log/logfile"
elif [ -r /usr/spool/smail/log/logfile ] ; then
	MAILLOG="/usr/spool/smail/log/logfile"
elif [ -r $LOCAL/var/log/smail/logfile ] ; then
	MAILLOG="$LOCAL/var/log/smail/logfile"
elif [ -r $LOCAL/spool/smail/log/logfile ] ; then
	MAILLOG="$LOCAL/spool/smail/log/logfile"
else
	MAILLOG="/var/log/smail/logfile"
fi
export MAILLOG

HAVEPRINT=false ; export HAVEPRINT
if expr "`type print`" : '^print is shell builtint$' >/dev/null 2>&1 ; then
	HAVEPRINT=true
fi
HAVEPRINTF=false ; export HAVEPRINTF
if expr "`type printf`" : '^printf is shell builtint$' >/dev/null 2>&1 ; then
	HAVEPRINTF=true
elif expr "`type printf`" : '.* is .*/printf$' >/dev/null 2>&1 ; then
	HAVEPRINTF=true
fi
if $HAVEPRINT ; then
	# use ``$echo'' if any of the other variables...
	echo=print
	nl='\n'
	n=''
	c='\c'
elif $HAVEPRINTF ; then
	# use ``$echo'' if any of the other variables...
	echo=printf
	nl='\n'
	n=''
	c='\c'
else
	echo=echo
	(echo "hi there\c" ; echo " ") >$HOME/echotmp
	# Configure checks to make sure grep returns a status...
	if grep c $HOME/echotmp >/dev/null 2>&1 ; then
		nl=''
		n='-n'
		c=''
	else
		nl='\n'
		n=''
		c='\c'
	fi
	rm -f $HOME/echotmp
fi

if expr "`type mktable`" : '.* is .*/mktable$' >/dev/null 2>&1 ; then
	MKTABLE="mktable"
else
	# a little ditty to throw away comments....
	# TODO: should call mkline (ala smail-3) if available....
	mktable ()
	{
		sed '	/^[ 	]*#/d
			/^[ 	]*$/d
		' ${1+"$@"}
	}
fi

HAVETPUT=false ; export HAVETPUT
if expr "`type tput`" : '.* is .*/tput$' >/dev/null 2>&1 ; then
	HAVETPUT=true
	# WARNING: this only works with SysV compatible tput.
	# Try without the "cr" for NetBSD's tput.
	TERMTESTCMD='tput -T"$ttytype" cr >/dev/null 2>&1'
else
	# WARNING: some tset(1)'s, esp. ULTRIX, fail if stderr is not a tty
	TERMTESTCMD='tset -I -Q "$ttytype" >/dev/null'
fi

HAVEMONTH=false ; export HAVEMONTH
if expr "`type month`" : '.* is .*/month$' >/dev/null 2>&1 ; then
	HAVEMONTH=true
fi

HAVELAYERS=false ; export HAVELAYERS
if expr "`type layers`" : '.* is .*/layers$' >/dev/null 2>&1 ; then
	HAVELAYERS=true
fi

HAVEMUSH=false ; export HAVEMUSH
MAILER=mail ; export MAILER
if [ -s $HOME/.mailer ] ; then
	# mktable just throws away comments....
	MAILER="`mktable $HOME/.mailer`"
elif expr "`type mush`" : '.* is .*/mush$' >/dev/null 2>&1 ; then
	HAVEMUSH=true
	MAILER="mush"
elif expr "`type Mail`" : '.* is .*/mailx$' >/dev/null 2>&1 ; then
	MAILER="Mail"
elif expr "`type mailx`" : '.* is .*/mailx$' >/dev/null 2>&1 ; then
	MAILER="mailx"
fi
case "$MAILER" in
mh )
	if [ -d $CONTRIB/mh ] ; then
		dirprepend PATH $CONTRIB/mh/bin
		dirprepend MANPATH $CONTRIB/mh/man
	elif [ -d $PKG/mh ] ; then
		dirprepend PATH $CONTRIB/mh/bin
		dirprepend MANPATH $CONTRIB/mh/man
	elif [ -d $LOCAL/mh ] ; then
		dirprepend PATH $LOCAL/mh/bin
		dirprepend MANPATH $LOCAL/mh/man
	elif [ -d /usr/mh ] ; then
		dirprepend PATH /usr/mh/bin
		dirprepend MANPATH /usr/mh/man
	elif [ -d $LOCAL/bin/mh ] ;then
		# this is a non-std setup -- $LOCAL/mh/man might not exist
		dirprepend PATH $LOCAL/bin/mh
		dirprepend MANPATH $LOCAL/mh/man
	fi
	;;
nmh )
	if [ -d $CONTRIB/nmh ] ; then
		dirprepend PATH $CONTRIB/nmh/bin
		dirprepend MANPATH $CONTRIB/nmh/man
	elif [ -d $PKG/nmh ] ; then
		dirprepend PATH $CONTRIB/nmh/bin
		dirprepend MANPATH $CONTRIB/nmh/man
	elif [ -d $LOCAL/nmh ] ; then
		dirprepend PATH $LOCAL/nmh/bin
		dirprepend MANPATH $LOCAL/nmh/man
	elif [ -d /usr/nmh ] ; then
		dirprepend PATH /usr/nmh/bin
		dirprepend MANPATH /usr/nmh/man
	elif [ -d $LOCAL/bin/nmh ] ;then
		# this is a non-std setup -- $LOCAL/nmh/man might not exist
		dirprepend PATH $LOCAL/bin/nmh
		dirprepend MANPATH $LOCAL/nmh/man
	fi
	;;
esac

if [ -z "$MAILDIR" ] ; then
	if [ -d /var/mail ] ; then
		MAILDIR="/var/mail"
	elif [ -d /var/spool/mail ] ; then
		MAILDIR="/var/spool/mail"
	elif [ -d /usr/mail ] ; then
		MAILDIR="/usr/mail"
	elif [ -d /usr/spool/mail ] ; then
		MAILDIR="/usr/spool/mail"
	fi
fi
export MAILDIR

# use MAIL instead of MAILPATH, primarily to avoid the clash of using
# a POP specification in MAILPATH for emacs VM
# 
unset MAILPATH
if [ -z "$MAIL" ] ; then
	MAIL=${MAILDIR}/${LOGNAME}
fi
export MAIL

HAVECALENDAR=false ; export HAVECALENDAR
if expr "`type calendar`" : '.* is .*/calendar$' >/dev/null 2>&1 ; then
	HAVECALENDAR=true
fi

HAVEFORTUNE=false ; export HAVEFORTUNE
if expr "`type fortune`" : '.* is .*/fortune$' >/dev/null 2>&1 ; then
	HAVEFORTUNE=true
	FORTUNE=fortune ; export FORTUNE
fi

if expr "`type less`" : '.* is .*/less$' >/dev/null 2>&1 ; then
	PAGER="`type less`"
elif expr "`type more`" : '.* is .*/more$' >/dev/null 2>&1 ; then
	PAGER="`type more`"
else
	PAGER="`type cat`"
fi
PAGER="`expr "$PAGER" : '^.*/\([^/]*\)$'`"; export PAGER
MANPAGER="$PAGER -s"; export MANPAGER
LESS="-eM" ; export LESS

if [ -s "$HOME/.editor" ] ; then
	# mktable just throws away comments....
	EDPREF=`mktable $HOME/.editor` ; export EDPREF
fi

case "$EDPREF" in
emacs | "" )
	if expr "`type emacs`" : '.* is .*/emacs$' >/dev/null 2>&1 ; then
		EDITOR="`type emacs`"
	elif expr "`type jove`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
		EDITOR="`type jove`"
	else
		EDITOR="`type ed`"
	fi
	if expr "`type emacs`" : '.* is .*/emacs$' >/dev/null 2>&1 ; then
		VISUAL="`type emacs`"
		if [ -n "$DISPLAY" -o "$TERM" = "xterm" ] ; then
			if [ -x /usr/bin/id ] ; then
				eval `id | sed 's/[^a-z0-9=].*//'`
				# TODO: maybe not?
				if [ "${uid:=0}" -ne 0 ] ; then
					VISUAL="`type emacsclient`"
				fi
			fi
		fi
	elif expr "`type jove`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
		VISUAL="`type jove`"
	else
		VISUAL="`type vi`"
	fi
	;;
vi )
	if expr "`type nvi`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		EDITOR="`type nvi`"
	elif expr "`type vi`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		EDITOR="`type vi`"
	else
		EDITOR="`type ed`"
	fi
	if expr "`type nvi`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		VISUAL="`type nvi`"
	elif expr "`type vi`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		VISUAL="`type vi`"
	else
		VISUAL="`type none`"
	fi
	;;
* )
	if expr "`type nvi`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		EDITOR="`type nvi`"
	elif expr "`type vi`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		EDITOR="`type vi`"
	else
		EDITOR="`type ed`"
	fi
	if expr "$EDPREF" : '.*/.*$' > /dev/null 2>&1 ; then
		VISUAL="$EDPREF"
	else
		VISUAL="`type $EDPREF`"
	fi
esac
EDITOR="`expr "$EDITOR" : '^.*/\([^/]*\)$'`"; export EDITOR
VISUAL="`expr "$VISUAL" : '^.*/\([^/]*\)$'`"; export VISUAL
EXINIT="set sm" ; export EXINIT

if [ -z "$CVSROOT" ] ; then
	CVSROOT="$LOCAL/src-CVS" ; export CVSROOT
fi

##ENSCRIPT="$ENSCRIPT -G" ; export ENSCRIPT

if [ -x $LOCAL/bin/diff ] ; then
	DIFF="$LOCAL/bin/diff" ; export DIFF
elif expr "`type gdiff`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
	DIFF="`type gdiff`" ; export DIFF
fi

if expr "`type auplay`" : '.* is .*/auplay$' >/dev/null 2>&1 ; then
	AUDIOPLAYER="`type auplay`"
elif expr "`type audioplay`" : '.* is .*/more$' >/dev/null 2>&1 ; then
	AUDIOPLAYER="`type audioplay`"
fi
if [ -n "$AUDIOPLAYER" ] ; then
	AUDIOPLAYER="`expr "$AUDIOPLAYER" : '^.*/\([^/]*\)$'`"; export AUDIOPLAYER
fi

MONTH="AIKO" ; export MONTH

RNINIT="-v -M -S -T -i=8 -g2" ; export RNINIT
TRNINIT="$HOME/.trninit" ; export TRNINIT

# set terminal type and tty settings, etc....
#
if [ "X$argv0" != "X.xsession" -a "X$argv0" != "X.xinitrc" ] ; then
	echo "Re-setting terminal preferences...."
	if [ -r "$HOME/.stty" ] ; then
		. $HOME/.stty
	else
		stty erase '^h' intr '^?' kill '^u' -ixany echo echoe echok
	fi
	if [ "$EMACS" = t -o "$TERM" = emacs ]; then
		echo "Turning off echo for an emacs shell...."
		stty -echo
	fi

	case "$UUNAME" in
	robohack | toile | wombat | spinne | tar | web | weirdo | most | very | isit | pretty )
		# we trust that everything is all set up as it should be on
		# sites we know, except for personal preferences set above...
		:
		;;
	* )
		# this is a function so it can be used interactively after login....
		#
		get_newterm ()
		{
			while [ "X$TERM" != "X$ttytype" ] ; do
				$echo $n "Please enter your terminal type [$ttytype]: $c"
				read newttytype
				if [ -n "$newttytype" ] ; then
					ttytype="$newttytype"
				fi
				if [ "$ttytype" = "dumb" ] ; then
					TERM="dumb"		# guarantee a way out of this!
					break
				fi
				if eval $TERMTESTCMD ; then
					TERM="$ttytype"
				else
					echo "Sorry, I don't know that terminal type."
					$echo "Use 'dumb' if you are stuck.${nl}"
				fi
			done
			unset newttytype
		}

		case "$TERM" in
		""|network|dialup|unknown|none)
			ttytype=dumb
			get_newterm
			;;
		esac

		if $HAVETPUT ; then
			# TODO: we could use 'tput init' on some systems, no?
			:
		else
			if expr "`type tset`" : '.* is .*/tset$' >/dev/null 2>&1 ; then
				# On BSD, without the "-I" it uses /etc/termcap....
				tset -I -r
			else
				echo "NOTICE:  I don't know how to set up your terminal."
			fi
		fi

		case $TTYN in
		tty[p-zP-Z]*|vt*|vg*|console)
			echo "Setting terminal for 8-bit transparency...."
			stty cs8 -istrip -parenb
			;;
		esac

		# try setting up for X11 if possible....
		case "$TERM" in
		xterm|sun|pc3|ibmpc3)
			# users will have to set their own $DISPLAY....
			dirappend PATH $X11PATH/bin
			dirappend MANPATH $X11PATH/man
			;;
		esac

		if [ -z "$SSH_TTY" ]; then
			echo "Your terminal is port $TTY."
		else
			echo "Secure connection from $SSH_CLIENT on $SSH_TTY (tty $TTY)\n"
		fi

		export TERM
		;;
	esac
fi

# TODO: find some way to see if login(1) ran, or xterm(n) started us
# TODO: since login(1) checks for mail too, but xterm(n) doesn't.
#
# check your mail...
if expr "`type messages`" : '.* is .*/messages$' >/dev/null 2>&1 ; then
	messages
else
	[ -x /bin/mail ] && /bin/mail -e
	HAVENEWMAIL=$?
	if $HAVEMUSH && [ $HAVENEWMAIL -eq 0 ] ; then
		echo 'You have mail:'
		mush -H:n
	elif [ "$MAILER" = mh -a $HAVENEWMAIL -eq 0 ] ; then
		echo "Change this line in $HOME/.profile to show new mail using MH"
	elif [ $HAVENEWMAIL -eq 0 ] ; then
		echo "You have some mail!"
	fi
	unset HAVENEWMAIL
fi


# TODO: this needs to be a lot smarter....
#
if [ -d $HOME/lib/terminfo ] ; then
	case $TERM in
	at386*|AT386*|386AT*|386at*|dmd|dmd-myx|ibmpc3|pc3)
		TERMINFO=$HOME/lib/terminfo ; export TERMINFO
		;;
	esac
fi

if [ "X$argv0" != "X.xsession" -a "X$argv0" != "X.xinitrc" ] ; then
	# WARNING: some stupid stty's cause this to fail!!!!
	# eg., ULTRIX V4.3 stty(1) 'cause it uses stdout, not stdin....
	SANE="`stty -g`" ; export SANE
fi

# one thing we assume here is that PS1 will be set in .*login or $ENV
#
if [ ${RANDOM:-0} -ne ${RANDOM:-0} -a -z "${BASH}" ] ; then
	# TODO: try to remember why we don't trust this...
	SHELL=""
	[ -x $PKG/bin/ksh ] && export SHELL="$PKG/bin/ksh"
	[ -x $CONTRIB/bin/ksh ] && export SHELL="$CONTRIB/bin/ksh"
	[ -x $LOCAL/bin/ksh ] && export SHELL="$LOCAL/bin/ksh"
	[ -z "$SHELL" -a -x /usr/bin/ksh ] && export SHELL="/usr/bin/ksh"
	[ -z "$SHELL" -a -x /bin/ksh ] && export SHELL="/bin/ksh"
	if [ -r $HOME/.kshlogin ] ; then
		. $HOME/.kshlogin
	fi
elif [ ${RANDOM:-0} -ne ${RANDOM:-0} -a -n "${BASH}" ] ; then
	if [ -r $HOME/.bashlogin ] ; then
		. $HOME/.bashlogin
	fi
elif [ "`echo ~`" = "$HOME" ] ; then
	# this will only be modern ash (eg. from 4.4BSD or newer)
	# TODO: actually, maybe this should be a Posix shell environment...
	if [ -r $HOME/.ashlogin ] ; then
		. $HOME/.ashlogin
	fi
elif [ -r $HOME/.shlogin ] ; then
	if [ -r $HOME/.shlogin ] ; then
		. $HOME/.shlogin
		# TODO: maybe this should be done last?
		if [ -n "$ENV" -a -r "$ENV" ] ; then
			. $ENV
		else
			if [ "X$LOGNAME" = "Xroot" ] ; then
				PS1="[$TTYN]<$LOGNAME@$UUNAME> # "
			else
				PS1="[$TTYN]<$LOGNAME@$UUNAME> $ "
			fi
		fi
	fi
else
	if [ "X$LOGNAME" = "Xroot" ] ; then
		PS1="[$TTYN]<$LOGNAME@$UUNAME> # "
	else
		PS1="[$TTYN]<$LOGNAME@$UUNAME> $ "
	fi
fi

if $HAVELAYERS && expr "`type ismpx`" : '.* is .*/ismpx$' >/dev/null 2>&1 ; then
	: might just be running layers
else
	# otherwise it's just not possible....
	ismpx ()
	{
		false
	}
fi

HAVEX=false ; export HAVEX
if expr "`type xinit`" : '.* is .*/xinit$' >/dev/null 2>&1 ; then
	HAVEX=true
fi

if $HAVEX && [ "X$argv0" != "X.xinitrc" -a "X$argv0" != "X.xsession" ] ; then
	case "$TTYN" in
	console|vg*|vt*|ttyc*)
		case "$TERM" in
		sun|pc3|at386|AT386)
			trap '' 2
			echo ""
			echo $n "Do you want to start X? ([y]/n) $c"
			read yn
			trap 2
			case "$yn" in
				"" | [yY]*)
				trap '' 2
				xinit
				tput clear
				exec sleep 1
				;;
			*)
				echo "OK, not starting X..."
				;;
			esac
			;;
		esac
		;;
	esac
fi

if $HAVELAYERS && [ "X$TERM" = "Xdmd" -a "`ismpx`" != "yes" ] ; then
	trap '' 2
	echo ""
	$echo $n "Do you want to start layers? ([y]/n/debug) $c"
	read yn
	trap 2
	case "$yn" in
	"" | [yY]* | d*)
		if expr "$yn" : '^d.*' >/dev/null ; then
			layers=layers-DEBUG
		else
			layers=layers
		fi
		# TODO: maybe not?
		if [ "$VISUAL" = "emacs" ] ; then
			VISUAL="emacsclient" ; export VISUAL
		fi
		LAYERSPID=$$ ; export LAYERSPID
		rc=.${TERM}rc
		# TODO: think about dmdmyx here....
		TERM=dmd; export TERM
		stty -ixon -ixoff -ixany
		if [ -s $HOME/$rc ] ; then
			exec $layers -f $rc 2>> $HOME/tmp/layers.stderr
		else
			exec $layers 2>> $HOME/tmp/layers.stderr
		fi
		echo "Couldn't exec layers."
		stty ixon ixoff -ixany
		;;
	*)
		echo "OK, not starting layers..."
		;;
	esac
fi

#
# NOTE:  we don't get here the first time if we're starting a windo system, so
# for first time in for window systems which emulate login shells in each window
#

# TODO:  should use $HAVEFORTUNE and $FORTUNE
if [ -x /usr/games/fortune ] ; then
	/usr/games/fortune
elif [ -x $LOCAL/games/fortune ] ; then
	$LOCAL/games/fortune
fi
if [ -r calendar -o -r .month ] ; then
	$echo "${nl}Today's Events:"
	if $HAVEMONTH && [ -r .month ] ; then
		month -B
		#		monthd -i5
	fi
	if $HAVECALENDAR && [ -r calendar ] ; then
		calendar
	fi
fi
if [ -d $HOME/notes ] ; then
	(
		cd $HOME/notes
		echo "${nl}You have notes on:"
		ls -C
	)
fi
if [ -r $HOME/.trninit$TERM ] ; then
	TRNINIT="$HOME/.trninit$TERM" ; export TRNINIT
fi

# minor cleanup
#
if [ ${RANDOM:-0} -eq ${RANDOM:-0} ] ; then
	unset RANDOM
fi

# TODO: do something with msgs(1) if needed....

# NOTE: trick 4.4BSD shell into -E by putting it in here, 'cause you can't
# "set -o emacs" in .ashrc, as that'll cause it to dump core....
#
if [ -s $HOME/.shell -a "X$argv0" != "X.xinitrc" -a "X$argv0" != "X.xsession" ] ; then
	# mktable just throws away comments....
	exec `mktable $HOME/.shell`
fi

cd

# End Of File
