#
#	.profile - for either sh, ksh, or ash (if type is defined).
#
#ident	"@(#)HOME:.profile	8.14	95/06/07 22:39:47 (woods)"

#
# Assumptions:
#
#	- standard environment has been set by login(1)
#	- $argv0 is `basename $0` from .xinitrc or .xsession
#

# Files referenced:
#
#	$HOME/.ashtype		- sourced once, if readable and if running ash(1)
#	$HOME/.ashlogin		- sourced once, if running ash(1)
#	$HOME/.editor		- name of prefered text editor command
#	$HOME/.kshlogin		- sourced once, if running ksh(1)[, or bash(1)?]
#	$HOME/.kshlogout	- set on trap 0, if running ksh(1)[, or bash(1)?]
#	$HOME/.localprofile	- sourced once, near end of this file
#	$HOME/.mailer		- name of prefered MUA command
#	$HOME/.shell		- mktable'd and exec'ed as shell (see end of this file)
#	$HOME/.shfuncs		- sourced once, and pathname set in $ENV
#	$HOME/.shlogin		- sourced once
#	$HOME/.shlogout		- set on trap 0
#	$HOME/.stty		- sourced for stty command(s), etc. just before tset(1)
#	$HOME/.trninit		- pathname set as value for $TRNINIT

if [ -r $HOME/.kshlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} ] ; then
	trap '. $HOME/.kshlogout ; exit $?' 0
elif [ -r $HOME/.shlogout ] ; then
	trap '. $HOME/.shlogout ; exit $?' 0
fi

if [ `echo ~` = $HOME -a -r .ashtype ] ; then
	# TODO: actually, maybe this should be a Posix shell environment...
	. $HOME/.ashtype
fi

if [ -z "$UUNAME" ] ; then
	if expr "`type uuname`" : '.* is .*/uuname$' >/dev/null 2>&1 ; then
		UUNAME="`uuname -l`" ; export UUNAME
	else
		UUNAME="`hostname`" ; export UUNAME
	fi
fi

if [ -z "$DOMAINNAME" ] ; then
	case "$UUNAME" in
	toile | oldweb )		# 386/ix machines
		DOMAINNAME=".web.apc.org" ; export DOMAINNAME
		;;
	* )
		if expr "`type domainname`" : '.* is .*/domainname$' >/dev/null 2>&1 ; then
			DOMAINNAME="`domainname`" ; export DOMAINNAME
		else
			# these cases for machines without domainname....
			#
			case "$UUNAME" in
			weirdo )
				DOMAINNAME=".weird.com" ; export DOMAINNAME
				;;
			* )
				DOMAINNAME=".UUCP" ; export DOMAINNAME
				;;
			esac
		fi
		;;
	esac
fi

if [ -z "$TTY" ] ; then
	TTY="`tty`" ; export TTY
fi
if [ -z "$TTYN" ] ; then
	TTYN="`basename $TTY`" ; export TTYN
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

case "$UUNAME" in
robohack | kuma | araignee | tar | spinne | toile | wombat | weirdo | most | very | isit )
	# we trust $PATH has been initialized correctly on these machines....
	;;
* )
	OPATH=$PATH
	PATH="/bin" ; export PATH	# start fresh...
	dirappend PATH /usr/bin /usr/lbin
	;;
esac

if [ -z "$LOCAL" ] ; then
	if [ -d /local -a -d /local/bin ] ; then
		LOCAL="/local" ; export LOCAL
	elif [ -d /usr/local -a -d /usr/local/bin ] ; then
		LOCAL="/usr/local" ; export LOCAL
	else
		LOCAL="/NO-local-FOUND" ; export LOCAL
	fi
fi

if [ -z "$CONTRIB" ] ; then
	if [ -d /contrib -a -d /contrib/bin ] ; then
		CONTRIB="/contrib" ; export CONTRIB
	elif [ -d /usr/contrib -a -d /usr/contrib/bin ] ; then
		CONTRIB="/usr/contrib" ; export CONTRIB
	else
		CONTRIB="/NO-contrib-FOUND" ; export CONTRIB
	fi
fi

if [ -z "$GNU" ] ; then
	if [ -d /local/gnu -a -d /local/gnu/bin ] ; then
		GNU="/local/gnu" ; export GNU
	elif [ -d /usr/gnu -a -d /usr/gnu/bin ] ; then
		GNU="/usr/gnu" ; export GNU
	elif [ -d /usr/local/gnu -a -d /usr/local/gnu/bin ] ; then
		GNU="/usr/local/gnu" ; export GNU
	else
		GNU="/NO-gnu-FOUND" ; export GNU
	fi
fi

# TODO: explore more options for this....  (xmkmf?)
# TODO: what if there's more than one?
#
if [ -z "$X11PATH" ] ; then
	if [ -d /local/X11R? ] ; then
		X11PATH="`echo /local/X11R?`" ; export X11PATH
	elif [ -d /usr/X11R? ] ; then
		X11PATH="`echo /usr/X11R?`" ; export X11PATH
	elif [ -d /usr/X? ] ; then
		X11PATH="`echo /usr/X11R?`" ; export X11PATH
	elif [ -d /usr/local/X11R? ] ; then
		X11PATH="`echo /usr/local/X11R?`" ; export X11PATH
	else
		X11PATH="/NO-X11-FOUND" ; export X11PATH
	fi
fi

dirappend PATH /usr/bin/X11 $LOCAL/bin $GNU/bin $CONTRIB/bin /usr/ucb
dirappend PATH /usr/games $LOCAL/games

# don't set MANPATH with 4.4BSD man....
#
if [ -z "$MANPATH" -a ! -r /etc/man.conf ] ; then
	if [ -d /usr/share/man ] ; then
		MANPATH="/usr/share/man" ; export MANPATH
	else
		MANPATH="/usr/man" ; export MANPATH
	fi
fi
OMANPATH="$MANPATH" ; export OMANPATH
dirprepend MANPATH $LOCAL/share/man $GNU/man $CONTRIB/man $X11PATH/man
#
# TODO: We also need to strip $LOCAL/man if it's a symlink -> $LOCAL/share/man
# TODO: but we can't depend on 'test -h' being available or working....
#
case "$UUNAME" in
web | robohack )
	dirprepend MANPATH $LOCAL/man
	;;
esac

ISSUN=false; export ISSUN
if [ -x /usr/bin/sun ] ; then
	if sun ; then
		ISSUN=true
		PATH=`echo $PATH | sed 's/^\/bin://'`
		if [ "$LOGNAME" != root ] ; then
			dirprepend PATH /usr/5bin
		else
			dirappend PATH /usr/5bin
		fi
		dirappend PATH /usr/openwin/bin
		dirappend MANPATH /usr/openwin/share/man
	fi
fi

if [ -d $LOCAL/dmdlayers/bin -a "$TERM" = "dmd" ] ; then
	DMD=$LOCAL/dmdlayers ; export DMD
	TOOLS=$DMD/local ; export TOOLS
	dirappend PATH $DMD/bin $TOOLS/bin
	dirprepend MANPATH $DMD/man $TOOLS/man
fi

if [ "$HOME" != "/" ] ; then
	dirprepend PATH $HOME/bin
	PATH="${PATH}:"
fi

if [ -r /usr/spool/smail/log/logfile ] ; then
	MAILLOG="/usr/spool/smail/log/logfile" ; export MAILLOG
elif [ -r /var/spool/smail/log/logfile ] ; then
	MAILLOG="/var/spool/smail/log/logfile" ; export MAILLOG
elif [ -r $LOCAL/lib/smail/logfile ] ; then
	MAILLOG="$LOCAL/lib/smail/logfile" ; export MAILLOG
elif [ -r $LOCAL/lib/smail/mail.log ] ; then
	MAILLOG="$LOCAL/lib/smail/mail.log" ; export MAILLOG
fi

(echo "hi there\c" ; echo " ") >$HOME/echotmp
# Configure checks to make sure grep returns a status...
if grep c echotmp >/dev/null 2>&1 ; then
	n='-n'
	c=''
else
	n=''
	c='\c'
fi
rm -f $HOME/echotmp

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
esac

HAVECALENDAR=false ; export HAVECALENDAR
if expr "`type calendar`" : '.* is .*/calendar$' >/dev/null 2>&1 ; then
	HAVECALENDAR=true
fi

HAVEFORTUNE=false ; export HAVEFORTUNE
if expr "`type fortune`" : '.* is .*/fortune$' >/dev/null 2>&1 ; then
	HAVEFORTUNE=true
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

MONTH="AIKO" ; export MONTH

RNINIT="-v -M -S -T -i=8 -g2" ; export RNINIT
TRNINIT="$HOME/.trninit" ; export TRNINIT

# set terminal type and tty settings, etc....
#
if [ "$argv0" != ".xsession" -a "$argv0" != ".xinitrc" ] ; then
	echo "Re-setting terminal preferences...."
	if [ -r "$HOME/.stty" ] ; then
		. $HOME/.stty
	else
		stty erase '^h' intr '^?' kill '^u' -ixany echo echoe echok
	fi

	case "$UUNAME" in
	robohack | toile | wombat | spinne | tar | web | weirdo | most | very )
		# we trust that everything is all set up as it should be on
		# sites we know, except for personal preferences set above...
		:
		;;
	* )
		# this is a function so it can be used interactively....
		#
		get_newterm ()
		{
			while [ "$TERM" != "$ttytype" ] ; do
				echo $n "Please enter your terminal type [$ttytype]: $c"
				read newttytype
				if [ -n "$newttytype" ] ; then
					ttytype="$newttytype"
				fi
				# NOTE: we assume tput is everywhere these days....
				if tput -T"$ttytype" cr >/dev/null 2>&1 ; then
					TERM="$ttytype"
				else
					echo "Sorry, I don't know that terminal type."
					echo "Use 'dumb' if you are stuck."
					echo ""
				fi
			done
			unset newttytype
		}

		case "$TERM" in
		""|network|dialup|unknown)
			ttytype=dumb
			get_newterm
			;;
		esac

		if expr "`type tset`" : '.* is .*/tset$' >/dev/null 2>&1 ; then
			# On BSD, without the "-I" it uses /etc/termcap....
			# TODO: tset might not work like this everywhere (clears screen
			# TODO: and doesn't print erase/kill settings on SysVr4)
			tset -I -r
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
			dirappend PATH /usr/bin/X11 $X11PATH/bin
			dirappend MANPATH /usr/share/X11/man $X11PATH/man
			;;
		esac

		echo "Your terminal is port $TTY."
		export TERM
		;;
	esac
fi

# TODO: find some way to see if login(1) ran, or xterm(n) started us
# TODO: since login(1) checks for mail too, but xterm(n) doesn't.
#
# check your mail...
#
case "$UUNAME" in
robohack | toile | wombat )
	# /etc/profile or login(1) does this for us
	:
	;;
* )
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
	;;
esac

# TODO: this needs to be a lot smarter....
#
if [ -d $HOME/lib/terminfo ] ; then
	case $TERM in
	at386*|AT386*|386AT*|386at*|dmd|dmd-myx|ibmpc3|pc3)
		TERMINFO=$HOME/lib/terminfo ; export TERMINFO
		;;
	esac
fi

if [ "$argv0" != ".xsession" -a "$argv0" != ".xinitrc" ] ; then
	SANE="`stty -g`" ; export SANE
fi

# one thing we assume here is that PS1 will be set in .*login or $ENV
#
if [ ${RANDOM:-0} -ne 0 ] ; then
	# TODO: try to remember why we don't trust this...
	SHELL=""
	[ -x $LOCAL/bin/ksh ] && export SHELL="$LOCAL/bin/ksh"
	[ -z "$SHELL" -a -x /usr/bin/ksh ] && export SHELL="/usr/bin/ksh"
	[ -z "$SHELL" -a -x /bin/ksh ] && export SHELL="/bin/ksh"
	if [ -r $HOME/.kshlogin ] ; then
		. $HOME/.kshlogin
	fi
elif [ `echo ~` = $HOME ] ; then
	# TODO: actually, maybe this should be a Posix shell environment...
	if [ -r $HOME/.ashlogin ] ; then
		. $HOME/.ashlogin
	fi
elif [ -r $HOME/.shlogin ] ; then
	if [ -r $HOME/.shlogin ] ; then
		. $HOME/.shlogin
		# TODO: maybe this should be done last?
		if [ -n "$ENV" ] ; then
			. $ENV
		else
			if [ "$LOGNAME" = root ] ; then
				PS1="[$TTYN]<$LOGNAME@$UUNAME> # "
			else
				PS1="[$TTYN]<$LOGNAME@$UUNAME> $ "
			fi
		fi
	fi
else
	if [ "$LOGNAME" = root ] ; then
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

if $HAVEX && [ "$argv0" != ".xinitrc" -a "$argv0" != ".xsession" ] ; then
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

if $HAVELAYERS && [ "$TERM" = "dmd" -a "`ismpx`" != "yes" ] ; then
	trap '' 2
	echo ""
	echo $n "Do you want to start layers? ([y]/n/debug) $c"
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
	echo "\nToday's Events:"
	if $HAVEMONTH && [ -r .month ] ; then
		month -B
		#		monthd -i5
	fi
	if $HAVECALENDAR && [ -r calendar ] ; then
		calendar
	fi
fi
if [ -d $HOME/notes ] ; then
	cd $HOME/notes
	if [ `ls|wc -w` -ne 0 ] ; then
		echo '\nYou have notes on: ' * '\n'
	fi
	cd $HOME
fi
if [ -r $HOME/.trninit$TERM ] ; then
	TRNINIT="$HOME/.trninit$TERM" ; export TRNINIT
fi

# final system-local user preferences go in here
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile
fi

# TODO: do something with msgs(1) if needed....

# NOTE: trick 4.4BSD shell into -E by putting it in here, 'cause you can't
# "set -o emacs" in .ashrc, as that'll cause it to dump core....
#
if [ -s $HOME/.shell -a "$argv0" != ".xinitrc" -a "$argv0" != ".xsession" ] ; then
	# mktable just throws away comments....
	exec `mktable $HOME/.shell`
fi

# End Of File
