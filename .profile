#
#	.profile - for either sh, or ksh.
#
#ident	"@(#)HOME:.profile	6.2	94/07/11 12:49:23 (woods)"

if [ -r $HOME/.kshlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} ] ; then
	trap '. $HOME/.kshlogout ; exit $?' 0
elif [ -r $HOME/.shlogout ] ; then
	trap '. $HOME/.shlogout ; exit $?' 0
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
	scilink )		# the one I want!
		DOMAINNAME=".planix.com" ; export DOMAINNAME
		;;
	kuma )			# the *real* one!
		DOMAINNAME=".web.net" ; export DOMAINNAME
		;;
	toile | web )		# 386/ix machines
		DOMAINNAME=".web.apc.org" ; export DOMAINNAME
		;;
	* )
		if expr "`type domainname`" : '.* is .*/domainname$' >/dev/null 2>&1 ; then
			DOMAINNAME="`domainname`" ; export DOMAINNAME
		else
			case "$UUNAME" in
			foobar )
				DOMAINNAME="" ; export DOMAINNAME
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
robohack | araignee | tar | spinne | toile | wombat )
	;;
* )
	PATH="/bin" ; export PATH
	dirappend PATH /usr/bin /usr/lbin
	;;
esac

case "$UUNAME" in
tar )
	APCSRCDIR=/kuma/big/web/work/apc ; export APCSRCDIR
	APCCONFIG=/kuma/big/web/work/apc/configure ; export APCCONFIG
	if [ `expr "$MFLAGS" : ".*$APCCONFIG.*"` -eq 0 ] ; then
		MFLAGS="$MFLAGS -I $APCCONFIG" ; export MFLAGS
	fi
	;;
web )
	APCCONFIG=/etc/apc ; export APCCONFIG
	;;
esac

if [ -z "$LOCAL" ] ; then
	if [ -d /local ] ; then
		LOCAL="/local" ; export LOCAL
	elif [ -d /usr/local ] ; then
		LOCAL="/usr/local" ; export LOCAL
	else
		echo -- "$0: WARNING: this system doesn't seem to have a LOCAL root"
		LOCAL="/local" ; export LOCAL
	fi
fi

if [ -z "$GNU" ] ; then
	if [ -d /local/gnu ] ; then
		GNU="/local/gnu" ; export GNU
	elif [ -d /usr/gnu ] ; then
		GNU="/usr/gnu" ; export GNU
	elif [ -d /usr/local/gnu ] ; then
		GNU="/usr/local/gnu" ; export GNU
	else
		echo -- "$0: WARNING: this system doesn't seem to have a GNU root"
		GNU="/gnu" ; export GNU
	fi
fi

dirappend PATH /usr/bin/X11 $LOCAL/bin $GNU/bin /usr/ucb
dirappend PATH /usr/games $LOCAL/games

if [ -z "$MANPATH" ] ; then
	if [ -d /usr/share/man ] ; then
		MANPATH="/usr/share/man" ; export MANPATH
	else
		MANPATH="/usr/man" ; export MANPATH
	fi
fi
dirprepend MANPATH $LOCAL/share/man $LOCAL/man $GNU/man /X11R5/man

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

HAVEMONTH=false ; export HAVEMONTH
if expr "`type month`" : '.* is .*/month$' >/dev/null 2>&1 ; then
	HAVEMONTH=true
fi

HAVELAYERS=false ; export HAVELAYERS
if expr "`type layers`" : '.* is .*/layers$' >/dev/null 2>&1 ; then
	HAVELAYERS=true
fi

HAVEMUSH=false ; export HAVEMUSH
if expr "`type mush`" : '.* is .*/mush$' >/dev/null 2>&1 ; then
	HAVEMUSH=true
	MAILER="mush" ; export MAILER
fi

HAVECALENDAR=false ; export HAVECALENDAR
if expr "`type calendar`" : '.* is .*/calendar$' >/dev/null 2>&1 ; then
	HAVECALENDAR=true
fi

HAVEFORTUNE=false ; export HAVEFORTUNE
if expr "`type fortune`" : '.* is .*/fortune$' >/dev/null 2>&1 ; then
	HAVEFORTUNE=true
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

if expr "`type less`" : '.* is .*/less$' >/dev/null 2>&1 ; then
	PAGER="`type less`"
elif expr "`type more`" : '.* is .*/more$' >/dev/null 2>&1 ; then
	PAGER="`type more`"
else
	PAGER="`type cat`"
fi
PAGER="`expr "$PAGER" : '^.*/\([^/]*\)$'`"; export PAGER
LESS="-eM" ; export LESS

if expr "`type emacs`" : '.* is .*/emacs$' >/dev/null 2>&1 ; then
	EDITOR="`type emacs`"
elif expr "`type jove`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
	EDITOR="`type jove`"
else
	EDITOR="`type ed`"
fi
EDITOR="`expr "$EDITOR" : '^.*/\([^/]*\)$'`"; export EDITOR

if expr "`type emacs`" : '.* is .*/emacs$' >/dev/null 2>&1 ; then
	if [ -n "$DISPLAY" -o \( "$TERM" = "xterm" -a -n "$APCCONFIG" -a "$UUNAME" = "web" \) ] ; then
		VISUAL="`type emacsclient`"
	else
		VISUAL="`type emacs`"
	fi
elif expr "`type jove`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
	VISUAL="`type jove`"
else
	VISUAL="`type vi`"
fi
VISUAL="`expr "$VISUAL" : '^.*/\([^/]*\)$'`"; export VISUAL

EXINIT="set sm" ; export EXINIT

if [ -n "$APCCONFIG" ] ; then
	# for pnotes message composition...
	#
	APCEDIT="`type $VISUAL`" ; export APCEDIT
	APCEDIT="`expr "$APCEDIT" : '^[^/]*\(/.*\)$'`"
	#
	APCEDITOR="`type $VISUAL`" ; export APCEDITOR
	APCEDITOR="`expr "$APCEDITOR" : '^[^/]*\(/.*\)$'`"
	INTERFACE="apcsh" ; export INTERFACE
	dirappend PATH $APCCONFIG/bin /apc/bin /apc/xbin /apc/lbin
	dirappend PATH /usr/local/apc/bin /usr/local/apc/xbin
	dirprepend MANPATH /usr/catman
	dirappend MANPATH /apc/man
fi

CVSROOT="$LOCAL/src-CVS" ; export CVSROOT

##ENSCRIPT="$ENSCRIPT -G" ; export ENSCRIPT

if [ -x $LOCAL/bin/diff ] ; then
	DIFF="$LOCAL/bin/diff" ; export DIFF
elif expr "`type gdiff`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
	DIFF="`type gdiff`" ; export DIFF
fi

MONTH="AIKO" ; export MONTH

RNINIT="-v -M -S -T -i=8 -g2" ; export RNINIT
TRNINIT="$HOME/.trninit" ; export TRNINIT

# set terminal type..
case "$UUNAME" in
robohack )
	: we trust that everything is all set up as it should be....
	;;
toile | wombat | araignee | spinne )
	if [ -r $HOME/.kshedit ] ; then
		if grep "^set -o vi" $HOME/.kshedit ; then
			# this horrible hack assumes that vi users
			# will also have the prevailing default stty
			# settings in /etc/profile....
			: real men use emacs!
		else
			# we don't want no stinking defaults!
			stty intr '^?'
		fi
	fi
	;;
* )
	echo "Re-setting terminal preferences...."
	stty erase '^h' intr '^?' kill '^u' -ixany echo echoe echok
	TERM=`tset -r - -m dmd:dmd -m dmd-myx:dmd-myx -m sun:sun -m xterm:xterm -m vt100:vt100 -m vt102:vt102 -m at386:at386 -m AT386:at386 -m :?$TERM`
	case $TTY in
	/dev/tty[p-zP-Z]* | /dev/vt* | /dev/console )
		echo "Setting up an 8-bit tty environment...."
		stty cs8 -istrip -parenb
		;;
	esac
	if [ -d $HOME/lib/terminfo ] ; then
		case $TERM in
		at386*|AT386*|386AT*|386at*)
			TERMINFO=$HOME/lib/terminfo ; export TERMINFO
			;;
		esac
	fi
	if [ -d $HOME/.pn ] ; then
		echo "$TERM" > $HOME/.pn/TERM
		stty -g > $HOME/.pn/SANE
		echo "$TERMINFO" > $HOME/.pn/TERMINFO
	fi
	if $HAVEMUSH && /bin/mail -e ; then
		echo 'You have mail:'
		mush -H:n
	fi
	;;
esac

SANE="`stty -g`" ; export SANE

if [ ${RANDOM:-0} -ne 0 ] ; then
	SHELL=""
	[ -x $LOCAL/bin/ksh ] && export SHELL="$LOCAL/bin/ksh"
	[ -z "$SHELL" -a -x /usr/bin/ksh ] && export SHELL="/usr/bin/ksh"
	[ -z "$SHELL" -a -x /bin/ksh ] && export SHELL="/bin/ksh"
	. $HOME/.kshlogin
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

HAVEX=false ; export HAVEX
if expr "`type xinit`" : '.* is .*/xinit$' >/dev/null 2>&1 ; then
	HAVEX=true
fi

if $HAVEX && [ "`tty`" = "/dev/console" ] ; then
	case "$TERM" in
	sun|pc3|at386|AT386)
		trap '' 2
		echo "\nDo you want to start X? ([y]/n) \c"
		read yn
		trap 2
		case "$yn" in
			"" | [yY]*)
			TRNINIT="$HOME/.trninitX" ; export TRNINIT
			trap '' 2
			xinit
			tput clear
			$HAVEFORTUNE && fortune
			exec sleep 1
			;;
		*)
			if $HAVEMONTH && [ -r .month ] ; then
				monthd -i5
			fi
			;;
		esac
		;;
	*)
		;;
	esac
fi

if $HAVELAYERS && [ "$TERM" = "dmd" -a "`ismpx`" != "yes" ] ; then
	trap '' 2
	echo "\nDo you want to start layers? ([y]/n/debug) \c"
	read yn
	trap 2
	case "$yn" in
	"" | [yY]* | d*)
		if expr "$yn" : '^d.*' >/dev/null ; then
			layers=layers-DEBUG
		else
			layers=layers
		fi
		TRNINIT="$HOME/.trninitdmd" ; export TRNINIT
		if [ "$VISUAL" = "emacs" ] ; then
			VISUAL=emacsclient ; export VISUAL
		fi
		LAYERSPID=$$ ; export LAYERSPID
		rc=.${TERM}rc
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
		if $HAVEMONTH && [ -r .month ] ; then
			monthd -i5
		fi
		;;
	esac
fi

# End Of File
