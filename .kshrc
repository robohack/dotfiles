#
#	.kshrc - per-interactive-shell startup stuff
#
#ident	"@(#)HOME:.kshrc	34.2	13/12/02 18:06:56 (woods)"

# WARNING:
# don't put comments at the bottom or you'll bugger up ksh-11/16/88e's history

# Assumptions:
#
#	$ISSUN			- must be "true" or "false; set by ~/.profile

# Files referenced:
#
#	$HOME/.kshsccs		- sourced, if it is readable
#	$HOME/.kshpwd		- sourced, if it is readable
#	$HOME/.kshlocal		- sourced, if it is readable
#	$HOME/.kshedit		- sourced, if it is readable else gmacs editing set
#	$HOME/.kshdir		- dir autoload aliases set, if it is readable

#set -o nolog		# no functions in $HISTFILE

export PATH="$PATH"

# XXX these variants using ${..##..} and ${..%%..} instead of $(expr ...) may be better

# add to end of path
append2path()
{
	if ! eval test -z "\"\${$1##*:$2:*}\"" -o -z "\"\${$1%%*:$2}\"" -o -z "\"\${$1##$2:*}\"" -o -z "\"\${$1##$2}\"" ; then
		eval "$1=\$$1:$2"
	fi
}

# add to front of path
prepend2path()
{
	if ! eval test -z "\"\${$1##*:$2:*}\"" -o -z "\"\${$1%%*:$2}\"" -o -z "\"\${$1##$2:*}\"" -o -z "\"\${$1##$2}\"" ; then
		eval "$1=$2:\$$1"
	fi
}

if typeset -f dirappend >/dev/null ; then
	unset -f dirappend
fi

function dirappend
{
	if [ $# -le 1 -o -z "$1" ] ; then
		print "Usage: dirappend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'${varname} # XXX broken on KSH Version M-12/28/93f
	while [ $# -gt 0 ] ; do
		if [ -d "$1" -a $(expr ":$varvalue:" : ".*:$1:.*") -eq 0 ] ; then
			eval $varname='$'"$varname"'":$1"'
		fi
		shift
	done
	unset varname varvalue
}

if typeset -f dirprepend >/dev/null ; then
	unset -f dirprepend
fi

function dirprepend
{
	if [ $# -le 1 -o -z "$1" ] ; then
		print "Usage: dirprepend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'${varname} # XXX broken on KSH Version M-12/28/93f
	while [ $# -gt 0 ] ; do
		if [ -d "$1" -a $(expr ":$varvalue:" : ".*:$1:.*") -eq 0 ] ; then
			eval $varname='"$1:"$'"$varname"
		fi
		shift
	done
	unset varname varvalue
}

if typeset -f dirremove >/dev/null ; then
	unset -f dirremove
fi

function dirremove
{
	if [ $# -le 1 ] ; then
		echo "Usage: dirremove variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	while [ $# -gt 0 ] ; do
		if [ "$1" = ":" -o -z "$1" ] ; then
			eval $varname=$(eval echo '$'$varname | sed -e 's|::||g' -e 's|:$||')
		else
			eval $varname=$(eval echo '$'$varname | sed 's|\(:*\)'$1':*|\1|')
		fi
		shift
	done
	unset varname
}

if typeset -f lnotes >/dev/null ; then
	unset -f lnotes
fi

function lnotes
{
	if [ -d $HOME/notes ] ; then
	(
		# in a subshell
		cd $HOME/notes
		if [ $(ls|wc -w) != 0 ] ; then
			if test -t 1; then
				print '\nYou have notes on:'
				ls -dC *[!~]
			else
				ls -d *[!~]
			fi
		fi
	)
	fi
}

if typeset -f do_first_time >/dev/null ; then
	unset -f do_first_time
fi

# for window systems which don't emulate login sessions in each window
#
function do_first_time
{
	if [ -x /usr/games/fortune ] ; then
		/usr/games/fortune
	elif [ -x "$FORTUNE" ] ; then
		$FORTUNE
	fi
	if [ -r calendar -o -r .month ] ; then
		print "\nToday's Events:"
		if [ -r .month ] ; then
			month -B
		fi
		if [ -r calendar ] ; then
			calendar
		fi
	fi
	lnotes
	if [ -r $HOME/.trninit$TERM ] ; then
		TRNINIT="$HOME/.trninit$TERM" ; export TRNINIT
	fi
}

if type ismpx 2>&1 >/dev/null ; then
	: might just be running layers
else
	# otherwise it's just not possible....
	function ismpx
	{
		false
	}
fi

if [ "$PPID" -eq 1 -o "$PPID" -eq "$LAYERSPID" ] ; then
	typeset -i LEV=0
else
	if [ $LEV ] ; then
		let LEV=$LEV+1
	else
		let LEV=0
	fi
fi
export LEV

if [ -z "$TTY" ] ; then
	export TTY=$(tty)
fi
export TTYN=$(tty|sed 's|/dev/||')

# UGLY, but it works
#
# NOTE: there's a trick in here -- if there's no group-ID for your GID
# then the last expression won't do anything.  However since we've
# already pre-trimmed the extra "groups=..." stuff off the end the
# only thing that'll be left is the original "gid=20" string, and
# that'll have the same effect we want anyway.
#
# That's why we call the variable which holds the primary group name
# "gid" and not gname or group or whatever...
#
eval "$(id | sed -e 's/ groups=.*$//' \
		 -e 's/uid=\([0-9]*\)(\(..*\)) /id=\1 uid=\2 /' \
		 -e 's/gid=[0-9]*(\([^) ]*\)).*$/gid=\1/')"

function krcmd
{
	# ps -ax -o uid,pid,ppid,ucomm

	kill -9 $(ps -axlc | awk '$1 == '${id}' && $3 == 1 && $13 == "rcmd" {print $2}')
}

if [ "$id" -eq 0 ] ; then
	#
	# we always want persistent (and shared) history for 'su'
	#
	# XXX this should probably look for ~root/.sh_history first...
	#
	if [ -n "$HISTFILE" ] ; then
		HISTFILE="/$(basename $HISTFILE)"
	else
		HISTFILE="/.sh_history"
	fi
	# got to get rid of lone ":" or any "." in PATH
	PATH=$(echo $PATH | sed -e 's/::/:/g'	\
				-e 's/^://'	\
				-e 's/:$//'	\
				-e 's/^\.://'	\
				-e 's/:\.://'	\
				-e 's/:\.$//')
	# also get rid of the login user's ~/bin because it's usually first
	LOGNAMEPATH=$(eval echo ~$LOGNAME/bin)
	PATH=$(echo $PATH | sed -e "s|$LOGNAMEPATH:||")
	# must have X11BIN before openwin if newer X on system....
	dirappend PATH /usr/lbin /usr/ucb $X11BIN


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
	if [ ! -d /usr/sbin ] ; then
		dirprepend PATH /usr/etc	# only old BSDs
	fi
	if [ ! -d /sbin -a ! -d /usr/etc ] ; then
		dirprepend PATH /etc		# only really old systems...
	fi
	dirprepend PATH /sbin /usr/sbin
	dirappend PATH /usr/libexec/uucp /usr/lib/uucp /usr/lib

	# this next section duplicates ~/.profile since these
	# environment variables are not, and should not, be passed
	# through su (or sudo)

	if [ -z "$LOCAL" ] ; then
		if [ -d /local -a ! -L /local ] ; then
			LOCAL="/local"
		elif [ -d /usr/local -a ! -L /usr/local ] ; then
			LOCAL="/usr/local"
		else
			LOCAL="/NO-local-FOUND"
		fi
	fi
	export LOCAL

	if [ -z "$CONTRIB" ] ; then
		if [ -d /contrib -a ! -L /contrib ] ; then
			CONTRIB="/contrib"
		elif [ -d /usr/contrib -a ! -L /usr/contrib ] ; then
			CONTRIB="/usr/contrib"
		else
			CONTRIB="/NO-contrib-FOUND"
		fi
	fi
	export CONTRIB

	if [ -z "$PKG" ] ; then
		if [ -d /pkg -a ! -L /pkg ] ; then
			PKG="/pkg"
		elif [ -d /usr/pkg -a ! -L /usr/pkg ] ; then
			PKG="/usr/pkg"
		else
			PKG="/NO-pkg-FOUND"
		fi
	fi
	export PKG

	if [ -z "$SLASHOPT" ] ; then
		if [ -d /opt -a ! -L /opt ] ; then
			SLASHOPT="/opt"
		elif [ -d /usr/opt -a ! -L /usr/opt ] ; then
			SLASHOPT="/usr/opt"
		else
			SLASHOPT="/NO-opt-FOUND"
		fi
	fi
	export SLASHOPT

	if [ -z "$FINK" ] ; then
		if [ -d /sw -a ! -L /sw ] ; then
			FINK="/sw"
		else
			FINK="/NO-fink-FOUND"
		fi
	fi
	if [ "$FINK" != '/NO-fink-FOUND' ] ; then
		# for some bizzare reason AT&T Ksh Version M 1993-12-28 s+
		# doesn't seem to find the dir* functions in the namespace of
		# these functions, even though type and typeset show them.
		#
		finkfirst ()
		{
			dirremove PATH "$FINK/bin" "$FINK/sbin"
			dirprepend PATH "$FINK/bin"
		}
		finklast ()
		{
			dirremove PATH "$FINK/bin" "$FINK/sbin"
			dirappend PATH "$FINK/bin"
		}
	fi
	export FINK

	# we need to re-order the next paths so first we bulk remove them
	#
	PATH=$(echo $PATH | sed -e "s|${CONTRIB:-/NONE}/bin:||"	\
				-e "s|${CONTRIB:-/NONE}/sbin:||"\
				-e "s|${PKG:-/NONE}/bin:||"	\
				-e "s|${PKG:-/NONE}/sbin:||"	\
				-e "s|${SLASHOPT:-/NONE}/bin:||"	\
				-e "s|${SLASHOPT:-/NONE}/sbin:||"	\
				-e "s|${LOCAL:-/NONE}/bin:||"	\
				-e "s|${LOCAL:-/NONE}/sbin:||"	\
				-e "s|${FINK:-/NONE}/bin:||"	\
				-e "s|${FINK:-/NONE}/sbin:||"	\
				-e "s|${GNU:-/NONE}/bin:||"	\
				-e "s|${GNU:-/NONE}/sbin:||" )
	if [ -d "$CONTRIB" ] ; then
		dirappend PATH $CONTRIB/sbin $CONTRIB/bin
		if [ ! -d $CONTRIB/sbin ] ; then
			dirappend PATH $CONTRIB/etc
		fi
	fi
	if [ -d "$PKG" ] ; then
		dirappend PATH $PKG/sbin $PKG/bin
	fi
	if [ -d "$SLASHOPT" ] ; then
		dirappend PATH $SLASHOPT/sbin $SLASHOPT/bin
	fi
	if [ -d "$LOCAL" ] ; then
		dirappend PATH $LOCAL/sbin $LOCAL/bin
		if [ ! -d $LOCAL/sbin ] ; then
			dirappend PATH $LOCAL/etc
		fi
	fi
	if [ -d "$GNU" ] ; then
		dirappend PATH $GNU/sbin $GNU/bin
		if [ ! -d $GNU/sbin ] ; then
			dirappend PATH $GNU/etc
		fi
	fi
	if [ -d "$FINK" ] ; then
		dirprepend PATH $FINK/sbin $FINK/bin
		if [ -r $FINK/bin/init.sh ] ; then
			# this sets up other handy things for Fink
			. $FINK/bin/init.sh
		fi
	fi
	dirappend PATH $HOME/bin
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		dirappend PATH $DMD/bin $DMDSGS/bin/3b5 $DMD/local/bin
	fi
	case "$TERM" in
	xterm* | dmd-myx)
		PS1='[!] # '
		;;
	*)
		PS1='$TTYN:$LOGNAME@$UUNAME[$LEV.!] ${BANNER_PWD#$HOME} # '
		;;
	esac
	MAILPATH=${MAILDIR}/${LOGNAME}:${MAILDOR}/root:${MAILDIR}/uucp:${MAILDIR}/usenet
	if [ "$VISUAL" = "emacsclient" ] ; then
		export VISUAL="emacs -nw"
	fi
	if [ "$EDITOR" = "emacsclient" ] ; then
		export EDITOR="emacs -nw"
	fi
	# make damn sure PAGER is set...
	if expr "$(type less)" : '.* is .*/less$' >/dev/null 2>&1 ; then
		PAGER=$(type less)
		LESS="-M" ; export LESS
	elif [ -x /usr/xpg4/bin/more ] ; then
		# SunOS-5's, at least, has the 'G' command!
		PAGER="/usr/xpg4/bin/more"
		# use '-s' as it can't be turned on later during runtime
		MORE="-s" ; export MORE
	elif expr "$(type more)" : '.* is .*/more$' >/dev/null 2>&1 ; then
		PAGER=$(type more)
		# use '-s' as it can't be turned on later during runtime
		MORE="-sw" ; export MORE
	else
		# meow
		PAGER=$(type cat)
	fi
	PAGER=$(expr "$PAGER" : '.*/\([^/]*\)$'); export PAGER
	if [ "$PAGER" = "less" ]; then
		MANPAGER="$PAGER -si"; export MANPAGER
	fi

	if [ -n "$DISPLAY" ]; then
		#
		# XXX if root is using this .kshrc then perhaps we
		# should try copying the "xauth" information for the
		# current display to $HOME/.Xauthority instead of just
		# pointing at it...
		#
		XAUTHORITY=$(eval echo ~${SU_FROM}/.Xauthority); export XAUTHORITY
	fi

	function krcmd
	{
		# WARNING: this version kills everyone's rcmd procs!
		kill -9 $(ps -axlc | awk '$3 == 1 && $13 == "rcmd" {print $2}')
	}
	# I don't know if this is right, or not, but let's try for now...
	cd
elif [ "$uid" != "$LOGNAME" ] ; then
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	fi
	case "$TERM" in
	xterm* | dmd-myx)
		PS1='[!] $ '
		;;
	*)
		PS1='$TTYN:$uid($LOGNAME)@$UUNAME)[$LEV.!] ${BANNER_PWD#$HOME} $ '
		;;
	esac
else
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$LOGNAME{$gid}@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	fi
	case "$TERM" in
	xterm* | dmd-myx)
		PS1='[!] $ '
		;;
	*)
		PS1='$TTYN:$LOGNAME@$UUNAME[$LEV.!] ${BANNER_PWD#$HOME} $ '
		;;
	esac
fi

if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then

	if [ "$LEV" -eq 0 ] ; then
		do_first_time	# in xterms, we are a login shell, but not in layers
	fi
	MYXCLR_L="$(myxban -l)"
	MYXCLR_C="$(myxban -c)"
	MYXCLR_R="$(myxban -r)"
	MYXCLR="${MYXCLR_L}${MYXCLR_C}${MYXCLR_R}"
	MYXBAN_L='$BANNER_PWD'

	alias clearban='print "${MYXCLR}\c"'

	function setban
	{
		clearban
		if [ -z "$BANNER_PWD" ]; then
			BANNER_PWD=$(pwd)
		fi
		eval myxban -l "\"$MYXBAN_L\""
		myxban -c "${WBANNER:-$@}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	# NOTE:  may be re-defined in ~/.kshpwd
	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd "$@"
		BANNER_PWD=$(pwd)
		eval myxban -l "\"$MYXBAN_L\""
	}

	function psm
	{
		ps -ft $(tty | sed 's|/dev/xt|xt/|')
	}

fi

case "$TERM" in
xterm*)
	alias clearban='WBANNER=""; setban'

	function setban
	{
		if [ -z "$WBANNER" ]; then
			WBANNER=$@
		fi
		if [ -z "$BANNER_PWD" ]; then
			BANNER_PWD=$(pwd)
		fi
		if [ "$uid" = "$LOGNAME" ]; then
			eval TBANNER='"${WBANNER:-sh}://$UUNAME/$BANNER_PWD | $uid[$LEV]:$TTYN"'
		else
			eval TBANNER='"${WBANNER:-sh}://$UUNAME/$BANNER_PWD | $uid($LOGNAME)[$LEV]:$TTYN"'
		fi
		print "\033]0;$TBANNER\007\c"
		WBANNER=""
	}

	# NOTE:  may be re-defined in ~/.kshpwd
	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd "$@"
		BANNER_PWD=${PWD}
		setban
	}
	;;
esac

if type setban > /dev/null ; then
	#
	# XXX re-factor into a function which defines functions....
	#
	if [ "$VISUAL" = "emacsclient" -a -z "$DISPLAY" ] ; then
		unalias emacs
		alias emacs=_emacs
		function _emacs
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="GNU Emacs "
			setban
			mesg n
			\emacs "$@"
			setban
		}
	fi

	unalias cu
	alias cu=_cu
	function _cu
	{
		if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
			trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
			myxsize -s
		else
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
		fi
		WBANNER="CU $* "
		setban
		mesg n
		\cu "$@"
		setban
	}

	unalias ckermit
	alias ckermit=_ckermit
	function _ckermit
	{
		if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
			trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
			myxsize -s
		else
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
		fi
		WBANNER="C-Kermit $* "
		setban
		mesg n
		\ckermit "$@"
		setban
	}

	if expr "$(type rlogin)" : '.* is .*/rlogin$' >/dev/null 2>&1 ; then
		unalias rlogin
		alias rlogin=_rlogin
		function _rlogin
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="RLOGIN $* "
			setban
			mesg n
			\rlogin "$@"
			setban
		}
	fi

	if expr "$(type slogin)" : '.* is .*/slogin$' >/dev/null 2>&1 ; then
		unalias slogin
		alias slogin=_slogin
		function _slogin
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="SLOGIN $* "
			setban
			mesg n
			\slogin "$@"
			setban
		}
	fi

	if expr "$(type console)" : '.* is .*/console$' >/dev/null 2>&1 ; then
		unalias console
		alias console=_console
		function _console
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="CONSOLE $* "
			setban
			mesg n
			\console "$@"
			setban
		}
	fi

	if expr "$(type telnet)" : '.* is .*/telnet$' >/dev/null 2>&1 ; then
		unalias telnet
		alias telnet=_telnet
		function _telnet
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
				myxsize -s
			else
				trap "trap 1; setban; kill -1 $$" 1
				trap "trap 2; setban; kill -2 $$" 2
				trap "trap 3; setban; kill -3 $$" 3
				trap "trap 15; setban; kill -15 $$" 15
			fi
			WBANNER="TELNET $* "
			setban
			mesg n
			\telnet "$@"
			setban
		}
	fi

	if $HAVEMUSH ; then
		unalias mushC
		alias mushC=_mushC
		function _mushC
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="MUSH $* "
			setban
			mesg n
			mush -C "$@"
			setban
		}
	fi

	if expr "$(type irc)" : '.* is .*/irc$' >/dev/null 2>&1 ; then
		unalias irc
		alias irc=_irc
		function _irc
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="IRC $* "
			setban
			mesg n
			\irc "$@"
			setban
		}
	fi

	if expr "$(type trn)" : '.* is .*/trn$' >/dev/null 2>&1 ; then
		unalias trn
		alias trn=_trn
		function _trn
		{
			trap "trap 1; setban; kill -1 $$" 1
			trap "trap 2; setban; kill -2 $$" 2
			trap "trap 3; setban; kill -3 $$" 3
			trap "trap 15; setban; kill -15 $$" 15
			WBANNER="TRN $* "
			setban
			mesg n
			\trn "$@"
			setban
		}
	fi

	unalias su
	alias su=_su
	function _su
	{
		trap "trap 1; setban; kill -1 $$" 1
		trap "trap 2; setban; kill -2 $$" 2
		trap "trap 3; setban; kill -3 $$" 3
		trap "trap 15; setban; kill -15 $$" 15
		showargs="${*-root}"
		if [ "$showargs" = "root" ]; then
			\cd /
			PWD=/
			showargs=""
		fi
		WBANNER="SU $showargs"
		setban
		mesg n
		if [ -x /usr/5bin/su ] ; then
			/usr/5bin/su "$@"
		else
			\su "$@"
		fi
		if [ "${*-root}" = "root" ]; then
			\cd -
		fi
		PWD=$(pwd)
		setban
	}

	if expr "$(type nethack)" : '.* is .*/nethack$' >/dev/null 2>&1 ; then
		function nethack
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; loadfont thin.9x14; setban" 1 2 3 15
				loadfont rogue.9x18
			else
				trap "trap 1; setban; kill -1 $$" 1
				trap "trap 2; setban; kill -2 $$" 2
				trap "trap 3; setban; kill -3 $$" 3
				trap "trap 15; setban; kill -15 $$" 15
			fi
			WBANNER="NetHack"
			setban
			mesg n
			\nethack
			setban
		}
	fi

	setban
fi

# just X11 stuff here....
#
case "$TERM" in
xterm*)
	function onx11server
	{
		_RDISP=""
		_USAGE="$argv0: $0(): Usage: onx11server [-nS] [-D REMOTE_DISPLAY] SERVERNAME 'command string'"
		_nullopt=""
		while getopts nD: OPTCH
		do
			case ${OPTCH} in
			n)
				_nullopt="-n"
				;;
			D)
				_RDISP="${OPTARG}"
				;;
			*)
				echo "$_USAGE" >&2
				return 2
				;;
			esac
		done
		shift $(( $OPTIND - 1 ))

		if [ $# -ne 2 ]; then
			echo "$_USAGE" >&2
			return 2
		fi

		_X11server=$1
		echo "$argv0: starting $RSH $_nullopt $_X11server '. ./.profile; export DISPLAY=${_RDISP:-${DISPLAY}}; exec $2'"
		# note: don't run this in the background -- let the caller do that
		$RSH $_nullopt $_X11server ". ./.profile; export DISPLAY=${_RDISP:-${DISPLAY}}; exec $2"
	}
	function rxterm
	{
		rhost=$1
		shift
		onx11server -n "$rhost" "xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -display $DISPLAY -T rsh:$rhost $*" &
	}
	function ruxterm
	{
		rhost=$1
		shift
		onx11server -n "$rhost" "uxterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -display $DISPLAY -T rsh:$rhost $*" &
	}
	function lxterm
	{
		LANG=C LC_ALL=C xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -T $HOSTNAME $* &
	}
	function luxterm
	{
		uxterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -T $HOSTNAME $* &
	}
	function rxauth
	{
		xauth nextract - $DISPLAY | onx11server $* 'xauth nmerge -'
	}

	setban
	;;
esac

# initialize SECONDS to the number of seconds since the last
# local wall-clock midnight hour
#
# For our purposes this is more useful than the shell's total
# wall-clock run time.
#
alias set_secs_to_midnight='SECONDS=$(date "+3600*%H+60*%M+%S")'
#
#set_secs_to_midnight
#
# XXX we can't actually use the alias here because in real Ksh the
# alias must be defined before any command using it is read, and when
# sourcing this file from a script it seems somehow possible that the
# whole block of text containing both the definition and use might be
# read at the same time, eg. on Mac OS X's "Version M 1993-12-28 s+".
#
SECONDS=$(date '+3600*%H+60*%M+%S')

# We wouldn't normally have to reset $SECONDS since our calculations
# for hours and minutes only need to find the remainder values for the
# seconds/unit since any epoch.  However wall-clock time will shift
# when daylight savings time transitions happen.  Resetting $SECONDS
# once per hour is probably overkill but will do the job -- it only
# really needs to be done once a day at most in order to catch any
# number of daylight savings time transitions.
#
# The first one in the loop should only sleep to the top of the hour,
# but calculating that would really be overkill!
#
# The extra sub-shell is to prevent the main shell from tracking the
# kill job...
#
#trap 'SECONDS="$(date '+3600*%H+60*%M+%S')"; ( { sleep 3600; kill -14 $$; } & )' 14

# start the cycle
#
#kill -14 $$

# expressions to calculate hours, minutes, and seconds since midnight
# given the number of $SECONDS (we ignore the number of days that may
# have passed since that midnight hour)
#
_hh="(SECONDS/3600)%24"
_mm="(SECONDS/60)%60"
_ss="(SECONDS)%60"

# We'll be wanting zero-filled 2-digit expansion for our hours and
# minutes variables
#
typeset -Z2 _h _m

# a magic expression that evaluates the above expressions to set the two
# variables we've configured specially above, and then expands those two
# variables in a standard "HH:MM" 24-hr format to show the current time.
#
_time='${_x[(_m=_mm)==(_h=_hh)]}$_h:$_m'

PS1="$_time $PS1"
PS2=">>> "
PS3="??? "

if [ "$uid" = usenet -o "$uid" = news ] ; then
	dirprepend PATH $LOCAL/lib/newsbin $LOCAL/lib/newsbin/maint $LOCAL/lib/newsbin/input
fi

if $ISSUN; then
	alias df="/usr/bin/df"
fi

if [ "$(whence man)" = "/usr/bin/man" -a -x $LOCAL/bin/man ] ; then
	alias man=$LOCAL/bin/man
	alias osman='MANPATH=$OMANPATH /usr/bin/man'
else
	alias osman='MANPATH=$OMANPATH man'
fi
alias gman='MANPATH=$GNU/man man'
alias lman='MANPATH=$LOCAL/share/man man'
alias pkgman='MANPATH=$PKG/share/man man'
alias optman='MANPATH=$SLASHOPT/share/man man'
alias tkman='MANPATH=$LOCAL/share/man.tcltk man'
alias x11man='MANPATH=$X11PATH/man man'

if $HAVETPUT; then
	alias c='tput clear'
	alias clear='tput clear'
else
	alias c='clear'
fi

if $HAVEMUSH; then
	alias mfrom='mush -H:n'
	alias mhdrs='mush -H -f'
fi

# (note: sh "read" command also reads backslash continued lines)
# XXX need an option to strip leading whitespace on continued lines
alias backslashjoin='sed -e :a -e "/\\\\$/N; s/\\\\\\n//; ta"'
# NOTE: never forget this -- it's the most incredible sed script!!!!
alias blsqueeze='sed "/./,/^$/!d"'
alias blstrip='sed "/./!d"'
alias cdpkgwrksrc='cd $(make show-var VARNAME=WRKSRC)'
# NOTE: replacing the last '-print' with '-exec CMD {} +' lets one work on the files
alias pkgfind="find . -type d -name CVS -prune -o -type f \( -name 'Make*' -o -name '*.mk' \) ! -name '.#*' ! -name '*~' ! -name .cvsignore ! -print"
alias cvsfind="find . -type d -name CVS -prune -o -type f ! -name '.#*' ! -name '*~' ! -name .cvsignore ! -print"
alias cvsfind0="find . -type d -name CVS -prune -o -type f ! -name '.#*' ! -name '*~' ! -name .cvsignore ! -print0"
alias deadlinks='find . -type l -a ! \( -follow -type f \) -print'
alias dlog='$PAGER -en +G /var/log/debug'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias elc='emacs -batch -q -no-site-file -f batch-byte-compile'
alias f='finger'
# Note: use "findls" as the prefix where globbing will exceed ARG_MAX
# "find" never lists the '..' entry, so if you also exclude the '.'
# entry and then apply "-prune" to all the remaining entries, find
# certainly won't descend into any sub-directory.  This is an
# alternative to using "-maxdepth 1" (which is not in POSIX.2)
# Don't list directories and links by appending "! -type d ! -type l"
alias findls='find . ! -name . -prune'
alias funclist='typeset +f'
alias gitfind='git ls-tree -r --name-only HEAD'
alias h='fc -l'
alias history='fc -l 1'
alias hmeme='fc -l 1 | awk "\$1 > 0 {print \$2}" | sort  | uniq -c | sort -rn | sed 20q'
alias ilog='$PAGER -en +G /var/log/important'
alias j='jobs -l'
alias l='/bin/ls -CF'
alias la='/bin/ls -CFa'
alias lD='/bin/ls -CFd'
alias lL='/bin/ls -CFL'
alias ll='/bin/ls -lis'
alias llL='/bin/ls -lisL'
alias lla='/bin/ls -lisa'
alias lld='/bin/ls -lisd'
alias llr='/bin/ls -lisR'
alias llra='/bin/ls -lisaR'
alias lr='/bin/ls -CFR'
alias lra='/bin/ls -CFRa'
alias lsa='/bin/ls -a'
alias logout='exit 0'
alias maildate='LANG=c date "+%a, %d %b %Y %T %z"'
alias mynotes='( \cd ~/notes && /bin/ls -CF *[!~] )'
alias nosgr='echo '
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias pkg_sizes="/usr/sbin/pkg_info -s \* | sed -e '/^$/d' -e 's/Information for //' -e 's/:$/:\\\\/' | sed -e :a -e '$!N;s/Size of this package in bytes://;ta' -e 'P;D' | backslashjoin"
alias realias='let LEV=$LEV-1;exec ksh'		# useless?
alias rehash='_SV_PATH=$PATH; PATH=$_SV_PATH; unset _SV_PATH'
alias rinfo='rlog -L -h -l $(find RCS -type f -print)'
alias rstty='stty $SANE'
alias scvs='export CVSROOT="$(< CVS/Root)"; print "CVSROOT=$CVSROOT"'
alias snmpoidinfo='snmptranslate -T d -O f'
alias wcvs='print $CVSROOT'
alias zds='zmore'

alias xload-1="xload -geometry 120x40-200+48 -hl red &"
alias xload-2="xload -geometry 120x40-200+96 -hl red &"
alias xload-3="xload -geometry 120x40-200+144 -hl red &"
alias xload-4="xload -geometry 120x40-200+192 -hl red &"
alias xload-5="xload -geometry 120x40-200+240 -hl red &"

# Smail related tools...
#
alias badsenders='fgrep RHSBL: $MAILLOG | sed "s/[<>]/ /g" | awk "{print \$8}" | sort -u'
# Warning: less (at least as of v. 374) has a limitation of about 95
# chars to the length of the '-p' parameter.
alias maillog='$PAGER -enM -p ": \[[0-9]+\] (\[.+\] )?((remote[A-Z ]*:)|remote ..LO: (rejected: inv[^:]*:|refusing) )|^.*kill.*" +G $MAILLOG'
alias mlog='$PAGER -en +G /var/log/messages'
alias rblcount='fgrep " matched " $MAILLOG | cut -d " " -f 13 | cut -d . -f 5- | sort | uniq -c'
alias rblstats='fgrep " matched " $MAILLOG | cut -d " " -f 10- | sort | uniq -c | sort -n | ds'

# This is only useful on SysV and NetBSD-5 and newer
alias fw='who -HurTbA'

# these are only useful on SysV
#alias lpq='lpstat -o'

# TODO: find a test so these are usable.
#alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok rows $LINES cols $COLUMNS'
#alias rstty='stty $SANE; stty rows ${LINES:-$(tput lines)} cols ${COLUMNS:-$(tput cols)}'

# TODO: find a way to test if HoneyDanBer UUCP or not....
# (other than [ -d /etc/uucp ])
alias uuq='uustat -a'

# TODO: should only set this if "xtail" and UUCP are both available?
if [ -d /var/spool/uucp ] ; then
	alias uufollow='xtail /var/spool/uucp/.[AL]*/*'
else
	alias uufollow='xtail /usr/spool/uucp/.[AL]*/*'
fi

# This is silly, but it usually works....
if [ -x /usr/ucb/rsh -a -x /bin/rsh ] ; then
	alias rsh=/usr/ucb/rsh
fi

if [ -r $HOME/.kshsccs ] ; then
	. $HOME/.kshsccs
fi
if [ -r $HOME/.kshpwd ] ; then
	. $HOME/.kshpwd
fi
if [ -r $HOME/.kshlocal ] ; then
	. $HOME/.kshlocal
fi
if [ -r $HOME/.kshedit ] ; then
	. $HOME/.kshedit
else
	set -o gmacs
	alias __A=''		# up arrow
	alias __B=''		# down arrow
	alias __C=''		# right arrow
	alias __D=''		# left arrow
	alias __H=''		# beginning of line, HOME key
fi

if [ -n "$KSH_VERSION" ] ; then
	#
	# this is probably pdksh
	#
	set -o braceexpand
fi

#
#	more functions
#

if type killall 2>&1 >/dev/null ; then
	: already have a good killall?
else
	function killall
	{
		if [ $# -eq 1 ]; then
			signal="-TERM"
			cmd=$1
		elif [ $# -eq 2 ]; then
			signal=$1
			cmd=$2
		fi
		kill $signal $(ps -axuc | awk '$11 == "'$cmd'" { print $2}')
	}
fi

if [ -f /usr/share/misc/na.phone ]; then
	function areacode
	{
		fgrep "$@" /usr/share/misc/*.phone
	}
fi

function cvsupdateroot
{
	newroot=$1

	find . -name Root -print | fgrep CVS/Root | while read rootfile ; do
		echo $newroot > $rootfile
	done
}

function errno
{
	grep "^#define[ 	]*[A-Z][A-Z]*[ 	]*$1[ 	]" /usr/include/sys/errno.h
}

function ipaddrsort
{
	sed 's/\./ /g' ${1} |
	  sort -b -n -k 1 \
		  -n -k 2 \
		  -n -k 3 \
		  -n -k 4 |
	  sed -E -e 's/([0-9]) /\1./' \
		 -e 's/([0-9]) /\1./' \
		 -e 's/([0-9]) /\1./'
}

function kall
{
	SIGOPT=""
	case "$1" in
	-*)
		SIGOPT=$1
		shift;
		;;
	esac
	kill $SIGOPT $(ps -x | awk '$5 == "'$1'" {print $1}')

	unset SIGOPT
}

function lastcmd
{
	tr '[\001-\007]' '[\012*]' < .sh_history | tr '[\176-\377]' '[ *]' | egrep -v '[	 }#]|^$' | tail "$@"
}

function mailclients
{
	awk '$4 == "remote" && $5 == "connection" && $6 == "from" {
		sub(/\[.*$/, "", $7);
		print $7;
	}' $*
}

function mailsizes
{
	awk '$5 == "Received" {
		for (fn = 6; fn <= NF; fn++) {
			if (substr($fn, 1, 5) == "SIZE:") {
				size = substr($fn, 6) + 0;
				break;
			}
		}
		print size;
	}' $*
}

function sedjoinnext
{
	# pattern that starts the lines to be appended to their previous lines
	# (note a blank is inserted in place of the newline)
	#
	start=$1

	sed -e ':a' -e '$!N;s/\n'"$start"'/ \1/;ta' -e 'P;D'
}

#function sedjoinnext
#{
#	# pattern that ends the lines to be joined with to their previous lines
#	#
#	end=$1
#
#	sed -e :a -e '/\\$/N; s/\\\n//; ta'
#}

function signm
{
	grep "^#define[ 	]*SIG.*[ 	]*${1}[ 	]" /usr/include/sys/signal.h
}

function signo
{
	grep -i "^#define[ 	]*.*${1}[ 	]*[0-9]" /usr/include/sys/signal.h
}

function snmpmiblist
{
	cd $1; echo $(awk '{print $1}' .index) | sed 's/ /:/g'
}

# trivial hack to quickly search a source tree where traditional
# globbing would exceed ARG_MAX
#
# Requires a find with the SysVR4 "-exec ... {} ... +" feature.  (now
# also mandated by SUSv3, aka IEEE 1003.1-2001/2004)
#
# is a faster alternative to "find ... -print0 | xargs -0 ..."
#
function srcfind
{
	find . -type f -name '*.[ch]' -exec fgrep $* {} +
}

function typeof
{
	if [ -z "$LLIBDIR" ]; then
		if $ISSUN; then
			LLIBDIR=/usr/lib/lint
		elif [ -d /usr/libdata/lint ]; then
			# XXX not quite so useful when these are for xlint...
			LLIBDIR=/usr/libdata/lint
		else
			LLIBDIR=/usr/lib
		fi
	fi
	# should expand to allow '-l{lib}'
	egrep -i "$1" $LLIBDIR/llib-l*
	unset LLIBDIR
}

function zhead
{
	zcat $* | head
}

if [ -f /usr/adm/lastlog.ut -a -x /usr/lib/acct/fwtmp ] ; then
	function lastlog
	{
		YearGrep="grep $(date +%Y)"
		if [ $# -eq 1 -a "$1" = "-a" ] ; then
			YearGrep="grep -v 1969"
		elif [ $# -ne 0 ] ; then
			print 'Usage: lastlog [-a]' >&2
		fi
		/usr/lib/acct/fwtmp < /usr/adm/lastlog.ut | $YearGrep |
			awk '{printf("%-8s %-12s %s\n", $1, $3, substr($0, 56))}'
	}
fi

if [ -r $HOME/.kshdir ] ; then
	alias pushd='unalias pushd popd showd sd;. $HOME/.kshdir; pushd'
	alias popd='unalias pushd popd showd sd;. $HOME/.kshdir; popd'
	alias showd='unalias pushd popd showd sd;. $HOME/.kshdir; showd'
fi

unset -f do_first_time

trap '
	rc=$?;
	if ((ERRNO > 0)); then
		EMSG="; errno: $ERRNO"
	else
		EMSG=""
	fi;
	print "${0#-}: exit code: $rc$EMSG"
' ERR

# NOTE: some versions of some shells complain here if not connected to a tty
#
case "$0" in
-*)
	set -o monitor
	;;
esac
set -o trackall
