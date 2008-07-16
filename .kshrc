#
#	.kshrc - per-interactive-shell startup stuff
#
#ident	"@(#)HOME:.kshrc	28.4	08/07/15 18:57:06 (woods)"

# WARNING:
# don't put comments at the bottom or you'll bugger up ksh-11/16/88e's history

# Assumptions:
#

# Files referenced:
#
#	$HOME/.kshsccs		- sourced, if it is readable
#	$HOME/.kshpwd		- sourced, if it is readable
#	$HOME/.kshlocal		- sourced, if it is readable
#	$HOME/.kshedit		- sourced, if it is readable else gmacs editing set
#	$HOME/.kshdir		- dir autoload aliases set, if it is readable

#set -o nolog		# no functions in $HISTFILE

export PATH="$PATH"

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
			print '\nYou have notes on:' 
			ls -C *[!~]
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
	if $ISSUN; then
		PATH=$(echo $PATH | sed -e 's|^/bin:||' -e 's|:/etc:|:|')
		dirprepend PATH /usr/5bin
		dirappend PATH /usr/openwin/bin
	fi
	if [ ! -d /usr/sbin ] ; then
		dirprepend PATH /usr/etc	# only old BSDs
	fi
	if [ ! -d /sbin -a ! -d /usr/etc ] ; then
		dirprepend PATH /etc		# only really old systems...
	fi
	dirprepend PATH /sbin /usr/sbin
	dirappend PATH /usr/libexec/uucp /usr/lib/uucp /usr/lib
	# we need to re-order the next paths so first we remove them
	PATH=$(echo $PATH | sed -e "s|${CONTRIB:-/NONE}/bin:||"	\
				-e "s|${CONTRIB:-/NONE}/sbin:||"\
				-e "s|${PKG:-/NONE}/bin:||"	\
				-e "s|${PKG:-/NONE}/sbin:||"	\
				-e "s|${LOCAL:-/NONE}/bin:||"	\
				-e "s|${LOCAL:-/NONE}/sbin:||"	\
				-e "s|${GNU:-/NONE}/bin:||"	\
				-e "s|${GNU:-/NONE}/sbin:||" )
	if [ -n "$CONTRIB" ] ; then
		dirappend PATH $CONTRIB/sbin $CONTRIB/bin
		if [ ! -d $CONTRIB/sbin ] ; then
			dirappend PATH $CONTRIB/etc
		fi
	fi
	if [ -n "$PKG" ] ; then
		dirappend PATH $PKG/sbin $PKG/bin
		if [ ! -d $PKG/sbin ] ; then
			dirappend PATH $PKG/etc
		fi
	fi
	if [ -n "$LOCAL" ] ; then
		dirappend PATH $LOCAL/sbin $LOCAL/bin
		if [ ! -d $LOCAL/sbin ] ; then
			dirappend PATH $LOCAL/etc
		fi
	fi
	if [ -n "$GNU" ] ; then
		dirappend PATH $GNU/sbin $GNU/bin
		if [ ! -d $GNU/sbin ] ; then
			dirappend PATH $GNU/etc
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

	if [ -n "$DISPLAY" ]; then
		#
		# XXX if root is using this .kshrc then perhaps we
		# should try copying the "xauth" information for the
		# current display to $HOME/.Xauthority
		#
		echo "The X11 display '$DISPLAY' may be unusable without authorization."
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
		myxban -c "${WBANNER}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	# NOTE:  may be re-defined in ~/.kshpwd
	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd "$@"
		if [ -z "$BANNER_PWD" ]; then
			BANNER_PWD=$(pwd)
		fi
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
		setban 
	}
	;;
esac

if type setban > /dev/null ; then
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
			emacs "$@"
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
		/usr/bin/cu "$@"
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
		$LOCAL/bin/ckermit "$@"
		setban
	}

	if expr "$(type rlogin)" : '.* is .*/rlogin$' >/dev/null 2>&1 ; then
		RLOGIN="$(expr "$(type rlogin)" : '.*/\([^/]*\)$')"; export RLOGIN
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
			$RLOGIN "$@"
			setban
		}
	fi

	if expr "$(type slogin)" : '.* is .*/slogin$' >/dev/null 2>&1 ; then
		SLOGIN="$(expr "$(type slogin)" : '.*/\([^/]*\)$')"; export SLOGIN
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
			$SLOGIN "$@"
			setban
		}
	fi

	if expr "$(type console)" : '.* is .*/console$' >/dev/null 2>&1 ; then
		CONSOLE="$(expr "$(type console)" : '.*/\([^/]*\)$')"; export CONSOLE
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
			$CONSOLE "$@"
			setban
		}
	fi

	if expr "$(type telnet)" : '.* is .*/telnet$' >/dev/null 2>&1 ; then
		TELNET="$(expr "$(type telnet)" : '.*/\([^/]*\)$')" ; export TELNET
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
			$TELNET "$@"
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
			irc "$@"
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
			trn "$@"
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
		elif [ -x /usr/bin/su ] ; then
			/usr/bin/su "$@"
		else
			/bin/su "$@"
		fi
		if [ "${*-root}" = "root" ]; then
			\cd -
		fi
		PWD=$(pwd)
		setban
	}

	if [ -x $LOCAL/games/nethack ] ; then
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
			$LOCAL/games/nethack
			setban
		}
	fi

	setban
fi

# just X11 stuff here....
#
case "$TERM" in
xterm*)
	function roterm
	{
		rhost=$1
		shift
		rsh -n "$rhost" "OPENWINHOME=/usr/openwin XFILESEARCHPATH=/usr/openwin/lib/%T/%N%S /usr/openwin/demo/xterm -cn -rw -sb -si -sk -sl 1024 -ls -display $DISPLAY -T rsh:$rhost $*" &
	}
	function loterm
	{
		OPENWINHOME=/usr/openwin XFILESEARCHPATH=/usr/openwin/lib/%T/%N%S /usr/openwin/demo/xterm -cn -rw -sb -si -sk -sl 1024 -ls -T $HOSTNAME $* &
	}
	function rxterm
	{
		rhost=$1
		shift
		rsh -n "$rhost" "$X11BIN/xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -display $DISPLAY -T rsh:$rhost $*" &
	}
	function lxterm
	{
		$X11BIN/xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -T $HOSTNAME $* &
	}
	setban
	;;
esac

# initialize SECONDS to the number of seconds since the last
# local wall-clock midnight hour
#
alias set_secs_to_midnight='SECONDS="$(date '"'"'+3600*%H+60*%M+%S'"'"')"'
set_secs_to_midnight

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
alias optman='MANPATH=$OPT/share/man man'
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

alias backslashjoin='sed -e :a -e "/\\\\$/N; s/\\\\\\n//; ta"'
alias badsenders='fgrep RHSBL: $MAILLOG | sed "s/[<>]/ /g" | awk "{print \$8}" | sort -u'
# NOTE: never forget this -- it's the most incredible sed script!!!!
alias blsqueeze='sed "/./,/^$/!d"'
alias blstrip='sed "/./!d"'
alias deadlinks='find . -type l -a ! \( -follow -type f \) -print'
# XXX write one to collapse back-slash continued lines too!
alias dlog='$PAGER -en +G /var/log/debug'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias elc='emacs -batch -q -no-site-file -f batch-byte-compile'
alias f='finger'
alias funclist='typeset +f'
alias h='fc -l | tail'
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
# Warning: less (at least as of v. 374) has a limitation of about 95
# chars to the length of the '-p' parameter.
alias maillog='$PAGER -enM -p ": \[[0-9]+\] (\[.+\] )?((remote[A-Z ]*:)|remote ..LO: (rejected: inv[^:]*:|refusing) )|^.*kill.*" +G $MAILLOG'
alias mlog='$PAGER -en +G /var/log/messages'
alias mynotes='( \cd ~/notes && /bin/ls -CF *[!~] )'
alias nosgr='echo '
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias pkg_sizes="pkg_info -s \* | sed -e '/^$/d' -e 's/Information for //' | sed -e :a -e '$!N;s/\nSize of this package in bytes:/ /;ta' -e 'P;D'"
alias rblcount='fgrep " matched " $MAILLOG | cut -d " " -f 13 | cut -d . -f 5- | sort | uniq -c'
alias rblstats='fgrep " matched " $MAILLOG | cut -d " " -f 10- | sort | uniq -c | sort -n | ds'
alias realias='let LEV=$LEV-1;exec ksh'		# useless?
alias rehash='_SV_PATH=$PATH; PATH=$_SV_PATH; unset _SV_PATH'
alias rinfo='rlog -L -h -l $(find RCS -type f -print)'
alias rstty='stty $SANE'
alias scvs='export CVSROOT="$(< CVS/Root)"; print "CVSROOT=$CVSROOT"'
alias snmpoidinfo='snmptranslate -T d -O f'
alias syncdotfiles='rsync -v -lptHS --stats --files-from=once.weird.com:dotfiles.list once.weird.com:. $HOME'
alias wcvs='print $CVSROOT'
alias zds='zmore'

# these are only useful on SysV
#alias fw='who -HurTbA'
#alias lpq='lpstat -o'

# TODO: find a test so these are usable.
#alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok rows $LINES cols $COLUMNS'
#alias rstty='stty $SANE; stty rows ${LINES:-$(tput lines)} cols ${COLUMNS:-$(tput cols)}'

# TODO: find a way to test if HoneyDanBer UUCP or not....
# (other than [ -d /etc/uucp ])
alias uuq='uustat -a'

# TODO: should only set this if xtail is available?
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
