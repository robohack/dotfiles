#
#	.kshrc - per-shell startup stuff
#
#ident	"@(#)HOME:.kshrc	20.5	99/02/17 15:14:35 (woods)"

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
	varname="$1"
	shift
	eval varvalue='$'$varname
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
	eval varvalue='$'$varname
	while [ $# -gt 0 ] ; do
		if [ -d "$1" -a $(expr ":$varvalue:" : ".*:$1:.*") -eq 0 ] ; then
			eval $varname='"$1:"$'"$varname"
		fi
		shift
	done
	unset varname varvalue
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
	if [ -d $HOME/notes ] ; then
		(
			cd $HOME/notes
			if [ $(ls|wc -w) != 0 ] ; then
				print '\nNotes on: ' *
			fi
		)
	fi
	if [ -r $HOME/.trninit$TERM ] ; then
		TRNINIT="$HOME/.trninit$TERM" ; export TRNINIT
	fi
}

# we don't need this, I don't think, since it is in .profile....
#
## we need to check this early, as otherwise we won't find ismpx below...
##
#if [ -d $LOCAL/dmdlayers/bin -a "$TERM" = "dmd" ] ; then
#	DMD=$LOCAL/dmdlayers ; export DMD
#	TOOLS=$DMD/local ; export TOOLS
#	dirappend PATH $DMD/bin $TOOLS/bin
#	dirprepend MANPATH $DMD/man $TOOLS/man
#fi

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

if [ "$LAYERSPID" -gt 0 -o "$TERM" = xterm ] ; then
	if [ -z "$TTY" ] ; then
		export TTY=$(tty)
	fi
	export TTYN=$(tty|sed 's;/dev/;;')
fi

# UGLY, but it works
#
# WARNING: this sed expression breaks if there isn't a group-id for each gid
#
eval "$(id|sed -e 's/^uid=\([0-9]*\)(\(..*\)) gid=[0-9]*(\([^) ]*\)).*$/id=\1 uid=\2 gid=\3/')"

if [ "$id" -eq 0 ] ; then
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
	# must have X11BIN before openwin if newer X on system....
	dirappend PATH /usr/lbin /usr/ucb $X11BIN
	if $ISSUN; then
		PATH=$(echo $PATH | sed -e 's~^/bin:~~' -e 's~:/etc:~:~')
		dirprepend PATH /usr/5bin
		dirappend PATH /usr/openwin/bin
	fi
	if [ ! -d /usr/sbin ] ; then
		dirprepend PATH /usr/etc
	fi
	if [ ! -d /sbin -a ! -d /usr/etc ] ; then
		dirprepend PATH /etc
	fi
	dirprepend PATH /sbin /usr/sbin
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
	dirappend PATH /usr/lib/uucp /usr/lib
	dirappend PATH $HOME/bin
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		PS1='[!] # '
		dirappend PATH $DMD/bin $DMDSGS/bin/3b5 $DMD/local/bin
	elif [ "$TERM" = "xterm" ] ; then
		PS1='[!] # '
	else
		PS1='$TTYN:$LOGNAME@$UUNAME[$LEV.!] ${PWD#$HOME} # '
	fi
	if [ -d /var/spool/mail ] ; then
		MAILPATH="/var/spool/mail/${LOGNAME}:/var/spool/mail/root\
:/var/spool/mail/adm:/var/spool/mail/uucp:/var/spool/mail/badmail\
:/var/spool/mail/usenet:/usr/adm/lastlog:/usr/adm/sulog"
	elif [ -d /var/mail ] ; then
		MAILPATH="/var/mail/${LOGNAME}:/var/mail/root\
:/var/mail/adm:/var/mail/uucp:/var/mail/badmail\
:/var/mail/usenet:/var/adm/lastlog:/var/adm/sulog"
	else
		MAILPATH="/usr/mail/${LOGNAME}:/usr/mail/root:/usr/mail/adm\
:/usr/mail/uucp:/usr/mail/badmail:/usr/mail/usenet:/usr/adm/lastlog\
:/usr/adm/sulog"
	fi
	if [ "$VISUAL" = "emacsclient" ] ; then
		export VISUAL="emacs -nw"
	fi
	if [ "$EDITOR" = "emacsclient" ] ; then
		export EDITOR="emacs -nw"
	fi
elif [ "$uid" != "$LOGNAME" ] ; then
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	elif [ "$TERM" = "xterm" ] ; then
		PS1='[!] $ '
	else
		PS1='$TTYN:$uid($LOGNAME)@$UUNAME)[$LEV.!] ${PWD#$HOME} $ '
	fi
else
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		MYXBAN_R='$LOGNAME{$gid}@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	elif [ "$TERM" = "xterm" ] ; then
		PS1='[!] $ '
	else
		PS1='$TTYN:$LOGNAME@$UUNAME[$LEV.!] ${PWD#$HOME} $ '
	fi
fi

if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
	if [ "$LEV" -eq 0 ] ; then
		do_first_time	# in xterms, we are a login shell, but not in layers
	fi
	MYXCLR_L="$(myxban -l)"
	MYXCLR_C="$(myxban -c)"
	MYXCLR_R="$(myxban -r)"
	MYXCLR="${MYXCLR_L}${MYXCLR_C}${MYXCLR_R}"
	MYXBAN_L='$PWD'

	alias clearban='print "${MYXCLR}\c"'

	function setban
	{
		clearban
		eval myxban -l "\"$MYXBAN_L\""
		myxban -c "${WBANNER}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd $*
		eval myxban -l "\"$MYXBAN_L\""
	}

	function psm
	{
		ps -ft $(tty | sed 's~/dev/xt~xt/~')
	}

fi

if [ "$TERM" = "xterm" ] ; then
	alias clearban='WBANNER=""; setban'

	function setban
	{
		eval TBANNER='"${WBANNER:-sh}://$UUNAME/$PWD | $uid{$gid}($LOGNAME)[$LEV]:$TTYN"'
		print "\033]0;$TBANNER\007\c"
		WBANNER=""
	}

	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd $*
		setban 
	}
fi

if [ "$TERM" = "xterm" -o "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
	if [ "$VISUAL" = "emacsclient" -a -z "$DISPLAY" ] ; then
		unalias emacs
		alias emacs=_emacs
		function _emacs
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="GNU Emacs @ $UUNAME"
			setban
			mesg n
			emacs ${1+"$@"}
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
			trap "trap 1 2 3 15; setban" 1 2 3 15
		fi
		WBANNER="CU $*"
		setban
		mesg n
		/usr/bin/cu ${1+"$@"}
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
			trap "trap 1 2 3 15; setban" 1 2 3 15
		fi
		WBANNER="C-Kermit $*"
		setban
		mesg n
		$LOCAL/bin/ckermit ${1+"$@"}
		setban
	}

	if expr "$(type rlogin)" : '.* is .*/rlogin$' >/dev/null 2>&1 ; then
		RLOGIN="$(expr "$(type rlogin)" : '^.*/\([^/]*\)$')"; export RLOGIN
		unalias rlogin
		alias rlogin=_rlogin
		function _rlogin
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="rlogin $*"
			setban
			mesg n
			$RLOGIN ${1+"$@"}
			setban
		}
	fi

	if expr "$(type slogin)" : '.* is .*/slogin$' >/dev/null 2>&1 ; then
		SLOGIN="$(expr "$(type slogin)" : '^.*/\([^/]*\)$')"; export SLOGIN
		unalias slogin
		alias slogin=_slogin
		function _slogin
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="slogin $*"
			setban
			mesg n
			$SLOGIN ${1+"$@"}
			setban
		}
	fi

	if expr "$(type telnet)" : '.* is .*/telnet$' >/dev/null 2>&1 ; then
		TELNET="$(expr "$(type telnet)" : '^.*/\([^/]*\)$')" ; export TELNET
		unalias telnet
		alias telnet=_telnet
		function _telnet
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
				myxsize -s
			else
				trap "trap 1 2 3 15; setban" 1 2 3 15
			fi
			WBANNER="telnet $*"
			setban
			mesg n
			$TELNET ${1+"$@"}
			setban
		}
	fi

	if $HAVEMUSH ; then
		unalias mushC
		alias mushC=_mushC
		function _mushC
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="MUSH $*"
			setban
			mesg n
			mush -C ${1+"$@"}
			setban
		}
	fi

	if expr "$(type irc)" : '.* is .*/irc$' >/dev/null 2>&1 ; then
		unalias irc
		alias irc=_irc
		function _irc
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="IRC $*"
			setban
			mesg n
			irc ${1+"$@"}
			setban
		}
	fi

	if expr "$(type trn)" : '.* is .*/trn$' >/dev/null 2>&1 ; then
		unalias trn
		alias trn=_trn
		function _trn
		{
			trap "trap 1 2 3 15; setban" 1 2 3 15
			WBANNER="TRN $*"
			setban
			mesg n
			trn ${1+"$@"}
			setban
		}
	fi

	unalias su
	alias su=_su
	function _su
	{
		trap "trap 1 2 3 15; setban" 1 2 3 15
		WBANNER="SU $*"
		setban
		mesg n
		if [ -x /usr/5bin/su ] ; then
			/usr/5bin/su ${1+"$@"}
		elif [ -x /usr/bin/su ] ; then
			/usr/bin/su ${1+"$@"}
		else
			/bin/su ${1+"$@"}
		fi
		setban
	}

	if [ -x $LOCAL/games/nethack ] ; then
		function nethack
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; loadfont thin.9x14; setban" 1 2 3 15
				loadfont rogue.9x18
			else
				trap "trap 1 2 3 15; setban" 1 2 3 15
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
if [ "$TERM" = xterm ] ; then
	function roterm
	{
		$RSH -n "$1" "OPENWINHOME=/usr/openwin XFILESEARCHPATH=/usr/openwin/lib/%T/%N%S /usr/openwin/demo/xterm -cn -rw -sb -si -sk -sl 1024 -ls -display $DISPLAY:0 -T rsh:$1"
	}
	function rxterm
	{
		$RSH -n "$1" "$X11BIN/xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -display $DISPLAY:0 -T rsh:$1" &
	}
	function lxterm
	{
		$X11BIN/xterm -ziconbeep 1 -cn -rw -sb -si -sk -sl 2048 -ls -T $HOSTNAME $* &
	}
	setban
fi

export SECONDS="$(date '+3600*%H+60*%M+%S')"
typeset -Z2 _h _m
_hh="(SECONDS/3600)%24"
_mm="(SECONDS/60)%60"
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

# NOTE: never forget this -- it's the most incredible sed script!!!!
alias blstrip='sed "/./,/^$/!d"'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias elc='emacs -batch -q -no-site-file -f batch-byte-compile'
alias f='finger'
alias h='fc -l | tail'
alias j='jobs -l'
alias l='/bin/ls -CF'
alias la='/bin/ls -CFa'
alias ll='/bin/ls -lis'
alias lla='/bin/ls -lisa'
alias llr='/bin/ls -lisR'
alias llra='/bin/ls -lisaR'
alias lr='/bin/ls -CFR'
alias lra='/bin/ls -CFRa'
alias lsa='/bin/ls -a'
alias logout='exit 0'
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias maillog='$PAGER -e -p ": \[[0-9]*\] remote [A-Z ]*:" +G $MAILLOG'
alias realias='let LEV=$LEV-1;exec ksh'		# useless?
alias rstty='stty $SANE'
alias scvs='export CVSROOT="$(cat CVS/Root)"; print "CVSROOT=$CVSROOT"'
alias wcvs='print $CVSROOT'

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

#
#	more functions
#

function errno
{
	grep "^#define[ 	]*[A-Z][A-Z]*[ 	]*$1[ 	]" /usr/include/sys/errno.h
}

function lastcmd
{
	tr '[\001-\007]' '[\012*]' < .sh_history | tr '[\176-\377]' '[ *]' | egrep -v '^[	 }#]|^$' | tail ${1+"$@"}
}

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

function malias
{
	grep "alias[ 	]$*" $LOCAL/lib/mush/Mail.rc ~/.mushrc
}

# note, there is an mk(8) on source licensed sites
#
function mk
{
	make $* 2>&1 | tee Errors
}

function mka
{
	make $* 2>&1 | tee -a Errors
}

function mkbg
{
	nohup make $* &
	sleep 2
	mv nohup.out Errors
}

function mkd
{
	make DEBUG=-DDEBUG $* 2>&1 | tee Errors
}

function mkda
{
	make DEBUG=-DDEBUG $* 2>&1 | tee -a Errors
}

function mkdbg
{
	nohup make DEBUG=-DDEBUG $* &
	sleep 2
	mv nohup.out Errors
}

function psp
{
	ps -fp $* | sort -n +1
}

function pst
{
	ps -ft $* | sort -n +1
}

function signm
{
	grep "^#define[ 	]*SIG.*[ 	]*${1}[ 	]" /usr/include/sys/signal.h
}

function signo
{
	grep -i "^#define[ 	]*.*${1}[ 	]*[0-9]" /usr/include/sys/signal.h
}

function typeof
{
	if $ISSUN; then
		LLIBDIR=/usr/lib/lint
	else
		LLIBDIR=/usr/lib
	fi
	# should expand to allow '-l{lib}'
	egrep -i "$1" $LLIBDIR/llib-lc $LLIBDIR/llib-lm $LLIBDIR/llib-lcurses
	unset LLIBDIR
}

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
	print "ksh: exit code: $rc$EMSG"
' ERR

set -o monitor
set -o trackall
