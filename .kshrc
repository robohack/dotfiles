#
#	.kshrc - per-shell startup stuff
#
#ident	"@(#)HOME:.kshrc	4.1	94/06/17 22:17:49 (woods)"

# WARNING:
# don't put comments at the bottom or you'll bugger up ksh-11/16/88e's history

#set -o nolog		# no functions in $HISTFILE
set -o monitor
set -o trackall

export PATH="$PATH"

trap '
	rc=$?;
	if ((ERRNO > 0)); then
		EMSG="; errno: $ERRNO"
	else
		EMSG=""
	fi;
	echo "ksh: exit code: $rc$EMSG"
' ERR

if typeset -f dirappend >/dev/null ; then
	unset -f dirappend
fi

function dirappend
{
	if [ $# -le 1 -o -z "$1" ] ; then
		echo "Usage: dirappend variable directory [...]" >&2
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
		echo "Usage: dirprepend variable directory [...]" >&2
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

# for window systems, first time a shell starts (i.e. $LEV == 0)
#
function do_first_time
{
	if [ -x /usr/games/fortune ] ; then
		/usr/games/fortune
	elif [ -x $LOCAL/games/fortune ] ; then
		$LOCAL/games/fortune
	fi
	if [ -r calendar -o -r .month ] ; then
		echo "\nToday's Events:"
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
				echo '\nNotes on: ' *
			fi
		)
	fi
}

# we need to check this early, as otherwise we won't find ismpx below...
#
if [ -d $LOCAL/dmdlayers/bin -a "$TERM" = "dmd" ] ; then
	DMD=$LOCAL/dmdlayers ; export DMD
	TOOLS=$DMD/local ; export TOOLS
	dirappend PATH $DMD/bin $TOOLS/bin
	dirprepend MANPATH $DMD/man $TOOLS/man
fi

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
eval "$(id|sed -e 's/^uid=\([0-9]*\)(\(..*\)) gid=[0-9]*(\([^) ]*\)).*$/id=\1 uid=\2 gid=\3/')"

if [ "$id" -eq 0 ] ; then
	if [ -n "$HISTFILE" ] ; then
		HISTFILE="/$(basename $HISTFILE)"
	else
		HISTFILE="/.sh_history"
	fi
	if $ISSUN; then
		PATH=${PATH#".:"}		# in case /usr/5bin/su hasn't had the patch
		PATH=${PATH#"/bin:"}		# don't need /bin any more (it's a symlink)
		# should also trim /etc from the middle, but ksh doesn't offer this feature
		dirprepend PATH /usr/5bin
		dirappend PATH /usr/openwin/bin
	else
		dirprepend PATH /etc
	fi
	dirappend PATH /usr/etc /usr/lbin /usr/ucb /usr/bin/X11
	if [ -n "$LOCAL" ] ; then
		dirappend PATH $LOCAL/etc $LOCAL/bin $GNU/bin $LOCAL/lib
	fi
	dirappend PATH /usr/lib/uucp /usr/lib
	dirappend PATH /usr/ot/bin
	dirappend PATH $HOME/bin
	dirappend PATH /etc/apc/bin $APCBIN/xbin
	dirappend PATH $APCCONFIG/bin /apc/bin /apc/xbin /apc/lbin
	dirappend PATH /usr/local/apc/bin /usr/local/apc/xbin
	dirappend MANPATH /apc/man
	if [ "$(ismpx)" = yes ] ; then
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
	else
		MAILPATH="/usr/mail/${LOGNAME}:/usr/mail/root:/usr/mail/adm\
:/usr/mail/uucp:/usr/mail/badmail:/usr/mail/usenet:/usr/adm/lastlog\
:/usr/adm/sulog"
	fi
	if [ "$VISUAL" = "emacsclient" ] ; then
		export VISUAL=emacs
	fi
	# fix for ksh-11/16/88b
	#alias passwd='/bin/passwd'
elif [ "$uid" != "$LOGNAME" ] ; then
	if [ "$(ismpx)" = yes ] ; then
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	elif [ "$TERM" = "xterm" ] ; then
		PS1='[!] $ '
	else
		PS1='$TTYN:$uid($LOGNAME)@$UUNAME)[$LEV.!] ${PWD#$HOME} $ '
	fi
else
	if [ "$(ismpx)" = yes ] ; then
		MYXBAN_R='$LOGNAME{$gid}@$UUNAME[$LEV]:$TTYN'
		PS1='[!] $ '
	elif [ "$TERM" = "xterm" ] ; then
		PS1='[!] $ '
	else
		PS1='$TTYN:$LOGNAME@$UUNAME[$LEV.!] ${PWD#$HOME} $ '
	fi
fi
if [ "$(ismpx)" = yes ] ; then
	if [ "$LEV" -eq 0 ] ; then
		do_first_time
	fi
	MYXCLR_L="$(myxban -l)"
	MYXCLR_C="$(myxban -c)"
	MYXCLR_R="$(myxban -r)"
	MYXCLR="${MYXCLR_L}${MYXCLR_C}${MYXCLR_R}"
	MYXBAN_L='$PWD'

	function setban
	{
		echo "${MYXCLR}\c"
		eval myxban -l "\"$MYXBAN_L\""
		eval myxban -r "\"$MYXBAN_R\""
	}

	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd $*
		eval myxban -l "\"$MYXBAN_L\""
	}

	unalias cu
	alias cu=_cu
	function _cu
	{
		trap "trap 0 1 2 3 15; mkmenu -; myxban -c" 0 1 2 3 15
		myxban -c "cu $*"
		myxsize -s
		/usr/bin/cu "$@"
	}

	unalias ckermit
	alias ckermit=_ckermit
	function _ckermit
	{
		trap "trap 0 1 2 3 15; mkmenu -; myxban -c" 0 1 2 3 15
		myxban -c "C-Kermit"
		myxsize -s
		$LOCAL/bin/ckermit "$@"
	}

	unalias rlogin
	alias rlogin=_rlogin
	function _rlogin
	{
		trap "trap 0 1 2 3 15; mkmenu -; myxban -c" 0 1 2 3 15
		myxban -c "rlogin $*"
		/usr/ucb/rlogin "$@"
	}

	unalias telnet
	alias telnet=_telnet
	function _telnet
	{
		trap "trap 0 1 2 3 15; mkmenu -; myxban -c" 0 1 2 3 15
		myxban -c "telnet $*"
		/usr/ucb/telnet "$@"
	}

	unalias mushC
	alias mushC=_mushC
	function _mushC
	{
		trap "trap 0 1 2 3 15; mkmenu -; myxban -c" 0 1 2 3 15
		myxban -c "MUSH $*"
		mush -C "$@"
	}

	unalias su
	alias su=_su
	function _su
	{
		trap "trap 0 1 2 3 15; setban" 0 1 2 3 15
		if [ -x /usr/5bin/su ] ; then
			/usr/5bin/su "$@"
		else
			/bin/su "$@"
		fi
	}

	if [ -x $LOCAL/games/nethack ] ; then
		function nethack
		{
			trap "trap 0 1 2 3 15; loadfont thin.9x14" 0 1 2 3 15
			loadfont rogue.9x18
			$LOCAL/games/nethack
		}
	fi

	function psm
	{
		ps -ft $(tty | sed 's~/dev/xt~xt/~')
	}
	alias umenu="echo 'cat /usr/spool/news/out.going/*/batchlog' | mkmenu NewsChkBatch ; echo 'uustat -m' | mkmenu UUSTAT"
	setban
fi

if [ "$TERM" = "xterm" ] ; then
	if [ "$LEV" -eq 0 ] ; then
		do_first_time
	fi

	function setban
	{
		eval TBANNER='"${XTBANNER:+$XTBANNER - }$PWD - $uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN"'
		echo "\033]0;$TBANNER\007\c"
		XTBANNER=""
	}

	alias clearban='XTBANNER=""; setban'

	unalias cd
	alias cd='_cd'
	function _cd
	{
		\cd $*
		setban 
	}

	unalias cu
	alias cu=_cu
	function _cu
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="cu $*"
		setban
		/usr/bin/cu "$@"
	}

	unalias ckermit
	alias ckermit=_ckermit
	function _ckermit
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="C-Kermit $*"
		setban
		$LOCAL/bin/ckermit "$@"
	}

	unalias rlogin
	alias rlogin=_rlogin
	function _rlogin
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="rlogin $*"
		setban
		/usr/ucb/rlogin "$@"
	}

	unalias telnet
	alias telnet=_telnet
	function _telnet
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="telnet $*"
		setban
		/usr/ucb/telnet "$@"
	}

	unalias mushC
	alias mushC=_mushC
	function _mushC
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="MUSH $*"
		setban
		mush -C "$@"
	}

	unalias pnotes
	alias pnotes=_pnotes
	function _pnotes
	{
		trap "trap 0 1 2 3 15; clearban" 0 1 2 3 15
		XTBANNER="PNotes $*"
		setban
		xpnerun pnotes "$@"
	}

	unalias su
	alias su=_su
	function _su
	{
		trap "trap 0 1 2 3 15; setban" 0 1 2 3 15
		if [ -x /usr/5bin/su ] ; then
			/usr/5bin/su "$@"
		else
			/bin/su "$@"
		fi
	}

	function rxload
	{
		rsh -n "$1" "/usr/bin/X11/xload -display $UUNAME:0"
	}

	function rxperfmon
	{
		rexec "$1" "/usr/bin/X11/contrib/xperfmon -display $UUNAME:0"
	}

	function roterm
	{
	#	rsh -n "$1" "OPENWINHOME=/usr/openwin XFILESEARCHPATH=/usr/openwin/lib/%T/%N%S /usr/openwin/bin/xterm -ls -cn -rw -sb -si -sk -sl 1024 -vb -ut -fn 9x15 -fb 9x15bold -display $UUNAME:0 -name rsh:$1"
		rsh -n "$1" "OPENWINHOME=/usr/openwin XFILESEARCHPATH=/usr/openwin/lib/%T/%N%S /usr/openwin/bin/xterm -ls -cn -rw -sb -si -sk -sl 1024 -vb -ut -display $UUNAME:0 -name rsh:$1"
	}

	function rxterm
	{
	#	rsh -n "$1" "/usr/bin/X11/xterm -ls -cn -rw -sb -si -sk -sl 1024 -vb -ut -fn 9x15 -fb 9x15bold -display $UUNAME:0 -name rsh:$1"
		rsh -n "$1" "/usr/bin/X11/xterm -ls -cn -rw -sb -si -sk -sl 1024 -vb -ut -display $UUNAME:0 -name rsh:$1"
	}

#	alias xterm='/usr/bin/X11/xterm -cn -rw -sb -si -sk -sl 1024 -ut -fn 9x15 -fb 9x15bold -ls'
	alias xterm='/usr/bin/X11/xterm -cn -rw -sb -si -sk -sl 1024 -ut -ls'

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
fi

alias blstrip='sed "/./,/^$/!d"'
alias c='tput clear'
alias clear='tput clear'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias ee='e -p Errors'
alias f='finger'
alias mfrom='mush -H:n'
alias fw='who -HurTbA'
alias h='fc -l | tail'
alias mhdrs='mush -H -f'
alias j='jobs -l'
alias l='/bin/ls -CF'
alias la='/bin/ls -CFa'
alias ll='/bin/ls -lis'
alias lla='/bin/ls -lisa'
alias llr='/bin/ls -lisR'
alias llra='/bin/ls -lisaR'
alias lpq='lpstat -o'
alias lr='/bin/ls -CFR'
alias lra='/bin/ls -CFRa'
alias lsa='/bin/ls -a'
alias logout='exit 0'
#alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok rows $LINES cols $COLUMNS'
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias psa='ps -af | sort -n +1'
alias pse='ps -ef | sort -n +1'
alias maillog='$PAGER -e +G $MAILLOG'
alias realias='let LEV=$LEV-1;exec ksh'
#alias rstty='stty $SANE; stty rows ${LINES:-$(tput lines)} cols ${COLUMNS:-$(tput cols)}'
alias rstty='stty $SANE'
alias scvs='export CVSROOT="$(cat CVS/Root)"; echo "CVSROOT=$CVSROOT"'
alias uuq='uustat -a'
alias wcvs='echo $CVSROOT'

if [ -d /var/spool/uucp ] ; then
	alias uufollow='xtail /var/spool/uucp/.[AL]*/*'
else
	alias uufollow='xtail /usr/spool/uucp/.[AL]*/*'
fi

if [ -x /usr/ucb/rsh -a -x /bin/rsh ] ; then
	alias rsh=/usr/ucb/rsh
fi

if [ -n "$X11HOME" -a "$TERM" != xterm ] ; then
	alias X='xinit -a 2 -p 1 -s 15'
	alias Xtwm='WindowMgr=twm xinit -a 2 -p 1 -s 15'
	alias Xmwm='WindowMgr=mwm xinit -a 2 -p 1 -s 15'
fi

if [ -r $HOME/.kshsccs ] ; then
	. $HOME/.kshsccs
fi
if [ -r $HOME/.kshpwd ] ; then
	. $HOME/.kshpwd
	cd $(pwd)
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
		echo 'Usage: lastlog [-a]' >&2
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

alias pushd='unalias pushd popd showd sd;. $HOME/.kshdir; pushd'
alias popd='unalias pushd popd showd sd;. $HOME/.kshdir; popd'
alias showd='unalias pushd popd showd sd;. $HOME/.kshdir; showd'

unset -f do_first_time
