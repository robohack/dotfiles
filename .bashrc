#
#	.bashrc - per-shell startup stuff for bash via $ENV
#
#ident	"@(#)HOME:.bashrc	35.1	13/12/02 18:39:34 (woods)"

# Assumptions:

# Files referenced:
#
#	$HOME/.bashsccs		- sourced, if it is readable
#	$HOME/.bashedit		- sourced, if it is readable else emacs editing set
#	$HOME/.bashlocal	- sourced, if it is readable

if [ -z "$SHELL" ] ; then
	export SHELL=$BASH
fi

case $- in *i*)
# everything else in this file is for interactive use only

set -o monitor
if [ -r $HOME/.bashedit ] ; then
	. $HOME/.bashedit
else
	set -o emacs
fi

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

dirappend()
{
	if [ $# -le 1 -o -z "$1" ] ; then
		print "Usage: dirappend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'${varname}
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

dirprepend()
{
	if [ $# -le 1 -o -z "$1" ] ; then
		print "Usage: dirprepend variable directory [...]" >&2
		exit 2
	fi
	varname=$1
	shift
	eval varvalue='$'${varname}
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

dirremove()
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

lnotes()
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

# UGLY, but it works
#
# WARNING: this sed expression breaks if there isn't a group-id for each gid
#
eval "$(id|sed -e 's/^uid=\([0-9]*\)(\(..*\)) gid=[0-9]*(\([^) ]*\)).*$/id=\1 uid=\2 gid=\3/')"

if [ "$id" -eq 0 ] ; then
	# got to get rid of lone ":" or any "." in PATH
	PATH="`echo $PATH | sed -e 's/::/:/g' -e 's/^://' -e 's/:$//' -e 's/^\.://' -e 's/:\.://' -e 's/:\.$//'`"
	if $ISSUN; then
		PATH="`echo $PATH | sed -e 's~^/bin:~~' -e 's~:/etc:~:~'`"
		dirprepend PATH /usr/5bin
		dirappend PATH /usr/openwin/bin
	else
		dirprepend PATH /etc
	fi
	dirprepend PATH /sbin /usr/sbin
	dirappend PATH /usr/etc /usr/lbin /usr/ucb /usr/bin/X11
	if [ -n "$LOCAL" ] ; then
		dirappend PATH $LOCAL/etc $LOCAL/sbin $LOCAL/bin $LOCAL/lib
	fi
	if [ -n "$GNU" ] ; then
		dirappend PATH $GNU/etc $GNU/sbin $GNU/bin $GNU/lib
	fi
	if [ -n "$CONTRIB" ] ; then
		dirappend PATH $CONTRIB/etc $CONTRIB/sbin $CONTRIB/bin $CONTRIB/lib
	fi
	dirappend PATH /usr/lib/uucp /usr/lib
	dirappend PATH $HOME/bin
	case "$TERM" in
	xterm)
		PS1="$UUNAME # "
		;;
	*)
		PS1="$TTYN:<$LOGNAME@$UUNAME> # "
		;;
	esac
elif [ "$uid" != "$LOGNAME" ] ; then
	case "$TERM" in
	xterm)
		PS1="$UUNAME $ "
		;;
	*)
		PS1="$TTYN:<$uid($LOGNAME)@$UUNAME)> $ "
		;;
	esac
else
	case "$TERM" in
	xterm)
		PS1="$UUNAME $ "
		;;
	*)
		PS1="$TTYN:<$LOGNAME@$UUNAME> $ "
		;;
	esac
fi

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

if type ismpx >/dev/null  2>&1 ; then
	: might just be running layers
else
	# otherwise it's just not possible....
	ismpx ()
	{
		false
	}
fi

if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
	if [ "$LEV" -eq 0 ] ; then
		do_first_time	# in xterms, we are a login shell, but not in layers
	fi
	MYXCLR_L="$(myxban -l)"
	MYXCLR_C="$(myxban -c)"
	MYXCLR_R="$(myxban -r)"
	MYXCLR="${MYXCLR_L}${MYXCLR_C}${MYXCLR_R}"
	MYXBAN_L='`pwd`'

	alias clearban='print "${MYXCLR}\c"'

	setban ()
	{
		clearban
		eval myxban -l "\"$MYXBAN_L\""
		myxban -c "${WBANNER}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	cd ()
	{
		builtin cd "$@"
		eval myxban -l "\"$MYXBAN_L\""
	}

	setban
fi

if [ "$TERM" = "xterm" ] ; then
	alias clearban='WBANNER=""; setban'

	onx11server ()
	{
		_RDISP=""
		_USAGE="$argv0: $0(): Usage: onx11server [-nS] [-D REMOTE_DISPLAY] SERVERNAME 'command string'"
		_nullopt=""
		while getopts nD:S OPTCH
		do
			case $OPTCH in
			n)
				_nullopt="-n"
				;;
			D)
				_RDISP="${OPTARG}"
				;;
			S)
				RSH=${SSH:-ssh}
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

	setban ()
	{
		if [ -z "$WBANNER" ]; then
			WBANNER=$@
		fi
		if [ -z "$BANNERPWD" ]; then
			BANNERPWD=$(pwd)
		fi
		eval TBANNER='"${WBANNER:-sh}://$UUNAME/${BANNERWD} | $uid{$gid}($LOGNAME)[$LEV]:$TTYN"'
		echo -n "]0;$TBANNER"
		WBANNER=""
	}

	cd ()
	{
		builtin cd "$@"
		BANNERWD=`pwd | sed -e "s;^$HOME;~;" -e 's;^.*/work.d/;work.d/;' -e 's;.*/home.*/\([^/][^/]*\)$;\~\1;' -e 's;^/;;'`
# not yet implemented in 4.4BSD ash...
#		BANNERWD=`pwd`
#		case "$BANNERWD" in
#		*/work.d/*)
#			PWD='work.d:'"${BANNERWD#*/work.d/}"
#			;;
#		${HOME})
#			BANNERWD='~'
#			;;
#		`dirname ${HOME}`/*)
#			PWD='~'"${BANNERWD#`dirname $HOME`/}"
#			;;
#		/home/*)
#			PWD='~'"${BANNERWD#/home/}"
#			;;
#		*/src/*)
#			PWD='src:'"${BANNERWD#*/src/}"
#			;;
#		esac
		setban
	}

	setban
fi

if $ISSUN; then
	alias df="/usr/bin/df"
fi
if [ -d /var/spool/uucp ] ; then
	alias uufollow='xtail /var/spool/uucp/.[AL]*/*'
else
	alias uufollow='xtail /usr/spool/uucp/.[AL]*/*'
fi

if [ -x /usr/ucb/rsh -a -x /bin/rsh ] ; then
	alias rsh=/usr/ucb/rsh
fi

if [ -r $HOME/.bashsccs ] ; then
	. $HOME/.bashsccs
fi

# xxx this is different than .localprofile in that it is only for
# interactive shells...
#
if [ -r $HOME/.bashlocal ] ; then
	. $HOME/.bashlocal
fi

errno ()
{
	grep "^#define[ 	]*[A-Z][A-Z]*[ 	]*$1[ 	]" /usr/include/sys/errno.h
}

lastcmd ()
{
	tr '[\001-\007]' '[\012*]' < .sh_history | \
		tr '[\176-\377]' '[ *]' | \
		egrep -v '^[	 }#]|^$' | \
		tail ${1+"$@"}
}

malias ()
{
	grep "alias[ 	]$*" $LOCAL/lib/mush/Mail.rc ~/.mushrc
}

signm ()
{
	grep "^#define[ 	]*SIG.*[ 	]*${1}[ 	]" /usr/include/sys/signal.h
}

signo ()
{
	grep -i "^#define[ 	]*.*${1}[ 	]*[0-9]" /usr/include/sys/signal.h
}

typeof ()
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

alias blstrip='sed "/./,/^$/!d"'
alias c='tput clear'
alias clear='tput clear'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias elc='emacs -batch -q -no-site-file -f batch-byte-compile'
alias h='fc -l | tail'
alias j='jobs -l'
alias l='/bin/ls $LS_OPTIONS -CF'
alias la='/bin/ls $LS_OPTIONS -CFa'
alias ll='/bin/ls $LS_OPTIONS -lis'
alias lla='/bin/ls $LS_OPTIONS -lisa'
alias llr='/bin/ls $LS_OPTIONS -lisR'
alias llra='/bin/ls $LS_OPTIONS -lisaR'
alias lr='/bin/ls $LS_OPTIONS -CFR'
alias lra='/bin/ls $LS_OPTIONS -CFRa'
alias lsa='/bin/ls $LS_OPTIONS -a'
alias logout='exit 0'
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias maillog='$PAGER -e -p ": \[[0-9]*\] remote [A-Z ]*:" +G $MAILLOG'
alias rstty='stty $SANE'
alias uuq='uusnap -av'
alias wcvs='echo $CVSROOT'

unset -f do_first_time

esac
