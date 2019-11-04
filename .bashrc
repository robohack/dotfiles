#
#	.bashrc - per-shell startup stuff for bash via $ENV
#
#ident	"@(#)HOME:.bashrc	36.1	19/11/03 17:03:00 (woods)"

# Assumptions:

# Files referenced:
#
#	$HOME/.shrc		- sourced for common shell functions
#	$HOME/.bashsccs		- sourced, if it is readable
#	$HOME/.bashedit		- sourced, if it is readable else emacs editing set
#	$HOME/.bashlocal	- sourced, if it is readable

if [ -z "$SHELL" ] ; then
	export SHELL=$BASH
fi

# we assume ~/.profile is sourced only by login shells, and by
# ~/.xinitrc or by a window manager, but won't be sourced by default
# by a non-interactive shell
#
if ! typeset -f zhead >/dev/null ; then
	. $HOME/.shrc
fi
rm_alias_funcs

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
		echo "Usage: dirappend variable directory [...]" >&2
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
		echo "Usage: dirprepend variable directory [...]" >&2
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
				printf '\nYou have notes on:\n'
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
do_first_time()
{
	if [ -x /usr/games/fortune ] ; then
		/usr/games/fortune
	elif [ -x "$FORTUNE" ] ; then
		$FORTUNE
	fi
	if [ -r calendar -o -r .month ] ; then
		printf "\nToday's Events:\n"
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
				printf '\nNotes on: \n' *
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
	MYXBAN_L='$(pwd)'

	# xxx this doesn't mean what it used to mean...
	alias clearban='printf "${MYXCLR}"; WBANNER=""; setban'

	# XXX this probably doesn't do what I expect it to do any more either...
	# (because myxban is special in being able to set just one part)
	setban ()
	{
		if [ $# -ge 1 ]; then
			WBANNER="$@"
		fi
		if [ -z "$BANNER_PWD" ]; then
			BANNER_PWD=$(pwd)
		fi
		printf "${MYXCLR}"
		eval myxban -l "\"$MYXBAN_L\""
		myxban -c "${WBANNER:-$(basename ${SHELL})}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	# basic form -- may be re-defined in ~/.bashpwd
	cd ()
	{
		builtin cd "$*"
		eval myxban -l "\"$MYXBAN_L\""
	}

fi
# else
case "$TERM" in
"xterm")
	alias clearban='WBANNER=""; setban'

	setban ()
	{
		if [ -z "$WBANNER" ]; then
			# no trailing space -- usually just used to
			# set alternate shell name
			WBANNER=$@
		fi
		if [ -z "$BANNER_PWD" ]; then
			# only if needed (it's expensive), usually done by 'cd'
			BANNER_PWD=$(pwd)
		fi
		if [ "$uid" = "$LOGNAME" ]; then
			eval TBANNER='"${WBANNER:-$(basename ${SHELL})}://$UUNAME/$BANNER_PWD | $uid[$LEV]:$TTYN"'
		else
			eval TBANNER='"${WBANNER:-$(basename ${SHELL})}://$UUNAME/$BANNER_PWD | $uid:$gid($LOGNAME)[$LEV]:$TTYN"'
		fi
		printf "\033]0;${TBANNER}\007"
	}

	cd ()
	{
		builtin cd "$@"
		BANNER_PWD=$(pwd | sed -e "s;^$HOME;~;" -e 's;^.*/work.d/;work.d/;' -e 's;.*/home.*/\([^/][^/]*\)$;\~\1;' -e 's;^/;;')
# not yet implemented...
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
	;;
esac

# UGLY, but it works
#
# WARNING: this sed expression breaks if there isn't a group-id for each gid
#
eval "$(id|sed -e 's/^uid=\([0-9]*\)(\(..*\)) gid=[0-9]*(\([^) ]*\)).*$/id=\1 uid=\2 gid=\3/')"

if [ "$id" -eq 0 ] ; then
	# got to get rid of lone ":" or any "." in PATH
	PATH=$(echo $PATH | sed -e 's/::/:/g' -e 's/^://' -e 's/:$//' -e 's/^\.://' -e 's/:\.://' -e 's/:\.$//')
	if $ISSUN; then
		PATH=$(echo $PATH | sed -e 's~^/bin:~~' -e 's~:/etc:~:~')
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

alias backslashjoin='sed -e :a -e "/\\\\$/N; s/\\\\\\n//; ta"'
alias blsqueeze='sed "/./,/^$/!d"'
alias blstrip='sed "/./,/^$/!d"'
alias c='tput clear'
alias clear='tput clear'
alias deadlinks='find . -type l -a ! \( -follow -type f \) -print'
alias ds='$PAGER'
alias e='${VISUAL:-$EDITOR}'
alias ealias='e $ENV'
alias elc='emacs -batch -q -no-site-file -f batch-byte-compile'
# Don't list directories and links by appending "! -type d ! -type l"
alias findls='find . ! -name . -prune'
alias gitfind='git ls-tree -r --name-only HEAD'
alias h='fc -l | tail'
alias j='jobs -l'
alias l='/bin/ls $LS_OPTIONS -CF'
alias lD='/bin/ls -CFd'
alias lL='/bin/ls -CFL'
alias la='/bin/ls $LS_OPTIONS -CFa'
alias ll='/bin/ls $LS_OPTIONS -lis'
alias llD='/bin/ls -lisd'
alias llL='/bin/ls -lisL'
alias lla='/bin/ls $LS_OPTIONS -lisa'
alias lld='/bin/ls -lisd'
alias llr='/bin/ls $LS_OPTIONS -lisR'
alias llra='/bin/ls $LS_OPTIONS -lisaR'
alias lr='/bin/ls $LS_OPTIONS -CFR'
alias lra='/bin/ls $LS_OPTIONS -CFRa'
alias lsa='/bin/ls $LS_OPTIONS -a'
alias logout='exit 0'
alias maildate='LANG=c date "+%a, %d %b %Y %T %z"'
alias maillog='$PAGER -e -p ": \[[0-9]*\] remote [A-Z ]*:" +G $MAILLOG'
alias nosgr='echo '
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias rstty='stty $SANE'
alias rsyncbackup='rsync -a -H -E --numeric-ids'
alias srcfind="find . -type d \( -name CVS -or -name .git -or -name .svn -or -name build -or -name 'build-*' -or -name autom4te.cache \) -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name '.*ignore' ! -name '[Tt][Aa][Gg][Ss]' -print"
alias srcfind0="find . -type d \( -name CVS -or -name .git -or -name .svn -or -name build -or -name 'build-*' -or -name autom4te.cache \) -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name '.*ignore' ! -name '[Tt][Aa][Gg][Ss]' -print0"
alias uuq='uusnap -av'
alias wcvs='echo $CVSROOT'
alias zds="z$PAGER"

unset -f do_first_time
