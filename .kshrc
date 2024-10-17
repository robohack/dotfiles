#
#	.kshrc - per-interactive-shell startup stuff
#
# This should also work for bash and other ksh-compatibles
#
#ident	"@(#)HOME:.kshrc	37.15	24/10/16 17:01:42 (woods)"

# WARNING:
# don't put comments at the bottom or you'll bugger up ksh-11/16/88e's history

# Assumptions that may cause breakage:
#
#	- $ENVFILE is the file referenced by $ENV (see ~/.kshlogin)

# Notice:  HOME may be reset to $(dirname ${ENVFILE}) or ~${LOGNAME}.
#
# Files referenced:
#
#	$HOME/.shrc		- sourced for common funcs, if needed & readable
#	$HOME/.shaliases	- sourced, if it is readable
#	$HOME/.kshsccs		- sourced, if it is readable
#	$HOME/.kshpwd		- sourced, if it is readable
#	$HOME/.bashedit		- sourced, if BASH and it is readable else emacs editing set
#	$HOME/.kshedit		- sourced, if it is readable else gmacs editing set
#	$HOME/.kshdir		- dir autoload aliases set, if it is readable
#	$HOME/.kshlocal		- per $HOME local-only hacks

#echo "$0: in ~/.kshrc ...."

# try to find a related dotfiles, even when using "su" or a sub-shell
#
# note:  this code is duplicated in ~/.ashrc
#
if [ -n "${ENVFILE:-${ENV}}" ]; then
	envhome=$(dirname ${ENVFILE:-${ENV}})
fi
# there's a bit of a chicken&egg situation w.r.t. LOGNAME (see ~/.shrc)
loghome=$(eval print ~${LOGNAME})
if [ -n "${ENVFILE:-${ENV}}" -a -f ${envhome}/.shrc ]; then
	HOME=${envhome}
elif [ -r ${loghome}/.shrc ]; then
	HOME=${loghome}
fi
# else HOME stays unchanged...
unset envhome loghome

# Get basic shell setup from a .shrc
#
# It is assumed of course that "zhead" is defined as a function in the desired
# ~/.shrc file, and only there.
#
# note:  this code is duplicated in ~/.ashrc
#
if ! typeset -f zhead >/dev/null 2>&1 ; then
	if [ -r ${HOME}/.shrc ]; then
		. ${HOME}/.shrc
	fi
fi

# ~/.shrc also does the tests for interactive/login shells and will also have
# returned "early" if neither (except if $FROM_DOT_PROFILE).
#
if ${sh_is_interactive} || ${sh_is_login}; then
	: OK
else
	# Everything else in this file is for interactive use only, and since
	# most ksh versions default to ENV=~/.kshrc if ENV is not set, and since
	# most older versions, including pdksh and most of its derivatives
	# (except mksh(1) and oksh(1)) will source this file for non-interactive
	# shells, we must exit now.
	#
	return
fi

set -o nolog		# no functions in $HISTFILE (xxx not supported by BASH)

if [ -n "${BASH}" ]; then
	# note this is the "history number", matching the numbers as shown by
	# 'fc -l' vs. the so-called "command number" ('\#')
	_c='\!'
else
	_c='!'
fi

# If I remember correctly SHLVL was not in early Ksh.... (and it's not in pdksh
# nor its derivatives).  It counts from one and is exported in the environment.
#
# Also, LEV counts (effectively) from zero, and is not set at level zero so as
# to avoid presenting redundant useless information using a simple
# ${LEV:+${LEV}} style of expansion.
#
case ${0} in
-*)
	: this is a login shell
	unset LEV
	;;
*)
	if [ ${SHLVL:-0} -eq 1 -o ${PPID:-0} -eq 1 -o ${PPID:-1} -eq ${LAYERSPID:-0} ] ; then
		: this is still considered a "login" shell
		unset LEV
	else
		typeset -i LEV
		if [ -n "${LEV}" ] ; then
			let LEV+=1
		else
			# iff SHLVL is available, try starting with it...
			if [ -n "${SHLVL}" ]; then
				let LEV=${SHLVL}-1
			else
				let LEV=1
			fi
		fi
		export LEV
	fi
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
# Note also the expression produced by 'date' is evaluated immediately by 'bc'
# for the benefit of Bash, which doesn't expand variables and then evaluate
# their content whenever they are referenced in an arithmetic expression like
# Ksh does.
#
if type bc >/dev/null 2>&1; then
	SECONDS=$(date '+3600*%H+60*%M+%S' | bc)
else
	SECONDS=$(eval expr $(date '+3600 \* %H + 60 \* %M + %S'))
fi

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

# We'll be wanting zero-filled 2-digit expansion for our hours and minutes
# variables.  Sadly Bash cannot do this, so shows ugly times for ten minutes of
# every hour.
#
# N.B.:  Some ksh clones always interpret leading zeros as octal, and so some
# times (i.e. the hours of 8 and 9, or the minutes of 8 and 9) cannot be parsed
# as numbers, so be careful using $_h and $_m in strange places -- eval $_hh or
# $_mm instead.
#
if [ -z "${BASH}" ]; then
	typeset -Z2 _h _m
fi

# a magic expression that evaluates the above expressions to set the two
# variables we've configured specially above, and then expands those two
# variables in a standard "HH:MM" 24-hr format to show the current time.
#
if [ -z "${BASH}" ]; then
	_time='${_x[(_m=_mm)==(_h=_hh)]}$_h:$_m'
else
	# n.b.: bash (and pdksh and its derivatives) interprete numbers with a
	# leading zero as octal, thus we preventatively strip them to avoid
	# conversion errors
	#
	_time='${_x[(_m=_mm)==(_h=_hh)]}$(printf "%02d:%02d" ${_h##0} ${_m##0})'
fi

# note this will be appended to.... (here _time must be quoted with double quotes)
PS1="${_time} "

if [ "$(ismpx)" = "yes" -o "$TERM" = "dmd-myx" ] ; then
	unset -f cd || true
	alias cd='_cd'
	# NOTE:  may be re-defined in ~/.kshpwd
	function _cd
	{
		# Ksh doesn't have "chdir"
		\cd "${@}"
		# dumber than setban
		BANNER_PWD=${PWD}
		# faster than setban
		eval myxban -l "\"$MYXBAN_L\""
	}
fi
# else
case "$TERM" in
xterm*)
	PS1="${PS1}"'[${LEV:+${LEV}.}'"${_c}"']'

	unset -f cd || true
	alias cd='_cd'
	# NOTE:  may be re-defined in ~/.kshpwd
	function _cd
	{
		# Ksh doesn't have "chdir"
		\cd "${@}"
		BANNER_PWD=${PWD}
		setban
	}
	;;
*)
	# XXX these probably are not right to use ${BANNER_PWD} any more?
	#
	if [ "$user" != "$LOGNAME" ] ; then
		PS1="${PS1}"'$TTYN:$user($LOGNAME)@$UUNAME)[${LEV:+${LEV}.}'"$_"'] ${BANNER_PWD#$HOME}'
	else
		PS1="${PS1}"'$TTYN:$LOGNAME@$UUNAME[${LEV:+${LEV}.}'"$_"'] ${BANNER_PWD#$HOME}'
	fi
	;;
esac
PS1="${PS1} ${PSc} "

if [ -r $HOME/.shaliases ] ; then
	. $HOME/.shaliases
fi
# finally we can clean up unnecessary functions
if typeset -f rm_alias_funcs >/dev/null 2>&1 ; then
	rm_alias_funcs
fi
if [ -r $HOME/.kshsccs ] ; then
	. $HOME/.kshsccs
fi
if type setban >/dev/null 2>&1 && [ -r $HOME/.kshpwd ] ; then
	. $HOME/.kshpwd
fi
if [ -n "$KSH_VERSION" ] ; then
	#
	# this (now?) also works for pdksh and ksh93
	#
	set -o braceexpand
fi

if [ -r $HOME/.kshdir ] ; then
	alias pushd='unalias pushd popd showd sd;. $HOME/.kshdir; pushd'
	alias popd='unalias pushd popd showd sd;. $HOME/.kshdir; popd'
	alias showd='unalias pushd popd showd sd;. $HOME/.kshdir; showd'
fi

# provide ksh(1) "print" as echo, if not available (e.g. for bash and any other
# future shells which might share ~/.kshrc)
#
if type print >/dev/null 2>&1; then
	:
else
	if type alias >/dev/null 2>&1; then
		alias print=echo
	else
		print ()
		{
			echo ${1+"${@}"}
		}
	fi
fi

trap '
	rc=$?;
	if ((ERRNO > 0)); then
		EMSG="; errno: $ERRNO"
	else
		EMSG=""
	fi;
	print "${0#-}: exit code: $rc$EMSG"
	unset rc EMSG
' ERR

# NOTE: some versions of some shells complain here if not connected to a tty
#
set -o monitor

if [ -n "${BASH}" ]; then
	if [ -r $HOME/.bashedit ] ; then
		. $HOME/.bashedit
	else
		set -o emacs
	fi
else
	if [ -r $HOME/.kshedit ] ; then
		. $HOME/.kshedit
	else
		set -o gmacs 2>/dev/null || set -o emacs
		alias __A="$(printf '\020')"		# ^P: up arrow
		alias __B="$(printf '\016')"		# ^N: down arrow
		alias __C="$(printf '\006')"		# ^F: right arrow
		alias __D="$(printf '\002')"		# ^B: left arrow
		alias __F="$(printf '\005')"		# ^E: end of line, END key
		alias __H="$(printf '\001')"		# ^A: beginning of line, HOME key
	fi
fi

# Do this at very nearly the very end...
#
if [ -r $HOME/.kshlocal ] ; then
	. $HOME/.kshlocal
fi

# N.B.:  Do this only at the very very end!
#
set -h
