#
#	.kshrc - per-interactive-shell startup stuff
#
# This should also work for bash and other ksh-compatibles
#
#ident	"@(#)HOME:.kshrc	36.10	21/03/21 17:38:13 (woods)"

# WARNING:
# don't put comments at the bottom or you'll bugger up ksh-11/16/88e's history

# Files referenced:
#
#	$(dirname ${ENVFILE})/.shrc - sourced for common funcs if needed & avail.
#	~${LOGNAME}/.shrc       - sourced for common functs, if still needed
#	~${LOGNAME}/.localprofile - sourced for $MAILDOMAIN, if needed & avail.
#	$HOME/.kshsccs		- sourced, if it is readable
#	$HOME/.kshpwd		- sourced, if it is readable
#	$HOME/.kshedit		- sourced, if it is readable else gmacs editing set
#	$HOME/.kshdir		- dir autoload aliases set, if it is readable
#	$HOME/.kshlocal		- local-only hacks

case $SHELL in
*bash*)
	# note this is the "history number", matching the numbers as shown by
	# 'fc -l' vs. the so-called "command number" ('\#')
	_c='\!'
	alias print=echo
	;;
*)
	set -o nolog		# no functions in $HISTFILE
	_c='!'
	;;
esac

export PATH

# If I remember correctly SHLVL was not in early Ksh.... (and it's not in pdksh
# nor its derivatives)
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

# Handle the case of sourcing this file e.g. by a "su" shell user, or some other
# interactive sub-shell, etc.
#
# It is assumed that ~/.profile, which would otherwise source ~/.shrc, is
# sourced only by login shells, and by ~/.xinitrc or by a window manager, but
# won't be sourced by default by a non-interactive shell or a "su" shell.  It is
# also assumed of course that "zhead" is defined as a function in the desired
# ~/.shrc file.
#
if ! typeset -f zhead >/dev/null ; then
	# try to find a related .shrc, even when using "su" or a sub-shell
	if [ -n "${ENVFILE}" ]; then
		. $(dirname ${ENVFILE})/.shrc
		# also try to set MAILDOMAIN for .emacs.el if it was not set...
		if [ -z "${MAILDOMAIN}" -a -r $(dirname ${ENVFILE})/.localprofile ]; then
			. $(dirname ${ENVFILE})/.localprofile
		fi
	elif [ -n "${LOGNAME}" ]; then
		eval shrc=~${LOGNAME}/.shrc
		if [ -r ${shrc} ]; then
			. ${shrc}
		fi
		eval lprof=~${LOGNAME}/.localprofile
		if [ -z "${MAILDOMAIN}" -a -r ${lprof} ]; then
			. ${lprof}
		fi
		unset shrc lprof
	fi
	if typeset -f rm_alias_funcs >/dev/null ; then
		rm_alias_funcs
	fi
fi

# these functions using ${..##..} and ${..%%..} instead of $(expr ...)
# should be more efficient than their original dirappend() and
# dirprepend() counterparts (because of the fork&exec of expr)

function append2path
{
	if [ $# -le 1 -o -z "$1" ] ; then
		echo "Usage: $0 variable directory [...]" >&2
		return 2
	fi
	typeset varname=$1
	shift
	# the eval of 'test' below will trigger the ERR trap because the shell
	# doesn't see it as a condition expression, so disable ERR...
	trap - ERR
	while [ $# -gt 0 ] ; do
		if [ -d "$1" ]; then
			# xxx hmmm... I can't remember why this uses inverted logic...
			if ! eval test -z "\"\${${varname}##*:$1:*}\"" -o -z "\"\${${varname}%%*:$1}\"" -o -z "\"\${${varname}##$1:*}\"" -o -z "\"\${${varname}##$1}\"" ; then
###				echo "append2path: adding $1 to the end of ${varname}"
				eval "${varname}=\$${varname}:$1"
###			else
###				echo "append2path: ignoring $1 (for ${varname})"
			fi
		fi
		shift
	done
}

# xxx hmmm....  this seems to exit with the wrong exit code
# (1 if a dir is found, but 0 if a dir is not found)
#
#	$ dirprepend INFOPATH ${PKG}/share/info
#	prepend2path: exit code: 1
#
#	$ dirprepend INFOPATH /junk/share/info
#	$
#
function prepend2path
{
	if [ $# -le 1 -o -z "$1" ] ; then
		echo "Usage: $0 variable directory [...]" >&2
		return 2
	fi
	typeset varname=$1
	shift
	while [ $# -gt 0 ] ; do
		if [ -d "$1" ]; then
			if ! eval test -z "\"\${${varname}##*:$1:*}\"" -o -z "\"\${${varname}%%*:$1}\"" -o -z "\"\${${varname}##$1:*}\"" -o -z "\"\${${varname}##$1}\"" ; then
###				echo "prepend2path: adding $1 to the beginning of ${varname}"
				eval "${varname}=$1:\$${varname}"
###			else
###				echo "prepend2path: ignoring $1 (for ${varname})"
			fi
		fi
		shift
	done
}


function removefrompath
{
	if [ $# -le 1 ] ; then
		echo "Usage: $0 variable directory [...]" >&2
		exit 2
	fi
	typeset varname=$1
	shift
	while [ $# -gt 0 ] ; do
		# xxx perhaps these can be done in-shell, i.e. without sed
		if [ "$1" = ":" -o -z "$1" ] ; then
			eval $varname=$(eval echo '$'$varname | sed -e 's|::||g' -e 's|:$||')
		else
			eval $varname=$(eval echo '$'$varname | sed 's|\(:*\)'$1':*|\1|')
		fi
		shift
	done
}

unset -f dirappend dirprepend

alias dirappend=append2path
alias dirprepend=prepend2path
alias dirremove=removefrompath

unset -f ismpx
if type ismpx >/dev/null 2>&1 ; then # hmmm.... ksh93 and bash vs type?
	: might just be running layers
else
	# otherwise it's just not possible....
	alias ismpx=false
fi

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
SECONDS=$(date '+3600*%H+60*%M+%S' | bc)

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
case $SHELL in
*bash*)
	;;
*)
	typeset -Z2 _h _m
	;;
esac

# a magic expression that evaluates the above expressions to set the two
# variables we've configured specially above, and then expands those two
# variables in a standard "HH:MM" 24-hr format to show the current time.
#
_time='${_x[(_m=_mm)==(_h=_hh)]}$_h:$_m'

# note this will be appended to.... (and must be set with double quotes)
PS1="${_time} "

PS2=">>> "
PS3="??? "


if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then

	if [ "${LEV:-0}" -eq 0 ] ; then
		# in xterms, we are (normally, supposed to be) a login
		# shell, but not in layers
		do_first_time
	fi
	MYXCLR_L="$(myxban -l)"
	MYXCLR_C="$(myxban -c)"
	MYXCLR_R="$(myxban -r)"
	MYXCLR="${MYXCLR_L}${MYXCLR_C}${MYXCLR_R}"
	MYXBAN_L='$BANNER_PWD'

	# xxx this doesn't mean what it used to mean...
	function clearban
	{
		printf '%s' "${MYXCLR}";
		WBANNER="${OWBANNER}";
		setban
	}
	# XXX this probably doesn't do what I expect it to do any more either...
	# (because myxban is special in being able to set just one part)
	function setban
	{
		if [ $# -ge 1 ]; then
			WBANNER="$@"
		fi
		if [ -z "$BANNER_PWD" ]; then
			BANNER_PWD=$(pwd)
		fi
		printf '%s' "${MYXCLR}"
		eval myxban -l "\"$MYXBAN_L\""
		myxban -c "${WBANNER:-$(basename ${SHELL})}"
		eval myxban -r "\"$MYXBAN_R\""
	}

	# NOTE:  may be re-defined in ~/.kshpwd
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

	setban

fi
# else
case "$TERM" in
xterm*)
	PS1="${PS1}"'[${LEV:+${LEV}.}'"${_c}"']'

	function clearban
	{
		WBANNER="${OWBANNER}";
		setban
	}

	function setban
	{
		if [ $# -ge 1 ]; then
			# no trailing space -- usually just used to
			# set alternate shell name
			OWBANNER=${WBANNER}
			WBANNER="$@"
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

	# NOTE:  may be re-defined in ~/.kshpwd
	alias cd='_cd'
	function _cd
	{
		\cd "$@"
		BANNER_PWD=${PWD}
		setban
	}

	setban
	;;
*)
	if [ "$uid" != "$LOGNAME" ] ; then
		PS1="${PS1}"'$TTYN:$uid($LOGNAME)@$UUNAME)[${LEV:+${LEV}.}'"$_"'] ${BANNER_PWD#$HOME}'
	else
		PS1="${PS1}"'$TTYN:$LOGNAME@$UUNAME[${LEV:+${LEV}.}'"$_"'] ${BANNER_PWD#$HOME}'
	fi
	;;
esac

# UGLY, but it works
#
# NOTE:  there's a trick in here -- if there's no group-ID for your
# GID then the final expression parameter won't do anything.  However
# since we've already pre-trimmed the extra "groups=..." stuff off the
# end the only thing that'll be left is the original "gid=20" string,
# and that'll have the same effect we want anyway.
#
# That's why we call the variable which holds the primary group name
# "gid" and not gname or group or whatever...
#
eval "$(id | sed -e 's/ groups=.*$//' \
		 -e 's/uid=\([0-9]*\)(\(..*\)) /id=\1 uid=\2 /' \
		 -e 's/gid=[0-9]*(\([^) ]*\)).*$/gid=\1/')"

if [ "$id" -eq 0 ] ; then
	#
	# we always want persistent (and shared) history for 'su'
	#
	# Note only some shells append a slash when expanding "~user"...
	#
	: ${ROOT_HOME:=~root}
	if [ -n "$HISTFILE" ]; then
		HISTFILE="${ROOT_HOME%/}/$(basename $HISTFILE)"
	else
		HISTFILE="${ROOT_HOME%/}/.sh_history"
	fi
	if [ ! -f $HISTFILE ]; then
		touch $HISTFILE
	fi
	# see also ~/.kshlogin
	export HISTSIZE=2000

	export PRE_SU_PATH=$PATH

	# got to get rid of lone ":" or any "." in PATH
	PATH=$(echo $PATH | sed -e 's/::/:/g'	\
				-e 's/^://'	\
				-e 's/:$//'	\
				-e 's/^\.://'	\
				-e 's/:\.://'	\
				-e 's/:\.$//')
	if [ -n "$SU_FROM" ]; then
		# xxx this is a trick, and only necessary on machines
		# without my fixed "su" -- we reset LOGNAME in case
		# this was a proper and secure 'su' (i.e. one that
		# reset the environment, especially and including
		# $HOME, etc., including of course $LOGNAME).  This
		# does two things.  First, and most important, though
		# most dangerous, it tricks emacs, for now, into
		# finding ~$LOGNAME/.emacs[.elc].  Second it
		# simplifies the next PATH cleanup step....
		#
		# we set USER to "root" explicitly even if the user
		# did not use "su -l" as they should have...
		#
		export LOGNAME=$SU_FROM
		export USER="root"
	fi
	if [ -n "$SUDO_USER" ]; then
		# xxx this is a similar trick for OSX and Linux braindamage
		export LOGNAME=$SUDO_USER
		export USER="root"
	fi
	# also get rid of the login user's ~/{usr/bin,bin} because
	# it's usually first, and it usually contains personal hacks
	# that are probably not safe to use as root, and (if this is
	# not ~$SU_FROM/.kshrc then ~$SU_FROM/bin may also contain
	# trojans or worse)
	#
	LOGNAMEPATH=$(eval echo ~$LOGNAME/usr/bin)
	# XXX hmmm... "dirremove"???
	PATH=$(echo $PATH | sed -e "s|${LOGNAMEPATH}[:]*||")
	LOGNAMEPATH=$(eval echo ~$LOGNAME/bin)
	# XXX hmmm... "dirremove"???
	PATH=$(echo $PATH | sed -e "s|${LOGNAMEPATH}[:]*||")

###	# XXX for some reason Ksh "Version AJM 93u+ 2012-08-01" on macOS will
###	# not allow dirappend to be used while sourcing
###
###	echo $PATH
###
###	alias dirappend
###	alias dirprepend
###
###	type dirappend
###	type dirprepend
###
###	type append2path
###	type prepend2path

	# must have X11BIN before openwin if newer X on system....
	append2path PATH /usr/lbin /usr/ucb $X11BIN

	if [ -z "$UUNAME" ]; then
		# xxx don't bother with trying the real uuname -l
		export UUNAME=$(hostname)
	fi
	if [ -z "$HOSTNAME" ]; then
		export HOSTNAME=$(hostname)
	fi
	if [ -z "$DOMAINNAME" ]; then
		export DOMAINNAME=$(domainname)
	fi

	export ISSUN=false
	if [ -x /usr/bin/sun ] ; then
		if sun ; then
			ISSUN=true
			PATH=$(echo $PATH | sed 's/^\/bin://')
			if [ $(uname -r | sed 's/^\([0-9]*\).*$/\1/') -lt 5 ] ; then
				if [ "X$LOGNAME" != "Xroot" ] ; then
					prepend2path PATH /usr/5bin
				else
					append2path PATH /usr/5bin
				fi
			else
				prepend2path PATH /opt/SUNWspro/bin
			fi
			# XXX FIXME: should use OPENWINHOME ???
			# XXX FIXME: should only do this if DISPLAY set???
			append2path PATH /usr/openwin/bin /usr/openwin/demo
			append2path MANPATH /usr/openwin/share/man
		fi
	fi
	if [ ! -d /usr/sbin ] ; then
		prepend2path PATH /usr/etc	# only old BSDs
	fi
	if [ ! -d /sbin -a ! -d /usr/etc ] ; then
		prepend2path PATH /etc		# only really old systems...
	fi
	prepend2path PATH /sbin /usr/sbin
	append2path PATH /usr/libexec/uucp /usr/lib/uucp /usr/lib

	if [ -z "$UUNAME" ]; then
		TYPE_UUNAME=$(type clang 2>/dev/null)
		if [ "${TYPE_UUNAME##*/}" = "uuname" ] ; then
			UUNAME=`uuname -l`
		else
			UUNAME=`hostname`
		fi
		export UUNAME
	fi

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
		# XXX some OSX has an un-related /pkg so test /usr/pkg first!
		if [ -d /usr/pkg -a ! -L /usr/pkg ] ; then
			PKG="/usr/pkg"
		elif [ -d /opt/pkg -a ! -L /opt/pkg ] ; then
			PKG="/opt/pkg"
		elif [ -d /pkg -a ! -L /pkg ] ; then
			PKG="/pkg"
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

	if [ -z "${GNU}" ] ; then
		# NOTE:  ${GNU} must not contain multiple words!
		if [ -d /local/gnu -a ! -L /local/gnu -a -d /local/gnu/bin ] ; then
			GNU="/local/gnu"
		elif [ -d /usr/gnu -a -d /usr/gnu/bin ] ; then
			GNU="/usr/gnu"
		elif [ -d /usr/local/gnu -a -d /usr/local/gnu/bin ] ; then
			GNU="/usr/local/gnu"
		elif [ -d /opt/pkg/gnu -a -d /opt/pkg/gnu/bin ] ; then
			GNU="/opt/pkg/gnu"
		else
			GNU="/NO-gnu-FOUND"
		fi
	fi
	export GNU

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
				-e "s|${GNU:-/NONE}/bin:||"	\
				-e "s|${GNU:-/NONE}/sbin:||" )
	if [ -d "$CONTRIB" ] ; then
		append2path PATH $CONTRIB/sbin $CONTRIB/bin
		if [ ! -d $CONTRIB/sbin ] ; then
			append2path PATH $CONTRIB/etc
		fi
	fi
	if [ -d "$PKG" ] ; then
		append2path PATH $PKG/sbin $PKG/bin
	fi
	if [ -d "$SLASHOPT" ] ; then
		append2path PATH $SLASHOPT/sbin $SLASHOPT/bin
	fi
	if [ -d "$LOCAL" ] ; then
		append2path PATH $LOCAL/sbin $LOCAL/bin
		if [ ! -d $LOCAL/sbin ] ; then
			append2path PATH $LOCAL/etc
		fi
	fi
	if [ -d "$GNU" ] ; then
		append2path PATH $GNU/sbin $GNU/bin
		if [ ! -d $GNU/sbin ] ; then
			append2path PATH $GNU/etc
		fi
	fi

	append2path PATH $HOME/bin

	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		# xxx should do this in setban...
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
		append2path PATH $DMD/bin $DMDSGS/bin/3b5 $DMD/local/bin
	fi

	PS1="${PS1}"' # '
	MAILPATH=${MAILDIR}/${LOGNAME}:${MAILDOR}/root:${MAILDIR}/uucp:${MAILDIR}/usenet
	if [ "$VISUAL" = "emacsclient" ] ; then
		export VISUAL="emacs -nw"
	fi
	if [ "$EDITOR" = "emacsclient" ] ; then
		export EDITOR="emacs -nw"
	fi
	# just make damn sure PAGER is set...
	PAGER=$(type less 2>/dev/null)
	if [ "${PAGER##*/}" = "less" ] ; then
		PAGER="less"
		LESS="-M" ; export LESS
	elif [ -x /usr/xpg4/bin/more ] ; then
		# SunOS-5's, at least, has the 'G' command!
		PAGER="/usr/xpg4/bin/more"
		# use '-s' as it can't be turned on later during runtime
		MORE="-s" ; export MORE
	elif [ "${PAGER##*/}" = "more" ] ; then
		PAGER="more"
		# use '-s' as it can't be turned on later during runtime
		MORE="-sw" ; export MORE
	else
		# meow
		PAGER="cat"
	fi
	export PAGER
	if [ "$PAGER" = "less" ]; then
		MANPAGER="$PAGER -si"; export MANPAGER
	fi

	if [ -n "$DISPLAY" -a -z "$XAUTHORITY" ]; then
		#
		# XXX if root is using this .kshrc then perhaps we
		# should try copying the "xauth" information for the
		# current display to $HOME/.Xauthority instead of just
		# pointing at it...  but why bother???
		#
		export XAUTHORITY=$(eval echo ~${SU_FROM}/.Xauthority)
	fi

	if typeset -f krcmd >/dev/null ; then
		unset -f krcmd
	fi
	function krcmd
	{
		# WARNING: this version kills everyone's rcmd procs!
		kill -9 $(ps -axlc | awk '$3 == 1 && $13 == "rcmd" {print $2}')
	}
	# I don't know if this is right, or not, but let's try for now...
	cd
	OWBANNER=${WBANNER}
	WBANNER="SU $(basename ${SHELL})"
	setban
elif [ "$uid" != "$LOGNAME" ] ; then
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		# xxx should do this in setban...
		MYXBAN_R='$uid{$gid}($LOGNAME)@$UUNAME[$LEV]:$TTYN'
	fi
	PS1="${PS1}"' $ '
else
	if [ "$(ismpx)" = yes -o "$TERM" = "dmd-myx" ] ; then
		# xxx should do this in setban...
		MYXBAN_R='$LOGNAME{$gid}@$UUNAME[$LEV]:$TTYN'
	fi
	PS1="${PS1}"' $ '
fi

if type setban > /dev/null ; then

	#
	# XXX re-factor into a function which defines functions....
	#
	if [ "$VISUAL" = "emacsclient" -a -z "$DISPLAY" ] ; then
		alias emacs=_emacs
		function _emacs
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="GNU Emacs "
			setban
			mesg n
			\emacs "$@"
			clearban
		}
	fi

	alias cu=_cu
	function _cu
	{
		if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
			trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
			myxsize -s
		else
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
		fi
		OWBANNER=${WBANNER}
		WBANNER="CU $* "
		setban
		mesg n
		\cu "$@"
		clearban
	}

	alias ckermit=_ckermit
	function _ckermit
	{
		if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
			trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
			myxsize -s
		else
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
		fi
		OWBANNER=${WBANNER}
		WBANNER="C-Kermit $* "
		setban
		mesg n
		\ckermit "$@"
		clearban
	}

	_cmd=$(type rlogin 2>/dev/null)
	if [ "${_cmd##*/}" = "rlogin" ] ; then
		alias rlogin=_rlogin
		function _rlogin
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="RLOGIN $* "
			setban
			mesg n
			\rlogin "$@"
			clearban
		}
	fi

	_cmd=$(type slogin 2>/dev/null)
	if [ "${_cmd##*/}" = "slogin" ] ; then
		alias slogin=_slogin
		function _slogin
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="SLOGIN $* "
			setban
			mesg n
			\slogin "$@"
			clearban
		}
	fi

	_cmd=$(type console 2>/dev/null)
	if [ "${_cmd##*/}" = "console" ] ; then
		alias console=_console
		function _console
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="CONSOLE $* "
			setban
			mesg n
			\console "$@"
			clearban
		}
	fi

	_cmd=$(type telnet 2>/dev/null)
	if [ "${_cmd##*/}" = "telnet" ] ; then
		alias telnet=_telnet
		function _telnet
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; mkmenu -; setban" 1 2 3 15
				myxsize -s
			else
				trap "trap 1; clearban; kill -1 $$" 1
				trap "trap 2; clearban; kill -2 $$" 2
				trap "trap 3; clearban; kill -3 $$" 3
				trap "trap 15; clearban; kill -15 $$" 15
			fi
			OWBANNER=${WBANNER}
			WBANNER="TELNET $* "
			setban
			mesg n
			\telnet "$@"
			clearban
		}
	fi

	if $HAVEMUSH ; then
		alias mushC=_mushC
		function _mushC
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="MUSH $* "
			setban
			mesg n
			mush -C "$@"
			clearban
		}
	fi

	_cmd=$(type irc 2>/dev/null)
	if [ "${_cmd##*/}" = "irc" ] ; then
		alias irc=_irc
		function _irc
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="IRC $* "
			setban
			mesg n
			\irc "$@"
			clearban
		}
	fi

	_cmd=$(type trn 2>/dev/null)
	if [ "${_cmd##*/}" = "trn" ] ; then
		alias trn=_trn
		function _trn
		{
			trap "trap 1; clearban; kill -1 $$" 1
			trap "trap 2; clearban; kill -2 $$" 2
			trap "trap 3; clearban; kill -3 $$" 3
			trap "trap 15; clearban; kill -15 $$" 15
			OWBANNER=${WBANNER}
			WBANNER="TRN $* "
			setban
			mesg n
			\trn "$@"
			clearban
		}
	fi

	alias su=_su
	function _su
	{
		trap "trap 1; clearban; kill -1 $$" 1
		trap "trap 2; clearban; kill -2 $$" 2
		trap "trap 3; clearban; kill -3 $$" 3
		trap "trap 15; clearban; kill -15 $$" 15
		showargs="root"
		if [ $# -ge 1 ]; then
			case "$@" in
			"-")
				showargs="root"
				;;
			*)
				showargs="$@"
				;;
			esac
		fi
		if [ "$showargs" = "root" ]; then
			# we know who root is...
			showargs=""
		fi
		OWBANNER=${WBANNER}
		WBANNER="SU ${showargs:+${showargs} }sh"
		setban
		mesg n
		if [ -x /usr/5bin/su ] ; then
			/usr/5bin/su "$@"
		else
			\su "$@"
		fi
		PWD=$(pwd)
		clearban
	}

	_cmd=$(type nethack 2>/dev/null)
	if [ "${_cmd##*/}" = "nethack" ] ; then
		function nethack
		{
			if [ "$TERM" = "dmd" -o "$TERM" = "dmd-myx" ] ; then
				trap "trap 1 2 3 15; loadfont thin.9x14; setban" 1 2 3 15
				loadfont rogue.9x18
			else
				trap "trap 1; clearban; kill -1 $$" 1
				trap "trap 2; clearban; kill -2 $$" 2
				trap "trap 3; clearban; kill -3 $$" 3
				trap "trap 15; clearban; kill -15 $$" 15
			fi
			OWBANNER=${WBANNER}
			WBANNER="NetHack"
			setban
			mesg n
			\nethack
			clearban
		}
	fi
fi

if [ "$uid" = usenet -o "$uid" = news ] ; then
	dirprepend PATH $LOCAL/lib/newsbin $LOCAL/lib/newsbin/maint $LOCAL/lib/newsbin/input
fi

if $ISSUN; then
	alias df="/usr/bin/df"
fi

if [ $(uname -s) = "Darwin" ]; then
        # N.B.  The OS product version can be found with /usr/bin/sw_vers
        
	# it's not the same, but it has similar uses...
	alias ldd="otool"

	# macOS 'df' has a nasty bug in parsing its options and cannot
        # see any past the first '-t' (which is apparently
        # deprecated), so be sure to use '-T'
	#
	alias df="/bin/df -P -T nonullfs"
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

# delete all CONSECUTIVE blank lines from file except the first; also
# deletes all blank lines from top and end of file (emulates "cat -s")
# method 1, allows 0 blanks at top, 1 at EOF
# NOTE: never forget this -- it's the most incredible little sed script!!!!
alias blsqueeze='sed "/./,/^$/!d"'
# method 2, allows 1 blank at top, 0 at EOF
alias blsqueezebot="sed '/^$/N;/\n$/D'"
# delete all CONSECUTIVE blank lines from file except the first 2:
alias blsqueezenot2="sed '/^$/N;/\n$/N;//D'"
# delete ALL blank lines from a file (same as "grep '.' ")
alias blstrip='sed "/./!d"'
alias blstrip2="sed '/^$/d'"

# delete all leading blank lines at top of file
alias blstriptop="sed '/./,$!d'"
# delete all trailing blank lines at end of file
alias blstripbot="sed -e :a -e '/^\n*$/N;/\n$/ba'"

# reverse order of lines (emulates "tac")
alias sed-tac="sed '1!G;h;$!d'"
# reverse each character on the line (emulates "rev")
alias sed-rev="sed '/\n/!G;s/\(.\)\(.*\n\)/&\2\1/;//D;s/.//'"
# join pairs of lines side-by-side (like "paste")
alias sed-paste="sed 'N;s/\n/ /'"

# delete duplicate lines from a sorted file (emulates "uniq"). First
# line in a set of duplicate lines is kept, the rest are deleted
alias sed-uniq="sed '$!N; /^\(.*\)\n\1$/!P; D'"

# NOTE: replacing the last '-print' with '-exec CMD {} +' lets one use 'CMD' directly on the files
alias pkgfind="find . -type d -name CVS -prune -or -type f \( -name 'Make*' -or -name '*.mk' \) ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name .cvsignore -print"
alias cvsfind="find . -type d -name CVS -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name .cvsignore ! -name '[Tt][Aa][Gg][Ss]' -print"
alias cvsfind0="find . -type d -name CVS -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name .cvsignore ! -name '[Tt][Aa][Gg][Ss]' -print0"
alias srcfind="find . -type d \( -name CVS -or -name .git -or -name .svn -or -name build -or -name 'build-*' -or -name autom4te.cache \) -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name '.*ignore' ! -name '[Tt][Aa][Gg][Ss]' -print"
alias srcfind0="find . -type d \( -name CVS -or -name .git -or -name .svn -or -name build -or -name 'build-*' -or -name autom4te.cache \) -prune -or -type f ! -name '.#*' ! -name '#*#' ! -name '*~' ! -name '.*ignore' ! -name '[Tt][Aa][Gg][Ss]' -print0"
alias deadlinks='find . -type l -a ! \( -follow -type f \) -print'
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
alias j='jobs -l'
alias l='/bin/ls -CF'
alias la='/bin/ls -CFa'
alias lD='/bin/ls -CFd'
alias lL='/bin/ls -CFL'
alias ll='/bin/ls -ls'
alias lli='/bin/ls -lis'
alias llD='/bin/ls -lsd'
alias llL='/bin/ls -lsL'
alias lla='/bin/ls -lsa'
alias llia='/bin/ls -lisa'
alias llai='/bin/ls -lisa'
alias lld='/bin/ls -lsd'
alias llr='/bin/ls -lsR'
alias llir='/bin/ls -lisR'
alias llra='/bin/ls -lsaR'
alias llira='/bin/ls -lisaR'
alias lr='/bin/ls -CFR'
alias lra='/bin/ls -CFRa'
alias lsa='/bin/ls -as'
alias lss='/bin/ls -s'
alias local='typeset'		# always in pdksh, but not ast-ksh, used by git
alias logout='exit 0'
alias maildate='LANG=c date "+%a, %d %b %Y %T %z"'
alias nosgr='echo '
alias nstty='stty sane intr "^?" erase "^h" kill "^u" echoe echok'
alias pkg_sizes="/usr/sbin/pkg_info -s \* | sed -e '/^$/d' -e 's/Information for //' -e 's/:$/:\\\\/' | sed -e :a -e '$!N;s/Size of this package in bytes://;ta' -e 'P;D' | backslashjoin"
alias realias='let LEV=$LEV-1;exec ${SHELL}'		# useless?
alias rehash='_SV_PATH=$PATH; PATH=$_SV_PATH; unset _SV_PATH'
alias rinfo='rlog -L -h -l $(find RCS -type f -print)'
alias rstty='stty $SANE'
alias rsyncbackup='rsync -a -H -E --numeric-ids'
alias scvs='export CVSROOT="$(< CVS/Root)"; print "CVSROOT=$CVSROOT"'
alias snmpoidinfo='snmptranslate -T d -O f'
alias wcvs="printf '%s' \$CVSROOT"
alias zds="z$PAGER"

alias xload-1="xload -geometry 120x40-200+48 -hl red &"
alias xload-2="xload -geometry 120x40-200+96 -hl red &"
alias xload-3="xload -geometry 120x40-200+144 -hl red &"
alias xload-4="xload -geometry 120x40-200+192 -hl red &"
alias xload-5="xload -geometry 120x40-200+240 -hl red &"

alias dlog='$PAGER -en +G /var/log/debug'
alias ilog='$PAGER -en +G /var/log/important'
alias klog='$PAGER -en +G /var/log/kern'
alias mlog='$PAGER -en +G /var/log/messages'

# Smail related tools...
#
alias badsenders='fgrep RHSBL: $MAILLOG | sed "s/[<>]/ /g" | awk "{print \$8}" | sort -u'
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
elif [ -d /usr/spool/uucp ]; then
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
if [ -n "$KSH_VERSION" ] ; then
	#
	# this is probably pdksh
	#
	set -o braceexpand
fi

if [ -r $HOME/.kshdir ] ; then
	alias pushd='unalias pushd popd showd sd;. $HOME/.kshdir; pushd'
	alias popd='unalias pushd popd showd sd;. $HOME/.kshdir; popd'
	alias showd='unalias pushd popd showd sd;. $HOME/.kshdir; showd'
fi

#unset -f do_first_time

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
case $SHELL in
*bash*)
	if [ -r $HOME/.bashedit ] ; then
		. $HOME/.bashedit
	else
		set -o emacs
	fi
	;;
*)
	if [ -r $HOME/.kshedit ] ; then
		. $HOME/.kshedit
	else
		set -o gmacs || set -o emacs
	fi
	alias __A=''		# up arrow
	alias __B=''		# down arrow
	alias __C=''		# right arrow
	alias __D=''		# left arrow
	alias __H=''		# beginning of line, HOME key
	;;
esac

# Do this at very nearly the very end...
#
if [ -r $HOME/.kshlocal ] ; then
	. $HOME/.kshlocal
fi

# N.B.:  Do this only at the very very end!
#
set -h
