#
#	.bashrc - per-shell startup stuff for bash via $ENV
#
#ident	"@(#)HOME:.bashrc	37.4	24/10/20 14:44:48 (woods)"

# Assumptions:

# Files referenced:
#
#	$HOME/.kshrc		- sourced for common shell functions
#	$HOME/.bashlocal	- sourced, if it is readable

#echo "$0: in ~/.bashrc ...."

# Apple is stupid
#
export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -z "$SHELL" ] ; then
	export SHELL=$BASH
fi

# XXX not quite right, but near enough?  Probably not, but better than nothing!
#
# Note that some/many GNU/Linux systems have a "run-mailcap" package (from
# Debian?) which includes a /usr/bin/print command (a link to the
# /usr/bin/run-mailcap command).  So far *BSD packages which might install this
# are usually called "misc/mime-support", and they either don't install the link
# aliases, or they sanely call them something else, like "mime-print".
#
# Note the AT&T ksh(1) manual page says:  "the behavior of echo is system
# dependent and print or printf described below should be used."
#
print ()
{
	echo ${1+"${@}"}
}

alias whence="command -v"

# for most intents and purposes we can pretend bash is ksh-like....
#
. $HOME/.kshrc

# ~/.shrc, sourced by ~/.kshrc, also does the tests for interactive/login shells
# and will also have returned "early" if neither (except if $FROM_DOT_PROFILE).
#
if ${sh_is_interactive} || ${sh_is_login}; then
	: OK
else
	return
fi

# ancient-Bash-specific?
#
lastcmd ()
{
	tr '[\001-\007]' '[\012*]' < ${HISTFILE} | \
		tr '[\176-\377]' '[ *]' | \
		egrep -v '^[	 }#]|^$' | \
		tail ${1+"$@"}
}

# XXX this should not be necessary?
set -o monitor

# xxx this is different than .localprofile in that it is only for
# interactive shells...
#
if [ -r $HOME/.bashlocal ] ; then
	. $HOME/.bashlocal
fi
