#
#	.bashrc - per-shell startup stuff for bash via $ENV
#
#ident	"@(#)HOME:.bashrc	37.2	22/09/06 12:17:52 (woods)"

# Assumptions:

# Files referenced:
#
#	$HOME/.kshrc		- sourced for common shell functions
#	$HOME/.bashlocal	- sourced, if it is readable

# Apple is stupid
#
export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -z "$SHELL" ] ; then
	export SHELL=$BASH
fi

# XXX this should not be necessary?
set -o monitor

# XXX not quite right, but near enough?
#
alias print=echo

alias whence="command -v"

. $HOME/.kshrc

# ancient-Bash-specific?
#
lastcmd ()
{
	tr '[\001-\007]' '[\012*]' < ${HISTFILE} | \
		tr '[\176-\377]' '[ *]' | \
		egrep -v '^[	 }#]|^$' | \
		tail ${1+"$@"}
}

# xxx this is different than .localprofile in that it is only for
# interactive shells...
#
if [ -r $HOME/.bashlocal ] ; then
	. $HOME/.bashlocal
fi
