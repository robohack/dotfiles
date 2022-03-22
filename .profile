#
#	.profile - Bourne Sh, most Ash, almost any ksh-like, some GNU Bash, etc.
#
# N.B.:  GNU Bash is a big ugly slow incompatible behemoth.  Do not use it if at
# all possible!  Recent releases of AT&T Ksh are faster, but also big and ugly.
#
# My preference for years has been PDKsh, now as Ksh in NetBSD.
#
#ident	"@(#)HOME:.profile	37.12	22/03/22 16:13:35 (woods)"

# Assumptions that may cause breakage:
#
#	- the shell is more or less POSIX compatible
#		- but we don't use $() for command expansion
#		- we don't use arithmetic expressions either
#		- we don't use parameter expansion substring processing
#		- we don't use ~ for pathname expansion (use $HOME)
#		- we don't use "export VAR=value"
#	- the shell supports functions (but not necessarily "typeset")
#	- the shell supports "getopts" (used by some functions)
#	- standard environment has been set by login(1)
#	- $argv0 (if set) is `basename $0` from .xinitrc or .xsession
#	- test(1), aka "[", supports '-L' for testing symlinks
#	  (note that "test -L" is POSIX, but old systems had "-h")
#
# For now this all works well enough, but minimally, with Heirloom Shell; most
# modern versions of Ash, including NetBSD sh and it's bass-ackwards stunted
# clone dash; and has been tested with two of the most recent variants of Schily
# Bourne Shell (bosh and pbosh).  It hasn't been tested with old SysV shells,
# old Ash, etc., in a very long time.  DMD/mpx/layers support has not been
# tested in a similarly long time.

# Files referenced [all optional, some specific to the type of shell]:
#
#	$HOME/.ashtype	- sourced once, if 'type' command fails
#	$HOME/.ashlogin	- sourced once, if running ash(1) or similar (expands ~)
#	$HOME/.ashrc	- set as $ENV if running ash(1) or similar
#	$HOME/.ashlocal	- source from .ashrc, if readable
#	$HOME/.bashrc	- set as $ENVFILE in ~/.bashlogin
#	$HOME/.bashlogin - sourced once, if running GNU bash(1)
#	$HOME/.bashlogout - sourced  on trap 0, if running GNU bash(1)
#	$HOME/.editor	- mktable'd & assigned to $EDPREF, preferred editor type
#	$HOME/.kshrc	- set as $ENVFILE in ~/.kshlogin
#	$HOME/.kshlocal	- source from .kshrc, if readable
#	$HOME/.kshlogin	- sourced once, if running any ksh(1)
#	$HOME/.kshlogout - sourced on trap 0, if running any ksh(1)
#	$HOME/.localprofile - sourced once early to set system-local prefs.
#	$HOME/.mailer	- name of prefered MUA command
#	$HOME/.shell	- mktable'd and exec'ed as shell (see end of this file)
#	$HOME/.shlogin	- sourced once, if running sh(1)
#	$HOME/.shlogout	- set on trap 0, if running sh(1) or ash(1)
#	$HOME/.shrc	- sourced very near the beginning of ~/.profile, and by $ENV
#	$HOME/.stty	- sourced for stty command(s), etc. just before tset(1)
#	$HOME/.trninit	- pathname set as value for $TRNINIT

# Notes:
#
#	.ashlocal, .editor, .kshlocal, .localprofile, .stty, and .shell are not
#	distributed.
#
#	.localprofile may set $PATH_IS_OKAY to "true" if it is so (and
#	of course may also (re)set $PATH to make it so first.
#
#	The purpose of ~/.${SHELL}login is to set up $ENV, or at least
#	arrange for it to be set (e.g. as in ~/.kshlogin), and to set
#	the prompt variables ($PS1, etc.) appropriately unless $ENV
#	will do that when running interactively (but remember $PS1 et
#	al must not be set for non-interactive shell processes!).
#
#	$ENV really shouldn't ever do anything for non-interactive
#	shells, as anything it does can make nightmares for portable
#	shell scripts (both the writing of, and the running of them).
#
#	Login shells can be tested with:
#
#	    cd $HOME && env -i SHELL=/path/to/sh HOME=$HOME DISPLAY=$DISPLAY PATH=$PATH xterm $XTERM_OPTS -ls &
#
#	Note the "cd $HOME" in the above test command.  Most shell manuals say
#	something like "read the .profile file in the user's home directory
#	($HOME)", or even more explicitly "read from $HOME/.profile", but in
#	fact some expect to be started with their current working directory
#	already in $HOME, e.g. NetBSD sh (and maybe older shells, but not dash).

# ToDo:
#
#	Don't force HAVE* variables if they are already set!

umask 022

echo "$0: startup PATH=$PATH"
OPATH=$PATH

if [ -n "${DISPLAY}" ]; then
	echo "$0: startup DISPLAY=$DISPLAY"
	ODISPLAY=${DISPLAY}
fi

#
# N.B.:  These next is_* functions are copied early in ~/.shrc too!
#

is_bash ()
{
	[ ${RANDOM:-0} -ne ${RANDOM:-0} -a -n "${BASH}" ]
}

is_ksh ()
{
	if [ ${RANDOM:-0} -ne ${RANDOM:-0} -a -z "${BASH}" ]; then
		case "${KSH_VERSION}" in
		"Version"*)
			return 0 # likely att ksh
			;;
		"@(#)MIRBSD"*)
			return 0 # mksh
			;;
		"@(#)"*)
			return 0 # likely pdksh or some other derivative
			;;
		*)
			return 1
			;;
		esac
	else
		return 1
	fi
}

is_attksh ()
{
	is_bash && return 1

	# Note the use of eval to avoid any non-Ksh, non-bosh shell from kicking
	# out an error about "bad substitution" and stopping execution as a
	# result.

	# ksh93 and [p]bosh
	_sh_version=`eval 'echo "${.sh.version}"' 2>/dev/null`
	# [p]bosh only
	_sh_shell=`eval 'echo "${.sh.shell}"' 2>/dev/null`

	test -n "${_sh_version}" -a -z "${_sh_shell}"
	_rc=$?
	unset _sh_shell _sh_version

	return ${_rc}
}

is_ash ()
{
	is_ksh && return 1
	is_bash && return 1
	# older Ash didn't have $RANDOM?
	[ "`echo ~`" = "${HOME}" ]
}

is_bourne_sh ()
{
	is_ash && return 1
	[ "`echo ~`" != "${HOME}" -a -z "${RANDOM}" ]
}

is_posixish_sh ()
{
	[ "`echo ~`" = "${HOME}" -a ${RANDOM:-0} -eq ${RANDOM:-0} ]
}

is_schily_sh ()
{
	_sh_shell=`eval 'echo "${.sh.shell}"' 2>/dev/null`

	test -n "${_sh_shell}"
	_rc=$?
	unset _sh_shell

	return ${_rc}
}

ISATTY=false
if tty >/dev/null; then
	ISATTY=true
fi

if ${ISATTY}; then
	if is_bash && [ -r ${HOME}/.bashlogout ]; then
		trap '. ${HOME}/.bashlogout ; exit $?' EXIT
	elif is_ksh && [ -r ${HOME}/.kshlogout ]; then
		trap '. ${HOME}/.kshlogout ; exit $?' EXIT
	elif [ -r ${HOME}/.shlogout ] ; then
		trap '. ${HOME}/.shlogout ; exit $?' 0
	fi
fi

# every shell gets all the basic functions and variable settings....
#
# we assume ~/.profile is sourced only by login shells, and by
# ~/.xinitrc or by a window manager
#
. ${HOME}/.shrc

# Note:  early ash reads SHINIT at start (except if a login shell or called with
# "sh file")

# the I/O re-direction doesn't actually get rid of the "type: not found" message
# from the old Ash implementation...  Perhaps it parses the whole line first?
#
# XXX but maybe it would if we redirected stderr within the subshell
# too, as we must do for GNU Bash?
#
if ( type type > /dev/null 2>&1 ) > /dev/null 2>&1 ; then
	:
elif [ -r ${HOME}/.ashtype ]; then
	. ${HOME}/.ashtype
	ENV=${HOME}/.ashrc
	export ENV
fi

if [ "`echo ~`" = "${HOME}" -a ${RANDOM:-0} -eq ${RANDOM:-0} ] ; then
	#
	# apparently a POSIX capable shell
	#
	: OK POSIX is good -- that is all for now...
	if [ -n "${KSH_VERSION}" ]; then
		: ksh93t or newer, or PDKSH.
	## xxx it seems impossible to use the following without tripping up
	## pdksh (which complains "ksh: : bad substitution")....  but...
	##elif [ -n "${.sh.version}" ]; then
	##	: ksh93 or newer
	fi
fi

if expr "`type ulimit 2>/dev/null`" : '^ulimit is a shell ' > /dev/null 2>&1 ; then
	#
	# force core, data, nofile, stack, and nproc limits to be equal to
	# their maximum hard limit.
	#
	# (assume RLIMIT_CPU (time) and RLIMIT_FSIZE (file) are already either
	# unlimited or as big as they can get)
	#
	# XXX sadly POSIX 1003.1 2004 only specifies '-f' (file size
	# limit in 512-byte blocks, aka RLIMIT_FSIZE)
	#
	RLIMIT_CORE=`ulimit -H -c`
	if [ "${RLIMIT_CORE}" != "`ulimit -S -c`" ]; then
		ulimit -S -c ${RLIMIT_CORE}	# coredump
	fi
	RLIMIT_DATA=`ulimit -H -d`
	if [ "${RLIMIT_DATA}" != "`ulimit -S -d`" ]; then
		# on NetBSD this will still give EINVAL....
		ulimit -S -d ${RLIMIT_DATA}	# data
	fi
	RLIMIT_STACK=`ulimit -H -s`
	if [ "${RLIMIT_STACK}" != "`ulimit -S -s`" ]; then
		ulimit -S -s ${RLIMIT_STACK}	# stack
	fi
	#
	# XXX AT&T KSH and GNU Bash see '-p' as pipesize (internal,
	# read-only), and use '-u' for number of processes
	# (RLIMIT_NPROC).
	#
	if is_attksh || is_bash || is_bourne_sh || is_schily_sh; then
		RLIMIT_PROC=`ulimit -H -u`
		if [ "${RLIMIT_PROC}" != "`ulimit -S -u`" ]; then
			ulimit -S -u ${RLIMIT_PROC}
		fi
	else
		RLIMIT_PROC=`ulimit -H -p`
		if [ "${RLIMIT_PROC}" != "`ulimit -S -p`" ]; then
			ulimit -S -p ${RLIMIT_PROC}
		fi
	fi
	RLIMIT_NOFILE=`ulimit -H -n`
	if [ "${RLIMIT_NOFILE}" != "`ulimit -S -n`" ]; then
		ulimit -S -n ${RLIMIT_NOFILE}	# nofile
		if [ $? -ne 0 ]; then
			new_nofile=`expr ${RLIMIT_PROC} \* 4`
			echo "Unable to increase RLIMIT_NOFILE to hard limit of '$RLIMIT_NOFILE', trying $new_nofile..."
			ulimit -S -n ${new_nofile}
		fi
	fi
else
	# force nproc, data, stack, and nofiles limits to be equal to
	# their maximum hard limit.
	#
	# (assume time, filesize, and coredump are already unlimited)
	#
	limit maxproc `limit -h maxproc | awk '{print $2}'`
	limit datasize `limit -h datasize | awk '{print $2 * 1024}'`
	limit stacksize `limit -h stacksize | awk '{print $2 * 1024}'`
	limit openfiles `limit -h openfiles | awk '{print $2}'`
fi

if [ -z "${LOGNAME}" ] ; then
	LOGNAME=${USER}
	export LOGNAME
fi

if [ -z "${UUNAME}" ] ; then
	if type uuname >/dev/null 2>&1; then
		UUNAME=`uuname -l`
	else
		UUNAME=`hostname`
	fi
	export UUNAME
fi

if [ -z "${HOSTNAME}" ] ; then
	if type hostname >/dev/null 2>&1; then
		HOSTNAME=`hostname`
	else
		HOSTNAME=${UUNAME}
	fi
	export HOSTNAME
fi

case "${HOSTNAME}" in
*.local)
	DOMAINNAME=".local"
	export DOMAINNAME
	;;
esac

if [ -z "${DOMAINNAME}" ] ; then
	DOMAINNAME=`get_domainname`
	export DOMAINNAME
fi

# system-local user preferences go in here
#
if [ -r ${HOME}/.localprofile ] ; then
	. ${HOME}/.localprofile
fi

if "${PATH_IS_OKAY:-false}" ; then
	: # we trust $PATH has been initialized correctly on these machines....
	echo "$0: keeping PATH"
else
	# otherwise start fresh...
	OPATH=${PATH}

	# XXX on FreeBSD systems using their half-assed virtual
	# environment system based on their jail(2) system call, /bin
	# (and /usr/bin) will potentially be a symlink pointing to
	# some shared storage, eg. /basejail/bin (and
	# /basejail/usr/bin).  However since it's difficult to
	# portably detect the target of a symlink (there is a
	# "readlink" utility on FreeBSD and newer NetBSDs, a variant
	# of the older NetBSD stat(1) command), we'll just depend on
	# use of $PATH_IS_OKAY for now to work around the problem.
	#
	# originally this code was to avoid having /bin in $PATH if
	# /bin were pointing to /usr/bin (i.e. making it redundant)

###	if [ -L /bin -a `ls -l /bin | awk '{print $(NF)}'` = "/usr/bin" ] ; then
	if [ -L /bin ] ; then
		PATH="/usr/bin"
###	if [ -L /usr/bin -a `ls -l /usr/bin | awk '{print $(NF)}'` = "/bin" -o ! -d "/usr/bin" ] ; then
	elif [ -L /usr/bin -o ! -d /usr/bin ] ; then
		PATH="/bin"
	else
		PATH="/bin:/usr/bin"
	fi
	export PATH
	dirappend PATH /usr/lbin
fi
export PATH

set_LOCAL_et_al

# TODO: explore more options for this....  (xmkmf?)
#
# TODO: what if there's more than one variant installed?
#
# don't worry about openwin -- it's handled in the ISSUN case below
#
if [ -z "${X11PATH}" ] ; then
	# for X11R? try to get the newest (highest numeric value) one first...
	x11paths=`eval echo /opt/X11 /local/X11R? /local/X11 /usr/X11R? /usr/X11 /usr/X??? /usr/local/X11R? /usr/local/X11`
	for x11pc in `reverse_word_order ${x11paths}`; do
		if [ -d ${x11pc} -a ! -L ${x11pc} -a -d ${x11pc}/bin -a -x ${x11pc}/bin/xterm ] ; then
			X11PATH=${x11pc}
			break;
		fi
	done
	if [ -z "${X11PATH}" ] ; then
		X11PATH="/NO-X11-FOUND"
	fi
	export X11PATH
	unset x11paths x11pc
fi
if [ -z "${X11BIN}" ] ; then
	if [ -d /usr/bin/X11 -a ! -L /usr/bin/X11 ] ; then
		# this is never(?) used any more....
		X11BIN="/usr/bin/X11"
	else
		# XXX: this is a best guess -- should check!
		X11BIN=${X11PATH}/bin
	fi
	export X11BIN
fi
# deal with startx madness of $oldbindir
if [ -L /usr/X11 -a $X11BIN != /usr/X11/bin ]; then
	dirremove PATH /usr/X11/bin
fi
if [ -L /usr/X11R6 -a $X11BIN != /usr/X11R6/bin ]; then
	dirremove PATH /usr/X11R6/bin
fi
if [ -L /usr/X11R7 -a $X11BIN != /usr/X11R7/bin ]; then
	dirremove PATH /usr/X11R7/bin
fi

# TODO: some of these we may want fixed in even if they don't exist at
# login time...
#
# On Solaris after 5.10 the most POSIX-ish behaviour is given by:
#
#	PATH=/usr/xpg6/bin:/usr/xpg4/bin:/usr/ccs/bin:/usr/bin
#
dirprepend PATH /usr/xpg6/bin /usr/xpg4/bin /usr/ccs/bin
dirappend PATH ${X11BIN} ${LOCAL}/bin ${CONTRIB}/bin
dirappend PATH ${PKG}/bin ${PKG}/DWB/bin
dirappend PATH ${PKG}/heirloom-xpg4/bin ${PKG}/heirloom-ccs/bin ${PKG}/heirloom-doctools/bin ${PKG}/heirloom/bin
dirappend PATH ${SLASHOPT}/bin
dirappend PATH ${GNU}/bin ${SLASHOPT}/gnu/bin

# silly fuzting for older OSX...
dirappend PATH /Developer/usr/bin
if [ -d /Developer/usr/bin ]; then
	dirprepend PATH ${PKG}/nbase/bin
fi
# always, for bootstrapped pkgsrc???
dirappend PATH ${PKG}/nbase/bin

dirappend PATH /usr/ucb /usr/bsd
dirappend PATH ${HI_TECH_C}/bin
dirappend PATH /usr/games ${LOCAL}/games ${SLASHOPT}/games/bin

if [ -n "${MANPATH}" ]; then
	OMANPATH=${MANPATH} ; export OMANPATH
fi

# don't set initial MANPATH with 4.4BSD man....
#
if [ -z "${MANPATH}" -a ! -r /etc/man.conf ] ; then
	if [ -d /usr/share/man ] ; then
		MANPATH="/usr/share/man"
	else
		MANPATH="/usr/man"
	fi
	export MANPATH
fi
dirprepend MANPATH ${LOCAL}/share/man ${LOCAL}/man ${GNU}/man ${CONTRIB}/share/man ${CONTRIB}/man ${PKG}/share/man ${PKG}/gnu/share/man ${PKG}/man ${X11PATH}/man

# more silly fuzting for OSX...
dirappend MANPATH /Developer/usr/share/man /Library/Developer/CommandLineTools/usr/share/man ${SDKPREFIX}/usr/share/man
if [ -d /Developer/usr/bin ]; then
	dirprepend MANPATH ${PKG}/nbase/share/man
fi
# always, for bootstrapped (or just Joyent?) pkgsrc???
dirappend MANPATH ${PKG}/nbase/share/man

if [ -z "${INFOPATH}" ] ; then
	if [ -d /usr/share/info ] ; then
		INFOPATH="/usr/share/info"
	else
		INFOPATH="/usr/man"
	fi
	export INFOPATH
fi
dirprepend INFOPATH ${LOCAL}/share/info ${LOCAL}/info ${GNU}/info ${CONTRIB}/share/info ${CONTRIB}/info ${PKG}/share/info ${PKG}/gnu/share/info ${PKG}/info ${X11PATH}/info

if $ISSUN; then
	PATH=`echo ${PATH} | sed 's/^\/bin://'`
	if [ "`uname -r | sed 's/^\([0-9]*\).*$/\1/'`" -lt 5 ] ; then
		if [ "X${LOGNAME}" != "Xroot" ] ; then
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

if [ -d ${LOCAL}/dmdlayers/bin -a "X${TERM}" = "Xdmd" ] ; then
	DMD=${LOCAL}/dmdlayers ; export DMD
	TOOLS=${DMD}/local ; export TOOLS
	dirappend PATH ${DMD}/bin ${TOOLS}/bin
	dirprepend MANPATH ${DMD}/man ${TOOLS}/man
fi

# make sure our home-dir is set up properly...
#
if [ ! -d ${HOME}/tmp ] ; then
	mkdir ${HOME}/tmp
	chmod 700 ${HOME}/tmp
fi
if [ ! -d ${HOME}/Mail ] ; then
	mkdir ${HOME}/Mail
	chmod 700 ${HOME}/Mail
fi
if [ -f ${HOME}/.xinitrc ] ; then
	if [ ! -x ${HOME}/.xinitrc ] ; then
		echo "WARNING: fixing execute bit on ~/.xinitrc!"
		chmod +x ${HOME}/.xinitrc
	fi
	if [ ! -r ${HOME}/.xsession ] ; then
		rm -f ${HOME}/.xsession
		ln -s .xinitrc ${HOME}/.xsession
	fi
fi
# note .emacs.elc may not yet exist
if [ ! -r ${HOME}/.emacs -a -r ${HOME}/.emacs.el ] ; then
	rm -f ${HOME}/.emacs
	ln -s .emacs.elc ${HOME}/.emacs
fi

if [ "X${HOME}" != "X/" ] ; then
	if [ ! -d ${HOME}/bin ] ; then
		mkdir ${HOME}/bin
		chmod 755 ${HOME}/bin
	fi
	if [ -d  ${HOME}/usr/bin ]; then
		echo ""
		echo "$0: what's up with $HOME/usr/bin?"
		echo ""
		sleep 5
	fi
	dirprepend PATH ${HOME}/bin ${HOME}/go/bin ${HOME}/usr/bin ${HOME}/pkg/bin
	case "${PATH}" in
	*:)
		echo 'NOTICE: PATH already ends in a colon.' ;;
	*)
		PATH=${PATH}:
		;;
	esac
fi

#
# PATH should finally be set properly!  Just Mh and X11 set below
#

if type mktable >/dev/null 2>&1; then
	MKTABLE="mktable"
else
	# a little ditty to throw away comment lines....
	# TODO: could call mkline (ala smail-3) if available....
	# TODO: should this remove trailing comments too? (-e '/#.*$//')
	mktable ()
	{
		sed				\
			-e '/^[ 	]*#/d'	\
			-e '/^[ 	]*$/d'	\
		${1+"$@"}
	}
fi

# This is kind of crude and not likely to work all the time, but the
# idea is that since less(1) will use setlocale() if possible, we will
# try to be smart about when we can tell it about which charset it
# should try to use.
#
# note:  LC_ALL, if valid, overrides all LC_* variables, LANG is the default for
# unset LC_* variables, and LESSCHARSET is the equivalent of LC_CTYPE (just for
# "less", of course)
#
# Note also that "uxterm" will (have) set LC_CTYPE=en_US.UTF-8 if none of
# LC_ALL, LC_CTYPE, or LANG were set on invocation.
#
if [ -z "${LC_CTYPE}" -a -z "${LC_ALL}" -a -z "${LANG}" ]; then
	case "${TERM}" in
	wsvt25*)
		# this lets those pesky high-bit chars show through...
		#
		# NOTE:  with older versions of less 'latin1' is the only way to
		# express "ISO 8859-1", while with newer versions 'latin1' is
		# merely an alias for "iso8859", so is ISO-8859-*, i.e. whatever
		# extended set your terminal displays.
		#
		# more modern versions of less, on systems with setlocale(3)
		# will use LC_CTYPE (or LANG) if set.
		#
		LESSCHARSET="latin1"; export LESSCHARSET
		#
		# We'll assume for now that the console is in vt100 emulation
		# mode and loaded with an "iso" (i.e. ISO-8859-1) font.
		;;
	xterm*)
		# Xterm will normally set the environment up for itself, i.e. if
		# it was started as "uxterm" then we're NOT in this 'if' block!
		#
		# For less, from the man page:
		#	If neither LESSCHARSET nor LESSCHARDEF is set, but any of the strings
		#	"UTF-8", "UTF8", "utf-8" or "utf8" is found in the LC_ALL, LC_CTYPE or
		#	LANG environment variables, then the default character set is utf-8.
		#
		# So for plain (ascii) xterm, we probably also want to avoid
		# using LESSCHARSET too....
		#
		unset LESSCHARSET
		;;
	esac
else
	# Here we have LC_CTYPE (and/or LC_ALL and/or LANG)...
	#
	# xxx printf(3)'s "%'d" flag (i.e. the "'") is pedantic about having a
	# known locale set for at least LC_NUMERIC before it does commification!
	#
	# XXX XXX but this hard-coded choice may not be available everywhere,
	# never mind appropriate for anyone but me...  On NetBSD there's a
	# locale alias called "C.UTF-8", which is currently by default an alias
	# for "en_US.UTF-8". but maybe the alias is more universal?
	#
	LC_NUMERIC="en_CA.UTF-8"
	export LC_NUMERIC
	LC_MONETARY="en_CA.UTF-8" # this is the only one different from en_US
	export LC_MONETARY

	# XXX BTW, why is $(LANG= LC_ALL= LC_CTYPE= locale charmap)=="646" on
	# NetBSD but "locale -m" does not include "646" (only "US-ASCII"!)
	#
	# XXX Besides, shouldn't it properly be "ISO646"????
fi
# I am unlikely to ever want command to display times and dates in locale form!
LC_TIME="C"
export LC_TIME
# Similarly we don't need any surprises from sorting order changes!
LC_COLLATE="C"
export LC_COLLATE

# XXX can these ($RSH and $SSH) cause problems with other tools?
# (will be OK with at least cvs and rsync which use ${ARGV0}_RSH)
#
if type rsh >/dev/null 2>&1 ; then
	RSH="rsh"
elif [ -x /usr/ucb/rsh ] ; then	# maybe /usr/ucb not in $PATH?
	RSH="/usr/ucb/rsh"
else
	# assuming 'ssh' exists and is the only way to run remote shells...
	RSH="ssh"
fi
export RSH			# used by .twmrc, .ctwmrc, as well as .xinitrc

if type ssh >/dev/null 2>&1; then
	SSH="ssh"
elif type ssh2 >/dev/null 2>&1; then
	SSH="ssh2"
else
	# assuming 'ssh' really is available...
	SSH="ssh"
fi
export SSH			# used by .twmrc, .ctwmrc, as well as .xinitrc

HAVEMONTH=false
if type month >/dev/null 2>&1; then
	HAVEMONTH=true
fi

MONTH="AIKO" ; export MONTH

HAVELAYERS=false
if type layers >/dev/null 2>&1; then
	HAVELAYERS=true
fi

HAVEMUSH=false
MAILER=mail ; export MAILER
if [ -s ${HOME}/.mailer ] ; then
	# mktable just throws away comments....
	MAILER=`mktable ${HOME}/.mailer`
elif type mush >/dev/null 2>&1; then
	HAVEMUSH=true
	MAILER="mush"
elif type Mail >/dev/null 2>&1; then
	MAILER="Mail"
elif type mailx >/dev/null 2>&1; then
	MAILER="mailx"
fi
case "${MAILER}" in
mh )
	if [ -d ${CONTRIB}/mh ] ; then
		dirprepend PATH ${CONTRIB}/mh/bin
		dirprepend MANPATH ${CONTRIB}/mh/man
	elif [ -d ${PKG}/mh ] ; then
		dirprepend PATH ${CONTRIB}/mh/bin
		dirprepend MANPATH ${CONTRIB}/mh/man
	elif [ -d ${LOCAL}/mh ] ; then
		dirprepend PATH ${LOCAL}/mh/bin
		dirprepend MANPATH ${LOCAL}/mh/man
	elif [ -d /usr/mh ] ; then
		dirprepend PATH /usr/mh/bin
		dirprepend MANPATH /usr/mh/man
	elif [ -d ${LOCAL}/bin/mh ] ;then
		# this is a non-std setup -- ${LOCAL}/mh/man might not exist
		dirprepend PATH ${LOCAL}/bin/mh
		dirprepend MANPATH ${LOCAL}/mh/man
	fi
	;;
nmh )
	if [ -d ${CONTRIB}/nmh ] ; then
		dirprepend PATH ${CONTRIB}/nmh/bin
		dirprepend MANPATH ${CONTRIB}/nmh/man
	elif [ -d ${PKG}/nmh ] ; then
		dirprepend PATH ${CONTRIB}/nmh/bin
		dirprepend MANPATH ${CONTRIB}/nmh/man
	elif [ -d ${LOCAL}/nmh ] ; then
		dirprepend PATH ${LOCAL}/nmh/bin
		dirprepend MANPATH ${LOCAL}/nmh/man
	elif [ -d /usr/nmh ] ; then
		dirprepend PATH /usr/nmh/bin
		dirprepend MANPATH /usr/nmh/man
	elif [ -d ${LOCAL}/bin/nmh ] ;then
		# this is a non-std setup -- ${LOCAL}/nmh/man might not exist
		dirprepend PATH ${LOCAL}/bin/nmh
		dirprepend MANPATH ${LOCAL}/nmh/man
	fi
	;;
esac

if [ -z "${MAILDIR}" ] ; then
	if [ -d /var/mail ] ; then
		MAILDIR="/var/mail"
	elif [ -d /var/spool/mail ] ; then
		MAILDIR="/var/spool/mail"
	elif [ -d /usr/mail ] ; then
		MAILDIR="/usr/mail"
	elif [ -d /usr/spool/mail ] ; then
		MAILDIR="/usr/spool/mail"
	fi
fi
export MAILDIR

if [ -z "${UUCPSPOOLDIR}" ] ; then
	if [ -d /var/spool/uucp ] ; then
		UUCPSP0OLDIR=/var/spool/uucp
		export UUCPSPOOLDIR
	elif [ -d /usr/spool/uucp ]; then
		UUCPSPOOLDIR=/usr/spool/uucp
		export UUCPSPOOLDIR
	fi
fi

# use MAIL instead of MAILPATH, primarily to avoid the clash of using
# a POP specification in MAILPATH for emacs VM
#
unset MAILPATH
if [ -z "${MAIL}" ] ; then
	MAIL=${MAILDIR}/${LOGNAME}
fi
export MAIL

HAVECALENDAR=false
if type calendar >/dev/null 2>&1; then
	HAVECALENDAR=true
fi

HAVEFORTUNE=false
if type fortune >/dev/null 2>&1; then
	HAVEFORTUNE=true
	FORTUNE=fortune ; export FORTUNE
fi

if type less >/dev/null 2>&1; then
	PAGER="less"
	LESS="-M" ; export LESS
	if [ ! -f ${HOME}/.less ] ; then
		# xxx lesskey(1) seems to have gone missing on OSX 10.8 and 10.9
		# (maybe even on 10.7 too?)
		if type lesskey >/dev/null 2>&1; then
			if [ ! -f ${HOME}/.lesskey ] ; then
				echo "N	next-file" > ${HOME}/.lesskey
				echo "P	prev-file" >> ${HOME}/.lesskey
			fi
			lesskey
		fi
	fi
elif [ -x /usr/xpg4/bin/more ] ; then
	# SunOS-5's, at least, has the 'G' command!
	PAGER="/usr/xpg4/bin/more"
	# use '-s' as it can't be turned on later during runtime
	MORE="-s" ; export MORE
elif type more >/dev/null 2>&1; then
	PAGER="more"
	# use '-s' as it can't be turned on later during runtime
	MORE="-sw" ; export MORE
else
	PAGER="cat"
fi
export PAGER
if [ "${PAGER}" = "less" ] ; then
	MANPAGER="${PAGER} -Rsi"; export MANPAGER # n.b. more and less both suppport "-Rsi"
fi

if [ -s "${HOME}/.editor" ] ; then
	# mktable just throws away comments....
	EDPREF=`mktable ${HOME}/.editor` ; export EDPREF
fi

if [ -z "${MY_EMACS}" ]; then
	if type emacs >/dev/null 2>&1; then
		MY_EMACS=`type emacs`
		MY_EMACS=`expr "${EMACS}" : '^[^/]*\(/[^ )]*\)'`
		case `uname -s` in
		Darwin*)
			# native emacs is usually very old!
			if [ -x /opt/pkg/bin/emacs ]; then
				MY_EMACS=/opt/pkg/bin/emacs
			elif [ -x /usr/pkg/bin/emacs ]; then
				MY_EMACS=/usr/pkg/bin/emacs
			elif [ -x /usr/local/bin/emacs ]; then
				MY_EMACS=/usr/local/bin/emacs
			fi
			;;
		esac
		export MY_EMACS
		HAVEEMACS=true
	fi
else
	HAVEEMACS=true
fi

case "${EDPREF}" in
emacs | "" )
	HAVEEMACS=false
	HAVEJOVE=false
	if ${HAVEEMACS}; then
		EDITOR="${MY_EMACS}"
	elif type jove >/dev/null 2>&1; then
		EDITOR="jove"
		HAVEJOVE=true
	else
		EDITOR="ed"
	fi
	if ${HAVEEMACS} ; then
		VISUAL="${MY_EMACS}"
	elif ${HAVEJOVE} ; then
		VISUAL="jove"
	else
		VISUAL="vi"
	fi
	;;
vi )
	if type nvi >/dev/null 2>&1; then
		EDITOR="nvi"
	elif type vi >/dev/null 2>&1; then
		EDITOR="vi"
	else
		EDITOR="ed"
	fi
	if type nvi >/dev/null 2>&1; then
		VISUAL="nvi"
	elif type vi >/dev/null 2>&1; then
		VISUAL="vi"
	else
		VISUAL="no-visual-editor"
	fi
	;;
* )
	if type nvi >/dev/null 2>&1; then
		EDITOR="nvi"
	elif type vi >/dev/null 2>&1; then
		EDITOR="vi"
	else
		EDITOR="ed"
	fi
	if type ${EDPREF} >/dev/null 2>&1; then
		VISUAL=${EDPREF}
	else
		VISUAL=${EDITOR}
	fi
esac
export EDITOR
export VISUAL

EXINIT="set sm" ; export EXINIT

HAVEAUPLAY=false
if type auplay >/dev/null 2>&1; then
	HAVEAUPLAY=true
fi
HAVEAUDIOPLAY=false
if type audioplay >/dev/null 2>&1; then
	HAVEAUDIOPLAY=true
fi
HAVEESDPLAY=false
if type esdplay >/dev/null 2>&1; then
	HAVEESDPLAY=true
fi

if [ -z "${AUDIOPLAYER}" ] ; then
	if [ -n "${AUDIOSERVER}" ] ; then
		if ${HAVEAUPLAY} ; then
			AUDIOPLAYER="auplay -v 20"
		fi
	elif ${HAVEESDPLAY} ; then
		# XXX ESound is actually a dead-end....
		AUDIOPLAYER="esdplay"
		if [ -z "$ESPEAKER" ]; then
			# n.b.:  a hostname alias....
			ESPEAKER="audiosrvr"; export ESPEAKER
		fi
	elif [ -w /dev/audio ] ; then
		if ${HAVEAUDIOPLAY} ; then
			AUDIOPLAYER="audioplay"
		fi
	fi
fi
if [ -z "${AUDIOPLAYER}" ] ; then
	# avoid trying to run audio files...
	AUDIOPLAYER=":"
fi
export AUDIOPLAYER

RNINIT="-v -M -S -T -i=8 -g2" ; export RNINIT
TRNINIT=${HOME}/.trninit; export TRNINIT

# This is supposedly mostly for pine....
#
if [ -x ${PKG}/bin/ispell ]; then
	SPELL=${PKG}"/bin/ispell -l"
	export SPELL
fi

# set terminal type and tty settings, etc....
#
if ${ISATTY} && [ "X$argv0" != "X.xsession" -a "X$argv0" != "X.xinitrc" ] || [ "X${TERM}" = "Xdmd" -a "`ismpx`" != "yes" ] ; then
	echo "Re-setting terminal preferences...."

	# turn this off by default, turn it on by hand in one main window?
	mesg n

	if [ -r "${HOME}/.stty" ] ; then
		. ${HOME}/.stty
	else
		stty erase '^h' intr '^?' kill '^u' -ixany echo echoe echok
		# a separate command as it is a non-standard parameter
		stty status '^t' 2>/dev/null || echo "Sorry, probably no SIGNIFO support on this system."
	fi
	if [ "${EMACS}" = t -o "${TERM}" = emacs ]; then # there is also ${INSIDE_EMACS}
		echo "Turning off echo for an emacs shell...."
		stty -echo
	fi

	case "${UUNAME}" in
	robohack | weirdo | most | very | isit )
		# we trust that everything is all set up as it should be on
		# sites we know, except for personal preferences set above...
		:
		;;
	*)
		case "${TERM}" in
		""|network|dialup|unknown|none)
			# n.b.:  a function may not work to set an env var, see
			# the note above the definition of ~/.shrc:get_newterm()
			get_newterm
			;;
		*)
			# xxx hmmm.... this could just use $TERMTESTCMD (also
			# from ~/.shrc), but doing so would make showing the
			# warning notice impossible...
			#
			if ${HAVETPUT} ; then
				if tput longname > /dev/null; then
					:
				else
					echo "NOTICE:  the preset TERM=${TERM} is unknown...";
					TERM=unknown;
					get_newterm
				fi
			elif type tset >/dev/null 2>&1; then
				# n.b.:  this asks if TERM is unknown...
				eval `tset -s`
				# xxx if we want this, we get it later!
				unset TERMCAP
			else
				echo "NOTICE:  I don't know how to test your TERM (${TERM})."
			fi
			;;
		esac

		export TERM

		case "${TERM}" in
		vt220*)
			if [ ! -r ${HOME}/.stty ] ; then
				# real vt220 keyboards make this the best setup
				# n.b.:  on *BSD this could be "stty dec"
				stty intr '^C' erase '^?'
			fi
			;;
		esac

		case "${TTYN}" in
		tty[p-zP-Z]*|vt*|vg*|console)
			echo "Setting TTY modes for 8-bit transparency...."
			stty cs8 -istrip -parenb
			;;
		esac

		# now, one more time to really do the correct initialisation...
		#
		# unfortunately "tput reset" will almost always clear an
		# xterm, and "tput init" may do so, depending on the vintage of
		# terminfo definitions the host system has
		#
		case "${TERM}" in
		xterm*)
			;;
		*)
			if ${HAVETPUT} ; then
				tput init
			fi
			;;
		esac
		# Note: in other places we assume tset is avaliable....
		if type tset >/dev/null 2>&1; then
			# On BSD, without the "-I" it uses /etc/termcap....
			# (but maybe that is a good thing?)
			tset -r
			if ${HAVETPUT} ; then
				echo ${TERM}: `tput longname`
			fi
		elif ${HAVETPUT}; then
			:
		else
			echo "NOTICE:  I don't know how to set up your terminal."
		fi

		# "tput" and/or "tset" do not support setting or clearing tabs.
		#
		# (note "tabs" is required by POSIX (since at least SUSv2, i.e.
		# 1997), but it was not in NetBSD until 6.0 -- "tset" though is
		# not in POSIX at all.)
		#
		if type tabs >/dev/null 2>&1; then
			tabs -8
		fi

		case "${TERM}" in
		xterm*)
			if type resize >/dev/null 2>&1; then
				resize > /dev/null
			else
				# XXX it would be nice to detect if/when this
				# may actually be necessary!
				echo "NOTICE:  you may have to manually run 'stty rows \$LINES columns \$COLUMNS'."
			fi
			;;
		esac

		# try setting up for X11 if possible....
		# (XXX this may not be the right place for this)
		case "${TERM}" in
		xterm*|sun|pc3|ibmpc3)
			# users will have to set their own $DISPLAY....
			dirappend PATH ${X11PATH}/bin
			dirappend MANPATH ${X11PATH}/man
			#
			# In case this is onx11server running an xterm via rsh/ssh
			#
			# N.B.:  once upon a time -ziconbeep was not universally available
			#
			if [ -z "$XTERM_OPTS" ]; then
				XTERM_OPTS="-fbx -bc -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1"
			fi
			export XTERM_OPTS
			;;
		esac

		if [ -n "${SSH_TTY}" -o -n "${SSH_CLIENT}" -o -n "${SSH2_CLIENT}" ]; then
			echo "Secure connection from ${SSH_CLIENT:-${SSH2_CLIENT}} on ${SSH_TTY} (tty ${TTY})"
			if [ -n "${DISPLAY}" ] ; then
				echo "Secure X11 connections forwarded via ${DISPLAY}"
			fi
			echo ""
		else
			echo "Your terminal is port ${TTY}."
		fi

		;;
	esac
fi

# TODO: find some way to see if login(1) ran, or xterm(n) started us
# TODO: since login(1) checks for mail too, but xterm(n) doesn't.
# XXX or maybe we just always make xterm run "login -pf ${USER}" ???
#
# check your mail...
if type messages >/dev/null 2>&1; then
	messages
else
	[ -x /bin/mail ] && /bin/mail -e
	HAVENEWMAIL=$?
	if ${HAVEMUSH} && [ ${HAVENEWMAIL} -eq 0 ] ; then
		echo 'You have mail:'
		mush -H:n
	elif [ "${MAILER}" = mh -a ${HAVENEWMAIL} -eq 0 ] ; then
		echo "Change this line in ${HOME}/.profile to show new mail using MH"
	elif [ ${HAVENEWMAIL} -eq 0 ] ; then
		echo "You have some mail!"
	fi
	unset HAVENEWMAIL
fi


# once upon a time this needed to be a lot smarter....
#
if [ -d ${HOME}/lib/terminfo ] ; then
	case "${TERM}" in
	at386*|AT386*|386AT*|386at*|dmd|dmd-myx|ibmpc3|pc3)
		TERMINFO=${HOME}/lib/terminfo ; export TERMINFO
		;;
	esac
fi

if ${ISATTY} && [ "X$argv0" != "X.xsession" -a "X$argv0" != "X.xinitrc" ] ; then
	# WARNING: some stupid stty's cause this to fail!!!!
	# eg., ULTRIX V4.3 stty(1) 'cause it uses stdout, not stdin....
	SANE=`stty -g` ; export SANE
fi

# one thing we assume for ~/${SHELL}login is that it will set, or
# arrange to be set, $ENV; and that either $ENV or ~/${SHELL}login
# will set $PS1 (and any other prompt variables) as desired.
#
# If no ~/${SHELL}login is found, or or the special case of plain
# sh(1), then we will set $PS1 here, since anything sourcing
# ~/.profile must, by definition, be an interactive shell.
#
if is_ksh ; then
	if [ -r ${HOME}/.kshlogin ] ; then
		. ${HOME}/.kshlogin
	fi
elif is_bash ; then
	if [ -r ${HOME}/.bashlogin ] ; then
		. ${HOME}/.bashlogin
	fi
elif is_ash ; then
	# this will only be modern ash(1) or a derivative of it
	# (eg. from 4.4BSD or newer), or Schily Shell
	#
	if [ -r ${HOME}/.ashlogin ] ; then
		. ${HOME}/.ashlogin
	fi
elif [ -r ${HOME}/.shlogin ] ; then
	# plain old Bourne shell, e.g. Heirloom Sh
	#
	. ${HOME}/.shlogin
else
	if [ "X${LOGNAME}" = "Xroot" ] ; then
		PS1="login [${TTYN}]<${LOGNAME}@${UUNAME}> # "
	else
		PS1="login [${TTYN}]<${LOGNAME}@${UUNAME}> $ "
	fi
fi

HAVEX=false
if type xinit >/dev/null 2>&1; then
	HAVEX=true
fi

if ${ISATTY} && ${HAVEX} && [ "X$argv0" != "X.xinitrc" -a "X$argv0" != "X.xsession" ] ; then
	case "${TTYN}" in
	console|vg*|vt*|ttyc*|ttyE*)
		case "${TERM}" in
		sun|pc3|at386|AT386|vt220|vt100|wsvt25)
			trap '' 2
			echo ""
			echo $n "Do you want to start X? ([y]/n) $c"
			read yn
			trap 2
			case "$yn" in
				"" | [yY]*)
				trap '' 2
				xinit
				exec sleep 1
				;;
			*)
				echo "OK, not starting X..."
				;;
			esac
			;;
		esac
		;;
	esac
fi

if ${ISATTY} && ${HAVELAYERS} && [ "X${TERM}" = "Xdmd" -a "`ismpx`" != "yes" ] ; then
	trap '' 2
	echo ""
	$echo $n "Do you want to start layers? ([y]/n/debug) $c"
	read yn
	trap 2
	case "$yn" in
	"" | [yY]* | d*)
		if expr "$yn" : 'd.*' >/dev/null ; then
			layers=layers-DEBUG
		else
			layers=layers
		fi
		# TODO: maybe not?
		if [ "${VISUAL}" = "emacs" ] ; then
			VISUAL="emacsclient" ; export VISUAL
		fi
		LAYERSPID=$$ ; export LAYERSPID
		rc=.${TERM}rc
		# TODO: think about dmdmyx here....
		TERM="dmd"; export TERM
		stty -ixon -ixoff -ixany
		if [ -s ${HOME}/$rc ] ; then
			exec $layers -f $rc 2>> ${HOME}/tmp/layers.stderr
		else
			exec $layers 2>> ${HOME}/tmp/layers.stderr
		fi
		echo "Couldn't exec layers."
		stty ixon ixoff -ixany
		;;
	*)
		echo "OK, not starting layers..."
		;;
	esac
fi

#
# NOTE:  we don't get here the first time if we're starting a window system, so
# for first time in for window systems which emulate login shells in each window
#

if ${ISATTY}; then
	do_first_time
fi

if [ -r ${HOME}/.trninit${TERM} ] ; then
	TRNINIT=${HOME}/.trninit${TERM} ; export TRNINIT
fi

# There is a trick here -- if your shell is like ksh(1) or modern GNU Bash, and
# it has arrays, then you can use the ${ENVFILE[]} array expansion magic in
# ~/.kshlogin to have the shell only source the $ENV file for interactive
# shells.  Other shells that auto-source $ENV (e.g. ash, [p]bosh) will of course
# require it to be a plain filename, and they will also need the "case $- in
# *i*)" wrapper on the contents of $ENV and they will still suffer the cost of
# reading and parsing the whole file every time.
#
# Otherwise $ENV may have to be sourced directly (or manually), so if $ENV is
# set, and if it is readable, do that now.  In this case sub-shells will not
# have automatic sourcing of $ENV.  (and of course ~/.shrc was sourced above)
#
if [ -n "${ENV}" -a "${ENV}" != "${HOME}/.shrc" -a -r "${ENV}" ] ; then
	. ${ENV}
fi

# minor cleanup
#
if [ ${RANDOM:-0} -eq ${RANDOM:-0} ] ; then
	unset RANDOM
fi

# TODO: do something with msgs(1) if needed....

# NOTE: trick 4.4BSD shell into -E by putting it in here, 'cause you can't
# "set -o emacs" in .ashrc, as that'll cause some versions of it to dump core....
#
if [ -s ${HOME}/.shell -a "X${argv0}" != "X.xinitrc" -a "X${argv0}" != "X.xsession" ] ; then
	# mktable just throws away comments....
	exec `mktable ${HOME}/.shell`
fi

cd

# End Of File
