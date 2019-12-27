#
#	.profile - for either SysV sh, 4BSD sh, any ksh, some GNU bash, or even old ash.
#
#ident	"@(#)HOME:.profile	36.3	19/12/27 13:08:44 (woods)"

# Assumptions that may cause breakage:
#
#	- the shell is more or less POSIX compatible
#		- but we don't use $() for command expansion
#		- we don't use arithmetic expressions either
#		- we don't use parameter expansion substring processing
#	- the shell supports functions (but not necessarily "typeset")
#	- the shell supports "getopts"
#	- standard environment has been set by login(1)
#	- $argv0 is `basename $0` from .xinitrc or .xsession
#	- test(1), aka "[", supports '-L' for testing symlinks
#	  (note that "test -L" is POSIX, but old systems had "-h")

# Files referenced [all optional, some specific to the type of shell]:
#
#	$HOME/.ashtype	- sourced once, if 'type' command fails
#	$HOME/.ashlogin	- sourced once, if running ash(1) or similar (expands ~)
#	$HOME/.ashrc	- set as $ENV if running ash(1) or similar
#	$HOME/.bashrc	- set as $ENVFILE in ~/.bashlogin
#	$HOME/.bashlogin - sourced once, if running GNU bash(1)
#	$HOME/.bashlogout - sourced  on trap 0, if running GNU bash(1)
#	$HOME/.editor	- mktable'd & assigned to $EDPREF, preferred editor type
#	$HOME/.kshrc	- set as $ENVFILE in ~/.kshlogin
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
#	.editor, .localprofile, .stty, and .shell are not distributed.
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

umask 022

ISATTY=false
if tty >/dev/null; then
	ISATTY=true
fi

if ${ISATTY}; then
	if [ -r ${HOME}/.bashlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} -a -n "${BASH}" ] ; then
		trap '. ${HOME}/.bashlogout ; exit $?' 0
	elif [ -r ${HOME}/.kshlogout -a ${RANDOM:-0} -ne ${RANDOM:-0} -a -z "${BASH}" ] ; then
		trap '. ${HOME}/.kshlogout ; exit $?' 0
	elif [ -r ${HOME}/.shlogout ] ; then
		trap '. ${HOME}/.shlogout ; exit $?' 0
	fi
fi

# every shell gets all the basic functions....
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
	## xxx it seems impossible to use this without tripping up pdksh....  but... see SO?
	##elif [ -n "${.sh.version}" ]; then
	##	: ksh93 or newer
	fi
fi

if expr "`type ulimit 2>/dev/null`" : 'ulimit is a shell builtin$' > /dev/null 2>&1 ; then
	#
	# force core, data, nofile, stack, and nproc limits to be equal to
	# their maximum hard limit.
	#
	# (assume RLIMIT_CPU and RLIMIT_FSIZE are already either
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
	case "${KSH_VERSION}" in
	"@(#)"*)
		RLIMIT_PROC=`ulimit -H -p`
		if [ "${RLIMIT_PROC}" != "`ulimit -S -p`" ]; then
			ulimit -S -p ${RLIMIT_PROC}
		fi
		;;
	*)
		RLIMIT_PROC=`ulimit -H -u`
		if [ "${RLIMIT_PROC}" != "`ulimit -S -u`" ]; then
			ulimit -S -u ${RLIMIT_PROC}
		fi
		;;
	esac
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
	if expr "`type uuname 2>/dev/null`" : '.* is .*/uuname$' >/dev/null 2>&1 ; then
		UUNAME=`uuname -l`
	else
		UUNAME=`hostname`
	fi
	export UUNAME
fi

if [ -z "${HOSTNAME}" ] ; then
	if expr "`type hostname 2>/dev/null`" : '.* is .*/hostname$' >/dev/null 2>&1 ; then
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
	if [ -r /etc/resolv.conf ] && fgrep domain /etc/resolv.conf >/dev/null 2>&1; then
		#
		# here we use domain, not "search", on purpose so that
		# we will only set DOMAINNAME if that's been done
		# explicitly in resolv.conf -- normally dhclient and
		# such will set "search" from the network
		#
		# xxx except on MacOS X, sigh... (but see above....)
		#
		eval `sed -n '/^[;#]/d;s/domain[ 	]*/DOMAINNAME=./p' /etc/resolv.conf`
	elif expr "`type domainname 2>/dev/null`" : '.* is .*/domainname$' >/dev/null 2>&1 ; then
		DOMAINNAME=`domainname`
	elif expr "${HOSTNAME}" : '[^\.]*\.' >/dev/null 2>&1 ; then
		DOMAINNAME="."`expr "${HOSTNAME}" : '[^\.]*\.\(.*\)$'`
	else
		# these cases for machines without domainname,
		# and a short hostname....
		#
		case "${UUNAME}" in
		weirdo )
			DOMAINNAME=".weird.com"
			;;
		* )
			DOMAINNAME=".UUCP"
			;;
		esac
	fi
	export DOMAINNAME
fi

TTY=`tty` ; export TTY
TTYN=`tty | sed 's|/dev/||'`; export TTYN

###echo "$0: startup PATH=$PATH"

# this is a bit ugly, but we have to do this before .localprofile so
# that Fink's brain-dead insistance on being first can be corrected.
# (otherwise it's impossible to put ${HOME}/bin first!)
#
if [ -r ${FINK}/bin/init.sh ] ; then
	# this sets up other handy things for Fink
	. ${FINK}/bin/init.sh
	dirremove PATH ${FINK}/sbin
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



if [ -z "${LOCAL}" ] ; then
	# NOTE:  ${LOCAL} must not contain multiple words!
	if [ -d /local -a ! -L /local ] ; then
		LOCAL="/local"
	elif [ -d /usr/local -a ! -L /usr/local ] ; then
		LOCAL="/usr/local"
	else
		LOCAL="/NO-local-FOUND"
	fi
fi
export LOCAL

if [ -z "${CONTRIB}" ] ; then
	# NOTE:  ${CONTRIB} must not contain multiple words!
	if [ -d /contrib -a ! -L /contrib ] ; then
		CONTRIB="/contrib"
	elif [ -d /usr/contrib -a ! -L /usr/contrib ] ; then
		CONTRIB="/usr/contrib"
	else
		CONTRIB="/NO-contrib-FOUND"
	fi
fi
export CONTRIB

if [ -z "${PKG}" ] ; then
	# NOTE:  ${PKG} must not contain multiple words!
	if [ -d /pkg -a ! -L /pkg ] ; then
		PKG="/pkg"
	elif [ -d /usr/pkg -a ! -L /usr/pkg ] ; then
		PKG="/usr/pkg"
	elif [ -d /opt/pkg -a ! -L /opt/pkg ] ; then
		PKG="/opt/pkg"
	else
		PKG="/NO-pkg-FOUND"
	fi
fi
export PKG

if [ -z "${SLASHOPT}" ] ; then
	# NOTE:  ${SLASHOPT} must not contain multiple words!
	if [ -d /opt -a ! -L /opt ] ; then
		SLASHOPT="/opt"
	elif [ -d /usr/opt -a ! -L /usr/opt ] ; then
		SLASHOPT="/usr/opt"
	else
		SLASHOPT="/NO-opt-FOUND"
	fi
fi
export SLASHOPT

if [ -z "${FINK}" ] ; then
	# NOTE:  ${FINK} must not contain multiple words!
	if [ -d /sw -a ! -L /sw ] ; then
		FINK="/sw"
	else
		FINK="/NO-fink-FOUND"
	fi
fi
export FINK

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

if [ -z "${PROJECT}" ] ; then
	PROJECT="SCCS"
fi
export PROJECT

if [ -z "${WORKPATH}" ] ; then
	WORKPATH=${HOME}/work.d:${HOME}/work
fi
dirappend WORKPATH /work/${LOGNAME} ${LOCAL}/work.d/${LOGNAME}
export WORKPATH

# TODO: explore more options for this....  (xmkmf?)
#
# TODO: what if there's more than one variant installed?
#
# don't worry about openwin -- it's handled in the ISSUN case below
#
if [ -z "${X11PATH}" ] ; then
	for x11pc in /opt/X11 /local/X11R? /local/X11 /usr/X11R? /usr/X11 /usr/X??? /usr/local/X11R? /usr/local/X11; do
		if [ -d ${x11pc} -a ! -L ${x11pc} -a -d ${x11pc}/bin -a -x ${x11pc}/bin/xterm ] ; then
			X11PATH=${x11pc}
			break;
		fi
	done
	if [ -z "${X11PATH}" ] ; then
		X11PATH="/NO-X11-FOUND"
	fi
	export X11PATH
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

# TODO: some of these we may want fixed in even if they don't exist at
# login time...
#
dirappend PATH /usr/ccs/bin /usr/xpg4/bin ${X11BIN} ${LOCAL}/bin ${GNU}/bin ${CONTRIB}/bin ${PKG}/bin /usr/ucb /usr/bsd ${SLASHOPT}/bin ${SLASHOPT}/gnu/bin
dirappend PATH ${HI_TECH_C}/bin
dirappend PATH /usr/games ${LOCAL}/games ${SLASHOPT}/games/bin

# CDPATH isn't supported in all shells, but it won't hurt....
#
# make sure these directories are fixed in even if they are not
# present at login time.
#
if [ -n "${CDPATH}" ]; then
	OCDPATH=${CDPATH}; export OCDPATH
fi
CDPATH=:${HOME}:${WORKPATH}:${HOME}/src:${HOME}/src/lib:/usr/pkgsrc:/usr/ports

export CDPATH

if [ -n "${MANPATH}" ]; then
	OMANPATH=${MANPATH} ; export OMANPATH
fi

# don't set MANPATH with 4.4BSD man....
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

dirprepend INFOPATH ${LOCAL}/share/info ${LOCAL}/info ${GNU}/info ${CONTRIB}/share/info ${CONTRIB}/info ${PKG}/share/info ${PKG}/gnu/share/info ${PKG}/info ${X11PATH}/info

ISSUN=false; export ISSUN
if [ -x /usr/bin/sun ] ; then
	if sun ; then
		ISSUN=true
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
	if [ ! -f ${HOME}/.xsession ] ; then
		ln -fs .xinitrc ${HOME}/.xsession
	fi
fi
# note .emacs.elc may not yet exist
if [ ! -f ${HOME}/.emacs -a -f ${HOME}/.emacs.el ] ; then
	ln -fs .emacs.elc ${HOME}/.emacs
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
	dirprepend PATH ${HOME}/bin ${HOME}/go/bin ${HOME}/usr/bin
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

if expr "`type mktable 2>/dev/null`" : '.* is .*/mktable$' >/dev/null 2>&1 ; then
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
# note: LC_ALL overrides all LC_* variables, LANG is the default for
# unset LC_* variables, and LESSCHARSET is the equivalent of LC_CTYPE
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
		# XXX when can I jump to UTF-8????
		;;
	xterm*)
		# Xterm probably set things up OK itself, especially if uxterm
		#
		# For less, from the man page:
		#	If neither LESSCHARSET nor LESSCHARDEF is set, but any of the strings
		#	"UTF-8", "UTF8", "utf-8" or "utf8" is found in the LC_ALL, LC_CTYPE or
		#	LANG environment variables, then the default character set is utf-8.
		unset LESSCHARSET
		;;
	esac
fi

# XXX can these ($RSH and $SSH) cause problems with other tools?
# (will be OK with at least cvs and rsync which use ${ARGV0}_RSH)
#
if expr "`type rsh 2>/dev/null`" : '.* is .*/rsh$' >/dev/null ; then
	RSH="rsh"
elif [ -x /usr/ucb/rsh ] ; then	# maybe /usr/ucb not in $PATH?
	RSH="/usr/ucb/rsh"
else
	# assuming 'ssh' exists and is the only way to run remote shells...
	RSH="ssh"
fi
export RSH			# used by .twmrc, .ctwmrc, as well as .xinitrc

if expr "`type ssh 2>/dev/null`" : '.* is .*/ssh$' >/dev/null ; then
	SSH="ssh"
elif expr "`type ssh2 2>/dev/null`" : '.* is .*/ssh2$' >/dev/null ; then
	SSH="ssh2"
else
	# assuming 'ssh' really is available...
	SSH="ssh"
fi
export SSH			# used by .twmrc, .ctwmrc, as well as .xinitrc

HAVEMONTH=false ; export HAVEMONTH
if expr "`type month 2>/dev/null`" : '.* is .*/month$' >/dev/null 2>&1 ; then
	HAVEMONTH=true
fi

MONTH="AIKO" ; export MONTH

HAVELAYERS=false ; export HAVELAYERS
if expr "`type layers 2>/dev/null`" : '.* is .*/layers$' >/dev/null 2>&1 ; then
	HAVELAYERS=true
fi

HAVEMUSH=false ; export HAVEMUSH
MAILER=mail ; export MAILER
if [ -s ${HOME}/.mailer ] ; then
	# mktable just throws away comments....
	MAILER=`mktable ${HOME}/.mailer`
elif expr "`type mush 2>/dev/null`" : '.* is .*/mush$' >/dev/null 2>&1 ; then
	HAVEMUSH=true
	MAILER="mush"
elif expr "`type Mail 2>/dev/null`" : '.* is .*/mailx$' >/dev/null 2>&1 ; then
	MAILER="Mail"
elif expr "`type mailx 2>/dev/null`" : '.* is .*/mailx$' >/dev/null 2>&1 ; then
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

HAVECALENDAR=false ; export HAVECALENDAR
if expr "`type calendar 2>/dev/null`" : '.* is .*/calendar$' >/dev/null 2>&1 ; then
	HAVECALENDAR=true
fi

HAVEFORTUNE=false ; export HAVEFORTUNE
if expr "`type fortune 2>/dev/null`" : '.* is .*/fortune$' >/dev/null 2>&1 ; then
	HAVEFORTUNE=true
	FORTUNE=fortune ; export FORTUNE
fi

if expr "`type less 2>/dev/null`" : '.* is .*/less$' >/dev/null 2>&1 ; then
	PAGER="less"
	LESS="-M" ; export LESS
	if [ ! -f ${HOME}/.less ] ; then
		# xxx lesskey(1) seems to have gone missing on OSX 10.8 and 10.9
		# (maybe even on 10.7 too?)
		if expr "`type lesskey 2>/dev/null`" : '.* is .*/lesskey$' >/dev/null 2>&1 ; then
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
elif expr "`type more 2>/dev/null`" : '.* is .*/more$' >/dev/null 2>&1 ; then
	PAGER="more"
	# use '-s' as it can't be turned on later during runtime
	MORE="-sw" ; export MORE
else
	PAGER="cat"
fi
export PAGER
if [ "${PAGER}" = "less" ] ; then
	MANPAGER="${PAGER} -si"; export MANPAGER
fi

if [ -s "${HOME}/.editor" ] ; then
	# mktable just throws away comments....
	EDPREF=`mktable ${HOME}/.editor` ; export EDPREF
fi

case "${EDPREF}" in
emacs | "" )
	HAVEEMACS=false
	HAVEJOVE=false
	if expr "`type emacs 2>/dev/null`" : '.* is .*/emacs$' >/dev/null 2>&1 ; then
		EDITOR="emacs"
		HAVEEMACS=true
	elif expr "`type jove 2>/dev/null`" : '.* is .*/jove$' >/dev/null 2>&1 ; then
		EDITOR="jove"
		HAVEJOVE=true
	else
		EDITOR="ed"
	fi
	if ${HAVEEMACS} ; then
		VISUAL="emacs"
		if [ -n "${DISPLAY}" ] ; then
			case "${TERM}" in
			xterm*)
				# XXX this is ugly and not exactly right.
				# (should use type to test "id" for one)
				if [ -x /usr/bin/id ] ; then
					eval `id | sed 's/[^a-z0-9=].*//'`
					# TODO: maybe not?
					if [ "${uid:=0}" -ne 0 -a -n "${DISPLAY}" ] ; then
						VISUAL="emacsclient"
					fi
				fi
			;;
			esac
		fi
	elif ${HAVEJOVE} ; then
		VISUAL="jove"
	else
		VISUAL="vi"
	fi
	;;
vi )
	if expr "`type nvi 2>/dev/null`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		EDITOR="nvi"
	elif expr "`type vi 2>/dev/null`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		EDITOR="vi"
	else
		EDITOR="ed"
	fi
	if expr "`type nvi 2>/dev/null`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		VISUAL="nvi"
	elif expr "`type vi 2>/dev/null`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		VISUAL="vi"
	else
		VISUAL="no-visual-editor"
	fi
	;;
* )
	if expr "`type nvi 2>/dev/null`" : '.* is .*/nvi$' >/dev/null 2>&1 ; then
		EDITOR="nvi"
	elif expr "`type vi 2>/dev/null`" : '.* is .*/vi$' >/dev/null 2>&1 ; then
		EDITOR="vi"
	else
		EDITOR="ed"
	fi
	if expr "type ${EDPREF} 2>/dev/null" : '.*/.*$' > /dev/null 2>&1 ; then
		VISUAL=${EDPREF}
	else
		VISUAL=${EDPREF}
	fi
esac
export EDITOR
export VISUAL

EXINIT="set sm" ; export EXINIT

if [ -z "${CVSROOT}" ] ; then
	CVSROOT=${LOCAL}/src-CVS ; export CVSROOT
fi

# on older systems GNU Diff is preferred for things that use $DIFF,
# but sometimes it's in ${LOCAL}/bin as just "diff"
#
if [ -x ${LOCAL}/bin/diff ] ; then
	DIFF=${LOCAL}/bin/diff ; export DIFF
elif expr "`type gdiff 2>/dev/null`" : '.* is .*/gdiff$' >/dev/null 2>&1 ; then
	# XXX this isn't always best any more!
	DIFF="gdiff" ; export DIFF
fi

HAVEAUPLAY=false ; export HAVEAUPLAY
if expr "`type auplay 2>/dev/null`" : '.* is .*/auplay$' >/dev/null 2>&1 ; then
	HAVEAUPLAY=true
fi
HAVEAUDIOPLAY=false ; export HAVEAUDIOPLAY
if expr "`type audioplay 2>/dev/null`" : '.* is .*/audioplay$' >/dev/null 2>&1 ; then
	HAVEAUDIOPLAY=true
fi
HAVEESDPLAY=false ; export HAVEESDPLAY
if expr "`type esdplay 2>/dev/null`" : '.* is .*/esdplay$' >/dev/null 2>&1 ; then
	HAVEESDPLAY=true
fi

if [ -n "${AUDIOPLAYER}" ] ; then
	if [ -n "${AUDIOSERVER}" ] ; then
		if ${HAVEAUPLAY} ; then
			AUDIOPLAYER="auplay -v 20"
		fi
	elif [ -w /dev/audio ] ; then
		if ${HAVEAUDIOPLAY} ; then
			AUDIOPLAYER="audioplay"
		fi
	else
		if ${HAVEESDPLAY} ; then
			AUDIOPLAYER="esdplay"
		fi
	fi
fi
if [ -n "${AUDIOPLAYER}" ] ; then
	# avoid trying to run audio files...
	AUDIOPLAYER=":"
fi
export AUDIOPLAYER

RNINIT="-v -M -S -T -i=8 -g2" ; export RNINIT
TRNINIT=${HOME}/.trninit; export TRNINIT

# set terminal type and tty settings, etc....
#
if ${ISATTY} && [ "X$argv0" != "X.xsession" -a "X$argv0" != "X.xinitrc" ] ; then
	echo "Re-setting terminal preferences...."

	# turn this off by default, turn it on by hand?
	mesg n

	if [ -r "${HOME}/.stty" ] ; then
		. ${HOME}/.stty
	else
		stty erase '^h' intr '^?' kill '^u' -ixany echo echoe echok
		# a separate command as it is a non-standard parameter
		stty status '^t' 2>/dev/null || echo "Sorry, probably no SIGNIFO support on your system."
	fi
	if [ "${EMACS}" = t -o "${TERM}" = emacs ]; then
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
			ttytype=${TERM}
			# xxx a function may not work to set an env var
			get_newterm
			;;
		*)
			if ${HAVETPUT} ; then
				tput init || { ttytype=dumb; get_newterm; }
			else
				if expr "`type tset 2>/dev/null`" : '.* is .*/tset$' >/dev/null 2>&1 ; then
					# n.b.:  this asks if TERM is unknown...
					eval $(tset -s)
					# xxx if we want this, we get it later!
					unset TERMCAP
				else
					echo "NOTICE:  I don't know how to test your TERM ($TERM)."
				fi
			fi
			;;
		esac

		export TERM

		case "${TERM}" in
		vt220)
			if [ ! -r ${HOME}/.stty ] ; then
				# real vt220 keyboards make this the best setup
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
		if ${HAVETPUT} ; then
			# argh!  "init" does not reset tabs on some
			# terminals, e.g. xterm, if they have been
			# cleared, but "reset" will clear the screen!
			#
			tput init
		else
			# Note: in other places we assume tset is avaliable....
			if expr "`type tset 2>/dev/null`" : '.* is .*/tset$' >/dev/null 2>&1 ; then
				# On BSD, without the "-I" it uses /etc/termcap....
				# (but maybe that is a good thing?)
				tset -r
			elif expr "`type tabs 2>/dev/null`" : '.* is .*/tabs$' >/dev/null 2>&1 ; then
				# maybe the best we can do for now?
				tabs -8
			else
				echo "NOTICE:  I don't know how to set up your terminal."
			fi
		fi

		# try setting up for X11 if possible....
		case "${TERM}" in
		xterm*|sun|pc3|ibmpc3)
			# users will have to set their own $DISPLAY....
			dirappend PATH ${X11PATH}/bin
			dirappend MANPATH ${X11PATH}/man
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

		# normally the message from "tset -s" on stderr will
		# tell us what our current erase and interrupt chars are
		#
		tset -s > /dev/null

		;;
	esac
fi

# TODO: find some way to see if login(1) ran, or xterm(n) started us
# TODO: since login(1) checks for mail too, but xterm(n) doesn't.
#
# check your mail...
if expr "`type messages 2>/dev/null`" : '.* is .*/messages$' >/dev/null 2>&1 ; then
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


# TODO: this needs to be a lot smarter....
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
if [ ${RANDOM:-0} -ne ${RANDOM:-0} -a -z "${BASH}" ] ; then
	if [ -r ${HOME}/.kshlogin ] ; then
		. ${HOME}/.kshlogin
	fi
elif [ ${RANDOM:-0} -ne ${RANDOM:-0} -a -n "${BASH}" ] ; then
	if [ -r ${HOME}/.bashlogin ] ; then
		. ${HOME}/.bashlogin
	fi
elif [ "`echo ~`" = "${HOME}" ] ; then
	# this will only be modern ash(1) or a derivative of it
	# (eg. from 4.4BSD or newer)
	#
	if [ -r ${HOME}/.ashlogin ] ; then
		. ${HOME}/.ashlogin
	fi
elif [ -r ${HOME}/.shlogin ] ; then
	if [ -r ${HOME}/.shlogin ] ; then
		. ${HOME}/.shlogin
	fi
else
	if [ "X${LOGNAME}" = "Xroot" ] ; then
		PS1="login [${TTYN}]<${LOGNAME}@${UUNAME}> # "
	else
		PS1="login [${TTYN}]<${LOGNAME}@${UUNAME}> $ "
	fi
fi


HAVEX=false ; export HAVEX
if expr "`type xinit 2>/dev/null`" : '.* is .*/xinit$' >/dev/null 2>&1 ; then
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
				tput clear
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
	# TODO:  should use $HAVEFORTUNE and $FORTUNE
	if [ -x /usr/games/fortune ] ; then
		/usr/games/fortune
	elif [ -x ${LOCAL}/games/fortune ] ; then
		${LOCAL}/games/fortune
	fi
	if [ -r calendar -o -r diary -o -r .month ] ; then
		echo ""
		echo "Today's Events:"
		if ${HAVEMONTH} && [ -r .month ] ; then
			month -B
			#		monthd -i5
		fi
		if ${HAVECALENDAR} ; then
			if [ -r calendar ] ; then
				calendar -l 2 -w 4
			elif [ -r diary ] ; then
				#
				# uses cpp, which gets confused with some
				# comments....  and unfortunately won't accept
				# '-f -', nor will it read from /dev/stdin or
				# /fdesc/stdin...  grrr....
				#
				calendar -l 2 -w 4 -f diary 2>/dev/null
			fi
		fi
	fi
	if [ -d ${HOME}/notes ] ; then
		(
			cd ${HOME}/notes
			echo ""
			echo "You have notes on:"
			ls -dC *[!~]
		)
	fi
fi

if [ -r ${HOME}/.trninit${TERM} ] ; then
	TRNINIT=${HOME}/.trninit${TERM} ; export TRNINIT
fi

# There is a trick here -- if your shell is like ksh(1) or modern GNU Bash, and
# it has arrays, then you can use the ${ENVFILE[]} array expansion magic in
# ~/.kshlogin to have the shell only source the $ENV file for interactive
# shells.
#
# Otherwise $ENV will just be a filename and it may have to be sourced directly
# (or manually), so if $ENV is set, and if it is readable, do that now.
#
if [ -n "${ENV}" -a -r "${ENV}" ] ; then
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
