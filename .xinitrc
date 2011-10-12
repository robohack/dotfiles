#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	33.1	11/10/12 16:06:56 (woods)"

# Assumptions that may cause breakage:
#
#	- the shell supports functions and getopts and $((expr))
#	- test(1), aka "[", supports '-L' for testing symlinks
#	  (note that "test -L" is POSIX, but old systems had "-h")

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="`basename $0`"
echo "$argv0: running from $0"
XOPATH=$PATH
echo "$argv0: initial PATH='$PATH'"
XODISPLAY=$DISPLAY
echo "$argv0: starting up with DISPLAY='$DISPLAY'"

# note we redirect input from /dev/null while sourcing in hopes of
# showing these scripts that they are not attached to a TTY.
#
if [ -r /etc/profile ] ; then
	. /etc/profile < /dev/null
else
	echo "$argv0: no /etc/profile on this host!"
fi
if [ -r $HOME/.profile ] ; then
	. $HOME/.profile < /dev/null
else
	echo "$argv0: no $HOME/.profile on this host!"
fi

if [ "$XOPATH" != "$PATH" ] ; then
	echo "$argv0: adjusted PATH='$PATH'"
fi
unset XOPATH

if [ $argv0 = ".xsession" -a -z "$XDMHOST" ] ; then
	# assume HOSTNAME & DOMAINNAME are set by profile(s)
	XDMHOST="${HOSTNAME}${DOMAINNAME}"
	export XDMHOST					# also used by .twmrc
	echo "$argv0: have set XDMHOST='$XDMHOST'"
fi

# XXX for debugging
#
#set -x

ONCONSOLE=false
export ONCONSOLE
case "$DISPLAY" in
:[0-9]*)
	# 
	# OK, so if we're running on the system console there's a good
	# chance the user will never see system messages sent to
	# /dev/console so we will try to run xconsole and also
	# redirect our own output to /dev/console so that window
	# manager and application errors will also go to the xconsole
	# window.  Unfortunately there's no logging this way though...
	# 
	# don't bother with this unless it'll do us some good...
	# (i.e. unless XDM ran the GiveConsole script)
	#
	if [ `uname -s` = "Darwin" ]; then
		tail -F /var/log/system.log | xconsole -geometry 500x120-0-0 -notify -verbose -file /dev/stdin &
	elif [ -w /dev/console ] ; then
		ONCONSOLE=true
		echo "$argv0: redirecting stdout and stderr to /dev/console."
		exec 1>/dev/console 2>&1
		echo "$argv0: redirected stdout and stderr to /dev/console, starting xconsole"
		xconsole -geometry 500x120-0-0 -notify -verbose &
		# wait a wee bit in hopes xconsole gets running to display any errors....
		sleep 1
	else
		echo "$argv0: WARNING: display was '$DISPLAY' but /dev/console is not writable by $USER!"
		ls -l /dev/console
	fi
	if [ -n "${XDMHOST}" -o -n "${DOMAINNAME}" ]; then
		DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}${DISPLAY}"
		echo "$argv0: have set DISPLAY='$DISPLAY'"
	else
		# using the local ":N.n" syntax is supremely preferred for the local host
		# XXX ${DOMAINNAME} is probably not right here....
		# XXX and if so, ${HOATNAME} must be something remote client hosts understand
		REMOTE_DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}${DISPLAY}"
		export REMOTE_DISPLAY
		echo "$argv0: have set REMOTE_DISPLAY='$REMOTE_DISPLAY'"
	fi
	;;
esac
export DISPLAY

if [ "$XODISPLAY" != "$DISPLAY" ] ; then
	echo "$argv0: adjusted DISPLAY='$DISPLAY'"
fi

# may be things that depend on DISPLAY in here, so source again....
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile < /dev/null
fi

# if this still isn't set then we probably didn't have a ~/.profile!
#
# TODO: what if there's more than one variant installed?
#
if [ -z "$X11PATH" ] ; then
	# FIXME: this won't work very well if X11R? is multiple names....
	if [ -d /local/X11R? -a ! -L /local/X11R? ] ; then
		X11PATH="`echo /local/X11R?`"
	elif [ -d /local/X11 -a ! -L /local/X11 ] ; then
		X11PATH="/local/X11"
	elif [ -d /usr/X11R? -a ! -L /usr/X11R? ] ; then
		X11PATH="`echo /usr/X11R?`"
	elif [ -d /usr/X11 -a ! -L /usr/X11 ] ; then
		X11PATH="/usr/X11"
	elif [ -d /usr/X??? -a ! -L /usr/X??? ] ; then	# X386, for example
		X11PATH="`echo /usr/X???`"
	elif [ -d /usr/local/X11R? -a ! -L /usr/local/X11R? ] ; then
		X11PATH="`echo /usr/local/X11R?`"
	elif [ -d /usr/local/X11 -a ! -L /usr/local/X11 ] ; then
		X11PATH="/usr/local/X11"
	else
		X11PATH="/NO-X11-FOUND"
	fi
	export X11PATH
fi

# if this still isn't set then we probably didn't have a ~/.profile!
#
if [ -z "$X11BIN" ] ; then
	echo "$argv0: X11PATH not set  by ~/.*profile!"
	if [ -d /usr/bin/X11 -a ! -L /usr/bin/X11 ] ; then
		X11BIN=/usr/bin/X11
	else
		X11BIN=$X11PATH/bin
	fi
	export X11BIN
fi

# just in case we don't have a ~/.profile
#
if expr ":${PATH}:" : ".*:$X11BIN:.*" > /dev/null ; then
	:
else
	PATH=$PATH:$X11BIN
	export PATH
fi

VENDORSTR="`xdpyinfo | sed -n 's/^vendor string:[ ]*\(.*\)$/\1/p'`"
DEPTHSTR="`xdpyinfo | fgrep 'depth of root window'`"
SCREENDEPTH=`expr "$DEPTHSTR" : '[ ]*depth of root window:[ ]*\([0-9]*\) '`
export VENDORSTR SCREENDEPTH
unset DEPTHSTR

# TODO:  think about using `type` ala .profile, but type isn't everywhere
if [ -z "$RSH" ] ; then
	if expr "`type rsh 2>/dev/null`" : '.* is .*/rsh$' >/dev/null ; then
		RSH="rsh"
	elif [ -x /usr/ucb/rsh ] ; then	# maybe /usr/ucb not in $PATH?
		RSH="/usr/ucb/rsh"
	else				# TODO: also remsh?
		echo "$argv0: assuming 'rsh' really is available..."
		RSH="rsh"
	fi
	export RSH			# used by .twmrc, .ctwmrc, as well as below
fi

onx11server ()
{
	_RDISP=""
	_USAGE="$argv0: $0(): Usage: onx11server [-n] [-D REMOTE_DISPLAY] SERVERNAME 'command string'"
	_nullopt=""
	while getopts nD: OPTCH
	do
		case $OPTCH in
		n)
			_nullopt="-n"
			;;
		D)
			_RDISP="${OPTARG}"
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


if [ -r "/usr/lib/X11/Xdefaults" ] ; then
	xrdb /usr/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$X11PATH/lib/X11/Xdefaults" ] ; then
	xrdb $X11PATH/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$OPENWINHOME/lib/Xdefaults" ] ; then
	xrdb $OPENWINHOME/lib/Xdefaults	# Load default X11 resource database
fi
if [ -r $HOME/.Xdefaults ] ; then
	xrdb $HOME/.Xdefaults		# Load user's X11 resource database
fi

# Unfortunately there is no real way to tell what type of keyboard is
# attached and no way to revert unknown earlier changes, so instead we
# must just rely on the display name to know what keyboard is attached.
#
#case "$VENDORSTR" in
#"Network Computing Devices"*)
case $DISPLAY in
xtra*:*|xtremely*:*)
	if [ -r $HOME/.Xmodmap-ncd-108 ] ; then
		case "$XDMHOST" in
		proven.weird.com|starting-out.weird.com)
			# should have been done by xdm setup script....
			echo "$argv0: NOT setting NCD keyboard map on $XDMHOST!"
			;;
		*)
			echo "$argv0: setting NCD keyboard map from $HOME/.Xmodmap-ncd-108"
			xmodmap $HOME/.Xmodmap-ncd-108
			;;
		esac
	else
			echo "$argv0: $HOME/.Xmodmap-ncd-108 not found!"
	fi
	;;
esac

# TODO:  allowing the local host might be a mistake (since other users
# might also have access to the local host), but doing so might save
# your bacon if xdm has not set up ~/.Xauthority correctly!
#
xhost +`hostname` +localhost -

# deprecated xhost crap
#
if [ -r $HOME/.xhosts ] ; then
	echo "$argv0: now setting up all the DANGEROUS deprecated xhost access controls!"
	while read XHOST junk; do
		case "$XHOST" in
		"" | '\#'* )
			;;
		* )
			# NEVER remove the "" from the first clause!!!
			xhost +$XHOST
			;;
		esac
	done < $HOME/.xhosts
fi

# If you're running on a diskless workstation that runs xdm itself
# then you probably have a server that's more powerful and on which
# you'd rather run the majority of your X applications.  Normally
# this will be the same server your home directory resides on....
#
if [ "$XDMHOST" = "`echo $DISPLAY | sed 's/:.*$//'`" ] ; then
	echo "$argv0: looks like $XDMHOST is running xdm for our display..."
	case "$XDMHOST" in
	almost.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		# now move our AUTH cookies over to the server in case it
		# doesn't have a shared $HOME...
		xauth extract - $DISPLAY | onx11server $X11SERVER 'xauth merge -'
		# now start server programs we like for this scenario
		onx11server -n $X11SERVER 'xload -display $DISPLAY -geometry 120x40-200+48 -hl grey90' &
		;;
	very.local:very.weird.com|very.robohack.planix.com)
		X11SERVER=${X11SERVER:-"once.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		# now move our AUTH cookies over to the server in case it
		# doesn't have a shared $HOME...
		xauth extract - $DISPLAY | onx11server $X11SERVER 'xauth merge -'
		# we run these ones in the background because the hosts might not be up
		xauth extract - $DISPLAY | onx11server most.weird.com 'xauth merge -' &
		xauth extract - $DISPLAY | onx11server isit.weird.com 'xauth merge -' &
		xauth extract - $DISPLAY | onx11server historically.weird.com 'xauth merge -' &
		# now start server programs we like for this scenario
		onx11server -n $X11SERVER 'xload -display $DISPLAY -geometry 120x40-200+48 -hl red' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
		;;
	*)
		# not setting X11SERVER hopefully just makes the WM run all X11SERVER clients on
		# the current login host....  unless it eliminates the menu entries entirely!
		unset X11SERVER
		echo "$argv0: have unset X11SERVER; XDMHOST is $XDMHOST"
		;;
	esac
fi

if [ -z "$WM" ] ; then
	echo "$argv0: searching for a usable window manager..."
	#
	# XXX maybe it would be nice to have a window-manager-chooser
	#
	if expr "`type ctwm 2>/dev/null`" : '.* is .*/ctwm$' >/dev/null ; then
		# NOTE: don't bother with '-v' -- it's too much and slows things!
		if [ $SCREENDEPTH -eq 1 ] ; then
			# the welcome screen on monochrome is silly and slow
			WM="ctwm -k -display $DISPLAY -W"
		else
			WM="ctwm -k -display $DISPLAY"
		fi
	elif expr "`type tvtwm 2>/dev/null`" : '.* is .*/tvtwm$' >/dev/null ; then
		WM=tvtwm
	elif expr "`type wmx 2>/dev/null`" : '.* is .*/wmx$' >/dev/null ; then
		WM=wmx
	elif expr "`type wm2 2>/dev/null`" : '.* is .*/wm2$' >/dev/null ; then
		WM=wm2
	elif expr "`type scwm 2>/dev/null`" : '.* is .*/scwm$' >/dev/null ; then
		WM=scwm
	elif expr "`type wmaker 2>/dev/null`" : '.* is .*/wmaker$' >/dev/null ; then
		WM=wmaker
	elif expr "`type fvwm 2>/dev/null`" : '.* is .*/fvwm$' >/dev/null ; then
		WM=fvwm
	else
		echo "$argv0: assuming 'twm' is available..."
		WM=twm					# silly assumption?
	fi
	export WM
fi

HAVESWISSWATCH=false ; export HAVESWISSWATCH
if expr "`type swisswatch 2>/dev/null`" : '.* is .*/swisswatch$' >/dev/null ; then
	HAVESWISSWATCH=true
fi
HAVEXBUFFY=false ; export HAVEXBUFFY
if expr "`type xbuffy 2>/dev/null`" : '.* is .*/xbuffy$' >/dev/null ; then
	HAVEXBUFFY=true
fi
HAVEXEARTH=false ; export HAVEXEARTH
if expr "`type xearth 2>/dev/null`" : '.* is .*/xearth$' >/dev/null ; then
	HAVEXEARTH=true
fi
HAVEXPHOON=false ; export HAVEXPHOON
if expr "`type xphoon 2>/dev/null`" : '.* is .*/xphoon$' >/dev/null ; then
	HAVEXPHOON=true
fi
HAVEXV=false ; export HAVEXV
if expr "`type xv 2>/dev/null`" : '.* is .*/xv$' >/dev/null ; then
	HAVEXV=true
fi

# argv0 is .xinitrc if called by xinit or startx
#
if [ "$argv0" = ".xinitrc" ] ; then
	echo "$argv0: looks like X was started with xinit or startx..."
	case $DISPLAY in
	:*)
		# XXX assume for now that "once" is at 10.0.1.128....
		#
		# we could/should run these ones in the background
		# because the hosts might not be up, but then we would
		# also have to figure out if we could/should try to
		# start the next programs as well....
		xauth nextract - ${HOSTNAME}${DISPLAY} | onx11server -D ${REMOTE_DISPLAY} 10.0.1.128 'xauth nmerge -'
		# and then start some useful programs...
		onx11server -n -D $REMOTE_DISPLAY 10.0.1.128 'xload -geometry 120x40-200+48 -hl red' &
		onx11server -n -D $REMOTE_DISPLAY 10.0.1.128 'uxterm -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &
		onx11server -n -D $REMOTE_DISPLAY 10.0.1.128 'uxterm -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
###
### XXX these would need to be run though SSH with ssh-agent or
### automatic keys or in an existing SSH tunnel:
###
###		xauth nextract - ${SSH_DISPLAY} | onx11server centrally.weird.com 'xauth nmerge -'
###		onx11server -S -n -D $SSH_DISPLAY centrally.weird.com 'xload -geometry 120x40-200+96 -hl red' &
###		onx11server -S -n -D $SSH_DISPLAY centrally.weird.com 'uxterm -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &
###		onx11server -S -n -D $SSH_DISPLAY centrally.weird.com 'uxterm -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
		;;
	very.weird.com:*|very.robohack.planix.com:*)

		# XXX note: for the moment the rest of this section is identical to the XDM variant above for this host.

		X11SERVER=${X11SERVER:-"once.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		# now move our AUTH cookies over to the server in case it
		# doesn't have a shared $HOME...
		xauth extract - $DISPLAY | onx11server $X11SERVER 'xauth merge -'
		# we could/should run these ones in the background because the hosts might not be up
		xauth extract - $DISPLAY | onx11server most.weird.com 'xauth merge -'
		xauth extract - $DISPLAY | onx11server isit.weird.com 'xauth merge -'
		xauth extract - $DISPLAY | onx11server historically.weird.com 'xauth merge -'
		# and then start some useful programs...
		onx11server -n $X11SERVER 'xload -display $DISPLAY -geometry 120x40-200+48 -hl red' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
		;;
	esac
	if $HAVEXV && [ -d '/Library/Desktop Pictures/Nature' ]; then
		xv -root '/Library/Desktop Pictures/Nature/'* "$HOME/Pictures/"*.jpg &
	elif $HAVEXEARTH ; then
		XEGAMMA=""
		if [ $SCREENDEPTH -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"	# for Sun3 CG2
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
		unset XEGAMMA
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	elif [ -r /usr/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
elif $ONCONSOLE ; then
	#
	#	I/O is already redirected to /dev/console above, so we will see it...
	#
	:
else
	#
	#	we're probably signing in on an independent xterm....
	#
	if [ -r $HOME/.xsession-errors-$DISPLAY ] ; then
		xconsole -geometry 500x120-0-0 -notify -verbose -file $HOME/.xsession-errors-$DISPLAY &
	elif [ "$XODISPLAY" != "$DISPLAY" -a -r $HOME/.xsession-errors-$XODISPLAY ] ; then
		xconsole -geometry 500x120-0-0 -notify -verbose -file $HOME/.xsession-errors-$XODISPLAY &
	else
		echo "$argv0: didn't find ~/.xsession-errors-$DISPLAY, assuming old XDM config in use"
		xconsole -geometry 500x120-0-0 -notify -verbose -file $HOME/.xsession-errors &
	fi
fi

# run localhost mailbox monitor of some sort...
#
if $HAVEXBUFFY ; then
	if [ -n "$AUDIOPLAYER" -a -r $HOME/lib/sounds/newmail ] ; then
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 -acmd "$AUDIOPLAYER $HOME/lib/sounds/newmail" &
	else
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 &
	fi
else
	xbiff -geometry 48x48-0+235 -bw 0 &
fi

#
# now we start more programs we like depending on which $DISPLAY we're
# using, but which don't depend on whether X was started by XDM or
# xinit/startx.
#

case $DISPLAY in

:*|very.local:*|very.weird.com:*|very.robohack.planix.com:*)
	# very has a very large, very hi-res, display....
	xset m 5 10

	xset fp= '/usr/X11/lib/X11/fonts/misc/,/usr/X11/lib/X11/fonts/TTF/,/usr/X11/lib/X11/fonts/OTF,/usr/X11/lib/X11/fonts/Type1/,/usr/X11/lib/X11/fonts/100dpi/:unscaled,/usr/X11/lib/X11/fonts/100dpi/,/sw/lib/X11/fonts/intl/,/sw/lib/X11/fonts/efont-unicode/,/sw/lib/X11/fonts/applettf/,/Library/Fonts/,/System/Library/Fonts/'
	xset fp rehash

	xload -geometry 120x40-200+0 -hl red &
	xclock -update 1 -geometry 215x30-0+196 -digital -chime &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi
	# local server performance levels we want to be aware of
	# (NOTE: in stacked order, 48 pixels apart)
#	$RSH -n once.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+48 -hl red &
#	$RSH -n most.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+96 -hl red &
#	$RSH -n isit.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+144 -hl red &
#	$RSH -n sometimes.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+192 -hl red &
#	$RSH -n historically.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+242 -hl red &
	# (and there is room for one under the clocks too)
#	$RSH -n almost.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-70+242 -hl red &

	# All the small lower-right command terminals (starting with our own first one)
	# note the -155 is the height of the normal xconsole window, at 120, plus its decorations (I think)
	uxterm -display $DISPLAY -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
#	onx11server -n once.weird.com 'uxterm -display $DISPLAY -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &

	# All the big main area terminals (starting with our own first one)
	uxterm -display $DISPLAY -geometry  132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
#	onx11server -n once.weird.com 'uxterm -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &

	# All the normal emacs windows...
	# oddly using 130x64 turns into 163x55 with the font change!
	# this one's supposed to go in virtualscreen "Mail"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -display $DISPLAY -geometry 130x75+0+0" &
	# this one's supposed to go in virtualscreen "Coding"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -display $DISPLAY -geometry 130x75+0+0" &
	# this one should go in the virtualscreen "News" 
###	$RSH -n whats.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -display $DISPLAY -geometry 130x75+0+0" &
	;;

xceptionally*:*)
	# xceptionally also has a large, hi-res, display....
	xset m 10 5
	# monitors (these could/should be started by xdm)
	xload -geometry 120x40-200+0 -hl red &
	xclock -update 1 -geometry 190x30-0+196 -digital -chime &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
	# local server performance levels we want to be aware of
	$RSH -n most.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+48 -hl grey90 &
	$RSH -n once.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+144 -hl grey90 &
#	$RSH -n starting-out.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+192 -hl grey90 &
#	$RSH -n whats.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-200+242 -hl grey90 &
#	$RSH -n always.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x40-70+242 -hl grey90 &

	# All the small lower-right command terminals (starting with our own first one)
	xterm -display $DISPLAY -geometry 80x42-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &

	# All the consoles:
	# first column....
	xterm -display $DISPLAY -geometry 102x24+0+0   -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:isit -T "Console [isit.weird.com]" -e console -S isit &
	xterm -display $DISPLAY -geometry 102x24+0+205 -fn 5x7 -cn -rw -sb -si -sk -sl 100 -ls -ziconbeep 1 -n Con:hubly -T "Console [hubly.weird.com]" -e console -S hubly &
	xterm -display $DISPLAY -geometry 102x24+0+410 -fn 5x7 -cn -rw -sb -si -sk -sl 100 -ls -ziconbeep 1 -n Con:flipping -T "Console [flipping.weird.com]" -e console -S flipping &
	xterm -display $DISPLAY -geometry 102x24+0+615 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:building -T "Console [building.weird.com]" -e console -S building &
	xterm -display $DISPLAY -geometry 102x24+0+820 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:whats -T "Console [whats.weird.com]" -e console -S whats &

	# first column, from the bottom...
	xterm -display $DISPLAY -geometry 80x24+0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 100 -ls -ziconbeep 1 -n Con:RAID0 -T "Console [RAID-00 most:/var]" -e console -S raid-00 &


	# second column....
#	xterm -display $DISPLAY -geometry 102x24+545+0 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:proven -T "Console [proven.weird.com]" -e console -S proven &
	xterm -display $DISPLAY -geometry 102x24+545+0 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:once -T "Console [once.weird.com]" -e console -S once &
	xterm -display $DISPLAY -geometry 102x24+545+205 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:sometimes -T "Console [sometimes.weird.com]" -e console -S sometimes &
	xterm -display $DISPLAY -geometry 102x24+545+410 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:always -T "Console [always.weird.com]" -e console -S always &
	xterm -display $DISPLAY -geometry 102x24+545+615 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:starting-out -T "Console [starting-out.weird.com]" -e console -S starting-out &
	xterm -display $DISPLAY -geometry 102x24+545+820 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n CallerID -T "very:/dev/ttyb [Caller ID Box]" -e console -S callerid &

	# second column, from the bottom...
	xterm -display $DISPLAY -geometry 102x24+545-0 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Con:best-1 -T "Console [BEST-3.1-UPS-1]" -e console -S best-3.1-1 &


	# start xv first, in a subshell that sleeps first for a bit
	# -- it needs to be running before we try our tkined trick, but after the window manager
###	( sleep 20; xv -owncm -iconic & ) &
	# start tkined last, in a subshell that sleeps first for a bit
	# -- hoping xv (from above) and the window manager are running already
###	( sleep 40; $RSH -n building.weird.com exec /usr/local/bin/tkined1.5.0 -display $DISPLAY -use $(xwininfo -name "xv 3.10a(PNG) <unregistered>" | awk '/xwininfo/ {print $4}') -geometry 1200x925+0-0 /home/building/woods/tkined-3.tki & ) &
	;;

xtremely*:*)
	xload -geometry 190x50-0+233 -hl red &
	xclock -norender -geometry 190x30-0+196 -digital &
	$RSH -n always.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x50-198+233 -hl red &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -norender -geometry 190x190-0+0 &
	fi
	# All the small lower-right command terminals (starting with our own first one)
	xterm -display $DISPLAY -geometry 80x42-0-0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &

	;;

*)
	# make everything rather small so that enough will fit on even
	# tiny screens....
	#
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
#	if [ $SCREENDEPTH -eq 1 ] ; then
#		xload -geometry 100x50-0+150 -hl grey90 &
#	else
#		xload -geometry 100x50-0+150 -hl red &
#	fi
#	xclock -norender -geometry 100x30-0+110 -digital &
	if $HAVESWISSWATCH ; then
		swisswatch -name swissclock -geometry 100x100-100+252 &
#	else
#		xclock -geometry 100x100-0+0 &
	fi
	xterm -display $DISPLAY -geometry 100x50+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
	xterm -display $DISPLAY -geometry 80x24+0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	;;
esac


# when the window manager exits so shall we...
#
exec $WM

# and if that fails then we try a last-ditch trick
#
xterm -geometry 80x24+0+0 -cn -rw -sb -si -sk -ls -n FailSafe
exit 1

# Local Variables:
# eval: (defun make-buffer-file-executable () (let* ((current-mode (file-modes (buffer-file-name))) (add-mode (logand ?\111 (default-file-modes)))) (or (/= (logand ?\111 current-mode) 0) (zerop add-mode) (set-file-modes (buffer-file-name) (logior current-mode add-mode)))))
# vc-checkout-hook: (make-buffer-file-executable)
# vc-checkin-hook: (make-buffer-file-executable)
# after-save-hook: (make-buffer-file-executable)
# End:
