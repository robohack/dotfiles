#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident        "@(#)HOME:.xinitrc     6.8     96/03/22 15:33:54 (woods)"

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="`basename $0`"

. /etc/profile
. $HOME/.profile

echo "$argv0: running from $0"
echo "$argv0: current PATH='$PATH'"
echo "$argv0: starting up with DISPLAY='$DISPLAY'"

if [ $argv0 = ".xsession" -a -z "$XDMHOST" ] ; then
	# assume HOSTNAME & DOMAINNAME are set by profile(s)
	XDMHOST="${HOSTNAME}${DOMAINNAME}"
	export XDMHOST					# used by .twmrc
	echo "$argv0: have set XDMHOST='$XDMHOST'"
fi

ONCONSOLE=false; export ONCONSOLE
case "$DISPLAY" in
":0"|":0.0"|"")
	# don't bother with this unless it'll do us some good...
	if [ -w /dev/console ] ; then
		ONCONSOLE=true; export ONCONSOLE
		exec 1>/dev/console 2>&1
	fi
	DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}:0.0"
	export DISPLAY
	echo "$argv0: have set DISPLAY='$DISPLAY'"
	;;
esac

# TODO: think about using `type` ala .profile, but type isn't everywhere
if [ -z "$RSH" ] ; then
	if [ -x /usr/ucb/rsh ] ; then
		RSH=/usr/ucb/rsh
	else
		RSH=/usr/bin/rsh
	fi
	export RSH					# used by .twmrc
fi

if [ -z "$ONX11SERVER" ] ; then
	case "$HOME" in
	*/most/*)
		if [ "$XDMHOST" != most.weird.com ] ; then
			ONX11SERVER="$RSH -n most.weird.com"
		fi
		;;
	esac
fi

if [ -z "$LOGNAME" ] ; then
	LOGNAME="$USER"
	export LOGNAME
fi

if [ -f "/usr/lib/X11/Xdefaults" ] ; then
	xrdb /usr/lib/X11/Xdefaults	# Load Default X11 resource database
elif [ -f "$X11PATH/lib/X11/Xdefaults" ] ; then
	xrdb $X11PATH/lib/X11/Xdefaults	# Load Default X11 resource database
elif [ -f "$OPENWINHOME/lib/Xdefaults" ] ; then
	xrdb $OPENWINHOME/lib/Xdefaults	# Load Default X11 resource database
fi
if [ -f $HOME/.Xdefaults ] ; then
	xrdb $HOME/.Xdefaults		# Load Users X11 resource database
fi

# TODO: this might be a mistake, but it might also save your bacon!
#
xhost +`hostname` +localhost -

if [ -s $HOME/.xhosts ] ; then
	HOSTLIST=""
	while read HOST junk; do
		case "$HOST" in
		"" | '#'* )
			;;
		* )
			if [ -n "$HOST" ] ; then
				HOSTLIST="$HOSTLIST +$HOST"
			fi
			;;
		esac
	done < $HOME/.xhosts
	if [ -n "$HOSTLIST" ] ; then
		xhost $HOSTLIST
	fi
	unset HOSTLIST
fi

# argv0 is .xinitrc if called by xinit or startx
# TODO:  always do, but only if we're not on the console....
#
if [ "$argv0" = .xinitrc ] ; then
	xconsole -geometry 500x160-4-4 &
	# wait to ensure xconsole is running to display errors....
	sleep 1
	if [ -x $X11BIN/xearth ] ; then
		DEPTHSTR="`xdpyinfo | fgrep 'depth of root window'`"
		if [ `expr "$DEPTHSTR" : '[ ]*depth of root window:[ ]*\([0-9]*\) '` -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"		# for Sun CG3
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif [ -x $X11BIN/xphoon ] ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	elif [ -r /usr/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
	elif [ -r /usr/openwin/share/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
	fi
	xclock -geometry 190x30-4+196 -digital &
	# ensure they get overlayed in the right order
	sleep 1
	xclock -geometry 190x190-4+4 &
	sleep 1
elif $ONCONSOLE ; then
	: I/O is already redirected to /dev/console, so we'll see it...
else
	# starting on an xterm....
	if [ -r $HOME/bin/xerrors ] ; then
		xterm -geometry 80x12-4-4 -cn -rw -sb -si -sk -sl 200 -e xerrors &
	else
		# TODO:  grrr....  this needs to do a 'mesg n' too somehow...
		xterm -geometry 80x12-4-4 -cn -rw -sb -si -sk -sl 200 -T "Xsession Errors" -e tail -200f $HOME/.xsession-errors &
	fi
	if [ -x $X11BIN/xsysstats ] ; then
		xsysstats -geometry 120x137-205+4 -type load1 -type disk -sample 5 &
		case "$HOME" in
		*/most/*)
			xsysstats -geometry 120x137-205+150 -type load1 -host most -type disk -host most -sample 5 &
			;;
		*/kuma/*)
			xsysstats -geometry 120x137-205+150 -type load1 -host kuma -type disk -host kuma -sample 5 &
			;;
		esac
	else
		# it's in the MIT X11R5 distribution....
		xload -geometry 120x137-205+4 -hl grey90 &
		case "$HOME" in
		*/most/*)
			$RSH -n most.weird.com exec $X11BIN/xload -display $DISPLAY -geometry 120x137-205+150 -hl grey90 &
			;;
		*/kuma/*)
			$RSH -n kuma.web.net exec $X11BIN/xload -display $DISPLAY -geometry 120x137-205+150 -hl grey90 &
			;;
		esac
	fi
fi

if [ -x $X11BIN/xmailwatcher ] ; then
	case "$HOME" in
	*/kuma/*)
		xmailwatcher -geometry 300x100-4+240 -iconic -icongeometry -4+240 -iconic -mailbox $MAILPATH &
		# WARNING: assumes MAILPATH is same on server....
		$RSH -n kuma.web.net exec $X11BIN/xmailwatcher -display $DISPLAY -geometry 300x100-4+300 -iconic -icongeometry -4+300 -iconic -mailbox $MAILPATH &
		;;
	*/most/*)
		# WARNING: assumes MAILPATH is same on server....
		$RSH -n most.weird.com exec $X11BIN/xmailwatcher -display $DISPLAY -geometry 300x100-4+235 -iconic -icongeometry -4+235 -iconic -mailbox $MAILPATH &
		;;
	esac
else                          # think about using xlbiff too...
	case "$HOME" in
	*/most/*)
		$RSH -n most.weird.com exec $X11BIN/xbiff -display $DISPLAY -geometry 48x48-4+4 -title Most-biff &
		;;
	*/kuma/*)
		$RSH -n kuma.web.net exec $X11BIN/xbiff -display $DISPLAY -geometry 48x48-4+4 -title Kuma-biff &
		;;
	*)
		xbiff -geometry 48x48-4+4 -bw 0 &
		;;
	esac
fi

sleep 1

# Ah!  Your Free localhost Window!
xterm -geometry 80x24-4-195 -sl 500 -sb -sk -si -rw  -ls -T Primary &

# when the window manager exits so shall we...
#
exec twm
