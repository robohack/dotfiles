#! /bin/sh
:
#
#	.xinitrc - X Window System startup file
#
#ident        "@(#)HOME:.xinitrc     6.4     95/11/30 11:06:33 (woods)"

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="`basename $0`"

. /etc/profile
. $HOME/.profile

echo "$0: current PATH='$PATH'"
echo "$argv0: starting on $DISPLAY...."

if [ -z "$X11HOST" ] ; then
	# assume HOSTNAME & DOMAINNAME are set by profile(s)
	X11HOST="${X11HOST:-${HOSTNAME}${DOMAINNAME}}"	# used by .twmrc
	echo "$argv0: have set X11HOST='$X11HOST'"
fi
case "$DISPLAY" in
":0" | ":0.0" | "" )
	DISPLAY="$X11HOST:0.0"
	echo "$argv0: have set DISPLAY='$DISPLAY'"
	;;
esac
export DISPLAY X11HOST

# TODO: think about using `type` ala .profile
if [ -x /usr/ucb/rsh ] ; then
	RSH=/usr/ucb/rsh
else
	RSH=/usr/bin/rsh
fi
export RSH				# used by .twmrc

if [ -z "$LOGNAME" ] ; then
	LOGNAME="$USER"
	export LOGNAME
fi

if [ -f $HOME/.Xdefaults ]; then
	xrdb $HOME/.Xdefaults		# Load Users X11 resource database
elif [ -f "/usr/lib/X11/Xdefaults" ] ; then
	xrdb /usr/lib/X11/Xdefaults	# Load Default X11 resource database
elif [ -f "$X11HOME/lib/X11/Xdefaults" ] ; then
	xrdb $X11HOME/lib/X11/Xdefaults	# Load Default X11 resource database
elif [ -f "$OPENWINHOME/lib/Xdefaults" ] ; then
	xrdb $OPENWINHOME/lib/Xdefaults	# Load Default X11 resource database
fi

# TODO: this might be a mistake, but it might also save your bacon!
#
xhost `hostname` localhost -

if [ -s $HOME/.xhosts ] ; then
	while read HOST junk; do
		case "$HOST" in
		"" | '#'* )
			;;
		* )
			if [ -n "$HOST" ] ; then
				xhost +$HOST			
			fi
			;;
		esac
	done < $HOME/.xhosts
fi

# argv0 is .xinitrc if called by xinit or startx
#
if [ "$argv0" = .xinitrc ] ; then
	if [ -x $X11BIN/xearth ] ; then
		DEPTHSTR="`xdpyinfo | fgrep 'depth of root window'`"
		if [ `expr "$DEPTHSTR" : '[ ]*depth of root window:[ ]*\([0-9]*\) '` -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"		# for Sun CG3
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif [ -x $X11BIN/xphoon ] ; then
		xphoon -t 10 &
	elif [ -r /usr/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
	elif [ -r /usr/openwin/share/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
	fi
fi

# TODO: always only if we're not on the console....
#
if [ "$argv0" = ".xinitrc" ] ; then
	xconsole -geometry 500x100-4-4 &
	sleep 1

	xclock -geometry 190x30-4+196 -digital &
	# ensure they get overlayed in the right order
	sleep 1
	xclock -geometry 190x190-4+4 &
else
	xterm -geometry 80x12-4-4 -cn -rw -sb -si -sk -sl 200 -e tail -f $HOME/.xsession-errors &
fi

if [ -x $X11BIN/xmailwatcher ] ; then
	xmailwatcher -geometry 300x100-4+240 -iconic -icongeometry -140+240 -iconic -mailbox $MAILPATH &
	# TODO; get rid of these host name dependencies....
	if [ -r /kuma/$MAILPATH ] ; then
		xmailwatcher -geometry 300x100-4+300 -iconic -icongeometry -140+300 -iconic -mailbox /kuma/$MAILPATH &
	fi
	if [ -r /most/$MAILPATH ] ; then
		xmailwatcher -geometry 300x100-4+300 -iconic -icongeometry -140+300 -iconic -mailbox /kuma/$MAILPATH &
	fi
fi

# Grrr.... xsol doesn't take standard toolkit args....
#xsol -geometry 32x48-4+260 -iconic &

if [ -x $X11BIN/xsysstats ] ; then
	xsysstats -geometry 120x137-205+4 -type load1 -type disk -sample 5 &
	case "$DOMAINNAME" in
	.web.net)
		xsysstats -geometry 120x137-205+150 -type load1 -host kuma -type disk -host kuma -sample 5 &
		;;
	esac
else
	# it's in the MIT distribution....
	xload -geometry 120x137-205+4 -hl grey90 &
fi

sleep 1

# Ah!  Your Free localhost Window!
xterm -geometry 80x24-4-195 -sl 500 -sb -sk -si -rw  -ls -T Primary &

# when the window manager exits so shall we...
#
exec twm
