#! /bin/sh
:
#
#	.xinitrc - X Window System startup file
#
#ident        "@(#)HOME:.xinitrc     5.6     95/03/19 11:46:44 (woods)"

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="`basename $0`"

. /etc/profile
. $HOME/.profile

echo "$0: current PATH='$PATH'"
echo "$argv0: starting on $DISPLAY...."

case "$DISPLAY" in
":0" | ":0.0" | "" )
	# TODO: need to check existence of domainname and results
	X11HOST="`hostname``domainname`"	# used by .twmrc
	DISPLAY="$X11HOST:0.0"
	;;
esac
export DISPLAY X11HOST

echo "$argv0: have set DISPLAY='$DISPLAY'"

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

if [ "$argv0" = .xinitrc ] ; then
	if [ -x /usr/bin/X11/xearth ] ; then
		xearth &
	elif [ -x /usr/bin/X11/xphoon ] ; then
		xphoon -t 10 &
	elif [ -r /usr/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
	elif [ -r /usr/openwin/share/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
	fi
fi

# if we're on the console,
# TODO: and only if it's not already running courtesy of xdm....
#
if [ "$argv0" = ".xinitrc" ] ; then
	xconsole -geometry 500x100-4-4 &
	sleep 1

	xclock -geometry 190x30-4+196 -digital &
	# ensure they get overlayed in the right order
	sleep 1
	xclock -geometry 190x190-4+4 &
fi

if [ -x /usr/bin/X11/xmailwatcher ] ; then
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

if [ -x /usr/bin/X11/xsysstats ] ; then
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
twm
