#! /bin/sh
:
#
#	.xinitrc - X Window System startup file
#
#ident	"@(#)HOME:.xinitrc	3.1	94/03/25 18:34:31 (woods)"

PATH=$PATH:$HOME/bin
export PATH

argv0="`basename $0`"

echo "$argv0: starting on $DISPLAY...."

case "$DISPLAY" in
":0" | ":0.0" | "" )
	DISPLAY="`hostname``domainname`:0.0"
	;;
esac
export DISPLAY

echo "$argv0: setting DISPLAY='$DISPLAY'"

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

xhost kuma araignee

if [ "$argv0" = .xinitrc ] ; then
	#xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
	xphoon -t 10 &
fi

if [ "$argv0" = .xinitrc -a "$TTY" = /dev/console ] ; then
	# if we're on the console,
	# and only if it's not already running courtesy of xdm....
	xconsole -geometry 500x80-4-350 &
fi

xclock -geometry 190x30-4+196 -digital &
# ensure they get overlayed in the right order
sleep 1
xclock -geometry 190x190-4+4 &

rsh -n kuma /usr/openwin/bin/perfmeter -geometry 130x640-205+4 -display $DISPLAY kuma.web.net &
rsh -n kuma /usr/openwin/bin/perfmeter -geometry 130x640-340+4 -display $DISPLAY tar.web.net &

xmailwatcher -geometry 300x100-4+240 -iconic -icongeometry -140+240 -iconic -mailbox /var/spool/mail/$LOGNAME &
xmailwatcher -geometry 300x100-4+300 -iconic -icongeometry -140+300 -iconic -mailbox /kuma/big/var/spool/mail/$LOGNAME &

# Grrr.... xsol doesn't take standard toolkit args....
#xsol -geometry 32x48-4+260 -iconic &

#if [ -x /usr/bin/X11/xsysstats ] ; then
#	xsysstats -geometry 120x137-205+4 -type load1 -type disk -sample 5 &
#	xsysstats -geometry 120x137-205+150 -type load1 -host kuma -type disk -host kuma -sample 5 &
#else
#	xload -geometry 120x137-205+4 -hl grey90 &
#	rsh -n kuma /usr/openwin/bin/xload -geometry 120x137-205+150 -jumpscroll 1 -hl grey90 -display $DISPLAY &
#fi

sleep 1

xterm -geometry 80x24-4+360 -sl 500 -sb -sk -si -rw  -ls -T Primary &

# when the window manager exits so shall we...
twm
