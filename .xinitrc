#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	18.6	97/11/16 17:42:50 (woods)"

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="`basename $0`"
echo "$argv0: running from $0"
XOPATH=$PATH
echo "$argv0: initial PATH='$PATH'"
XODISPLAY=$DISPLAY
echo "$argv0: starting up with DISPLAY='$DISPLAY'"

if [ -r /etc/profile ] ; then
	. /etc/profile
else
	echo "$argv0: no /etc/profile on this host!"
fi
if [ -r $HOME/.profile ] ; then
	. $HOME/.profile
else
	echo "$argv0: no $HOME/.profile on this host!"
fi

if [ "$XOPATH" != "$PATH" ] ; then
	echo "$argv0: adjusted PATH='$PATH'"
fi
unset XOPATH
if [ "$XODISPLAY" != "$DISPLAY" ] ; then
	echo "$argv0: adjusted DISPLAY='$DISPLAY'"
fi
unset XODISPLAY

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
		echo "$argv0: redirecting stdout and stderr to /dev/console."
		exec 1>/dev/console 2>&1
	fi
	DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}:0.0"
	export DISPLAY
	echo "$argv0: have set DISPLAY='$DISPLAY'"
	;;
esac

VENDORSTR="`xdpyinfo | sed -n 's/^vendor string:[ ]*\(.*\)$/\1/p'`"
export VENDORSTR

# TODO:  think about using `type` ala .profile, but type isn't everywhere
if [ -z "$RSH" ] ; then
	if [ -x /usr/ucb/rsh ] ; then
		RSH=/usr/ucb/rsh
	else				# TODO: also remsh?
		RSH=/usr/bin/rsh
	fi
	export RSH			# used by .twmrc as well as below
fi

if [ -z "$ONX11SERVER" ] ; then
	# If you're running on a diskless workstation that runs xdm itself
	# then you probably have a server that's more powerful and on which
	# you'd rather run the majority of your X applications.  Normally
	# this will be the same server your home directory resides on....
	#
	if [ "$XDMHOST" = `echo $DISPLAY | sed 's/:.*$//'` ] ; then
		case "$XDMHOST" in
		*weird.com)
			ONX11SERVER="$RSH -n most.weird.com exec"
			export ONX11SERVER
			echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
			;;
		*planix.com)
			ONX11SERVER="$RSH -n woffi.weird.com exec"
			export ONX11SERVER
			echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
			;;
		*)
			# not setting this just makes twm run stuff on the
			# current login/display host....
			unset ONX11SERVER
			echo "$argv0: have unset ONX11SERVER"
			;;
		esac
	fi
fi

# TODO:  think about using `type` ala .profile, but type isn't everywhere
if [ -z "$WM" ] ; then
	if [ -x $X11BIN/xsession -a -r $HOME/.X11Startup ] ; then
		WM=xsession	# TODO: '$ONX11SERVER xsession -remote'?
	elif [ -x $X11BIN/ctwm -a -r $HOME/.ctwmrc ] ; then
		WM=ctwm
	else
		WM=twm					# assumptions....
	fi
	export WM
fi

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

case "$VENDORSTR" in
"Network Computing Devices"*)
	if [ -r $HOME/.Xmodmap-ncd-108 ] ; then
		# Unfortunately there is no real way to tell what type of keyboard
		# is attached....
		case "$UUNAME" in
		most|very|almost|pretty|sometimes|always)
			# may be done by xdm setup script....
			;;
		*)
			xmodmap $HOME/.Xmodmap-ncd-108
			;;
		esac
	fi
	;;
*)
	if [ -r $HOME/.Xmodmap ] ; then
		# Unfortunately there is no real way to tell what type of keyboard
		# is attached.... this is primarily to fix dumb PC(-like) keyboards
		case "$UUNAME" in
		most|very|almost|pretty|sometimes|always)
			;;
		*)
			xmodmap $HOME/.Xmodmap	# Load user's keymap
			;;
		esac
	fi
	;;
esac

# TODO:  this might be a mistake, but it might also save your bacon!
#
xhost +`hostname` +localhost -

# TODO:  What we really need to do is to setup and use Xauth properly.
# I think this means we want to find our new "cookie" and then in some
# fairly secure way we run 'xauth add $COOKIE' on each host we'll
# permit connections from.  Note that some applications (eg. emacs
# ver-19.34?) seem to require xhost in any case.
#
if [ -r $HOME/.xhosts ] ; then
	while read XHOST junk; do
		case "$XHOST" in
		"" | '#'* )
			;;
		* )
			# NEVER remove the "" from the first clause!!!
			xhost +$XHOST
			;;
		esac
	done < $HOME/.xhosts
else
	echo "$argv0: no $HOME/.xhosts found..."
fi

# argv0 is .xinitrc if called by xinit or startx
# TODO:  always do, but only if we're not on the console....
#
if [ "$argv0" = .xinitrc ] ; then
	xconsole -geometry 500x160-4-4 &
	# wait to ensure xconsole is running to display errors....
	sleep 1
	if [ -x $X11BIN/xearth ] ; then
		XEGAMMA=""
		DEPTHSTR="`xdpyinfo | fgrep 'depth of root window'`"
		if [ `expr "$DEPTHSTR" : '[ ]*depth of root window:[ ]*\([0-9]*\) '` -ne 1 ] ; then
			case "$VENDORSTR" in
			"Network Computing Devices"*)
				XEGAMMA="-gamma 1.3"		# for NCDhmx with Samsung SyncMaster 6c
				;;
			"MIT X Consortium"*)
				XEGAMMA="-gamma 1.6 -night 15"	# for Sun3 CG2
				;;
			esac
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
		unset DEPTHSTR XEGAMMA
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
	# wait to ensure they get overlayed in the right order
	sleep 1
	xclock -geometry 190x190-4+4 &
	sleep 1
elif $ONCONSOLE ; then
	#
	#	I/O is already redirected to /dev/console, so we will see it...
	#
	:
else
	#
	#	probably starting on an independent xterm....
	#
	if [ -r $HOME/bin/xerrors ] ; then
		xterm -geometry 80x12-4-4 -cn -rw -sb -si -sk -sl 200 -e xerrors &
	else
		# TODO:  grrr....  this needs to do a 'mesg n' too somehow...
		xterm -geometry 80x12-4-4 -cn -rw -sb -si -sk -sl 200 -T "Xsession Errors" -e tail -200f $HOME/.xsession-errors &
	fi
	if [ -n "$ONX11SERVER" ] ; then
		if [ -x $X11BIN/xsysstats ] ; then
			# this is normally done in Xsetyp by xdm, and if not there's a menu
			#xsysstats -geometry 120x137-205+4 -type load1 -type disk -sample 5 &
			$ONX11SERVER xsysstats -geometry 120x137-205+150 -type load1 -host most -type disk -host $XDMHOST -sample 5 &
		else
			# xload is in the MIT X11R5 distribution, so default to it....
			# this is normally done in Xsetyp by xdm, and if not there's a menu
			#xload -geometry 120x137-205+4 -hl grey90 &
			$ONX11SERVER $X11BIN/xload -display $DISPLAY -geometry 120x137-205+150 -hl grey90 &
		fi
	fi
fi

if [ -x $X11BIN/xmailwatcher ] ; then
	# TODO:  this is still a silly way to do things....
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
	*)
		xmailwatcher -display $DISPLAY -geometry 300x100-4+235 -iconic -icongeometry -4+235 -iconic &
		;;
	esac
elif [ -x $X11BIN/xbuffy ] ; then
	# TODO:  and this is only slightly better
	case "$DOMAINNAME" in
	*seachange.com)
		$RSH -n clapton.seachange.com /usr/local/bin/xbuffy -display $DISPLAY -geometry 90x30-4+260 -mail /var/mail/woods -shortnames -header 5 &
		;;
	*weird.com)
		$RSH -n most.weird.com $X11BIN/xbuffy -display $DISPLAY -geometry 90x30-4+260 -shortnames -header 5 -mail $MAILPATH &
		;;
	*)
		xbuffy -display $DISPLAY -geometry 90x30-4+260 -shortnames -header 5 &
		;;
	esac
else
	# TODO:  as above, this is still a silly way to do things....
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

# when the window manager exits so shall we...
#
exec $WM
