#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	20.4	98/08/04 00:50:24 (woods)"

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
	else
		echo "$argv0: WARNING: display was $DISPLAY but /dev/console not writable by $USER!"
		ls -l /dev/console
	fi
	DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}:0.0"
	export DISPLAY
	echo "$argv0: have set DISPLAY='$DISPLAY'"
	;;
esac

# may be things that depend on DISPLAY in here, so source again....
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile
fi

VENDORSTR="`xdpyinfo | sed -n 's/^vendor string:[ ]*\(.*\)$/\1/p'`"
DEPTHSTR="`xdpyinfo | fgrep 'depth of root window'`"
SCREENDEPTH=`expr "$DEPTHSTR" : '[ ]*depth of root window:[ ]*\([0-9]*\) '`
export VENDORSTR SCREENDEPTH
unset DEPTHSTR

# TODO:  think about using `type` ala .profile, but type isn't everywhere
if [ -z "$RSH" ] ; then
	if [ -x /usr/ucb/rsh ] ; then
		RSH=/usr/ucb/rsh
	else				# TODO: also remsh?
		RSH=/usr/bin/rsh
	fi
	export RSH			# used by .twmrc as well as below
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
		# is attached and no way to revert unknown earlier changes.
		case "$XDMHOST" in
		*weird.com)
			# may have been done by xdm setup script....
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
		case "$XDMHOST" in
		*weird.com)
			# should have been done by xdm setup script....
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

if [ -z "$ONX11SERVER" ] ; then
	# If you're running on a diskless workstation that runs xdm itself
	# then you probably have a server that's more powerful and on which
	# you'd rather run the majority of your X applications.  Normally
	# this will be the same server your home directory resides on....
	#
	if [ "$XDMHOST" = `echo $DISPLAY | sed 's/:.*$//'` ] ; then
		case "$XDMHOST" in
		some.diskless.workstation)
			X11SERVER="some.more.powerful.host"
			ONX11SERVER="$RSH -n $X11SERVER exec"
			export X11SERVER ONX11SERVER
			echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
			# now start special programs we like for this scenario
			$ONX11SERVER $X11BIN/xbuffy -mail /var/mail/$LOGNAME -display $DISPLAY -geometry 90x25-100+235 -shortnames -header 0 &
			;;
		almost.weird.com)
			X11SERVER="most.weird.com"
			ONX11SERVER="$RSH -n $X11SERVER exec"
			export X11SERVER ONX11SERVER
			echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
			# now start special programs we like for this scenario
			$ONX11SERVER $X11BIN/xbuffy -mail /var/mail/$LOGNAME -display $DISPLAY -geometry 90x25-100+235 -shortnames -header 0 &
			;;
		very.weird.com)
			X11SERVER="most.weird.com"
			ONX11SERVER="$RSH -n $X11SERVER exec"
			export X11SERVER ONX11SERVER
			echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
			# now start special programs we like for this scenario
			$ONX11SERVER $X11BIN/xbuffy -mail /var/mail/$LOGNAME -display $DISPLAY -geometry 90x25-100+235 -shortnames -header 0 -acmd '"/usr/X11/bin/auplay -audio '$AUDIOSERVER' '$HOME'/lib/sounds/newmail"' &
			;;
		*)
			# not setting this just makes WM run stuff on the
			# current login host....
			unset X11SERVER ONX11SERVER
			echo "$argv0: have unset X11SERVER and ONX11SERVER; XDMHOST is $XDMHOST"
			;;
		esac
	fi
fi

# TODO:  think about using `type` ala .profile, but type isn't everywhere
if [ -z "$WM" ] ; then
	if [ -x $X11BIN/xsm -a -r $HOME/.xsmstartup ] ; then
		WM=xsm
	elif [ -x $X11BIN/xsession -a -r $HOME/.X11Startup ] ; then
		WM=xsession	# TODO: '$ONX11SERVER xsession -remote'?
	elif [ \( -x $X11BIN/ctwm -o -x $HOME/bin/ctwm \) -a -r $HOME/.ctwmrc ] ; then
		if [ $SCREENDEPTH -eq 1 ] ; then
			WM="ctwm -W"
		else
			WM=ctwm
		fi
	else
		WM=twm					# assumptions....
	fi
	export WM
fi

# argv0 is .xinitrc if called by xinit or startx
# TODO:  always do, but only if we're not on the console....
#
if [ "$argv0" = .xinitrc ] ; then
	xconsole -geometry 500x160-0-0 &
	# wait to ensure xconsole is running to display errors....
	sleep 1
	if [ -x $X11BIN/xearth ] ; then
		XEGAMMA=""
		if [ $SCREENDEPTH -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"	# for Sun3 CG2
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
		unset XEGAMMA
	elif [ -x $X11BIN/xphoon ] ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	elif [ -r /usr/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
	elif [ -r /usr/openwin/share/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
	fi
	xclock -geometry 190x30-0+196 -digital &
	# wait to ensure they get overlayed in the right order
	sleep 1
	xclock -geometry 190x190-0+0 &
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
	if [ -r $HOME/.xsession-errors-$DISPLAY ] ; then
		xterm -geometry 60x12-0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 500 -ut -T "Xsession Messages for '$DISPLAY'" -e sh -c "mesg n; exec tail -200f $HOME/.xsession-errors-$DISPLAY" &
	else
		echo "$argv0: didn't find .xsession-errors-$DISPLAY"
		xterm -geometry 60x12-0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 500 -ut -T "Xsession Messages" -e sh -c "mesg n; exec tail -200f $HOME/.xsession-errors" &
	fi
fi

if [ -x $X11BIN/xbuffy ] ; then
	if [ -n "$AUDIOPLAYER" -a -r $HOME/lib/sounds/newmail ] ; then
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 -acmd "$AUDIOPLAYER $HOME/lib/sounds/newmail" &
	else
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 &
	fi
else
	xbiff -geometry 48x48-0+235 -bw 0 &
fi

# when the window manager exits so shall we...
#
exec $WM

# Local Variables:
# eval: (defun save-this-file-and-make-it-executable () (write-region (point-min) (point-max) buffer-file-name nil 't) (set-file-modes buffer-file-name 493) nil)
# local-write-file-hooks: (save-this-file-and-make-it-executable)
# End:
