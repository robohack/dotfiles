#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	27.1	03/11/23 19:15:56 (woods)"

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

ONCONSOLE=false; export ONCONSOLE
case "$DISPLAY" in
":0"|":0.0"|"")
	# don't bother with this unless it'll do us some good...
	if [ -w /dev/console ] ; then
		ONCONSOLE=true; export ONCONSOLE
		echo "$argv0: redirecting stdout and stderr to /dev/console."
		exec 1>/dev/console 2>&1
		echo "$argv0: redirected stdout and stderr to /dev/console."
	else
		echo "$argv0: WARNING: display was $DISPLAY but /dev/console not writable by $USER!"
		ls -l /dev/console
	fi
	DISPLAY="${XDMHOST:-${HOSTNAME}${DOMAINNAME}}:0.0"
	echo "$argv0: have set DISPLAY='$DISPLAY'"
	;;
esac
if [ "$XODISPLAY" != "$DISPLAY" ] ; then
	echo "$argv0: adjusted DISPLAY='$DISPLAY'"
fi
unset XODISPLAY
export DISPLAY

# may be things that depend on DISPLAY in here, so source again....
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile < /dev/null
fi

# if this still isn't set then we probably didn't have a ~/.profile!
#
if [ -z "$X11PATH" ] ; then
	# FIXME: this won't work very well if X11R? is multiple names....
	if [ -d /local/X11R? -a ! -h /local/X11R? ] ; then
		X11PATH="`echo /local/X11R?`"
	elif [ -d /usr/X11 -a ! -h /usr/X11 ] ; then
		X11PATH="/usr/X11"
	elif [ -d /usr/X11R? -a ! -h /usr/X11? ] ; then
		X11PATH="`echo /usr/X11R?`"
	elif [ -d /usr/X??? -a ! -h /usr/X??? ] ; then	# X386, for example
		X11PATH="`echo /usr/X???`"
	elif [ -d /usr/local/X11R? -a ! -h /usr/local/X11R? ] ; then
		X11PATH="`echo /usr/local/X11R?`"
	else
		X11PATH="/NO-X11-FOUND"
		echo "$argv0: X11PATH not found!"
	fi
	export X11PATH
fi

# if this still isn't set then we probably didn't have a ~/.profile!
#
if [ -z "$X11BIN" ] ; then
	# TODO: this is a best guess that might fail for remote hosts
	# note that "test -L" is POSIX, but old systems had "-h"
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
	if [ -x /usr/ucb/rsh ] ; then
		RSH=/usr/ucb/rsh
	else				# TODO: also remsh?
		RSH=/usr/bin/rsh
	fi
	export RSH			# used by .twmrc, .ctwmrc, as well as below
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
*)
	if [ -r $HOME/.Xmodmap ] ; then
		echo "$argv0: setting default keyboard map from $HOME/.Xmodmap"
		xmodmap $HOME/.Xmodmap	# Load user's default keymap
	else
		echo "$argv0: $HOME/.Xmodmap not found!"
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
fi

# If you're running on a diskless workstation that runs xdm itself
# then you probably have a server that's more powerful and on which
# you'd rather run the majority of your X applications.  Normally
# this will be the same server your home directory resides on....
#
if [ "$XDMHOST" = `echo $DISPLAY | sed 's/:.*$//'` ] ; then
	case "$XDMHOST" in
	almost.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		ONX11SERVER="$RSH -n $X11SERVER exec"
		export X11SERVER ONX11SERVER
		echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
		# now start server programs we like for this scenario
		$ONX11SERVER $X11BIN/xload -display $DISPLAY -geometry 120x50-200+58 -hl grey90 &
		;;
	very.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		ONX11SERVER="$RSH -n $X11SERVER exec"
		export X11SERVER ONX11SERVER
		echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
		# now start server programs we like for this scenario
		$ONX11SERVER $X11BIN/xload -display $DISPLAY -geometry 120x50-200+58 -hl grey90 &
		$ONX11SERVER $X11BIN/xterm -display $DISPLAY -geometry 176x96+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
###		$ONX11SERVER $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
		;;
	*)
		# not setting ONX11SERVER just makes the WM run clients on
		# the current login host....
		unset X11SERVER ONX11SERVER
		echo "$argv0: have unset X11SERVER and ONX11SERVER; XDMHOST is $XDMHOST"
		;;
	esac
fi

# TODO:  think about using `type` ala .profile, but type isn't in (a)sh, etc.
if [ -z "$WM" ] ; then
	#
	# XXX look for a window-manager-cooser
	#
	if [ \( -x $X11BIN/ctwm -o -x $HOME/bin/ctwm \) -a $HOME/.ctwmrc ] ; then
		# NOTE: don't bother with '-v' -- it's too much and slows things!
		if [ $SCREENDEPTH -eq 1 ] ; then
			# the welcome screen on monochrome is silly and slow
			WM="ctwm -k -display $DISPLAY -W"
		else
			WM="ctwm -k -display $DISPLAY"
		fi
	elif [ -x $X11BIN/tvtwm -o -x $HOME/bin/tvtwm ] ; then
		WM=tvtwm
	elif [ -x $X11BIN/wmx -o -x $HOME/bin/wmx ] ; then
		WM=wmx
	elif [ -x $X11BIN/wm2 -o -x $HOME/bin/wm2 ] ; then
		WM=wm2
	elif [ -x $X11BIN/scwm -o -x $HOME/bin/scwm ] ; then
		WM=scwm
	elif [ \( -x $X11BIN/wmaker -o -x $HOME/bin/wmaker \) -a $HOME/GNUstep/Defaults/WindowMaker ] ; then
		WM=wmaker
	elif [ -x $X11BIN/fvwm -o -x $HOME/bin/fvwm ] ; then
		WM=fvwm
	else
		echo "$argv0: assuming 'twm' is available..."
		WM=twm					# silly assumption?
	fi
	export WM
fi

HAVESWISSWATCH=false ; export HAVESWISSWATCH
if expr "`type swisswatch`" : '.* is .*/swisswatch$' >/dev/null 2>&1 ; then
	HAVESWISSWATCH=true
fi
HAVEXEARTH=false ; export HAVEXEARTH
if expr "`type xearth`" : '.* is .*/xearth$' >/dev/null 2>&1 ; then
	HAVEXEARTH=true
fi
HAVEXPHOON=false ; export HAVEXPHOON
if expr "`type xphoon`" : '.* is .*/xphoon$' >/dev/null 2>&1 ; then
	HAVEXPHOON=true
fi
HAVEXBUFFY=false ; export HAVEXBUFFY
if expr "`type xbuffy`" : '.* is .*/xbuffy$' >/dev/null 2>&1 ; then
	HAVEXBUFFY=true
fi

# argv0 is .xinitrc if called by xinit or startx
#
if [ "$argv0" = ".xinitrc" ] ; then
	xconsole -geometry 500x130-0-0 -notify -verbose &
	# wait a wee bit in hopes xconsole gets running to display any errors....
	sleep 1
	case $DISPLAY in
	very.weird.com:*)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		ONX11SERVER="$RSH -n $X11SERVER exec"
		export X11SERVER ONX11SERVER
		echo "$argv0: have set ONX11SERVER='$ONX11SERVER'"
		$ONX11SERVER $X11BIN/xload -display $DISPLAY -geometry 120x50-200+58 -hl grey90 &
		$ONX11SERVER $X11BIN/xterm -display $DISPLAY -geometry 176x96+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
		$ONX11SERVER $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
		# for the "Main" workspace...
		xterm -geometry 176x96+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
		;;
	esac
	if $HAVEXEARTH ; then
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
	elif [ -r /usr/openwin/share/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap /usr/openwin/share/include/X11/bitmaps/escherknot
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
		xconsole -geometry 500x130-0-0 -notify -verbose -file $HOME/.xsession-errors-$DISPLAY &
	else
		echo "$argv0: didn't find .xsession-errors-$DISPLAY"
		xconsole -geometry 500x130-0-0 -notify -verbose -file $HOME/.xsession-errors &
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

case $DISPLAY in

very.weird.com:*)
	# this is a large, hi-res, display with an optical mouse....
	xset m 10 5
	# local server performance levels we want to be aware of
	$RSH -n proven.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x50-200+116 -hl grey90 &
	$RSH -n starting-out.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x50-200+174 -hl grey90 &
	$RSH -n most.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x50-200+232 -hl grey90 &
##	# mailbox monitors....
###	$RSH -n mail.weird.com $X11BIN/xbuffy -mail /var/mail/$LOGNAME -display $DISPLAY -geometry 90x25-100+235 -shortnames -header 0 -acmd '"/usr/X11/bin/auplay -audio '$AUDIOSERVER' -v 25 '$HOME'/lib/sounds/newmail"' &
###	$RSH -n mail.weird.com $X11BIN/xbuffy -mail /var/mail/$LOGNAME -display $DISPLAY -geometry 90x25-100+235 -shortnames -header 0 -acmd '"/usr/X11/bin/auplay -audio tcp/xtremely.weird.com:8000 -v 25 '$HOME'/lib/sounds/newmail"' &
	# Additional BIG windows for the "Main" workspace
	$RSH -n proven.weird.com $X11BIN/xterm -display $DISPLAY -geometry 176x96+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
	# All the small lower-right command terminals (starting with our own first one)
	xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n proven.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n proven.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n starting-out.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n starting-out.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n becoming.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	$RSH -n sometimes.weird.com $X11BIN/xterm -display $DISPLAY -geometry 80x42-0+530 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	# oddly using 130x64 turns into 163x55 with the font change!
	# this one's supposed to go in virtualscreen "Mail"
###	$RSH -n proven.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/local/bin/emacs -display $DISPLAY -geometry 130x75+0+0" &
	# this one's supposed to go in virtualscreen "Coding"
###	$RSH -n proven.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/local/bin/emacs -i -display $DISPLAY -geometry 130x75+0+0" &
	;;

xtremely.weird.com:*)
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
	xload -geometry 190x60-0+233 -hl red &
	xclock -geometry 190x30-0+196 -digital &
	$RSH -n proven.weird.com exec /usr/X11R6/bin/xload -display $DISPLAY -geometry 120x50-198+243 -hl red &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -geometry 190x190-0+0 &
	fi
	xterm -display $DISPLAY -geometry 80x42-0-0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
###	$RSH -n proven.weird.com exec env PATH=$PATH DISPLAY=$DISPLAY /usr/pkg/bin/MozillaFirebird -install -display $DISPLAY -geometry 935x990+0+0 &

	# first column....
	xterm -display $DISPLAY -geometry 88x24+0+0   -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn07 -T "Console-07 [isit.weird.com]" -e console -S isit &
	xterm -display $DISPLAY -geometry 88x24+0+205 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn06 -T "Console-06 [proven.weird.com]" -e console -S proven &
	xterm -display $DISPLAY -geometry 88x24+0+420 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn08 -T "Console-08 [becoming.weird.com]" -e console -S becoming &
	xterm -display $DISPLAY -geometry 88x24+0+615 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n oCn1 -T "oCons-1 [DEChub 900MS] (hubly)" -e console -S hubly &
	xterm -display $DISPLAY -geometry 88x24+0+820 -fn 5x7 -cn -rw -sb -si -sk -sl 100  -ls -ziconbeep 1 -n oCn1 -T "Console-01 [Exabyte-210 library]" -e console -S exb210 &

	# second column....
	xterm -display $DISPLAY -geometry 88x24+475+0   -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn10 -T "Console-10 [most.weird.com]" -e console -S most &
	xterm -display $DISPLAY -geometry 88x24+475+205 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn03 -T "Console-03 [sometimes.weird.com]" -e console -S sometimes &
	xterm -display $DISPLAY -geometry 88x24+475+420 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn09 -T "Console-09 [always.weird.com]" -e console -S building &
	xterm -display $DISPLAY -geometry 88x24+475+615 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn11 -T "Console-11 [starting-out.weird.com]" -e console -S starting-out &
	xterm -display $DISPLAY -geometry 88x24+475+820 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n cid -T "very:/dev/ttyb [Caller ID Box]" -e console -S callerid &

	# right side...
	xterm -display $DISPLAY -geometry 80x24-0-420 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn02 -T "Console-02 [BEST-1.4-UPS]" -e console -S best-1.4 &
	xterm -display $DISPLAY -geometry 80x24-0-420 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn13 -T "Console-13 [BEST-3.1-UPS-0]" -e console -S best-3.1-0 &
	xterm -display $DISPLAY -geometry 80x24-0-420 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n Cn14 -T "Console-14 [BEST-3.1-UPS-1]" -e console -S best-3.1-1 &
	xterm -display $DISPLAY -geometry 80x24-0-205 -fn 5x7 -cn -rw -sb -si -sk -sl 100 -ls -ziconbeep 1 -n Cn04 -T "Console-04 [RAID-00 /work]" -e console -S raid-00 &
	xterm -display $DISPLAY -geometry 80x24-0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 100 -ls -ziconbeep 1 -n Cn05 -T "Console-05 [RAID-01 /cvs]" -e console -S raid-01 &

	# start xv first, in a subshell that sleeps first for a bit
	# -- it needs to be running before we try our tkined trick, but after the window manager
	( sleep 20; xv -owncm -iconic & ) &
	# start tkined last, in a subshell that sleeps first for a bit
	# -- hoping xv (from above) and the window manager are running already
	( sleep 40; $RSH -n proven.weird.com exec /usr/local/bin/tkined1.5.0 -display $DISPLAY -use $(xwininfo -name "xv 3.10a(PNG) <unregistered>" | awk '/xwininfo/ {print $4}') -geometry 1200x925+0-0 /home/proven/woods/tkined-3.tki & ) &
	;;

worm.planix.com:*)
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
	xload -geometry 190x60-0+233 -hl red &
	xclock -geometry 190x30-0+196 -digital &
	if $HAVESWISSWATCH ; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -geometry 190x190-0+0 &
	fi
	xterm -display $DISPLAY -geometry 101x66+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	xterm -display $DISPLAY -geometry 80x42-0-0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	mozilla -display $DISPLAY -SelectProfile &
	;;
*)
	# make everything rather small so that enough will fit on even
	# tiny screens....
	#
	if [ $SCREENDEPTH -eq 1 ] ; then
		xload -geometry 100x50-0+150 -hl grey90 &
	else
		xload -geometry 100x50-0+150 -hl red &
	fi
	xclock -geometry 100x30-0+110 -digital &
	if $HAVESWISSWATCH ; then
		swisswatch -name swissclock -geometry 100x100-0+0 &
	else
		xclock -geometry 100x100-0+0 &
	fi
	xterm -display $DISPLAY -geometry 80x30+0+0 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	xterm -display $DISPLAY -geometry 80x24-0-0 -fn 5x7 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	;;
esac


# when the window manager exits so shall we...
#
exec $WM

# Local Variables:
# eval: (defun make-buffer-file-executable () (let* ((current-mode (file-modes (buffer-file-name))) (add-mode (logand ?\111 (default-file-modes)))) (or (/= (logand ?\111 current-mode) 0) (zerop add-mode) (set-file-modes (buffer-file-name) (logior current-mode add-mode)))))
# vc-checkout-hook: (make-buffer-file-executable)
# vc-checkin-hook: (make-buffer-file-executable)
# after-save-hook: (make-buffer-file-executable)
# End:
