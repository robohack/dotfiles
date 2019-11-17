#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	36.3	19/11/16 16:10:38 (woods)"

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

# XXX I should think harder about just how much of the environment I really do
# want all subsequent X11 applications to inherit, especially, maybe, the window
# manager.  For example some things that can be over-ridden in ~/.localprofile
# will not be reset by ~/.profile if they're exported via the environment here.

if [ "$XOPATH" != "$PATH" ] ; then
	echo "$argv0: adjusted PATH='$PATH'"
fi
unset XOPATH

if [ $argv0 = ".xsession" -a -z "$XDMHOST" ] ; then
	#
	# assume HOSTNAME & DOMAINNAME are set reasonably by the user
	# or system shell profile script(s) such that this combination
	# is useful both on the local host as well as on any servers
	# where X11 applications may run.
	#
	XDMHOST="${HOSTNAME}${DOMAINNAME}"
	export XDMHOST					# also used by .[c]twmrc
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
		# XXX
		# XXX hmmmm.... what can we do to direct our output here????
		# XXX (but maybe we don't need to with startx using the /var/log/system.log)
		# XXX
		tail -F /var/log/system.log | xconsole -geometry 500x120-0-0 -saveLines 2000 -notify -verbose -file /dev/stdin &
	elif [ -w /dev/console ] ; then
		ONCONSOLE=true
		echo "$argv0: redirecting stdout and stderr to /dev/console."
		exec 1>/dev/console 2>&1
		echo "$argv0: redirected stdout and stderr to /dev/console, starting xconsole"
		xconsole -geometry 500x120-0-0 -saveLines 2000 -notify -verbose &
		# wait a wee bit in hopes xconsole gets running to display any errors....
		sleep 1
	else
		echo "$argv0: WARNING: display was '$DISPLAY' but /dev/console is not writable by $USER!"
		ls -l /dev/console
	fi
	if [ ! -w /dev/console -a -n "${XDMHOST}" ]; then
		#
		# If $XDMHOST is set then assume we are on a fixed
		# xterm or "diskless" workstation on a LAN and that we
		# will commonly be using X applications running on
		# other local servers.
		#
		# For diskless workstations the use of a few local
		# applications with a $DISPLAY set to include the
		# network name is usually acceptable.
		#
		DISPLAY="${XDMHOST}${DISPLAY}"
		echo "$argv0: have set DISPLAY='$DISPLAY'"
		#
		# we should not need to set this here, but it does
		# make .ctwmrc simpler....
		#
		REMOTE_DISPLAY="${DISPLAY}"
	else
		# assume we are on a mobile workstation (laptop!) or
		# some similar host where X is started with startx
		# (i.e. not XDM) and that we will normally run all of
		# our applications either locally or via RSH, or SSH
		# tunnels, etc....
		#
		# so, using the local ":N.n" syntax is supremely
		# preferred for the local host
		#
		# XXX ${DOMAINNAME} is probably not right here....
		# XXX and if so, ${HOSTNAME} must be something remote
		# XXX client hosts understand
		#
		case "$HOSTNAME" in
		*.*)
			REMOTE_DISPLAY="${HOSTNAME}${DISPLAY}"
			;;
		*)
			REMOTE_DISPLAY="${HOSTNAME}${DOMAINNAME}${DISPLAY}"
			;;
		esac
		echo "$argv0: for DISPLAY='$DISPLAY' have set REMOTE_DISPLAY='$REMOTE_DISPLAY'"
	fi
	;;
esac
export REMOTE_DISPLAY
export DISPLAY

if [ "$XODISPLAY" != "$DISPLAY" ] ; then
	echo "$argv0: notice: XODISPLAY='$XODISPLAY', DISPLAY='$DISPLAY'"
fi

# may be things that depend on DISPLAY in here, so source it again....
#
if [ -r $HOME/.localprofile ] ; then
	. $HOME/.localprofile < /dev/null
fi

if [ -z "$LONGITUDE" ]; then
	# kelowna, bc
	LATITUDE="49.87157"
	LONGITUDE="-119.49038"
	export LATITUDE LONGITUDE
fi

# XXX these should be a single AWK script (the shell can eval the AWK
# output to set variables), and they should fetch only the values for
# the screen number which matches the $DISPLAY value.
#
VENDORSTR="`xdpyinfo | sed -n 's/^vendor string:[ ]*\(.*\)$/\1/p;//q'`"
SCREENDEPTH=`xdpyinfo | sed -n 's/^[ ]*depth of root window:[ ]*\([^ ][^ ]*\) .*$/\1/p;//q'`
SCREENSIZE=`xdpyinfo | sed -n 's/^[ ]*dimensions:[ ]*\([^ ][^ ]*\) .*$/\1/p;//q'`
SCREENSIZE_X=`expr "$SCREENSIZE" : '[0-9]*x\([0-9]*\)'`
SCREENSIZE_Y=`expr "$SCREENSIZE" : '\([0-9]*\)x'`
export VENDORSTR SCREENDEPTH SCREENSIZE SCREENSIZE_X SCREENSIZE_Y

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
# your bacon if xdm or xinit has not set up ~/.Xauthority correctly!
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
if [ -n "$XDMHOST" -a "$XDMHOST" = "`echo $DISPLAY | sed 's/:.*$//'`" ] ; then
	echo "$argv0: looks like '$XDMHOST' is running xdm for our display..."
	case "$XDMHOST" in
	almost.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		# now move our AUTH cookies over to the server in case it
		# doesn't have a shared $HOME...
		# (note: don't run this in the background!)
		xauth extract - $DISPLAY | onx11server $X11SERVER 'xauth merge -'
		# now start server programs we like for this scenario
		onx11server -n $X11SERVER 'xload -display $DISPLAY -geometry 120x40-200+48 -hl grey90' &
		;;
	very.weird.com|very.robohack.planix.com)
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
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 80x45-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
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
		#
		# N.B.:  in newer versions (>= 4.x) a new command-line syntax
		# makes use of "-display $DISPLAY" break.  However since well
		# before 3.8.2 the default is the $DISPLAY in the environment,
		# so we can just avoid using it at all.
		#
		# also, don't bother with '-v' -- it's too much and slows things!
		#
		if [ ${SCREENDEPTH:-1} -eq 1 ] ; then
			# the welcome screen on monochrome is silly and slow
			WM="ctwm -k -W"
		else
			WM="ctwm -k"
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
HAVEXBIFF=false ; export HAVEXBIFF
if expr "`type xbiff 2>/dev/null`" : '.* is .*/xbiff$' >/dev/null ; then
	HAVEXBIFF=true
fi
HAVEXEARTH=false ; export HAVEXEARTH
if expr "`type xearth 2>/dev/null`" : '.* is .*/xearth$' >/dev/null ; then
	HAVEXEARTH=true
fi
HAVEXPLANET=false ; export HAVEXPLANET
if expr "`type xplanet 2>/dev/null`" : '.* is .*/xplanet$' >/dev/null ; then
	HAVEXPLANET=true
fi
HAVEXPHOON=false ; export HAVEXPHOON
if expr "`type xphoon 2>/dev/null`" : '.* is .*/xphoon$' >/dev/null ; then
	HAVEXPHOON=true
fi
HAVEXV=false ; export HAVEXV
if expr "`type xv 2>/dev/null`" : '.* is .*/xv$' >/dev/null ; then
	HAVEXV=true
fi
HAVEIMAGEMAGICK=false ; export HAVEXV
if expr "`type display 2>/dev/null`" : '.* is .*/display$' >/dev/null ; then
	HAVEIMAGEMAGICK=true
fi

# argv0 is .xinitrc if called by xinit or startx (XXX this is nearly useless now...)
#
if [ "$argv0" = ".xinitrc" ] ; then
	echo "$argv0: looks like X was started with xinit or startx..."
	#
	# setup for "remote" windows
	#
	case $DISPLAY in
	:*)
		case $REMOTE_DISPLAY in
		very.local:*)	# xxx not so good for incoming SSH sessions...
			#
			# probably available hosts...
			#
			xauth nextract - ${REMOTE_DISPLAY} | onx11server more.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server historically.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server once.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | RSH=ssh onx11server freebsd.local 'xauth nmerge -'
			# next ones share a home dir mounted from more.local
			#xauth nextract - ${REMOTE_DISPLAY} | onx11server building.local 'xauth nmerge -'
			#xauth nextract - ${REMOTE_DISPLAY} | onx11server future.local 'xauth nmerge -'
			#
			# local server performance levels we want to be aware of
			# (NOTE: in stacked order, 48 pixels apart)
			onx11server -n building.local 'xload -geometry 120x40-200+48 -hl red' &
			onx11server -n more.local 'xload -geometry 120x40-200+96 -hl red' &
# slot for central	onx11server -n central.weird.com 'xload -geometry 120x40-200+144 -hl red' &
			onx11server -n future.local 'xload -geometry 120x40-200+192 -hl red' &
			onx11server -n xenful.local 'xload -geometry 120x40-200+240 -hl red' &
			# (and there is room for one under the clocks too)
#			onx11server -n historically.local 'xload -geometry 120x40-70+240 -hl red' &
			onx11server -n xentastic.local 'xload -geometry 120x40-70+240 -hl red' &
			#
			onx11server -n building.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
			onx11server -n more.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
			onx11server -n future.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
#			onx11server -n historically.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
			onx11server -n xenful.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
			onx11server -n xentastic.local 'uxterm -tn xterm-256color -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
			#
			# SSH-only hosts... (hopefully with auto-auth,
			# but note the use of both '-f' and '&')
			#
			ssh -Y -C -f woods@central.weird.com '. ./.profile; exec xload -geometry 120x40-200+144 -hl red' &
# xxx not yet...
#			ssh -Y -C -f woods@xentral.avoncote.ca '. ./.profile; exec xload -geometry 75x40-0+240 -hl red' &
			#
			# possibly available hosts -- one background
			# sub-shell for host each...
			#
#			(
#				xauth nextract - ${REMOTE_DISPLAY} | onx11server isit.local 'xauth nmerge -'
#				onx11server -n isit.local 'xload -geometry 120x40-70+240 -hl red' &
#			) &
#			(
#				xauth nextract - ${REMOTE_DISPLAY} | onx11server lilbit.local 'xauth nmerge -'
#			) &
###
### XXX these launch a terminal running on the remote host, sending
### X11 back through the SSH tunnel opened by each, which is obviously
### not ideal as it leaves us with no clue as to why a window
### disappears when the connection is dropped....
###
###			ssh -Y -n central.weird.com '. ./.profile; exec uxterm -tn xterm-256color -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &
###			ssh -Y -n central.weird.com '. ./.profile; exec uxterm -tn xterm-256color -geometry 80x45-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
###
### XXX what we really want is to stuff an "ssh -Y hostname" command
### into the xterm so that it would be the first command run by an
### interactive terminal -- note this is not the same as running
### "xterm -e 'ssh -Y hostname'" since that too will cause the window
### to close if/when the remote connection ends for whatever reason.
###
### One way to do this with a shell that supports $ENV might be to
### create a temporary file containing the command to run, then set
### $ENV to point at that file, and then to start the xterm as:
###
###	xterm -e env ENV=$tmp_rcfile $SHELL
###
### The trick is to figure out if/which $SHELL supports $ENV properly.
###
### Another trick is how to remove the temporarly file at the right time.
			;;
		esac
		;;
	very.weird.com:*|very.robohack.planix.com:*)

		# XXX note: for the moment the rest of this section is identical to the XDM variant above for this host.

		X11SERVER=${X11SERVER:-"once.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		# now move our AUTH cookies over to the server in case it
		# doesn't have a shared $HOME...
		xauth nextract - $DISPLAY | onx11server $X11SERVER 'xauth nmerge -'
		# we could/should run these ones in the background because the hosts might not be up
		xauth nextract - $DISPLAY | onx11server most.weird.com 'xauth nmerge -'
		xauth nextract - $DISPLAY | onx11server isit.weird.com 'xauth nmerge -'
		xauth nextract - $DISPLAY | onx11server historically.weird.com 'xauth nmerge -'
		# and then start some useful programs...
		onx11server -n $X11SERVER 'xload -display $DISPLAY -geometry 120x40-200+48 -hl red' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &
		onx11server -n $X11SERVER 'uxterm -display $DISPLAY -geometry 80x45-0-155 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &
		;;
	esac
	if $HAVEXV && [ -d '/Library/Desktop Pictures/Nature' ]; then
		xv -root '/Library/Desktop Pictures/Nature/'* "$HOME/Pictures/"*.jpg &
	elif $HAVEIMAGEMAGICK && [ -d "$HOME/Pictures" ]; then
		# this is wonky and slow, but perhaps the best we can do...
		# (nothing else sets the root window!)
		display "vid:$HOME/Pictures/"'*.[gjp]*' vid:'/Library/Desktop Pictures/Nature/*.[gjp]*' &
	elif $HAVEXEARTH ; then
		XEGAMMA=""
		if [ ${SCREENDEPTH:-1} -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"	# for Sun3 CG2
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
		unset XEGAMMA
	elif $HAVEXPLANET ; then
		XPLANETCONFIG=""
		if [ -r $HOME/xplanet.conf ]; then
			XPLANETCONFIG="-config $HOME/xplanet.conf"
		fi
		xplanet ${XPLANETCONFIG} -label -labelpos +15-15 -radius 60 -latitude $LATITUDE -longitude $LONGITUDE -wait 300 &
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
		tail -F $HOME/.xsession-errors-$DISPLAY | xconsole -geometry 500x120-0-0 -saveLines 2000 -notify -verbose -file /dev/stdin &
	elif [ "$XODISPLAY" != "$DISPLAY" -a -r $HOME/.xsession-errors-$XODISPLAY ] ; then
		tail -F $HOME/.xsession-errors-$XODISPLAY | xconsole -geometry 500x120-0-0 -saveLines 2000 -notify -verbose -file /dev/stdin &
	else
		echo "$argv0: didn't find ~/.xsession-errors-$DISPLAY, assuming old XDM config in use"
		tail -F $HOME/.xsession-errors | xconsole -geometry 500x120-0-0 -saveLines 2000 -notify -verbose -file /dev/stdin &
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
elif $HAVEXBIFF ; then
	xbiff -geometry 48x48-0+235 -bw 0 &
fi

#
# now we start more programs we like depending on which $DISPLAY we're
# using, but which don't depend on whether X was started by XDM or
# xinit/startx.
#

set_osx_fontpath ()
{
	if [ -d /opt/X11/share/fonts/misc ]; then
		X11FONTPATH='/opt/X11/share/fonts/misc/,/opt/X11/share/fonts/TTF/,/opt/X11/share/fonts/OTF/,/opt/X11/share/fonts/Type1/,/opt/X11/share/fonts/100dpi/:unscaled,/opt/X11/share/fonts/100dpi/'
	fi
	if [ -d /usr/X11/lib/X11/fonts/misc -a ! -L /usr/X11/lib/X11/fonts ]; then
		X11FONTPATH="$X11FONTPATH,/usr/X11/lib/X11/fonts/misc/,/usr/X11/lib/X11/fonts/TTF/,/usr/X11/lib/X11/fonts/OTF,/usr/X11/lib/X11/fonts/Type1/,/usr/X11/lib/X11/fonts/100dpi/:unscaled,/usr/X11/lib/X11/fonts/100dpi/"
	fi
	#
	# NB:  /System/Library/Fonts must come last else many things
	# will blow chunks with:
	#
	#	X Error of failed request:  BadValue (integer parameter out of range for operation)
	#	  Major opcode of failed request:  45 (X_OpenFont)
	#	  Value in failed request:  0x3600045
	#	  Serial number of failed request:  1991
	#	  Current serial number in output stream:  1992
	#
	# xfontsel can reproduce this by explicitly selecting family "menlo"
	#
	X11FONTPATH="$X11FONTPATH,/Library/Fonts/,/System/Library/Fonts/"
	xset fp= $X11FONTPATH
	if [ -d /sw/lib/X11/fonts/intl ]; then
		xset fp+ /sw/lib/X11/fonts/intl/
	fi
	if [ -d /sw/lib/X11/fonts/efont-unicode ]; then
		xset fp+ /sw/lib/X11/fonts/efont-unicode/
	fi
	if [ -d /sw/lib/X11/fonts/applettf ]; then
		xset fp+ /sw/lib/X11/fonts/applettf/
	fi
	if [ -d $HOME/.fonts ]; then
		xset +fp $HOME/.fonts
	fi
	xset fp rehash
}

case ${REMOTE_DISPLAY:-${DISPLAY}} in

very.local:*|very.weird.com:*|very.robohack.planix.com:*)
	# very has a very large, very (or now, "rather") hi-res, display....
	xset m 2 0

	set_osx_fontpath

	xload -geometry 120x40-200+0 -hl red &
	xclock -update 1 -geometry 215x30-0+196 -digital -chime &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi

	# All the small lower-right command terminals (starting with our own first one)
	# note the -155 is the height of the normal xconsole window, at 120, plus its ctwm decorations (I think)
	uxterm -tn xterm-256color -display $DISPLAY -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
#	onx11server -n once.weird.com 'uxterm -tn xterm-256color -display $DISPLAY -geometry 80x45-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll' &

	# All the big main area terminals (starting with our own first one)
	uxterm -tn xterm-256color -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
#	onx11server -n once.weird.com 'uxterm -tn xterm-256color -display $DISPLAY -geometry 132x100+0+0 -font 7x14 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1' &

	# All the normal emacs windows...
	# oddly using 130x64 turns into 163x55 with the font change!
	# this one's supposed to go in virtualscreen "Mail"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -display $DISPLAY -geometry 130x75+0+0" &
	# this one's supposed to go in virtualscreen "Coding"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -display $DISPLAY -geometry 130x75+0+0" &
	# this one should go in the virtualscreen "News"
###	$RSH -n whats.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -display $DISPLAY -geometry 130x75+0+0" &
	;;

macweird.*:*|*xquartz:*)
	#
	# macweird: Macbook 13" ancient
	#
	# also tested:
	#
	#   Macbook Air 13-inch; 1440x900 32-bit
	#   Macbook Air 13.3-inch; 2560x1600; 227 pixels per inch; 16:10 aspect ratio
	#
	set_osx_fontpath

	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,43.36,-79.40 -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPLANET ; then
		XPLANETCONFIG=""
		if [ -r $HOME/xplanet.conf ]; then
			XPLANETCONFIG="-config $HOME/xplanet.conf"
		fi
		xplanet ${XPLANETCONFIG} -label -labelpos +15-15 -radius 60 -latitude $LATITUDE -longitude $LONGITUDE -wait 300 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi

	xload -geometry 120x40-200+0 -hl red &
	xclock -update 1 -geometry 190-0+196 -digital -chime -face lucidasans-9 &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi
	# All the small lower-right command terminals (starting with our own first one)
	# note the -155 is the height of the normal xconsole window, at 120, plus its twm decorations (I think)
	uxterm -tn xterm-256color -geometry 80x30-0-155 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &

	# All the big main area terminals (starting with our own first one)
	# XXX damn, the default "-font 7x14" is just a wee bit too wide at 132 chars!
	# (but oddly not on the old macbook, only on the new macbook air!)
	# the old 13" macbook can only handle 59 rows, but the 13" macbook air does 66
	uxterm -tn xterm-256color -geometry 132x50+0+0 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
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
	uxterm -tn xterm-256color -display $DISPLAY -geometry 80x42-0-0 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &

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
#	if [ ${SCREENDEPTH:-1} -eq 1 ] ; then
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
	uxterm -tn xterm-256color -display $DISPLAY -geometry 100x50+0+0 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 &
	uxterm -tn xterm-256color -display $DISPLAY -geometry 80x24+0-0 -fn 5x7 -fs 7.5 -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1 -n OccupyAll &
	;;
esac


# when the window manager exits so shall we...
#
exec $WM

# and if that fails then we try a last-ditch trick
#
xterm -geometry 80x24+0+0 -cn -rw -sb -si -sk -ls -n FailSafe
exit 1
