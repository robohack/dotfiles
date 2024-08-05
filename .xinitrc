#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	37.24	24/08/05 13:39:31 (woods)"

# N.B.:  see below for the best place to enable 'set -x' for debugging...

# XXX WARNING XXX:  It seems something on macos (launchd_startx?) blows up if
# any line of output from this script is more than 512 bytes!!!  Pipe long lines
# through:   | sed 's/:/: /g' | fold -s

# Assumptions that may cause breakage:
#
#	- the shell supports functions
#	- the shell supports $(cmd) substitution
#	- other assumptions made by ~/.profile

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="$(basename $0)"
echo "$argv0: running from $0 (shell flags: -$-) on '$(tty)'"
XOPATH=$PATH
echo "$argv0: initial PATH='$PATH'" | sed 's/:/: /g' | fold -s
XODISPLAY=$DISPLAY
echo "$argv0: starting up with DISPLAY='$DISPLAY'"

#echo "With the following arguments:"
#
#c=0
#for i in "${@}"; do
#	c=$(expr ${c} + 1)
#	echo "arg #${c}: \"${i}\""
#done
#if [ $c -eq 0 ]; then
#	echo "# (No arguments)"
#fi
#echo '# End-of-Arguments'

# note we redirect input from /dev/null while sourcing in hopes of
# showing these scripts that they are not attached to a TTY.
#
if [ -r /etc/profile ] ; then
	echo "$argv0: sourcing /etc/profile"
	. /etc/profile < /dev/null
else
	echo "$argv0: no /etc/profile on this host!"
fi
if [ -r $HOME/.profile ] ; then
	echo "$argv0: sourcing $HOME/.profile"
	. $HOME/.profile < /dev/null
else
	echo "$argv0: no $HOME/.profile on this host!"
fi

# XXX I should think harder about just how much of the environment I really do
# want all subsequent X11 applications to inherit, especially, maybe, the window
# manager.  For example some things that can be over-ridden in ~/.localprofile
# will not be reset by ~/.profile if they're exported via the environment here.

if [ "$XOPATH" != "$PATH" ] ; then
	echo "$argv0: adjusted PATH='$PATH'" | sed 's/:/: /g' | fold -s
fi
unset XOPATH

echo "$argv0: using X11 in '$X11PATH'"

# XXX for debugging
#
#DEBUG=true
DEBUG=false
if $DEBUG; then
	echo "$argv0: Debugging enabled ...."
	set -x
fi

# try making sure we are using the maximum display resolution right away
#
# N.B.:  This assumes we've told startx to set the DPI parameter correctly for
# the current screen.  On macos, with XQuartz, for the Retina 6K Pro Display
# XDR, which has approximately 218 DPI, this is done with:
#
#	defaults write org.xquartz.X11 dpi -int 218
#
# On XQuartz/Darwin (and in general with the Xorg Xserver) the command-line
# "-dpi" value is used to compute the frame buffer's width and height based on
# the number of pixels each way, but there's no way to fetch this original
# command-line parameter back; except in the way xdpyinfo does, i.e. by getting
# the pixel dimensions and the screen dimensions and re-computing the x/y DPI.
# Due to rounding errors this may end up returning an off-by-one value for one
# of the dimentions.
#
# Note that when in XQuartz is in full-screen mode you can get back to the macos
# desktop with the "toggle full screen" keyboard shortcut (which is normally
# <command><option><a>).  Then if you want to get back into X11 full-screen mode
# again you can re-run the "xrandr -s" command again.  An easy way to do this is
# to use the Shortcuts application to create a shortcut that can also be hooked
# to the same key sequence (in the "Run with:" field in the shortcut editor
# "Details" sub-panel) as this key sequence is almost never used in any other
# macos app (that I use, anyway) [it doesn't work quite right from Mail though].
#
# Here's the script for the shortcut (with the resolution of my display
# hard-coded here):
#
#	
#	# XXX This gets terribly confused if there is more than one
#	# ~/.serverauth.* file, obviously....
#	#
#	if [ -r $HOME/.serverauth.* ]; then
#		#
#		# xxx should check the owner process is running....
#		# xxx if xauth complains, try removing ~/.serverauth.*
#		# xxx if X is started manually a fake ~/.serverauth.* might
#		# have to be created from the .Xauthority file using
#		# "xauth add" (using the creds for the correct display)
#		#
#		/opt/X11/bin/xrandr -s 6016x3384 -display $(launchctl getenv DISPLAY)
#		#
#	else
#		osascript -e 'display alert "Switch-To-X11" message "Just hit the X icon in the dock!"' > /dev/null
#	fi
#	
#
echo "$argv0: Getting ready to try xrandr stuff ...."
if type xrandr >/dev/null 2>&1; then
	xrandr -s $(xrandr 2>/dev/null | awk '/^Screen [0-9]+:/ {print $(NF-2) "x" $(NF)}')
	echo "$argv0: Done xrandr ...."
else
	echo "$argv0: cannot force hi-res display without xrandr utility..."
fi

# XXX these should be in a single AWK script (the shell can eval the AWK output
# to set variables), and the script should fetch only the values for the screen
# number which matches the $DISPLAY value!!!
#
#	SCREEN_NUM=${DISPLAY##*:}
#	xdpyinfo | sed -n "/^screen #${SCREEN_NUM}:/,+2p'
#
# for now this is only designed to work in single-screen environments.
#
# XXX this could also be a shell fragment using xrdb's pre-defined symbols and
# their values, with input fed to stdin and then spit back out and eval'ed from:
#
#	xrdb -quiet -cpp m4 -E -n | sed 1d
#
# however xdpyinfo provides all values without need for calculations that might
# suffer rounding, while xrdb only provides [XY]_RESOLUTION values in pixels per
# metre for some stupid reason.
#
VENDORSTR="$(xdpyinfo | sed -n 's/^vendor string:[ ]*\(.*\)$/\1/p;//q')"
SCREENDEPTH=$(xdpyinfo | sed -n 's/^[ ]*depth of root window:[ ]*\([^ ][^ ]*\) .*$/\1/p;//q')
SCREENSIZE=$(xdpyinfo | sed -n 's/^[ ]*dimensions:[ ]*\([^ ][^ ]*\) .*$/\1/p;//q')
SCREENDIM=$(xdpyinfo | sed -n 's/^[ ]*dimensions:[ ]*[^ ][^ ]* [^ ]* (\(.*\))$/\1/p;//q')
SCREENDPI=$(xdpyinfo | sed -n 's/^[ ]*resolution:[ ]*\([^ ][^ ]*\) .*$/\1/p;//q')
# break out millimetre dimensions
SCREENDIMmm_X=$(expr "$SCREENDIM" : '\([0-9]*\)x')
SCREENDIMmm_Y=$(expr "$SCREENDIM" : '[0-9]*x\([0-9]*\)')
# and in pixel dimensions
SCREENSIZE_X=$(expr "$SCREENSIZE" : '\([0-9]*\)x')
SCREENSIZE_Y=$(expr "$SCREENSIZE" : '[0-9]*x\([0-9]*\)')
SCREENDPI_X=$(expr "$SCREENDPI" : '\([0-9]*\)x')
SCREENDPI_Y=$(expr "$SCREENDPI" : '[0-9]*x\([0-9]*\)')
# use DPI to get approximate inches....
SCREENDIM_X=$(expr "$SCREENSIZE_X" / "$SCREENDPI_X")
SCREENDIM_Y=$(expr "$SCREENSIZE_Y" / "$SCREENDPI_Y")
#
# xxx we only actually use SCREENDIM_[XY], SCREENDPI_[XY], and SCREENSIZE_Y (and
# potentially VENDORSTR); and only within this file!
#
export VENDORSTR SCREENDEPTH SCREENSIZE SCREENDIM SCREENDPI
export SCREENDIMmm_X SCREENDIMmm_Y SCREENDIM_X SCREENDIM_Y
export SCREENDPI_X SCREENDPI_Y SCREENSIZE_X SCREENSIZE_Y

# N.B.:  If the true screen resolution is within 10% of 100DPI it makes the most
# sense to claim exactly 100DPI to avoid font-scaling artifacts for bitmap fonts
if expr \( $SCREENDPI_X / 100 = 1 \) \& \( $SCREENDPI_X % 100 \<= 10 \) >/dev/null; then
	FontXDPI=100
else
	FontXDPI=${SCREENDPI_X}
fi
if expr \( $SCREENDPI_Y / 100 = 1 \) \& \( $SCREENDPI_Y % 100 \<= 10 \) >/dev/null; then
	FontYDPI=100
else
	FontYDPI=${SCREENDPI_Y}
fi
export FontXDPI FontYDPI

# Xrdb stuff must come before any other client is started!
#
if [ -r "/usr/lib/X11/Xdefaults" ] ; then
	xrdb -merge /usr/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$X11PATH/lib/X11/Xdefaults" ] ; then
	xrdb -merge $X11PATH/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$OPENWINHOME/lib/Xdefaults" ] ; then
	xrdb -merge $OPENWINHOME/lib/Xdefaults	# Load default X11 resource database
fi
if [ -r $HOME/.Xdefaults ] ; then
	# n.b.:  note the custom use of m4!
	xrdb -cpp m4 -merge $HOME/.Xdefaults	# Load user's X11 resource database
fi

if [ -r ${X11PATH}/lib/X11/fonts/misc/fonts.dir ]; then
	X11FONTDIR=${X11PATH}/lib/X11/fonts
#elif [ -r ${X11PATH}/share/fonts/misc/fonts.dir ]; then
else
	X11FONTDIR=${X11PATH}/share/fonts
fi
#
# Choose some defaults for pixel-sized things...
#
# XXX TitleHeight here is mostly based on observation, but could be calculated
# with output from xwininfo (e.g. top corner - TDBorderWidth), or even from some
# font info... and we're assuming a 10pt font is used....
#
if [ $SCREENDPI_Y -ge 200 ]; then
	# a "6k" display
	TDBorderWidth="10"
	TitleHeight=55
elif [  $SCREENDPI_Y -ge 110 ]; then
	# a "4k" display
	TDBorderWidth="8"
	TitleHeight=42		# xxx unknown
else
	# a "normal" display
	TDBorderWidth="6"
	TitleHeight=31
fi
export TDBorderWidth
export TitleHeight

DecentDeciPt="80"
DecentPt="8"
export DecentDeciPt DecentPt
#
# Best is to arrange one's font-path to get the desired one first....
#
# XXX this should probably check the family name is actually installed and
# available, but only after the font-path is set?
#
# See also https://www.x.org/releases/X11R7.7/doc/xorg-docs/xlfd/xlfd.html
#
DefaultFontSpec='-*-commitmono-medium-r-*-*-*-${DecentDeciPt}-${FontXDPI}-${FontYDPI}-m-*-iso10646-1'
DefaultFontSpecL1='-*-commitmono-medium-r-*-*-*-${DecentDeciPt}-${FontXDPI}-${FontYDPI}-m-*-iso8859-1'
export DefaultFontSpec DefaultFontSpecL1
#
DecentFontSpec=$(eval echo "${DefaultFontSpec}")
DecentFontSpecL1=$(eval echo "${DefaultFontSpecL1}")
export DecentFontSpec DecentFontSpecL1
#
# For Xft we've set the Xft.dpi resource so this allows the physical font size
# to be specified (e.g. with Xterm's "-fs" option, using the following with it's
# "-fa" option) and for a decent scalable font to be chosen:
#
# Note for Xft using fc-list(1) is probably the best way to fint Xft fonts...
#
# See "Font Name" in the following for how XftPatterns are specified:
#
#	https://fontconfig.pages.freedesktop.org/fontconfig/fontconfig-user.html
#
DefaultXftFontSpec="CommitMono"
DefaultXftFontSpecL1="CommitMono"
export DefaultXftFontSpec DefaultXftFontSpecL1

#
# For hi-res displays one critical entry should be in ~/.fonts/fonts.alias:
#
#	cursor -xfree86-cursor-medium-r-normal--0-160-218-218-p-0-adobe-fontspecific
#
# (note there are "xcursor-*" pkgsrc packages with scalable loadable cursors)
#
# FontPath setup must also come before any other graphical client is started!
#
# Install any/all of the general-coverage (and symbol) TTF or OTF fonts,
# e.g. from pkgsrc, and then:
#
#	cd ${PKG}/share/fonts/X11/{OTF,TTF}
#	mkfontscale .
#	mkfontdir .
#	xset fp rehash	# if X is already running....
#
# Non-packaged TrueType (TTF) fonts should be installed similarly in ~/.fonts:
#
# Note that OpenType (OTF) fonts don't work unless they are _also_ installed on
# the client system.  They can be listed by xlsfonts, but not opened!  See:
#
#	https://stackoverflow.com/questions/57078805/xloadqueryfont-fails-on-font-found-with-xlistfonts
#
# FontConfig is a stupid STUPID design!  Having to have fonts installed on the
# both client and server is STUPID!
#
if [ ! -d ${HOME}/.fonts ]; then
	echo "$argv0: making a new empty ~/.fonts directory!"
	echo "$argv0: NOTICE: you may need to install some TTF fonts here (e.g. Liberation)!"
	mkdir ${HOME}/.fonts
fi
#
if [ -r ${HOME}/.fonts.alias.m4 ] ; then
	# n.b.:  note the abuse of xrdb and the custom use of m4!
	xrdb -quiet -cpp m4 -E -n ${HOME}/.fonts.alias.m4 | sed 1,2d > ${HOME}/.fonts/fonts.alias
fi
#
# XQuartz comes with a script called "font_cache" that is run (if the preference
# "org.xquartz.X11 cache_fonts" is true) by startx (and privileged_startx) that
# does this and more for all standard font directories on macos.
#
# XXX However since we have possibly created or updated the fonts.alias file we
# must re-run mkfontdir!
#
# XXX If a private "~/.fonts/encodings" directory is needed, add it here too!
#
MKF_ENC=${X11FONTDIR}/encodings
mkfontscale -a ${MKF_ENC} -a ${MKF_ENC}/large -- ${HOME}/.fonts
mkfontdir -e ${MKF_ENC} -e ${MKF_ENC}/large -- ${HOME}/.fonts
#
if [ -n "${X11FONTPATH}" ]; then
	echo "$argv0: initial X11FONTPATH=${X11FONTPATH}"
fi
# xxx ~/.fonts should always come first!
fpappend X11FONTPATH "${HOME}/.fonts/"
if [ -d ${HOME}/Library/Fonts ]; then
	for fd in $(find "${HOME}/Library/Fonts" -type d) ; do
		fpappend X11FONTPATH ${fd}
	done
fi
fpappend X11FONTPATH "${LOCAL}/share/fonts/X11/TTF/" "${LOCAL}/share/fonts/X11/OTF/" "${LOCAL}/share/fonts/X11/Type1/"
fpappend X11FONTPATH "${PKG}/share/fonts/X11/TTF/" "${PKG}/share/fonts/X11/OTF/" "${PKG}/share/fonts/X11/Type1/"
fpappend X11FONTPATH "${PKG}/share/fonts/X11/intlfonts/TrueType" "${PKG}/share/fonts/X11/intlfonts/Type1"
#
# now for the standard X11 fonts, preferring scalable fonts of course!
#
fpappend X11FONTPATH "${X11FONTDIR}/TTF/" "${X11FONTDIR}/OTF/"
#
# N.B.:  If you've set the "cursor" alias above then this is where it is from!
#
fpappend X11FONTPATH "${X11FONTDIR}/Type1/"
#
# The next three are of course just for OS X:
#
# (see /opt/X11/bin/font_cache)
#
# this may have been a fiction of faulty cut&paste....
fpappend X11FONTPATH "${X11PATH}/share/fonts/system_fonts/" "${X11PATH}/share/fonts/system_fonts/Supplemental"
fpappend X11FONTPATH "${X11PATH}/share/system_fonts/" "${X11PATH}/share/system_fonts/Supplemental"
#
# Don't bother with unscalable fonts on high-res systems...
#
# XXX this assumes we have a modified fonts.alias though!
#
if [ $SCREENDPI_Y -lt 110 ]; then
	: so this block can be commented out for testing but for this line
	#
	### xxx n.b.:  misc has "nil2", for which there doesn't seem to be a
	### scalable variant, but do we really need it?  It only seems to be
	### used by Xterm for the default "Unreadable" font (which we override
	### anyway).
	#
	# XXX this/these give Ctwm's "identify" window a nicer, smaller ("normal
	# sized"?) font on the iMac27 (the :unscaled variants above do not).
	#
	# N.B.:  The ":unscaled" suffix is undocumented (at least in xset(1))
	# (it is mentioned as a "hack" in the main "Fonts in X11" document).  It
	# prevents the server from scaling bitmap fonts, avoiding pixelation and
	# other weird artifacts.
	#
	fpappend X11FONTPATH "${X11FONTDIR}/100dpi/:unscaled"
	#
	# "misc" should come last, but must be present
	#
	# (XXX unless an equivalent fonts.alias is available elsewhere(?), which
	# of course it does with my ~/.fonts/fonts.alias, but beware that it
	# contains many of the same alias names pointing at scalable fonts!)
	#
	fpappend X11FONTPATH "${X11FONTDIR}/misc/:unscaled"
fi
#
#
#echo "$argv0: final FONTPATH='$FONTPATH'" | sed 's/:/: /g' | fold -s
export X11FONTPATH

#
# finally we can set and activate the new font path
#
xset fp= "${X11FONTPATH}"
xset fp rehash

fc-cache -v

##################
#xlsfonts > $HOME/tmp/xlsfonts.out
#xset q
## /Users/woods/.fonts/,/Users/woods/Library/Fonts,/opt/pkg/share/fonts/X11/TTF/,/opt/pkg/share/fonts/X11/intlfonts/TrueType,/opt/pkg/share/fonts/X11/intlfonts/Type1,/opt/X11/share/fonts/TTF/,/opt/X11/share/fonts/OTF/,/opt/X11/share/fonts/Type1/,/opt/X11/share/system_fonts/,/opt/X11/share/system_fonts/Supplemental
#set -x
#xlsfonts -fn "-misc-commitmono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-*-commitmono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-*-*-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-misc-commitmono-medium-r-normal--0-86-218-218-m-0-*-1"
#xlsfonts -o -fn "-misc-commitmono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-*-commitmono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-*-*-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-misc-commitmono-medium-r-normal--0-86-218-218-m-0-*-1"
#
#xlsfonts -fn "-xfree86-cursor-medium-r-normal--0-160-218-218-p-0-adobe-fontspecific"
#xlsfonts -o -fn "-xfree86-cursor-medium-r-normal--0-160-218-218-p-0-adobe-fontspecific"
#xlsfonts -fn "cursor"
#xlsfonts -o -fn "cursor"
#
#set +x
##################

#
# now in case we've updated the "cursor" alias....
#
xsetroot -cursor_name tcross

if [ $argv0 = ".xsession" -a -z "$XDMHOST" ] ; then
	#
	# assume HOSTNAME & DOMAINNAME are set reasonably by the user
	# or system shell profile script(s) such that this combination
	# is useful both on the local host as well as on any servers
	# where X11 applications may run.
	#
	case "${HOSTNAME}${DOMAINNAME}" in
	*${DOMAINNAME})
		XDMHOST=${HOSTNAME}
		;;
	*)
		XDMHOST=${HOSTNAME}${DOMAINNAME}
		;;
	esac
	export XDMHOST					# also used by .[c]twmrc
	echo "$argv0: have set XDMHOST='$XDMHOST'"
fi

if [ "$SCREENDIM_X" -gt 12 ]; then
	ClockSize="2"			# 2"x2" is OK on larger screens, but....
else
	ClockSize="1"
fi
ClockWidth=$(expr ${SCREENDPI_X} \* ${ClockSize})
ClockHeight=$(expr ${SCREENDPI_Y} \* ${ClockSize})
ClockGeom=${ClockWidth}x${ClockHeight}-0+0
ClockOff=$(expr ${ClockWidth} + ${TDBorderWidth} + ${TDBorderWidth})
DClockHeight=$(expr ${SCREENDPI_Y} / 4) # 1/4 inch should always suffice?
DClockGeom=${ClockWidth}x${DClockHeight}-0+${ClockOff}
export ClockSize ClockWidth ClockHeight ClockGeom ClockOff DClockHeight DClockGeom

# should this use floating point to get finer control???
# note that m4 evals for ~/.ctwmrc use integer math too
XloadWidth=$(expr ${SCREENDPI_X} \* 1)
XloadHeight=$(expr ${SCREENDPI_Y} / 3)
XloadGeom=${XloadWidth}x${XloadHeight}-${ClockOff}+0
export XloadWidth XloadHeight XloadGeom

#
# XXX re-calculate WorkSpaceManager's geometry
#
# should this use floating point to get finer control???
# note that m4 evals for ~/.ctwmrc use integer math too
#
# define(WS_GEOM, eval(WIDTH / 30 * WS_COLS)[x]eval(HEIGHT / 30 * WS_ROWS))
WSWidth=$(expr ${SCREENSIZE_X} / 30 \* 3)
WSHeight=$(expr ${SCREENSIZE_Y} / 30 \* 4)
#
# define(WS_OFF, eval(ClockWidth + XloadWidth + (2 * TDBORDERWIDTH)))
#
# XXX on a really wide screen, like the XDR, we might have two Xload columns
#
WSOff=$(expr ${ClockOff} + ${XloadWidth} + ${TDBorderWidth} + ${TDBorderWidth})
#
export WSOff WSWidth WSHeight

XconsoleWidth=$(expr ${WSOff} + ${WSWidth})
if [ $SCREENSIZE_Y -gt 3000 ]; then
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 2)
elif [ $SCREENSIZE_Y -ge 1440 ]; then
	# this should give 130 on the 27" iMac, matching my old pixel setting
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 12 / 10)
else
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 1)
fi
# n.b. not sure why there's only one borderWidth involved!
XconsoleVoff=$(expr ${XconsoleHeight} + ${TitleHeight} + ${TDBorderWidth})
export XconsoleWidth XconsoleHeight XconsoleVoff

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
	xconsole_args="-geometry ${XconsoleWidth}x${XconsoleHeight}-0-0"
	xconsole_args=$xconsole_args" -saveLines 2000 -notify -verbose"
	#
	if [ $(uname -s) = "Darwin" ]; then
		# XXX
		# XXX hmmmm.... what can we do to direct our output here????
		# XXX (but maybe we don't need to with startx writing to /var/log/system.log)
		# XXX
#		tail -F /var/log/system.log | \
		# XXX
		# XXX n.b.:  with the advent of the new Unified Logging System
		# XXX and log(1) but with XQuartz not (yet) using os_log(3), so
		# XXX 'subsystem == "org.xquartz.X11"' cannot be used, though
		# XXX then debug would be too much....
		#
		# XXX the "log" and "awk" may get left behind if nothing ever
		# generates another "launchd_startx" message and so they never
		# write to the closed pipe and so never get a SIGPIPE....
		#
		TEN_S_AGO=$(date -r $(($(date '+%s') - 10)) '+%Y-%m-%d %H:%M:%S''%z')
                { log show --info --debug --predicate 'process == "launchd_startx" || process == "privileged_startx"' --style compact --start "${TEN_S_AGO}";
                  log stream --level debug --predicate 'process == "launchd_startx" || process == "privileged_startx"' --style compact; } | \
			awk '{
				match($0, /[^\]]*\][^\])]*[\])] /);
				print substr($0, 1, 24) substr($0, (RLENGTH > 0) ? RLENGTH : 25);
				fflush();
			     }' | \
		xconsole $xconsole_args -file /dev/stdin &
	elif [ -w /dev/console ] ; then
		ONCONSOLE=true
		echo "$argv0: redirecting stdout and stderr to /dev/console."
		exec 1>/dev/console 2>&1
		echo "$argv0: redirected stdout and stderr to /dev/console, starting xconsole"
		xconsole $xconsole_args &
		# wait a wee bit in hopes xconsole gets running to display any errors....
		sleep 1
	else
		echo "$argv0: WARNING: display was '$DISPLAY' but /dev/console is not writable by ${USER:-$LOGNAME}!"
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
	#LATITUDE="49.87157"
	#LONGITUDE="-119.49038"
	# Avoncote Farm (gives XEarth a nice(r) view of Canada too)
	LATITUDE=50.14349201581629
	LONGITUDE=-101.77988255896784
	export LATITUDE LONGITUDE
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
# xxx xhost tries to open a font for some unknown reason!
#
xhost +$(hostname) +localhost +local: -

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

# N.B.:  once upon a time -ziconbeep was not universally available
#
if [ -z "$XTERM_OPTS" ]; then
	XTERM_OPTS="-fbx -cn -rw -sb -si -sk -sl 2048 -ls -ziconbeep 1"
fi
export XTERM_OPTS

# If you're running on a diskless workstation that runs xdm itself
# then you probably have a server that's more powerful and on which
# you'd rather run the majority of your X applications.  Normally
# this will be the same server your home directory resides on....
#
if [ -n "$XDMHOST" -a "$XDMHOST" = "$(echo $DISPLAY | sed 's/:.*$//')" ] ; then
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
		onx11server -n $X11SERVER 'xload -geometry 120x40-200+48 -hl grey90' &
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
		onx11server -n $X11SERVER 'xload -geometry 120x40-200+48 -hl red' &
		onx11server -n $X11SERVER "uxterm -tn xterm -geometry 132x82+0+0 -font 7x14 $XTERM_OPTS -n OccupyAll" &
		onx11server -n $X11SERVER "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} $XTERM_OPTS -n OccupyAll" &
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
	if type ctwm >/dev/null 2>&1; then
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
	elif type tvtwm >/dev/null 2>&1; then
		WM=tvtwm
	elif type wmx >/dev/null 2>&1; then
		WM=wmx
	elif type wm2 >/dev/null 2>&1; then
		WM=wm2
	elif type scwm >/dev/null 2>&1; then
		WM=scwm
	elif type wmaker >/dev/null 2>&1; then
		WM=wmaker
	elif type fvwm >/dev/null 2>&1; then
		WM=fvwm
	elif type twm >/dev/null 2>&1; then
		WM=twm
	else
		echo "$argv0: assuming 'xterm' is available in lieu of a known WM..."
		WM=xterm
	fi
	export WM
fi

HAVESWISSWATCH=false ; export HAVESWISSWATCH
if type swisswatch >/dev/null 2>&1; then
	HAVESWISSWATCH=true
fi
HAVEXBUFFY=false ; export HAVEXBUFFY
if type xbuffy >/dev/null 2>&1; then
	HAVEXBUFFY=true
fi
HAVEXBIFF=false ; export HAVEXBIFF
if type xbiff >/dev/null 2>&1; then
	HAVEXBIFF=true
fi
HAVEXEARTH=false ; export HAVEXEARTH
if type xearth >/dev/null 2>&1; then
	HAVEXEARTH=true
fi
HAVEXPLANET=false ; export HAVEXPLANET
if type xplanet >/dev/null 2>&1; then
	HAVEXPLANET=true
fi
HAVEXPHOON=false ; export HAVEXPHOON
if type xphoon >/dev/null 2>&1; then
	HAVEXPHOON=true
fi
HAVEXV=false ; export HAVEXV
if type xv >/dev/null 2>&1; then
	HAVEXV=true
fi
HAVEIMAGEMAGICK=false ; export HAVEIMAGEMAGICK
if type display >/dev/null 2>&1; then
	HAVEIMAGEMAGICK=true
fi

# argv0 is .xinitrc if called by xinit or startx (likely only for XQuartz)
#
if [ "$argv0" = ".xinitrc" ] ; then
	echo "$argv0: looks like X was started with xinit or startx..."
	#
	# setup for "remote" windows
	#
	case $DISPLAY in
	:*)
		case $REMOTE_DISPLAY in
		#
		# XXX most/all of this for my main desktop should move below to
		# the main REMOTE_DISPLAY section as there's little possibility
		# or likelyhood of ever using xdm with XQuartz
		#
		very.local:*|Gregs-Mac-Pro.local:*|very-old.local:*)	# xxx not so good for incoming SSH sessions...
			#
			# probably available hosts...
			#
			xauth nextract - ${REMOTE_DISPLAY} | onx11server more.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server xenful.local 'xauth nmerge -'
# xentastic is currently using $HOME from more.local
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server xentastic.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server nb10.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server -S nbday.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server nbt.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server nbt2.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server nbtest.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server nbtcur.local 'xauth nmerge -'
			xauth nextract - ${REMOTE_DISPLAY} | onx11server -S fezzik.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server -S frodo.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server historically.local 'xauth nmerge -'
#			xauth nextract - ${REMOTE_DISPLAY} | onx11server once.local 'xauth nmerge -'
			# next ones share a home dir mounted from more.local
			#xauth nextract - ${REMOTE_DISPLAY} | onx11server building.local 'xauth nmerge -'
			#xauth nextract - ${REMOTE_DISPLAY} | onx11server future.local 'xauth nmerge -'
			#
			# XXX for starting windows it would be ideal if we could
			# tell the window manager to do these using its existing
			# menu operations!
			#
			# local server performance levels we want to be aware of
			xloadoff="0"

#			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
#			XloadGeomBuilding=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
#			export XloadGeomBuilding
#			onx11server -n building.local "xload -geometry ${XloadGeomBuilding -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomMore=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
			export XloadGeomMore
			onx11server -n more.local "xload -geometry ${XloadGeomMore} -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomCentral=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
			export XloadGeomCentral
# see below central	onx11server -n central.weird.com "xload -geometry ${XloadGeomCentral} -hl red" &

#			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
#			XloadGeomFuture=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
#			export XloadGeomFuture
#			onx11server -n future.local "xload -geometry ${XloadGeomFuture} -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomXenful=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
			export XloadGeomXenful
			onx11server -n xenful.local "xload -geometry ${XloadGeomXenful} -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomXentastic=${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}
			export XloadGeomXentastic
			onx11server -n xentastic.local "xload -geometry ${XloadGeomXentastic} -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomXentral="${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}"
			export XloadGeomXentral
# see below for xentral	onx11server -nS xentral "xload -geometry ${XloadGeomXentral} -hl red" &

#			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
#			XloadGeomOnce="${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}"
#			export XloadGeomOnce
#			onx11server -n once.local "xload -geometry ${XloadGeomOnce} -hl red" &

#			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
#			XloadGeomHistorically="${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}"
#			export XloadGeomHistorically
#			onx11server -n historically.local "xload -geometry ${XloadGeomHistorically} -hl red: &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomB2="${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}"
			export XloadGeomB2
# see below for b2	onx11server -n b2.local "xload -geometry ${XloadGeomB2} -hl red: &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			XloadGeomFezzik="${XloadWidth}x${XloadHeight}-${ClockOff}+${xloadoff}"
			export XloadGeomFezzik
			onx11server -nS fezzik.local "xload -geometry ${XloadGeomFezzik} -hl red" &

			# these are under the workspace manager
			#
			xloadoff=$(expr ${WSHeight} + ${TDBorderWidth} + ${TDBorderWidth})
			#
			XloadGeomNbtest="${XloadWidth}x${XloadHeight}-${WSOff}+${xloadoff}"
			export XloadGeomNbtest
			onx11server -n nbtest.local "xload -geometry ${XloadGeomNbtest} -hl red" &

			xload2east=$(expr ${WSOff} + ${XloadWidth} + ${TDBorderWidth} + ${TDBorderWidth})

			XloadGeomNbtcur="${XloadWidth}x${XloadHeight}-${xload2east}+${xloadoff}"
			export XloadGeomNbtcur
			onx11server -n nbtcur.local "xload -geometry ${XloadGeomNbtcur} -hl red" &

			xloadoff=$(expr ${xloadoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})

			XloadGeomNb10="${XloadWidth}x${XloadHeight}-${WSOff}+${xloadoff}"
			export XloadGeomNb10
			onx11server -n nb10.local "xload -geometry ${XloadGeomNb10} -hl red" &

			XloadGeomNbday="${XloadWidth}x${XloadHeight}-${xload2east}+${xloadoff}"
			export XloadGeomNbday
			onx11server -nS nbday.local "xload -geometry ${XloadGeomNbday} -hl red" &

# xxx these need a variable for "80x45" as it could be wider (e.g. 91x45, 95x50), or taller, or shorter....
# (calculate based on character size and $XconsoleWidth and something related to IR_GEOM vs. $XconsoleHeight)

#			onx11server -n building.local "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n more.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n nb10.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -nS nbday.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n nbt.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
#			onx11server -n nbt2.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n nbtest.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n nbtcur.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -nS fezzik.local "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
#			onx11server -n future.local "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
#			onx11server -n historically.local "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n xenful.local "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			onx11server -n xentastic.local "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &
			#
			# SSH-only hosts... (hopefully with auto-xauth via -Y,
			# but note the use of both '-f' and '&')
			#
			# XXX n.b. try -Snone "because session multiplexing
			# doesn't work very well with X forwarding"
			#
			ssh -CY -C -f woods@central.weird.com ". ./.profile; exec xload -geometry ${XloadGeomCentral} -hl red" &
			ssh -CY -C -f woods@xentral.avoncote.ca ". ./.profile; exec xload -geometry ${XloadGeomXentral} -hl red" &
			ssh -CY -C -f woods@b2.avoncote.ca ". ./.profile; exec xload -geometry ${XloadGeomB2} -hl red" &
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
###			ssh -Y -n central.weird.com ". ./.profile; exec uxterm -tn xterm-256color -geometry 132x82+0+0 -font 7x14 $XTERM_OPTS" &
###			ssh -Y -n central.weird.com ". ./.profile; exec uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} $XTERM_OPTS -n OccupyAll" &
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
### Another trick is how to remove the temporary file at the right time.
###
### Or maybe they can just be prototype scripts, one per remote host, and so
### they pre-exist and persist and don't have to be removed.
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
		onx11server -n $X11SERVER 'xload -geometry 120x40-200+48 -hl red' &
		onx11server -n $X11SERVER "uxterm -tn xterm-256color -geometry 132x100+0+0 -font 7x14 $XTERM_OPTS" &
		onx11server -n $X11SERVER "uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} $XTERM_OPTS -n OccupyAll" &
		;;
	esac

	#
	# Common for all xinit/startx startups
	#

	if $HAVEXV && [ -d '/Library/Desktop Pictures/Nature' ]; then
		xv -root '/Library/Desktop Pictures/Nature/'* "$HOME/Pictures/"*.jpg &
#	elif $HAVEIMAGEMAGICK && [ -d "$HOME/Pictures" ]; then
#		# this is wonky and slow, but perhaps the best we can do...
#		# (nothing else sets the root window!)
#		display "vid:$HOME/Pictures/"'*.[gjp]*' vid:'/Library/Desktop Pictures/Nature/*.[gjp]*' &
	elif $HAVEXEARTH ; then
		XEGAMMA=""
		if [ ${SCREENDEPTH:-1} -ne 1 ] ; then
			XEGAMMA="-gamma 1.6 -night 15"	# for Sun3 CG2
		fi
		xearth $XEGAMMA -bigstars 10 -pos fixed,${LATITUDE:-0},${LONGITUDE:-0} -label -labelpos +1+1 -grid -wait 900 -nice 10 &
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
	#	we're probably signing in via xdm on the system console....
	#
	# I/O is already redirected to /dev/console above, so we will see it...
	#
	:
else
	#
	#	we're probably signing in on an independent xterm....
	#
	# XXX I think these "tail"s could get left behind if nothing ever writes
	# to the file they are watching (so they'll never write anything more to
	# the closed pipe, and so never get a SIGPIPE....
	#
	if [ -r $HOME/.xsession-errors-$DISPLAY ] ; then
		tail -F $HOME/.xsession-errors-$DISPLAY | xconsole $xconsole_args -file /dev/stdin &
	elif [ "$XODISPLAY" != "$DISPLAY" -a -r $HOME/.xsession-errors-$XODISPLAY ] ; then
		tail -F $HOME/.xsession-errors-$XODISPLAY | xconsole $xconsole_args -file /dev/stdin &
	else
		echo "$argv0: didn't find ~/.xsession-errors-$DISPLAY, assuming old XDM config in use"
		tail -F $HOME/.xsession-errors | xconsole $xconsole_args -file /dev/stdin &
	fi
fi

#
# now we start more programs we like depending on which $DISPLAY we're
# using, but which don't depend on whether X was started by XDM or
# xinit/startx.
#

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

case ${REMOTE_DISPLAY:-${DISPLAY}} in

very.local:*|Gregs-Mac-Pro.local:*|very-old.local:*|very.weird.com:*|very.robohack.planix.com:*)
	# very has a very large, very hi-res, display....
	# very-old also has a quite large, quite hi-res, display...
	xset m 2 0

	xload -geometry $XloadGeom -hl red &
	#
	# xxx the xclock supplied with XQuartz 2.8.2 is broken somehow in how it
	# displays text, but the pkgsrc build works OK....
	#
	XCLOCK="xclock"
	if [ -x ${PKG}/bin/xclock ]; then
		XCLOCK=${PKG}/bin/xclock
	fi
	LC_CTYPE=C ${XCLOCK} -geometry $DClockGeom -face ${DefaultXftFontSpec}:size=10 -digital -update 1 -chime &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry $ClockGeom &
	else
		${XCLOCK} -update 1 -geometry $ClockGeom &
	fi

	# All the small lower-right command terminals (starting with our own first one)
	uxterm -tn xterm-256color -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -n OccupyAll -e login -pf ${USER:-$LOGNAME} &
#	onx11server -n once.weird.com "uxterm -tn xterm -geometry 80x45-0-${XconsoleVoff} -fs 7.5 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS} -n OccupyAll" &

	# All the big main area terminals (starting with our own first one)
	uxterm -tn xterm-256color -geometry 132x82+0+0 -fs 9 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -e login -pf ${USER:-$LOGNAME} &
#	onx11server -n once.weird.com "uxterm -tn xterm -geometry 132x82+0+0 -fs 9 -fa \"${DefaultXftFontSpec}\" ${XTERM_OPTS}" &

	if type xmahjongg >/dev/null 2>&1; then
		#
		# XXX the positioning portion of a geometry spec never seems to
		# apply properly to xmahjongg (except if it is exactly "+0+0")
		#
		# assume the tiles and bin directory are in related places
		MP=$(type xmahjongg)
		MP=$(expr "${MP}" : '^[^/]*\(/[^ )]*\)')
		MP=$(dirname $(dirname $MP))
		#
		# Bigger tiles for a high-res display: resize the tile images to
		# 200% using mogrify, xv, etc.
		#
		if [ $SCREENDPI_Y -gt 110 -a -f ${MP}/share/xmahjongg/tiles/real_x2.gif ]; then
			if [ -r $HOME/public_html/marble.gif ]; then
				MB="--bg $HOME/public_html/marble.gif"
			fi
# XXX xmahjongg parses but does not correctly use offset info from -g (i.e. --geometry)
#			xmahjongg -t real_x2 -g 2000x1500+2200+0 $MB &
			xmahjongg -t real_x2 -g 2000x1500 $MB &
		elif [ $SCREENDPI_Y -gt 110 ]; then
			echo "You might want to make a bigger tile set for XmahJongg."
			xmahjongg -g +1000+0 &
		else
			xmahjongg -g +1000+0 &
		fi
	fi

	uxterm -tn xterm-256color -geometry 80x25+$(expr \( $SCREENDIM_X \* $SCREENDPI_X / 2 \) - \( 3 \* $SCREENDPI_X \))-$(expr $SCREENDPI_Y \* 2) -fs 9 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -fc 5 -n AirTrafficMonitor -T AirTrafficMonitor -e air-traffic-monitor &

	# All the normal emacs windows...
	# oddly using 130x64 turns into 163x55 with the font change!
	# this one's supposed to go in virtualscreen "Mail"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -geometry 130x75+0+0" &
	# this one's supposed to go in virtualscreen "Coding"
###	$RSH -n building.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -geometry 130x75+0+0" &
	# this one should go in the virtualscreen "News"
###	$RSH -n whats.weird.com ". ./.localprofile; PATH=$PATH:/usr/pkg/bin:/usr/local/bin; export PATH; MAIL=/var/mail/$LOGNAME; export MAIL; exec /usr/pkg/bin/emacs -i -geometry 130x75+0+0" &
	;;

macweird.*:*|Gregs-MBP.*|*xquartz:*)
	#
	# macweird: Macbook 13" ancient
	#
	# also tested:
	#
	#   Macbook Air 13-inch; 1440x900 32-bit
	#   Macbook Air 13.3-inch; 2560x1600; 227 pixels per inch; 16:10 aspect ratio
	#
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,${LATITUDE:-0},${LONGITUDE:-0} -label -labelpos +1+1 -grid -wait 900 -nice 10 &
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
	LC_CTYPE=C xclock -update 1 -geometry 190-0+196 -digital -chime -face lucidasans-9 &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi
	# All the small lower-right command terminals (starting with our own first one)
	uxterm -tn xterm-256color -geometry 80x30-0-${XconsoleVoff} -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -n OccupyAll -e login -pf ${USER:-$LOGNAME} &

	# All the big main area terminals (starting with our own first one)
	# XXX damn, the default "-font 7x14" is just a wee bit too wide at 132 chars!
	# (but oddly not on the old macbook, only on the new macbook air!)
	# the old 13" macbook can only handle 59 rows, but the 13" macbook air does 66
	uxterm -tn xterm-256color -geometry 132x50+0+0 -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -e login -pf ${USER:-$LOGNAME} &
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
		xearth -gamma 1.3 -bigstars 10 -pos fixed,${LATITUDE:-0},${LONGITUDE:-0} -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
	# local server performance levels we want to be aware of
	$RSH -n most.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+48 -hl grey90 &
	$RSH -n once.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+144 -hl grey90 &
#	$RSH -n starting-out.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+192 -hl grey90 &
#	$RSH -n whats.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+242 -hl grey90 &
#	$RSH -n always.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-70+242 -hl grey90 &

	# All the small lower-right command terminals (starting with our own first one)
	xterm -geometry 80x42-0-${XconsoleVoff} $XTERM_OPTS -n OccupyAll &

	# All the consoles:
	# first column....
	xterm -geometry 102x24+0+0   -fn 5x7 $XTERM_OPTS -n Con:isit -T "Console [isit.weird.com]" -e console -S isit &
	xterm -geometry 102x24+0+205 -fn 5x7 $XTERM_OPTS -ls -n Con:hubly -T "Console [hubly.weird.com]" -e console -S hubly &
	xterm -geometry 102x24+0+410 -fn 5x7 $XTERM_OPTS -ls -n Con:flipping -T "Console [flipping.weird.com]" -e console -S flipping &
	xterm -geometry 102x24+0+615 -fn 5x7 $XTERM_OPTS -n Con:building -T "Console [building.weird.com]" -e console -S building &
	xterm -geometry 102x24+0+820 -fn 5x7 $XTERM_OPTS -n Con:whats -T "Console [whats.weird.com]" -e console -S whats &

	# first column, from the bottom...
	xterm -geometry 80x24+0-0 -fn 5x7 $XTERM_OPTS -ls -n Con:RAID0 -T "Console [RAID-00 most:/var]" -e console -S raid-00 &


	# second column....
#	xterm -geometry 102x24+545+0 -fn 5x7 $XTERM_OPTS -n Con:proven -T "Console [proven.weird.com]" -e console -S proven &
	xterm -geometry 102x24+545+0 -fn 5x7 $XTERM_OPTS -n Con:once -T "Console [once.weird.com]" -e console -S once &
	xterm -geometry 102x24+545+205 -fn 5x7 $XTERM_OPTS -n Con:sometimes -T "Console [sometimes.weird.com]" -e console -S sometimes &
	xterm -geometry 102x24+545+410 -fn 5x7 $XTERM_OPTS -n Con:always -T "Console [always.weird.com]" -e console -S always &
	xterm -geometry 102x24+545+615 -fn 5x7 $XTERM_OPTS -n Con:starting-out -T "Console [starting-out.weird.com]" -e console -S starting-out &
	xterm -geometry 102x24+545+820 -fn 5x7 $XTERM_OPTS -n CallerID -T "very:/dev/ttyb [Caller ID Box]" -e console -S callerid &

	# second column, from the bottom...
	xterm -geometry 102x24+545-0 -fn 5x7 $XTERM_OPTS -n Con:best-1 -T "Console [BEST-3.1-UPS-1]" -e console -S best-3.1-1 &


	# start xv first, in a subshell that sleeps first for a bit
	# -- it needs to be running before we try our tkined trick, but after the window manager
###	( sleep 20; xv -owncm -iconic & ) &
	# start tkined last, in a subshell that sleeps first for a bit
	# -- hoping xv (from above) and the window manager are running already
###	( sleep 40; $RSH -n building.weird.com exec /usr/local/bin/tkined1.5.0 -use $(xwininfo -name "xv 3.10a(PNG) <unregistered>" | awk '/xwininfo/ {print $4}') -geometry 1200x925+0-0 /home/building/woods/tkined-3.tki & ) &
	;;

xtremely*:*)
	xload -geometry 190x50-0+233 -hl red &
	xclock -norender -geometry 190x30-0+196 -digital &
	$RSH -n always.weird.com exec /usr/X11R6/bin/xload -geometry 120x50-198+233 -hl red &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -norender -geometry 190x190-0+0 &
	fi
	# All the small lower-right command terminals (starting with our own first one)
	uxterm -tn xterm -geometry 80x42-0-0 -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -n OccupyAll &

	;;

*)
	# make everything rather small so that enough will fit on even
	# tiny screens....
	#
	if $HAVEXEARTH ; then
		xearth -gamma 1.3 -bigstars 10 -pos fixed,${LATITUDE:-0},${LONGITUDE:-0} -label -labelpos +1+1 -grid -wait 900 -nice 10 &
	elif $HAVEXPHOON ; then
		xphoon -t 10 &
	elif [ -r $X11PATH/include/X11/bitmaps/escherknot ] ; then
		xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
	else
		echo "$argv0: no background set"
	fi
	if [ ${SCREENDEPTH:-1} -eq 1 ] ; then
		xload -geometry $XloadGeom -hl grey90 &
	else
		xload -geometry $XloadGeom -hl red &
	fi
	LC_CTYPE=C xclock -geometry $DClockGeom -face :size=10 -digital -update 1 -chime &
	if $HAVESWISSWATCH ; then
		swisswatch -name swissclock -geometry $ClockGeom &
	else
		xclock -update 1 -geometry $ClockGeom &
	fi
	uxterm -tn xterm -geometry 100x50+0+0 -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} &
	uxterm -tn xterm -geometry 80x24+0-0 -fn 5x7 -fs 7.5 -fa "${DefaultXftFontSpec}" ${XTERM_OPTS} -n OccupyAll &
	;;
esac


# when the window manager exits so shall we...
#
exec $WM

### $WM || twm


# and if that fails then we try a last-ditch "trick"
#
xterm -geometry 80x24+0+0 $XTERM_OPTS -n FailSafe
exit 1
