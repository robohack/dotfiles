#! /bin/sh
:
#
#	.xinitrc - X Window System startup file (also .xsession)
#
#ident	"@(#)HOME:.xinitrc	37.32	24/11/02 11:15:56 (woods)"

# N.B.:  see below for the best place to enable 'set -x' for debugging...

# XXX WARNING XXX:  It seems something on macos (launchd_startx?) blows up if
# any line of output from this script is more than 512 bytes!!!  Pipe long lines
# through:   | sed 's/:/: /g' | fold -s

# Assumptions that may cause breakage:
#
#	- the shell supports functions
#	- the shell supports $(cmd) substitution
#	- other assumptions made by ~/.profile
#
# ./.profile should provide proper path setup
#
# ./.shrc should provide a function "onx11server"

# argv0 must be set before sourcing .profile, else we'll loop!
#
argv0="$(basename $0)"
echo "$argv0: running from $0 (shell flags: -$-) on '$(tty)'"
XOPATH=$PATH
echo "$argv0: initial PATH='$PATH'" | sed 's/:/: /g' | fold -s
XODISPLAY=$DISPLAY
echo "$argv0: starting up with DISPLAY='$DISPLAY'"

# N.B.:  we assume $DISPLAY is set correctly enough that clients run from this
# script can connect and use it.

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
if [ -r /etc/profile ]; then
	echo "$argv0: sourcing /etc/profile"
	. /etc/profile < /dev/null
else
	echo "$argv0: no /etc/profile on this host!"
fi
# XXX /etc/shrc ???  or should ~/.profile or ~/.shrc do that?
if [ -r $HOME/.profile ]; then
	echo "$argv0: sourcing $HOME/.profile"
	. $HOME/.profile < /dev/null
else
	echo "$argv0: no $HOME/.profile on this host!"
fi

# XXX I should think harder about just how much of the environment I really do
# want all subsequent X11 applications to inherit, especially, maybe, the window
# manager.  For example some things that can be over-ridden in ~/.localprofile
# will not be reset by ~/.profile if they're exported via the environment here.

if [ "$XOPATH" != "$PATH" ]; then
	echo "$argv0: adjusted PATH='$PATH'" | sed 's/:/: /g' | fold -s
fi
unset XOPATH

echo "$argv0: using X11 in '$X11PATH'"

# XXX for debugging
#
# N.B.:  debugging can break XQuartz as mentioned in the warning above!
#
DEBUG=false
#DEBUG=true
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
# NOTE: MacBook Air 13-inch Retina is 227 dpi	(2880 x 1800)
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
# Here's the script for the shortcut:
#
#	
#	#!/bin/ksh
#	#
#	# XXX This gets terribly confused if there is more than one
#	# ~/.serverauth.* file, obviously....
#	#
#	if [ -r $HOME/.serverauth.* ]; then
#		DISPLAY=$(launchctl getenv DISPLAY)
#		MAXRES=$(xrandr -display $DISPLAY -q 2>/dev/null | awk '/^Screen [0-9]+:/ {print $(NF-2) "x" $(NF)}')
#		echo "Switch-To-X11: switching $DISPLAY to '$MAXRES'" 1>&2
#		xrandr -display $DISPLAY -s "$MAXRES"
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

# Modern X11, since Release 5, has had decent i18n support
#
# This is primarily done here to make sure LC_CTYPE propagates through the
# window manager.
#
# Note these settings should match what ~/.profile will set for a uxterm(1)....
#
if $HAVELOCALE &&  [ -z "${LC_CTYPE}" ]; then
	# xxx should maybe check if "locale -a" reports a matching en_CA*?
	LC_CTYPE="en_CA.UTF-8"
	export LC_CTYPE
	LC_NUMERIC="en_CA.UTF-8"
	export LC_NUMERIC
	LC_MONETARY="en_CA.UTF-8"
	export LC_MONETARY
else
	echo "$argv0: WARNING!!!  An X11 system without locale(1) is unheard of!"
fi

# Xrdb stuff must come before any other client is started!
#
if [ -r "/usr/lib/X11/Xdefaults" ]; then
	xrdb -merge /usr/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$X11PATH/lib/X11/Xdefaults" ]; then
	xrdb -merge $X11PATH/lib/X11/Xdefaults	# Load default X11 resource database
elif [ -r "$OPENWINHOME/lib/Xdefaults" ]; then
	xrdb -merge $OPENWINHOME/lib/Xdefaults	# Load default X11 resource database
fi
if [ -r $HOME/.Xdefaults ]; then
	# n.b.:  note the custom use of m4!
	xrdb -cpp m4 -merge $HOME/.Xdefaults	# Load user's X11 resource database
fi

if [ -r ${X11PATH}/lib/X11/fonts/misc/fonts.dir ]; then
	X11FONTDIR=${X11PATH}/lib/X11/fonts
elif [ -r ${X11PATH}/share/fonts/X11/misc/fonts.dir ]; then
	X11FONTDIR=${X11PATH}/share/fonts/X11
else
	X11FONTDIR=${X11PATH}/share/fonts
fi
export X11FONTDIR
echo "$argv0: X11FONTDIR='${X11FONTDIR}'"

# Set shell variables from things calculated in ~/.X11-macros.m4
#
# n.b.:  note the abuse of xrdb and the custom use of m4!
#
# XXX xdpyinfo provides the DPI values we need without need for calculations
# that might suffer rounding, but without going metric getting $SCREENSIZE* from
# either xdpyinfo or xrdb's m4 defines is a rough calculation.  The latter only
# provides [XY]_RESOLUTION values in pixels per metre for some stupid reason!
#
# However everywhere else, i.e. namely in .X11-m4macros.m4 (and .ctwmrc), we
# have to use those defines and the rough calculations they require, so we'll
# use xrdb here to make sure everything matches.
#
# XXX there are some ugly translations between M4 names and variable names that
# should probably all be normalised!
#
eval `
{
	echo "changequote([, ])"
	cat .X11-m4macros.m4
	echo "[VENDORSTR]='VENDOR'"
	echo "[SCREENDEPTH]=PLANES"
	echo "[SCREENSIZE]=WIDTH[x]HEIGHT"
	echo "[SCREENSIZE_X]=WIDTH"
	echo "[SCREENSIZE_Y]=HEIGHT"
	echo "[SCREENDIM]='ScreenDimX[x]ScreenDimY'"
	echo "[SCREENDIM_X]=ScreenDimX"
	echo "[SCREENDIM_Y]=ScreenDimY"
	echo "[SCREENDPI_X]=HorizDPI"
	echo "[SCREENDPI_Y]=VertDPI"
	echo "[FontXDPI]=FontXDPI"
	echo "[FontYDPI]=FontYDPI"
	echo "[BorderWidth]=BORDERWIDTH"
	echo "[TDBorderWidth]=TDBORDERWIDTH"
	echo "[TitleHeight]=TitleHeight"
	echo "[ClockSize]=ClockSize"
	echo "[ClockWidth]=ClockWidth"
	echo "[ClockHeight]=ClockHeight"
	echo "[ClockOffH]=ClockOffH"
	echo "[ClockOffV]=ClockOffV"
	echo "[ClockGeom]='ClockGeom'"
	echo "[DClockHeight]=DClockHeight"
	echo "[DClockGeom]='DClockGeom'"
	echo "[XloadWidth]=XloadWidth"
	echo "[XloadHeight]=XloadHeight"
	echo "[XloadGeom]='XloadGeom'"
	echo "[WSWidth]=WS_WIDTH"
	echo "[WSHeight]=WS_HEIGHT"
	echo "[WSOff]=WS_OFF"
	echo "[XchronoWidth]='XchronoWidth'"
	echo "[XchronoGeom]='XchronoGeom'"
} | xrdb -quiet -cpp m4 -E -n  | sed -e 1d -e '/^!/d'`

# Choose some defaults for pixel-sized things...
#
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
# Note: the *L1 (Latin-1) specs are not currently used.
#
DecentFont="DejaVu Sans Mono"

DefaultFontSpec='-*-${DecentFont}-medium-r-normal--0-${DecentDeciPt}-${FontXDPI}-${FontYDPI}-m-*-iso10646-1'
DefaultFontSpecL1='-*-${DecentFont}-medium-r-normal--0-${DecentDeciPt}-${FontXDPI}-${FontYDPI}-m-*-iso8859-1'
export DefaultFontSpec DefaultFontSpecL1
#
DecentFontSpec=$(eval echo "${DefaultFontSpec}")
DecentFontSpecL1=$(eval echo "${DefaultFontSpecL1}")
export DecentFontSpec DecentFontSpecL1
#
# For Xft ~/.Xdefaults forces the Xft.dpi resource to be set to what is worked
# out above so this allows the physical font size to be specified, in points,
# (either with a ":size=" attribute in the font spec (which can be abbreviated
# as "-POINTS"), or with a command-line option to override that, e.g. with
# Xterm's "-fs" option.  See fonts.conf(5).
#
# Note for Xft using fc-list(1) is probably the best way to fint Xft fonts...
#
#	fc-list :scalable=true:spacing=100:lang=en: family
#
# See "Font Name" in the following for how XftPatterns are specified:
#
#	https://fontconfig.pages.freedesktop.org/fontconfig/fontconfig-user.html
#
# N.B.:  Xft does seem able to find fonts with the spaces removed from their
# "fullname", e.g. "DejaVuSansMono" will find "DejaVu Sans Mono".  Phew!  (This
# seems to be due to testing with a rule: "Equal(ignore blanks)")
#
# (xxx this probably isn't necessary as it is also specified in ~/.Xdefaults
# with the wild-carded faceName resource)
#
DefaultXftFont=$(echo "${DecentFont}" | sed 's/ //g')
# note: ":size=N" can be replaced by "-N" but this form is more explicit
DecentXftFontSpec="${DefaultXftFont}:size=9"
export DefaultXftFont DecentXftFontSpec

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
# Note that OpenType (OTF) fonts don't work even if they are _also_ installed on
# the client system.  They can be listed by xlsfonts, but not opened!  See:
#
#	https://stackoverflow.com/questions/57078805/xloadqueryfont-fails-on-font-found-with-xlistfonts
#
# FontConfig is a stupid STUPID design!  Having to have fonts installed on the
# both client and server is STUPID!
#
if [ ! -d ${HOME}/.fonts ]; then
	echo "$argv0: making a new empty ~/.fonts directory!"
	echo "$argv0: NOTICE: you may need to install some TTF fonts here (e.g. DejaVu)!"
	mkdir ${HOME}/.fonts
fi
#
if [ -r ${HOME}/.fonts.alias.m4 ]; then
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
# N.B.:  If you've set the "cursor" alias then this is where it is from!
#
fpappend X11FONTPATH "${X11FONTDIR}/Type1/"
#
# The next is of course just for OS X:
#
# (see /opt/X11/bin/font_cache)
#
# XXX these are not usable on older macos, e.g. 10.13.... hmmmm....  maybe we
# just leave these to fontconfig?
#
#fpappend X11FONTPATH "${X11PATH}/share/system_fonts/" "${X11PATH}/share/system_fonts/Supplemental"
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
if xset q | sed -n '/^Font/ {n;p;}' | fgrep built-ins 2>&1 >/dev/null; then
	X11FONTPATH="${X11FONTPATH},built-ins"
fi
#
echo "$argv0: final X11FONTPATH='$X11FONTPATH'" | sed 's/:/: /g' | fold -s
export X11FONTPATH

#
# finally we can set and activate the new font path
#
xset fp= "${X11FONTPATH}"
xset fp rehash

fc-cache -v

##################
#ls -lisa $HOME/.fonts
#xlsfonts > $HOME/tmp/xlsfonts.out
#xset q
## /Users/woods/.fonts/,/Users/woods/Library/Fonts,/opt/pkg/share/fonts/X11/TTF/,/opt/pkg/share/fonts/X11/intlfonts/TrueType,/opt/pkg/share/fonts/X11/intlfonts/Type1,/opt/X11/share/fonts/TTF/,/opt/X11/share/fonts/OTF/,/opt/X11/share/fonts/Type1/,/opt/X11/share/system_fonts/,/opt/X11/share/system_fonts/Supplemental
## /Users/woods/.fonts/,/opt/X11/share/fonts/TTF/,/opt/X11/share/fonts/OTF/,/opt/X11/share/fonts/Type1/,/opt/X11/share/system_fonts/,/opt/X11/share/system_fonts/Supplemental
#if ! $DEBUG; then
#	set -x
#fi
#xlsfonts -fn "-misc-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-*-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-*-*-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -fn "-misc-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-*-1"
#xlsfonts -o -fn "-misc-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-*-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-*-*-medium-r-normal--0-86-218-218-m-0-iso8859-1"
#xlsfonts -o -fn "-misc-dejavu sans mono-medium-r-normal--0-86-218-218-m-0-*-1"
#
#xlsfonts -fn "-xfree86-cursor-medium-r-normal--0-160-218-218-p-0-adobe-fontspecific"
#xlsfonts -o -fn "-xfree86-cursor-medium-r-normal--0-160-218-218-p-0-adobe-fontspecific"
#xlsfonts -fn "cursor"
#xlsfonts -o -fn "cursor"
#
#if ! $DEBUG; then
#	set +x
#fi
##################

#
# now in case we've updated the "cursor" alias....
#
xsetroot -cursor_name tcross

if [ -z "${HOSTNAME}" ]; then
	HOSTNAME=`hostname`
fi
export HOSTNAME

case "${HOSTNAME}" in
*.local)
	DOMAINNAME=".local"
	;;
esac
if [ -z "${DOMAINNAME}" ]; then
	# from ~/.shrc
	DOMAINNAME=`get_domainname`
fi
export DOMAINNAME

if [ $argv0 = ".xsession" -a -z "$XDMHOST" ]; then
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

# Align the xconsole at the bottom of the screeen to the workspace manager at
# the top of the screen....
#
# xxx at the moment nothing in ~/.ctwmrc needs $Xconsole*, but instead we pass
# it $XTERM_LITTLE_OPTS (which is using these calculations) trough the environ.
#
XconsoleWidth=$(expr ${WSOff} + ${WSWidth})
if [ $SCREENSIZE_Y -gt 3000 ]; then
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 2) # 2 inches
elif [ $SCREENSIZE_Y -ge 1440 ]; then
	# this should give 130 on the 27" iMac, matching my old pixel setting
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 12 / 10) # 1.2 inches
else
	XconsoleHeight=$(expr ${SCREENDPI_Y} \* 1) # 1 inch
fi
# n.b. not sure why there was only one borderWidth involved originally!
XconsoleVoff=$(expr ${XconsoleHeight} + ${TitleHeight} + ${TDBorderWidth} + ${TDBorderWidth} + ${TDBorderWidth})

# XTerm options which cannot to be set consistently with resources, ("+ai" may
# be added below, depending on which window manager is used), or which should
# not because they depend on other things in the environment, such as which
# window manager is being used.
#
# n.b.:  this is also set in ~/.profile, and should be identical
#
if [ -z "$XTERM_OPTS" ]; then
	XTERM_OPTS=""
fi
export XTERM_OPTS

if [ -z "$WM" ]; then
	echo "$argv0: searching for a usable window manager..."
	#
	# XXX maybe it would be nice to have a window-manager-chooser, which is
	# effectively what a modern "session" manager is... but xsm(1) isn't
	# really that modern, and it's the only thing available by default, and
	# it wants to be started instead of the window manager.  Even xsm(1)
	# though can do all the secondary app startups, in theory, but it seems
	# it needs the deprecated rstart(1) and friends to start programs on
	# remote machines, and:
	#
	# *** rstart and iceauth should be installed on each machine that you expect
	#     to run applications on that will be part of your session ***
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
		if [ ${SCREENDEPTH:-1} -eq 1 ]; then
			# the welcome screen on monochrome is silly and slow
			WM="ctwm -k -K $HOME/.ctwmrc.m4out -W"
		else
			WM="ctwm -k -K $HOME/.ctwmrc.m4out"
		fi
		# force activeIcon on for ctwm, though it may not be used if an
		# IconManager is in use for Xterm windows....
		#
		XTERM_OPTS="+ai ${XTERM_OPTS}"
	elif type etwm >/dev/null 2>&1; then
		# Brian Bidulock's "Enhanced TWM", forked from CTWM, possibly dead
		# https://github.com/bbidulock/etwm
		#
		WM=etwm
		XTERM_OPTS="+ai ${XTERM_OPTS}"
	elif type adwm >/dev/null 2>&1; then
		# Brian Bidulock's "advanced dynamic window manager" for his(?)
		# XDE (X Desktop Environment)
		# https://github.com/bbidulock/xde-desktop
		#
		WM=adwm
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
		WM="twm"
		XTERM_OPTS="+ai ${XTERM_OPTS}"
	else
		echo "$argv0: assuming 'xterm' is available in lieu of a known WM..."
		WM=xterm
	fi
	export WM
fi
echo "$argv0: WM='${WM}'"

ONCONSOLE=false
export ONCONSOLE
case "$DISPLAY" in
:[0-9]*)
	#
	# OK, so if we're running on the system console there's a good chance
	# the user will never see system messages sent to /dev/console so we
	# will try to run xconsole and also redirect our own output to
	# /dev/console so that window manager and application errors will also
	# go to the xconsole window.  Unfortunately there's no logging to a
	# permanent file this way though...
	#
	xconsole_args="-geometry ${XconsoleWidth}x${XconsoleHeight}-0-0"
	#
	# XXX add '-fn "$(eval echo \"$DecentFontSpec\" | sed s/-80-/-60-/)' for small laptop screens
	#
	xconsole_args=$xconsole_args" -saveLines 2000 -notify -verbose"
	#
	if [ $(uname -s) = "Darwin" ]; then
		#
		# XXX n.b.:  with the advent of the new Unified Logging System
		# XXX and log(1) but with XQuartz not (yet) using os_log(3), so
		# XXX 'subsystem == "org.xquartz.X11"' cannot be used, though
		# XXX then debug would be too much easier....
		#
		# XXX the "log" and "awk" may get left behind if nothing ever
		# generates another message, and so they never write to the
		# closed pipe, and so never get a SIGPIPE....
		#
		TEN_S_AGO=$(date -r $(($(date '+%s') - 10)) '+%Y-%m-%d %H:%M:%S''%z')
		{
			log show --info --debug --predicate 'process == "launchd_startx" || process == "privileged_startx"' --style compact --start "${TEN_S_AGO}"
			log stream --level debug --predicate 'process == "launchd_startx" || process == "privileged_startx"' --style compact
		} | awk '{
				match($0, /[^\]]*\][^\])]*[\])] /);
				print substr($0, 1, 24) substr($0, (RLENGTH > 0) ? RLENGTH : 25);
				fflush();
			}' | xconsole $xconsole_args -file /dev/stdin &
		#
	#
	# don't bother with this unless it'll do us some good though...
	# (i.e. unless XDM ran the GiveConsole script)
	#
	elif [ -w /dev/console ]; then
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
		# XXX this is likely startx on a system without 
	fi

	# Now since $DISPLAY doesn't have a hostname we need to figure out what
	# name to use so that remotely running clients can connect back to this
	# Xserver...
	#
	# N.B.:  This first test, per chance, also excludes Darwin hosts,
	# handled above....
	#
	if [ ! -w /dev/console -a -n "${XDMHOST}" ]; then
		#
		# If $XDMHOST is set then assume we are on a fixed xterminal or
		# "diskless" workstation on a LAN and that we will commonly be
		# using X applications running on other local servers, including
		# $X11SERVER which will be set below.
		#
		# For diskless workstations the use of a few local applications
		# with a $DISPLAY set to include the network name is usually
		# acceptable.
		#
		DISPLAY="${XDMHOST}${DISPLAY}"
		echo "$argv0: have set DISPLAY='$DISPLAY' using XDMHOST, setting REMOTE_DISPLAY to match"
		#
		# we should not need to set this here, but it does make .ctwmrc
		# simpler....
		#
		REMOTE_DISPLAY="${DISPLAY}"
	else
		# assume we are on a (mobile?) workstation (laptop?) or some
		# similar host where X is started with startx (i.e. not XDM),
		# e.g. XQuartz on Darwin, and that we will normally run all of
		# our applications either locally or via RSH, or SSH tunnels,
		# etc., so we keep the short-form $DISPLAY for local clients and
		# set $REMOTE_DISPLAY to a name that hopefully they can resolve
		# to an address by which they can connect back to this Xserver.
		#
		# so, using the local short-form ":N.n" syntax is supremely
		# preferred for the local host as it should use its local Unix
		# socket.
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


if [ "$XODISPLAY" != "$DISPLAY" ]; then
	echo "$argv0: notice: XODISPLAY='$XODISPLAY', DISPLAY='$DISPLAY'"
fi

# may be things that depend on DISPLAY in here, so source it again....
#
if [ -r $HOME/.localprofile ]; then
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

#	Possibly needed xmodmap fixups....
#
# Unfortunately there is no real way to tell what type of keyboard is
# attached and no way to revert unknown earlier changes, so instead we
# must just rely on the display name to know what keyboard is attached.
#
#case "$VENDORSTR" in
#*"Network Computing Devices"*)
case $DISPLAY in
xtra*:*|xtremely*:*)
	if [ -r $HOME/.Xmodmap-ncd-108 ]; then
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
wayback*:*)
	if [ -r $HOME/.Xmodmap-reverse-scroll ]; then
		xmodmap $HOME/.Xmodmap-reverse-scroll
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
if [ -r $HOME/.xhosts ]; then
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

# Notes:
#
# - setting -fs (faceSize) to 7.5 makes the TrueType font size most closely
#   match the size of the default "fixed" bitmap font's size.
#
# - Unfortunately on both the iMac 27" (at 2560x1440) and the Retina 6K Pro
#   there is not quite room enough horizontally for the three standard xterms
#   (two at 132 cols, one at +80 cols) with a point size of 10 for the big ones,
#   but 9pt is quite readable anyway.
#
# XXX with a laptop in the range of 300mmx200mm we want no more than 60x132 -fs 7.5
#
# XXX to accurately calculate a fit onto the screen we would need to use
# xprop(1) or xwininfo(1) (-size) and an existing window displaying the desired
# font size.  This would give us the character cell size ("program specified
# resize increment").  So far the only way I can think of doing this is to start
# one or more test xterms (one for each desired font size) and have them run
# xwininfo to find out about their own WM size hints, but this would require
# using a temporary file (and then parsing its contents):
#
#	xterm -fs 9 -e 'xwininfo -id $WINDOWID -size > $HOME/tmp/xwi.out; exit'
#
if [ ${SCREENSIZE_X} -ge 6016 -a ${SCREENSIZE_Y} -ge 3384 ]; then
	XTERM_BIG_OPTS="-geometry 132x100+0+0 -fs 9"
elif [ ${SCREENSIZE_X} -ge 2560 -a ${SCREENSIZE_Y} -ge 1440 ]; then
	XTERM_BIG_OPTS="-geometry 132x82+0+0 -fs 9"
elif [ ${SCREENSIZE_X} -ge 1600 -a ${SCREENSIZE_Y} -ge 1280 ]; then
	XTERM_BIG_OPTS="-geometry 117x82+0+0 -fs 9"
elif [ ${SCREENSIZE_X} -ge 1280 -a ${SCREENSIZE_Y} -ge 1024 ]; then
	XTERM_BIG_OPTS="-geometry 100x50+0+0 -fs 9"
else
	XTERM_BIG_OPTS="-geometry 80x40+0+0 -fs 7.5"
fi
export XTERM_BIG_OPTS

# With CommitMono font this could be 51 high
# With DejaVu Sans Mono this could be 45 high
# With Liberation Mono this could be 47 high
# With .sf ns mono (apple) this could be 49 high
# With CommitMono and DejaVu Sans Mono fonts this could be 91 wide
# With Liberation Mono and .sf ns mono (appl) this could be 92 wide
#
# XXX with a laptop in the range of 300mmx200mm we want no more than 40x80 -fs 6
#
# (e.g. 91x45, 95x50), or taller, or shorter....  (calculate based on character
# size (see above about 'xwininfo -size') and $XconsoleWidth and something
# related to IR_GEOM vs. $XconsoleHeight)
#
# N.B.: for the '-T OccupyAll' to work the terminal must be started after the
# window manager is running, so put a little sleep in front of ones using these
# flags....
#
XTERM_LITTLE_OPTS="-geometry 91x47-0-${XconsoleVoff} -fs 7.5 -T OccupyAll"
export XTERM_LITTLE_OPTS

# on OSX Xterm isn't configured to record logins properly with "-ls", but if we
# start login(1) instead then it will record the login session.
#
# N.B.:  because this contains another variable expansion the command must be
# started with "eval"
#
if [ $(uname -s) = "Darwin" ]; then
	XTERM_LOGIN_OPTS="-e login -pf ${USER:-$LOGNAME}"
	export XTERM_LOGIN_OPTS
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

# If you're running on a diskless workstation that runs xdm itself
# then you probably have a server that's more powerful and on which
# you'd rather run the majority of your X applications.  Normally
# this will be the same server your home directory resides on....
#
if [ -n "$XDMHOST" -a "$XDMHOST" = "$(echo $DISPLAY | sed 's/:.*$//')" ]; then
	echo "$argv0: looks like '$XDMHOST' is running xdm for our display..."
	case "$XDMHOST" in
	almost.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		;;
	very.weird.com|very.robohack.planix.com)
		X11SERVER=${X11SERVER:-"once.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		;;
	*)
		# not setting X11SERVER hopefully just makes the WM run all
		# X11SERVER clients on the current login host....  unless it can
		# and does eliminate the menu entries entirely!
		unset X11SERVER
		echo "$argv0: have unset X11SERVER; XDMHOST is $XDMHOST"
		;;
	esac

	#
	# Common for all xdm startups
	#
	# XXX anything special???

fi

# else...

# argv0 is .xinitrc if called by xinit or startx
#
if [ "$argv0" = ".xinitrc" ]; then
	echo "$argv0: looks like X was started with xinit or startx..."
	#
	# setup for "remote" windows
	#
	case $DISPLAY in
	:*)
		case $REMOTE_DISPLAY in
		very.local:*|very-old.local:*)
			;;
		esac
		;;
	almost.weird.com)
		X11SERVER=${X11SERVER:-"most.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		;;
	very.weird.com:*|very.robohack.planix.com:*)
		X11SERVER=${X11SERVER:-"once.weird.com"}
		export X11SERVER
		echo "$argv0: have set X11SERVER='$X11SERVER'"
		;;
	esac

	#
	# Common for all xinit/startx startups
	#
	# XXX anything special???

elif $ONCONSOLE ; then
	#
	#	we're probably signing in via xdm on the system console....  maybe?
	#
	# I/O is already redirected to /dev/console above, so we will see it...
	#
	:
else
	#
	#	we're probably signing in on an independent xterminal....
	#
	# XXX I think these "tail"s could get left behind if nothing ever writes
	# to the file they are watching (so they'll never write anything more to
	# the closed pipe, and so never get a SIGPIPE....
	#
	if [ -r $HOME/.xsession-errors-$DISPLAY ]; then
		tail -F $HOME/.xsession-errors-$DISPLAY | xconsole $xconsole_args -file /dev/stdin &
	elif [ "$XODISPLAY" != "$DISPLAY" -a -r $HOME/.xsession-errors-$XODISPLAY ]; then
		tail -F $HOME/.xsession-errors-$XODISPLAY | xconsole $xconsole_args -file /dev/stdin &
	else
		echo "$argv0: didn't find ~/.xsession-errors-$DISPLAY, assuming old XDM config in use"
		tail -F $HOME/.xsession-errors | xconsole $xconsole_args -file /dev/stdin &
	fi
fi

#	Programs for everyone
#	====================

# if possible run a localhost mailbox monitor of some sort...
#
if $HAVEXBUFFY ; then
	if [ -n "$AUDIOPLAYER" -a -r $HOME/lib/sounds/newmail ]; then
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 -acmd "$AUDIOPLAYER $HOME/lib/sounds/newmail" &
	else
		xbuffy -geometry 80x25-0+235 -shortnames -header 0 &
	fi
elif $HAVEXBIFF ; then
	xbiff -geometry 48x48-0+235 -bw 0 &
fi

# set/start the root window display/app
#
if $HAVEXEARTH ; then
	XEGAMMA=""
	if [ ${SCREENDEPTH:-1} -ne 1 ]; then
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
elif $HAVEXV && [ -d '/Library/Desktop Pictures/Nature' ]; then
	xv -root '/Library/Desktop Pictures/Nature/'* "$HOME/Pictures/"*.jpg &
#elif $HAVEIMAGEMAGICK && [ -d "$HOME/Pictures" ]; then
#	# this is wonky and slow, but perhaps the best we can do...
#	# (nothing else sets the root window!)
#	display "vid:$HOME/Pictures/"'*.[gjp]*' vid:'/Library/Desktop Pictures/Nature/*.[gjp]*' &
elif [ -r $X11PATH/include/X11/bitmaps/escherknot ]; then
	xsetroot -bitmap $X11PATH/include/X11/bitmaps/escherknot
elif [ -r /usr/include/X11/bitmaps/escherknot ]; then
	xsetroot -bitmap /usr/include/X11/bitmaps/escherknot
else
	echo "$argv0: no background set"
fi

if [ ${SCREENDEPTH:-1} -eq 1 ]; then
	xload -geometry $XloadGeom -hl grey90 &
else
	xload -geometry $XloadGeom -hl red &
fi

XCLOCK="xclock"
if [ -x ${PKG}/bin/xclock ]; then
	XCLOCK=${PKG}/bin/xclock
fi
LC_CTYPE=C ${XCLOCK} -geometry $DClockGeom -face ${DefaultXftFont}:size=10 -digital -update 1 -chime &
if $HAVESWISSWATCH; then
	swisswatch -name swissclock -geometry $ClockGeom &
else
	${XCLOCK} -update 1 -geometry $ClockGeom &
fi

if [ -n "${X11SERVER}" ]; then
	# now move our AUTH cookies over to the server in case it
	# doesn't have a shared $HOME...
	# (note: don't run this in the background!)
	xauth nextract - ${REMOTE_DISPLAY} | onx11server -X \"${X11SERVER}\" 'xauth nmerge -'
	# now start server programs we like for this scenario
	if [ ${SCREENDEPTH:-1} -eq 1 ]; then
		onx11server -n -X \"${X11SERVER}\" 'xload -geometry 120x40-200+48 -hl grey90' &
	else
		onx11server -n -X \"${X11SERVER}\" 'xload -geometry 120x40-200+48 -hl red' &
	fi
	onx11server -n -X \"${X11SERVER}\" "sleep 5; uxterm ${XTERM_BIG_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X \"${X11SERVER}\" "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
fi

#	Default programs to start, by $DISPLAY
#	======================================
#
# now we start more programs we like depending on which $DISPLAY we're
# using, but which don't depend on whether X was started by XDM or
# xinit/startx.
#
case ${REMOTE_DISPLAY:-${DISPLAY}} in

very.local:*|very-old.local:*)
	# very has a very large, very hi-res, display....
	# very-old also has a quite large, quite hi-res, display...
	xset m 2 0

	# xxx the xclock supplied with XQuartz 2.8.2 is broken somehow in how it
	# displays text, but the pkgsrc build works OK....
	#
	# xxx width here is based on what's left to the left of xloads on a
	# high-res display, but there is no more space on a ~100dpi display so
	# it collides anyway....  120 will suffice everywhere for now
	XchronoOpts="-width ${XchronoWidth} -rv -update 1 -newyork -sydney -frankfurt"
	export XchronoOpts XchronoGeom
	if type xchrono >/dev/null 2>&1; then
		xchrono -geometry ${XchronoGeom} ${XchronoOpts} &
	fi

	# our own big main area terminal
	eval uxterm ${XTERM_BIG_OPTS} ${XTERM_OPTS} ${XTERM_LOGIN_OPTS} &

	# xxx so many ways to choose a font size -- the -fs sets the default,
	# but the -fc selects what is given in the "*VT100.font5" resource
	# (normally this will be 11.0)
	#
	uxterm -tn xterm-256color -geometry 80x25+$(expr \( $SCREENDIM_X \* $SCREENDPI_X / 2 \) - \( 3 \* $SCREENDPI_X \))-$(expr $SCREENDPI_Y \* 2) -fs 9 -fa "${DefaultXftFont}" ${XTERM_OPTS} -fc 5 -n AirTrafficMonitor -T AirTrafficMonitor -e air-traffic-monitor &
	#
	# probably available hosts...
	#
	# XXX N.B.:  note the filter to get only address-family "0" i.e. IPv4
	# addresses since we don't use IPv6 and loading IPv6 addresses in the
	# remote host may confuse it should it have code that supports IPv6 but
	# it is not configured properly to use IPv6 on the network....
	#
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X more.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X xenful.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X xentastic.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X nb10.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -S -X nbday.local 'xauth nmerge -'
	# xxx something goes wrong with the bridge started by UTM after some time and its VMs can't reach the main host!
	#xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X nbt.local 'xauth nmerge -'
	#xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X nbt2.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X nbtest.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X nbtcur.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -S -X fezzik.local 'xauth nmerge -'
	#xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -S -X frodo.local 'xauth nmerge -'
	#xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X historically.local 'xauth nmerge -'
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X once.local 'xauth nmerge -'
	# very-old runs in the background -- it's not used immediately below
	xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -S -X very-old.local 'xauth nmerge -' &
	#
	# XXX for starting windows it would be ideal if we could
	# tell the window manager to do these using its existing
	# menu operations!
	#
	# local server performance levels we want to be aware of
	xloadVoff="0"

	#xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	#XloadGeomBuilding=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	#export XloadGeomBuilding
	#onx11server -n -X building.local "xload -geometry ${XloadGeomBuilding -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomMore=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	export XloadGeomMore
	onx11server -n -X more.local "xload -geometry ${XloadGeomMore} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomCentral=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	export XloadGeomCentral
	onx11server -fSY -X central.weird.com "xload -geometry ${XloadGeomCentral} -hl red" &

	#xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	#XloadGeomFuture=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	#export XloadGeomFuture
	#onx11server -n -X future.local "xload -geometry ${XloadGeomFuture} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomXenful=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	export XloadGeomXenful
	onx11server -n -X xenful.local "xload -geometry ${XloadGeomXenful} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomXentastic=${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}
	export XloadGeomXentastic
	onx11server -n -X xentastic.local "xload -geometry ${XloadGeomXentastic} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomXentral="${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}"
	export XloadGeomXentral
	onx11server -fSY -X xentral.avoncote.ca "xload -geometry ${XloadGeomXentral} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	XloadGeomOnce="${XloadWidth}x${XloadHeight}-${ClockOffH}+${xloadVoff}"
	export XloadGeomOnce
	onx11server -n -X once.local "xload -geometry ${XloadGeomOnce} -hl red" &

	# aligned vertically with XloadGeomOnce, under the clocks!
  	XloadGeomFezzik="${XloadWidth}x${XloadHeight}-0+${xloadVoff}" # under the clocks!
	export XloadGeomFezzik
	onx11server -fS -X fezzik.local "xload -geometry ${XloadGeomFezzik} -hl red" &

	# aligned vertically with XloadGeomOnce, under the clocks
	XloadGeomB2="${XloadWidth}x${XloadHeight}-${XloadWidth}+${xloadVoff}"
	export XloadGeomB2
	onx11server -fSY -X b2.avoncote.ca "xload -geometry ${XloadGeomB2} -hl red" &

	# these are under the workspace manager, not quite aligned vertically
	# with adjacent xloads
	#
	xloadVoff=$(expr ${WSHeight} + ${TDBorderWidth} + ${TDBorderWidth})
	#
	XloadGeomNbtest="${XloadWidth}x${XloadHeight}-${WSOff}+${xloadVoff}"
	export XloadGeomNbtest
	onx11server -n -X nbtest.local "xload -geometry ${XloadGeomNbtest} -hl red" &

	xload2east=$(expr ${WSOff} + ${XloadWidth} + ${TDBorderWidth} + ${TDBorderWidth})

	XloadGeomNbtcur="${XloadWidth}x${XloadHeight}-${xload2east}+${xloadVoff}"
	export XloadGeomNbtcur
	onx11server -n -X nbtcur.local "xload -geometry ${XloadGeomNbtcur} -hl red" &

	xloadVoff=$(expr ${xloadVoff} + ${XloadHeight} + ${TDBorderWidth} + ${TDBorderWidth})

	XloadGeomNb10="${XloadWidth}x${XloadHeight}-${WSOff}+${xloadVoff}"
	export XloadGeomNb10
	onx11server -n -X nb10.local "xload -geometry ${XloadGeomNb10} -hl red" &

	XloadGeomNbday="${XloadWidth}x${XloadHeight}-${xload2east}+${xloadVoff}"
	export XloadGeomNbday
	onx11server -fS -X nbday.local "xload -geometry ${XloadGeomNbday} -hl red" &

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
			# XXX xmahjongg parses but does not correctly use offset
			# info from -g (i.e. --geometry) this is worked around
			# in ~/.ctwmrc with WindowGeometries.
			# xmahjongg -t real_x2 -g 2000x1500+2200+0 $MB &
			xmahjongg -t real_x2 -g 2000x1500 $MB &
		elif [ $SCREENDPI_Y -gt 110 ]; then
			echo "You might want to make a bigger tile set for XmahJongg."
			xmahjongg -g +1000+0 &
			onx11server -n -X more.local 'xmahjongg -t real_x2 -g 2000x1500 --bg $HOME/public_html/marble.gif' &
		else
			xmahjongg -g +1000+0 &
		fi
		unset MP MB
	fi

	onx11server -n -X more.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X once.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X nb10.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -fS -X nbday.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	#onx11server -n -X nbt.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X nbtest.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X nbtcur.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -fS -X fezzik.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X xenful.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	onx11server -n -X xentastic.local "sleep 5; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS}" &
	#
	# SSH-only hosts... (hopefully with auto-xauth via -Y,
	# but note the use of both '-f' and '&')
	#
	#
	# possibly available hosts -- one background sub-shell for host each...
	#
	#	(
	#		xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X isit.local 'xauth nmerge -'
	#		onx11server -n -X isit.local 'xload -geometry 120x40-70+240 -hl red' &
	#	) &
	#	(
	#		xauth nextract - ${REMOTE_DISPLAY} | awk '$1 == 0 {print}' | onx11server -X lilbit.local 'xauth nmerge -'
	#		onx11server -n -X lilbit.local 'xload -geometry 120x40-70+280 -hl red' &
	#	) &
	###
	### XXX these are obviously not ideal as they leave us with no clue as
	### to why a window disappears when the connection is dropped....
	###
	( sleep 5; exec uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} -e 'slogin -CY b2.avoncote.ca' & ) &
	( sleep 5; exec uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} -e 'slogin -CY central.avoncote.ca' & ) &
	( sleep 5; exec uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} -e 'slogin -CY xentral.avoncote.ca' & ) &
	###
	### XXX what we really want is to stuff an "ssh -Y hostname" command
	### into the xterm so that it would be the first command run by an
	### interactive terminal.
	###
	### One way to do this with a shell that supports $ENV would be to
	### create a temporary file containing the command to run, then set $ENV
	### to point at that file, and then to start the xterm as:
	###
	###	luxterm -e "PS1='lost connection to host $ ' ENV=$tmp_rcfile $SHELL"
	###
	### The trick is to figure out if/which $SHELL supports $ENV properly.
	###
	### And also tricky is how to remove the temporary file at the right
	### time.
	###
	### Or maybe they can just be prototype scripts, one per remote host,
	### and so they pre-exist and persist and don't have to be removed.

	# finally our own small lower-right command terminal
	#
	# (the rest, for "remote" windows, are done in xinit/startx section)
	#
	( sleep 5; eval exec uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} ${XTERM_LOGIN_OPTS} & ) &
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
	xload -geometry 120x40-200+0 -hl red &
	LC_CTYPE=C xclock -update 1 -geometry 190-0+196 -digital -chime -face lucidasans-9 &
	if $HAVESWISSWATCH; then
		swisswatch -name swissclock -geometry 190x190-0+0 &
	else
		xclock -update 1 -geometry 190x190-0+0 &
	fi
	# All the big main area terminals (starting with our own first one)
	eval uxterm ${XTERM_BIG_OPTS} ${XTERM_OPTS} ${XTERM_LOGIN_OPTS} &

	# All the small lower-right command terminals (starting with our own first one)
	( sleep 2; eval uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} ${XTERM_LOGIN_OPTS} & ) &
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
	# local server performance levels we want to be aware of
	$RSH -n most.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+48 -hl grey90 &
	$RSH -n once.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+144 -hl grey90 &
#	$RSH -n starting-out.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+192 -hl grey90 &
#	$RSH -n whats.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-200+242 -hl grey90 &
#	$RSH -n always.weird.com exec /usr/X11R6/bin/xload -geometry 120x40-70+242 -hl grey90 &

	# All the small lower-right command terminals (starting with our own first one)
	xterm -geometry 80x42-0-${XconsoleVoff} $XTERM_OPTS $XTERM_LITTLE_OPTS &

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
	( sleep 2; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} & ) &

	;;

almost.weird.com)
	;;

very.weird.com:*|very.robohack.planix.com:*)
	# we run these ones in the background because the hosts might not be up
	xauth nextract - $DISPLAY | onx11server -X most.weird.com 'xauth nmerge -' &
	xauth nextract - $DISPLAY | onx11server -X isit.weird.com 'xauth nmerge -' &
	xauth nextract - $DISPLAY | onx11server -X historically.weird.com 'xauth nmerge -' &
	;;

*)
	uxterm ${XTERM_BIG_OPTS} ${XTERM_OPTS} &
	( sleep 2; uxterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} & ) &
	;;

esac

# when the window manager exits so shall we...
#
eval exec $WM

### $WM || twm

# and if that fails then we try a last-ditch "trick"
#
if [ $? -ne 0 ]; then
	xterm ${XTERM_LITTLE_OPTS} ${XTERM_OPTS} -n FailSafe
fi
echo "$0: we are going down!!!" 1>&2

# XXX for supreme debugging of window-manager and xterm problems
### sleep 100000000

exit 1
