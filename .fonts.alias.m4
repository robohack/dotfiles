! -*- conf-xdefaults -*-
!
!	private fonts.alias -- must be first to have effect
!
!#ident	"@(#)HOME:.fonts.alias.m4	37.9	24/10/20 14:58:49 (woods)"
!
! changequote([, ])	n.b.:  brackets will disappear from comments!
!
! N.B. WARNING:  This file must be processed with "xrdb -cpp m4 -E" with macros
! and macro calls in the comments.
!
! Reload with (this is done initially in ~/.xinitrc):
!
!	xrdb -quiet -cpp m4 -E -n $HOME/.fonts.alias.m4 | sed 1,2d > $HOME/.fonts/fonts.alias
!	xset fp rehash
!
! You can see the preprocessor macro values with "xrdb -symbols".
!
! include(.X11-m4macros.m4)
!
! Note that although the M4 macros appear commented out in .Xdefaults syntax,
! they are still visible to m4.  Keeping them in comments helps line counting.
!
! N.B.:  There shall be no blank lines!  Every line herein should be an xrdb(1)
! comment!  The format is two white-space separated columns, the first
! containing aliases and the second containing font-name patterns.  Lines
! beginning with "!" are comment lines and are ignored.  To embed white space in
! either name, simply enclose it in double-quote marks; to embed double-quote
! marks (or any other character), precede them with back-slash.
!
! XXX comment out most/all of this unless on a hi-res display
! ifelse(eval(FontYDPI > 110), 1, [
!
! N.B.:  Applications using Xt font support emit a warning as follows with any
! LC_CTYPE other than "C".
!
!	Warning: Missing charsets in String to FontSet conversion
!
! Note this also means font specs for them should only be using iso8859-*.
!
!	Scalable Cursors!
!	=================
!
! remap the "cursor" font to the scalable cursors font, with the monitor DPI
! (the scaling for this one does not work right -- 16pts is too small on the
! iMac27, but is amply big on the XDR)
!
cursor       "-xfree86-cursor-medium-r-normal--0-160-FontXDPI-FontYDPI-p-0-adobe-fontspecific"
!
!
!	The Old Short Pixel-size Abbreviations
!	======================================
!
! N.B.:  The following is essentially a copy of $X11FONTDIR/misc/fonts.alias!
!
! remap the common default font aliases
!
! Assuming the original resolution was 75dpi (and that these names are actually
! representing pixel dimmensions), makes them still way too big despite the fact
! these point sizes are smaller than those "suggested" in the actual
! misc/fonts.alias, given that commenting them out makes xfig look normal on the
! imac27.  Assuming 100dpi is much closer to expectations, though still a bit
! big if left uncommented on the imac27.  Perhaps this is because many X11
! developers, including for xfig, were using these fonts on an actual 75dpi
! screen so they only allowed room in boxes, etc. for their apparent size, not
! for their actual size on a 75dpi screen.  This seems to match:  pix/75*72*.75
!
fixed        "-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
variable     "-*-DecentFontSans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! add some new simple names to be used in ~/.Xdefaults
!
bold         "-*-DecentFont-bold-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
italic       "-*-DecentFont-medium-o-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
bolditalic   "-*-DecentFont-bold-o-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
!
varbold      "-*-DecentFontSans-bold-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
varitalic    "-*-DecentFontSans-medium-o-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
varbolditalic "-*-DecentFontSans-bold-o-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
!
5x7          "-*-DecentFont-medium-r-normal--0-55-FontXDPI-FontYDPI-m-0-iso8859-1"
5x7bold      "-*-DecentFont-bold-r-normal--0-55-FontXDPI-FontYDPI-m-0-iso8859-1"
!
5x8          "-*-DecentFont-medium-r-normal--0-58-FontXDPI-FontYDPI-m-0-iso8859-1"
5x8bold      "-*-DecentFont-bold-r-normal--0-58-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x9          "-*-DecentFont-medium-r-normal--0-65-FontXDPI-FontYDPI-m-0-iso8859-1"
6x9bold      "-*-DecentFont-bold-r-normal--0-65-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x10         "-*-DecentFont-medium-r-normal--0-72-FontXDPI-FontYDPI-m-0-iso8859-1"
6x10bold     "-*-DecentFont-bold-r-normal--0-72-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x12         "-*-DecentFont-medium-r-normal--0-86-FontXDPI-FontYDPI-m-0-iso8859-1"
6x12bold     "-*-DecentFont-bold-r-normal--0-86-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! condensed would match better, but there is only one in in 'c'-width, not 'm',
! and its spacing is all wonky for most uses
!6x13         "-*-*-medium-r-semi condensed--0-92-FontXDPI-FontYDPI-c-0-iso8859-1"
!6x13bold     "-*-*-bold-r-semi condensed--0-92-FontXDPI-FontYDPI-c-0-iso8859-1"
! ubuntu mono is tiniest, but rarely installed
!6x13         "-*-ubuntu mono-medium-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
!6x13bold     "-*-ubuntu mono-bold-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
6x13         "-*-DecentFont-medium-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
6x13bold     "-*-DecentFont-bold-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! bitstream seems a wee bit more compact
7x13         "-*-bitstream vera sans mono-medium-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-1"
7x13bold     "-*-bitstream vera sans mono-bold-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-1"
7x13euro     "-*-bitstream vera sans mono-medium-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-15"
7x13eurobold "-*-bitstream vera sans mono-bold-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-15"
!
7x14         "-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
7x14bold     "-*-DecentFont-bold-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
!
8x13         "-*-DecentFont-medium-r-normal--0-94-FontXDPI-FontYDPI-m-0-iso8859-1"
8x13bold     "-*-DecentFont-bold-r-normal--0-94-FontXDPI-FontYDPI-m-0-iso8859-1"
!
8x16         "-*-DecentFont-medium-r-normal--0-115-FontXDPI-FontYDPI-m-0-iso8859-1"
8x16bold     "-*-DecentFont-bold-r-normal--0-115-FontXDPI-FontYDPI-m-0-iso8859-1"
!
9x15         "-*-DecentFont-medium-r-normal--0-110-FontXDPI-FontYDPI-m-0-iso8859-1"
9x15bold     "-*-DecentFont-bold-r-normal--0-110-FontXDPI-FontYDPI-m-0-iso8859-1"
!
10x20        "-*-DecentFont-medium-r-normal--0-144-FontXDPI-FontYDPI-m-0-iso8859-1"
10x20bold    "-*-DecentFont-bold-r-normal--0-144-FontXDPI-FontYDPI-m-0-iso8859-1"
!
12x24        "-*-DecentFont-medium-r-normal--0-172-FontXDPI-FontYDPI-m-0-iso8859-1"
12x24bold    "-*-DecentFont-bold-r-normal--0-172-FontXDPI-FontYDPI-m-0-iso8859-1"
!
!	Fix Any SILLY 100dpi to 75dpi Mappings
!	======================================
!
-misc-fixed-medium-r-normal--7-50-100-100-c-50-iso8859-1	-*-DecentFont-medium-r-normal--0-70-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--8-60-100-100-c-50-iso8859-1	-*-DecentFont-medium-r-normal--0-80-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--9-80-100-100-c-60-iso8859-1	-*-DecentFont-medium-r-normal--0-90-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--10-70-100-100-c-60-iso8859-1	-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-semicondensed--12-90-100-100-c-60-iso8859-1	-*-DecentFont-medium-r-semicondensed--0-110-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-semicondensed--13-100-100-100-c-60-iso8859-1	-*-DecentFont-medium-r-semicondensed--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-bold-r-semicondensed--13-100-100-100-c-60-iso8859-1	-*-DecentFont-bold-r-semicondensed--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--13-100-100-100-c-70-iso8859-1	-*-DecentFont-medium-r-normal--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-bold-r-normal--13-100-100-100-c-70-iso8859-1	-*-DecentFont-bold-r-normal--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--13-100-100-100-c-80-iso8859-1	-*-DecentFont-medium-r-normal--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-bold-r-normal--13-100-100-100-c-80-iso8859-1	-*-DecentFont-bold-r-normal--0-120-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--14-110-100-100-c-70-iso8859-1	-*-DecentFont-medium-r-normal--0-130-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--15-120-100-100-c-90-iso8859-1	-*-DecentFont-medium-r-normal--0-140-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-bold-r-normal--15-120-100-100-c-90-iso8859-1	-*-DecentFont-bold-r-normal--0-140-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-normal--20-140-100-100-c-100-iso8859-1	-*-DecentFont-medium-r-normal--0-200-FontXDPI-FontYDPI-m-0-iso8859-1
-misc-fixed-medium-r-semicondensed--13-100-100-100-c-60-iso8859-8	-*-DecentFont-medium-r-semicondensed--0-120-FontXDPI-FontYDPI-m-0-iso8859-8
-misc-fixed-medium-r-normal--13-100-100-100-c-80-iso8859-8	-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-8
-sony-fixed-medium-r-normal--16-150-75-75-c-80-iso8859-1	-*-DecentFont-medium-r-normal--0-150-100-100-m-0-iso8859-1
!-sony-fixed-medium-r-normal--16-150-75-75-c-80-jisx0201.1976-0	-*-DecentFont-medium-r-normal--0-150-100-100-m-0-jisx0201.1976-0
-sony-fixed-medium-r-normal--24-230-75-75-c-120-iso8859-1	-*-DecentFont-medium-r-normal--0-230-100-100-m-0-iso8859-1
!-sony-fixed-medium-r-normal--24-230-75-75-c-120-jisx0201.1976-0	-*-DecentFont-medium-r-normal--0-230-100-100-m-0-jisx0201.1976-0
!-jis-fixed-medium-r-normal--16-110-100-100-c-160-jisx0208.1983-0	-*-DecentFont-medium-r-normal--0-110-FontXDPI-FontYDPI-m-0-jisx0208.1983-0
!-jis-fixed-medium-r-normal--24-170-100-100-c-240-jisx0208.1983-0	-*-DecentFont-medium-r-normal--0-170-FontXDPI-FontYDPI-m-0-jisx0208.1983-0
!
! End replacements for misc/fonts.alias, leaving out the less-used non-iso8859
! and OpenLook mappings.
!
!
!	Useful Font Aliases:
!	====================
!
! XXX there should be an option for Xft(?) that says to totally ignore bitmap
! fonts, and to ignore all pixel size values in requests if there is also a
! point size in the request -- and of course to fill in the actual screen
! resolution when either '*' or '0' is given for each/either.
!
! The next ones quiet down xpdf, but they do not put glyphs in its menus....
!
! XXX XXX XXX and also magically one of the entries below the first affects the
! Ctwm "identify" window!  (but the original font was monospaced (and
! sans-serif), this makes it proportional and serifed, and also still the wrong
! size) Luckily something below, probably the "-*-fixed-*" entries, "fixes" it
! again, properly.
!
"-*-times-bold-i-normal--20-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-bold-i-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-medium-r-normal--0-90-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-*-*"		"-*-DecentFontSerif-medium-r-normal--0-90-FontXDPI-FontYDPI-p-0-iso8859-*"
!
"-*-*-medium-r-normal--16-*-*-*-*-*-*-*"		"-*-DecentFont-medium-r-normal--0-90-FontXDPI-FontYDPI-m-0-iso8859-*"
!
! these are from the ctwm code post 4.0.3
!
! N.B.:  Note that although the original name might be an abbreviated
! specification, the result cannot seem to be abbreviated!  (Perhaps because
! aliases cannot refer to other aliases?)
!
! DECENT_NICE_FONT
"-*-helvetica-bold-r-normal-*-*-120-*"				"-*-DecentFontSans-bold-r-normal-*-0-100-FontXDPI-FontYDPI-p-0-iso8859-*"
"-adobe-helvetica-bold-r-normal-*-*-120-*"			"-*-DecentFontSans-bold-r-normal-*-0-100-FontXDPI-FontYDPI-p-0-iso8859-*"
"-adobe-helvetica-bold-r-normal-*-*-100-*"			"-*-DecentFontSans-bold-r-normal-*-0-90-FontXDPI-FontYDPI-p-0-iso8859-*"
"-adobe-helvetica-bold-r-normal--*-100-*-*-*"			"-*-DecentFontSans-bold-r-normal-*-0-90-FontXDPI-FontYDPI-p-0-iso8859-*"
! DECENT_FAST_FONT
"-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-*"	"-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-*"
! workSpaceMgr.windowFont
"-adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1"	"-*-DecentFont-medium-r-normal--0-90-FontXDPI-FontYDPI-m-0-iso8859-1"
! deffontname (is actually a fontset, but aliases can't handle fontsets(?))
!"fixed,*"							"-*-DecentFont-medium-r-normal--0-90-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! twm
!
"-*-courier-bold-o-normal-*-*-100-100-100-*-*-*-*"              "-*-DecentFont-bold-o-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-*"
!
! Xcalc needs an "-adobe-symbol-*" equivalent for square-root and Pi
!
! try:  xfontsel -scaled -pattern "-*-*-*-*-*-*-*-120-FontXDPI-FontYDPI-*-*-adobe-fontspecific"
!
! In the end the best solution is probably to copy the symb*.pcf.gz files from
! ${X11FONTDIR} to ~/.fonts.
!
! Explicit aliases for default resources in xpdf
! xxx something else still gets too-big fonts for menus and some buttons
!
"-*-helvetica-medium-r-normal--12-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-courier-medium-r-normal--12-*-*-*-*-*-iso8859-1"	"-*-DecentFont-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-times-bold-i-normal--20-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-bold-i-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-medium-r-normal--0-110-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! xv -- still tiny, but usable....
!
"-misc-fixed-medium-r-normal-*-13-*"			"-*-DecentFont-medium-r-normal--0-70-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-courier-medium-r-*-*-12-*"				"-*-DecentFont-medium-r-normal--0-70-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*"		"-*-DecentFontSerif-medium-r-normal--0-70-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*"		"-*-DecentFontSans-medium-r-normal--0-70-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-r-*-*-11-*-*-*-*-*-*-*"		"-*-DecentFontSans-medium-r-normal--0-65-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! try some generic aliases -- these seem to help Ctwm with its "default" font
!
"-*-fixed-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-medium-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-bold-r-normal--*-*-*-*-*-*-iso8859-1"		"-*-DecentFont-bold-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-medium-o-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-bold-i-normal--*-*-*-*-*-*-iso8859-1"		"-*-DecentFont-bold-o-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-misc-fixed-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-medium-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-misc-fixed-bold-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-bold-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-misc-fixed-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-medium-o-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-misc-fixed-bold-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFont-bold-o-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
!
"-*-variable-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-variable-bold-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-bold-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-variable-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-medium-o-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-variable-bold-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-bold-o-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
!
"-*-times-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-bold-r-normal--*-*-*-*-*-*-iso8859-1"		"-*-DecentFontSerif-bold-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSerif-medium-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-bold-i-normal--*-*-*-*-*-*-iso8859-1"		"-*-DecentFontSerif-bold-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
!
"-*-helvetica-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-bold-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-bold-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-medium-o-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-bold-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-DecentFontSans-bold-o-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! specific fonts wanted by gv:
!
"-*-Helvetica-Medium-R-Normal--*-140-*-*-P-*-ISO8859-1"	"-*-DecentFontSans-medium-r-normal--0-140-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Medium-R-Normal--*-120-*-*-P-*-ISO8859-1"	"-*-DecentFontSans-medium-r-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Medium-R-Normal--*-100-*-*-P-*-ISO8859-1"	"-*-DecentFontSans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Bold-R-Normal--*-120-*-*-P-*-ISO8859-1"	"-*-DecentFontSans-bold-r-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Bold-R-Normal--*-180-*-*-P-*-ISO8859-1"	"-*-DecentFontSans-bold-r-normal--0-180-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! These are in some library(ies):
!
"-*-*-*-R-*-*-*-120-*-*-*-*-ISO8859-*"		"-*-*-*-r-*--0-120-FontXDPI-FontYDPI-*-0-iso8859-*"
!
"-*-times-*-*-*--*-*-*-*-*-*-*-*"		"-*-DecentFontSerif-*-*-*--0-*-FontXDPI-FontYDPI-*-0-iso8859-*"
"-*-helvetica-*-*-*--*-*-*-*-*-*-*-*"		"-*-DecentFontSans-*-*-*--0-*-FontXDPI-FontYDPI-*-0-iso8859-*"
!
"-*-*sans-bold-r-normal--*-100-*-*-p-*-iso8859-1"	"-*-DecentFontSans-bold-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-*-medium-r-*--*-100-*-*-m-*-iso8859-1"		"-*-*-medium-r-*--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! xxx this one is actually a part of a fontset, with the second appended, so it
! is last -- aliases do not seem to be powerful enough to manage this transform
!
"-*-*-*-R-*-*-*-120-*-*-*-*"		"-*-*-*-r-*--0-120-FontXDPI-FontYDPI-*-0-iso8859-*"
"*"					"-*-DecentFont-*-*-*--0-90-FontXDPI-FontYDPI-*-0-iso8859-*"
!
!
! End of part for DPI > 110
! ], [])
!
! Extra Ideas:
! ===========
!
! Some people also provide "pre-defined" point sizes for scalable fonts, though
! I'm not so sure yet if these are wise and/or useful:
!
! -monotype-Arial-medium-r-normal--*-110-0-0-p-0-iso8859-1	-monotype-Arial-medium-r-normal--0-110-FontXDPI-FontYDPI-p-0-iso8859-1
!
! (referenced from https://tldp.org/HOWTO/archived/FDU/truetype.html#FALIAS,
! where the common old mistake of not understanding how to specify actual screen
! resolution for scaling is made and perpetuated)
