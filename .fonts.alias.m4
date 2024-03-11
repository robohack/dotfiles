! -*- conf-xdefaults -*-
!
!	private fonts.alias -- must be first to have effect
!
!#ident	"@(#)HOME:.fonts.alias.m4	37.5	24/03/11 11:53:15 (woods)"
!
! Reload with (this is done in ~/.xinitrc):
!
!	xrdb -quiet -cpp m4 -E -n $HOME/.fonts.alias.m4 | sed 1,2d > $HOME/.fonts/fonts.alias
!	xset fp rehash
!
! XXX the next chunk should probably be an include file, shared with .ctwmrc
! (and .Xdefaults)
!
! Note that although these m4 macros appear commented out in .Xdefaults syntax
! (e.g. in emacs), they are still visible to m4.  Keeping them in comments helps
! with line counting in error reports, but it does mess up paren balancing in
! emacs, sigh.
!
! changequote([, ])
!
!	Work out the screen resolution in order to specify default Font DPIs:
!
! n.b.:  [XY]_RESOLUTION values are in pixels per metre, for some stupid
! reason....  so 4500 dpM (/39.37) is just over 110 dpi
!
! define(HorizDPI, eval(((X_RESOLUTION * 100) / 3937) + ((((X_RESOLUTION * 1000) / 3937) % 10) >= 5)))
! define(VertDPI, eval(((Y_RESOLUTION * 100) / 3937) + ((((X_RESOLUTION * 1000) / 3937) % 10) >= 5)))
! define(ScreenDimX, eval(WIDTH / HorizDPI))
! define(ScreenDimY, eval(HEIGHT / VertDPI))
!
! N.B.:  If true screen resolution is within 10% of 100DPI it could make the
! most sense to simply claim 100DPI to avoid font-scaling artifacts for bitmap
! fonts, but since we already alias most font names that did refer to bitmap
! fonts into scalable fonts anyway, there is not much to gain by sticking to
! exactly 100DPI.
!
! ifelse(eval((HorizDPI / 100 == 1) && (HorizDPI % 100 <= 10)), 1,
!  [define(FontXDPI, HorizDPI)],
!  [define(FontXDPI, HorizDPI)]
! )
! ifelse(eval((VertDPI / 100 == 1) && (VertDPI % 100 <= 10)), 1,
!  [define(FontYDPI, VertDPI)],
!  [define(FontYDPI, VertDPI)]
! )
!
! XXX comment out most/all of this unless on a hi-res display
! ifelse(eval(FontYDPI > 110), 1, [
!
! remap the "cursor" font to the scalable cursors font, with the monitor DPI
! (the scaling for this one doesn't work right -- 16pts is too small on the
! iMac27, but is amply big on the XDR)
!
cursor       "-xfree86-cursor-medium-r-normal--0-160-FontXDPI-FontYDPI-p-0-adobe-fontspecific"
!
! remap the common default font aliases
!
! N.B.:  The following is essentially a copy of $X11FONTDIR/misc/fonts.alias!
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
! (of course neither commitmono nor liberation mono look quite right all round,
! especially since there are no condensed or semicondensed variants).
!
!
fixed        "-*-commitmono-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
variable     "-*-liberation sans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! add some new simple names to be used in ~/.Xdefaults
!
bold         "-*-commitmono-bold-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
italic       "-*-commitmono-medium-i-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
bolditalic   "-*-commitmono-bold-i-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
!
varbold      "-*-liberation sans-bold-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
varitalic    "-*-liberation sans-medium-i-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
varbolditalic "-*-liberation sans-bold-i-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
!
5x7          "-*-commitmono-medium-r-normal--0-55-FontXDPI-FontYDPI-m-0-iso8859-1"
5x7bold      "-*-commitmono-bold-r-normal--0-55-FontXDPI-FontYDPI-m-0-iso8859-1"
!
5x8          "-*-commitmono-medium-r-normal--0-58-FontXDPI-FontYDPI-m-0-iso8859-1"
5x8bold      "-*-commitmono-bold-r-normal--0-58-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x9          "-*-commitmono-medium-r-normal--0-65-FontXDPI-FontYDPI-m-0-iso8859-1"
6x9bold      "-*-commitmono-bold-r-normal--0-65-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x10         "-*-commitmono-medium-r-normal--0-72-FontXDPI-FontYDPI-m-0-iso8859-1"
6x10bold     "-*-commitmono-bold-r-normal--0-72-FontXDPI-FontYDPI-m-0-iso8859-1"
!
6x12         "-*-commitmono-medium-r-normal--0-86-FontXDPI-FontYDPI-m-0-iso8859-1"
6x12bold     "-*-commitmono-bold-r-normal--0-86-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! condensed would match better, but there is only one in in 'c'-width, not 'm',
! and its spacing is all wonky for most uses
!6x13         "-*-*-medium-r-semi condensed--0-92-FontXDPI-FontYDPI-c-0-iso8859-1"
!6x13bold     "-*-*-bold-r-semi condensed--0-92-FontXDPI-FontYDPI-c-0-iso8859-1"
! ubuntu mono is tiniest, but rarely installed
!6x13         "-*-ubuntu mono-medium-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
!6x13bold     "-*-ubuntu mono-bold-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
6x13         "-*-commitmono-medium-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
6x13bold     "-*-commitmono-bold-r-normal--0-92-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! bitstream seems a wee bit smaller
7x13         "-*-bitstream vera sans mono-medium-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-1"
7x13bold     "-*-bitstream vera sans mono-bold-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-1"
7x13euro     "-*-bitstream vera sans mono-medium-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-15"
7x13eurobold "-*-bitstream vera sans mono-bold-r-normal--0-93-FontXDPI-FontYDPI-m-0-iso8859-15"
!
7x14         "-*-commitmono-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
7x14bold     "-*-commitmono-bold-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
!
8x13         "-*-commitmono-medium-r-normal--0-94-FontXDPI-FontYDPI-m-0-iso8859-1"
8x13bold     "-*-commitmono-bold-r-normal--0-94-FontXDPI-FontYDPI-m-0-iso8859-1"
!
8x16         "-*-commitmono-medium-r-normal--0-115-FontXDPI-FontYDPI-m-0-iso8859-1"
8x16bold     "-*-commitmono-bold-r-normal--0-115-FontXDPI-FontYDPI-m-0-iso8859-1"
!
9x15         "-*-commitmono-medium-r-normal--0-110-FontXDPI-FontYDPI-m-0-iso8859-1"
9x15bold     "-*-commitmono-bold-r-normal--0-110-FontXDPI-FontYDPI-m-0-iso8859-1"
!
10x20        "-*-commitmono-medium-r-normal--0-144-FontXDPI-FontYDPI-m-0-iso8859-1"
10x20bold    "-*-commitmono-bold-r-normal--0-144-FontXDPI-FontYDPI-m-0-iso8859-1"
!
12x24        "-*-commitmono-medium-r-normal--0-172-FontXDPI-FontYDPI-m-0-iso8859-1"
12x24bold    "-*-commitmono-bold-r-normal--0-172-FontXDPI-FontYDPI-m-0-iso8859-1"
!
! End replacements for misc/fonts.alias, leaving out the less-used non-iso8859
! and OpenLook mappings.
!
!	Useful Font Aliases:
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
"-*-times-bold-i-normal--20-*-*-*-*-*-iso8859-1"	"-*-liberation serif-bold-i-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-iso8859-1"	"-*-liberation serif-medium-r-normal--0-90-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-*-*"		"-*-liberation serif-medium-r-normal--0-90-FontXDPI-FontYDPI-p-0-*-*"
!
"-*-*-medium-r-normal--16-*-*-*-*-*-*-*"		"-*-commitmono-medium-r-normal--0-90-FontXDPI-FontYDPI-m-0-*-*"
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
"-*-helvetica-medium-r-normal--12-*-*-*-*-*-iso8859-1"	"-*-liberation sans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-courier-medium-r-normal--12-*-*-*-*-*-iso8859-1"	"-*-courier-medium-r-normal--0-100-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-times-bold-i-normal--20-*-*-*-*-*-iso8859-1"	"-*-liberation serif-bold-i-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-r-normal--16-*-*-*-*-*-iso8859-1"	"-*-liberation serif-medium-r-normal--0-110-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! xv -- still tiny, but usable....
!
"-misc-fixed-medium-r-normal-*-13-*"			"-*-commitmono-medium-r-normal--0-70-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-courier-medium-r-*-*-12-*"				"-*-commitmono-medium-r-normal--0-70-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*"		"-*-liberation serif-medium-r-normal--0-70-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*"		"-*-liberation sans-medium-r-normal--0-70-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-r-*-*-11-*-*-*-*-*-*-*"		"-*-liberation sans-medium-r-normal--0-65-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! try some generic aliases -- these seem to help Ctwm with its "default" font
!
"-*-fixed-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-commitmono-medium-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-bold-r-normal--*-*-*-*-*-*-iso8859-1"		"-*-commitmono-medium-r-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-commitmono-medium-i-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
"-*-fixed-bold-i-normal--*-*-*-*-*-*-iso8859-1"		"-*-commitmono-medium-i-normal--0-*-FontXDPI-FontYDPI-m-0-iso8859-1"
!
"-*-times-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation serif-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-bold-r-normal--*-*-*-*-*-*-iso8859-1"		"-*-liberation serif-bold-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation serif-medium-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-times-bold-i-normal--*-*-*-*-*-*-iso8859-1"		"-*-liberation serif-bold-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
!
"-*-helvetica-medium-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation sans-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-bold-r-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation sans-medium-r-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-medium-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation sans-medium-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-helvetica-bold-i-normal--*-*-*-*-*-*-iso8859-1"	"-*-liberation sans-medium-i-normal--0-*-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! specific fonts wanted by gv:
!
"-*-Helvetica-Medium-R-Normal--*-140-*-*-P-*-ISO8859-1"	"-*-liberation sans-medium-r-normal--0-140-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Medium-R-Normal--*-120-*-*-P-*-ISO8859-1"	"-*-liberation sans-medium-r-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Medium-R-Normal--*-100-*-*-P-*-ISO8859-1"	"-*-liberation sans-medium-r-normal--0-100-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Bold-R-Normal--*-120-*-*-P-*-ISO8859-1"	"-*-liberation sans-bold-r-normal--0-120-FontXDPI-FontYDPI-p-0-iso8859-1"
"-*-Helvetica-Bold-R-Normal--*-180-*-*-P-*-ISO8859-1"	"-*-liberation sans-bold-r-normal--0-180-FontXDPI-FontYDPI-p-0-iso8859-1"
!
! These are in some library(ies):
!
"-*-*-*-R-*-*-*-120-*-*-*-*-ISO8859-*"	"-*-commitmono-*-r-normal--0-120-FontXDPI-FontYDPI-m-0-iso8859-*"
!
"-*-times-*-*-*--*-*-*-*-*-*-*-*"		"-*-liberation serif-*-*-*--0-*-FontXDPI-FontYDPI-*-0-*-*"
"-*-helvetica-*-*-*--*-*-*-*-*-*-*-*"		"-*-liberation sans-*-*-*--0-*-FontXDPI-FontYDPI-*-0-*-*"
!
! xxx this one is actually a part of a fontset, with the second appended, so it
! is last -- aliases do not seem to be powerful enough to manage this transform
!
!"-*-*-*-R-*-*-*-120-*-*-*-*"		"-*-freemono-*-r-normal--0-120-FontXDPI-FontYDPI-m-0-*-*"
!"*"					"-*-commitmono-*-*-*--0-90-FontXDPI-FontYDPI-*-0-*-*"
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
