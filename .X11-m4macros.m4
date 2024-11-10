! -*- m4 -*-
!
!	.X11-m4macros.m4 - shared M4 macros for xrdb and ctwm uses
!
!#ident	"@(#)HOME:.X11-m4macros.m4	37.6	24/11/10 14:58:36 (woods)"
!
! This file is sourced for a couple of uses by ~/.xinitrc, once to define shell
! variables, and again to process ~/.fonts.alias.m4; and it is also sourced
! through a sed command (to change the comment character) by ~/.ctwmrc.
!
! N.B.:  M4 quoting must be changed to '[' and ']' before including this file!
!
! n.b.:  as a result brackets will disappear from comments!
!
! N.B.:  There shall be no blank lines!  Every line herein should be an xrdb(1)
! comment!  Note magic happens with M4 on multi-line macros with ifelse(), etc.,
! as long as their arguments are all carefully quoted.
!
! Note that although these m4 macros appear commented out in X11 (xdefaults)
! syntax (e.g. in emacs), they are still visible to m4.  Keeping them in
! comments helps with line counting in error reports, at least for xrdb(1)
! itself.
!
! Screen Resolutions
! ==================
!
! Work out the screen resolution in order to specify default Font DPIs.
!
! Note none of this would be necessary if the Xserver (or libX11?) were to fill
! in wildcard (*) or "0" values for "resx" and "resy" with its known screen DPI.
!
! n.b.:  resolutions are in pixels per metre, for some stupid reason, so we can
! divide by the number of inches in a metre (39.370079) to get dots-per-inch:
!
! define(HorizDPI, eval(((X_RESOLUTION * 100) / 3937) + ((((X_RESOLUTION * 1000) / 3937) % 10) >= 5)))
! define(VertDPI, eval(((Y_RESOLUTION * 100) / 3937) + ((((Y_RESOLUTION * 1000) / 3937) % 10) >= 5)))
!
! Also work out the approximate dimensions in inches
!
! define(ScreenDimX, eval(WIDTH / HorizDPI))
! define(ScreenDimY, eval(HEIGHT / VertDPI))
!
! N.B.:  If the true screen resolution is within 10% of 100DPI it could make
! sense to use the 100DPI fonts to avoid font-scaling artifacts for bitmap
! fonts, but since we already alias most font names that did refer to bitmap
! fonts into scalable fonts anyway, there is not much to gain by sticking to
! exactly 100DPI....
!
! ifelse(eval((HorizDPI / 100 == 1) && (HorizDPI % 100 <= 10)), 1,
!   [define(FontXDPI, 100)],
!   [define(FontXDPI, HorizDPI)]
! )
!
! ifelse(eval((VertDPI / 100 == 1) && (VertDPI % 100 <= 10)), 1,
!   [define(FontYDPI, 100)],
!   [define(FontYDPI, VertDPI)]
! )
!
! Font Sizes in Pixels
! ====================
!
! What we really want here is the maximum bounding box for a given font size,
! e.g. for Xterm which wants its geometry in character cells, not overall
! pixels.  We could learn this from the "window size hints" it gives to the
! window manager, but we'd need to start a prototype xterm with each given font
! of the desired size to query for those values (e.g. using "xwininfo -size").
!
! From "X Logical Font Description":
!
!	DeciPointsPerInch = 722.7
!	PIXEL_SIZE = ROUND((RESOLUTION_Y * POINT_SIZE) / DeciPointsPerInch)
!
! The XLFD spec defines the POINT_SIZE field as: "An integer giving the size of
! an em-square in decipoints".
!
! This is one way to work out the number of pixels in height for a given point
! size.  In digital typefaces a font's point size is defined by the _width_ of a
! capital M at that point size (in "points", i.e. units of 1/72.27 of an
! inch)[*], and although it is normally an almost square character, that doesn't
! take into account the descender.  What we really want is the distance between
! one baseline and the next, which is the sum of the body height and the
! leading, often expressed as "characters per inch vertically" (as in RFC 678)
! or lines of text per inch.
!
! The XFLD spec says:
!
!	The logical extent of the font is inclusive between the Y-coordinate
!	values:  Y - FONT_ASCENT and Y + FONT_DESCENT + 1.
!
! So if we could retrieve the calculated FONT_ASCENT and FONT_DESCENT (which we
! can do with "xlsfonts -l") then we could in theory calculate the inter-line
! spacing as simply:
!
!	FONT_ASCENT + FONT_DESCENT.
!
! However xterm does this (starting with the actual ascender and descender
! sizes) and ends up with, on average, about 23% more than the pixel height in
! points (at least for these "normal: sized fonts)!  E.g. for a 9.0pt, with
! FONT_ASCENT=28, FONT_DESCENT=10, xterm calculates its "y resize increment" as
! 33, so:  (XXX maybe these should be rounded up as is done for the DPI values?)
!
! define(VertPix75dpt, eval((VertDPI * ((7500 * 123) / 7227)) / 1000))
! define(VertPix90dpt, eval((VertDPI * ((9000 * 123) / 7227)) / 1000))
! define(VertPix100dpt, eval((VertDPI * ((10000 * 123) / 7227)) / 1000))
!
!
! Window and Window Element Size and Positioning macros
! =====================================================
!
! *DPI could be used here, but [XY]_RESOUTION is the native value, avoiding
! rounding:
!
!	4000 dpM is ~101 dpi
!	4288 dpM is ~109 dpi	# iMac 27"
!	8000 dpM is ~203 dpi
!	8594 dpM is ~218 dpi	# XDR Pro
!	12000 dpM is ~304 dpi
!	13100 dpM is ~331 dpi	# ASUS Zenbook
!
! These are used by both ~/.ctwmrc and ~/.xinitrc
!
! ifelse(eval(X_RESOLUTION >= 12000 && Y_RESOLUTION >= 12000), 1,
!   [define(BORDERWIDTH, [12]) define(TDBORDERWIDTH, [14]) define(ShadowDepth, [7])],
! [ifelse(eval(X_RESOLUTION >= 8000 && Y_RESOLUTION >= 8000), 1,
!   [define(BORDERWIDTH, [8]) define(TDBORDERWIDTH, [10]) define(ShadowDepth, [5])],
! [ifelse(eval(X_RESOLUTION >= 4500 && Y_RESOLUTION >= 4500), 1,
!   [define(BORDERWIDTH, [6]) define(TDBORDERWIDTH, [8]) define(ShadowDepth, [4])],
!   [define(BORDERWIDTH, [4]) define(TDBORDERWIDTH, [6]) define(ShadowDepth, [3])]
! )]
! )]
! )
!
! The TitleHeight in the CTWM code is: Scr->TitleBarFont.height + Scr->FramePadding * 2;
! plus: if (Scr->use3Dtitles) Scr->TitleHeight += 2 * Scr->TitleShadowDepth
! FramePadding=0 (and is the default) with UseThreeDTitles set
!
! XXX in ~/.ctwmrc the TitleFont is set as a 9.0pt font, and so with the poor
! approximation of VertPix90dpt we can try to calculate a title hight as:
!
! define(TitleHeight, eval(VertPix90dpt + (2 * ShadowDepth)))
!
!
! Some Pleasing Xclock and Xload sizing
! =====================================
!
! A 2"x2" main clock is OK on larger screens...
!
! ifelse(eval(ScreenDimX > 12), 1,
!   [define(ClockSize, [2])],
!   [define(ClockSize, [1])]
! )
!
! The main clock might look like a circle, but it occupies a square!
!
! define(ClockWidth, eval(HorizDPI * ClockSize))
! define(ClockHeight, eval(VertDPI * ClockSize))
!
! The borderwidth adjustment is actually for the digital clock....
!
! define(ClockOffH, eval(ClockWidth + (2 * TDBORDERWIDTH)))
! define(ClockOffV, eval(ClockHeight + (2 * TDBORDERWIDTH)))
!
! The Digital clock should always be the same width as the main clock
!
! ifelse(eval(ScreenDimY > 6), 1,
!   [define(DClockHeight, eval(HorizDPI / 4))],
!   [define(DClockHeight, eval(HorizDPI / 8))]
! )
!
! define(ClockGeom, ClockWidth[x]ClockHeight[-0+0])
! define(DClockGeom, ClockWidth[x]DClockHeight[-0+]ClockOffV)
!
! Xload is one inch by 1/3 inch
!
! define(XloadWidth, eval(HorizDPI * 1))
! define(XloadHeight, eval(VertDPI / 3))
!
! define(XloadGeom, XloadWidth[x]XloadHeight[-]ClockOffH[+0])
!
! Once upon a time I had different WorkSpace layouts on different xterminals,
! but for now I'll go back to one consitent layout.
!
! define(WS_COLS, [3])
! define(WS_ROWS, [4])
! define(WS_HEIGHT, eval(HEIGHT / 30 * WS_ROWS))
! define(WS_WIDTH, eval(WIDTH / 30 * WS_COLS))
! define(WS_GEOM, WS_WIDTH[x]WS_HEIGHT)
!
! XXX ToDo on a really wide screen, like the XDR, we might have two Xload
! columns (and still be able to have two 132-column emacs windows)
!
! define(WS_OFF, eval(ClockOffH + XloadWidth + (2 * TDBORDERWIDTH)))
!
! define(XchronoWidth, 120)
! define(XchronoGeom, [-]eval(WS_OFF + WS_WIDTH - XchronoWidth)[+]eval(WS_HEIGHT + (2 * TDBORDERWIDTH)))
!
!
! Font Names and Specifications:
! ==============================
!
! Note for Xft using fc-list(1) is probably the best way to find Xft fonts...
! So to find all possible usable mono-space fonts (e.g. for emacs and xterm,
! etc.) use the following command:
!
!	fc-list :scalable=true:spacing=100:lang=en: family
!
! To find all the usable fonts with all their available styles:  (XXX for some
! reason "fc-match -s" does not find some of the best fonts such as "Liberation
! Mono" and "CommitMono" -- I am not sure why, or what the manual means by
! "best")
!
!	fc-list :scalable=true:spacing=100:lang=en: family style | sort
!
! (rule out those with the fewest styles, e.g. "CentSchbook Mono BT", Crystal,
! "Consola Mono", "Oxygen Mono", "PT Mono", and "Quinze")
!
! See "Font Name" in the following for how XftPatterns are specified:
!
!	https://fontconfig.pages.freedesktop.org/fontconfig/fontconfig-user.html
!
! The best looking is perhaps "commitmono", most complete is "DejaVu Sans Mono",
! but "Liberation Mono" might look better in some areas and is a default X11
! font with all four regular styles.  "Hack" is fairly complete too, but ugly.
!
! (of course neither dejavu sans mono nor commitmono look quite right all round,
! especially since there are no condensed or semicondensed variants).
!
! Freemono is a possible alternative (even more glyphs than liberation mono, but
! no mark in the zero) (from the fonts/freefont-ttf package).
!
! "IBM Plex Mono" and "Cascadia Mono" are more alternatives (not quite as many
! glyphs).
!
! N.B.:  Xft does seem able to find fonts with the spaces removed from their
! "fullname", e.g. "DejaVuSansMono" will find "DejaVu Sans Mono".  Phew!  (This
! seems to be due to testing with a rule: "Equal(ignore blanks)") However so far
! this has not been necessary in any resources using the DecentFont macro for
! Xft font-specs (but see ~/.xinitrc where we strip the blanks to avoid shell
! quoting nightmares).
!
! N.B.:  These are also used in XFLD specs so the mono one must have a 'm'
! spacing, and the others must have a 'p' spacing.  ("luxi mono" is the only
! default X11 monospaced font with a 'c' spacing, so it is unusable -- but it is
! missing rather a large number of glyphs anyway!  though it is the only
! good-looking monospace font with serifs)
!
! N.B.:  Currently with the DejaVu fonts, in addition to the normal slant="r",
! the DejaVuSansMono and DejaVuSans fonts only have slant="o" while DejaVuSerif
! only has slant="i" (and DejaVuSansLite has no italic/oblique face).
!
! xxx note the *Spc macros are not yet used....  Are they really needed?  Maybe
! just the one for monospace, which could be 'm' or 'c'
!
! define(DecentFont, [DejaVu Sans Mono])
! define(DecentFontSlant, [o])
! define(DecentFontSpc, [m])
! define(DecentFontSans, [DejaVu Sans])
! define(DecentFontSansSlant, [o])
! define(DecentFontSansSpc, [p])
! define(DecentFontSerif, [DejaVu Serif])
! define(DecentFontSerifSlant, [i])
! define(DecentFontSerifSpc, [p])
!
! On a smaller, say laptop, screen smaller fonts may be nicer (xxx but only if
! DPI is "high enough")?
!
! ifelse(eval(ScreenDimY <= 8), 1,
!  [
!   define(HugeDeciPt, [110])
!   define(HugePt, [11])
!
!   define(BiggerDeciPt, [90])
!   define(BiggerPt, [9])
!
!   define(DecentDeciPt, [80])
!   define(DecentPt, [8])
!
!   define(SmallerDeciPt, [70])
!   define(SmallerPt, [7])
!
!   define(TinyDeciPt, [60])
!   define(TinyPt, [6])
!  ], [
!   define(HugeDeciPt, [120])
!   define(HugePt, [12])
!
!   define(BiggerDeciPt, [100])
!   define(BiggerPt, [10])
!
!   define(DecentDeciPt, [90])
!   define(DecentPt, [9])
!
!   define(SmallerDeciPt, [75])
!   define(SmallerPt, [7.5])
!
!   define(TinyDeciPt, [60])
!   define(TinyPt, [6])
!  ])
!
! End of .X11-m4macros.m4
!
