! -*- conf-xdefaults -*-
!
!	.X11-m4macros.m4 - shared M4 macros for xrdb uses
!
!#ident	"@(#)HOME:.X11-m4macros.m4	37.2	24/10/14 15:02:04 (woods)"
!
! N.B.:  it is assumed quoting has been changed to square brackets!!!
! n.b.:  brackets will disappear from comments!
!
! N.B.:  There shall be no blank lines!  Every line herein should be an xrdb(1)
! comment!
!
! Note that although these m4 macros appear commented out in X11 (xdefaults)
! syntax (e.g. in emacs), they are still visible to m4.  Keeping them in
! comments helps with line counting in error reports, but it does sometimes mess
! up paren balancing in emacs, sigh.
!
! XXX N.B. XXX:  figure out how to make this usable by ~/.ctwmrc too as the
! screen resolutions macros are duplicated there.
!
!
! Screen Resolutions
! ==================
!
! Work out the screen resolution in order to specify default Font DPIs.
!
! n.b.:  resolutions are in pixels per metre, for some stupid reason....
! so 4500 dpM (/39.37) is just over 110 dpi (8594 dpM is 218 dpi)
!
! define(HorizDPI, eval(((X_RESOLUTION * 100) / 3937) + ((((X_RESOLUTION * 1000) / 3937) % 10) >= 5)))
! define(VertDPI, eval(((Y_RESOLUTION * 100) / 3937) + ((((Y_RESOLUTION * 1000) / 3937) % 10) >= 5)))
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
!  [define(FontXDPI, 100)],
!  [define(FontXDPI, HorizDPI)]
! )
! ifelse(eval((VertDPI / 100 == 1) && (VertDPI % 100 <= 10)), 1,
!  [define(FontYDPI, 100)],
!  [define(FontYDPI, VertDPI)]
! )
!
! XXX this is not really necessary, but an example of how to work out the number
! of pixels for a given point size (9.0)
!
! define(VertPix9pt, eval((VertDPI * (90000 / 720)) / 1000))
!
!
! Font Names and Specifications:
! ==============================
!
! Note for Xft using fc-list(1) is probably the best way to fint Xft fonts...
!
!	fc-list :scalable=true:spacing=100:lang=en: family
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
! When using Xft all possible usable mono-space fonts (for emacs and xterm,
! etc.) can be found with the following command:
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
! missing rather a large number of glyphs anyway!  but it is the only
! good-looking monospace font with serifs)
!
! define(DecentFont, [DejaVu Sans Mono])
! define(DecentFontSans, [DejaVu Sans])
! define(DecentFontSerif, [DejaVu Serif])
!
! End of .X11-m4macros.m4
!
