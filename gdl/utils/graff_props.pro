; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Graff_props, file, pdefs, title = title, subtitle = subtitle, $
                 charsize = charsize, thick = thick, $
                 corners = corners, $
                 aspect = aspect, comment = comment, xtitle = xtitle, $
                 xrange = xrange, xlog = xlog, xl_bands = xl_bands, $
                 xexact = xexact, $
                 xextend = xextend, xaxes = xaxes, xbox = xbox, $
                 xminor = xminor, xtime = xtime, xorigin = xorigin, $
                 xgrid = xgrid, xauto = xauto, xannotate = xannotate, $
                 xmajor = xmajor, xtickv = xtickv, $
                 ytitle = ytitle, $
                 yrange = yrange, ylog = ylog, yl_bands = yl_bands, $
                 yexact = yexact, $
                 yextend = yextend, yaxes = yaxes, ybox = ybox, $
                 yminor = yminor, ytime = ytime, yorigin = yorigin, $
                 ygrid = ygrid, yauto = yauto, yannotate = yannotate, $
                 ymajor = ymajor, ytickv = ytickv, $
                 yr_enable = yr_enable, $
                 yrtitle = yrtitle, $
                 yrmajor = yrmajor, yrtickv = yrtickv, $
                 yrrange = yrrange, yrlog = yrlog, yrl_bands = $
                 yrl_bands, $
                 yrexact = yrexact, $
                 yrextend = yrextend, yraxes = yraxes,  $
                 yrminor = yrminor, yrtime = yrtime, yrorigin = $
                 yrorigin, $
                 yrgrid = yrgrid, yrauto = yrauto, yrannotate = $
                 yrannotate, display = display, $ $
                 graffer = graffer, ascii = ascii, h_orient = h_orient, $ $
                 h_colour = h_colour, h_eps = h_eps, h_xsize = h_xsize, $
                 h_ysize = h_ysize, h_xmargin = h_xmargin, $ $
                 h_ymargin = h_ymargin, isotropic = isotropic, h_cmyk = $
                 h_cmyk, ctable = ctable, h_print = h_print, h_viewer $
                 = h_viewer, h_file = h_file, h_psdev = h_psdev, $
                 h_epsdev = h_epsdev, h_pdfdev = h_pdfdev, $
                 h_pdfviewer = h_pdfviewer

;+
; GRAFF_PROPS
;	User-callable interface to set global properties of a graffer
;	file.
;
; Usage:
; pro Graff_props, file[, pdefs], title = title, subtitle = subtitle, $
;                 charsize = charsize, thick = thick, corners = $
;                 corners, $
;                 aspect = aspect, comment = comment, xtitle = xtitle, $
;                 xrange = xrange, xlog = xlog, xl_bands = xl_bands, $
;                 xexact = xexact, $
;                 xextend = xextend, xaxes = xaxes, xbox = xbox, $
;                 xminor = xminor, xtime = xtime, xorigin = xorigin, $
;                 xgrid = xgrid, xauto = xauto, xannotate = xannotate, $
;                 xmajor = xmajor, xtickv = xtickv, $
;                 ytitle = ytitle, $
;                 yrange = yrange, ylog = ylog, xl_bands = xl_bands, $
;                 yexact = yexact, $
;                 yextend = yextend, yaxes = yaxes, ybox = ybox, $
;                 yminor = yminor, ytime = ytime, yorigin = yorigin, $
;                 ygrid = ygrid, yauto = yauto, yannotate = yannotate, $
;                 ymajor = ymajor, ytickv = ytickv, $
;                 yr_enable = yr_enable, $
;                 yrtitle = yrtitle, $
;                 yrmajor = xmajor, yrtickv = xtickv, $
;                 yrrange = yrrange, yrlog = yrlog, xl_bands = $
;                 xl_bands, yrexact = yrexact, $
;                 yrextend = yrextend, yraxes = yraxes, $ $
;                 yrminor = yrminor, yrtime = yrtime, yrorigin = yrorigin, $
;                 yrgrid = yrgrid, yrauto = yrauto, yrannotate = $
;                 yrannotate, display = display, $ $
;                 graffer = graffer, ascii = ascii, h_orient = h_orient, $ $
;                 h_colour = h_colour, h_xsize = h_xsize, $
;                 h_ysize = h_ysize, h_xmargin = h_xmargin, $ $
;                 h_ymargin = h_ymargin, isotropic = isotropic, h_cmyk = $
;                 h_cmyk, ctable = ctable, h_print = h_print, h_viewer $
;                 = h_viewer, h_file = h_file, h_psdev = h_psdev, $
;                 h_epsdev = h_epsdev, h_pdfdev = h_pdfdev, h_pdfdev = $
;                 h_pdfdev
;
; Arguments:
;	file	string	input	The graffer file to modify.
;	pdefs	struct	output	Argument to return the graffer
;				structure. If this is present and
;				mutable, then the structure is
;				returned instead of being saved back
;				to the file.
;
; Keywords:
; 	title		input	Set the plot title.
; 	subtitle	input	Set the subtitle for the plot.
; 	charsize	input	Set the character size to be used for
;			 	axis labelling and plot annotations.
; 	thick		input	Set the line thickness to be used for
;			 	drawing the axes.
; 	corners		input	Set the location of the plot in
;			 	normalized coordinates by specifying
;			 	the locations of the corners
;			 	(4-elemant array [x0,y0, x1,y1])
; 	aspect		input	Set the location of the plot within
;			 	the normalized coordinate system by
;			 	aspect ratio and margin (2-element
;			 	array [aspect, margin]
;			 		N.B. Specifying both ASPECT &
;			 		CORNERS is an error and the
;			 		plot location is unchanged.
;	isotropic	input	Set the plot to use isotropic
;				coordinates.
; 	comment		input	Set a descriptive comment for the
;			 	whole file. (String array)
; 	[xyyr]title	input	Set the title for the specified axis.
;	[xyyr]range	input	Set the range of the specified axis
;				(2-element array).
; 	[xyyr]log		input	Set or unset the use of logarithmic
; 				axes.
; 	[xyyr]l_bands	input	A 3-element array controlling the
; 				transitions of log axis labelling. [6,
; 				15, 30]
; 	[xyyr]exact	input	Set or unset the exact range bit of
; 				the IDL axis style setting
; 	[xyyr]extend	input	Set or unset the extended range bit of
;			 	the IDL axis style setting
;	[xyyr]axes	input	Set or unset the axis plotting bit of
;				the IDL axis style setting.
;	[xy]box		input	Set or unset the "box-axis" bit in the
;				IDL axis style setting
;	[xyyr]minor	input	If set, then display minor ticks on
;				the plot; if explicitly zero, then
;				turn off the minor ticks. If a
;				non-unit value then set the number of
;				minor intervals to that.
;	[xyyr]major	input	Set the number of major intervals.
;	[xyyr]tickv	input	Set explicit tick locations. Set to a
;				scalar value, or set [xyyr]major without
;				setting this key to revert to automatic.
;	[xyyr]time	input	If set to zero, then turn off time
;				labelling, otherwise this must be a
;				structure with the following members:
;				unit: - 0 == seconds
;				     - 1 == minutes
;				     - 2 == hours
;				     - 3 == days
;				       Gives the unit in which the
;				       time is expressed in the axis data.
;				max_unit: gives the largest unit to
;				     display on the plot (same code as
;				     for unit)
;				zero: gives the value to be used for
;				    the zero of the axis (expressed in
;				    units of  max_unit
;	[xyyr]origin	input	If set, then plot an axis at the origin.
;	[xyyr]grid	input   Make a grid from the major ticks,
;				using linestyle n-1 (0 == no grid).
;	[xyyr]auto	input	If set, then perform an autoscale on
;				the specified axis, the corresponding
;				range setting takes precedence over
;				this setting.
;	[xyyr]annotate	input	Set this explicity to zero to suppress
;				annotations on the axis
;	yr_enable	input	If set, then enable the secondary Y-axis.
;	display		input	If set, then display the plot on the
;				current device.
;	graffer		input	If set, then invoke GRAFFER after
;				adding the dataset.
;	ascii		input	If set, then save as an ASCII GRAFFER
;				file (by default a binary graffer file
;				is generated).
;	h_orient	input	Set landscape(0) or portrait (1)
;				orientation of the page.
;	h_colour	input	Set or unset the generation of a
;				colour (E)PS file.
;	h_cmyk		input	Set or unset the use of the CMYK model
;				for (E)PS files. Specifying this
;				keyword will force colour (E)PS.
;	h_[xy]size	input	Set the X(Y) dimension of the page in cm
;	h_[xy]margin	input	Set the X(Y) offset of the page from
;				the lower-left corner of the page.
;	ctable		input	Set the default colour table for image display.
;	h_print		input	Specify the command to print PS output
;				files (can be a scalar or 2-element aray).
;	h_viewer	input	Specify the command to view EPS output
;				files (can be a scalar or 2-element aray).
;	h_file		input	Specify the output file for hardcopies.
;	h_psdev		input	Specify the PS device driver (plplot).
;	h_epsdev	input	Specify the EPS device driver (plplot).
;	h_pdfdev	input	Specify the PDF device driver (plplot).
;	h_svgdev	input	Specify the SVG device driver (plplot).
;				
; Restrictions:
;	The ASPECT and CORNERS keys are exclusive (if both are given,
;	both are ignored).
;	[XY]RANGE overrides [XY]AUTO.
;	The GRAFFER key overrides the DISPLAY key and the ASCII key.
;	As yet the addition/modification of a key (legend) is not
;	supported.
;	Not all hardcopy options can be set.
;
; Side Effects:
;	A graffer file is updated or created.
;	The DISPLAY option will cause your device colour table to be
;	changed.
;
; History:
;	Original (uses some code from GRAFF_ADD): 12/3/98; SJT
;	Added hardcopy settings: 13/3/98; SJT
;	Convert handles to pointers: 28/6/05; SJT
;	Add default colour table: 28/11/11; SJT
;	Add options for secondary Y-axis: 23/12/11; SJT
;	Add some more hardcopy options: 16/2/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Add options for plplot drivers: 29/11/13; SJT
;-

;	Check that the necessary inputs are present

  on_error, 2                   ; Return to caller on error

  if n_params() eq 0 || size(file, /type) ne 7 then $
     message, "Must specify a GRAFFER file"

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

;	Titles & other global options

  if n_elements(title) ne 0 then pdefs.title = title
  if n_elements(subtitle) ne 0 then pdefs.subtitle = subtitle

  if n_elements(charsize) ne 0 then pdefs.charsize = charsize
  if n_elements(thick) ne 0 then pdefs.axthick = thick

  case (keyword_set(aspect)+keyword_set(corners)+n_elements(isotropic)) of
     
     0:
     
     1: begin
        if (n_elements(aspect) eq 2) then begin
           pdefs.aspect = aspect
           pdefs.position = 0
        endif else if (n_elements(corners) eq 4) then begin
           pdefs.aspect = 0
           pdefs.position = corners
        endif else if n_elements(isotropic) eq 1 then begin
           pdefs.isotropic = keyword_set(isotropic)
        endif else begin
           graff_msg, 0l, "Invalid position setting -- ignored"
        endelse
     end
     else: graff_msg, 0l, $
                      "Attempt to define position multiple ways -- ignoring all"
  endcase


  if (n_elements(comment) ne 0) then begin
     ptr_free, pdefs.remarks
     pdefs.remarks = ptr_new(comment)
  endif

  if n_elements(ctable) ne 0 then pdefs.ctable = ctable

;	X axis settings

  if (n_elements(xtitle) ne 0) then pdefs.xtitle = xtitle
  if (n_elements(xlog) ne 0) then pdefs.xtype = keyword_set(xlog)

  if (n_elements(xrange) eq 2) then pdefs.xrange = xrange $
  else if keyword_set(xauto) then  gr_autoscale, pdefs, /xaxis, /ignore

;	Standard IDL style settings

  if (n_elements(xexact) ne 0) then begin
     if keyword_set(xexact) then pdefs.xsty.idl = pdefs.xsty.idl or 1 $
     else  pdefs.xsty.idl = pdefs.xsty.idl and (not 1)
  endif
  if (n_elements(xextend) ne 0) then begin
     if keyword_set(xextend) then pdefs.xsty.idl = pdefs.xsty.idl or 2 $
     else  pdefs.xsty.idl = pdefs.xsty.idl and (not 2)
  endif
  if (n_elements(xaxes) ne 0) then begin
     if ~keyword_set(xaxes) then pdefs.xsty.idl = pdefs.xsty.idl or 4 $
     else  pdefs.xsty.idl = pdefs.xsty.idl and (not 4)
  endif
  if (n_elements(xbox) ne 0) then begin
     if ~keyword_set(xbox) then pdefs.xsty.idl = pdefs.xsty.idl or 8 $
     else  pdefs.xsty.idl = pdefs.xsty.idl and (not 8)
  endif

;	Extra settings

  if (n_elements(xminor) ne 0) then begin
     case xminor of
        1: pdefs.xsty.minor = 0
        0: pdefs.xsty.minor = 1
        else: pdefs.xsty.minor = xminor
     endcase
  endif
  if n_elements(xmajor) ne 0 then begin
     pdefs.xsty.major = xmajor
     if  ptr_valid(pdefs.xsty.values) and n_elements(xtickv) eq 0 then $
        ptr_free, pdefs.xsty.values
  endif
  if n_elements(xtickv) ne 0 then begin
     if ptr_valid(pdefs.xsty.values) then ptr_free, pdefs.xsty.values
     if n_elements(xtickv) gt 1 then pdefs.xsty.values = $
        ptr_new(xtickv)
  endif

  if (n_elements(xorigin) ne 0) then begin
     if (keyword_set(xorigin)) then pdefs.xsty.extra = pdefs.xsty.extra $
        or 2 $
     else  pdefs.xsty.extra = pdefs.xsty.extra and (not 2)
  endif
  if n_elements(xannotate) ne 0 then begin
     if keyword_set(xannotate) then pdefs.xsty.extra = pdefs.xsty.extra $
        and (not 4) $
     else pdefs.xsty.extra = pdefs.xsty.extra or 4
  endif

  if n_elements(xl_bands) ne 0 then $
     pdefs.xsty.log_bands = xl_bands $
  else if max(pdefs.xsty.log_bands) eq 0 then $
     pdefs.xsty.log_bands = [6, 15, 30]
  
;	time labelling

  if (n_elements(xtime) ne 0) then begin
     if (not keyword_set(xtime)) then pdefs.xsty.time = pdefs.xsty.time $
        and (not 1) $
     else begin
        pdefs.xsty.time = 1 + 2*xtime.unit + 8*xtime.max_unit
        pdefs.xsty.tzero = xtime.zero
     endelse
  endif

;	Grid

  if (n_elements(xgrid) ne 0) then pdefs.xsty.grid = xgrid

;	Y axis settings

  if (n_elements(ytitle) ne 0) then pdefs.ytitle = ytitle
  if (n_elements(ylog) ne 0) then pdefs.ytype = keyword_set(ylog)

  if (n_elements(yrange) eq 2) then pdefs.yrange = yrange $
  else if keyword_set(yauto) then  gr_autoscale, pdefs, /yaxis, /ignore

;	Standard IDL style settings

  if (n_elements(yexact) ne 0) then begin
     if keyword_set(yexact) then pdefs.ysty.idl = pdefs.ysty.idl or 1 $
     else  pdefs.ysty.idl = pdefs.ysty.idl and (not 1)
  endif
  if (n_elements(yextend) ne 0) then begin
     if keyword_set(yextend) then pdefs.ysty.idl = pdefs.ysty.idl or 2 $
     else  pdefs.ysty.idl = pdefs.ysty.idl and (not 2)
  endif
  if (n_elements(yaxes) ne 0) then begin
     if ~keyword_set(yaxes) then pdefs.ysty.idl = pdefs.ysty.idl or 4 $
     else  pdefs.ysty.idl = pdefs.ysty.idl and (not 4)
  endif
  if (n_elements(ybox) ne 0) then begin
     if ~keyword_set(ybox) then pdefs.ysty.idl = pdefs.ysty.idl or 8 $
     else  pdefs.ysty.idl = pdefs.ysty.idl and (not 8)
  endif

;	Extra settings

  if (n_elements(yminor) ne 0) then begin
     case yminor of
        1: pdefs.ysty.minor = 0
        0: pdefs.ysty.minor = 1
        else: pdefs.ysty.minor = yminor
     endcase
  endif
  if n_elements(ymajor) ne 0 then begin
     pdefs.ysty.major = ymajor
     if  ptr_valid(pdefs.ysty.values) and n_elements(ytickv) eq 0 then $
        ptr_free, pdefs.ysty.values
  endif
  if n_elements(ytickv) ne 0 then begin
     if ptr_valid(pdefs.ysty.values) then ptr_free, pdefs.ysty.values
     if n_elements(ytickv) gt 1 then pdefs.ysty.values = $
        ptr_new(ytickv)
  endif

  if (n_elements(yorigin) ne 0) then begin
     if (keyword_set(yorigin)) then pdefs.ysty.extra = pdefs.ysty.extra $
        or 2 $
     else  pdefs.ysty.extra = pdefs.ysty.extra and (not 2)
  endif
  if n_elements(yannotate) ne 0 then begin
     if keyword_set(yannotate) then pdefs.ysty.extra = pdefs.ysty.extra $
        and (not 4) $
     else pdefs.ysty.extra = pdefs.ysty.extra or 4
  endif

  if n_elements(yl_bands) ne 0 then $
     pdefs.ysty.log_bands = yl_bands $
  else if max(pdefs.ysty.log_bands) eq 0 then $
     pdefs.ysty.log_bands = [6, 15, 30]
  
;	time labelling

  if (n_elements(ytime) ne 0) then begin
     if (not keyword_set(ytime)) then pdefs.ysty.time = pdefs.ysty.time $
        and (not 1) $
     else begin
        pdefs.ysty.time = 1 + 2*ytime.unit + 8*ytime.max_unit
        pdefs.ysty.tzero = ytime.zero
     endelse
  endif

;	Grid

  if (n_elements(ygrid) ne 0) then pdefs.ysty.grid = ygrid

; Secondary (Yr) Y axis settings.

  if (n_elements(yr_enable) ne 0) then pdefs.y_right = $
     keyword_set(yr_enable)

  if (n_elements(yrtitle) ne 0) then pdefs.ytitle_r = yrtitle
  if (n_elements(yrlog) ne 0) then pdefs.ytype_r = keyword_set(yrlog)

  if (n_elements(yrrange) eq 2) then pdefs.yrange_r = yrrange $
  else if keyword_set(yrauto) then  gr_autoscale, pdefs, yaxis = 2, /ignore

;	Standard IDL style settings

  if (n_elements(yrexact) ne 0) then begin
     if keyword_set(yrexact) then pdefs.ysty_r.idl = pdefs.ysty_r.idl or 1 $
     else  pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 1)
  endif
  if (n_elements(yrextend) ne 0) then begin
     if keyword_set(yrextend) then pdefs.ysty_r.idl = pdefs.ysty_r.idl or 2 $
     else  pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 2)
  endif
  if (n_elements(yraxes) ne 0) then begin
     if ~keyword_set(yraxes) then pdefs.ysty_r.idl = pdefs.ysty_r.idl or 4 $
     else  pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 4)
  endif

;	Extra settings

  if (n_elements(yrminor) ne 0) then begin
     case yrminor of
        1: pdefs.ysty_r.minor = 0
        0: pdefs.ysty_r.minor = 1
        else: pdefs.ysty_r.minor = yrminor
     endcase
  endif
  if n_elements(yrmajor) ne 0 then begin
     pdefs.ysty_r.major = yrmajor
     if  ptr_valid(pdefs.ysty_r.values) and n_elements(yrtickv) eq 0 then $
        ptr_free, pdefs.ysty_r.values
  endif
  if n_elements(yrtickv) ne 0 then begin
     if ptr_valid(pdefs.ysty_r.values) then ptr_free, pdefs.ysty_r.values
     if n_elements(yrtickv) gt 1 then pdefs.ysty_r.values = $
        ptr_new(yrtickv)
  endif

  if (n_elements(yrorigin) ne 0) then begin
     if (keyword_set(yrorigin)) then pdefs.ysty_r.extra = $
        pdefs.ysty_r.extra or 2 $
     else  pdefs.ysty_r.extra = pdefs.ysty_r.extra and (not 2)
  endif
  if n_elements(yrannotate) ne 0 then begin
     if keyword_set(yrannotate) then pdefs.ysty_r.extra = pdefs.ysty_r.extra $
        and (not 4) $
     else pdefs.ysty_r.extra = pdefs.ysty_r.extra or 4
  endif

  if n_elements(yrl_bands) ne 0 then $
     pdefs.ysty_r.log_bands = yrl_bands $
  else if max(pdefs.ysty_r.log_bands) eq 0 then $
     pdefs.ysty_r.log_bands = [6, 15, 30]
  
;	time labelling

  if (n_elements(yrtime) ne 0) then begin
     if (not keyword_set(yrtime)) then pdefs.ysty_r.time = $
        pdefs.ysty_r.time and (not 1) $
     else begin
        pdefs.ysty_r.time = 1 + 2*yrtime.unit + 8*yrtime.max_unit
        pdefs.ysty_r.tzero = yrtime.zero
     endelse
  endif

;	Grid

  if (n_elements(yrgrid) ne 0) then pdefs.ysty_r.grid = yrgrid

;	Hardcopy options.

  if (n_elements(h_orient) ne 0) then pdefs.hardset.orient = $
     keyword_set(h_orient)
  if (n_elements(h_colour) ne 0) then pdefs.hardset.colour = $
     keyword_set(h_colour)
  if (n_elements(h_eps) ne 0) then begin
     print, "Output type is no longer stored."
  endif

  if (n_elements(h_xsize) ne 0) then pdefs.hardset.size(0) = h_xsize
  if (n_elements(h_ysize) ne 0) then pdefs.hardset.size(1) = h_ysize

  if (n_elements(h_xmargin) ne 0) then pdefs.hardset.off(0) = h_xmargin
  if (n_elements(h_ymargin) ne 0) then pdefs.hardset.off(1) = h_ymargin
  if (n_elements(h_cmyk) ne 0) then begin
     pdefs.hardset.cmyk = h_cmyk
     pdefs.hardset.colour = 1b
  endif

  case n_elements(h_print) of
     0:                         ; Not given do nothing
     1: pdefs.hardset.action = [h_print, '']
     else: pdefs.hardset.action = h_print[0:1]
  endcase
  case  n_elements(h_viewer) of
     0:                         ; Not given do nothing
     1: pdefs.hardset.viewer = [h_viewer, ' &']
     2: pdefs.hardset.viewer = h_viewer[0:1]
  endcase
  case  n_elements(h_pdfviewer) of
     0:                         ; Not given do nothing
     1: pdefs.hardset.pdfviewer = [h_pdfviewer, ' &']
     2: pdefs.hardset.pdfviewer = h_pdfviewer[0:1]
  endcase
  if n_elements(h_file) ne 0 then pdefs.hardset.name = h_file
  if n_elements(h_psdev) ne 0 then pdefs.hardset.psdev = h_psdev
  if n_elements(h_epsdev) ne 0 then pdefs.hardset.epsdev = h_epsdev
  if n_elements(h_pdfdev) ne 0 then pdefs.hardset.pdfdev = h_pdfdev
  if n_elements(h_svgdev) ne 0 then pdefs.hardset.svgdev = h_svgdev

;	If the PDEFS arguement is given then do not save.

  if arg_present(pdefs) then return

;	Display or enter Graffer?

  if keyword_set(graffer) && graff_have_gui() then begin
     gr_state, /save
     gr_bin_save, pdefs
     graffer, file
     return
  endif else if (keyword_set(display)) then begin
     gr_state, /save
     gr_plot_object, pdefs
     gr_state
  endif

  if (keyword_set(ascii)) then gr_asc_save, pdefs $
  else gr_bin_save, pdefs

  graff_clear, pdefs

end

