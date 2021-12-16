; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Graff_info, file, nsets = nsets,  title = title, $
                subtitle = subtitle, charsize = charsize, $
                thick = thick, corners = corners, $
                aspect = aspect, comment = comment, xtitle = xtitle, $
                xrange = xrange, xlog = xlog, xexact = xexact, $
                xextend = xextend, xaxes = xaxes, xbox = xbox, $
                xminor = xminor, xtime = xtime, xorigin = xorigin, $
                xgrid = xgrid, xannotate = xannotate, $
                xmajor = xmajor, xtickv = xtickv, xstyle = xstyle, $
                ytitle = ytitle, yrange = yrange, ylog = ylog, $
                yexact = yexact, yextend = yextend, yaxes = yaxes, $
                ybox = ybox, yminor = yminor, ytime = ytime, $
                yorigin = yorigin, ygrid = ygrid, $
                yannotate = yannotate, ymajor = ymajor, $
                ytickv = ytickv, ystyle = ystyle, $
                yr_enable = yr_enable, yrtitle = yrtitle, $
                yrmajor = yrmajor, yrtickv = yrtickv, $
                yrstyle = yrstyle, yrrange = yrrange, yrlog = yrlog, $
                yrexact = yrexact, yrextend = yrextend, $
                yraxes = yraxes, yrminor = yrminor, yrtime = yrtime, $
                yrorigin = yrorigin, yrgrid = yrgrid, $
                yrannotate = yrannotate, $
                h_orient = h_orient, h_colour = h_colour, $
                h_eps = h_eps, h_xsize = h_xsize, $
                h_ysize = h_ysize, h_xmargin = h_xmargin, $
                h_ymargin = h_ymargin, isotropic = isotropic, $
                h_cmyk =  h_cmyk, ctable = ctable, $
                h_print = h_print, h_viewer = h_viewer, $
                h_pdfviewer = h_pdfviewer, $
                h_file = h_file, $
                ds_descriptions = ds_descriptions, $
                ds_types = ds_types, ds_modes = ds_modes

;+
; GRAFF_INFO
;	User-callable interface to retrieve global properties of a graffer
;	file.
;
; Usage:
;	Graff_info, file, nsets = nsets,  title = title, $
;                subtitle = subtitle, charsize = charsize, $
;                thick = thick, corners = corners, $
;                aspect = aspect, comment = comment, xtitle = xtitle, $
;                xrange = xrange, xlog = xlog, xexact = xexact, $
;                xextend = xextend, xaxes = xaxes, xbox = xbox, $
;                xminor = xminor, xtime = xtime, xorigin = xorigin, $
;                xgrid = xgrid, xannotate = xannotate, $
;                xmajor = xmajor, xtickv = xtickv, xstyle = xstyle, $
;                ytitle = ytitle, yrange = yrange, ylog = ylog, $
;                yexact = yexact, yextend = yextend, yaxes = yaxes, $
;                ybox = ybox, yminor = yminor, ytime = ytime, $
;                yorigin = yorigin, ygrid = ygrid, $
;                yannotate = yannotate, ymajor = ymajor, $
;                ytickv = ytickv, ystyle = ystyle, $
;                yr_enable = yr_enable, yrtitle = yrtitle, $
;                yrmajor = yrmajor, yrtickv = yrtickv, $
;                yrstyle = yrstyle, yrrange = yrrange, yrlog = yrlog, $
;                yrexact = yrexact, yrextend = yrextend, $
;                yraxes = yraxes, yrminor = yrminor, yrtime = yrtime, $
;                yrorigin = yrorigin, yrgrid = yrgrid, $
;                yrannotate = yrannotate, $
;                h_orient = h_orient, h_colour = h_colour, $
;                h_xsize = h_xsize, $
;                h_ysize = h_ysize, h_xmargin = h_xmargin, $
;                h_ymargin = h_ymargin, isotropic = isotropic, $
;                h_cmyk =  h_cmyk, ctable = ctable, $
;                h_print = h_print, h_viewer = h_viewer, $
;                h_pdfviewer = h_pdfviewer, $
;                h_file = h_file, $
;                ds_descriptions = ds_descriptions, $
;                ds_types = ds_types, ds_modes = ds_modes
;
; Argument:
;	file	string	input	The graffer file to query.
;
; Keywords:
;	nsets		output	Get the number of datasets in the file
; 	title		output	Get the plot title.
; 	subtitle	output	Get the subtitle for the plot.
; 	charsize	output	Get the character size to be used for
;			 	axis labelling and plot annotations.
; 	thick		output	Get the line thickness to be used for
;			 	drawing the axes.
; 	corners		output	Get the location of the plot in
;			 	normalized coordinates by specifying
;			 	the locations of the corners
;			 	(4-elemant array [x0,y0, x1,y1])
; 	aspect		output	Get the location of the plot within
;			 	the normalized coordinate system by
;			 	aspect ratio and margin (2-element
;			 	array [aspect, margin]
;			 		N.B. Specifying both ASPECT &
;			 		CORNERS is an error and the
;			 		plot location is unchanged.
;	isotropic	output	Get the plot to use isotropic
;				coordinates.
; 	comment		output	Get a descriptive comment for the
;			 	whole file. (String array)
; 	[xyyr]title	output	Get the title for the specified axis.
;	[xyyr]range	output	Get the range of the specified axis
;				(2-element array).
; 	[xyyr]log	output	Get  the use of logarithmic
; 				axes.
; 	[xyyr]exact	output	Get  the exact range bit of
; 				the IDL axis style setting
; 	[xyyr]extend	output	Get  the extended range bit of
;			 	the IDL axis style setting
;	[xyyr]axes	output	Get  the axis plotting bit of
;				the IDL axis style setting.
;	[xy]box		output	Get  the "box-axis" bit in the
;				IDL axis style setting
;	[xyyr]style	Output	Get the IDL axis style parameter.
;	[xyyr]minor	output	If set, then display minor ticks on
;				the plot; if explicitly zero, then
;				turn off the minor ticks. If a
;				non-unit value then set the number of
;				minor intervals to that.
;	[xyyr]major	output	Get the number of major intervals.
;	[xyyr]tickv	output	Get explicit tick locations. Set to a
;				scalar value, or set [xyyr]major without
;				setting this key to revert to automatic.
;	[xyyr]time	output	Will return a 
;				structure with the following members:
;				set: Whether time labeling is set.
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
;	[xyyr]origin	output	If set, then plot an axis at the origin.
;	[xyyr]grid	output   Make a grid from the major ticks,
;				using linestyle n-1 (0 == no grid).
;	[xyyr]annotate	output	Get this explicity to zero to suppress
;				annotations on the axis
;	yr_enable	output	If set, then enable the secondary Y-axis.
;	h_orient	output	Get landscape(0) or portrait (1)
;				orientation of the page.
;	h_colour	output	Get  the generation of a
;				colour (E)PS file.
;	h_cmyk		output	Get  the use of the CMYK model
;				for (E)PS files. Specifying this
;				keyword will force colour (E)PS.
;	h_[xy]size	output	Get the X(Y) dimension of the page in cm
;	h_[xy]margin	output	Get the X(Y) offset of the page from
;				the lower-left corner of the page.
;	ctable		output	Get the default colour table for image display.
;	h_print		output	Specify the command to print PS output
;				files (can be a scalar or 2-element aray).
;	h_viewer	output	Specify the command to view EPS output
;				files (can be a scalar or 2-element aray).
;	h_pdfviewer	output	Specify the command to view PDF output
;				files (can be a scalar or 2-element aray).
;	h_file		output	Specify the output file for hardcopies.
;	ds_descripions	output	A variable for the dataset descriptions.
;	ds_types	output	A variable for the dataset types.
;	ds_modes	output	A variable for the dataset modes
;				(rect/polar coords)
;				
; Restrictions:
; 	Some settings may not return meaningful values for all files
;
;
; History:
;	Original, using graff_props as a template.: Sep 2016; SJT
;	Added ds_* keys: 17/11/21; SJT
;-

;	Check that the necessary inputs are present

;  on_error, 2                   ; Return to caller on error

  if (n_params() ne 1) then message, "Must specify a GRAFFER file"

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

; Number of datasets, and basic DS properties.

  if arg_present(nsets) then nsets = pdefs.nsets

  if pdefs.nsets gt 0 then begin
     if arg_present(ds_descriptions) then $
        ds_descriptions = (*pdefs.data).descript
     if arg_present(ds_types) then $
        ds_types = (*pdefs.data).type
     if arg_present(ds_modes) then $
        ds_modes = (*pdefs.data).mode
  endif
  
;	Titles & other global options

  if arg_present(title) then title = pdefs.title
  if arg_present(subtitle) then subtitle = pdefs.subtitle

  if arg_present(charsize) then charsize = pdefs.charsize
  if arg_present(thick) then axthick = pdefs.thick

; Corners etc (note may not always give meaningful results)
  if arg_present(aspect) then aspect =  pdefs.aspect
  if arg_present(corners) then corners = pdefs.position
  if arg_present(isotropic) then isotropic = pdefs.isotropic

  if arg_present(comment) then comment = *(pdefs.remarks)
  
  if arg_present(ctable) then ctable = pdefs.ctable

;	X axis settings

  if arg_present(xrange) then xrange = pdefs.xrange
  if arg_present(xtitle) then xtitle = pdefs.xtitle
  if arg_present(xlog) then xlog = pdefs.xtype

;	Standard IDL style settings

  if arg_present(xstyle) then xstyle = pdefs.xsty.idl
  if arg_present(xexact) then xexact = (pdefs.xsty.idl and 1) ne 0
  if arg_present(xextend) then xextend =  (pdefs.xsty.idl and 2) ne 0
  if arg_present(xaxes) then xaxes = (pdefs.xsty.idl and 4) ne 0
  if arg_present(xbox) then xbox = (pdefs.xsty.idl and 8) ne 0

;	Extra settings

  if arg_present(xminor) then begin
     case pdefs.xsty.minor of
        1: xminor = 0
        0: xminor = 1
        else: xminor = pdefs.xsty.minor
     endcase
  endif
  if arg_present(xmajor) then xmajor = pdefs.xsty.major

  if arg_present(xtickv)  then begin
     if ptr_valid(pdefs.xsty.values) then xtickv = $
        *(pdefs.xsty.values) $
     else xtickv = !values.d_nan
  endif

  if arg_present(xorigin) then xorigin = (pdefs.xsty.extra and 2) ne 0
  if arg_present(xannotate) then $
     xannotate = (pdefs.xsty.extra and 4) ne 0

;	time labelling

  if arg_present(xtime) then $
     xtime = {set: pdefs.xsty.time and 1, $
              unit: (pdefs.xsty.time and 6)/2, $
              max_unit: (pdefs.xsty.time and 24)/8, $
              zero: pdefs.xsty.tzero}
  
;	Grid

  if arg_present(xgrid) then xgrid = pdefs.xsty.grid 

;	Y axis settings

  if arg_present(yrange) then yrange = pdefs.yrange

  if arg_present(ytitle) then ytitle = pdefs.ytitle
  if arg_present(ylog) then ylog = pdefs.ytype

;	Standard IDL style settings

  if arg_present(ystyle) then ystyle = pdefs.ysty.idl
  if arg_present(yexact) then yexact = (pdefs.ysty.idl and 1) ne 0
  if arg_present(yextend) then yextend =  (pdefs.ysty.idl and 2) ne 0
  if arg_present(yaxes) then yaxes = (pdefs.ysty.idl and 4) ne 0
  if arg_present(ybox) then ybox = (pdefs.ysty.idl and 8) ne 0

  
;	Extra settings

  if arg_present(yminor) then begin
     case pdefs.ysty.minor of
        1: yminor = 0
        0: yminor = 1
        else: yminor = pdefs.ysty.minor
     endcase
  endif
  if arg_present(ymajor) then ymajor = pdefs.ysty.major
  
  if arg_present(ytickv)  then begin
     if ptr_valid(pdefs.ysty.values) then ytickv = $
        *(pdefs.ysty.values) $
     else ytickv = !values.d_nan
  endif

  if arg_present(yorigin) then yorigin = (pdefs.ysty.extra and 2) ne 0
  if arg_present(yannotate) then $
     yannotate = (pdefs.ysty.extra and 4) ne 0

;	time labelling

  if arg_present(ytime) then $
     ytime = {set: pdefs.ysty.time and 1, $
              unit: (pdefs.ysty.time and 6)/2, $
              max_unit: (pdefs.ysty.time and 24)/8, $
              zero: pdefs.ysty.tzero}
 
;	Grid

  if arg_present(ygrid) then ygrid = pdefs.ysty.grid 

; Secondary (Yr) Y axis settings.

  if arg_present(yr_enable) then yr_enable = pdefs.y_right

  if arg_present(yrrange) then yrrange = pdefs.yrange_r
  if arg_present(yrtitle) then yrtitle = pdefs.ytitle_r
  if arg_present(yrlog) then yrlog = pdefs.ytype_r

;	Standard IDL style settings

  if arg_present(yrstyle) then xstyle = pdefs.ysty_r.idl
  if arg_present(yrexact) then xexact = (pdefs.ysty_r.idl and 1) ne 0
  if arg_present(yrextend) then xextend =  (pdefs.ysty_r.idl and 2) ne 0
  if arg_present(yraxes) then xaxes = (pdefs.ysty_r.idl and 4) ne 0


;	Extra settings

  if arg_present(yrminor) then begin
     case pdefs.ysty_r.minor of
        1: yrminor = 0
        0: yrminor = 1
        else: yrminor = pdefs.ysty_r.minor
     endcase
  endif
  if arg_present(yrmajor) then yrmajor = pdefs.ysty_r.major
 
  if arg_present(yrtickv)  then begin
     if ptr_valid(pdefs.ysty_r.values) then yrtickv = $
        *(pdefs.ysty_r.values) $
     else yrtickv = !values.d_nan
  endif

  if arg_present(yrorigin) then yrorigin = (pdefs.ysty_r.extra and 2) ne 0
  if arg_present(yrannotate) then $
     yrannotate = (pdefs.ysty_r.extra and 4) ne 0

;	time labelling

  if arg_present(yrtime) then $
     yrtime = {set: pdefs.ysty_r.time and 1, $
               unit: (pdefs.ysty_r.time and 6)/2, $
               max_unit: (pdefs.ysty_r.time and 24)/8, $
               zero: pdefs.ysty_r.tzero}

;	Grid

  if arg_present(yrgrid) then yrgrid = pdefs.ysty_r.grid 

;	Hardcopy options.

  if arg_present(h_orient) then h_orient = pdefs.hardset.orient
  if arg_present(h_colour) then h_colour = pdefs.hardset.colour
  if arg_present(h_eps) then h_eps = 0

  if arg_present(h_xsize) then h_xsize = pdefs.hardset.size[0]
  if arg_present(h_ysize) then h_ysize = pdefs.hardset.size[1]

  if arg_present(h_xmargin) then h_xmargin = pdefs.hardset.off[0]
  if arg_present(h_ymargin) then h_ymargin = pdefs.hardset.off[1]
  if arg_present(h_cmyk) then h_cmyk = pdefs.hardset.cmyk

  if arg_present(h_print) then h_print = pdefs.hardset.action
  if arg_present(h_viewer) then h_viewer = pdefs.hardset.viewer
  if arg_present(h_pdfviewer) then h_pdfviewer = pdefs.hardset.pdfviewer
  if arg_present(h_file)  then h_file = pdefs.hardset.name

  graff_clear, pdefs

end

