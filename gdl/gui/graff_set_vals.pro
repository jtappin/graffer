; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Graff_set_vals, pdefs, set_only = set_only

;+
; GRAFF_SET_VALS
;	Setup the values of the graffer widgets
;
; Usage:
;	graff_set_vals, pdefs
;
; Argument:
;	pdefs	struct	input	The graffer control structure
;
; Keyword:
;	set_only	input	If set, then only do those values
;				which change with the current data set
;
; History:
;	Original: 18.8.95; SJT
;	Add Mode: 12/8/96; SJT
;	Add settings of the styles: 17/1/97; SJT
;	Modify for extended symbol definitions: 20/1/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add current-only setting: 26/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Replace cw_pdtsmenu with cw_pdmenu_plus: 28/9/16; SJT
; 	Add font setting: 12/2/20; SJT
;-

  common Gr_psym_maps, psym_bm, col_bm
  common graffer_options, optblock

  if (not keyword_set(set_only)) then begin
     widget_control, pdefs.ids.graffer, tlb_set_title =  $
                     string(pdefs.version, pdefs.name,  $
                            format = "('Graffer V',I0,'.',I2.2,': ',A)")
     
     if strpos(pdefs.dir, path_sep(), /reverse_search) ne $
        strlen(pdefs.dir)-1 then fullname = pdefs.dir+path_sep()+pdefs.name $
     else fullname = pdefs.dir+pdefs.name
     widget_control, pdefs.ids.name, set_value = fullname

     widget_control, pdefs.ids.title, set_value = pdefs.title
     widget_control, pdefs.ids.subtitle, set_value = pdefs.subtitle
     widget_control, pdefs.ids.charsize, set_value = pdefs.charsize
     widget_control, pdefs.ids.axthick, set_value = pdefs.axthick
     widget_control, pdefs.ids.fontsel, set_droplist_select = $
                     pdefs.fontopt
     
     widget_control, pdefs.ids.xtitle, set_value = pdefs.xtitle
     widget_control, pdefs.ids.xmin, set_value = pdefs.xrange(0)
     widget_control, pdefs.ids.xmax, set_value = pdefs.xrange[1]
     widget_control, pdefs.ids.xlog, set_droplist_select = pdefs.xtype
     
     cw_pdmenu_plus_set, pdefs.ids.xsty[0], pdefs.xsty.idl and 1
     cw_pdmenu_plus_set, pdefs.ids.xsty[1], (pdefs.xsty.idl and 2) ne 0
     cw_pdmenu_plus_set, pdefs.ids.xsty[2], (pdefs.xsty.idl and 4) eq 0
     cw_pdmenu_plus_set, pdefs.ids.xsty[3], (pdefs.xsty.idl and 8) eq 0
     
     cw_pdmenu_plus_set, pdefs.ids.xsty[4], pdefs.xsty.minor eq 0
     cw_pdmenu_plus_set, pdefs.ids.xsty[5], (pdefs.xsty.extra and 4) eq 0

     if (pdefs.xsty.extra and 2) eq 0 then xostat = 0 $
     else if (pdefs.xsty.extra and 8) eq 0 then xostat = 1 $
     else xostat = 2
     cw_pdmenu_plus_set, pdefs.ids.xsty[7], index = xostat
     
     cw_pdmenu_plus_set, pdefs.ids.xsty[6], (pdefs.xsty.time and 1) ne 0
     cw_pdmenu_plus_set, pdefs.ids.xsty[8], index = pdefs.xsty.grid
     
     widget_control, pdefs.ids.ytitle, set_value = pdefs.ytitle
     widget_control, pdefs.ids.ymin, set_value = pdefs.yrange[0]
     widget_control, pdefs.ids.ymax, set_value = pdefs.yrange[1]
     widget_control, pdefs.ids.ylog, set_droplist_select = pdefs.ytype

     cw_pdmenu_plus_set, pdefs.ids.ysty[0], pdefs.ysty.idl and 1
     cw_pdmenu_plus_set, pdefs.ids.ysty[1], (pdefs.ysty.idl and 2) ne 0
     cw_pdmenu_plus_set, pdefs.ids.ysty[2], (pdefs.ysty.idl and 4) eq 0
     cw_pdmenu_plus_set, pdefs.ids.ysty[3], (pdefs.ysty.idl and 8) eq 0
     
     cw_pdmenu_plus_set, pdefs.ids.ysty[4], pdefs.ysty.minor eq 0
     cw_pdmenu_plus_set, pdefs.ids.ysty[5], (pdefs.ysty.extra and 4) eq 0

     if (pdefs.ysty.extra and 2) eq 0 then yostat = 0 $
     else if (pdefs.ysty.extra and 8) eq 0 then yostat = 1 $
     else yostat = 2
     cw_pdmenu_plus_set, pdefs.ids.ysty[7], index = yostat
     
     cw_pdmenu_plus_set, pdefs.ids.ysty[6], (pdefs.ysty.time and 1) ne 0
     cw_pdmenu_plus_set, pdefs.ids.ysty[8], index = pdefs.ysty.grid

     widget_control, pdefs.ids.ytitle_r, set_value = pdefs.ytitle_r
     widget_control, pdefs.ids.ymin_r, set_value = pdefs.yrange_r[0]
     widget_control, pdefs.ids.ymax_r, set_value = pdefs.yrange_r[1]
     widget_control, pdefs.ids.ylog_r, set_droplist_select = pdefs.ytype_r

     cw_pdmenu_plus_set, pdefs.ids.ysty_r[0], pdefs.ysty_r.idl and 1
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[1], (pdefs.ysty_r.idl and 2) ne 0
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[2], (pdefs.ysty_r.idl and 4) eq 0
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[3], (pdefs.ysty_r.idl and 8) eq 0
     
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[4], pdefs.ysty_r.minor eq 0
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[5], (pdefs.ysty_r.extra and 4) eq 0

     if (pdefs.ysty_r.extra and 2) eq 0 then yrostat = 0 $
     else if (pdefs.ysty_r.extra and 8) eq 0 then yrostat = 1 $
     else yrostat = 2
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[7], index = yrostat

     cw_pdmenu_plus_set, pdefs.ids.ysty_r[6], (pdefs.ysty_r.time and 1) ne 0
     cw_pdmenu_plus_set, pdefs.ids.ysty_r[8], index = $
                         pdefs.ysty_r.grid

     widget_control, pdefs.ids.current, set_button = $
                     pdefs.transient.current_only 

     widget_control, pdefs.ids.y_axis, sensitive = pdefs.y_right
     widget_control, pdefs.ids.y_right, set_button = pdefs.y_right
     widget_control, pdefs.ids.ybase_r, sensitive = pdefs.y_right
  endif

;	The remainder depend on pdefs.cset and handles must be extracted

  data = (*pdefs.data)[pdefs.cset]
  if ~optblock.bitmaps then  widget_control, pdefs.ids.psym, $
     set_droplist_select = data.psym $
  else widget_control, pdefs.ids.psym, set_value = data.psym
;  cw_pdmenu_plus_set, pdefs.ids.psym, index = data.psym
  widget_control, pdefs.ids.pline, set_droplist_select = data.pline
  widget_control, pdefs.ids.symsize, set_value = $
                  data.symsize
  widget_control, pdefs.ids.line, set_droplist_select = data.line, $
                  sensitive = data.pline ne 0
  
  if data.colour eq -2 then begin
     ci = graff_colours(/max_index)+2
  endif else ci = data.colour+1
  widget_control, pdefs.ids.colour, set_value = ci
  gr_show_colour, pdefs

  widget_control, pdefs.ids.thick, set_value = data.thick
  sedit = data.type ge 0 && data.type ne 9 
  widget_control, pdefs.ids.dsxtra[0], sensitive = sedit
  widget_control, pdefs.ids.dsxtra[2], sensitive = sedit
  cw_pdmenu_plus_set, pdefs.ids.dsxtra[0], data.sort
  cw_pdmenu_plus_set, pdefs.ids.dsxtra[1], ~data.noclip
  cw_pdmenu_plus_set, pdefs.ids.dsxtra[2], data.medit
  widget_control, pdefs.ids.draw, get_uvalue = state
  if (state eq 'DRAW') then widget_control, pdefs.ids.draw, $
                                            draw_button_events = $
                                            data.medit, track = $
                                            data.medit
  widget_control, pdefs.ids.y_axis, set_droplist_select = data.y_axis

  widget_control, pdefs.ids.mode, set_droplist_select = data.mode
  widget_control, pdefs.ids.descr, set_value = data.descript
  widget_control, pdefs.ids.cset, set_value = pdefs.cset+1

  typedescs = ["Function z = f(x,y)", $
               "Function: x = f(t), y = g(t)", $
               "Function: x = f(y)", $
               "Function: y = f(x)", $
               "Data: X, Y", $
               "Data: X, Y, errors: ±Y", $
               "Data: X, Y, errors: -Y, +Y", $
               "Data: X, Y, errors: ±X", $
               "Data: X, Y, errors: -X, +X", $
               "Data: X, Y, errors: ±X, ±Y", $
               "Data: X, Y, errors: ±X, -Y, +Y", $
               "Data: X, Y, errors: -X, +X, ±Y", $
               "Data: X, Y, errors: -X, +X, -Y, +Y", $
               "Data: Z, X, Y"]
  if data.ndata eq 0 then widget_control, pdefs.ids.type, $
                                          set_value = 'Undefined' $
  else widget_control, pdefs.ids.type, set_value = typedescs[data.type+4]
 
  widget_control, pdefs.ids.zmode, set_droplist_select = $
                  data.zopts.format

  if data.type ge 0 then begin
     widget_control, pdefs.ids.minval, set_value = data.min_val
     widget_control, pdefs.ids.maxval, set_value = data.max_val
     widget_control, pdefs.ids.minmaxbase, sensitive = data.mode eq 0
  endif else begin
     widget_control, pdefs.ids.minval, set_value = !values.d_nan
     widget_control, pdefs.ids.maxval, set_value = !values.d_nan
     widget_control, pdefs.ids.minmaxbase, sensitive = 0
  endelse

  if (data.type eq 9 or $
      data.type eq -4) then begin
     widget_control, pdefs.ids.plopts[0], map = 0
     widget_control, pdefs.ids.plopts[1], map = 1
     if (widget_info(/valid, pdefs.ids.zopts.bases[0])) then $
        graff_set_zvals, pdefs
  endif else begin
     widget_control, pdefs.ids.plopts[1], map = 0     
     widget_control, pdefs.ids.plopts[0], map = 1     
  endelse

  widget_control, pdefs.ids.export, sensitive = data.type ge 0 and $
                  ptr_valid(data.xydata) 

;	Clear the message box

  widget_control, pdefs.ids.message, set_value = ''

  gr_plot_object, pdefs

end
