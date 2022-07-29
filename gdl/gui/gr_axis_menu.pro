; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_AXIS_MENU
;	Make up an axis options menu for GRAFFER
;
; Usage:
;	gr_axis_menu, axis, base, pdefs
;
; Arguments:
;	axis	char	input	"X" or "Y" to say which axis it is.
;	base	long	input	Widget ID of the parent base.
;	pdefs	struct	in/out	The GRAFFER control & data structure.
;
; History:
;	Extracted: 6/12/96; SJT
;	Change to "Stated" pulldown & Move event handler here (to try
;	and reduce the size of the EH in GRAFFER): 17/1/97; SJT
;	Add CAPTURE key to entry boxes: 6/2/97; SJT
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add annotation suppression: 12/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Replace cw_pdtsmenu with cw_pdmenu_plus: 28/9/16; SJT
;	Make coordinates double: 24/5/17; SJT
;-

pro Gr_axis_event, event

  widget_control, event.id, get_uvalue = object

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy

  idraw_flag = 1
  ichange = 1b
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  nch = 1

  if track_flag then begin
     idraw_flag = 0
     ichange = 0b
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case object of
                                ; X-axis properties
     
     'XMIN': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Enter minimum value on X axis (floating point)' $
     else  pdefs.xrange[0] = event.value
     
     'XMAX': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Enter maximum value on X axis (floating point)' $
     else pdefs.xrange[1] = event.value
     
     'XSTY': begin
        val = strsplit(event.value, '/', /extr)
        if (n_elements(val) eq 1) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Select X-axis style options' $
        else case val[1] of
           'Logarithmic': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                 'Toggle linear or logarithmic X axis' $
              else pdefs.xtype = event.select
           end
           'Exact Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Select exact  or rounded X axis range' $
              else if event.select  then  $
                 pdefs.xsty.idl = pdefs.xsty.idl or 1 $
              else pdefs.xsty.idl = pdefs.xsty.idl and (not 1)
           end
           'Extended Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch "Extended" X-axis range on or off' $
              else if event.select then $
                 pdefs.xsty.idl = pdefs.xsty.idl or 2 $
              else pdefs.xsty.idl = pdefs.xsty.idl and (not 2)
           end
           'Draw Axes': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch drawing of X axes on or off' $
              else if event.select then $
                 pdefs.xsty.idl = pdefs.xsty.idl and (not 4) $
              else pdefs.xsty.idl = pdefs.xsty.idl or 4
           end
           'Draw Box Axis': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Switch drawing of ' + $
                            'right-hand X axis on or off' $
              else if event.select then $
                 pdefs.xsty.idl = pdefs.xsty.idl and (not 8) $
              else pdefs.xsty.idl = pdefs.xsty.idl or 8
           end
           'Minor Ticks': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Switch drawing of ' + $
                            'X axis minor tick marks on or off' $
              else pdefs.xsty.minor = ~event.select
           end
           'Annotation': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch axis annotation on or off' $
              else if event.select then $
                 pdefs.xsty.extra = pdefs.xsty.extra and (not 4) $
              else pdefs.xsty.extra = pdefs.xsty.extra or 4
           end
           'Time Labelling': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Switch time-format ' + $
                            'labelling of X axis on or off' $
              else if event.select then begin
                 to = gr_tm_opts(pdefs.xsty.time, $ 
                                 pdefs.xsty.tzero, group = $
                                 pdefs.ids.graffer)
                 pdefs.xsty.time = to(0)
                 pdefs.xsty.tzero = to(1)
                 nch = 5
              endif else pdefs.xsty.time = pdefs.xsty.time and (not 1)
           end
           'Origin Axis': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Toggle inclusion of X axis at Y=0' $
              else if event.select then case val(2) of
                 "On": pdefs.xsty.extra = (pdefs.xsty.extra or 2) $
                                          and (not 8)
                 "Off": pdefs.xsty.extra = pdefs.xsty.extra and (not 10)
                 "Full": pdefs.xsty.extra = pdefs.xsty.extra or 10
              endcase
           end
           'Grid': begin
              if (track_flag and (n_elements(val) eq 2)) then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Select X-grid options' $
              else  case val[2] of
                 ' None ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'No grid lines' $
                    else if event.select then pdefs.xsty.grid = 0
                 end
                 
                 '______': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Solid grid lines' $
                    else if event.select then pdefs.xsty.grid = 1
                 end
                 
                 '......': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dotted grid lines' $
                    else if event.select then pdefs.xsty.grid = 2
                 end
                 
                 '_ _ _ ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dashed grid lines' $
                    else if event.select then pdefs.xsty.grid = 3
                 end
                 '_._._.': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dash-dot grid lines' $
                    else if event.select then pdefs.xsty.grid = 4
                 end
                 '_...  ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Dash dot dot dot grid lines' $
                    else if event.select then pdefs.xsty.grid = 5
                 end
                 '__  __': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Long dash grid lines' $
                    else if event.select then pdefs.xsty.grid = 6
                 end
              endcase
           end
           'Autoscale': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Adjust the X-axis ' + $
                            'scaling to accomodate current data' $
              else case val[2] of
                 'Extend': gr_autoscale, pdefs, /xaxis 
                 'Extend or Shrink': gr_autoscale, pdefs, /xaxis, $
                                                   /ignore
                 'Visible Only': gr_autoscale, pdefs, /xaxis, $
                                              /visible 
              endcase
           end
           'Advanced ...': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Advanced axis settings for the X axis' $
              else ichange = gr_axis_adv_menu( pdefs, /xaxis)
           end
        endcase
     end
     'XLAB': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Enter label for the X axis' $
     else pdefs.xtitle = event.value
     
                                ; Y Axis properties

     'YMIN': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Enter minimum value on Y axis (floating point)' $
     else pdefs.yrange[0] = event.value
     
     'YMAX': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Enter maximum value on Y axis (floating point)' $
     else pdefs.yrange[1] = event.value
     
     'YSTY': begin
        val = strsplit(event.value, '/', /extr)
        if (n_elements(val) eq 1) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Select Y-axis style options' $
        else case val[1] of
           'Logarithmic': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                 'Toggle linear or logarithmic Y axis' $
              else pdefs.ytype = event.select
           end
           'Exact Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Select exact  or rounded Y axis range' $
              else if event.select  then  $
                 pdefs.ysty.idl = pdefs.ysty.idl or 1 $
              else pdefs.ysty.idl = pdefs.ysty.idl and (not 1)
           end
           'Extended Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch "Extended" Y-axis range on or off' $
              else if event.select then $
                 pdefs.ysty.idl = pdefs.ysty.idl or 2 $
              else pdefs.ysty.idl = pdefs.ysty.idl and (not 2)
           end
           'Draw Axes': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch drawing of Y axes on or off' $
              else if event.select then $
                 pdefs.ysty.idl = pdefs.ysty.idl and (not 4) $
              else pdefs.ysty.idl = pdefs.ysty.idl or 4
           end
           'Draw Box Axis': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch drawing of right-hand Y axis on or off' $
              else if event.select then $
                 pdefs.ysty.idl = pdefs.ysty.idl and (not 8) $
              else pdefs.ysty.idl = pdefs.ysty.idl or 8
           end
           'Minor Ticks': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch drawing of Y axis minor tick ' + $
                            'marks on or off' $
              else pdefs.ysty.minor = ~event.select
           end
           'Annotation': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch axis annotation on or off' $
              else if event.select then $
                 pdefs.ysty.extra = pdefs.ysty.extra and (not 4) $
              else pdefs.ysty.extra = pdefs.ysty.extra or 4
           end
           'Time Labelling': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch time-format ' + $
                            'labelling of Y axis on or off' $
              else if event.select then begin
                 to = gr_tm_opts(pdefs.ysty.time, $
                                 pdefs.ysty.tzero, group = $
                                 pdefs.ids.graffer)
                 pdefs.ysty.time = to(0)
                 pdefs.ysty.tzero = to(1)
                 nch = 5
              endif else pdefs.ysty.time = pdefs.ysty.time and (not 1)
           end
           'Origin Axis': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Toggle inclusion of Y ' + $
                            'axis at X=0', /help $
              else if event.select then case val(2) of
                 "On": pdefs.ysty.extra = (pdefs.ysty.extra or 2) $
                                          and (not 8)
                 "Off": pdefs.ysty.extra = pdefs.ysty.extra and (not 10)
                 "Full": pdefs.ysty.extra = pdefs.ysty.extra or 10
              endcase
           end
           'Grid': begin
              if (track_flag and (n_elements(val) eq 2)) then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Select Y-grid options' $
              else case val[2] of
                 ' None ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'No grid lines' $
                    else if event.select then pdefs.ysty.grid = 0
                 end
                 
                 '______': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Solid grid lines' $
                    else if event.select then pdefs.ysty.grid = 1
                 end
                 
                 '......': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dotted grid lines' $
                    else if event.select then pdefs.ysty.grid = 2
                 end
                 
                 '_ _ _ ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dashed grid lines' $
                    else if event.select then pdefs.ysty.grid = 3
                 end
                 '_._._.': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dash-dot grid lines' $
                    else if event.select then pdefs.ysty.grid = 4
                 end
                 '_...  ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Dash dot dot dot grid lines' $
                    else if event.select then pdefs.ysty.grid = 5
                 end
                 '__  __': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Long dash grid lines' $
                    else if event.select then pdefs.ysty.grid = 6
                 end
              endcase
           end
           'Autoscale': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Adjust the Y-axis ' + $
                            'scaling to accomodate current data' $
              else case val[2] of
                 'Extend': gr_autoscale, pdefs, /yaxis 
                 'Extend or Shrink': gr_autoscale, pdefs, /yaxis, $
                                                   /ignore
                 'Visible Only': gr_autoscale, pdefs, /yaxis, $
                                              /visible 
              endcase
           end
           'Advanced ...': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Advanced axis ' + $
                            'settings for the Y axis' $
              else ichange = gr_axis_adv_menu( pdefs, /yaxis)
           end
        endcase
     end
     'YLAB': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Enter label for the Y axis' $
     else begin
        pdefs.ytitle = event.value
     end
     
                                ; Right-hand Y axis settings
     'YrMIN': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, 'Enter minimum value on Y(r) axis ' + $
                   '(floating point)', /help $
     else begin
        pdefs.yrange_r[0] = event.value
     end
     
     'YrMAX': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, 'Enter maximum value on Y(r) axis ' + $
                   '(floating point)', /help $
     else begin
        pdefs.yrange_r(1) = event.value
     end
     
     'YrSTY': begin
        val = strsplit(event.value, '/', /extr)
        if n_elements(val) eq 1 then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Select Y(r)-axis style options' $
        else case val[1] of
           'Logarithmic': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                 'Toggle linear or logarithmic Y(r) axis' $
              else pdefs.ytype_r = event.select
           end
           'Exact Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Select exact or ' + $
                            'rounded Y(r) axis range' $
              else if event.select then $
                 pdefs.ysty_r.idl = pdefs.ysty_r.idl or 1 $
              else pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 1) 
           end
           'Extended Range': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Switch "Extended" ' + $
                            'Y(r)-axis range on or off' $
              else if event.select then $
                 pdefs.ysty_r.idl = pdefs.ysty_r.idl or 2 $
              else pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 2)
           end
           'Draw Axes': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Switch drawing of Y(r) axis on or off' $
              else if event.select then $
                 pdefs.ysty_r.idl = pdefs.ysty_r.idl and (not 4) $
              else pdefs.ysty_r.idl = pdefs.ysty_r.idl or 4 
           end
           'Draw Box Axis':     ; Shouldn't happen

           'Minor Ticks': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, /help, 'Switch drawing of ' + $
                            'Y(r) axis minor tick marks on or off' $
              else pdefs.ysty_r.minor = ~event.select
           end
           'Annotation': begin
              if track_flag then $ $
                 graff_msg, pdefs.ids.hlptxt, 'Switch axis annotation ' + $
                 'on or off', /help $
              else if event.select then $
                 pdefs.ysty_r.extra = pdefs.ysty_r.extra and (not 4) $
              else pdefs.ysty_r.extra = pdefs.ysty_r.extra or 4
           end
           'Time Labelling': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Switch time-format ' + $
                            'labelling of Y(r) axis on or off', /help $
              else if event.select then begin
                 to = gr_tm_opts(pdefs.ysty_r.time, $
                                 pdefs.ysty_r.tzero, group = $
                                 pdefs.ids.graffer)
                 pdefs.ysty_r.time = to(0)
                 pdefs.ysty_r.tzero = to(1)
                 nch = 5
              endif else pdefs.ysty_r.time = pdefs.ysty_r.time and (not 1)
           end
           'Origin Axis': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Toggle inclusion of Y(r) ' + $
                            'axis at Y=0', /help $
              else if event.select then case val[2] of
                 "On": pdefs.ysty_r.extra = (pdefs.ysty_r.extra or 2) $
                                            and (not 8)
                 "Off": pdefs.ysty_r.extra = pdefs.ysty_r.extra and (not 10)
                 "Full": pdefs.ysty_r.extra = pdefs.ysty_r.extra or 10
              endcase
           end
           'Grid': begin
              if (track_flag and (n_elements(val) eq 2)) then $
                 graff_msg, pdefs.ids.hlptxt, /help, $
                            'Select Y(r)-grid options' $
              else if event.select then case val[2] of
                 ' None ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'No grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 0
                 end
                 
                 '______': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Solid grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 1
                 end
                 
                 '......': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dotted grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 2
                 end
                 '_ _ _ ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, 'Dashed grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 3
                 end
                 '_._._.': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Dash-dot grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 4
                 end
                 '_...  ': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Dash dot dot dot grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 5
                 end
                 '__  __': begin
                    if track_flag then $
                       graff_msg, pdefs.ids.hlptxt, /help, $
                                  'Long dash grid lines' $
                    else if event.select then pdefs.ysty_r.grid = 6
                 end
              endcase
           end
           'Autoscale': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Adjust the Y(r)-axis ' + $
                            'scaling to accomodate current data', /help $
              else case val[2] of
                 'Extend': gr_autoscale, pdefs, yaxis = 2  
                 'Extend or Shrink': gr_autoscale, pdefs, yaxis = $
                                                   2, /ignore
                 'Visible Only': gr_autoscale, pdefs, yaxis = 2, $
                                               /visible 
              endcase
           end
           'Advanced ...': begin
              if track_flag then $
                 graff_msg, pdefs.ids.hlptxt, 'Advanced axis ' + $
                            'settings for the Y(r) axis', /help $
              else ichange = gr_axis_adv_menu(pdefs, yaxis = 2)
           end
        endcase
     end
     'YrLAB': if track_flag then $
        graff_msg, pdefs.ids.hlptxt,/help, $
                   'Enter label for the Y(r) axis' $
     else pdefs.ytitle_r = event.value

  endcase

  if (idraw_flag) then gr_plot_object, pdefs
  if (ichange) then begin
     pdefs.chflag = 1b
     pdefs.transient.changes = pdefs.transient.changes+nch
     if (pdefs.transient.changes gt 20) then gr_bin_save, pdefs, /auto
  endif
  widget_control, pdefs.ids.chtick, map = pdefs.chflag

Miss_case:

  widget_control, base, set_uvalue = pdefs, /no_copy

end


pro Gr_axis_menu, axis, base, pdefs

  common graffer_options, optblock

  tjb = widget_base(base, $
                    /column, $
                    /frame, $
                    xpad = 0, $
                    ypad = 0, $
                    space = 0, $
                    event_pro = 'gr_axis_event')
  
  junk = widget_label(tjb, value = axis+'-Axis')

                                ; Title

  title = cw_enter(tjb, $
                   /all_events, $
                   value = '', $
                   xsize = 25, $
                   uvalue = axis+'LAB', $
                   label = axis+' Label:', $
                   track = optblock.track, $
                   /capture, $
                   /graphics)


                                ; Log/linear

  jb = widget_base(tjb, /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  ;; log = widget_droplist(jb, $
  ;;                       value = ['Linear', 'Log'], $
  ;;                       uvalue = axis+'LOG', $
  ;;                       title = axis+' Log/Lin:', $
  ;;                       track = optblock.track)

                                ; Exact or rounded axis range

  box_ok =  ~(axis eq 'Yr' || (axis eq 'Y' && pdefs.y_right))
  oa_ok = axis ne 'X' ||  ~pdefs.y_right

  stydesc = [{AXMENU_OPTS, flag:3, label:axis+' style', state:0b, $
              group: 0, sensitive: 1b},  $
             {axmenu_opts, 4, 'Logarithmic', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Exact Range', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Extended Range', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Draw Axes', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Draw Box Axis', 0b, 0, box_ok}, $
             {axmenu_opts, 4, 'Minor Ticks', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Annotation', 0b, 0, 1b}, $
             {axmenu_opts, 4, 'Time Labelling', 0b, 0, 1b}, $
             {axmenu_opts, 1, 'Origin Axis', 0b, 0, oa_ok}, $
             {axmenu_opts, 4, 'Off', 1b, 1, 1b}, $
             {axmenu_opts, 4, 'On', 0b, 1, 1b}, $
             {axmenu_opts, 6, 'Full', 0b, 1, 1b}, $
             {axmenu_opts, 1, 'Grid', 0b, 0, 1b}, $
             {axmenu_opts, 4, ' None ', 1b, 2, 1b}, $
             {axmenu_opts, 4, '______', 0b, 2, 1b}, $
             {axmenu_opts, 4, '......', 0b, 2, 1b}, $
             {axmenu_opts, 4, '_ _ _ ', 0b, 2, 1b}, $
             {axmenu_opts, 4, '_._._.', 0b, 2, 1b}, $
             {axmenu_opts, 4, '_...  ', 0b, 2, 1b}, $
             {axmenu_opts, 6, '__  __', 0b, 2, 1b}, $
             {axmenu_opts, 1, 'Autoscale', 0b, 0, 1b}, $
             {axmenu_opts, 0, 'Extend', 0b, 0, 1b}, $
             {axmenu_opts, 0, 'Extend or Shrink', 0b, 0, 1b}, $
             {axmenu_opts, 2, 'Visible Only', 0b, 0, 1b}, $
             {axmenu_opts, 2, 'Advanced ...', 0b, 0, 1b}]

  junk = widget_label(jb, $
                      value = 'Style:')

  junk = cw_pdmenu_plus(jb, $
                        stydesc, $
                        return_type = 'full_name', $
                        uvalue = axis+'STY', $
                        track = optblock.track, $
                        delimiter = '/', $
                        ids = buts)

;if (axis eq 'Yr') then widget_control, buts[19], sensitive = 0

  if axis eq 'Y' then pdefs.ids.y_box = buts[4]
  if axis eq 'X' then pdefs.ids.x_origin = buts[8]

  asty_pos = [1, 2, 3, 4, 5, 6, 7, 8, 9, 13]
  
                                ; Minimum

  jb = widget_base(tjb, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  

  amin = cw_enter(jb, $
                  /double, $
                  /all_events, $
                  value = 0., $
                  xsize = 12, $
                  uvalue = axis+'MIN', $
                  label = axis+' Min:', $
                  format = "(g14.7)", $
                  track = optblock.track, $
                  /capture)

                                ; Maximum

  amax = cw_enter(jb, $
                  /double, $
                  /all_events, $
                  value = 0., $
                  xsize = 12, $
                  uvalue = axis+'MAX', $
                  label = 'Max:', $
                  format = "(g14.7)", $
                  track = optblock.track, $
                  /capture)

  if (axis eq 'X') then begin
     pdefs.ids.xtitle = title
;;     pdefs.ids.xlog = log
     pdefs.ids.xmin = amin
     pdefs.ids.xmax = amax
     pdefs.ids.xsty = buts[asty_pos]
  endif else if (axis eq 'Y') then begin
     pdefs.ids.ytitle = title
;;     pdefs.ids.ylog = log
     pdefs.ids.ymin = amin
     pdefs.ids.ymax = amax
     pdefs.ids.ysty = buts[asty_pos]
  endif else if (axis eq 'Yr') then begin
     pdefs.ids.ytitle_r = title
;;     pdefs.ids.ylog_r = log
     pdefs.ids.ymin_r = amin
     pdefs.ids.ymax_r = amax
     pdefs.ids.ysty_r = buts[asty_pos]
  endif else message, '** O U C H ** Unknown axis ('+axis+')'

end
