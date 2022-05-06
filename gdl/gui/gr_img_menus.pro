; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_IMG_MENUS
;	Menu for setting options for "IMAGE" format display.
;
; Usage:
;	gr_img_menus, sb, pdefs
;
; Arguments:
; 	sb	long	input	The base widget into which to put the
; 				menu.
;	pdefs	struct	input	The Graffer data & control structure.
;
; History:
;	Original (after gr_img_menus): 13/12/11; SJT
;	Revert to original name: 5/1/12; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

pro Img_event, event

  widget_control, event.id, get_uvalue = but
  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy
  zopts = (*pdefs.data)[pdefs.cset].zopts
  localct = !d.n_colors gt 256

  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  if (track_flag) then begin
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case but of
     'R1': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Enter lower bound of range" $
     else begin
        widget_control, event.id, get_value = r1
        zopts.range[0] = r1
     endelse
     'R2': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Enter upper bound of range" $
     else begin
        widget_control, event.id, get_value = r2
        zopts.range[1] = r2
     endelse

     'PX': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Enter pixel size in mm for PS " + $
                   "output" $
     else begin
        widget_control, event.id, get_value = px
        zopts.pxsize = px
     endelse

     'LOG': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Use logarithmic colour scaling?" $
     else zopts.ilog = event.index

     'INVERT': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Invert the colour map?" $
     else zopts.invert = event.select
     
     'TAB': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Select colour table" $
     else if localct then zopts.ctable = event.index+1 $
     else pdefs.table = event.index

     'GAM': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Set the gamma value for the colour " + $
                   "map" $
     else begin
        widget_control, event.id, get_value = g
        if localct then zopts.gamma = g $
        else pdefs.gamma = g
     endelse

     'MISS': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, "Set a value for points " + $
                   "that don't map into the data" $
     else begin
        widget_control, event.id, get_value = m
        zopts.missing = m
     endelse
  endcase

  if ~track_flag then begin
     (*pdefs.data)[pdefs.cset].zopts = zopts
     gr_plot_object, pdefs
     pdefs.chflag = 1b
     pdefs.transient.changes = pdefs.transient.changes+1
     if (pdefs.transient.changes gt 20) then begin
        gr_bin_save, pdefs, /auto
     endif
     widget_control, pdefs.ids.chtick, map = pdefs.chflag
  endif

miss_case:

  widget_control, event.handler, set_uvalue = uv
  widget_control, base, set_uvalue = pdefs, /no_copy

end

pro Gr_img_menus, sb, pdefs

  common graffer_options, optblock

  i = pdefs.cset
  zopts = (*pdefs.data)[i].zopts

  local_ct = !d.n_colors gt 256
  if (local_ct and zopts.ctable gt 0) then begin
     ctable = zopts.ctable-1
     gamma = zopts.gamma
  endif else begin
     ctable = pdefs.ctable
     gamma = pdefs.gamma
  endelse

  base = widget_base(sb, $
                     /column, $
                     event_pro = 'img_event', $
                     map = zopts.format, $
                     xpad = 0, $
                     ypad = 0, $
                     space = 0)
  pdefs.ids.zopts.bases[1] = base


  itl = ''
  loadct, get_names = itl

  junk = widget_label(base, $
                      value = 'Colour Table')
  pdefs.ids.zopts.i_ctable = widget_list(base, $
                                         value = itl, $
                                         uvalue = 'TAB', $
                                         ysize = 5)
  widget_control, pdefs.ids.zopts.i_ctable, set_list_select = ctable

  obase = widget_base(base, $
                      /column, $
                      xpad = 0, $
                      ypad = 0, $
                      space = 0)

  junk = widget_label(obase, value = 'Range')
  jb = widget_base(obase, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)

  pdefs.ids.zopts.i_range[0] = cw_enter(jb, $
                                        /double, $
                                        value = zopts.range[0], $
                                        format = "(g12.4)", $
                                        xsize = 12, $
                                        uvalue = 'R1', $
                                        label = 'Min:', $
                                        /capture, $
                                        track = optblock.track, $
                                        /all_events)
  pdefs.ids.zopts.i_range[1] = cw_enter(jb, $
                                        /double, $
                                        value = zopts.range[1], $
                                        format = "(g12.4)", $
                                        xsize = 12, $
                                        uvalue = 'R2', $
                                        label = 'Max:', $
                                        /capture, $
                                        track = optblock.track, $
                                        /all_events)
  
  jb = widget_base(obase, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)

  jba = widget_base(jb, $
                    /column)

  pdefs.ids.zopts.i_missid = cw_enter(jba, $
                                      /double, $
                                      value = zopts.missing, $
                                      xsize = 8, $
                                      uvalue = 'MISS', $
                                      label = "Missing:", $ $
                                      track = optblock.track, $
                                      /capture, $
                                      /all)

  
  pdefs.ids.zopts.i_log = $
     widget_droplist(jba, $
                     title = 'Scaling:', $
                     value = ['Linear', 'Logarithmic', 'Square Root'], $
                     $
                     $
                     uvalue = 'LOG', $
                     track = optblock.track)
  widget_control, pdefs.ids.zopts.i_log, $
                  set_droplist_select = zopts.ilog
  
  jbb = widget_base(jba, $
                    /nonexclusive, $
                    /column)
  pdefs.ids.zopts.i_invert = widget_button(jbb, $ $
                                           value = 'Invert colours?', $
                                           uvalue = 'INVERT', $
                                           track = optblock.track)
  widget_control, pdefs.ids.zopts.i_invert, set_button = zopts.invert
  
  jbb = widget_base(jb, $
                    space = 0, $
                    xpad = 0, $
                    ypad = 0, $
                    /column)
  pdefs.ids.zopts.i_pxsz = cw_enter(jbb, $
                                    /double, $
                                    value = zopts.pxsize, $
                                    format = "(f5.2)", $
                                    xsize = 5, $
                                    uvalue = 'PX', $
                                    label = 'Pixel Size:', $
                                    track = optblock.track, $
                                    /capture, $
                                    /all)
  
  pdefs.ids.zopts.i_gamma = cw_enter(jbb, $
                                     /double, $
                                     value = gamma, $
                                     format = "(f7.3)", $
                                     xsize = 9, $
                                     uvalue = 'GAM', $
                                     label = 'Gamma:', $
                                     track = optblock.track, $
                                     /capture, $
                                     /all_events)
  


end

