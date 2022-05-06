; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; NAME:
;	gr_ds_menu
;
;
; PURPOSE:
;	Menus for adding, sorting etc. datasets in graffer
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	gr_ds_menu, base, pdefs
;
;
; INPUTS:
;	base	long	The parent widget_base for the menus
;	pdefs	struct	The graffer master data structure
;
;
; MODIFICATION HISTORY:
;	Extracted from graff_one: 4/7/05; SJT
;	Add "current_only" option: 26/1/12; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

pro gr_ds_event, event

  widget_control, event.id, get_uvalue = object

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1

  idraw_flag = 0b

  case object of
     'DESC': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Give the current data set a ' + $
                   'memorable description' $
     else begin
        (*pdefs.data)[pdefs.cset].descript = event.value
        idraw_flag = pdefs.key.use
        pdefs.transient.changes++
        pdefs.chflag = 1b
     endelse

     'CURRENT_ONLY': if track_flag then $ 
        graff_msg, pdefs.ids.hlptxt, /help, $
                   "Toggle display of only the current DS" $
     else begin
        pdefs.transient.current_only = event.select
        idraw_flag = 1b
        ichange = 0b
     endelse

     else: stop, "Impossible"
  endcase
  if (idraw_flag) then gr_plot_object, pdefs
  if (pdefs.chflag) then begin
     if (pdefs.transient.changes gt 20) then begin
        gr_bin_save, pdefs, /auto
     endif
  endif
  widget_control, pdefs.ids.chtick, map = pdefs.chflag
  widget_control, base, set_uvalue = pdefs

end

pro gr_ds_menu, base, pdefs

  common graffer_options, optblock

  tjb = widget_base(base, $
                    /column, $
                    /frame, $
                    event_pro = 'gr_ds_event')

  junk = widget_label(tjb, value = 'Data Set:')

  gr_ds_pulldown, tjb, pdefs


  jb = widget_base(tjb, /row, xpad = 0, ypad = 0, space = 0)

  pdefs.ids.descr = cw_enter(jb, $
                             /all, $
                             xsize = 15, $
                             value = '', $
                             format = "(A)", $
                             label = 'DS Name:', $
                             uvalue = 'DESC', $
                             track = optblock.track, $
                             /capture)

  pdefs.ids.cset = cw_enter(jb, $
                            /int, $
                            /display, $
                            xsize = 3, $
                            value = 0, $
                            format = '(I0)', $
                            label = '#:')


  
  pdefs.ids.type = cw_enter(tjb, $
                            /display, $
                            /text, $
                            xsize = 34, $
                            format = "(a)", $
                            value = '', $
                            label = 'Type:')
  
  jbb = widget_base(tjb, $
                   /row, $
                   /nonexclusive)

  pdefs.ids.current = widget_button(jbb, $
                                    value = "Only show current DS", $
                                    uvalue = 'CURRENT_ONLY', $
                                    track = optblock.track)

end
