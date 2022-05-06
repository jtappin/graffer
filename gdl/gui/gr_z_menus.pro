; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_Z_MENUS
;	Defines the menus for options for 2-D datasets.
;
; Usage:
;	gr_z_menus, optbb, pdefs
;
; Arguments:
;	optbb	long	input	The widget ID of the parent base
;	pdefs	struct	in/out	The Graffer data & control structure.
;
; History:
;	Original: 11/12/96; SJT
;	Move handler in: 17/1/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Add hidden option: 26/1/12; SJT
;-

pro Gr_z_event, event

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy

  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1

  if (track_flag) then begin    ; All this is much simplified as
                                ; there's only 1 object in this
                                ; handler's domain
     if (event.enter eq 0) then $
        graff_msg, pdefs.ids.hlptxt, /help, '' $
     else graff_msg, pdefs.ids.hlptxt, 'Select contoured or ' + $
                     '"image" display format', /help
     
  endif else begin
     (*pdefs.data)[pdefs.cset].zopts.format = event.index
     widget_control, pdefs.ids.zopts.bases[0], map = event.index eq 0
     widget_control, pdefs.ids.zopts.bases[1], map = event.index eq 1

     gr_plot_object, pdefs
     pdefs.chflag = 1b
     gr_bin_save, pdefs, /auto
     widget_control, pdefs.ids.chtick, map = pdefs.chflag
  endelse

  widget_control, base, set_uvalue = pdefs, /no_copy

end


pro Gr_z_menus, optbb, pdefs

  common graffer_options, optblock

  pdefs.ids.plopts[1] = widget_base(optbb, $
                                    /row, $
                                    event_pro = 'gr_z_event')


  jb = widget_base(pdefs.ids.plopts[1], $
                   /column)

  pdefs.ids.zmode = widget_droplist(jb, $
                                    value = ['Contoured...', $
                                             'Colour/Grey...', $
                                             'Hidden'], $ 
                                    uvalue = 'CONCOL', $  
                                    title = 'Select display format', $
                                    track = optblock.track)
  widget_control, pdefs.ids.zmode, set_droplist_select = $
                  (*pdefs.data)[pdefs.cset].zopts.format

  sb = widget_base(jb)

  gr_cont_menus, sb, pdefs
  gr_img_menus, sb, pdefs

; This is needed to ensure that the X-Y ds menu doesn't get truncated!
  
  for j = 0, 4 do junk = widget_label(jb, $
                                      value = ' ')
  
end
