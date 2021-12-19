; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_ZCOPY_MENU
;	Select a function to copy to the current dataset.
;
; Usage:
;	icopy=gr_zcopy_menu(pdefs)
;
; Returns:
;	1 of success, 0 of failure.
;
; Argument:
;	pdefs	struct	in/out	The main GRAFFER data structure.
;
; History:
;	Original (after gr_xycopy_menu): 7/2/12; SJT
;-

function zcopy_event, event

lab = widget_info(event.top, /child)
widget_control, lab, get_uvalue = uvs
widget_control, event.id, get_uvalue = but

iexit = 0
case but of
    "DONT": iexit = -1
    "DO": iexit = 1
    "PICK": begin
        uvs.index = event.index
        if event.clicks eq 2 then iexit = 1
    end
endcase

widget_control, uvs.dobut, sensitive = uvs.index ge 0
widget_control, lab, set_uvalue = uvs

return, {id: event.id, $
         top: event.top, $
         handler: 0l, $
         exited: iexit}

end

function gr_zcopy_menu, pdefs

  data = *pdefs.data

  funlocs = where(data.type eq 9 and data.ndata gt 0 and $
                  data.ndata2 gt 0, nf)

  if nf eq 0 || nf eq 1 && funlocs[0] eq pdefs.cset then begin
     junk = dialog_message(['There are no Z datasets', $
                            'available to copy'], $
                           /info, $
                           dialog_parent = pdefs.ids.graffer, $
                           resource = 'Graffer')
     return, 0
  endif

  if (data[pdefs.cset].type ne 9) then begin
     ans = dialog_message(['Current dataset is not an Z dataset.', $
                           'Do you want to overwrite it?'], $
                          /question, $
                          dialog_parent = pdefs.ids.graffer, $
                          resource = 'Graffer')
     if ans eq 'No' then return, 0
  endif

  if data[pdefs.cset].type eq 9 then begin
     ll = where(funlocs ne pdefs.cset, nnc)
     if nnc eq 0 then begin
        ans = dialog_message(['There are no other 2-D datasets', $
                              'aborting.'], $
                             /info, $
                             dialog_parent = pdefs.ids.graffer, $
                             resource = 'Graffer')
        return, 0
     endif
     locs = funlocs[ll]
  endif else begin
     locs = funlocs
     nmf = nf
  endelse


  idx = string(locs+1, format = "(I3,'] ')")
  ndata = string(data[locs].ndata, data[locs].ndata2, format = $
                 "('(',I5,',',I5,')')")

  desc = idx+ndata+' '+data[locs].descript

  base = widget_base(title = 'Select dataset to copy', $
                     resource = 'Graffer', $
                     /column, $
                     event_func = 'zcopy_event')

  lab = widget_label(base, $
                     value = 'Select dataset to copy')

  list = widget_list(base, $
                     ysize = 4 > n_elements(desc) < 12, $
                     value = desc, $
                     uvalue = 'PICK')

  jb = widget_base(base, $
                   /row)

  dobut = widget_button(jb, $
                        value = 'Apply', $
                        uvalue = 'DO', $
                        sensitive = 0)

  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  uvs = {dobut: dobut, $
         index: -1l}

  widget_control, lab, set_uvalue = uvs

  widget_control, pdefs.ids.graffer, sensitive = 0
  widget_control, base, /real

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)
  widget_control, lab, get_uvalue = uvs

  widget_control, base, /destroy
  widget_control, pdefs.ids.graffer, sensitive = 1

  if ev.exited eq -1 then return, 0

  idx = locs[uvs.index]

  return, gr_z_copy(pdefs, idx, /force)

end
