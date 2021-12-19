; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_SORT_DSS
;	Change data set
;
; Usage:
;	graff_sort_dss, pdefs
;
; Argument:
;	pdefs	struct	in/out	The plot definition structure.
;
; History:
;	Original (after graff_ch_dset): 6/12/96; SJT
;	Convert handles to pointers: 28/6/05; SJT
;	Prevent use of "DO" on incomplete sorting operation: 19/12/11; SJT
;-

function Grf_sort_event, event

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, event.id, get_uvalue = but

  iexit = 0
  case (but) of
     'CHOOSE': begin
        if (uv.moving eq -1) then begin
           if ~uv.iflag then begin
              uv.moving = (*uv.list)(event.index)
              if (event.index eq 0) then (*uv.list) = (*uv.list)(1:*) $
              else if (event.index eq n_elements((*uv.list))-1) then $
                 (*uv.list) = (*uv.list)(0:n_elements((*uv.list))-2) $
              else (*uv.list) = [(*uv.list)(0:event.index-1), $
                                 (*uv.list)(event.index+1:*)]
              widget_control, event.id, set_value = ['<start>', $
                                                     uv.dlist((*uv.list))]
              widget_control, uv.label, set_value = 'Place it after'
              widget_control, uv.dobut, sensitive = 0
           endif else uv.iflag = 0b
        endif else begin
           if (event.index eq 0) then (*uv.list) = [uv.moving, (*uv.list)] $
           else if (event.index eq n_elements((*uv.list))) then $
              (*uv.list) = [(*uv.list), uv.moving] $ $
           else (*uv.list) = [(*uv.list)(0:event.index-1), uv.moving, $
                              (*uv.list)(event.index:*)]
           uv.moving = -1
           widget_control, event.id, set_value = [uv.dlist((*uv.list))]
           widget_control, uv.label, set_value = 'Data set to move'
           widget_control, uv.dobut, sensitive = 1
        endelse
     end
     
     'DO': iexit = 1
     
     'DONT': iexit = -1
  end

  widget_control, base, set_uvalue = uv, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:event.handler, $
           Exit:iexit}
end

pro Graff_sort_dss, pdefs

  tc = ['F(XY)', 'PF', 'F(Y)', 'F(X)', 'XY', 'XYE', 'XYEE', 'XYF', $
        'XYFF', $
        'XYFE', 'XYFEE', 'XYFFE', 'XYFFEE', 'Z']

  dlist = (*pdefs.data).descript

  indlist = indgen(n_elements(dlist))
  nlist = string(indlist+1, format = "(I3,')')")

  llist = string([(*pdefs.data).ndata, 0], format = "(' <',I0,'> ')")
  lll = max(strlen(llist))
  fmt = "(A"+string(lll, format = "(I0)")+")"
  llist = string(llist, format = fmt)
  nlist = nlist+llist
  nlist = nlist+string(tc((*pdefs.data).type+4), format = "(A6, ' - ')")

  dlist = nlist+dlist

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer data set sort', group = $
                    pdefs.ids.graffer, resource = 'Graffer')
  base = widget_base(tlb, /column)

  curr = widget_label(base, $
                      value = 'Data set to move')
  junk = widget_list(base, $
                     value = dlist, $
                     uvalue = 'CHOOSE',  $
                     ysize = (12 < n_elements(dlist)))
  widget_control, junk, set_list_select = -1 ;pdefs.cset

  jb = widget_base(base, /row)

  dobut = widget_button(jb, $
                        value = 'Apply', $
                        uvalue = 'DO', $
                        sensitive = 0)
  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  uv = {dlist:dlist, $
        Label: curr, $
        List:ptr_new(indlist), $
        dobut: dobut, $
        Moving: -1, $ 
        iflag: is_gdl()}        ; GDL sends a spurious select event
                                ; that needs to be swallowed.

  widget_control, tlb, /real

  widget_control, base, set_uvalue = uv, $
                  event_func = 'grf_sort_event'


;			DIY widget management here

  repeat ev = widget_event(base) until ev.exit ne 0

  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exit eq -1) then return

  (*pdefs.data) = (*pdefs.data)((*uv.list))
  if ptr_valid(pdefs.key.list) then begin
     ikey = bytarr(n_elements((*pdefs.data)))
     ikey(*pdefs.key.list) = 1b
     ikey = ikey((*uv.list))
     *pdefs.key.list = where(ikey)
  endif

  loc = where((*uv.list) eq pdefs.cset)
  pdefs.cset = loc(0)
  
  graff_set_vals, pdefs, /set_only
  ptr_free, uv.list

end

