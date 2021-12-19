; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_APP_W
;	Menu to control GR_APPEND
;
; Usage:
;	gr_app_w, pdefs
;
; Argument:
;	pdefs	struct	in/out	The GRAFFER data structure
;
; History:
;	Original: 12/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;	Fix labelling: 11/1/12; SJT
;-

function gr_app_is_ok, idx, type, count

  rv = (type eq type[idx] or count[idx] eq 0) and $
       count ne 0

  rv[idx] = 0b

  return, rv
end
function Gr_app_event, event

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, event.id, get_uvalue = but

  iexit = 0
  case (but) of
     'CANCEL': iexit = -1
     'DONE': iexit = 1
     
     'EXTEND': if (event.select ne 0) then begin
        uv.res[0] = event.value

        sst = gr_app_is_ok(uv.res[0], uv.dtypes, uv.dcount)
        
        for j = 0, n_elements(uv.apids)-1 do $
           widget_control, uv.apids[j], sensitive = sst[j]
                           ;; (uv.dtypes(j) eq uv.dtypes(uv.res[0])) && $
                           ;; j ne uv.res[0]
        
        widget_control, uv.dobut, sensitive = uv.res[0] ne uv.res[1]
     endif
     
     'APPEND': if (event.select ne 0) then begin
        uv.res[1] = event.value
        widget_control, uv.dobut, /sensitive
     endif
     
     'DELETE': uv.delete = event.select
     'SORT': uv.sort = event.select
  endcase

  widget_control, base, set_uvalue = uv, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:event.handler, $
           Exit:iexit}

end

pro Gr_app_w, pdefs

;	First generate the list of DSS

  data = *pdefs.data

  tc = ['XY', 'XYE', 'XYEE', 'XYF', 'XYFF', 'XYFE', 'XYFEE', 'XYFFE', $
        'XYFFEE']
  xydi = where(data.type ge 0 and data.type le 8, nxy) ; X/Y data only
  if (nxy eq 0) then begin
     junk = dialog_message(["No mergable datasets", $
                            "All datasets are functions or 2-D"], $
                           dialog_parent = pdefs.ids.graffer)
     return
  endif

  res0 = where(xydi eq pdefs.cset) > 0
  res0 = res0[0]                ; Force to scalar
  
  dlist = data[xydi].descript
  dtypes = data[xydi].type
  dcount = data[xydi].ndata
  
  nlist = string(xydi+1, $
                 format = "(I3,')')")

  llist = string(data[xydi].ndata, format = "(' <',I0,'> ')")
  lll = max(strlen(llist))
  fmt = "(A"+string(lll, format = "(I0)")+")"
  llist = string(llist, format = fmt)
  nlist = nlist+llist
  nlist = nlist+string(tc[dtypes], format = "(A6, ' - ')")

  dlist = nlist+dlist


;	Build the menus.

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer data set merge', $
                    group = pdefs.ids.graffer, $
                    resource = 'Graffer')

  base = widget_base(tlb, $
                     /column)

  if pdefs.nsets gt 25 then begin
     device, get_screen_size = sxy
     pbase = widget_base(base, $
                         /row, $
                         /scroll, $
                         y_scroll_size = sxy[1]/2)
  endif else pbase = widget_base(base, $
                                 /row)
  exid = cw_bgroup(pbase, $
                   dlist, $
                   /column, $
                   /exclusive, $
                   label_top = 'Dataset to extend', $
                   /return_index, $
                   ids = exids, $
                   set_value = res0, $
                   uvalue = 'EXTEND') 
  apid = cw_bgroup(pbase, $
                   dlist, $
                   /column, $
                   /exclusive, $
                   label_top = 'Dataset to append', $
                   /return_index, $
                   ids = apids, $
                   set_value = res0, $
                   uvalue = 'APPEND')
  sst = gr_app_is_ok(res0, dtypes, dcount)

  for j = 0, n_elements(apids)-1 do $
     widget_control, apids[j], sensitive = sst[j]
  
  jb = widget_base(base, /row, /nonexclusive)
  junk = widget_button(jb, $
                       value = 'Delete appended', $
                       uvalue = 'DELETE')
  junk = widget_button(jb, $
                       value = 'Sort resultant', $
                       uvalue = 'SORT')

  jb = widget_base(base, $
                   /row)
  dobut = widget_button(jb, $
                        value = '       Apply       ', $
                        uvalue = 'DONE')
  junk = widget_button(jb, $
                       value = '      Cancel      ', $
                       uvalue = 'CANCEL')
  widget_control, dobut, sensitive = 0

  widget_control, base, set_uvalue = {exid:exid,  $
                                      Exids:exids,  $
                                      Apid:apid,  $
                                      Apids:apids, $
                                      Dobut:dobut, $
                                      Dtypes:dtypes, $
                                      dcount: dcount, $
                                      Res: [res0, res0], $
                                      Delete:0, $
                                      sort:0}, $
                  event_func = 'gr_app_event'

  widget_control, tlb, /real

;			DIY widget management here

  repeat ev = widget_event(base) until ev.exit ne 0

  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exit eq -1) then return

  gr_append, pdefs, xydi(uv.res[0]), xydi(uv.res[1]), delete = $
             uv.delete, sort = uv.sort

end
