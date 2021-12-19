; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_FCOPY_MENU
;	Select a function to copy to the current dataset.
;
; Usage:
;	icopy=gr_fcopy_menu(pdefs)
;
; Returns:
;	1 of success, 0 of failure.
;
; Argument:
;	pdefs	struct	in/out	The main GRAFFER data structure.
;
; History:
;	Original: 7/2/12; SJT
;-

function fcopy_event, event

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
    "ALL": begin
        data = uvs.data
        if event.select then $
          locs = where(data.type lt 0) $
        else locs = where(data.type eq data[uvs.current].type)
        ll = where(locs ne uvs.current)
        locs = locs[ll]
        types = (['y=F(x)        : ', $
                  'x=F(y)        : ', $
                  'x=F(t), y=G(t): ', $
                  'z=F(x,y)      : '])[-1-data[locs].type]
        idx = string(locs+1, format = "(I3,'] ')")

        desc = idx+types+data[locs].descript
        widget_control, uvs.list, set_value = desc, set_list_select $
          = -1
        uvs.index = -1
        *uvs.locs = locs
        uvs.all = event.select
    end
endcase

widget_control, uvs.dobut, sensitive = uvs.index ge 0
widget_control, lab, set_uvalue = uvs

return, {id: event.id, $
         top: event.top, $
         handler: 0l, $
         exited: iexit}

end

function gr_fcopy_menu, pdefs

data = *pdefs.data

funlocs = where(data.type lt 0, nf)

if nf eq 0 || nf eq 1 && funlocs[0] eq pdefs.cset then begin
    junk = dialog_message(['There are no function datasets', $
                           'available to copy'], $
                          /info, $
                          dialog_parent = pdefs.ids.graffer, $
                          resource = 'Graffer')
    return, 0
endif

if (data[pdefs.cset].type ge 0 && $
    ptr_valid(data[pdefs.cset].xydata) && $
    data[pdefs.cset].ndata gt 0) then begin
    ans = dialog_message(['Current dataset is not a function.', $
                          'Do you want to overwrite it?'], $
                         /question, $
                         dialog_parent = pdefs.ids.graffer, $
                         resource = 'Graffer')
    if ans eq 'No' then return, 0
endif

if data[pdefs.cset].type lt 0 then begin
    locs = where(data.type eq data[pdefs.cset].type, nmf)
    if nmf gt 0 then begin
        all = nmf eq nf
        ll = where(locs ne pdefs.cset, nnc)
    endif
    if nmf eq 0 || nnc eq 0 then begin
        ans = dialog_message(['There are no functions of the same', $
                              'kind as the current dataset', $
                              'Do you want to use all functions?'], $
                             /question, $
                             dialog_parent = pdefs.ids.graffer, $
                             resource = 'Graffer')
        if ans eq 'No' then return, 0
        locs = funlocs
        nmf = nf
        all = 1b
        ll = where(locs ne pdefs.cset, nnc)
    endif
    locs = locs[ll]
endif else begin
    locs = funlocs
    nmf = nf
    all = 1b
endelse

types = (['y=F(x)        : ', $
          'x=F(y)        : ', $
          'x=F(t), y=G(t): ', $
          'z=F(x,y)      : '])[-1-data[locs].type]
idx = string(locs+1, format = "(I3,'] ')")

desc = idx+types+data[locs].descript

base = widget_base(title = 'Select function to copy', $
                   resource = 'Graffer', $
                   /column, $
                   event_func = 'fcopy_event')

lab = widget_label(base, $
                   value = 'Select function to copy')

list = widget_list(base, $
                   ysize = 4 > n_elements(desc) < 12, $
                   value = desc, $
                   uvalue = 'PICK')
if (~all) then begin
    jb = widget_base(base, $
                     /row, $
                     /nonexclusive)
    junk = widget_button(jb, $
                         value = 'Show all functions?', $
                         uvalue = 'ALL')
endif

jb = widget_base(base, $
                 /row)

dobut = widget_button(jb, $
                      value = 'Apply', $
                      uvalue = 'DO', $
                      sensitive = 0)

junk = widget_button(jb, $
                     value = 'Cancel', $
                     uvalue = 'DONT')

uvs = {list: list, $
       dobut: dobut, $
       data: data, $
       current: pdefs.cset, $
       locs: ptr_new(locs), $
       index: -1l, $
       all: all}

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

idx = (*uvs.locs)[uvs.index]
ptr_free, uvs.locs

return, gr_func_copy(pdefs, idx, force = uvs.all)

end
