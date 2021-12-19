; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_PICK_TLV
;	A widget to choose a top-level variable.
;
; Usage:
;	name = gr_pick_tlv(pdefs)
;
; Returns:
;	The variable name or an empty string
;
; Argument:
;	pdefs	struct	The main GRAFFER data structure.
;
; History:
;	Original: 6/2/12; SJT
;-

function gr_pick_tlv_event, event

widget_control, event.id, get_uvalue = but
lab = widget_info(event.top, /child)
widget_control, lab, get_uvalue = state
case but of
    "DONT": iexit = -1
    "DO": iexit = 1
    "PICK": begin
        state.idx = event.index
        if (event.clicks eq 2) then iexit = 1 $
        else iexit = 0
    end
    'LEVEL': begin
        state.level = event.index+1
        *state.namedesc = gr_examine_tlv(level = state.level, /exclude)
        widget_control, state.list, set_value = *state.namedesc, $
          set_list_select = -1
        state.idx = -1
        iexit = 0
    end
endcase

widget_control, state.dobut, sensitive = state.idx ge 0
widget_control, lab, set_uvalue = state

return, {id:event.handler, $
         top:event.top, $
         handler:0l, $
         exited:iexit}

end

function gr_pick_tlv, parent, level

if arg_present(level) then begin
    stack = scope_traceback(/structure)
    locs = where(stack.routine eq 'GRAFFER', ngr)
endif else ngr = 0

widget_control, parent, sensitive = 0

base = widget_base(group = parent, $
                   /column, $
                   title = "Pick top-level variable", $
                   resource = "Graffer", $
                   event_func = "gr_pick_tlv_event")

lab = widget_label(base, $
                   value = "Choose a top-level variable")

if ngr eq 1 && locs[0] gt 1 then $
  junk = widget_droplist(base, $
                         value = stack[0:locs[0]-1].routine, $
                         title = "Level:", $
                         uvalue = 'LEVEL')

namedesc = gr_examine_tlv(/exclude)

list = widget_list(base, $
                   value = namedesc, $
                   ysize = n_elements(namedesc) < 12, $
                   uvalue = 'PICK')

jb = widget_base(base, $
                 /row)

dobut = widget_button(jb, $
                      value = "Apply", $
                      uvalue = "DO", $
                      sensitive = 0)

junk = widget_button(jb, $
                     value = "Cancel", $
                     uvalue = "DONT")

widget_control, lab, set_uvalue = {list:list, $
                                   dobut: dobut, $
                                   namedesc: ptr_new(namedesc), $
                                   level: 1l, $
                                   idx: -1l}

widget_control, base, /real

repeat begin
    ev = widget_event(base)
endrep until (ev.exited ne 0)
widget_control, lab, get_uvalue = state, /no_copy

widget_control, parent, sensitive = 1
widget_control, base, /destroy

level = state.level
idx = state.idx
namedesc = *state.namedesc
ptr_free, state.namedesc

if ev.exited eq -1 then return, ''

namec = strsplit(namedesc[idx], ' ', /extr)
return, namec[0]

end
