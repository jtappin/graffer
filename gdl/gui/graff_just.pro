; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_JUST
;	Set "non-standard" text justification
;
; Usage:
;	align = graff_just(initial, group=group)
;
; Return Value:
;	align	float	The new justification setting
;
; Argument:
;	initial	float	input	The previous justification setting
;
; Keyword:
;	group	long	input	The widget ID of the caller.
;
; History:
;	Original: 5/10/95; SJT
;-

function Just_event, event

widget_control, event.id, get_uvalue = but
widget_control, event.handler, get_uvalue = jsl

iexit = 0
alg = 0

case (but) of
    'JUST': alg = event.value
    'DONE': begin
        iexit = 1
        widget_control, jsl, get_value = alg
    end
endcase

return, {id:event.handler, $
         top:event.top, $
         handler:0l, $
         value:alg, $
         Exit:iexit}

end

function Graff_just, align, group=group

widget_control, group, sensitive = 0

tlb = widget_base(group = group, title = 'Graffer alignment',  $
                 resource = 'Graffer')
base = widget_base(tlb, /column, event_func = 'just_event')

jb = widget_base(base, /row)
junk = widget_label(jb, value = 'Left:')
jsl = cw_fslider(jb, value = align, min = 0., max = 1., /drag, uvalue $
                  = 'JUST', format = "(F5.3)")
junk = widget_label(jb, value = ':Right')

junk = widget_button(base, value = 'Done', uvalue = 'DONE')

widget_control, base, set_uvalue = jsl
widget_control, tlb, /real


repeat ev = widget_event(base) until ev.exit

widget_control, ev.top, /destroy
widget_control, group, /sensitive

return, ev.value

end
