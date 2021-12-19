; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_TM_OPTS
;	Set time labelling options
;
; History:
;	Shorten name: 25/11/96; SJT
;	Add CAPTURE key to text inputs: 6/2/97; SJT
;	Replace cw_bbselectors with widget_droplist: 14/12/11; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

function Gr_to_event, event

widget_control, event.id, get_uvalue = but
widget_control, event.handler, get_uvalue = topt, /no_copy

iexit = 0

case but of
    'DO': iexit = 1
    'CANCEL': iexit = -1
    
    'UNIT': topt.unit = event.index
    'MUNIT': topt.munit = event.index
    'ZERO': topt.zero = event.value
    
end

widget_control, topt.dispid, sensitive = topt.unit le 3
widget_control, topt.zid, sensitive = topt.unit le 3

widget_control, event.handler, set_uvalue = topt, /no_copy

rv = {id:event.id, $
      top:event.top, $
      handler:event.handler, $
      Exited:iexit}

return, rv

end

function Gr_tm_opts, tmopt, zero, group = group


  unit = (tmopt / 2) and 3
  munit = (tmopt / 8) and 3

  ulist = ['Seconds', 'Minutes', 'Hours', 'Days']

  widget_control, group, sensitive = 0

  tlb = widget_base(title = 'GRAFFER time opts', $
                    group = group, $
                    resource = 'Graffer')
  base = widget_base(tlb, $
                     /column)

  junk = widget_label(base, $
                      value = 'Time label options')

  jb = widget_base(base, $
                   /row)
  junk = widget_droplist(jb, $
                         value = ulist, $
                         title = 'Units', $
                         uvalue = 'UNIT')
  widget_control, junk, set_droplist_select = unit

  dispid = widget_droplist(jb, $
                           value = ulist, $
                           title = 'Display in', $
                           uvalue = 'MUNIT')
  widget_control, dispid, sensitive = unit le 3, set_droplist_select = munit

  zid = cw_enter(base, $
                 /int, $
                 value = zero, $
                 uvalue = 'ZERO', $
                 label = 'Label 0 as:', $
                 xsize = 5, $
                 /all, $
                 /capture)
  widget_control, zid, sensitive = unit le 3

  jb = widget_base(base, $
                   /row)
  junk = widget_button(jb, $
                       value = '   Apply   ', $
                       uvalue = 'DO')
  junk = widget_button(jb, $
                       value = '  Cancel  ', $
                       uvalue = 'CANCEL')

;	RYO widget management to allow us to get the values back from
;	the event handler without using a common block, even after the
;	hierarchy has been destroyed.

  widget_control, tlb, /real
  widget_control, base, event_func = 'gr_to_event',  $
                  set_uvalue = {unit:unit, $
                                munit:munit, $
                                zero:zero, $
                                dispid: dispid, $
                                zid: zid}

  repeat begin
     ev = widget_event(tlb)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = ntopt, /no_copy

  widget_control, ev.top, /destroy
  widget_control, group, sensitive = 1


  if (ev.exited eq -1) then return, [tmopt, zero]$
  else return, [1 + ntopt.unit*2 + ntopt.munit*8, ntopt.zero]

end

