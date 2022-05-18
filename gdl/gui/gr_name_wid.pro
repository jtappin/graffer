; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_NAME_WID
;	Request a variable name.
;
; Usage:
; 	name = gr_name_wid(top)
;
; Returns:
;	The name of the variable, or an empty string.
;
; Argument:
;	top	long	The widget id of the top-level widget of the
;			caller. 
;
; History:
;	Original: 3/1/18(?); SJT
;	Document: 3/7/18; SJT
;	Replace xmanager call: 1/4/22; SJT
;-

function grname_event, event

  widget_control, event.top, get_uvalue = state
  widget_control, event.id, get_uvalue = mnu

  evr = {id: event.handler, $
         top: event.top, $
         handler: event.handler, $
         name: '', $
         action: 0}
  
  case mnu of
     'QUIT': begin
        case event.value of
           'DO': begin
              evr.action = 1
              widget_control, state.namid, get_value = name
              evr.name = name
           end
           'DONT': begin
              evr.name = ''
              evr.action = -1
           end
        endcase
     end
     'NAME':
  endcase

  return, evr
end

function gr_name_wid, top

  defname = 'grf_image'

  widget_control, top, sensitive = 0
  
  base = widget_base(group = top, $
                     /column, $
                     title = "Variable name")

  namid = cw_enter(base, $
                  label = 'Variable name:', $
                   /all, $
                   /capture, $
                   /text, $
                   xsize = 20, $
                   value = defname, $
                   uvalue = 'NAME')

  junk = cw_bgroup(base, $
                   ['Apply', 'Cancel'], $
                   /row, $
                   button_uvalue = ['DO', 'DONT'], $
                   uvalue = 'QUIT')

  state = {namid: namid}

  widget_control, base, /real, set_uvalue = state, $
                  event_func = 'grname_event'

  repeat begin
     ev = widget_event(base)
  endrep until ev.action ne 0

  if ev.action eq 1 then rname = ev.name $
  else rname = ''
  widget_control, base, /destroy
  
  widget_control, top, /sensitive
  
  return, rname
end
