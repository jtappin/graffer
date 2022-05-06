; LICENCE:
; Copyright (C) 2022: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_MSG_RECALL
;	Recall past diagnostic messages.
;
; Usage:
;	graff_msg_recall, id
;
; Argument:
;	id	long	Widget ID of the group leader.
;
; History:
;	Boiler plate: 6/5/22; SJT
;-

function gr_msg_rcl_event, event
  evr =  {id: event.handler, $
          top: event.top, $
          handler: 0l}
  
  return, evr
end

pro graff_msg_recall, id
  common graff_msg_log, messages

  if widget_info(/valid, id) then widget_control, id, sensitive = 0
  
  base = widget_base(title = "Message log", $
                     /column, $
                     group = id)

  msgid = widget_text(base, $
                      xsize = 80, $
                      ysize = 20, $
                      /scroll)

  junk = widget_button(base, $
                       value = 'Quit', $
                       uvalue = 'QUIT')

  if n_elements(messages) ne 0 then begin
     for j = 0, messages.count()-1 do begin
        if j ne 0 then widget_control, msgid, set_value = '', /append
        widget_control, msgid, set_value = messages[j], /append
     endfor
  endif
  
  widget_control, base, /real, event_func = 'gr_msg_rcl_event'

  ev = widget_event(base)
  widget_control, base, /destroy
  if widget_info(/valid, id) then widget_control, id, /sensitive

end
