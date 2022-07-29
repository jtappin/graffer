; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Graff_msg, mwid, message, help = help

;+
; GRAFF_MSG
;	Display a message in the graffer message box
;
; Usage:
;	graff_msg, mwid, message
;
; Arguments:
;	mwid	long	input	Widget ID of message box
;	message	string	input	The message
;
; History:
;	Original: 18/8/95; SJT
;	Change to take widget ID as first argument: 12/5/95; SJT
;	Just print the message (no widgets exist): ??/??/2013; SJT
;	Add the ID argument back, but ignore it if it's there:
;	19/6/20; SJT
;-

  common graff_msg_log, messages

  if (widget_info(mwid, /valid)) then $
     widget_control, mwid, set_value = message $
  else print, message

  if ~keyword_set(help) then begin
     if n_elements(messages) eq 0 then messages = list()
     now = systime()
     dss = strsplit(now, /extr)
     tp = scope_traceback(/str)
     hdr = dss[3]+': '+tp[-2].routine

     messages.add, [hdr, message]
  endif
end
