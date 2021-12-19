; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_state, id, save = save

;+
; GR_STATE
;	Restore the entry state of graffer. 
;
; Usage:
;	gr_state[, id]
;
; Argument:
;	id	long	Widget ID of the calling widget. Not actually
;			used, but a KILL_NOTIFY event needs it.
;
; Keyword:
;	/save	If set, then save the current state (prior to
;		configuring for GRAFFER).
;
; Notes:
;	This routine is intended to operate as a "KILL_NOTIFY"
;	callback procedure -- I hope that this will prevent some of
;       the problems associated with the IDL being left in a stange
;       state when GRAFFER exits out of control. It cannot free heap
;       variables other than by a garbage collection, which may not be
;       at the right level.
;
; History:
;	Original: 23/1/97; SJT
;	Call garbage collector: 30/6/05; SJT
;	Add a save key, to allow gr_entry state to be hidden. ??; SJT
;	Change to use decomposed colours: May 16; SJT
;-

  common Gr_entry_state, pstate, xstate, ystate, zstate, qstate, $
     rstate, gstate, bstate, gdstate, dstate

  if keyword_set(save) then begin
     gdstate = !d.name
     pstate = !p
     xstate = !x
     ystate = !y
     zstate = !z                ; Not touched at present in graffer
                                ; but if & when GRAFFER for surfaces is
                                ; done then it may be
     qstate = !quiet
     tvlct, rstate, gstate, bstate, /get

     if ((!d.flags and 65536) eq 0) then begin
        if strupcase(!version.os_family) eq 'UNIX' then dev = 'X' $
        else dev = 'WIN'        ; Probably won't work
        print, "GRAFFER needs widgets; current device (", !D.name, $
               ") does not support them, switching to ", dev, "."
        set_plot, dev
     endif
     device, get_decomposed = dstate, get_visual_depth = vdep
     if (~dstate && vdep gt 8) then begin
        print, "GRAFFER now needs decomposed colours, setting"
        device, decomposed = 1
     endif
     !p.color = graff_colours(1)
     !p.background = graff_colours(0)

  endif else begin
                                ; Restore system variables and colour
                                ; tables 

     device, decomposed = dstate
     set_plot, gdstate
     !quiet = qstate            ; Restore message state
     !p = pstate
     !x = xstate
     !y = ystate
     !z = zstate
     tvlct, rstate, gstate, bstate
     if dstate then !p.color = graff_colours([255, 255, 255]) $
     else !p.color = 255b
  endelse

  heap_gc

end
