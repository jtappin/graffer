; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_POSITION
;	Set the position of the corners of the plot
;
; Usage:
;	ichange = gr_position(pdefs)
;
; Return value:
;	ichange	byte	Flag indicating if change has been made
;
; Argument:
;	pdefs	struct	in/out	The GRAFFER control structure
;
; History:
;	Original (made to be a pop-up): 12/12/96; SJT
;	Changed to be function returning change flag: 30/1/97; SJT
;	Add CAPTURE key to entry boxes: 6/2/97; SJT
;	Replace cw_bbselector with widget_droplist: 13/12/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;	Make coordinates double: 24/5/17; SJT
;-

function Gr_pos_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.handler, get_uvalue = uv, /no_copy

  iexit = 0

  iflag = 0b
  case but of
     'CANCEL': iexit = -1
     'DO': iexit = 1
     'DEF': iexit = 2
     
     'MODE': uv.state = event.index

     'PXMIN': begin
        uv.pos(0) = event.value
        if (event.cr) then cw_enter_focus, uv.pids(1)
     end                                     
     
     'PYMIN': begin                          
        uv.pos(1) = event.value             
        if (event.cr) then cw_enter_focus, uv.pids(2)
     end                                     
     
     'PXMAX': begin                          
        uv.pos(2) = event.value             
        if (event.cr) then cw_enter_focus, uv.pids(3)
     end
     
     'PYMAX': begin
        uv.pos(3) = event.value
        if (event.cr) then cw_enter_focus, uv.pids(0)
     end
     
     'ASPECT': begin                          
        uv.asp(0) = event.value             
        if (event.cr) then cw_enter_focus, uv.rids(1)
     end
     
     'MARGIN': begin                          
        uv.asp(1) = event.value             
        if (event.cr) then cw_enter_focus, uv.rids(0)
     end
     'ISO': begin
        uv.iso = event.select
        iflag = 1b
     end
     'HCA': begin
        uv.match = event.select
        iflag = 1b
     end
  endcase

  widget_control, uv.rbase, sensitive = uv.state ne 0 && ~uv.iso
  widget_control, uv.pbase, sensitive = uv.state eq 0 && ~uv.iso

  if (uv.state) then valid = uv.asp(0) gt 0 and (uv.asp(1) ge 0 and $
                                                 uv.asp(1) lt 0.5) $
  else valid = (max(uv.pos, min = mp) le 1.) and (mp ge 0.) and $
               (uv.pos(2) gt uv.pos(0)) and (uv.pos(3) gt uv.pos(1))
  widget_control, uv.dobut, sensitive = valid || iflag

  widget_control, event.handler, set_uvalue = uv, /no_copy

  return, {id:event.id, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end

function Gr_position, pdefs

  state = pdefs.aspect(0) gt 0
  iso = pdefs.isotropic

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer Plot Position', $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, $
                     /column)

  junk = widget_droplist(base, $
                         value = ['Corners', 'Aspect Ratio'], $
                         title = 'Determine position by:', $
                         uvalue = 'MODE')
  widget_control, junk, set_droplist_select = state

  pbase = widget_base(base, $
                      /column, $
                      /frame)

  junk = widget_label(pbase, $
                      value = 'Set corner position')

  pids = lonarr(4)
  jb = widget_base(pbase, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  pids[0] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.position[0], $
                     format = "(f6.4)", $
                     uvalue = 'PXMIN', $
                     label = 'Lower left, X:', $
                     xsize = 6, $
                     /capture)
  pids[1] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.position[1], $
                     format = "(f6.4)", $
                     uvalue = 'PYMIN', $
                     label = ' Y:', $
                     xsize = 6, $
                     /capture)

  jb = widget_base(pbase,  $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  pids[2] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.position[2], $
                     format = "(f6.4)", $
                     uvalue = 'PXMAX', $
                     label = 'Upper right, X:', $
                     xsize = 6, $
                     /capture)
  pids[3] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.position[3], $
                     format = "(f6.4)", $
                     uvalue = 'PYMAX', $
                     label = ' Y:', $
                     xsize = 6, $
                     /capture)

  rbase = widget_base(base, $
                      /column, $
                      /frame)

  junk = widget_label(rbase, $
                      value = 'Set aspect & margin')

  rids = lonarr(2)
  jb = widget_base(rbase, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)

  rids[0] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.aspect[0], $
                     format = "(f8.4)", $
                     uvalue = 'ASPECT', $
                     label = 'Aspect ratio:', $
                     xsize = 8, $
                     /capture)
  rids[1] = cw_enter(jb, $
                     /all_events, $
                     /double, $
                     value = pdefs.aspect[1], $
                     format = "(f6.4)", $
                     uvalue = 'MARGIN', $
                     label = 'Margin:', $
                     xsize = 6, $
                     /capture)


  jb = widget_base(base, $
                   /row, $
                   /nonexclusive)
  
  junk = widget_button(jb, $
                       value = "HC Aspect ratio", $
                       uvalue = 'HCA')
  widget_control, junk, set_button = pdefs.match
  if ~pdefs.y_right then begin
     junk = widget_button(jb, $
                          value = "Isotropic Axes?", $
                          uvalue = "ISO")
     widget_control, junk, set_button = iso
  endif

  jb = widget_base(base, /row, xpad = 0, ypad = 0, space = 0)
  dobut = widget_button(jb, $
                        value = '    Apply    ', $
                        uvalue = 'DO')
  junk = widget_button(jb, $
                       value = '  Default  ', $
                       uvalue = 'DEF')
  junk = widget_button(jb, $
                       value = '   Cancel   ', $
                       uvalue = 'CANCEL')

  widget_control, rbase, sensitive = state ne 0 && ~iso 
  widget_control, pbase, sensitive = state eq 0 && ~iso
  widget_control, dobut, sensitive = 0

  uv = { $
       Pos:pdefs.position,  $
       Asp:pdefs.aspect, $
       iso:pdefs.isotropic, $
       match:pdefs.match, $
       State:state, $
       Pbase:pbase,  $
       Rbase:rbase,  $
       Dobut:dobut,  $
       Pids:pids,  $
       Rids:rids $
       }

  widget_control, tlb, /real
  widget_control, base, set_uvalue = uv, event_func = 'gr_pos_event', $
                  /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, tlb, /destroy

  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exited eq 1) then begin
     pdefs.aspect = uv.asp
     if (uv.state eq 0) then pdefs.aspect(0) = 0.
     pdefs.position = uv.pos
     pdefs.isotropic = uv.iso
     pdefs.match = uv.match
  endif else if (ev.exited eq 2) then begin
     pdefs.aspect = 0.
     pdefs.position = 0.
     pdefs.isotropic = 0
  endif

  return, ev.exited gt 0

end
