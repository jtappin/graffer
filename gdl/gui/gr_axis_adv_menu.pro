; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_AXIS_ADV_MENU
;	Advanced axis options.
;
; Usage:
;	gr_axis_axis_adv_menu, pdefs, <selector key>
;
; Arguments:
;	pdefs	struct	in/out	The GRAFFER control & data structure.
;
; Keywords:
;	xaxis	If set, then this is an X-axis
;	yaxis	If 1 then a regular Y axis, if 2 an RHS Y axis.
;
; History:
;	Original: 21/8/12; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-
function gr_is_format, name

  ;; N.B.: This is not a rigorous test of a format. It is really to
  ;; check that you haven't left the brackets off. Any string
  ;; enclosed in parentheses or any actual found function name will
  ;; match. 
  if name eq '' then return, 1b

  if strpos(name, '(') eq 0 && strpos(name, ')', /reverse_search) eq $
     strlen(name)-1 then return, 1b

  locs = where(strupcase(name) eq routine_info(/system, /functions),  nsf)
  if nsf gt 0 then return, 1b
  locs = where(strupcase(name) eq routine_info(/functions),  nuf)
  if nuf gt 0 then return, 1b

  pd = strpos(name, '.', /reverse_search) 
  if pd ne -1 then $
     sname = strlowcase(name) $
  else sname = strlowcase(name)+'.pro'

  path = strsplit(!path, ':', count = npe, /extr)
  for j = 0l, npe-1 do if file_test(path[j]+'/'+sname) then return, 1b

  return, 0b
end
  
function gr_adv_axis_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.top, get_uvalue = state

  valid_form = gr_is_format(state.sty.format) 
  iexit = 0
  case but of
     'DONT': iexit = -1
     'DO': iexit = 1

     'MAJOR': state.sty.major = event.value
     'MINOR': state.sty.minor = event.value
     'FORMAT': begin
        valid_form = gr_is_format(event.value) 
        if valid_form then state.sty.format = event.value
     end

     'VALS': begin
        if (ptr_valid(state.sty.values)) then ptr_free, state.sty.values
        if n_elements(event.value) gt 1 then $
           state.sty.values = ptr_new(event.value)
     end
     'CLEAR': begin
        widget_control, state.locid, set_value = 0.
        if (ptr_valid(state.sty.values)) then ptr_free, state.sty.values
     end
  endcase

  widget_control, event.top, set_uvalue = state
  if (iexit eq 0) then widget_control, state.dobut, sensitive = $
                                       valid_form

  return, {id:event.id, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end

function gr_axis_adv_menu, pdefs, xaxis = xaxis, yaxis = yaxis

  if keyword_set(xaxis) then begin
     sty = pdefs.xsty
     axname = 'X'
  endif else if keyword_set(yaxis) then begin
     case yaxis of
        1: begin
           sty = pdefs.ysty
           axname = 'Y'
        end
        2: begin
           sty = pdefs.ysty_r
           axname = 'Yr'
        end
        else: begin
           graff_msg, pdefs.ids.hlptxt, "Invalid Y-axis choice"
           return, 0
        end
     endcase
  endif else begin
     graff_msg, pdefs.ids.hlptxt, "No axis selected"
     return, 0
  endelse

  widget_control, pdefs.ids.graffer, sensitive = 0
  
  base = widget_base(group = pdefs.ids.graffer, $
                     title = 'Advanced '+axname+' axis settings', $
                     /column)

  
  junk = widget_label(base, $
                      value = 'Advanced '+axname+' axis settings')

  ntickid = cw_enter(base, $
                     label = "Number of major ticks:", $
                     /int, $
                     value = sty.major, $
                     xsize = 5, $
                     uvalue = 'MAJOR', $
                     /all, $
                     /capture)

  junk = cw_enter(base, $
                  label = "Format specification:", $
                  /text, $
                  value = sty.format, $
                  xsize = 10, $
                  uvalue = 'FORMAT', $
                  /all, $
                  /capture)

  junk = cw_enter(base, $
                  label = "Number of minor ticks:", $
                  /int, $
                  value = sty.minor, $
                  xsize = 5, $
                  uvalue = 'MINOR', $
                  /all, $
                  /capture)

  jb = widget_base(base, $
                   /column, $
                   /frame)

  if ptr_valid(sty.values) then begin
     vlist = *sty.values
     ny = 10 > n_elements(vlist)
  endif else begin
     vlist = [0.d0]
     ny = 10
  endelse

  locid = cw_enter(jb, $
                   /column, $
                   label = "Tick locations", $
                   /double, $
                   value = vlist, $
                   xsize = 25, $
                   ysize = ny, $
                   /array, $
                   uvalue = 'VALS', $
                   /all, $
                   /capture, $
                   /scroll)

  junk = widget_button(jb, $
                       value = 'Clear', $
                       uvalue = 'CLEAR')

  jb = widget_base(base, $
                   /row)

  dobut = widget_button(jb, $
                        value = 'Apply', $
                        uvalue = 'DO', $
                        sensitive = 0)
  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')


  state = {sty: sty, $
           dobut: dobut, $
           ntickid: ntickid, $
           locid: locid}

  widget_control, base, /real, set_uvalue = state, event_func = $
                  'gr_adv_axis_event', /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = state, /no_copy
  widget_control, base, /destroy

  widget_control, pdefs.ids.graffer, /sensitive
  
  if (ev.exited eq 1) then begin
     case axname of
        'X': pdefs.xsty = state.sty
        'Y': pdefs.ysty = state.sty
        'Yr': pdefs.ysty_r = state.sty
     endcase
  endif

  return, ev.exited eq 1
end
