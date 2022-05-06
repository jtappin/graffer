; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_FUNCT
;	Define a function to graffer.
;
; Usage:
;	ichange=graff_funct(pdefs)
;
; Return value:
;	ichange	int	1 if the DO button was used, 0 if cancel
;
; Argument:
;	pdefs	struct	in/out	The Graffer plot structure
;
; History:
;	Original: 6/8/95; SJT
;	Add timer event to push to front if obscured: 23/8/95; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to text inputs: 6/2/97; SJT
;	Change handles into pointers: 28/6/05; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

function Funct_event, event

  widget_control, event.id, get_uvalue = but

  iexit = 0

  funct = ''
  numpts = 0
  range = dblarr(2)

  widget_control, event.handler, get_uvalue = uvs, /no_copy

  case but of
     'ACTION': begin
        if (event.value eq 1) then begin
           widget_control, uvs.fid, get_value = funct
           widget_control, uvs.nid, get_value = numpts
           widget_control, uvs.minid, get_value = rng
           range(0) = rng
           widget_control, uvs.maxid, get_value = rng
           range(1) = rng
        endif
        iexit = event.value
     end
     
     'MIN': cw_enter_focus, uvs.minid
     'MAX': cw_enter_focus, uvs.nid
     'NUM': cw_enter_focus, uvs.fid
     'FUN': cw_enter_focus, uvs.maxid
     
     
  endcase
  widget_control, event.handler, set_uvalue = uvs, /no_copy

  return, {id:event.id, $
           top:event.top, $
           handler:event.handler, $
           Exited:iexit, $
           funct:funct, $
           numpts:numpts, $
           range:range}

end

function Graff_funct, pdefs, y_funct = y_funct

  uvs = { $
        Fid:    0l, $
        Nid:    0l, $
        Minid:  0l, $
        Maxid:  0l $
        }

;	Find if the dataset is already defined as a function

  if ((*pdefs.data)[pdefs.cset].type eq -1 or $
      (*pdefs.data)[pdefs.cset].type eq -2) then begin 
     funct = (*(*pdefs.data)[pdefs.cset].xydata).funct
     range = (*(*pdefs.data)[pdefs.cset].xydata).range
     numpts = (*pdefs.data)[pdefs.cset].ndata
     dflag = 0b
  endif else begin
     dflag = (*pdefs.data)[pdefs.cset].ndata gt 0
     if (dflag) then $
        if dialog_message(['CURRENT DATA SET IS NOT A SIMPLE', $
                           'FUNCTION, ENTERING A SIMPLE FUNCTION', $
                           'WILL OVERWRITE IT', $
                           'DO YOU REALLY WANT TO DO THIS?'], $ $
                          /question, dialog_parent = $
                          pdefs.ids.graffer, $
                          resource = 'Graffer') eq 'No' then return, 0
     
     funct = ''
     range = dblarr(2)
     numpts = 25
  endelse

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer Function Plot', $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, /column)

                                ; The actual function definition


  uvs.fid = cw_enter(base, $
                     xsize = 40, $
                     value = funct, $
                     label = 'Function:', $
                     /capture, $
                     uvalue = 'FUN')

                                ; X axis range
  if (keyword_set(y_funct)) then ax = 'Y' $
  else ax = 'X'
  rgb = widget_base(base, /row)
  uvs.minid = cw_enter(rgb, $
                       /double, $
                       xsize = 12, $
                       uvalue = 'MIN', $
                       value = range[0], $
                       format = "(g12.5)", $
                       label = ax+' axis range: Min:', $
                       /capture)
  uvs.maxid = cw_enter(rgb, $
                       /double, $
                       xsize = 12, $
                       uvalue = 'MAX', $
                       value = range[1], $
                       format = "(g12.5)",  $
                       label = ' Max:', $
                       /capture)

                                ; Number of points

  uvs.nid = cw_enter(base, $
                     /int, $
                     xsize = 5, $
                     uvalue = 'NUM', $
                     value = numpts, $
                     format = "(I0)",  $
                     label = 'Number of function evaluations:', $
                     /capture)

                                ; Control

  junk = cw_bgroup(base, $
                   ['Apply', 'Cancel'], $
                   /row, $
                   uvalue = 'ACTION', $
                   button_uvalue = [1, -1])

                                ; Realise and do RYO event handling

  widget_control, tlb, /real
  widget_control, base, set_uvalue = uvs, event_func = 'funct_event', $
                  /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, tlb, /destroy

  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exited eq -1) then return, 0

  xydata = {graff_funct}
  xydata.Range = ev.range
  xydata.Funct = ev.funct

  (*pdefs.data)[pdefs.cset].ndata = ev.numpts 

  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     if (*pdefs.data)[pdefs.cset].type eq 9 then $
        ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).z $
     else if (*pdefs.data)[pdefs.cset].type ge 0 then $
        ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y_err
     
     ptr_free, (*pdefs.data)[pdefs.cset].xydata
  endif
  
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  if (keyword_set(y_funct)) then (*pdefs.data)[pdefs.cset].type = -2 $
  else (*pdefs.data)[pdefs.cset].type = -1

  return, 1

end
