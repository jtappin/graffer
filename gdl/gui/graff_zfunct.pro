; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_ZFUNCT
;	Define a 2-D function to graffer.
;
; Usage:
;	ichange = graff_zfunct(pdefs)
;
; Return value:
;	ichange	int	1 if the DO button was used, 0 if cancel
;
; Argument:
;	pdefs	struct	in/out	The Graffer plot structure
;
; History:
;	Original (after GRAFF_FUNCT): 10/12/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to text inputs: 6/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

function Zfunct_event, event

widget_control, event.id, get_uvalue = but

iexit = 0

funct = ''
numpts = intarr(2)
range = dblarr(2, 2)
widget_control, event.handler, get_uvalue = uvs, /no_copy
case but of
    'ACTION': begin
        if (event.value eq 1) then begin
            widget_control, uvs.fid, get_value = funct
            for j = 0, 1 do begin
                widget_control, uvs.nid(j), get_value = np
                numpts(j) = np
                for k = 0, 1 do begin
                    widget_control, uvs.rgid(j, k), get_value = rng
                    range(j, k) = rng
                endfor
            endfor
        endif
        iexit = event.value
    end
    
    'XMIN': cw_enter_focus, uvs.rgid(1, 0)
    'XMAX': cw_enter_focus, uvs.rgid(0, 1)
    'YMIN': cw_enter_focus, uvs.rgid(1, 1)
    'YMAX': cw_enter_focus, uvs.nid(0)
    'NUM1': cw_enter_focus, uvs.nid(1)
    'NUM2': cw_enter_focus, uvs.fid
    'FUNC': cw_enter_focus, uvs.rgid(0, 0)
    
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

function Graff_zfunct, pdefs

  uvs = { Fid:    0l, $
          Nid:    lonarr(2), $
          rgid:   lonarr(2, 2) $
        }

;	Find if the dataset is already defined as a 2-D function

  if ((*pdefs.data)[pdefs.cset].type eq -4) then begin
     funct = xydata.funct
     range = xydata.range
     numpts = [(*pdefs.data)[pdefs.cset].ndata, $
               (*pdefs.data)[pdefs.cset].ndata2]  
     dflag = 0b
  endif else begin
     dflag = (*pdefs.data)[pdefs.cset].ndata gt 0
     if dflag then $
        if dialog_message(['CURRENT DATA SET IS NOT A 2-D', $
                           'FUNCTION, ENTERING A 2-D FUNCTION', $
                           'WILL OVERWRITE IT', $
                           'DO YOU REALLY WANT TO DO THIS?'], $ $
                          /question, dialog_parent = pdefs.ids.graffer, $
                          resource = 'Graffer') eq $
        'No' then return, 0

     funct = ''
     range = dblarr(2, 2)
     numpts = [25, 25]
  endelse

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer 2-D Function Plot', $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, /column)

                                ; The actual function definition

  uvs.fid = cw_enter(base, $
                     xsize = 40, $
                     value = funct, $
                     label = 'Function:', $
                     uvalue = 'FUNC', $
                     /capture)

                                ; X axis range

  rgb = widget_base(base, /row)
  uvs.rgid(0, 0) = cw_enter(rgb, $
                            /double, $
                            xsize = 10, $
                            uvalue = 'XMIN', $
                            value = range[0, 0], $
                            format = "(g10.3)", $
                            label = 'X axis range: Min:', $
                            /capture)
  uvs.rgid(1, 0) = cw_enter(rgb, $
                            /double, $
                            xsize = 10, $
                            uvalue = 'XMAX', $
                            value = range[1, 0], $
                            format = "(g10.3)", $
                            label = ' Max:', $
                            /capture)

                                ; Y axis range

  rgb = widget_base(base, /row)
  uvs.rgid(0, 1) = cw_enter(rgb, $
                            /double, $
                            xsize = 10, $
                            uvalue = 'YMIN', $
                            value = range[0, 1], $
                            format = "(g10.3)", $
                            label = 'Y axis range: Min:', $
                            /capture)
  uvs.rgid(1, 1) = cw_enter(rgb, $
                            /double, $
                            xsize = 10, $
                            uvalue = 'YMAX', $
                            value = range[1, 1], $
                            format = "(g10.3)", $
                            label = ' Max:', $
                            /capture)

                                ; Number of points

  njb = widget_base(base, /row)
  uvs.nid(0) = cw_enter(njb, $
                        /int, $
                        xsize = 5, $
                        uvalue = 'NUM1', $
                        value = numpts[0], $
                        format = "(I0)", $
                        label = 'Number of function evaluations X:', $
                        /capture)
  uvs.nid(1) = cw_enter(njb, $
                        /int, $
                        xsize = 5, $
                        uvalue = 'NUM2', $
                        value = numpts[1], $
                        format = "(I0)", $
                        label = 'Y:', /capture)

                                ; Control

  junk = cw_bgroup(base, $
                   ['Apply', 'Cancel'], $
                   /row, $
                   uvalue = 'ACTION', $
                   button_uvalue = [1, -1])

                                ; Realise and do RYO event handling

  widget_control, tlb, /real
  widget_control, base, set_uvalue = uvs, event_func = 'zfunct_event', $
                  /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, tlb, /destroy

  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exited eq -1) then return, 0

  xydata = {graff_zfunct}
  xydata.Range = ev.range
  xydata.Funct = ev.funct

  (*pdefs.data)[pdefs.cset].ndata = ev.numpts[0]
  (*pdefs.data)[pdefs.cset].ndata2 = ev.numpts[1]

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
  (*pdefs.data)[pdefs.cset].type = -4

  graff_set_vals, pdefs, /set_only
  return, 1

end
