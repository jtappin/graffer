; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_RESCALE
;	Rescale the current GRAFFER dataset
;
; Usage:
;	ichange = graff_rescale(pdefs)
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
; Argument:
;	prefs	struct	input	The GRAFFER data & control structure.
;
; History:
;	Original: 16/8/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to text inputs: 6/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add division option: 12/9/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;	Make factors double: 24/5/17; SJT
;	Allow scaling of data values in 2-D datasets: 2/12/19; SJT
;-

function Rescale_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.handler, get_uvalue = wids

  iexit = 0
  value = 0.

  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  if (track_flag) then begin
     idraw = 0
     if (event.enter eq 0) then begin
        graff_msg, wids.msg, ''
        goto, miss_case
     endif
  endif

  case (but) of
     'CANCEL': if (track_flag) then $
        graff_msg, wids.msg, "Abandon operation and return" $
     else iexit = -1
     
     'DO': if (track_flag) then $
        graff_msg, wids.msg, "Apply scalings and shifts and return" $
     else  begin
        if widget_info(wids.boxes[4], /valid) then naxes = 3 $
        else naxes = 2
        value = dblarr(2*naxes)
        dflag = bytarr(naxes)
        for j = 0, 2*naxes-1 do begin
           widget_control, wids.boxes[j], get_value = xy
           value[j] = xy
        endfor
        for j = 0, naxes-1 do dflag[j] = widget_info(wids.toggles[j], $
                                               /button_set)
        if dflag[0] then value[0] = 1.d/value[0]
        if dflag[1] then value[2] = 1.d/value[2]
        if naxes eq 3 && dflag[2] then value[4] = 1.d/value[4]

        iexit = 1
     endelse
     
     'XSCALE': if (track_flag) then $
        graff_msg, wids.msg, "Specify scaling factor for X values" $
     else cw_enter_focus, wids.boxes[1]

     'XSHIFT': if (track_flag) then $
        graff_msg, wids.msg, $
                   "Specify shift for X values (post scaling units)" $
     else cw_enter_focus, wids.boxes[2]

     'YSCALE': if (track_flag) then $
        graff_msg, wids.msg, "Specify scaling factor for Y values" $
     else cw_enter_focus, wids.boxes[3]

     'YSHIFT': if (track_flag) then $
        graff_msg, wids.msg, $
                   "Specify shift for Y values (post scaling units)" $
     else cw_enter_focus, wids.boxes[0]
     
     'ZSCALE': if (track_flag) then $
        graff_msg, wids.msg, "Specify scaling factor for Z values" $
     else cw_enter_focus, wids.boxes[4]

     'ZSHIFT': if (track_flag) then $
        graff_msg, wids.msg, $
                   "Specify shift for Z values (post scaling units)" $
     else cw_enter_focus, wids.boxes[5]

     'DIVX': if (track_flag) then $
        graff_msg, wids.msg, 'Set to divide by the X scale factor'

     'DIVY': if (track_flag) then $
        graff_msg, wids.msg, 'Set to divide by the Y scale factor'

     'DIVZ': if (track_flag) then $
        graff_msg, wids.msg, 'Set to divide by the Z scale factor'

  endcase

Miss_case:

  return, { $
          Id:      event.id, $
          Top:     event.top,  $
          Handler: 0l,  $
          Value:   value, $
          Exited:  iexit $
          }

end

function Graff_rescale, pdefs

  common graffer_options, optblock

; Extract the datasets

  if ((*pdefs.data)[pdefs.cset].type lt 0) then begin ; This a function -- can't
                                ; rescale a function!
     graff_msg, pdefs.ids.message,  $
                "This a function -- can't rescale a function!"
     return, 0
  end
  if ((*pdefs.data)[pdefs.cset].ndata eq 0) then begin ; This a new data
                                ; set -- can't 
                                ; rescale no data!
     graff_msg, pdefs.ids.message,  $
                "This an empty dataset -- can't rescale an empty dataset!"
     return, 0
  end

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'GRAFFER rescale', group = $
                    pdefs.ids.graffer, resource = 'Graffer')

  base = widget_base(tlb, /column)

  popper = widget_label(base, value = 'GRAFFER Dataset rescaler')

  wids = {boxes: lonarr(6), $
          toggles: lonarr(3), $
          msg: 0l}

  if (*pdefs.data)[pdefs.cset].type eq 9 then begin ; Contour/image
     jb = widget_base(base, /row, /frame)
     
     wids.boxes[4] = cw_enter(jb, $
                              label = 'Z: Scaling:', $
                              value = 1.0, $
                              /double, $
                              xsize = 11, $
                              uvalue = 'ZSCALE', $
                              track = optblock.track, $
                              /capture)
     
     jbb = widget_base(jb, $
                       /nonexclusive)
     wids.toggles[2] = widget_button(jbb, $
                                     value = 'Divide', $
                                     uvalue = 'DIVZ')

     wids.boxes[5] = cw_enter(jb, $
                              label = 'Shift:', $
                              value = 0., $
                              /double, $
                              xsize = 11, $
                              uvalue = 'ZSHIFT', $
                              track = optblock.track, $
                              /capture)
  endif else begin
     wids.boxes[4:5] = 0l
     wids.toggles[2] = 0l
  endelse
  
  jb = widget_base(base, /row, /frame)

  wids.boxes[0] = cw_enter(jb, $
                           label = 'X: Scaling:', $
                           value = 1.0, $
                           /double, $
                           xsize = 11, $
                           uvalue = 'XSCALE', $
                           track = optblock.track, $
                           /capture)

  jbb = widget_base(jb, $
                    /nonexclusive)
  wids.toggles[0] = widget_button(jbb, $
                                  value = 'Divide', $
                                  uvalue = 'DIVX')

  wids.boxes[1] = cw_enter(jb, $
                           label = 'Shift:', $
                           value = 0., $
                           /double, $
                           xsize = 11, $
                           uvalue = 'XSHIFT', $
                           track = optblock.track, $
                           /capture)

  jb = widget_base(base, /row, /frame)

  wids.boxes[2] = cw_enter(jb, $
                           label = 'Y: Scaling:', $
                           value = 1.0, $
                           /double, $
                           xsize = 11, $
                           uvalue = 'YSCALE', $
                           track = optblock.track, $
                           /capture)

  jbb = widget_base(jb, $
                    /nonexclusive)
  wids.toggles[1] = widget_button(jbb, $
                                  value = 'Divide', $
                                  uvalue = 'DIVY')

  wids.boxes[3] = cw_enter(jb, $
                           label = 'Shift:', $
                           value = 0., $
                           /double, $
                           xsize = 11, $
                           uvalue = 'YSHIFT', $
                           track = optblock.track, $
                           /capture)

  wids.msg = widget_text(base, value = '')

  jb = widget_base(base, /row)

  junk = widget_button(jb, $
                       value = '     Apply     ', $
                       uvalue = 'DO', $
                       track = optblock.track)

 junk = widget_button(jb, $
                       value = '     Cancel    ', $
                       uvalue = 'CANCEL', $
                       track = optblock.track)
 
;	Realise the widgets and use a DIY widget handling procedure
;	(as with all the graffer popups) to facilitate getting values
;	back into the right place.

  widget_control, tlb, /real
  widget_control, base, set_uvalue = wids, /no_copy, event_func = $
                  'rescale_event'

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, /sensitive 


  if (ev.exited lt 0) then return, 0

  xydata = *(*pdefs.data)[pdefs.cset].xydata

  if ((*pdefs.data)[pdefs.cset].type eq 9) then begin
     
     *xydata.x = *xydata.x*ev.value[0] + ev.value[1]
     *xydata.y = *xydata.y*ev.value[2] + ev.value[3]
     *xydata.z = *xydata.z*ev.value[4] + ev.value[5]
     
  endif else begin
     *xydata.x = *xydata.x*ev.value[0] + ev.value[1]
     *xydata.y = *xydata.y*ev.value[2] + ev.value[3]

     if ptr_valid(xydata.x_err) then $
        *xydata.x_err = *xydata.x_err*ev.value[0]
     if ptr_valid(xydata.y_err) then $
        *xydata.y_err = *xydata.y_err*ev.value[2]
     
  endelse
  
  *(*pdefs.data)[pdefs.cset].xydata = xydata

  return, 1

end
