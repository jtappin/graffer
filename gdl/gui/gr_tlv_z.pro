; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_TLV_Z
;	Construct a graffer data set from top-level variables
;
; Usage:
;	ichange = gr_tlv_z(pdefs)
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
; Argument:
;	pdefs	struct	in/out	The graffer control/data structure
;
; History:
;	Original: 21/9/95; SJT
;	Add x=findgen(ny) when no X variable given: 28/10/96; SJT
;	MOve GRF_TLV_GET to a separate file: 6/12/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to entry boxes: 6/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add a chooser: 6/2/12; SJT
;	Improve handling of "undefined" settings: 5/10/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-


function Grf_tlz_event, event

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uvs, /no_copy

  widget_control, event.id, get_uvalue = object

  iexit = 0
  case object of
     'ACTION': if (event.value eq -1) then begin
        iexit = -1 
     endif else begin
        iexit = 1
        
        widget_control, uvs.zid, get_value = zvar
        zvar = strtrim(zvar, 2)
        if (zvar eq '.') then begin
           if ptr_valid(uvs.z) then begin
              z = *uvs.z
              nz = n_elements(z)
           endif else nz = 0
        endif else z = grf_tlv_get(zvar, nz)
        if (nz eq 0) then begin
           graff_msg, uvs.mid, 'Z: '+zvar+ $
                      ' Undefined or non-numeric'
           iexit = 0
           goto, donefor
        endif
        sz = size(z)
        if (sz(0) ne 2) then begin
           graff_msg, uvs.mid, 'Z: '+zvar+' not 2-dimensional'
           iexit = 0
           goto, donefor
        endif else z = double(z)
        
        widget_control, uvs.xid, get_value = xvar
        xvar = strtrim(xvar, 2)
        if (xvar eq '') then begin
           x = dindgen(sz(1))
           nx = sz(1)
        endif else begin
           if (xvar eq '.') then begin
              if ptr_valid(uvs.x) then begin
                 x = *uvs.x
                 nx = n_elements(x)
              endif else nx = 0
           endif else x = grf_tlv_get(xvar, nx)
           if (nx eq 0) then begin
              graff_msg, uvs.mid, 'X: '+xvar+' Undefined or ' + $
                         'non-numeric'
              iexit = 0
              goto, donefor
           endif else if (nx eq 1 and sz[1] gt 1) then $
              x = dindgen(sz[1])*x[0] $
           else x = double(x)
        endelse
        sx = size(x)
        if (sx[0] eq 1 and sx[1] eq sz[1]) then iexit =  1 $ $
        else if (sx[0] eq 2 and sx[1] eq sz[1] and sx[2] eq sz[2]) then $
           iexit = 1 $
        else begin
           graff_msg, uvs.mid, 'X: '+xvar+' Must be 1-D and match X ' + $
                      'dimension of Z, or have the same size as Z'
           iexit = 0
           goto, donefor
        endelse
        
        widget_control, uvs.yid, get_value = yvar
        yvar = strtrim(yvar, 2)
        if (yvar eq '') then begin
           y = dindgen(sz(2))
           ny = sz(2)
        endif else begin
           if (yvar eq '.') then begin
              if ptr_valid(uvs.y) then begin
                 y = *uvs.y
                 ny = n_elements(y)
              endif else ny = 0
           endif else y = grf_tlv_get(yvar, ny)
           if (ny eq 0) then begin
              graff_msg, uvs.mid, 'Y: '+yvar+' Undefined or ' + $
                         'non-numeric'
              iexit = 0
              goto, donefor
           endif else if (ny eq 1 and sz[2] gt 1) then $
              y = dindgen*sz[2]*y[0] $
           else y = double(y)
        endelse
        sy = size(y)
        if (sy[0] eq 1 and sy[1] eq sz[2]) then iexit = 1 $
        else if (sy[0] eq 2 and sy[1] eq sz[1] and sy[2] eq sz[2]) $
        then iexit = 1 $
        else begin
           graff_msg, uvs.mid, 'Y: '+yvar+' Must be 1-D and match Y ' + $
                      'dimension of Z, or have the same size as Z'
           iexit = 0
           goto, donefor
        endelse
        
     endelse
     
     'Z': cw_enter_focus, uvs.xid
     'X': cw_enter_focus, uvs.yid
     'Y': cw_enter_focus, uvs.zid
     'ZP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin
           if level ne 1 then $
              widget_control, uvs.zid, set_value = $
                              string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.zid, set_value = name
        endif
        cw_enter_focus, uvs.zid
     end
     'XP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin
           if level ne 1 then $
              widget_control, uvs.xid, set_value = $
                              string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.xid, set_value = name
        endif
        cw_enter_focus, uvs.xid
     end
     'YP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin
           if level ne 1 then $
              widget_control, uvs.yid, set_value = $
                              string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.yid, set_value = name
        endif
        cw_enter_focus, uvs.yid
     end
  endcase

Donefor:

  if (iexit eq 1) then begin
     uvs.x = ptr_new(x)
     uvs.y = ptr_new(y)
     uvs.z = ptr_new(z)
  endif

  widget_control, base, set_uvalue = uvs, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end

function Gr_tlv_z, pdefs

  fflag = ((*pdefs.data)[pdefs.cset].type ne 9) and $
          (*pdefs.data)[pdefs.cset].ndata gt 0
  if (fflag) then begin
     if dialog_message(['CURRENT DATA SET IS 1-D OR A FUNCTION,', $
                        'ENTERING DATA WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0 $
     else xydata = {graff_zdata}
  endif else begin       

     if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
        xydata = *(*pdefs.data)[pdefs.cset].xydata
        if size(xydata, /type) ne 8 || $
           tag_names(xydata, /str) ne 'GRAFF_ZDATA' then $
              xydata = {graff_zdata}
     endif else xydata = {graff_zdata}
  endelse

  uvs = { $
        Xid:   0l, $
        Yid:   0l, $
        Zid:   0l, $
        X:     xydata.x, $
        Y:     xydata.y, $
        Z:     xydata.z, $
        mid:   0l $
        }

;	Check out the type of the current ds


; 	desensitize the main graffer panel and define the bases for
; 	this one.

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer 2-D data from Variables',  $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, $
                     /column)

                                ; The entry boxes for Z, X & Y

  jb = widget_base(base, $
                   /row)
  uvs.zid = cw_enter(jb, $
                     value = '', $
                     /text, $
                     uvalue = 'Z', $
                     label = 'Z Variable:', $
                     xsize = 12, $
                     /capture)
  junk = widget_button(jb, $
                       value = 'Pick...', $
                       uvalue = 'ZP')

  jb = widget_base(base, $
                   /row)
  uvs.xid = cw_enter(jb, $
                     value = '', $
                     /text, $
                     uvalue = 'X', $
                     label = 'X Variable:', $
                     xsize = 12, $
                     /capture)
  junk = widget_button(jb, $
                       value = 'Pick...', $
                       uvalue = 'XP')

  jb = widget_base(base, $
                   /row)
  uvs.yid = cw_enter(jb, $
                     value = '', $
                     /text, $
                     uvalue = 'Y', $
                     label = 'Y Variable:', $
                     xsize = 12, $
                     /capture)
  junk = widget_button(jb, $
                       value = 'Pick...', $
                       uvalue = 'YP')


  uvs.mid = cw_enter(base, $
                     value = '', $
                     ysize = 2, $
                     xsize = 30, $
                     /column, $
                     /display, $
                     label = 'Messages')

  junk = cw_bgroup(base, $
                   ['Apply', 'Cancel'], $
                   button_uvalue = [1, -1], $
                   uvalue = 'ACTION', $
                   /row)



                                ; Realise and do RYO event handling

  widget_control, tlb, /real

  cw_enter_focus, uvs.zid

  widget_control, base, event_func = 'grf_tlz_event', set_uvalue = $
                  uvs, /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = uvs, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, /sensitive 

  if (ev.exited eq -1) then return, 0


  xydata = {graff_zdata}

  xydata.x = ptr_new(*uvs.x)
  xydata.x_is_2d = size(*uvs.x, /n_dim) eq 2
  xydata.y = ptr_new(*uvs.y)
  xydata.y_is_2d = size(*uvs.y, /n_dim) eq 2
  xydata.z = ptr_new(*uvs.z)
  ptr_free, uvs.x
  ptr_free, uvs.y
  ptr_free, uvs.z

  sz = size(*xydata.z)
  (*pdefs.data)[pdefs.cset].ndata = sz[1]
  (*pdefs.data)[pdefs.cset].ndata2 = sz[2]

  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     if (*pdefs.data)[pdefs.cset].type eq 9 then ptr_free, $
        (*(*pdefs.data)[pdefs.cset].xydata).x, $
        (*(*pdefs.data)[pdefs.cset].xydata).y, $
        (*(*pdefs.data)[pdefs.cset].xydata).z $
     else if (*pdefs.data)[pdefs.cset].type ge 0 then ptr_free, $
        (*(*pdefs.data)[pdefs.cset].xydata).x, $
        (*(*pdefs.data)[pdefs.cset].xydata).y, $
        (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
        (*(*pdefs.data)[pdefs.cset].xydata).y_err


     ptr_free, (*pdefs.data)[pdefs.cset].xydata
  endif
  
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].type = 9


  if (*pdefs.data)[pdefs.cset].zopts.n_levels  eq 0 then $
     (*pdefs.data)[pdefs.cset].zopts.n_levels = 6
  if ~ptr_valid((*pdefs.data)[pdefs.cset].zopts.colours) then begin
     (*pdefs.data)[pdefs.cset].zopts.n_cols = 1
     (*pdefs.data)[pdefs.cset].zopts.colours = ptr_new(1)
     (*pdefs.data)[pdefs.cset].zopts.raw_colours = ptr_new(intarr(3))
  endif
  if ~ptr_valid((*pdefs.data)[pdefs.cset].zopts.style) then begin
     (*pdefs.data)[pdefs.cset].zopts.n_sty =   1
     (*pdefs.data)[pdefs.cset].zopts.style = ptr_new(0)
  endif
  if ~ptr_valid((*pdefs.data)[pdefs.cset].zopts.thick) then begin
     (*pdefs.data)[pdefs.cset].zopts.n_thick = 1
     (*pdefs.data)[pdefs.cset].zopts.thick = ptr_new(1.)
  endif
  if (*pdefs.data)[pdefs.cset].zopts.pxsize eq 0. then $
     (*pdefs.data)[pdefs.cset].zopts.pxsize = 0.5

  graff_set_vals, pdefs, /set_only

  return, 1

end
