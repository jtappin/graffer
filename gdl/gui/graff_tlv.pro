; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_TLV
;	Construct a graffer data set from top-level variables
;
; Usage:
;	ichange = graff_tlv(pdefs)
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
;	Move GRF_TLV_GET to a separate file: 6/12/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to entry boxes: 6/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add missing check for 2-D dataset: 21/1/09; SJT
;	Replace cw_bbselectors with widget_droplist: 14/12/11; SJT
;	Add a chooser: 6/2/12; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-


function Grf_tlv_event, event

  common Gr_tlvs_masks, exlm, exhm, eylm, eyhm

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uvs, /no_copy

  widget_control, event.id, get_uvalue = object

  iexit = 0
  case object of
     'ACTION': if (event.value eq -1) then begin
        iexit = -1 
     endif else begin
        iexit = 1
        
        y_missing = 0b
        widget_control, uvs.yid, get_value = yvar
        yvar = strtrim(yvar, 2)
        if (yvar eq '.') then begin
           if ptr_valid(uvs.y) then begin
              y = *uvs.y
              ny = n_elements(y)
           endif else ny = 0
        endif else if yvar eq '' then y_missing = 1b $ $
        else begin
           y = grf_tlv_get(yvar, ny)
           if ny eq 0 then begin
              widget_control, uvs.mid, set_value = 'Y: '+yvar+ $
                              ' Undefined or non-numeric'
              iexit = 0
              goto, donefor
           endif
        endelse
        
        widget_control, uvs.xid, get_value = xvar
        xvar = strtrim(xvar, 2)
        if (xvar eq '') then begin
           x = dindgen(ny)
           nx = ny
        endif else begin
           if (xvar eq '.') then begin
              if ptr_valid(uvs.x) then begin
                 x = *uvs.x
                 nx = n_elements(x)
              endif else nx = 0
           endif else x = grf_tlv_get(xvar, nx)
           if (nx eq 0) then begin
              widget_control, uvs.mid, set_value = 'X: '+xvar+ $
                              ' Undefined or non-numeric'
              iexit = 0
              goto, donefor
           endif else if ~y_missing then x = double(x) $
           else begin
              sx = size(x)
              if sx[0] eq 2 then begin
                 if sx[1] eq 2 then begin
                    y = double(reform(x[1, *]))
                    x = double(reform(x[0, *]))
                    nx = n_elements(x)
                    ny = n_elements(y)
                 endif else if sx[2] eq 2 then begin
                    y = double(x[*, 1])
                    x = double(x[*, 0])
                    nx = n_elements(x)
                    ny = n_elements(y)
                 endif else begin
                    widget_control, uvs.mid, set_value = $
                                    'If Y is not set, X must be a ' + $
                                    '2xN or Nx2 array'
                    iexit = 0
                    goto, donefor
                 endelse
              endif else if sx[sx[0]+1] eq 6 || $
                 sx[sx[0]+1] eq 9 then begin
                 y = imaginary(x[*])
                 x = real_part(x[*])
                 nx = n_elements(x)
                 ny = nx
               endif else begin
                 widget_control, uvs.mid, set_value = $
                                 'If Y is not set, X must be a ' + $
                                 '2xN or Nx2 array'
                 iexit = 0
                 goto, donefor
              endelse
           endelse
        endelse

        if (ny eq 0) then begin
           widget_control, uvs.mid, set_value = 'Y: '+yvar+ $
                           ' Undefined or non-numeric'
           iexit = 0
           goto, donefor
        endif else y = double(y)
        
        
        if (nx ne ny) then begin
           if nx eq 1 then x = dindgen(ny)*x[0] $
           else begin
              widget_control, uvs.mid, set_value = 'Length of X and ' + $
                              'Y must be equal'
              iexit = 0
              goto, donefor
           endelse
        endif
        
        if (uvs.type ne 0) then begin
           nce = ([0, 1, 2, 1, 2, 2, 3, 3, 4])[uvs.type]
           errs = dblarr(nce, nx)
           irr = 0 
           isdot = ptr_valid(uvs.err)
           if (isdot) then nerd = (size(*uvs.err))[0] $
           else nerd = 0
           if (exlm[uvs.type]) then begin
              widget_control, uvs.eloxid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs a lower X error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if irr lt nerd then begin
                    errtmp = *uvs.err[irr, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 0
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else errs(irr, *) = errtmp
              irr = irr+1
           endif
           if (exhm[uvs.type]) then begin
              widget_control, uvs.ehixid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs a upper X error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if irr lt nerd then begin
                    errtmp = *uvs.err[irr, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 0
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else errs(irr, *) = errtmp
              irr = irr+1
           endif
           
           if (eylm[uvs.type]) then begin
              widget_control, uvs.eloyid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs a lower Y error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if irr lt nerd then begin
                    errtmp = *uvs.err[irr, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 0
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else errs(irr, *) = errtmp
              irr = irr+1
           endif
           if (eyhm[uvs.type]) then begin
              widget_control, uvs.ehiyid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs a upper Y error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if irr lt nerd then begin
                    errtmp = *uvs.err[irr, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 0
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else errs(irr, *) = errtmp
              irr = irr+1
           endif
        endif 
     endelse
     
     'X': cw_enter_focus, uvs.yid
     'Y': cw_enter_focus, uvs.eloxid
     'ELOX': cw_enter_focus, uvs.ehixid
     'EHIX': cw_enter_focus, uvs.eloyid
     'ELOY': cw_enter_focus, uvs.ehiyid
     'EHIY': cw_enter_focus, uvs.xid
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
     'ELOXP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin $
           if level ne 1 then widget_control, uvs.eloxid, set_value = $
           string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.eloxid, set_value = name
        endif
        cw_enter_focus, uvs.eloxid
     end
     'EHIXP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin $
           if level ne 1 then widget_control, uvs.ehixid, set_value = $
           string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.ehixid, set_value = name
        endif
        cw_enter_focus, uvs.ehixid
     end
     'ELOYP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin $
           if level ne 1 then widget_control, uvs.eloyid, set_value = $
           string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.eloyid, set_value = name
        endif
        cw_enter_focus, uvs.eloyid
     end
     'EHIYP': begin
        name = gr_pick_tlv(event.top, level)
        if name ne '' then begin $
           if level ne 1 then widget_control, uvs.ehiyid, set_value = $
           string(level, format = "(I0,'\')")+name $
           else widget_control, uvs.ehiyid, set_value = name
        endif
        cw_enter_focus, uvs.ehiyid
     end


     'ERRS': begin
        uvs.type = event.index
        widget_control, uvs.eloxbid, sensitive = exlm[uvs.type]
        widget_control, uvs.ehixbid, sensitive = exhm[uvs.type]
        widget_control, uvs.eloybid, sensitive = eylm[uvs.type]
        widget_control, uvs.ehiybid, sensitive = eyhm[uvs.type]
     end
  endcase

Donefor:

  if (iexit eq 1) then begin
     uvs.x = ptr_new(x)
     uvs.y = ptr_new(y)
     if (uvs.type ge 1) then begin
        uvs.err = ptr_new(errs)
     endif
  endif

  widget_control, base, set_uvalue = uvs, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end





function Graff_tlv, pdefs

  common Gr_tlvs_masks, exlm, exhm, eylm, eyhm

  fflag = ((*pdefs.data)[pdefs.cset].type lt 0)
  flag2 = ((*pdefs.data)[pdefs.cset].type ge 9)
  if (fflag) then begin
     if dialog_message(['CURRENT DATA SET IS A FUNCTION', $
                        'OR A 2-D DATASET ENTERING DATA', $
                        'WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0 
     if  ptr_valid((*pdefs.data)[pdefs.cset].xydata) then $
        ptr_free, (*pdefs.data)[pdefs.cset].xydata
  endif
  if flag2 then begin
     if dialog_message(['CURRENT DATA SET IS A 2-D DATASET', $
                        'ENTERING 1-D DATA WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0
     (*pdefs.data)[pdefs.cset].type = 0 ; Force to a simple DS
     if  ptr_valid((*pdefs.data)[pdefs.cset].xydata) then $
        ptr_free, (*pdefs.data)[pdefs.cset].xydata
  endif

  uvs = { $
        Xid:    0l, $
        Yid:    0l, $
        Eloxid: 0l, $
        Ehixid: 0l, $
        Eloyid: 0l, $
        Ehiyid: 0l, $
        Xbid:   0l, $
        Ybid:   0l, $
        Eloxbid:0l, $
        Ehixbid:0l, $
        Eloybid:0l, $
        Ehiybid:0l, $
        Errid:  0l, $
        Errids: 0l, $
        Mid:    0l, $
        X:      ptr_new(), $
        Y:      ptr_new(), $
        Err:    ptr_new(), $
        Type:   (*pdefs.data)[pdefs.cset].type $
        }

  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     xydata = *(*pdefs.data)[pdefs.cset].xydata
     if n_elements(xydata) ne 0 then begin
        if (*pdefs.data)[pdefs.cset].ndata eq 1 then begin
           xtmp = xydata[0, 0]
           ytmp = xydata[1, 0]
           uvs.x = ptr_new(xtmp)
           uvs.y = ptr_new(ytmp)
           if (*pdefs.data)[pdefs.cset].type gt 0 then begin
              errtmp = xydata[2:*, 0]
              uvs.err = ptr_new(errtmp)
           endif
        endif else begin
           xtmp = xydata[0, *]
           ytmp = xydata[1, *]
           uvs.x = ptr_new(xtmp)
           uvs.y = ptr_new(ytmp)
           if (*pdefs.data)[pdefs.cset].type gt 0 then begin
              errtmp = xydata[2:*, *]
              uvs.err = ptr_new(errtmp)
           endif
        endelse
     endif
  endif

;	Check out the type of the current ds

  
  exlm = [0, 0, 0, 1, 1, 1, 1, 1, 1]
  exhm = [0, 0, 0, 0, 1, 0, 0, 1, 1]
  eylm = [0, 1, 1, 0, 0, 1, 1, 1, 1]
  eyhm = [0, 0, 1, 0, 0, 0, 1, 0, 1]

; 	desensitize the main graffer panel and define the bases for
; 	this one.

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer from Variables',  $
                    group_leader = pdefs.ids.graffer, resource = $
                    'Graffer')
  base = widget_base(tlb, /column)

                                ; The entry boxes for X & Y

  uvs.xbid = widget_base(base, $
                         /row)
  uvs.xid = cw_enter(uvs.xbid, $
                        value = '', $
                        /text, $
                        uvalue = 'X', $
                        label = 'X Variable:', $
                        xsize = 12, $
                        /capture)
  junk = widget_button(uvs.xbid, $
                       value = 'Pick...', $
                       uvalue = 'XP')

  uvs.ybid = widget_base(base, $
                         /row)
  uvs.yid = cw_enter(uvs.ybid, $
                        value = '', $
                        /text, $
                        uvalue = 'Y', $
                        label = 'Y Variable:', $
                        xsize = 12, $
                        /capture)
  junk = widget_button(uvs.ybid, $
                       value = 'Pick...', $
                       uvalue = 'YP')

  uvs.eloxbid = widget_base(base, $
                            /row)
  uvs.eloxid = cw_enter(uvs.eloxbid, $
                           value = '', $
                           /text, $
                           uvalue = 'ELOX', $
                           label = 'Lower X error:', $
                           xsize = 12, $
                           /capture)
  junk = widget_button(uvs.eloxbid, $
                       value = 'Pick...', $
                       uvalue = 'ELOXP')

  widget_control, uvs.eloxbid, sensitive = $
                  exlm((*pdefs.data)[pdefs.cset].type > 0) 

  uvs.ehixbid = widget_base(base, $
                            /row)
  uvs.ehixid = cw_enter(uvs.ehixbid, $
                           value = '', $
                           /text, $
                           uvalue = 'EHIX', $
                           label = 'Upper X error:', $
                           xsize = 12, $
                           /capture)
  junk = widget_button(uvs.ehixbid, $
                       value = 'Pick...', $
                       uvalue = 'EHIXP')
  widget_control, uvs.ehixbid, sensitive = $
                  exhm((*pdefs.data)[pdefs.cset].type > 0) 


  uvs.eloybid = widget_base(base, $
                            /row)
  uvs.eloyid = cw_enter(uvs.eloybid, $
                           value = '', $
                           /text, $
                           uvalue = 'ELOY', $
                           label = 'Lower Y error:', $
                           xsize = 12, $
                           /capture)
  junk = widget_button(uvs.eloybid, $
                       value = 'Pick...', $
                       uvalue = 'ELOYP')
  widget_control, uvs.eloybid, sensitive = $
                  eylm((*pdefs.data)[pdefs.cset].type > 0) 

  uvs.ehiybid = widget_base(base, $
                            /row)
  uvs.ehiyid = cw_enter(uvs.ehiybid, $
                           value = '', $
                           /text, $
                           uvalue = 'EHIY', $
                           label = 'Upper Y error:', $
                           xsize = 12, $
                           /capture)
  junk = widget_button(uvs.ehiybid, $
                       value = 'Pick...', $
                       uvalue = 'EHIYP')
  widget_control, uvs.ehiybid, sensitive = $
                  eyhm((*pdefs.data)[pdefs.cset].type > 0) 

  if ((*pdefs.data)[pdefs.cset].mode eq 0) then emds = ['None', $
                                                        '±Y', $
                                                        '-Y +Y', $
                                                        '±X', $
                                                        '-X +X', $
                                                        '±X ±Y', $
                                                        '±X -Y +Y', $
                                                        '-X +X ±Y', $
                                                        '-X +X -Y +Y'] $
  else emds = ['None', $
               '±Theta', $
               '-Theta +Theta', $
               '±R', $
               '-R +R', $
               '±R ±Theta', $
               '±R -Theta +Theta', $
               '-R +R ±Theta', $
               '-R +R -Theta +Theta']

  errid = widget_droplist(base, $
                          value = emds, $
                          title = 'Errors present : ', $
                          uvalue = 'ERRS')
  widget_control, errid, set_droplist_select = $
                  (*pdefs.data)[pdefs.cset].type > 0

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

  cw_enter_focus, uvs.xid

  widget_control, base, event_func = 'grf_tlv_event', set_uvalue = $
                  uvs, /no_copy

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = uvs, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, /sensitive 

  if (ev.exited eq -1) then return, 0

  nxy = n_elements(*uvs.x)
  xydata = dblarr(([2, 3, 4, 3, 4, 4, 5, 5, 6])[uvs.type], $
                  nxy > 2)

  xydata(0, 0:nxy-1) = *uvs.x
  xydata(1, 0:nxy-1) = *uvs.y

  if (uvs.type ge 1) then begin
     xydata(2, 0) = *uvs.err
     ptr_free, uvs.err
  endif

  if (*pdefs.data)[pdefs.cset].type eq 9 then ptr_free, $
     (*(*pdefs.data)(pdefs.cset).xydata).x, $
     (*(*pdefs.data)(pdefs.cset).xydata).y, $
     (*(*pdefs.data)(pdefs.cset).xydata).z

  ptr_free, (*pdefs.data)[pdefs.cset].xydata
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].ndata = n_elements(*uvs.x)
  (*pdefs.data)[pdefs.cset].type = uvs.type
  ptr_free, uvs.x, uvs.y

  return, 1

end
