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
     'ACTION': if (event.value eq 1) then begin
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
                                 'If Y is not set, X must be ' + $
                                 'complex or a ' + $
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

        nerc = gr_n_errors(uvs.type)
        
        if nerc[0] ne 0 then begin
           if ptr_valid(uvs.xerr) then begin
              sxer = size(uvs.xerr)
              if sxer[0] eq 1 then nxer = 1 $
              else nxer = sxer[1]
           endif else nxer = 0
           
           xerrs = dblarr(nerc[0], nx)
           
           widget_control, uvs.eloxid, get_value = elvar
           elvar = strtrim(elvar, 2)
           if (elvar eq '') then begin
              widget_control, uvs.mid, set_value = 'Requested ' + $
                              'data type needs a lower X error'
              iexit = 0
              goto, donefor
           endif
           if (elvar eq '.') then begin
              if nxer ge 1 then begin
                 errtmp = *uvs.xerr[0, *]
                 nerl = n_elements(errtmp)
              endif else nerl = 0
           endif else errtmp = grf_tlv_get(elvar, nerl)
           
           if (nerl ne nx) then begin
              widget_control, uvs.mid, set_value = $
                              'Errors and data must be same length'
              iexit = 0
              goto, donefor
           endif else xerrs[0, *] = errtmp
           if nerc[0] eq 2 then begin
              widget_control, uvs.ehixid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs an upper X error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if nxer eq 2 then begin
                    errtmp = *uvs.xerr[1, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 0
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else xerrs[1, *] = errtmp
           endif
        endif
        
        if nerc[1] ne 0 then begin
           if ptr_valid(uvs.yerr) then begin
              syer = size(uvs.yerr)
              if syer[0] eq 1 then nyer = 1 $
              else nyer = syer[1]
           endif else nyer = 0

           yerrs = dblarr(nerc[1], nx)
           
           widget_control, uvs.eloyid, get_value = elvar
           elvar = strtrim(elvar, 2)
           if (elvar eq '') then begin
              widget_control, uvs.mid, set_value = 'Requested ' + $
                              'data type needs a lower Y error'
              iexit = 0
              goto, donefor
           endif
           if (elvar eq '.') then begin
              if nyer ge 1 then begin
                 errtmp = *uvs.yerr[0, *]
                 nerl = n_elements(errtmp)
              endif else nerl = 0
           endif else errtmp = grf_tlv_get(elvar, nerl)
           if (nerl ne nx) then begin
              widget_control, uvs.mid, set_value = $
                              'Errors and data must be same length'
              iexit = 0
              goto, donefor
           endif else yerrs[0, *] = errtmp

           if nerc[1] eq 2 then begin
              widget_control, uvs.ehiyid, get_value = elvar
              elvar = strtrim(elvar, 2)
              if (elvar eq '') then begin
                 widget_control, uvs.mid, set_value = 'Requested ' + $
                                 'data type needs an upper Y error'
                 iexit = 0
                 goto, donefor
              endif
              if (elvar eq '.') then begin
                 if nyer eq 2 then begin
                    errtmp = *uvs.yerr[1, *]
                    nerl = n_elements(errtmp)
                 endif else nerl = 01
              endif else errtmp = grf_tlv_get(elvar, nerl)
              if (nerl ne nx) then begin
                 widget_control, uvs.mid, set_value = $
                                 'Errors and data must be same length'
                 iexit = 0
                 goto, donefor
              endif else yerrs[2, *] = errtmp
           endif
        endif
     endif else iexit = -1
     
     
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
     ptr_free, uvs.x, uvs.y, uvs.xerr, uvs.yerr
     uvs.x = ptr_new(x)
     uvs.y = ptr_new(y)
     if nerc[0] gt 0 then uvs.xerr = ptr_new(xerrs)
     if nerc[0] gt 0 then uvs.yerr = ptr_new(yerrs)
  endif

  widget_control, base, set_uvalue = uvs, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end





function Graff_tlv, pdefs

  common Gr_tlvs_masks, exlm, exhm, eylm, eyhm

  dstype = (*pdefs.data)[pdefs.cset].type
  fflag = (dstype lt 0)
  flag2 = (dstype ge 9)
  if (fflag) then begin
     if dialog_message(['CURRENT DATA SET IS A FUNCTION', $
                        'OR A 2-D DATASET ENTERING DATA', $
                        'WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0
     dstype = 0
  endif
  if flag2 then begin
     if dialog_message(['CURRENT DATA SET IS A 2-D DATASET', $
                        'ENTERING 1-D DATA WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0
     dstype = 0
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
        xerr:    ptr_new(), $
        yerr:    ptr_new(), $
        Type:   dstype $
        }

  if dstype ge 0 && dstype le 8 && $
     ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     xydata = *(*pdefs.data)[pdefs.cset].xydata
     if ptr_valid(xydata.x) then begin
        uvs.x = ptr_new(*xydata.x)
        uvs.y = ptr_new(*xydata.y)
        if ptr_valid(xydata.x_err) then $
           uvs.xerr = ptr_new(*xydata.x_err)
        if ptr_valid(xydata.y_err) then $
           uvs.xerr = ptr_new(*xydata.y_err)
     endif
  endif else xydata = {graff_xydata} 

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

  widget_control, uvs.eloxbid, sensitive = exlm[dstype]

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
  widget_control, uvs.ehixbid, sensitive = exhm[dstype]


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
  widget_control, uvs.eloybid, sensitive = eylm[dstype]

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
  widget_control, uvs.ehiybid, sensitive = eyhm[dstype]

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
  widget_control, errid, set_droplist_select = dstype

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

  ptr_free, xydata.x, xydata.y, xydata.x_err, xydata.y_err
  
  xydata.x = ptr_new(*uvs.x)
  xydata.y = ptr_new(*uvs.y)
  if ptr_valid(uvs.xerr) then xydata.x_err = ptr_new(*uvs.xerr)
  if ptr_valid(uvs.yerr) then xydata.y_err = ptr_new(*uvs.yerr)
  

  if (*pdefs.data)[pdefs.cset].type eq 9 then ptr_free, $
     (*(*pdefs.data)(pdefs.cset).xydata).x, $
     (*(*pdefs.data)(pdefs.cset).xydata).y, $
     (*(*pdefs.data)(pdefs.cset).xydata).z $
  else if (*pdefs.data)[pdefs.cset].type ge 0 then $
     ptr_free, (*(*pdefs.data)(pdefs.cset).xydata).x, $
               (*(*pdefs.data)(pdefs.cset).xydata).y, $
               (*(*pdefs.data)(pdefs.cset).xydata).x_err, $
               (*(*pdefs.data)(pdefs.cset).xydata).y_err

  ptr_free, (*pdefs.data)[pdefs.cset].xydata
  
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].ndata = n_elements(*uvs.x)
  (*pdefs.data)[pdefs.cset].type = uvs.type
  ptr_free, uvs.x, uvs.y, uvs.xerr, uvs.yerr

  return, 1

end
