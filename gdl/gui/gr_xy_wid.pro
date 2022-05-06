; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_XY_WID
;	Input or edit x & y values for Graffer
;
; Usage:
;	ichange = gr_xy_wid(pdefs)
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
;
; Argument:
;	pdefs	struct	in/out	Graffer definition structure
;
; Keyword:
;	line	int	input	Line number at which to set text
;				insertion point
;
; History:
;	Original: 17/8/95; SJT
;	Change to pro operating on pdefs: 22/8/95; SJT
;	Add timer event to push to front if obscured: 23/8/95; SJT
;	Shorten name: 25/11/96; SJT
;	Add LINE keyword: 28/11/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Fix "bug" when first line of widget is blank: 5/2/97; SJT
;	Make error selector tracking event update the sensitivity
;	mask: 7/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace cw_bbselector with cw_pdmenu_plus & add "hard" sort:
;	28/9/16; SJT 
;-

function Grf_emask, ids, type, text = text

  widget_control, ids.xyid, get_value = text

  tpr = where(strlen(strtrim(text)) ne 0, nv)
  if (nv ne 0) then text = text[tpr] $
  else begin
     iexit = 0
     return, -1
  endelse
  
  tl = strsplit(text[0], count = nc, /extr)
  case nc of
     1: mask = [1, 0, 0, 0, 0, 0, 0, 0, 0]
     2: mask = [1, 0, 0, 0, 0, 0, 0, 0, 0]
     3: mask = [0, 1, 0, 1, 0, 0, 0, 0, 0]
     4: mask = [0, 0, 1, 0, 1, 1, 0, 0, 0]
     5: mask = [0, 0, 0, 0, 0, 0, 1, 1, 0]
     6: mask = [0, 0, 0, 0, 0, 0, 0, 0, 1]
     Else: mask = intarr(9)
  endcase

  for j = 0, n_elements(ids.errids)-1 do  $
     widget_control, ids.errids[j], sensitive = mask[j]

  widget_control, ids.errid, get_value = type

  if (not mask(type)) then begin
     l = where(mask)
     widget_control, ids.errid, set_value = l[0]
  endif

  return, mask

end

function Xyw_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.handler, get_uvalue = ids

  iexit = 0

  txt = ''                      ; Dummy values for text & type.
  type = 0

  case but of
     'ACTION': begin
        if (event.value eq -1) then begin
           message, /continue, /noprint, 'User specified "CANCEL"'
           iexit = -1
        endif else begin
           
           mask = grf_emask(ids, type, text = txt)
           if (mask[0] lt 0) then goto, bailout
           
           if ~mask[type] then begin
              l = where(mask)
              widget_control, ids.errid, set_value = l[0]
              junk = dialog_message(["Error bar settings",  $
                                     "do not match available", + $
                                     "data. Please reselect the",  $
                                     "errors option."], $
                                    dialog_parent = event.top)
              type = l[0]
              iexit = 0
           endif else iexit = event.value
        endelse
        
     end
     
     'FUN': if (strpos(tag_names(event, /struct), $
                       'TRACK') ne -1) then begin
        if (event.enter) then widget_control, event.id, /input_focus
     endif else begin
        ;; if is_gdl() && event.type eq 0 && event.ch eq 10 then begin
        ;;                         ; Carriage return is not correctly
        ;;                         ; inserted by editable text widgets in
        ;;                         ; GDL.
        ;;    widget_control, event.id, get_value = txt
        ;;    txtr = strmid(txt, 0, event.offset) + string(10b) + $
        ;;           strmid(txt, event.offset)
        ;;    widget_control, event.id, set_value = txtr, $
        ;;                    set_text_select = event.offset+1
        ;; endif
        mask = grf_emask(ids, type)
        if (mask[0] lt 0) then goto, bailout
     endelse
     
     'ERRS': if (tag_names(event, /struct) eq 'WIDGET_TRACKING') then $
        mask = grf_emask(ids, type)
     
  endcase

Bailout:

  return, {id:event.id, $
           top:event.top, $
           handler:0l, $
           Exited:iexit, $
           value:txt, $
           type:type}


end

function Gr_xy_wid, pdefs, line = line

  common graffer_options, optblock

;	First extract the data


  fflag = ((*pdefs.data)[pdefs.cset].type lt 0 or $
           (*pdefs.data)[pdefs.cset].type eq 9)
  if (fflag) then $
     if dialog_message(['CURRENT DATA SET IS A FUNCTION', $
                        'OR A 2-D DATASET, ENTERING DATA', $
                        'WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0
  

  if (fflag or (*pdefs.data)[pdefs.cset].ndata eq 0) then txt = '' $
  else begin
     ner = gr_n_errors((*pdefs.data)[pdefs.cset].type)
     ncol = 2 + ner[0] + ner[1]
     xyvals = dblarr(ncol, (*pdefs.data)[pdefs.cset].ndata)
     xyvals[0, *] = *(*(*pdefs.data)[pdefs.cset].xydata).x
     xyvals[1, *] = *(*(*pdefs.data)[pdefs.cset].xydata).y
     if ner[0] ne 0 then  $
        xyvals[2:ner[0]+1, *] = *(*(*pdefs.data)[pdefs.cset].xydata).x_err
     if ner[1] ne 0 then  $
        xyvals[2+ner[0]:*, *] = *(*(*pdefs.data)[pdefs.cset].xydata).y_err
     
     txt = strarr((*pdefs.data)[pdefs.cset].ndata)
     fmt = string(ncol, format = "('(',I0,'G19.12)')")
     for k = 0l, (*pdefs.data)[pdefs.cset].ndata-1 do $ 
        txt[k] = string(xyvals[*, k], format = fmt)
  endelse

  if (keyword_set(line)) then begin
     txtl = strlen(txt)+1
     char0 = total(txtl(0:line-1))
  endif else char0 = 0

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer Data Input',  $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer', $
                    /column)
  base = widget_base(tlb, $
                     /column)

                                ; The actual data definition

  junk = widget_label(base, $
                      value = 'Enter x y values and optionally errors')
  junk = widget_label(base, $
                      value = '1 data point per line')


  xyid = widget_text(base, $
                     /edit, $
                     xsize = 30 > max(strlen(txt)), $
                     ysize = 30, $
                     uvalue = 'FUN', $
                     value = txt, $
                     /scroll, $
                     track = optblock.track)
  
  widget_control, xyid, set_text_select = char0

  case (*pdefs.data)[pdefs.cset].type > 0 of
     0: mask = [1, 0, 0, 0, 0, 0, 0, 0, 0]
     1: mask = [0, 1, 0, 1, 0, 0, 0, 0, 0]
     3: mask = [0, 1, 0, 1, 0, 0, 0, 0, 0]
     2: mask = [0, 0, 1, 0, 1, 1, 0, 0, 0]
     4: mask = [0, 0, 1, 0, 1, 1, 0, 0, 0]
     5: mask = [0, 0, 1, 0, 1, 1, 0, 0, 0]
     6: mask = [0, 0, 0, 0, 0, 0, 1, 1, 0]
     7: mask = [0, 0, 0, 0, 0, 0, 1, 1, 0]
     8: mask = [0, 0, 0, 0, 0, 0, 0, 0, 1]
     Else: mask = intarr(9)
  endcase

  if ((*pdefs.data)[pdefs.cset].mode eq 0) then $
     emds = [{label: 'None', sensitive: mask[0]}, $ $
             {label: '±Y', sensitive: mask[1]}, $
             {label: '-Y +Y', sensitive: mask[2]}, $
             {label: '±X', sensitive: mask[3]}, $
             {label: '-X +X', sensitive: mask[4]}, $
             {label: '±X ±Y', sensitive: mask[5]}, $
             {label: '±X -Y +Y', sensitive: mask[6]}, $
             {label: '-X +X ±Y', sensitive: mask[7]}, $
             {label: '-X +X -Y +Y', sensitive: mask[8]}] $
  else emds = [{label: 'None', sensitive: mask[0]}, $
               {label: '±Theta', sensitive: mask[1]}, $
               {label: '-Theta +Theta', sensitive: mask[2]}, $
               {label: '±R', sensitive: mask[3]}, $
               {label: '-R +R', sensitive: mask[4]}, $
               {label: '±R ±Theta', sensitive: mask[5]}, $
               {label: '±R -Theta +Theta', sensitive: mask[6]}, $
               {label: '-R +R ±Theta', sensitive: mask[7]}, $
               {label: '-R +R -Theta +Theta', sensitive: mask[8]}]

  jb = widget_base(base, $
                   /row)
  junk = widget_label(jb, $
                      value = 'Error columns:')
  errid = cw_pdmenu_plus(jb, $
                         emds, $
                         ids = errids, $
                         /selector, $
                         initial = (*pdefs.data)[pdefs.cset].type > 0, $
                         uvalue = 'ERRS', $
                         track = optblock.track)

  for j = 0, n_elements(errids)-1 do  $
     widget_control, errids(j), sensitive = mask(j)

                                ; Control

  junk = cw_bgroup(base, $
                   ['Apply', 'Apply and Sort', 'Cancel'], $
                   button_uvalue = [1, 2, -1], $
                   uvalue = 'ACTION', $
                   /row)

                                ; Realise and do RYO event handling

  widget_control, base, /real, event_func = 'xyw_event',  $
                  set_uvalue = {xyid:xyid, $
                                Errid:errid, $
                                Errids:errids}

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, tlb, /destroy

  if (ev.exited ge 1) then begin ; The DO button
     
;	This part handles the processing of the data read

     locs = where(strlen(strtrim(ev.value)) gt 0, nact)
     if (nact ne 0) then begin
        xyvals = graff_decode_xy(ev.value(locs), nt)
        if (nt lt 0) then goto, badfile
        
        if ev.exited eq 2 then begin
           idx = sort(xyvals[0, *])
           xyvals = xyvals[*, idx]
        endif
        (*pdefs.data)[pdefs.cset].ndata = nact

        if (*pdefs.data)[pdefs.cset].type eq 9 then ptr_free, $
           (*(*pdefs.data)(pdefs.cset).xydata).x, $
           (*(*pdefs.data)(pdefs.cset).xydata).y, $
           (*(*pdefs.data)(pdefs.cset).xydata).z $
        else if (*pdefs.data)[pdefs.cset].type ge 0 then ptr_free, $
           (*(*pdefs.data)(pdefs.cset).xydata).x, $
           (*(*pdefs.data)(pdefs.cset).xydata).y, $
           (*(*pdefs.data)(pdefs.cset).xydata).x_err, $
           (*(*pdefs.data)(pdefs.cset).xydata).y_err

        ptr_free, (*pdefs.data)[pdefs.cset].xydata

        ner = gr_n_errors(ev.type)
        
        xydata = {graff_xydata}
        xydata.x = ptr_new(reform(xyvals[0, *]))
        xydata.y = ptr_new(reform(xyvals[1, *]))
        if ner[0] ne 0 then xydata.x_err = ptr_new(xyvals[2:ner[0]+1, *])
        if ner[1] ne 0 then xydata.y_err = ptr_new(xyvals[2+ner[0]:*, *])
        
        (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
        (*pdefs.data)[pdefs.cset].type = ev.type
     endif
  endif else goto, badfile

  widget_control, pdefs.ids.graffer, sensitive = 1

  return, 1

Badfile:

  graff_msg, pdefs.ids.message, ["Graffer input failed:", !Err_string]
  widget_control, pdefs.ids.graffer, sensitive = 1

  return, 0

end
