; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_tlv_event, event

  widget_control, event.top, get_uvalue = state
  widget_control, event.id, get_uvalue = mnu

  evr =  {id: event.handler, $
          top: event.top, $
          handler: event.handler, $
          exit: 0}
  
  case mnu of
     "DONT": begin
        evr.exit = -1
     end

     'DO': begin

        if widget_info(state.zid, /valid) && state.names.z ne '' then $
           (scope_varfetch(state.names.z, level = 1, /enter)) = $
           *state.xydata.z
           
        if widget_info(state.xid, /valid) && state.names.x ne '' then $
           (scope_varfetch(state.names.x, level = 1, /enter)) = $
           *state.xydata.x
        
        if widget_info(state.yid, /valid) && state.names.y ne '' then $
              (scope_varfetch(state.names.y, level = 1, /enter)) = $
              *state.xydata.y 

        if widget_info(state.yeid, /valid) && state.names.y_err ne '' then $
           (scope_varfetch(state.names.y_err, level = 1, /enter)) = $
           reform((*state.xydata.y_err)[0, *])
        
        if widget_info(state.yeidl, /valid) && $
           state.names.y_err_l[0] ne '' then $
              (scope_varfetch(state.names.y_err_l[0], level = 1, /enter)) = $
           reform((*state.xydata.y_err)[0, *])
        
        if widget_info(state.yeidh, /valid) && $
           state.names.y_err_l[1] ne '' then $
              (scope_varfetch(state.names.y_err_l[1], level = 1, /enter)) = $
           reform((*state.xydata.y_err)[1, *])

        if widget_info(state.xeid, /valid) && state.names.x_err ne '' then $
           (scope_varfetch(state.names.x_err, level = 1, /enter)) = $
           reform((*state.xydata.x_err)[0, *])
        
        if widget_info(state.xeidl, /valid) && $
           state.names.x_err_l[0] ne '' then $
              (scope_varfetch(state.names.x_err_l[0], level = 1, /enter)) = $
           reform((*state.xydata.x_err)[0, *])
        
        if widget_info(state.xeidh, /valid) && $
           state.names.x_err_l[1] ne '' then $
              (scope_varfetch(state.names.x_err_l[1], level = 1, /enter)) = $
           reform((*state.xydata.x_err)[1, *])
        
        evr.exit = 1
     end

     'X': state.names.x = event.value
     'Y': state.names.y = event.value
     'Z': state.names.z = event.value
     'XERR': state.names.x_err = event.value
     'XERR-': state.names.x_err_l[0] = event.value
     'XERR+': state.names.x_err_l[1] = event.value
     'YERR': state.names.y_err = event.value
     'YERR-': state.names.y_err_l[0] = event.value
     'YERR+': state.names.y_err_l[1] = event.value

  endcase

  widget_control, event.top, set_uvalue = state

  return, evr
  
end
pro graff_to_tlv, pdefs

;+
; GRAFF_TO_TLV
;	Turn the data of the current dataset into variables at the
;	$MAIN$ level.
;
; Usage:
;	graff_to_tlv, pdefs
;
; Argument:
;	pdefs	struct	The main graffer data structure.
;
; History:
;	Original: 3/2/12; SJT
;	Make a gui to change names: 12/7/12; SJT
;	Make defaults include DS number, and allow non-saving:
;	2/12/15; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

  data = (*pdefs.data)[pdefs.cset]

  if not ptr_valid(data.xydata) then return
  xydata = *data.xydata
  
  if data.type lt 0 then return ; Not applicable to functions.

  sfx = string(pdefs.cset+1, format = "('_',i0)")

  widget_control, pdefs.ids.graffer, sensitive = 0
  
  base = widget_base(title = "Export dataset", $
                     /column, $
                     group = pdefs.ids.graffer)
  names = {gr_export_names, $
           x: "GR_X"+sfx, $
           y: "GR_Y"+sfx, $
           z: "GR_Z"+sfx, $
           x_err: "GR_X_ERR"+sfx, $
           x_err_l: ["GR_X_ERR_L"+sfx, "GR_X_ERR_U"+sfx], $
           y_err: "GR_Y_ERR"+sfx, $
           y_err_l: ["GR_Y_ERR_L"+sfx, "GR_Y_ERR_U"+sfx]}
  
  zid = 0l
  xid = 0l
  yid = 0l
  xeid = 0l
  xeidl = 0l
  xeidh = 0l
  yeid = 0l
  yeidl = 0l
  yeidh = 0l
  
  if data.type eq 9 then begin
     zid = cw_enter(base, $
                    /text, $
                    value = names.z, $
                    xsize = 10, $
                    label = "Z:", $
                    uvalue = 'Z', $
                    /all, $
                    /capture)
     xid = cw_enter(base, $
                    /text, $
                    value = names.x, $
                    xsize = 10, $
                    label = "X:", $
                    uvalue = 'X', $
                    /all, $
                    /capture)
     yid = cw_enter(base, $
                    /text, $
                    value = names.y, $
                    xsize = 10, $
                    label = "Y:", $
                    uvalue = 'Y', $
                    /all, $
                    /capture)

  endif else begin 
     xid = cw_enter(base, $
                    /text, $
                    value = names.x, $
                    xsize = 10, $
                    label = "X:", $
                    uvalue = 'X', $
                    /all, $
                    /capture)
     yid = cw_enter(base, $
                    /text, $
                    value = names.y, $
                    xsize = 10, $
                    label = "Y:", $
                    uvalue = 'Y', $
                    /all, $
                    /capture)

     if ptr_valid(xydata.x_err) then begin
        sz = size(*(xydata.x_err), /dim)
        case sz[0] of
           1: xeid = cw_enter(base, $
                              /text, $
                              value = names.x_err, $
                              xsize = 10, $
                              label = "X err:", $
                              uvalue = 'XERR', $
                              /all, $
                              /capture)
           2: begin
              xeidl = cw_enter(base, $
                               /text, $
                               value = names.x_err_l[0], $
                               xsize = 10, $
                               label = "X err-:", $
                               uvalue = 'XERR-', $
                               /all, $
                               /capture)
              xeidh = cw_enter(base, $
                               /text, $
                               value = names.x_err_l[1], $
                               xsize = 10, $
                               label = "X err+:", $
                               uvalue = 'XERR+', $
                               /all, $
                               /capture)
           end
        endcase
     endif


     if ptr_valid(xydata.y_err) then begin
        sz = size(*(xydata.y_err), /dim)
        case sz[0] of
           1: yeid = cw_enter(base, $
                              /text, $
                              value = names.y_err, $
                              xsize = 10, $
                              label = "Y err:", $
                              uvalue = 'YERR', $
                              /all, $
                              /capture)

           2: begin
              yeidl = cw_enter(base, $
                               /text, $
                               value = names.y_err_l[0], $
                               xsize = 10, $
                               label = "Y err-:", $
                               uvalue = 'YERR-', $
                               /all, $
                               /capture)
              yeidh = cw_enter(base, $
                               /text, $
                               value = names.y_err_l[1], $
                               xsize = 10, $
                               label = "Y err+:", $
                               uvalue = 'YERR+', $
                               /all, $
                               /capture)
           end
        endcase
     endif       
  endelse

  jb = widget_base(base, $
                   /row)

  junk = widget_button(jb, $
                       value = 'Apply', $
                       uvalue = 'DO')

  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  state = {xydata: *data.xydata, $
           type: data.type, $
           xid: xid, $
           yid: yid, $
           zid: zid, $
           xeid: xeid, $
           xeidl: xeidl, $
           xeidh: xeidh, $
           yeid: yeid, $
           yeidl: yeidl, $
           yeidh: yeidh, $
           names: names}

  widget_control, base, /real, set_uvalue = state, event_func = $
                  'gr_tlv_event' 

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exit ne 0)
  widget_control, base, /destroy
  widget_control, pdefs.ids.graffer, /sensitive

end
