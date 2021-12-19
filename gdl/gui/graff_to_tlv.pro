; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_tlv_event, event

  widget_control, event.top, get_uvalue = state
  widget_control, event.id, get_uvalue = mnu

  case mnu of
     "DONT": begin
        widget_control, /destroy, event.top
        return
     end

     'DO': begin

        if state.type eq 9 then begin
           if state.names.z ne '' then $
              (scope_varfetch(state.names.z, level = 1, /enter)) = $
              *(state.xydata.z) 
           if state.names.x ne '' then $
              (scope_varfetch(state.names.x, level = 1, /enter)) = $
              *(state.xydata.x)
           if state.names.y ne '' then $
              (scope_varfetch(state.names.y, level = 1, /enter)) = $
              *(state.xydata.y) 

        endif else begin

           if state.names.x ne '' then $
              (scope_varfetch(state.names.x, level = 1, /enter)) = $
              reform(state.xydata[0, *])
           if state.names.y ne '' then $
              (scope_varfetch(state.names.y, level = 1, /enter)) = $
              reform(state.xydata[1, *])

           case state.type of
              1: if state.names.y_err ne '' then $
                 (scope_varfetch(state.names.y_err, level = 1, $
                                 /enter)) = $
                 reform(state.xydata[2, *])
              3: if state.names.x_err ne '' then $
                 (scope_varfetch(state.names.x_err, level = 1, $
                                 /enter)) = $
                 reform(state.xydata[2, *])
              2: begin
                 if state.names.y_err_b[0] ne '' then $
                    (scope_varfetch(state.names.y_err_b[0], $
                                    level = 1, /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.y_err_b[1] ne '' then $
                    (scope_varfetch(state.names.y_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[3, *])
              end
              4: begin
                 if state.names.x_err_b[0] ne '' then $
                    (scope_varfetch(state.names.x_err_b[0], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.x_err_b[1] ne '' then $
                    (scope_varfetch(state.names.x_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[3, *])
              end
              5: begin
                 if state.names.x_err_ ne '' then $
                    (scope_varfetch(state.names.x_err, $
                                    level = 1, /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.y_err ne '' then $
                    (scope_varfetch(state.names.y_err, $
                                    level = 1, /enter)) = $
                    reform(state.xydata[3, *])
              end

              6: begin
                 if state.names.x_err ne '' then $
                    (scope_varfetch(state.names.x_err, $
                                    level = 1, /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.y_err_b[0] ne '' then $
                    (scope_varfetch(state.names.y_err_b[0], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[3, *])
                 if state.names.y_err_b[1] ne '' then $
                    (scope_varfetch(state.names.y_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[4, *])
              end

              7: begin
                 if state.names.x_err_b[0] ne '' then $
                    (scope_varfetch(state.names.x_err_b[0], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.x_err_b[1] ne '' then $
                    (scope_varfetch(state.names.x_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[3, *])
                 if state.names.y_err ne '' then $
                    (scope_varfetch(state.names.y_err, $
                                    level = 1, /enter)) = $
                    reform(state.xydata[4, *])
              end

              
              8: begin
                 if state.names.x_err_b[0] ne '' then $
                    (scope_varfetch(state.names.x_err_b[0], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[2, *])
                 if state.names.x_err_b[1] ne '' then $
                    (scope_varfetch(state.names.x_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[3, *])
                 if state.names.y_err_b[0] ne '' then $
                    (scope_varfetch(state.names.y_err_b[0], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[4, *])
                 if state.names.y_err_b[1] ne '' then $
                    (scope_varfetch(state.names.y_err_b[1], $
                                    level = 1, $
                                    /enter)) = $
                    reform(state.xydata[5, *])
              end
              else:
           endcase
        endelse
        widget_control, event.top, /destroy
        return
     end

     'X': state.names.x = event.value
     'Y': state.names.y = event.value
     'Z': state.names.z = event.value
     'XERR': state.names.x_err = event.value
     'XERR-': state.names.x_err_b[0] = event.value
     'XERR+': state.names.x_err_b[1] = event.value
     'YERR': state.names.y_err = event.value
     'YERR-': state.names.y_err_b[0] = event.value
     'YERR+': state.names.y_err_b[1] = event.value

  endcase

  widget_control, event.top, set_uvalue = state
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
           x_err_b: ["GR_X_ERR_L"+sfx, "GR_X_ERR_U"+sfx], $
           y_err: "GR_Y_ERR"+sfx, $
           y_err_b: ["GR_Y_ERR_L"+sfx, "GR_Y_ERR_U"+sfx]}
  
  if data.type eq 9 then begin
     junk = cw_enter(base, $
                     /text, $
                     value = names.z, $
                     xsize = 10, $
                     label = "Z:", $
                     uvalue = 'Z', $
                     /all, $
                     /capture)
     junk = cw_enter(base, $
                     /text, $
                     value = names.x, $
                     xsize = 10, $
                     label = "X:", $
                     uvalue = 'X', $
                     /all, $
                     /capture)
     junk = cw_enter(base, $
                     /text, $
                     value = names.y, $
                     xsize = 10, $
                     label = "Y:", $
                     uvalue = 'Y', $
                     /all, $
                     /capture)

  endif else begin 
     junk = cw_enter(base, $
                     /text, $
                     value = names.x, $
                     xsize = 10, $
                     label = "X:", $
                     uvalue = 'X', $
                     /all, $
                     /capture)
     junk = cw_enter(base, $
                     /text, $
                     value = names.y, $
                     xsize = 10, $
                     label = "Y:", $
                     uvalue = 'Y', $
                     /all, $
                     /capture)


     case data.type of
        1: junk = cw_enter(base, $
                           /text, $
                           value = names.y_err, $
                           xsize = 10, $
                           label = "Y err:", $
                           uvalue = 'YERR', $
                           /all, $
                           /capture)

        3: junk = cw_enter(base, $
                           /text, $
                           value = names.x_err, $
                           xsize = 10, $
                           label = "X err:", $
                           uvalue = 'XERR', $
                           /all, $
                           /capture)
        2: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[0], $
                           xsize = 10, $
                           label = "Y err-:", $
                           uvalue = 'YERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[1], $
                           xsize = 10, $
                           label = "Y err+:", $
                           uvalue = 'YERR+', $
                           /all, $
                           /capture)
        end
        4: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[0], $
                           xsize = 10, $
                           label = "X err-:", $
                           uvalue = 'XERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[1], $
                           xsize = 10, $
                           label = "X err+:", $
                           uvalue = 'XERR+', $
                           /all, $
                           /capture)
        end
        5: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err, $
                           xsize = 10, $
                           label = "X err:", $
                           uvalue = 'XERR', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err, $
                           xsize = 10, $
                           label = "Y err:", $
                           uvalue = 'YERR', $
                           /all, $
                           /capture)
        end

        6: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err, $
                           xsize = 10, $
                           label = "X err:", $
                           uvalue = 'XERR', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[0], $
                           xsize = 10, $
                           label = "Y err-:", $
                           uvalue = 'YERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[1], $
                           xsize = 10, $
                           label = "Y err+:", $
                           uvalue = 'YERR+', $
                           /all, $
                           /capture)
        end

        7: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[0], $
                           xsize = 10, $
                           label = "X err-:", $
                           uvalue = 'XERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[1], $
                           xsize = 10, $
                           label = "X err+:", $
                           uvalue = 'XERR+', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err, $
                           xsize = 10, $
                           label = "Y err:", $
                           uvalue = 'YERR', $
                           /all, $
                           /capture)
        end

        
        8: begin
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[0], $
                           xsize = 10, $
                           label = "X err-:", $
                           uvalue = 'XERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.x_err_l[1], $
                           xsize = 10, $
                           label = "X err+:", $
                           uvalue = 'XERR+', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[0], $
                           xsize = 10, $
                           label = "Y err-:", $
                           uvalue = 'YERR-', $
                           /all, $
                           /capture)
           junk = cw_enter(base, $
                           /text, $
                           value = names.y_err_l[1], $
                           xsize = 10, $
                           label = "Y err+:", $
                           uvalue = 'YERR+', $
                           /all, $
                           /capture)
        end
        else:
     endcase
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
           names: names}

  widget_control, base, /real, set_uvalue = state

  xmanager, "gr_tlv", base

  widget_control, pdefs.ids.graffer, /sensitive

end
