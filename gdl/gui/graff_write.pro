; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Graff_write, pdefs, event, track_flag

;+
; GRAFF_WRITE
;	Process a draw event in text mode
;
; Usage:
;	ichange = graff_write(pdefs, event, track_flag)
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
; Argument:
;	pdefs	struct	in/out	The plot definition structure.
;	event	struct	input	The draw event that triggered this
;	track_flag byte	input	A flag to say if it's a tracking event.
;
; History:
;	Carved from graffer: 17/8/95; SJT
;	Add tracking event handling: 5/12/95; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Change to plot crosshairs in normalized coords: 27/1/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;-

if (track_flag) then begin
    graff_msg, pdefs.ids.hlptxt, 'Left ' + $
      '= add string, Middle = edit string, Right = delete string'
    return, 0
endif

; Force primary Y-axis to be used.

opy = !y
if (pdefs.y_right) then $
  yaxis = (*pdefs.data)[pdefs.cset].y_axis $
else yaxis = 0

!y = pdefs.ytransform[yaxis]

gr_coord_convert, event.x, event.y, x, y, /device, /to_data
xy = [x, y]
gr_coord_convert, event.x, event.y, x, y, /device, /to_region
xyn = [x, y]
gr_coord_convert, event.x, event.y, x, y, /device, /to_frame
xyf = [x, y]

ichange = 0

case event.type of
    
    1: begin                    ; Release event
        
        case event.release of
            1: begin            ; Left: add a line
                if (pdefs.ntext eq 0) then posit = xy $
                else case pdefs.text_options.norm of
                    0: posit = xy
                    1: posit = xyn
                    2: posit = xyf
                endcase
                ichange = graff_text(pdefs, position = posit)
            end
            
            2:if (pdefs.ntext ge 1) then begin
                                ; Centre - edit a text string
                
                xddata = dblarr(pdefs.ntext)
                yddata = dblarr(pdefs.ntext)
                
                locs = where((*pdefs.text).norm eq 0, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /data, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif
                locs = where((*pdefs.text).norm eq 1, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /region, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif
                locs = where((*pdefs.text).norm eq 2, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /frame, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif
                
                dist = sqrt((xddata-event.x)^2 + $
                            (yddata-event.y)^2)
                md = min(dist, imin)
                if (md lt 5) then $
                  ichange = graff_text(pdefs, edit = imin) $
                else graff_msg, pdefs.ids.message, $
                  "No text anchor point within 5 pixels"
        
            endif

            
            4: if (pdefs.ntext ne 0) then begin
                                ; Right: delete.

                xddata = dblarr(pdefs.ntext)
                yddata = dblarr(pdefs.ntext)
                
                locs = where((*pdefs.text).norm eq 0, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /data, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif
                locs = where((*pdefs.text).norm eq 1, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /region, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif
                locs = where((*pdefs.text).norm eq 2, nm)
                if nm ne 0 then begin
                    gr_coord_convert, (*pdefs.text)[locs].x, $
                      (*pdefs.text)[locs].y, xt, yt, /frame, /to_device
                    xddata[locs] = xt
                    yddata[locs] = yt
                endif

                    
                dist = sqrt((xddata-event.x)^2 + $
                            (yddata-event.y)^2)
                md = min(dist, imin)
                
                if (md lt 5) then begin
                    if (pdefs.ntext eq 1) then begin
                        pdefs.ntext = 0
                    
                    endif else begin
                        if (imin eq 0) then (*pdefs.text) = $
                          (*pdefs.text)(1:*) $ 
                          
                        else if (imin eq pdefs.ntext-1) then $
                          (*pdefs.text) = (*pdefs.text)(0:Imin-1) $
                        
                             else (*pdefs.text) = $
                               [(*pdefs.text)(0:Imin-1), $
                                (*pdefs.text)(Imin+1:*)]     
                     
                        pdefs.ntext = pdefs.ntext-1
                    endelse
                    ichange = 1
                endif else begin
                    graff_msg, pdefs.ids.message, $
                      "No text anchor point within 5 pixels"
                endelse
            endif
        endcase
    end
    
    2: begin                    ; Motion events
        gr_coord_convert, event.x, event.y, x, y, /device, /to_norm
        gr_cross_hair, pdefs, [x, y]

        widget_control, pdefs.ids.xcp, set_value = xy(0)
        widget_control, pdefs.ids.ycp, set_value = xy(1)
    end
    
    Else:                       ; Ignore press events and any other
                                ; miscellanea
    
endcase

; Reset the transform.

!y = opy

return, ichange

end   

