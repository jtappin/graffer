; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_1dd_plot, pdefs, i, csiz

;+
; GR_1DD_PLOT
;	Plot a 1-D Data in graffer
;
; Usage:
;	gr_1dd_plot, pdefs, i
;
; Argument:
;	pdefs	struct	input	The Graffer control structure.
;	i	int	input	Which 
;	csiz	double	input	Charsize scaling (hardcopy only).
;
; History:
;	Farmed out from GR_PLOT_OBJECT: 10/12/96; SJT
;	Modify for extended symbol definitions: 20/1/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Convert handles to pointers: 27/6/05; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Ignore min & max for polar, but use in errors: 28/7/15; SJT
;-

  data = (*pdefs.data)[i]

  if ~ptr_valid(data.xydata) || data.ndata eq 0 then return
 
  if (data.colour eq -1) then return
  if data.colour eq -2 then $
     lcolour = graff_colours(data.c_vals) $
  else lcolour = graff_colours(data.colour)

  xydata = *data.xydata

  if data.mode eq 0 then begin
     if finite(data.min_val) then minv = data.min_val
     if finite(data.max_val) then maxv = data.max_val
  endif

  if (data.sort) then begin
     js = sort(xydata[0, *])
     xydata = xydata[*, js]
  endif
  if (data.mode eq 2) then pcf = !Dtor $
  else pcf = 1.0

  if (data.pline ne 0) && data.ndata ge 2 then begin
     lps = ([0, 0, 10])[data.pline]
     oplot, xydata[0, 0:data.ndata-1], xydata[1, *]*pcf, color = $
            lcolour, psym = lps, linesty = data.line, $
            thick = data.thick, polar = data.mode, noclip = $
            data.noclip, min_value = minv, max_value = maxv
  endif

  if (data.psym ne 0) then begin
     if (data.psym ge 8) then gr_symdef, data.psym, thick = data.thick
     oplot, xydata[0, 0:data.ndata-1], xydata[1, *]*pcf, color = $
            lcolour, psym = data.psym < 8, thick = $
            data.thick, $
            symsize = abs(data.symsize)*csiz, $
            polar = data.mode, $
            noclip = $
            data.noclip, min_value = minv, max_value = maxv
  endif

  if (data.mode eq 0) then begin ; Separate handling for rectangular &
                                ; polar error plots
     case (data.type) of
        0:                      ;No error bars = no action
        
        1: begin                ; Y
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*csiz*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv
        end
        
        2: begin                ; YY
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     xydata[3, *], width = data.symsize*0.01 > $
                     0, color = lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
        end                    
        
        3: begin                ; X
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
        end
        
        4: begin                ; XX
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     xydata[3, *], width = data.symsize*0.01 > $
                     0, color = lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv
        end                    
        
        5: begin                ; XY
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxvp 
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[3, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
        end
        
        6: begin                ; XYY
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[3, *], xydata[4, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
        end
        
        7: begin                ; XXY
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[4, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip 
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], xydata[3, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip 
        end
        
        8: begin                ; XXYY
           gr_err_x, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], xydata[3, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
           gr_err_y, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[4, *], xydata[5, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
        end
     endcase
  endif else begin
     case (data.type) of
        0:                      ;No error bars = no action
        
        1: begin                ; Y
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[2, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
        end
        
        2: begin                ; YY
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[2, *], $
                      xydata[3, *], width = data.symsize*0.01 > $
                      0, color = lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip 
        end                    
        
        3: begin                ; X
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
        end
        
        4: begin                ; XX
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     xydata[3, *], width = data.symsize*2.0 > $
                     0, color = lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
        end                    
        
        5: begin                ; XY
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[3, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
        end
        
        6: begin                ; XYY
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[3, *], xydata[4, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
        end
        
        7: begin                ; XXY
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[4, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], xydata[3, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
        end
        
        8: begin                ; XXYY
           gr_err_r, xydata[0, 0:data.ndata-1], $
                     xydata[1, *], xydata[2, *], xydata[3, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip
           gr_err_th, xydata[0, 0:data.ndata-1], $
                      xydata[1, *], xydata[4, *], xydata[5, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
        end
     endcase
  endelse

end
