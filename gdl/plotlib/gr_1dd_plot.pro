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
; 	Internal data restructure: 19/4/22; SJT
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
     js = sort(*xydata.x)
     *xydata.x = (*xydata.x)[js]
     *xydata.y = (*xydata.y)[js]
     if ptr_valid(xydata.x_err) then $
        *xydata.x_err = (*xydata.x_err)[*, js]
     if ptr_valid(xydata.y_err) then $
        *xydata.y_err = (*xydata.y_err)[*, js]
  endif
  
  if (data.mode eq 2) then pcf = !Dtor $
  else pcf = 1.0

  if (data.pline ne 0) && data.ndata ge 2 then begin
     lps = ([0, 0, 10])[data.pline]
     oplot, (*xydata.x)[0:data.ndata-1], (*xydata.y)*pcf, color = $
            lcolour, psym = lps, linesty = data.line, $
            thick = data.thick, polar = data.mode, noclip = $
            data.noclip, min_value = minv, max_value = maxv
  endif

  if (data.psym ne 0) then begin
     if (data.psym ge 8) then gr_symdef, data.psym, thick = data.thick
     oplot, (*xydata.x)[0:data.ndata-1], (*xydata.y)*pcf, color = $
            lcolour, psym = data.psym < 8, thick = $
            data.thick, $
            symsize = abs(data.symsize)*csiz, $
            polar = data.mode, $
            noclip = $
            data.noclip, min_value = minv, max_value = maxv
  endif

  if (data.mode eq 0) then begin ; Separate handling for rectangular &
                                ; polar error plots

     if ptr_valid(xydata.x_err) then begin
        sxe = size(*xydata.x_err, /dim)
        if sxe[0] eq 1 then $
           gr_err_x, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, *xydata.x_err, $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv $
        else $
           gr_err_x, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, (*xydata.x_err)[0, *], $
                     (*xydata.x_err)[1, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
     endif
     if ptr_valid(xydata.y_err) then begin
        sye = size(*xydata.y_err, /dim)
        if sye[0] eq 1 then $
           gr_err_y, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, *xydata.y_err, $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv $
        else $
           gr_err_y, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, (*xydata.y_err)[0, *], $
                     (*xydata.y_err)[1, *], $
                     width = data.symsize*0.01 > 0, color = $
                     lcolour, thick = $
                     data.thick, noclip = data.noclip, $
                     min_value = minv, max_value = maxv 
     endif

     
  endif else begin

     if ptr_valid(xydata.x_err) then begin
        sxe = size(*xydata.x_err, /dim)
        if sxe[0] eq 1 then $
           gr_err_r, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, *xydata.x_err, $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip $
        else $
           gr_err_r, (*xydata.x)[0:data.ndata-1], $
                     *xydata.y, (*xydata.x_err)[0, *], $
                     (*xydata.x_err)[1, *], $
                     width = data.symsize*2.0 > 0, color = $
                     lcolour, thick = $
                     data.thick, mode = data.mode, noclip = data.noclip 
     endif

     if ptr_valid(xydata.y_err) then begin
        sye = size(*xydata.y_err, /dim)
        if sye[0] eq 1 then $
           gr_err_th, (*xydata.x)[0:data.ndata-1], $
                      *xydata.y, *xydata.y_err, $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip $
        else $
           gr_err_th, (*xydata.x)[0:data.ndata-1], $
                      *xydata.y, (*xydata.y_err)[0, *], $
                      (*xydata.y_err)[1, *], $
                      width = data.symsize*0.01 > 0, color = $
                      lcolour, thick = $
                      data.thick, mode = data.mode, noclip = data.noclip
     endif
  endelse

end
