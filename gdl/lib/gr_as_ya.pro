; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_as_ya, data, xrange, yrange, range, visible = visible

;+
; GR_AS_YA
;	Autoscale graffer data (Y-Axis, Polar plot)
;
; Usage:
;	gr_as_ya, data, range
;
; Arguments:
;	data	struct	input	The Graffer data structure (extracted
;				from PDEFS)
;	xrange	double	input	The x- range of plot for functions th=f(r))
;	yrange	double	input	The theta- range for functions (r=f(th))
;	range	double	in/out	The range to use.
;
; History:
;	Extracted from GR_AUTOSCALE: 16/12/96; SJT
;	Change handles to pointers: 27/6/05; SJT
;	Add visible key: 31/5/16; SJT
;	Ignore undisplayed datasets: 13/10/16; SJT
;	Work around apparent GDL segfault bug: 24/11/20; SJT
;-

; Ignore undisplayed datasets

  if data.type eq 9 || data.type eq -4 then begin
     if data.zopts.format eq 2 then return
  endif else begin
     if data.colour eq -1 then return
  endelse


  maxrange = sqrt(max(xrange^2)+max(yrange^2))

  case (data.type) of
     -1: begin                  ; th = f(r)
        tmp_r = (*data.xydata).range
        if (tmp_r[0] ne tmp_r[1]) then begin
           amin = tmp_r[0]
           amax = tmp_r[1] < maxrange
        endif else begin
           amin = 0
           amax = maxrange
        endelse
        
        x = dindgen(data.ndata) * (amax-amin) $
            /  double(data.ndata-1) + amin
        
        fv = 0.
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if (data.mode eq 2) then fv = fv*!Dtor
        gr_pol_rect, x, fv, xx, yy
        
     end
     
     -2: begin                  ; X = f(y)
        tmp_r = (*data.xydata).range
        if (tmp_r[0] ne tmp_r[1]) then begin
           amin = tmp_r[0]
           amax = tmp_r[1]
        endif else begin
           amin = 0.
           if (data.mode eq 2) then amax = 360. $
           else amax = 2.*!pi
        endelse
        
        y = dindgen(data.ndata) * (amax-amin) $
            /  double(data.ndata-1) + amin
        
        fv = 0.
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if (data.mode eq 2) then y = y*!Dtor
        gr_pol_rect, fv, y, xx, yy
        
     end
     
     -3: begin                  ; x = f(t), y = f(t)
        tmp_r = (*data.xydata).range
        t = dindgen(data.ndata) *  $
            (tmp_r[1]-tmp_r[0]) $
            /  double(data.ndata-1) + tmp_r[0]
        
        fr = 0.
        ft = 0.
        iexe = execute('fr = '+(*data.xydata).funct[0])
        iexe = execute('ft = '+(*data.xydata).funct[1])
        
        if (data.mode eq 2) then ft = ft*!Dtor
        gr_pol_rect, fr, ft, xx, yy
     end
     
     -4: return
     9: return
     
     Else: begin                ; XY data, much easier (or it was)
        
        gr_ang_pts, (*data.xydata), data.ndata, data.type, r, t
        
        if (data.mode eq 2) then t = t*!Dtor
        
        gr_pol_rect, r, t, xx, yy
        
     end
  endcase
  if keyword_set(visible) then begin
     locs = where(xx ge xrange[0] and xx le xrange[1], nfe)
     if nfe ne 0 then begin
        range[0] = range[0] < min(yy[locs], max = mx, /nan)
        range[1] = range[1] > mx
     endif
  endif else begin
     range[0] = range[0] < min(yy, max = mx, /nan)
     range[1] = range[1] > mx
  endelse
end
