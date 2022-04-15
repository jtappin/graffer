; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_as_xr, data, yrange, ytype, range, visible = visible, positive $
              = positive

;+
; GR_AS_XR
;	Autoscale graffer data (X-Axis, rectangular plot)
;
; Usage:
;	gr_as_xr, data, range
;
; Arguments:
;	data	struct	input	The Graffer data structure (extracted
;				from PDEFS)
;	yrange	double	input	The Y- range for functions (x=f(y))
;                                      (or data when visible is set)
;	ytype	int	input	log or linear Y (ditto)
;	range	double	in/out	The range to use.
;
; Keyword:
; 	/visible	If set, then only consider data values that
; 			lie within yrange.
; 	/positive	If set then scale to positive values only (for
; 			log plots).
;
; History:
;	Extracted from GR_AUTOSCALE: 16/12/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;	Add visible key: 31/5/16; SJT
;	Ignore undisplayed datasets: 13/10/16; SJT
;	Add positive keyword: 12/2/18; SJT
;-

; Ignore undisplayed datasets

  if data.type eq 9 || data.type eq -4 then begin
     if data.zopts.format eq 2 then return
  endif else begin
     if data.colour eq -1 then return
  endelse

  fv = 0.                       ; Just create the variable

  case (data.type) of
     -1: begin
        tmp_r = (*data.xydata).range
        if (tmp_r[0] ne tmp_r[1]) then begin                   ;Y = F(x)
           range[0] <= tmp_r[0]
           range[1] >= tmp_r[1]
        endif
     end
     
     -2: begin                  ; X = f(y)
        tmp_r = (*data.xydata).range
        if (tmp_r[0] ne tmp_r[1]) then begin
           amin = yrange[0] > tmp_r[0]
           amax = yrange[1] < tmp_r[1]
        endif else begin
           amin = yrange[0]
           amax = yrange[1]
        endelse
        if (ytype) then begin
           amin = alog10(amin)
           amax = alog10(amax)
           y = 10^(dindgen(data.ndata) * (amax-amin) $
                   /  double(data.ndata-1) + amin)
        endif else y = dindgen(data.ndata) * (amax-amin) $
                       /  double(data.ndata-1) + amin
        
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if keyword_set(positive) then begin
           fvmn = gr_min_nz(fv, max = fvmx) 
           if ~finite(fvmn) ||  fvmn le 0. then return
           range[0] = range[0] <  fvmn
        endif else range[0] = range[0] < min(fv, max = fvmx)
        range[1] = range[1] > fvmx
     end
     
     -3: begin                  ; x = f(t), y = f(t)
        tmp_r = (*data.xydata).range
        t = dindgen(data.ndata) *  $
            (tmp_r[1]-tmp_r[0]) $
            /  double(data.ndata-1) + tmp_r[0]
        
        iexe = execute('fv = '+(*data.xydata).funct[0])
        
        if keyword_set(positive) then begin
           fvmn = gr_min_nz(fv, max = fvmx)
           if ~finite(fvmn) ||  fvmn le 0. then return
           range[0] = range[0] < fvmn
        endif else range[0] = range[0] < min(fv, max = fvmx)
        range[1] = range[1] > fvmx
     end
     
     -4: begin
        tmp_r = (*data.xydata).range
        if tmp_r[0, 0] ne tmp_r[1, 0] then begin ; z = f(x,y)
           range[0] <= tmp_r[0, 0]
           range[1] >= tmp_r[1, 0]
        endif
     end
     
     9: begin                   ; Surface data 
        if keyword_set(positive) then begin
           rgmn = gr_min_nz(*(*data.xydata).x, max = mx)
           if ~finite(rgmn) || rgmn lt 0. then return
           range[0] <= rgmn
        endif else range[0] <= min(*(*data.xydata).x, max = mx)
        range[1] >= mx
     end
     
     Else: begin                ; XY data, much easier (or it was)
        yy = ((*data.xydata).y)[0:data.ndata-1]
        xm = ((*data.xydata).x)[0:data.ndata-1]
        xp = xm
        if ptr_valid((*data.xydata).x_err) then begin
           xe = ((*data.xydata).x_err)[*, 0:data.ndata-1]
           xe and= finite(xe)
           xm -= xe[0, *]
           xp += xe[-1, *]
        endif
      
        if keyword_set(visible) then $
           locs = where(finite(xm) and $
                        yy ge yrange[0] and yy le yrange[1], nf) $
        else locs = where(finite(xm), nf)
        if (nf gt 0) then begin
           if keyword_set(positive) then begin
              rgmin = gr_min_nz(xm[locs])
              if finite(rgmin) && rgmin gt 0. then $
                 range[0] = range[0] < rgmin
           endif else range[0] = range[0] < min(xm[locs])
        endif

        if keyword_set(visible) then $
           locs = where(finite(xp) and $
                        yy ge yrange[0] and yy le yrange[1], nf) $
        else locs = where(finite(xp), nf)
        if (nf gt 0) then begin
           rgmax = max(xp[locs])
           if keyword_set(positive) && rgmax le 0. then return
           range[1] = range[1] > rgmax
        endif
     end
  endcase

end
