; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_as_yr, data, xrange, xtype, range, visible = visible, positive $
              = positive

;+
; GR_AS_YR
;	Autoscale graffer data (Y-Axis, rectangular plot)
;
; Usage:
;	gr_as_xr, data, xrange, xtype, range
;
; Arguments:
;	data	struct	input	The Graffer data structure (extracted
;				from PDEFS)
;	xrange	double	input	The X- range for functions (y=f(x))
;	xtype	int	input	log or linear X (ditto)
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
;	Change handles to pointers: 27/6/05; SJT
;	Support max & min vals: Apr 16; SJT
;	Add visible key: 31/5/16; SJT
;	Ignore undisplayed datasets: 13/10/16; SJT
;	Add positive keyword: 12/2/18; SJT
;	Typos fixed: 10/9/19; SJT
;	Work around apparent GDL segfault bug: 24/11/20; SJT
;-

; Ignore undisplayed datasets

  if data.type eq 9 || data.type eq -4 then begin
     if data.zopts.format eq 2 then return
  endif else begin
     if data.colour eq -1 then return
  endelse

  fv = 0.                       ; Just create the variable

  case (data.type) of
     -1: begin                  ; y = f(x)
        tmp_r = (*data.xydata).range ; Work around apparent GDL bug
        ;; if ((*data.xydata).range[0] ne (*data.xydata).range[1]) then $
        ;;    begin
        if tmp_r[0] ne tmp_r[1] then begin
           amin = xrange[0] > tmp_r[0] ;(*data.xydata).range[0]
           amax = xrange[1] < tmp_r[1] ;(*data.xydata).range[1]
        endif else begin
           amin = xrange[0]
           amax = xrange[1]
        endelse
        
        if (xtype) then begin
           amin = alog10(amin)
           amax = alog10(amax)
           x = 10^(dindgen(data.ndata) * (amax-amin) $
                   /  double(data.ndata-1) + amin)
        endif else x = dindgen(data.ndata) * (amax-amin) $
                       /  double(data.ndata-1) + amin
        
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if keyword_set(positive) then begin
           fvmn = gr_min_nz(fv, max = fvmx)
           if ~finite(fvmn) || fvmn le 0. then return
           range[0] <= fvmn
        endif else range[0] = range[0] < min(fv, max = fvmx)
        range[1] >= fvmx
     end
     
     -2: begin
        tmp_r = (*data.xydata).range ; Work around apparent GDL bug
        if tmp_r[0] ne tmp_r[1] then begin ;x = F(y)
           range[0] <= tmp_r[0]
           range[1] >= tmp_r[1]
        endif
     end
     
     -3: begin                  ; x = f(t), y = f(t)
        tmp_r = (*data.xydata).range ; Work around apparent GDL bug
        t = dindgen(data.ndata) *  $
            (tmp_r[1]-tmp_r[0]) $
            /  double(data.ndata-1) + tmp_r[0]
        
        iexe = execute('fv = '+(*data.xydata).funct[1])
        
        if keyword_set(positive) then begin
           fvmn = gr_min_nz(fv, max = fvmx)
           if ~finite(fvmn) || fvmn le 0. then return
           range[0] <= fvmn
        endif else range[0] <= min(fv, max = fvmx)
        range[1] >= fvmx
     end
     
     -4: begin
        tmp_r = (*data.xydata).range ; Work around apparent GDL bug

        if tmp_r[0, 1] ne tmp_r[1, 1] then begin ; z = f(x,y)
           range[0] <= tmp_r[0, 1]
           range[1] >= tmp_r[1, 1]
        endif
     end
     
     9: begin                   ; Surface data 
        if keyword_set(positive) then begin
           rgmn = gr_min_nz(*(*data.xydata).y, max = mx)
           if ~finite(rgmn) || rgmn lt 0. then return
           range[0] <= rgmn
        endif else range[0] <= min(*(*data.xydata).y, max = mx)
        range[1] >= mx
     end
     
     Else: begin
        xx = ((*data.xydata).x)[0:data.ndata-1]
        ym = ((*data.xydata).y)[0:data.ndata-1]
        yp = ym
        if ptr_valid((*data.xydata).y_err) then begin
           ye = ((*data.xydata).y_err)[*, 0:data.ndata-1]
           ye and= finite(ye)
           ym -= ye[0, *]
           yp += ye[-1, *]
        endif
      
        if finite(data.max_val) then yp <= data.max_val
        if finite(data.min_val) then ym >= data.min_val

        if keyword_set(visible) then $
           locs = where(finite(ym) and $
                        xx ge xrange[0] and xx le xrange[1], nf) $
        else locs = where(finite(ym), nf)
        if (nf gt 0) then begin
           if keyword_set(positive) then begin
              rgmin = gr_min_nz(ym[locs])
              if finite(rgmin) && rgmin gt 0. then $
                 range[0] <= rgmin
           endif else range[0] <= min(ym[locs])
        endif

        if keyword_set(visible) then $
           locs = where(finite(yp) and $
                        xx ge xrange[0] and xx le xrange[1], nf) $
        else locs = where(finite(yp), nf)
        if (nf gt 0) then begin
           rgmax = max(yp[locs])
           if keyword_set(positive) && rgmax le 0. then return
           range[1] >= rgmax
        endif
     end
     
  endcase

end
