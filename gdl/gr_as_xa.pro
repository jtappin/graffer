; Copyright (C) 2013
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

pro Gr_as_xa, data, xrange, yrange, range

;+
; GR_AS_XA
;	Autoscale graffer data (X-Axis, rectangular plot)
;
; Usage:
;	gr_as_xa, data, range
;
; Arguments:
;	data	struct	input	The Graffer data structure (extracted
;				from PDEFS)
;	xrange	float	input	The x- range for functions (th=f(r))
;	yrange	float	input	The y- range for functions (r=f(th))
;	range	float	in/out	The range to use.
;
; History:
;	Extracted from GR_AUTOSCALE: 16/12/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;-

maxrange = sqrt(max(xrange^2)+max(yrange^2))

case (data.type) of
    -1: begin                   ; th = f(r)
        if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then begin
            amin = (*data.xydata).range(0)
            amax = (*data.xydata).range(1) < maxrange
        endif else begin
            amin = 0.
            amax = maxrange
        endelse
        
        x = dindgen(data.ndata) * (amax-amin) $
          /  float(data.ndata-1) + amin
        
        fv = 0.
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if (data.mode eq 2) then fv = fv*!Dtor
        gr_pol_rect, x, fv, xx, yy
        
        range(0) = range(0) < min(xx, max = mx)
        range(1) = range(1) > mx
    end
    
    -2: begin                   ; X = f(y)
        if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then begin
            amin =  (*data.xydata).range(0)
            amax =  (*data.xydata).range(1)
        endif else begin
            amin = 0.
            if (data.mode eq 2) then amax = 360. $
            else amax = 2.*!pi
         endelse
        
        y = dindgen(data.ndata) * (amax-amin) $
          /  float(data.ndata-1) + amin
        
        fv = 0.
        iexe = execute('fv = '+(*data.xydata).funct)
        
        if (data.mode eq 2) then y = y*!Dtor
        gr_pol_rect, fv, y, xx, yy
        
        range(0) = range(0) < min(xx, max = mx)
        range(1) = range(1) > mx
    end
    
    -3: begin                   ; x = f(t), y = f(t)
        t = dindgen(data.ndata) *  $
          ((*data.xydata).range(1)-(*data.xydata).range(0)) $
          /  float(data.ndata-1) + (*data.xydata).range(0)
        
        fr = 0.
        ft = 0.
        iexe = execute('fr = '+(*data.xydata).funct(0))
        iexe = execute('ft = '+(*data.xydata).funct(1))
        
        if (data.mode eq 2) then ft = ft*!Dtor
        gr_pol_rect, fr, ft, x, y
        range(0) = range(0) < min(x, max = fvmx)
        range(1) = range(1) > fvmx
    end
    
    -4:
    9:
    
    Else: begin                 ; XY data, much easier (or it was)
        
        gr_ang_pts, (*data.xydata), data.ndata, data.type, r, t
        
        if (data.mode eq 2) then t = t*!Dtor
        
        gr_pol_rect, r, t, x, y
        
        range(0) = range(0) < min(x, max = mx)
        range(1) = range(1) > mx
    end
endcase

end
