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

pro Gr_as_xr, data, yrange, ytype, range

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
;	yrange	float	input	The Y- range for functions (x=f(y))
;	ytype	int	input	log or linear Y (ditto)
;	range	float	in/out	The range to use.
;
; History:
;	Extracted from GR_AUTOSCALE: 16/12/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;-

fv = 0.                         ; Just create the variable

case (data.type) of
    -1: if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then $
      begin                     ;Y = F(x)
        range(0) = range(0) < (*data.xydata).range(0)
        range(1) = range(1) > (*data.xydata).range(1)
    endif
    
    -2: begin                   ; X = f(y)
        if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then begin
            amin = yrange(0) > (*data.xydata).range(0)
            amax = yrange(1) < (*data.xydata).range(1)
        endif else begin
            amin = yrange(0)
            amax = yrange(1)
        endelse
        if (ytype) then begin
            amin = alog10(amin)
            amax = alog10(amax)
            y = 10^(dindgen(data.ndata) * (amax-amin) $
                    /  float(data.ndata-1) + amin)
        endif else y = dindgen(data.ndata) * (amax-amin) $
          /  float(data.ndata-1) + amin
        
        iexe = execute('fv = '+(*data.xydata).funct)
        
        range(0) = range(0) < min(fv, max = fvmx)
        range(1) = range(1) > fvmx
    end
    
    -3: begin                   ; x = f(t), y = f(t)
        t = dindgen(data.ndata) *  $
          ((*data.xydata).range(1)-(*data.xydata).range(0)) $
          /  float(data.ndata-1) + (*data.xydata).range(0)
        
        iexe = execute('fv = '+(*data.xydata).funct(0))
        
        range(0) = range(0) < min(fv, max = fvmx)
        range(1) = range(1) > fvmx
    end
    
    -4: if ((*data.xydata).range(0, 0) ne (*data.xydata).range(1, 0)) then $
      begin                     ; z = f(x,y)
        range(0) = range(0) < (*data.xydata).range(0, 0)
        range(1) = range(1) > (*data.xydata).range(1, 0)
    endif
    
    9: begin                    ; Surface data 
        range(0) = range(0) < min(*(*data.xydata).x, max = mx)
        range(1) = range(1) > mx
    end
    
    Else: begin                 ; XY data, much easier (or it was)
        if (data.type le 2) then begin
            xp = (*data.xydata)(0, 0:data.ndata-1)
            xm = (*data.xydata)(0, 0:data.ndata-1)
        endif else if (data.type eq 3 or $
                       data.type eq 5 or $
                       data.type eq 6) then begin
            xp = (*data.xydata)(0, 0:data.ndata-1) +  $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
            xm = (*data.xydata)(0, 0:data.ndata-1) - $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
        endif else begin
            xp = (*data.xydata)(0, 0:data.ndata-1) +  $
              (finite((*data.xydata)(3, 0:data.ndata-1)) and $
               (*data.xydata)(3, 0:data.ndata-1))
            xm = (*data.xydata)(0, 0:data.ndata-1) - $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
        endelse
        
        locs = where(finite(xm), nf)
        if (nf gt 0) then range(0) = range(0) < min(xm(locs))
        locs = where(finite(xp), nf)
        if (nf gt 0) then range(1) = range(1) > max(xp(locs))
        
    end
endcase

end
