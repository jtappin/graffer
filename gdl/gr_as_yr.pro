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

pro Gr_as_yr, data, xrange, xtype, range

;+
; GR_AS_YR
;	Autoscale graffer data (Y-Axis, rectangular plot)
;
; Usage:
;	gr_as_xr, data, range
;
; Arguments:
;	data	struct	input	The Graffer data structure (extracted
;				from PDEFS)
;	xrange	float	input	The X- range for functions (y=f(x))
;	xtype	int	input	log or linear X (ditto)
;	range	float	in/out	The range to use.
;
; History:
;	Extracted from GR_AUTOSCALE: 16/12/96; SJT
;	Change handles to pointers: 27/6/05; SJT
;-

fv = 0.                         ; Just create the variable

case (data.type) of
    -1: begin                   ; y = f(x)
        if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then begin
            amin = xrange(0) > (*data.xydata).range(0)
            amax = xrange(1) < (*data.xydata).range(1)
        endif else begin
            amin = xrange(0)
            amax = xrange(1)
        endelse
        if (xtype) then begin
            amin = alog10(amin)
            amax = alog10(amax)
            x = 10^(dindgen(data.ndata) * (amax-amin) $
                    /  float(data.ndata-1) + amin)
        endif else x = dindgen(data.ndata) * (amax-amin) $
          /  float(data.ndata-1) + amin
        
        iexe = execute('fv = '+(*data.xydata).funct)
        
        range(0) = range(0) < min(fv, max = fvmx)
        range(1) = range(1) > fvmx
    end
    
    -2: if ((*data.xydata).range(0) ne (*data.xydata).range(1)) then begin ;x = F(y)
        range(0) = range(0) < (*data.xydata).range(0)
        range(1) = range(1) > (*data.xydata).range(1)
    endif
    
    -3: begin                   ; x = f(t), y = f(t)
        t = dindgen(data.ndata) *  $
          ((*data.xydata).range(1)-(*data.xydata).range(0)) $
          /  float(data.ndata-1) + (*data.xydata).range(0)
        
        iexe = execute('fv = '+(*data.xydata).funct(1))
        
        range(0) = range(0) < min(fv, max = fvmx)
        range(1) = range(1) > fvmx
    end
    
    -4: if ((*data.xydata).range(0, 1) ne (*data.xydata).range(1, 1)) then $
      begin                     ; z = f(x,y)
        range(0) = range(0) < (*data.xydata).range(0, 1)
        range(1) = range(1) > (*data.xydata).range(1, 1)
    endif
    
    9: begin                    ; Surface data 
        range(0) = range(0) < min(*(*data.xydata).y, max = mx)
        range(1) = range(1) > mx
    end
    
    Else: begin
        if (data.type eq 0 or $
            data.type eq 3 or $
            data.type eq 4) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1)
            ym = (*data.xydata)(1, 0:data.ndata-1)
            
        endif else if (data.type eq 1) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
            
        endif else if (data.type eq 2) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(3, 0:data.ndata-1)) and $
               (*data.xydata)(3, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(2, 0:data.ndata-1)) and $
               (*data.xydata)(2, 0:data.ndata-1))
            
        endif else if (data.type eq 5) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(3, 0:data.ndata-1)) and $
               (*data.xydata)(3, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(3, 0:data.ndata-1)) and $
               (*data.xydata)(3, 0:data.ndata-1))
            
        endif else if (data.type eq 6) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(4, 0:data.ndata-1)) and $
               (*data.xydata)(4, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(3, 0:data.ndata-1)) and $
               (*data.xydata)(3, 0:data.ndata-1))
            
        endif else if (data.type eq 7) then begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(4, 0:data.ndata-1)) and $
               (*data.xydata)(4, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(4, 0:data.ndata-1)) and $
               (*data.xydata)(4, 0:data.ndata-1))
            
        endif else  begin
            yp = (*data.xydata)(1, 0:data.ndata-1) + $
              (finite((*data.xydata)(5, 0:data.ndata-1)) and $
               (*data.xydata)(5, 0:data.ndata-1))
            ym = (*data.xydata)(1, 0:data.ndata-1) - $
              (finite((*data.xydata)(4, 0:data.ndata-1)) and $
               (*data.xydata)(4, 0:data.ndata-1))
        endelse
        
        locs = where(finite(ym), nf)
        if (nf gt 0) then range(0) = range(0) < min(ym(locs))
        locs = where(finite(yp), nf)
        if (nf gt 0) then range(1) = range(1) > max(yp(locs))
    end
    
endcase

end
