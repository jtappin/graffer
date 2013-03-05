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

function Gr_dbl_rd, iunit, nvals, single = single

;+
; GR_DBL_RD
;	Read double value(s) from a binary GRAFFER file
;
; Usage:
;	val = gr_dbl_rd(ilu, nvals)
;
; Return value:
;	val	float	The required float
;
; Arguments:
;	iunit	int	input	The IDL file unit number
;	nvals	int	input	How many?
;
; Keywords:
;	single	If set, then read single precision and convert to
;		double. 
;
;
; History:
;	Original (after gr_flt_rd): 30/6/05; SJT
;	Remove obsolete swap keyword: 6/1/12; SJT
;-

if keyword_set(single) then begin
    if (n_elements(nvals) eq 1 && nvals eq 1) then val = 0. $
    else val = fltarr(nvals)

    readu, iunit, val
    
    return, double(val)
endif

if (n_elements(nvals) eq 1 && nvals eq 1) then val = 0.d0 $
else val = dblarr(nvals)

readu, iunit, val
    
return, val


end
