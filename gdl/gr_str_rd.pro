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

function Gr_str_rd, iunit

;+
; GR_STR_RD
;	Read a string value from a binary GRAFFER file
;
; Usage:
;	str = gr_str_rd(ilu)
;
; Return value:
;	str	string	The required string
;
; Argument:
;	iunit	int	input	The IDL file unit number
;
;
; History:
;	Original: 15/1/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Add array support: 23/6/97; SJT
;	Swap obsolete: 6/1/12; SJT
;-

ll = 0l
readu, iunit, ll

if (ll gt 0) then begin
    stg = bytarr(ll)
    readu, iunit, stg
    return, string(stg)
endif else if (ll lt 0) then begin
    nmx = abs(ll)-1
    rv = strarr(nmx+1)
    for j = 0, nmx do begin
        readu, iunit, ll
        if (ll gt 0) then begin
            stg = bytarr(ll)
            readu, iunit, stg
            rv(j) = string(stg)
        endif
    endfor
    return, rv
endif else return, ''

end
