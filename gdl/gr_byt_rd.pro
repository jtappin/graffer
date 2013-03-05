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

function Gr_byt_rd, iunit, nvals

;+
; GR_BYT_RD
;	Read byte value(s) from a binary GRAFFER file
;
; Usage:
;	val = gr_byt_rd(ilu, nvals)
;
; Return value:
;	val	byte	The required byte
;
; Arguments:
;	iunit	int	input	The IDL file unit number
;	nvals	int	input	How many?
;
; History:
;	Original: 15/1/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;-

if (nvals eq 1) then val = 0b $
else val = bytarr(nvals)

readu, iunit, val

return, val

end
