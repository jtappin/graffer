; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

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
