function Gr_int_rd, iunit, nvals

;+
; GR_INT_RD
;	Read int value(s) from a binary GRAFFER file
;
; Usage:
;	val = gr_int_rd(ilu, nvals)
;
; Return value:
;	val	int	The required int
;
; Arguments:
;	iunit	int	input	The IDL file unit number
;	nvals	int	input	How many?
;
;
; History:
;	Original: 15/1/96; SJT
;	Remove obsolete swap keyword: 6/1/12; SJT
;-

if (nvals eq 1) then val = 0 $
else val = intarr(nvals)

readu, iunit, val

return, val

end
