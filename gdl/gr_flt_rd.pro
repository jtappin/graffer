function Gr_flt_rd, iunit, nvals

;+
; GR_FLT_RD
;	Read float value(s) from a binary GRAFFER file
;
; Usage:
;	val = gr_flt_rd(ilu, nvals)
;
; Return value:
;	val	float	The required float
;
; Arguments:
;	iunit	int	input	The IDL file unit number
;	nvals	int	input	How many?
;
; Keywords:
;	swap	If set, then the file is byte-swapped relative to the
;		platform
;
; History:
;	Original: 15/1/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Remove obsolete swap keyword: 6/1/12; SJT
;-

if (nvals eq 1) then val = 0. $
else val = fltarr(nvals)

readu, iunit, val

return, val

end
