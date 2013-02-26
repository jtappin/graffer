function Gr_lon_rd, iunit, nvals

;+
; GR_LON_RD
;	Read long value(s) from a binary GRAFFER file
;
; Usage:
;	val = gr_lon_rd(ilu, nvals)
;
; Return value:
;	val	long	The required long
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
;	Remove obsolete swap key: 6/1/12; SJT
;-

if (nvals eq 1) then val = 0l $
else val = lonarr(nvals)

readu, iunit, val

return, val

end
