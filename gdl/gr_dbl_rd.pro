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
