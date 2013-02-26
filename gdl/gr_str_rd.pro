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
