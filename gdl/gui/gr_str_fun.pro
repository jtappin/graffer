; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_str_fun, fit, var, pieces=pieces

;+
; GR_STR_FUN
;	Given a set of coefficients and a variable return an IDL
;	function
; 
; Usage:
;	fun = gr_str_fun(fit, var)
;
; Return value:
;	fun	string	The polynomial as a string suitable for
;			EXECUTE
;
; Arguments:
;	fit	float	input 	An array of polynomial coefficients
;	var	string	input	The variable of which this is a
;				polynomial
;
; Keyword:
;	pieces	??	input	If set & non-zero then fit is in terms
;				of the piecewise linear fit.
;
; History:
;	Original: 22/11/96; SJT
;	Add PIECES: 4/2/97; SJT
;-


stfit = strtrim(string(fit), 2)
if (keyword_set(pieces)) then begin
    nc = string(n_elements(fit)-1, format = "(I0)")
    
    fun = 'gr_pieces('+var+', [' + $
      string(stfit, format = "("+nc+"(a,', '),a)") + '])'
        
endif else begin
    
    locs = where(fit gt 0, np)
    if (np ne 0) then stfit(locs) = '+'+stfit(locs)
    
    fun = stfit(0)
    if (n_elements(fit) gt 1) then fun = fun + ' ' + stfit(1)+'*'+var
    
    for j = 2, n_elements(fit)-1 do $
      fun = fun + ' ' + stfit(j)+'*'+var+'^'+string(j, format = '(I0)')
endelse

return, fun

end
