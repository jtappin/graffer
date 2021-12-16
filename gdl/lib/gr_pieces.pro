; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_pieces, x, a

;+
; GR_PIECES
;	A piecewise straight line routine. 
;
; Usage:
;	y = gr_pieces(x, a)
;
; Return Value:
;	y	float	The value of the function at the given X
;			values.
;
; Arguments:
;	x	float	input	The values at which the function is required
;	a	float	input	The coeffieients of the function as
;				explained below
;				a(0) = y intercept of first segment
;				a(1) = slope of first segment
;				a(2(n-1)) = Start X of nth segment
;				a(2(n-1)+1) = Slope of nth segment
;
; History:
;	Original: 4/2/97; SJT
;-

n_segs = n_elements(a)/2

breaks = a(indgen(n_segs-1)*2 + 2)
slopes = a(indgen(n_segs)*2 + 1)
intercepts = dblarr(n_segs)

intercepts(0) = a(0)
for j = 1, n_segs-1 do begin
    yb = intercepts(j-1) + slopes(j-1) * breaks(j-1)
    intercepts(j) = yb - slopes(j)*breaks(j-1)
endfor

y = intercepts(0) + slopes(0)*x

for j = 0, n_segs-2 do begin
    locs = where(x gt breaks(j), nover)
    if (nover eq 0) then goto, done
    y(locs) = intercepts(j+1) + slopes(j+1)*x(locs)
endfor

Done:
    
return, y

end
