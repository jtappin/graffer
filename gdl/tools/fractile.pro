; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Fractile, x, frac, nan = nan, dimension = dimension

;+
; FRACTILE
;	Return the requested fractile of the input data.
;
; Usage:
;	fr = fractile(x, frac)
;
; Return:
;	fr	<input>	The requested fractile.
;
; Arguments:
;	x	most	input	The array whose fractile(s) are to be
;				returned 
;	frac	float	input	The fractile(s) to return.
;
; Keywords:
;	/nan		If set on not specified, then treat IEEE NaN
;			values as missing data, it is inadvisable to
;			use fractile on data containing NaN values
;			without this keyword. (Now need to set it
;			explicitly to zero to obtain old behaviour).
;	dimension int	If set, then make the fractile calculation
;			along the specified dimension only.
;
; Restrictions:
;	The input data must be a SORTable array (i.e. not complex,
;	string or structure).
;
; Example:
;	To find the interquartile range of a data set, try:
;	q = fractile(data, [.25,.75])
;	iqr = q(1)-q(0)
;	
; History:
;	Original: 26/9/95; SJT
;	Modify to interpolate: 4/2/97; SJT
;	Added DIMENSION keyword: 21/9/07; SJT
;	Make ignoring NaN values default: 8/8/15; SJT
;-

if (n_params() ne 2) then message, 'Incorrect number of arguments'
enan = n_elements(nan) eq 0 || keyword_set(nan)

sx = size(x)

if ~keyword_set(dimension) or sx[0] le 1 then begin
    if enan then n = long(total(finite(x)))-1 $
    else n = n_elements(x)-1

    i = sort(x)

; One point or no valid points will break things if they're not
; handled specially.
    if n eq 0 then return, replicate(x[i[0]], n_elements(frac))
    if n eq -1 then return, replicate(!values.f_nan, n_elements(frac))

    f0 = floor(frac*n) < (n-1)
    w1 = frac*n - f0
    f1 = floor(frac*n+1) < n
    w0 = f1 - frac*n
    rv = x[i[f0]]*w0 + x[i[f1]]*w1
    locs = where(w1 eq 0, nh)
    if nh ne 0 then rv[locs] = x[i[f0[locs]]]
    return, rv
endif else begin

    if dimension le 0 or dimension gt sx[0] then message, "Invalid " + $
      "dimension specified"

; Permute the array to make the calculating dimension become the last

    di = indgen(sx[0])
    dl = where(di eq (dimension-1), comp = dn)
    dd = [di[dn], di[dl]]

    xx = transpose(x, dd)
    ndims = [product(sx[dn+1]), sx[dl+1]]
    xx = reform(xx, ndims, /overwrite)
    rv = make_array(type = sx[sx[0]+1], ndims[0], n_elements(frac))
    for j = 0l, ndims[0]-1 do rv[j, *] = fractile(xx[j, *], frac, nan = $
                                               nan)
    rv = reform(rv, [sx[dn+1], n_elements(frac)], /over)
    return, rv
endelse

end
