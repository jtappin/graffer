; LICENCE:
; Copyright (C) 2012-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function match_in, data, pattern, tolerance = tolerance

;+
; MATCH_IN
;	Find those elements of data that are in pattern
;
; Usage:
;	imatch=match_in(data, pattern)
;
; Returns:
;	A byte array of the same shape as data, with 1 for matched
;	values. 0 for unmatched
;
; Arguments:
;	data	any	The array to be checked, may be any type that
;			can compare equal
;	pattern	any	The array of test values.
;
;
; Keyword:
;	Tolerance	Set a tolerance for considering floating point
;			values equal.
;
; Example:
;	In place of:
;		locs = where(a gt 0. and (x eq 5 or x eq 7 or x eq 15))
;	one could use:
;		locs = where(a gt 0 and match_in(x, [5,7,15]))
;
; History:
;	Original: 31/1/12; SJT
;-

iuse = bytarr(size(data, /dimension))

if keyword_set(tolerance) then  $
  for j = 0l, n_elements(pattern)-1 do $
  iuse =  iuse or (abs(data - pattern[j]) lt tolerance) $
else for j = 0l, n_elements(pattern)-1 do $
  iuse =  iuse or (data eq pattern[j])


return, iuse

end
