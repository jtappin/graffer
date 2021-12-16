; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_min_nz, array, mindex, max = max, full = full, _extra = _extra

;+
; GR_MIN_NZ
;	Extract the smallest positive element of an array.
;
; Call:
;	val = gr_min_nz(array[, mindex, max=max])
;
; Return Value:
;	val	any	The smallest positive value or -1 if no positive
;			values.
;
; Argument:
;	array	any	input	The array whose min is needed
;	mindex	long	output	The (first) location in the array at
;				which the min value is found.
;
; Keyword:
;	max	any	output	The maximum value in the array.
;	full		input	If set, then just return the regular
;				minimum (to allow a procedure to
;				choose which by flag)
;
; History:
;	Prototype: 24/1/91; SJT
;	Improve and changes to smallest >0: 22/2/91; SJT
;	Add MAX keyword: 31/3/92; SJT
;	Add optional second argument for location of min value:
;							20/8/93; SJT
;	Add "finite" condition: 6/12/93; SJT
;	Add full key: 2/5/02; SJT
;	Add _extra to pass any extra keys direct to min (e.g. /nan):
;	4/8/04; SJT
;	Copy to GRAFFER and rename to ensure we get this one: 12/2/18; SJT
;-

if keyword_set(full) then begin
    minv = min(array, mindex, max = max, _extra = _extra)
endif else begin
    locs = where(array gt 0 and finite(array))
    if (locs[0] eq -1) then begin
        mindex = -1
        minv = -1l
        max = -1l
    endif else begin
        minv = min(array[locs], mindex, max = max, _Extra = _extra)
        mindex = locs[mindex]
    endelse
endelse

return, minv

end
