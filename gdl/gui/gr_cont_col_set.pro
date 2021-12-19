; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_CONT_COL_SET
;	Convert a list of contour colours to a string array.
;
; Usage:
;	cstr = gr_cont_col_set(clist, rawc)
;
; Returns:
;	A string array with the contour colours, or an empty string.
;
; Argument:
;	clist	list/int A list of contour colours, elements are
;			integer scalars or 3 element byte arrays. Or:
;			an array of coulour indices.
;	rawc	int	A 3xN array of decomposed colour values (if
;			this is present then cstr must be an integer
;			array, and the values from here are used if
;			cstr == -2 .
;
; History:
;	Original: 7/10/16; SJT
;	Handle int arrays: 3/4/20; SJT
;-

function gr_cont_col_set, clist, rawc
  
  ncols = n_elements(clist)
  if ncols eq 0 then return, ''

  rv = strarr(ncols)

  if isa(clist, 'LIST') then begin
     
     for j = 0, ncols-1 do begin
        tmp = clist[j]
        case n_elements(tmp) of
           0: rv[j] = ''
           1: rv[j] = string(tmp, format = "(i0)")
           3: rv[j] = string(tmp, format = "(3i4)")
           2: begin
              rv[j] = string(tmp[0], format = "(i0)")
              message, /cont, $
                       "Invalid 2-element list element, using first " + $
                       "element of it"
           end
           else: begin
              rv[j] = string(tmp[0:2], format = "(3i4)")
              message, /cont, $
                       "Too many elements in list, using first 3"
           end
        endcase
     endfor
  endif else begin
     for j = 0, ncols-1 do begin
        if clist[j] eq -2 && n_params() eq 2 then $
           rv[j] = string(rawc[*, j], format = "(3i4)") $
        else rv[j] = string(clist[j], format = "(i0)")
     endfor
  endelse
  return, rv
  
end
