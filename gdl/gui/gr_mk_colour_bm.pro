; LICENCE:
; Copyright (C) 2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_MK_COLOUR_BM
;	Generate a colour bitmap of a given colour.
;
; Usage:
;	cbm = gr_mk_colour_bm(col)
;
; Returns:
;	An n x m x 3 byte array containing the bitmap.
;
; Argument:
;	col	int	Either a scalar for a colour index or a
;			3-element array with the RGB components.
;
; Keyword:
;	size	long	Specify the size of the bitmap, default 40,
;			16. (2-element array)
;
; History:
;	Original: 10/9/21; SJT
;-

function gr_mk_colour_bm, col, size = xysize

  case n_elements(col) of
     1: begin
        if col lt 0 || col gt graff_colours(/max_index) then $
           clr = bytarr(3) $
        else  clr = graff_colours(col, /triple)
     end
     3:  clr = byte(col)
     else: message, 'Colour must have 1 or 3 elements.'
  endcase
  case n_elements(xysize) of
     0:  bms = [40, 16, 3]
     1: bms = [xysize, xysize, 3]
     2: bms = [xysize, 3]
     else: bms = [xysize[0:1], 3]
  endcase
  
  bm = bytarr(bms)

  for j = 0, 2 do bm[*, *, j] = clr[j]

  return, bm

end
