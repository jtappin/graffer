; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function graff_colours, index, triple = triple, max_index = max_index

;+
; GRAFF_COLOURS
;	Compute a colour value for graffer.
;
; Usage:
;	col = graff_colours(index)	; Not indended for use by the user.
;
; Returns:
;	A 32-bit integer decomposed colour for plotting.
;
; Argument:
;	index	int/byt	Either an integer colour index or a 3-element
;			colour triple.
;
; Keyword:
;	/triple		If set, then return a colour triplet for use
;			in image display.
;	/max_index	If set, then return the highest recognized
;			colour index
;
; History:
;	Original: 2/8/95; SJT
;	Rename as GRAFF_COLOURS (was s_colours): 18/9/96; SJT
;	Don't do extended colour table if not enough colours: 8/5/97; SJT
;	Extend discrete colours: 8/2/12; SJT
;	Essentially new routine for decom colours: 17/5/16; SJT
;	Add TRIPLE keyword: 23/8/16; SJT
;	Allow a long to be converted to a triple, not sure if it will
;	work for big-endian boxes: 1/3/19; SJT
;-

  cmap =  [[255l, 0l, 255l, 0l, 0l, 0l, 255l, 255l, 255l, 127l, $
            0l, 0l, 127l, 255l, 85l, 170l, 170l, 255l, 0l, 85l, $
            0l, 85l, 0l, 85l, 170l, 255l, 170l, 255l], $ ; Red 
           [255l, 0l, 0l, 255l, 0l, 255l, 0l, 255l, 127l, 255l, $ $
            255l, 127l, 0l, 0l, 85l, 170l, 0l, 85l, 170l, 255l, 0l, $
            85l, 170l, 255l, 0l, 85l, 170l, 255l], $ ; Green
           [255l, 0l, 0l, 0l, 255l, 255l, 255l, 0l, 0l, 0l, 127l, $
            255l, 255l, 127l, 85l, 170l, 0l, 85l, 0l, 85l, 170l, $
            255l, 170l, 255l, 170l, 255l, 0l, 85l]] ; Blue

  sz = size(cmap, /dim)
  imax = sz[0]

  if keyword_set(max_index) then return, imax-1
  
  if n_elements(index) eq 1  then begin
     
     if size(index, /type) eq 13 && keyword_set(triple) then begin
        rgb = byte(index, 0, 3)
        return, rgb
     endif 

     if index lt 0 || index ge imax then begin
        if ~keyword_set(triple) then return, index
;        rgb = byte(index, 0, 3)
        return, 0ul
     endif 
     
     if keyword_set(triple) then return, byte(reform(cmap[index, *])) $
     else return, ulong(cmap[index, 0]) + $
                  cmap[index, 1]*256ul + $
                  cmap[index, 2]*256ul^2
  endif else if n_elements(index) eq 3 then begin
     if keyword_set(triple) then return, byte(index)
     sindex = ulong(byte(index))
     return, sindex[0] + sindex[1]*256ul + sindex[2]*256ul^2
  endif else return, 0ul
end
