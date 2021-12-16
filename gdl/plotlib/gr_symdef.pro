; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_symdef, index, thick = thick

;+
; GR_SYMDEF
;	User USERSYM to define the GRAFFER symbols that aren't part of
;	the standard IDL set.
;
; Usage:
;	gr_symdef, index
;
; Argument:
;	index	int	input	The symbol index (8->n)
;					8 - circle
;					9 - filled diamond
;					10 - filled triangle
;					11 - filled square
;					12 - filled circle
;					13 - inverted triangle
;					14 - filled inverted triangle
;					15 - hexagon
;					16 - filled hexagon
;					17 - horizontal bar
;					18 - vertical bar.
;					any other value "?"
;
; History:
;	Original: Jan 97; SJT
;	Add hexagon: Oct 16; SJT
;	Add thick keyword: 29/11/16; SJT
;	Add bars: 25/5/17; SJT
;-

  case index of
     8: begin
        th = dindgen(31)*12*!Dtor
        x = cos(th)
        y = sin(th)
        ifill = 0
     end
     9: begin
        x = [0., 1., 0., -1.]
        y = [-1., 0., 1., 0.]
        ifill = 1
     end
     10: begin
        x = [-1., 1., 0., -1]
        y = [-1., -1., 1., -1]
        ifill = 1
     end
     11: begin
        x = [-1., 1., 1., -1.]
        y = [-1., -1., 1., 1.]
        ifill = 1
     end
     12: begin
        th = dindgen(30)*12*!Dtor
        x = cos(th)
        y = sin(th)
        ifill = 1
     end
     13: begin
        x = [-1., 1., 0., -1]
        y = [1., 1., -1., 1]
        ifill = 0
     end
     14: begin
        x = [-1., 1., 0., -1]
        y = [1., 1., -1., 1]
        ifill = 1
     end
     15: begin
        th = dindgen(7) * 60. * !dtor
        x = cos(th)
        y = sin(th)
        ifill = 0
     end
     16: begin
        th = dindgen(6) * 60. * !dtor
        x = cos(th)
        y = sin(th)
        ifill = 1
     end
     17: begin
        x = [-1., 1.]
        y = [0., 0.]
        ifill = 0
     end
     18: begin
        x = [0., 0.]
        y = [-1., 1.]
        ifill = 0
     end

     Else: begin
        x = [0., 0., .8, .8, .6, -.6, -.8]
        y = [-1., -.2, .2, .7, .9, .9, .7]
        ifill = 0
     end
  endcase

  if ifill eq 0 && keyword_set(thick) then ithick = thick
  usersym, fill = ifill, x, y, thick = ithick

end
