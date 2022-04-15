; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Graff_clear, pdefs

;+
; GRAFF_CLEAR
;	Release memory from handles in a Graffer plot structure
;
; Usage:
;	graff_clear,pdefs
;
; Argument:
;	pdefs	struct	in/out	The  graffer control structure
;
; Note:
;	The structure is not usable after clearing until
;	re-initialised
;
; History:
;	Extracted from GRAFF_EVENT: 18/8/95; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Update 1-D format: 14/4/22; SJT
;-

  for j = 0, pdefs.nsets-1 do begin
     if (*pdefs.data)[j].type eq 9 then ptr_free, $
        (*(*pdefs.data)[j].xydata).x, (*(*pdefs.data)[j].xydata).y, $
        (*(*pdefs.data)[j].xydata).z $
     else if (*pdefs.data)[j].type ge 0 then ptr_free, $
        (*(*pdefs.data)[j].xydata).x, (*(*pdefs.data)[j].xydata).y, $
        (*(*pdefs.data)[j].xydata).x_err, $
        (*(*pdefs.data)[j].xydata).y_err
     
     ptr_free, (*pdefs.data)[j].xydata
     ptr_free, (*pdefs.data)[j].zopts.levels, $
               (*pdefs.data)[j].zopts.style, $
               (*pdefs.data)[j].zopts.thick, $
               (*pdefs.data)[j].zopts.colours, $
               (*pdefs.data)[j].zopts.raw_colours

  endfor

  ptr_free, pdefs.data
  ptr_free, pdefs.text
  ptr_free, pdefs.key.list
  ptr_free, pdefs.remarks

end
