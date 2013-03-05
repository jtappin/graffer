; Copyright (C) 2013
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

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
;-

for j = 0, pdefs.nsets-1 do begin
    if (*pdefs.data)[j].type eq 9 then ptr_free, $
      (*(*pdefs.data)[j].xydata).x, (*(*pdefs.data)[j].xydata).y, $
      (*(*pdefs.data)[j].xydata).z
    ptr_free, (*pdefs.data)[j].xydata
    ptr_free, (*pdefs.data)[j].zopts.levels, $
      (*pdefs.data)[j].zopts.style, $
      (*pdefs.data)[j].zopts.thick, $
      (*pdefs.data)[j].zopts.colours

endfor

ptr_free, pdefs.data
ptr_free, pdefs.text
ptr_free, pdefs.key.list
ptr_free, pdefs.remarks

end
