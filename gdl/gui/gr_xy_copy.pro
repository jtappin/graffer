; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_xy_copy, pdefs, index, force = force

;+
; GR_XY_COPY
;	Copy one XY dataset to another.
;
; Usage:
;	ichange=gr_xy_copy(pdefs, index)
;
; Return value
;	ichange	int	1 if dataset copied, 0 if not
;
; Arguments:
;	pdefs	struct	in/out	The graffer data structure.
;	index	int	input	The index number of the dataset to
;				copy. 
;
; Keyword:
; 	/force	If set then do not prompt before overwriting 2-D data
; 		of functions.
;
; History:
;	Original (after gr_xy_read): 7/2/12; SJT
;-

if ((*pdefs.data)[index].type lt 0 || $
    (*pdefs.data)[index].type eq 9 || $
    ~ptr_valid((*pdefs.data)[index].xydata)) then begin
    junk = dialog_message(['Source dataset is not an XY', $
                           'dataset, aborting'], $
                          dialog_parent = pdefs.ids.graffer, $
                          resource = 'Graffer', $
                          /info)
    return, 0
endif

if ((*pdefs.data)[pdefs.cset].type lt 0 || $
    (*pdefs.data)[pdefs.cset].type eq 9) && ~keyword_set(force) then $
  begin 
    ans = dialog_message(['Current data set is a function', $
                          'or a 2-D dataset, reading 1-D data', $
                          'will overwrite it.', $
                          'Do you really want to do this?'], $
                         /question, $
                         dialog_parent = pdefs.ids.graffer, $
                         resource = 'Graffer')
    if ans eq 'No' then return, 0
endif

if (*pdefs.data)[pdefs.cset].type eq 9 then ptr_free, $
  (*(*pdefs.data)[pdefs.cset].xydata).x, $
  (*(*pdefs.data)[pdefs.cset].xydata).y, $
  (*(*pdefs.data)[pdefs.cset].xydata).z

ptr_free, (*pdefs.data)[pdefs.cset].xydata

(*pdefs.data)[pdefs.cset].xydata = $
  ptr_new(*(*pdefs.data)[index].xydata)

(*pdefs.data)[pdefs.cset].type = (*pdefs.data)[index].type
(*pdefs.data)[pdefs.cset].ndata = (*pdefs.data)[index].ndata
(*pdefs.data)[pdefs.cset].ndata2 = (*pdefs.data)[index].ndata2
(*pdefs.data)[pdefs.cset].mode = (*pdefs.data)[index].mode

return, 1

end
