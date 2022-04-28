; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_z_copy, pdefs, index, force = force

;+
; GR_Z_COPY
;	Copy a 2D dataset.
;
; Usage:
;	ichange = gr_z_copy(pdefs, index)
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
; 	/force	If set then do not prompt before overwriting 1-D data
; 		of functions.
;
; History:
;	Original (after gr_z_read & gr_xy_copy): 7/2/12; SJT
;-

  if (*pdefs.data)[index].type ne 9 || $
     ~ptr_valid((*pdefs.data)[index].xydata) then begin
     junk = dialog_message(['Source dataset is not a 2-D', $
                            'dataset, aborting'], $
                           dialog_parent = pdefs.ids.graffer, $
                           resource = 'Graffer', $
                           /info)
     return, 0
  endif

  if ~keyword_set(force) && $
     ptr_valid((*pdefs.data)[pdefs.cset].xydata) && $
     (*pdefs.data)[pdefs.cset].type ne 9 then begin
     ans = dialog_message(['Current data set is a function', $
                           'or a 1-D dataset, reading 2-D data', $
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
     (*(*pdefs.data)[pdefs.cset].xydata).z $
  else if (*pdefs.data)[pdefs.cset].type ge 0 then ptr_free, $
     (*(*pdefs.data)[pdefs.cset].xydata).x, $
     (*(*pdefs.data)[pdefs.cset].xydata).y, $
     (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
     (*(*pdefs.data)[pdefs.cset].xydata).y_err


  ptr_free, (*pdefs.data)[pdefs.cset].xydata

  xydata = {graff_zdata}
  xydata.x = ptr_new(*(*(*pdefs.data)[index].xydata).x)
  xydata.y = ptr_new(*(*(*pdefs.data)[index].xydata).y)
  xydata.z = ptr_new(*(*(*pdefs.data)[index].xydata).z)
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)

  (*pdefs.data)[pdefs.cset].type = 9
  (*pdefs.data)[pdefs.cset].ndata = (*pdefs.data)[index].ndata
  (*pdefs.data)[pdefs.cset].ndata2 = (*pdefs.data)[index].ndata2
  (*pdefs.data)[pdefs.cset].mode = (*pdefs.data)[index].mode

  return, 1

end
