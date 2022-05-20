; LICENCE:
; Copyright (C) 2022: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_DS_TRANSPOSE
;	Transpose a graffer dataset x<=>y.
;
; Usage:
;	ok = gr_ds_transpose(pdefs[, index])
;
; Returns:
;	1 if the dataset could be transposed, 0 if not (uninitialized
;	or a function)
; Arguments:
;	pdefs	struct	The top level GRAFFER structure.
;	index	long	Which dataset to transpose (defaults to
;			current DS).
;
; History:
;	Original (after FTN):  20/5/22; SJT
;-

function gr_ds_transpose, pdefs, index

  if n_params() eq 1 then index = pdefs.cset

  if (*pdefs.data)[index].type lt 0 then begin
     graff_msg, pdefs.ids.message, "Cannot transpose a function."
     return, 0
  endif
  if ~ ptr_valid((*pdefs.data)[index].xydata) then begin
     graff_msg, pdefs.ids.message, $
                "Cannot transpose an uninitialized dataset."
     return, 0
  endif
  
  xydata = *(*pdefs.data)[index].xydata

  if (*pdefs.data)[index].type eq 9 then begin
     x = *xydata.x
     y = *xydata.y
     z = *xydata.z
     ptr_free, xydata.x, xydata.y, xydata.z

     if size(x, /ndim) eq 2 then x = transpose(x)
     xydata.y = ptr_new(x)
     if size(y, /ndim) eq 2 then y = transpose(y)
     xydata.x = ptr_new(y)
     xydata.z = ptr_new(transpose(z)

     tmp = xydata.x_is_2d
     xydata.x_is_2d = xydata.y_is_2d
     xydata.y_is_2d = tmp

     ptr_free, (*(*pdefs.data)[index].xydata).x, $
               (*(*pdefs.data)[index].xydata).y, $
               (*(*pdefs.data)[index].xydata).z
     ptr_free, (*pdefs.data)[index].xydata
     
     (*pdefs.data)[index].xydata = ptr_new(xydata)
     
  endif else begin
     xeflag = ptr_valid(xydata.x_err)
     yeflag = ptr_valid(xydata.y_err)
     
     x = *xydata.x
     y = *xydata.y

     if xeflag then xerr = *xydata.x_err
     if yeflag then yerr = *xydata.y_err

     ptr_free, xydata.x, xydata.y, xydata.x_err, xydata.y_err

     xydata.x = ptr_new(y)
     xydata.y = ptr_new(x)
     if xeflag then xydata.y_err = ptr_new(xerr)
     if yeflag then xydata.x_err = ptr_new(yerr)

     nerr = gr_n_errors((*pdefs.data)[index].type)
     
     ptr_free, (*(*pdefs.data)[index].xydata).x, $
               (*(*pdefs.data)[index].xydata).y, $
               (*(*pdefs.data)[index].xydata).x_err, $
               (*(*pdefs.data)[index].xydata).y_err
     ptr_free, (*pdefs.data)[index].xydata
     
     (*pdefs.data)[index].xydata = ptr_new(xydata)
     (*pdefs.data)[index].type = gr_err_type(reverse(nerr))
  endelse

  return, 1
  
end
