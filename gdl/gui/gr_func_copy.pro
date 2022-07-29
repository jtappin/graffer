; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_func_copy, pdefs, index, force = force

;+
; GR_FUNC_COPY
;	Copy a function from a given dataset to the current.
;
; Usage:
;	icopy=gr_func_copy(pdefs, index)
;
; Returns:
;	1 on success, 0 on failure
;
; Arguments:
;	pdefs	struct	in/out	The main GRAFFER data structure.
;	index	long	input	The function datset to copy.
;	
; Keyword:
;	/force	If set then do not perform the usual check for
;		overwriting.
;
; History:
;	Original: 7/2/12; SJT
;-

; Check that the source is a function

  if (*pdefs.data)[index].type ge 0 || $
     ~ptr_valid((*pdefs.data)[index].xydata) then begin
     graff_msg, pdefs.ids.hlptxt, "Source DS is not a function, " + $
                "aborting"
     return, 0
  endif else stype = (*pdefs.data)[index].type

; Check that the current dataset is a function or empty
  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) && ~keyword_set(force) $
  then begin
     ttype = (*pdefs.data)[pdefs.cset].type

     if ttype ge 0 then begin
        if (*pdefs.data)[pdefs.cset].ndata gt 0 then begin
            resp = dialog_message(["Current dataset is not a function", $
                                   "do you want to overwrite it?"], $
                                  /question, $
                                  dialog_parent = pdefs.ids.graffer, $
                                  resource = "Graffer")
            if resp eq "No" then return, 0
            if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
               if ttype eq 9 then ptr_free, $
                  (*(*pdefs.data)[pdefs.cset].xydata).x, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).z $
               else if ttype ge 0 then ptr_free, $
                  (*(*pdefs.data)[pdefs.cset].xydata).x, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y_err
            endif
         endif
     endif else if ttype ne stype then begin 
        resp = dialog_message(["Current dataset and the source", $
                               "are different function types", $
                               "do you want to overwrite?"], $
                              /question, $
                              dialog_parent = pdefs.ids.graffer, $
                              resource = "Graffer")
        if resp eq "No" then return, 0
     endif
  endif

  xydata = *(*pdefs.data)[index].xydata

  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then $
     ptr_free, (*pdefs.data)[pdefs.cset].xydata

  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].type = stype

  if (*pdefs.data)[pdefs.cset].ndata eq 0 then $
     (*pdefs.data)[pdefs.cset].ndata = (*pdefs.data)[index].ndata
  if stype eq -4 && (*pdefs.data)[pdefs.cset].ndata2 eq 0 then $
     (*pdefs.data)[pdefs.cset].ndata2 = (*pdefs.data)[index].ndata2
  (*pdefs.data)[pdefs.cset].mode = (*pdefs.data)[index].mode

  return, 1

end
