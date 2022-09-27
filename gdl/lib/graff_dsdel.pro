; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_DSDEL
;	Delete the current data set.
;
; Usage:
;	graff_dsdel, pdefs[, index]
;
; Argument
;	pdefs	struct	input	The graffer structure.
;	index	long	input	The index of the dataset to delete.
;
; Keyword:
;	/noprompt	If set, then do not prompt for confirmation.
;
; History:
;	Original: 29/8/95; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace dialogues with system ones: 30/6/05; SJT
;	Update 1-D format: 14/4/22; SJT
;-

pro Graff_dsdel, pdefs, index, noprompt = noprompt

  cdesc = (*pdefs.data)[pdefs.cset].descript

  if ~keyword_set(noprompt) then begin
     if (pdefs.nsets gt 1) then begin
        msg = ['Do you really want to', $
               'delete the current', $
               'data set containing', $
               cdesc]
        ans = dialog_message(msg, $
                             /question, $
                             dialog_parent = pdefs.ids.graffer, $
                             resource = 'Graffer')
        if ans eq 'No' then return
     endif else begin
        msg = ["Can't delete the only", $
               "data set present"]
        ans = dialog_message(msg, $
                             dialog_parent = pdefs.ids.graffer, $
                             resource = 'Graffer')
        return
     endelse
  endif else if pdefs.nsets eq 0 then return

  if n_params() eq 1 then index = pdefs.cset
  
  if ptr_valid((*pdefs.data)[index].xydata) then begin
     if (*pdefs.data)[index].type eq 9 then $
        ptr_free, (*(*pdefs.data)[index].xydata).x, $
                  (*(*pdefs.data)[index].xydata).y, $
                  (*(*pdefs.data)[index].xydata).z $
     else if (*pdefs.data)[index].type ge 0 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).x_err, $
        (*(*pdefs.data)[index].xydata).y_err
     
     ptr_free, (*pdefs.data)[index].xydata
  endif
  
  ptr_free, (*pdefs.data)[index].zopts.levels, $
            (*pdefs.data)[index].zopts.style, $
            (*pdefs.data)[index].zopts.thick, $
            (*pdefs.data)[index].zopts.colours, $
            (*pdefs.data)[index].zopts.raw_colours

  if ptr_valid(pdefs.key.list) then list = *pdefs.key.list
  ikey = bytarr(n_elements((*pdefs.data)))

  if (n_elements(list) ne 0) then ikey(list) = 1b

  if (index eq 0) then begin
     *pdefs.data = (*pdefs.data)(1:*)
     ikey = ikey(1:*)
  endif else if (index eq pdefs.nsets-1) then begin
     *pdefs.data = (*pdefs.data)(0:index-1)
     ikey = ikey(0:index-1)
  endif else begin
     (*pdefs.data) = [(*pdefs.data)(0:index-1), $
                      (*pdefs.data)(index+1:*)]
     ikey = [ikey(0:index-1), ikey(index+1:*)]
  endelse
  list = where(ikey, nkey)

  if (nkey ne 0) then *pdefs.key.list = list $
  else begin
     ptr_free, pdefs.key.list
     pdefs.key.use = 0b
  endelse


  pdefs.nsets = pdefs.nsets-1
  if pdefs.cset ge index && pdefs.cset gt 0 then pdefs.cset--

  graff_set_vals, pdefs, /set_only

end
