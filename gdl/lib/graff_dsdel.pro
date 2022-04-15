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
;	graff_dsdel, pdefs
;
; Argument
;	pdefs	struct	input	The graffer structure, needed if save
;	requested.
;
; History:
;	Original: 29/8/95; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace dialogues with system ones: 30/6/05; SJT
;	Update 1-D format: 14/4/22; SJT
;-

pro Graff_dsdel, pdefs

  cdesc = (*pdefs.data)[pdefs.cset].descript

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

  if (*pdefs.data)[pdefs.cset].type eq 9 then $
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
               (*(*pdefs.data)[pdefs.cset].xydata).y, $
               (*(*pdefs.data)[pdefs.cset].xydata).z $
  else if (*pdefs.data)[j].type ge 0 then ptr_free, $
     (*(*pdefs.data)[j].xydata).x, (*(*pdefs.data)[j].xydata).y, $
     (*(*pdefs.data)[j].xydata).x_err, $
     (*(*pdefs.data)[j].xydata).y_err
  
  ptr_free, (*pdefs.data)[pdefs.cset].xydata

  ptr_free, (*pdefs.data)[pdefs.cset].zopts.levels, $
            (*pdefs.data)[pdefs.cset].zopts.style, $
            (*pdefs.data)[pdefs.cset].zopts.thick, $
            (*pdefs.data)[pdefs.cset].zopts.colours, $
            (*pdefs.data)[pdefs.cset].zopts.raw_colours

  if ptr_valid(pdefs.key.list) then list = *pdefs.key.list
  ikey = bytarr(n_elements((*pdefs.data)))

  if (n_elements(list) ne 0) then ikey(list) = 1b

  if (pdefs.cset eq 0) then begin
     *pdefs.data = (*pdefs.data)(1:*)
     ikey = ikey(1:*)
  endif else if (pdefs.cset eq pdefs.nsets-1) then begin
     *pdefs.data = (*pdefs.data)(0:pdefs.cset-1)
     ikey = ikey(0:pdefs.cset-1)
  endif else begin
     (*pdefs.data) = [(*pdefs.data)(0:Pdefs.cset-1), $
                      (*pdefs.data)(Pdefs.cset+1:*)]
     ikey = [ikey(0:Pdefs.cset-1), ikey(Pdefs.cset+1:*)]
  endelse
  list = where(ikey, nkey)

  if (nkey ne 0) then *pdefs.key.list = list $
  else begin
     ptr_free, pdefs.key.list
     pdefs.key.use = 0b
  endelse


  pdefs.nsets = pdefs.nsets-1
  pdefs.cset = pdefs.cset < (pdefs.nsets-1)

  graff_set_vals, pdefs, /set_only

end
