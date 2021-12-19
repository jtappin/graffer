; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_append, pdefs, n1, n2, delete=delete, sort=sort

;+
; GR_APPEND
;	Append one dataset to another optionally deleting the appended
;	dataset.
;
; Usage:
;	gr_append, pdefs, n1, n2[, /delete, /sort]
;
; Arguments:
;	pdefs	struct	in/out	The GRAFFER data structure.
;	n1	int	input	The index number of the appendee.
;	n2	int	input	The index number of the dataset to be
;				appended.
;
; Keywords:
;	delete	??	input	If set and non-zero, then delete the
;				dataset that was appended.
;	sort	??	input	If set and non-zero, then sort the
;				X-axis of the combined dataset.
;
; History:
;	Original: 11/11/96; SJT
;	Convert handles to pointers: 27/6/05; SJT
;-

data = *pdefs.data

if (data(n2).ndata eq 0) then begin
    graff_msg, pdefs.ids.message,  $
      ["Merge datasets failed:",  $
       "No data in dataset being appended"]
endif else if (data(n1).type ne data(n2).type and data(n2).ndata gt 0) $
  then begin
    graff_msg, pdefs.ids.message,  $
      ["Merge datasets failed:",  $
       "Datasets not of same type."]
endif else if (data(n1).mode ne data(n2).mode and data(n2).ndata gt 0) $
  then begin
    graff_msg, pdefs.ids.message,  $
      ["Merge datasets failed:",  $
       "Coordinate systems different."]
endif else if (data(n2).type lt 0) then begin
    graff_msg, pdefs.ids.message,  $
      ["Merge datasets failed:",  $
       "Cannot merge functions."]
endif else if (data(n2).type ge 8) then begin
    graff_msg, pdefs.ids.message,  $
      ["Merge datasets failed:",  $
       "Cannot merge surface datasets."]
endif else begin
    if ptr_valid(data[n1].xydata) then xy1 = *data[n1].xydata
    xy2 = *data[n2].xydata
    
    if (data(n1).ndata eq 0) then begin
        xy1 = xy2
        data(n1).type = data(n2).type
        data(n1).mode = data(n2).mode
    endif else xy1 = [[xy1(*, 0:data(n1).ndata-1)], $
                      [xy2(*, 0:data(n2).ndata-1)]]
    
    if (keyword_set(sort)) then begin
        idx = sort(xy1(0, *))
        xy1 = xy1(*, idx)
    endif
    if ptr_valid(data[n1].xydata)then *data[n1].xydata = xy1 $
    else data[n1].xydata = ptr_new(xy1)
    
    data(n1).ndata = data(n1).ndata + data(n2).ndata
    
    if (keyword_set(delete)) then begin
        ptr_free, data[n2].xydata
        if (n2 eq 0) then begin
            data = data(1:*)
        endif else if (n2 eq pdefs.nsets-1) then begin
            data = data(0:n2-1)
        endif else begin
            data = [data(0:N2-1), data(N2+1:*)]
        endelse
        pdefs.nsets = pdefs.nsets-1
        if (pdefs.cset eq n2) then begin
            pdefs.cset = n1-(n2 lt n1)
;;            widget_control, pdefs.ids.cset, set_value = n1
        endif else if (pdefs.cset gt n2) then pdefs.cset = pdefs.cset-1
    endif
    
endelse

*pdefs.data = data
graff_set_vals, pdefs, /set_only

end
