; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_write_ds, pdefs, file, index = index

;+
; GR_WRITE_DS
;	Write the current data set to a file in a format that can be
;	reread by the data from file options.
;
; Usage:
;	gr_write_ds, pdefs, file
;
; Arguments:
;	pdefs	struct	input	The GRAFFER data structure
;	file	string	input	The name of the file to write it to.
;
; Keyword:
;	index	int	input	THe index of the dataset to write
;
; History:
;	Original: 11/12/96; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add index keyword: 25/1/12; SJT
;-

if ~keyword_set(index) then index = pdefs.cset

xydata = *(*pdefs.data)[index].xydata

!Error = 0                      ; Clear any error

if ((*pdefs.data)[index].type eq 9) then begin ; 2-D data
    
    on_ioerror, bad_2
    
    openw, ilu, file, /get
    nvals = [(*pdefs.data)[index].ndata, $
             (*pdefs.data)[index].ndata2]
    if xydata.x_is_2d then nvals[0] *= -1
    if xydata.y_is_2d then nvals[1] *= -1

    printf, ilu, nvals
    printf, ilu, *xydata.x, format = "(5g19.12)"
    printf, ilu, *xydata.y, format = "(5g19.12)"
    printf, ilu, *xydata.z, format = "(5g19.12)"
    free_lun, ilu
    
    Bad_2:
    
endif else if ((*pdefs.data)[index].type lt 0) then begin
    
    on_ioerror, bad_f
    
    openw, ilu, file, /get
    printf, ilu, (['Y', 'X', 'XY', 'Z'])(-1-(*pdefs.data)[index].type)
    printf, ilu, xydata.range
    if ((*pdefs.data)[index].type eq -4) then  $
      printf, ilu, (*pdefs.data)[index].ndata, $
              (*pdefs.data)[index].ndata2 $ 
    else $
      printf, ilu, (*pdefs.data)[index].ndata
    printf, ilu, xydata.funct, format = '(A)' ; Ensure 2 lines for
                                ; Parametrics
    
    free_lun, ilu
    
    Bad_f:
    
endif else begin
    on_ioerror, bad_1
    openw, ilu, file, /get
    
    case (*pdefs.data)[index].type of
        0:
        1: printf, ilu, '#Y'
        2: printf, ilu, '#YY'
        3: printf, ilu, '#X'
        4: printf, ilu, '#XX'
        5: printf, ilu, '#XY'
        6: printf, ilu, '#XYY'
        7: printf, ilu, '#XXY'
        8: printf, ilu, '#XXYY'
    endcase

    nerr = gr_n_errors((*pdefs.data)[index].type)
    ncc = 2 + nerr[0]+nerr[1]
    xyvals = dblarr(ncc, (*pdefs.data)[index].ndata)
    
    xyvals[0, *] = *xydata.x
    xyvals[1, *] = *xydata.y
    if nerr[0] ne 0 then xyvals[2:1+nerr[0], *] = *xydata.x_err
    if nerr[1] ne 0 then xyvals[2+nerr[0]:*, *] = *xydata.y_err
    
    fmt = string(ncc, format = "('(',I0,'G19.12)')")
    
    printf, ilu, xyvals, format = fmt
    
    free_lun, ilu
    
    Bad_1:
endelse

if (!Error ne 0) then graff_msg, pdefs.ids.message, $
  ['Write dataset failed:', !Err_string]

end
