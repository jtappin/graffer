; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_fun_read, pdefs, file, force = force

;+
; GR_FUN_READ
;	Get a function from a data file
;
; Usage:
;	ichange = gr_fun_read(pdefs[, file])
;
; Return value:
;	ichange	int	1 if the DO button was used, 0 if cancel
;
; Argument:
;	pdefs	struct	in/out	The graffer plot structure.
;	file	string	in	An optional file from which to read
;				the function
;
; Keyword:
; 	/force	If set then do not prompt if attempting to replace a
; 		data dataset.
;
; History:
;	Original (after gr_xy_read): 17/12/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Convert to use pointers: 27/6/05; SJT
;	Add file argument: 20/12/11; SJT
;	Eliminate obsolete findfile call: 16/4/12; SJT
;-

  dflag = ((*pdefs.data)[pdefs.cset].ndata gt 0) and $
          ((*pdefs.data)[pdefs.cset].type ge 0) and ~keyword_set(force)

  if (dflag) then $
     if dialog_message(['CURRENT DATA SET IS NOT A', $
                        'FUNCTION, READING A FUNCTION', $
                        'WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $ 
                       /question, dialog_parent = pdefs.ids.graffer, $
                       resource = 'Graffer') eq 'No' then return, 0

  if file_test(pdefs.ds_dir, /directory) then path = pdefs.ds_dir $
  else cd, current = path

  if (n_elements(file) gt 0 && file_test(file)) then begin
     f = file
     if (file_dirname(f) ne '.') then pdefs.ds_dir = file_dirname(f)
  endif else begin
     widget_control, pdefs.ids.graffer, sensitive = 0
     f = dialog_pickfile(filter = '*.dat', title = 'Graffer function ' + $
                         'data', $
                         /must, path = path, dialog_parent = $
                         pdefs.ids.graffer, $
                         get_path = newpath, resource = 'Graffer')
     widget_control, pdefs.ids.graffer, sensitive = 1

     if (f eq '') then return, 0

     pdefs.ds_dir = newpath
  endelse

  on_ioerror, badfile

  openr, ilu, /get, f

  dv = ''
  readf, ilu, dv

  case strcompress(/remove, strupcase(dv)) of
     'Y': begin
        range = dblarr(2)
        nval = 0
        func = ''
        type = -1
     end
     'X': begin
        range = dblarr(2)
        nval = 0
        func = ''
        type = -2
     end
     'XY': begin
        range = dblarr(2)
        nval = 0
        func = strarr(2)
        type = -3
     end
     'Z': begin
        range = dblarr(2, 2)
        nval = intarr(2)
        func = ''
        type = -4
     end
     Else: begin
        free_lun, ilu
        graff_msg, pdefs.ids.message, ["Graffer function read " + $
                                       "failed:",  $
                                       "Unknown function type code"]
        return, 0
     end
  endcase

  readf, ilu, range
  readf, ilu, nval
  readf, ilu, func

  free_lun, ilu


  (*pdefs.data)[pdefs.cset].ndata = nval(0)
  if (type eq -4) then (*pdefs.data)[pdefs.cset].ndata2 = nval(1)

  if (type eq -1 or type eq -2) then xydata = {graff_funct} $
  else if (type eq -3) then xydata = {graff_pfunct} $
  else xydata = {graff_zfunct}

  xydata.Range = range
  xydata.Funct = func

  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     if (*pdefs.data)[pdefs.cset].type eq 9 then $      ; Overwriting a 2-D
        ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $ ;  dataset
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).z $
     else if (*pdefs.data)[pdefs.cset].type ge 0 then $ ; Overwriting a 1-D
        ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $ ; dataset
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).x_err, $ 
                  (*(*pdefs.data)[pdefs.cset].xydata).y_err

     ptr_free, (*pdefs.data)[pdefs.cset].xydata
  endif
  
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].type = type

  return, 1

Badfile:

  graff_msg, pdefs.ids.message, ["Graffer function read failed:", $
                                 !Err_string]

  return, 0

end
