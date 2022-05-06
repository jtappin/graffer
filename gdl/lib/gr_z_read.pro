; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_z_read, pdefs, file, force = force, index = index

;+
; GR_Z_READ
;	Get a plot from a data file
;
; Usage:
;	ichange = gr_z_read(pdefs[, file])
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
; Arguments:
;	pdefs	struct	in/out	The graffer plot structure.
;	file	string	in	A file to read, if not given, then
;				a dialogue is generated.
;
; Keyword:
; 	/force	If set then do not prompt before overwriting 1-D data
; 		of functions.
; 	index	Set to the index of a dataset to read if not current.
;
; History:
;	Original (after gr_xy_read): 11/12/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add file argument: 19/12/11; SJT
;	Add index keyword: 25/1/12; SJT
;	Eliminate obsolete findfile call: 16/4/12; SJT
;-

  if n_elements(index) eq 0 then index = pdefs.cset

  fflag = ((*pdefs.data)[index].type ne 9 and $
           (*pdefs.data)[index].ndata gt 0) and ~keyword_set(force)

  if (fflag) then $
     if dialog_message(['CURRENT DATA SET IS 1-D OR A FUNCTION,', $
                        'READING 2-D DATA WILL OVERWRITE IT', $
                        'DO YOU REALLY WANT TO DO THIS?'], $
                       /question, title = 'Overwriting ' + $
                       'function', dialog_parent = $
                       pdefs.ids.graffer, resource = 'Graffer') eq 'No' then $
                          return, 0

  if file_test(pdefs.ds_dir, /directory) then path = pdefs.ds_dir $
  else cd, current = path

  if (n_elements(file) gt 0 && file_test(file)) then begin
     f = file
     if (file_dirname(f) ne '.') then pdefs.ds_dir = file_dirname(f)
  endif else begin
     widget_control, pdefs.ids.graffer, sensitive = 0
     f = dialog_pickfile(filter = '*.dat', title = 'Graffer Z data', $
                         /must, path = path, dialog_parent = $
                         pdefs.ids.graffer, $
                         get_path = newpath, resource = 'Graffer')
     widget_control, pdefs.ids.graffer, sensitive = 1

     if (f eq '') then return, 0

     pdefs.ds_dir = newpath
  endelse

  on_ioerror, badfile

  openr, ilu, /get, f

  nx = 0 & ny = 0
  readf, ilu, nx, ny
  if nx lt 0 then x = dblarr(abs(nx), abs(ny)) $
  else x = dblarr(nx)

  if ny lt 0 then y = dblarr(abs(nx), abs(ny)) $
  else y = dblarr(ny)

  z = dblarr(abs(nx), abs(ny))

  readf, ilu, x
  readf, ilu, y
  readf, ilu, z

  free_lun, ilu

  (*pdefs.data)[index].ndata = abs(nx)
  (*pdefs.data)[index].ndata2 = abs(ny)

  if ptr_valid((*pdefs.data)[index].xydata) then begin
     if (*pdefs.data)[index].type eq 9 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).z $
     else if (*pdefs.data)[index].type  ge 0 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).x_err, $
        (*(*pdefs.data)[index].xydata).y_err
     ptr_free, (*pdefs.data)[index].xydata
  endif

  xydata = {graff_zdata}

  xydata.x = ptr_new(x)
  xydata.y = ptr_new(y)
  xydata.z = ptr_new(z)
  xydata.x_is_2d = nx lt 0
  xydata.y_is_2d = ny lt 0

  (*pdefs.data)[index].xydata = ptr_new(xydata)
  (*pdefs.data)[index].type = 9

  graff_set_vals, pdefs, /set_only

  return, 1

Badfile:

  graff_msg, pdefs.ids.message, ["Graffer read failed", !err_string]
  return, 0

end
