; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_xy_read, pdefs, file, force = force, index = index

;+
; GR_XY_READ
;	Get a plot from a data file
;
; Usage:
;	ichange = gr_xy_read(pdefs[, file])
;
; Return value
;	ichange	int	1 if file read, 0 if not
;
; Arguments:
;	pdefs	struct	in/out	The graffer plot structure.
;	file	string	in	A file to read, if not given, then
;				a dialogue is generated.
;
; Keyword:
; 	/force	If set then do not prompt before overwriting 2-D data
; 		of functions.
; 	index	Set to the index of a dataset to read if not current.
;
; History:
;	Original (after graff_funct): 17/8/95; SJT
;	Replace with "Pickfile" version: 21/8/95; SJT
;	Extend to more general error bars: 12/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Make reading more efficient: 24/2/11: SJT
;	Add file argument: 19/12/11; SJT
;	Add index keyword: 25/1/12; SJT
;	Eliminate obsolete findfile call: 16/4/12; SJT
;-

  if n_elements(index) eq 0 then index = pdefs.cset

  fflag = ((*pdefs.data)[index].type lt 0 or $
           (*pdefs.data)[index].type eq 9) and ~keyword_set(force)

  if (fflag) then $
     if dialog_message(['CURRENT DATA SET IS A FUNCTION', $
                        'OR A 2-D DATASET, READING 1-D DATA', $
                        'WILL OVERWRITE IT', $
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
     f = dialog_pickfile(filter = '*.dat', title = 'Graffer XY data', $
                         /must, path = path, dialog_parent = $
                         pdefs.ids.graffer, $
                         get_path = newpath, resource = 'Graffer')
     widget_control, pdefs.ids.graffer, sensitive = 1

     if (f eq '') then return, 0

     pdefs.ds_dir = newpath
  endelse

  on_ioerror, badfile

  ndat = file_lines(f)

  openr, ilu, /get, f

  dv = strarr(ndat)
;readf, ilu, dv

  ln = ''
;while (not eof(ilu)) do begin
  for j = 0l, ndat-1 do begin
     readf, ilu, ln
     dv[j] = ln
     ndat = ndat+1
  endfor

  free_lun, ilu

  locs = where(strpos(dv, '#') ne -1, ncl)
  if (ncl ne 0) then begin
     code = dv[locs[ncl-1]]     ; Use the last control line
     dv[locs] = ''              ; Make them null strings so they
                                ; aren't processed by decode
     code = strmid(strcompress(strupcase(code), /remove), 1, 10)
  endif else code = ''

  locs = where(strlen(dv) gt 0, nact)

  if (nact ne 0) then xyvals = graff_decode_xy(dv[locs], nt)

  if (nt eq 1) then begin       ; Special case of single-column data
     xyvals = [dindgen(1, n_elements(xyvals)), xyvals]
     nt = 2
  endif

  if (nt lt 0) then begin
     graff_msg, pdefs.ids.message, ["Graffer read failed", !Err_string]
     return, 0
  end

  case code of                  ; Interpret an errors code.
     'Y':    tp = 1
     'X':    tp = 3
     'YY':   tp = 2
     'XX':   tp = 4
     'XY':   tp = 5
     'XYY':  tp = 6
     'XXY':  tp = 7
     'XXYY': tp = 8
     '':     tp = ([0, 1, 5, 6, 8])[nt-2]
     Else: begin
        graff_msg, pdefs.ids.message, ["Graffer read failed", $
                                       "Bad errors code: "+code]
        return, 0
     end
  endcase

  nerr = gr_n_errors(tp)
  if nt ne 2 + nerr[0]+nerr[1] then begin
     graff_msg, pdefs.ids.message, ["Graffer read failed", $
                                    "Number of columns does not match " + $
                                    "errors code"]
     return, 0
  endif

  (*pdefs.data)[index].ndata = nact

  if ptr_valid((*pdefs.data)[index].xydata) then begin
     if (*pdefs.data)[index].type eq 9 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).z $
     else if (*pdefs.data)[index].type ge 0 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).x_err, $
        (*(*pdefs.data)[index].xydata).y_err
     
     ptr_free, (*pdefs.data)[index].xydata
  endif
  
  xydata = {graff_xydata}
  xydata.x = ptr_new(reform(xyvals[0, *]))
  xydata.y = ptr_new(reform(xyvals[1, *]))
  if nerr[0] ne 0 then xydata.x_err = $
     ptr_new(xyvals[2:1+nerr[0], *])
  if nerr[1] ne 0 then xydata.y_err = $
     ptr_new(xyvals[2+nerr[0]:*, *])

  (*pdefs.data)[index].xydata = ptr_new(xydata)

  (*pdefs.data)[index].type = tp

  return, 1

Badfile:

  graff_msg, pdefs.ids.message, ["Graffer read failed", !err_string]

  return, 0

end
