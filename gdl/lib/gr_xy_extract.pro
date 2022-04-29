;+
; GR_XY_EXTRACT
;	Extract the XY data arrays from a dataset structure.
;
; Usage:
;	gr_xy_extract, pdefs, x, y[, xerr, yerr]
;
; Arguments:
;	pdefs	struct	The graffer data structure.
;	x	double	A variable to return the X values.
;	y	double	A variable to return the Y values.
;	xerr	double	A variable to return the X errors (if they
;			exist). 
;	yerr	double	A variable to return the Y errors (if they
;			exist).
;
; Keywords:
;	index	long	Which dataset to extract (if not given, then
;			the current dataset is extracted).
;	nerr	long	A named variable to return the number of
;			errors as a 2-element array.
;	status	int	A named variable to return the status. (1 =
;			success, 0 = fail (not an XY dataset)).
;	ndata	long	A variable to return the number of points in
;			the dataset.
;
; History:
;	Original: 27/4/22; SJT
;-

pro gr_xy_extract, pdefs, x, y, xerr, yerr, index = index, nerr = $
                   nerr, status = status, ndata = ndata

  if n_elements(index) eq 0 then index = pdefs.cset

  if (*pdefs.data)[index].type lt 0 || $
     (*pdefs.data)[index].type gt 8 then begin
     graff_msg, pdefs.ids.message, $
                ["Requested dataset is not an XY dataset.", $
                 "Cannot extract values."]
     status = 0
     return
  endif

  if ~ptr_valid(*(*pdefs.data)[pdefs.cset].xydata) || $
     (*pdefs.data)[pdefs.cset].ndata eq 0 then ndata = 0l $
  else ndata = (*pdefs.data)[pdefs.cset].ndata

  if ndata eq 0 then begin      ; No data to extract.
     nerr = [0l, 0l]
     status = 1
     return
  endif
  
  nerr = gr_n_errors((*pdefs.data)[index].type)

  x = *(*(*pdefs.data)[pdefs.cset].xydata).x
  y = *(*(*pdefs.data)[pdefs.cset].xydata).y
  if nerr[0] gt 0 && arg_present(xerr) then xerr = $
     *(*(*pdefs.data)[pdefs.cset].xydata).x_err
  if nerr[1] gt 0 && arg_present(yerr) then yerr = $
     *(*(*pdefs.data)[pdefs.cset].xydata).y_err

end
