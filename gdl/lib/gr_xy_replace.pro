;+
; GR_XY_REPLACE
;	Replace the XY data values in a graffer dataset.
;
; Usage:
;	gr_xy_replace, pdefs, x, y[, xerr=xerr, yerr=yerr]
;
; Arguments:
;	pdefs	struct	The graffer data structure.
;	x	double	The new X values
;	y	double	The new Y values
;
; Keywords:
;	xerr	double	The new X error values
;	yerr	double	The new Y error values
;	index	long	Which dataset to extract (if not given, then
;			the current dataset is updateded).
;	/allow_type_change If set, then the dataset type may be
;			changed, otherwise it is an error to do so.		
;	status	int	A named variable to return the status. (1 =
;			success, 0 = fail (would require forbidden
;			type change or sizes inconsistent)).
;
; Notes:
;	If xerr and/or yerr are not set and /allow_type_change is not
;	specified, then (a) the number of points in the DS must not be
;	changed and (b) the old errors are retained. 
;
; History:
;	Original: 27/4/22; SJT
;-

pro gr_xy_replace, pdefs, x, y, xerr = xerr, yerr = yerr, index = $
                   index, status = status, allow_type_change = atc


  if n_elements(index) eq 0 then index = pdefs.cset
  
; Pull these out so that we don't have to mess around with too
; many messy pointer expressions while sanity checking
  
  ctype = (*pdefs.data)[index].type
  cndata = (*pdefs.data)[index].ndata

  nx = n_elements(x)
  ny = n_elements(y)
  
  if keyword_set(xerr) then begin
     sxe = size(xerr)
     if sxe[0] eq 1 then begin
        nxe = 1l
        nxev = sxe[1]
     endif else begin
        nxe = sxe[1]
        nxev = sxe[2]
     endelse
  endif else begin
     nxe = 0l
     nxev = 0l
  endelse
  
  if keyword_set(yerr) then begin
     sye = size(yerr)
     if sye[0] eq 1 then begin
        nye = 1l
        nyev = sye[1]
     endif else begin
        nye = sye[1]
        nyev = sye[2]
     endelse
  endif else begin
     nye = 0l
     nyev = 0l
  endelse
  
  itype = gr_err_type(nxe, nye)

  if nx ne ny then begin
     graff_msg, pdefs.ids.message, $
                "X & Y have different lengths, cannot use."
     status = 0
     return
  endif
  if nxe ne 0 && nxev ne nx then begin
     graff_msg, pdefs.ids.message, $
                     "X & X-errors have different lengths, cannot use."
     status = 0
     return
  endif
  if nye ne 0 && nyev ne ny then begin
     graff_msg, pdefs.ids.message, $
                     "Y & Y-errors have different lengths, cannot use."
     status = 0
     return
  endif

  if ~keyword_set(atc) then begin
     if ctype lt 0 || ctype gt 8 then begin
        graff_msg, pdefs.ids.message, $
                   ["Current dataset is not X-Y data.", $
                    "To force the change /allow_type_change must be " + $
                    "set."]
        status = 0
        return
     endif
     if (nxe ne 0 || nye ne 0) && itype ne ctype then begin
        graff_msg, pdefs.ids.message, $
                   ["Supplied errors will change dataset type.", $
                    "To do this /allow_type_change must be set."]
        status = 0
        return
     endif
     if ctype ne 0 && n_elements(x) ne cndata then begin
        graff_msg, pdefs.ids.message, $
                   ["Dataset has error limits, no new values " + $
                    "supplied", $
                    "Cannot change length of dataset unless " + $
                    "/allow_type_change is set."]
        status = 0
        return
     endif

  endif

  if ctype lt 0 || ctype gt 8 then begin ; Replace a non-XY dataset.
     if ctype eq 9 then begin   ; Replacing a Z dataset, need to free
        ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
                  (*(*pdefs.data)[pdefs.cset].xydata).y, $
                  (*(*pdefs.data)[pdefs.cset].xydata).z
        ptr_free, (*pdefs.data)[pdefs.cset].xydata
     endif
     (*pdefs.data)[pdefs.cset].xydata = ptr_new({graff_xydata})
  endif

  ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
            (*(*pdefs.data)[pdefs.cset].xydata).y
  
  (*(*pdefs.data)[pdefs.cset].xydata).x = ptr_new(x[*]) ; N.B. force
                                ; to 1D
  (*(*pdefs.data)[pdefs.cset].xydata).y = ptr_new(y[*]) ; N.B. force
                                ; to 1D

  if keyword_set(xerr) then begin
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x_err
     if sxe[0] eq 1 then $
        (*(*pdefs.data)[pdefs.cset].xydata).x_err = $
        ptr_new(reform(xerr, 1, nxev)) $
     else (*(*pdefs.data)[pdefs.cset].xydata).x_err = ptr_new(xerr)
  endif else if keyword_set(act) then $
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x_err

  
  if keyword_set(yerr) then begin
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).y_err
     if sye[0] eq 1 then $
        (*(*pdefs.data)[pdefs.cset].xydata).y_err = $
        ptr_new(reform(yerr, 1, nyev)) $
     else (*(*pdefs.data)[pdefs.cset].xydata).y_err = ptr_new(yerr)
  endif else if keyword_set(act) then $
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).y_err



  (*pdefs.data)[pdefs.cset].ndata = nx
  (*pdefs.data)[pdefs.cset].type = itype

  status = 1

  pdefs.chflag = 1b
  pdefs.transient.changes += 21 ; Changes of actual data values count
                                ; as 21 changes to force an autosave
                                ; next time it's checked.
  
end
