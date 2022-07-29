;+
; GR_UPDATE_XY
;	Update the data values in an X-Y dataset.
;
; Usage:
;	ok = gr_update_xy(data, x_values, y_values, errors, errtype, $
;                         retain)
;
; Returns:
;	1 if successful, 0 if the supplied values are inconsistent.
;
; Arguments:
;	data	struct	The dataset structure for the dataset.
;	x_values double	The new X-values.
;	y_values double	The new Y values.
;	x_errors double	The new X error values.
;	y_errors double	The new Y error values.
;	retain	bool	Controls treatment of unspecified fields.
;
; Notes:
;	Not intended as a user-called routine, but as part of
;	graff_update.
;
; History:
;	Partially extracted from graff_update: 1/4/22; SJT
;	Major rebuild: 18/4/22; SJT
;-

function gr_update_xy, data, x_values, y_values, x_errors, y_errors, $
                       retain

; Sanity checks.
  xflag = n_elements(x_values) ne 0
  yflag = n_elements(y_values) ne 0
  xeflag = n_elements(x_errors) ne 0
  yeflag = n_elements(y_errors) ne 0

  if xeflag then begin
     sxe = size(x_errors)
     case sxe[0] of
        1: begin
           nxev = sxe[1]
           nxerr = 1
        end
        2: begin
           nxev = sxe[2]
           if sxe[1] gt 2 then begin
              message, /continue, $
                       "X_ERRORS must have 1 or 2 values per " + $
                       "point."
              return, 0
           endif
           nxerr = sxe[1]
        end
        else: begin
           message, /continue, $
                    "X_ERRORS must be a 1 or 2 dimensional array, " + $
                    "not updating data."
           return, 0
        end
     endcase
  endif else begin
     nxerr = 0
     nxev = 0
  endelse
  
  if yeflag then begin
     sye = size(y_errors)
     case sye[0] of
        1: begin
           nyev = sye[1]
           nyerr = 1
        end
        2: begin
           nyev = sye[2]
           if sye[1] gt 2 then begin
              message, /continue, $
                       "Y_ERRORS must have 1 or 2 values per " + $
                       "point."
              return, 0
           endif
           nyerr = sye[1]
        end
        else: begin
           message, /continue, $
                    "Y_ERRORS must be a 1 or 2 dimensional array, " + $
                    "not updating data."
           return, 0
        end
     endcase
  endif else begin
     nyerr = 0
     nyev = 0
  endelse

  nx = n_elements(x_values)
  if xflag then xty = size(x_values, /type) $
  else xty = 0l
  
  ny = n_elements(y_values)
  cplxflag = 0b
  
  if xflag && yflag then begin
     if nx ne ny then begin
        message, /continue, $
                 "X_VALUES & Y_VALUES must have the same number of " + $
                 "elements."
        return, 0
     endif
     if xeflag && nxev ne ny then begin
        message, /continue, $
                 "X_ERRORS must have the same number of " + $
                 "values as X & Y."
        return, 0
     endif
     if yeflag && nyev ne ny then begin
        message, /continue, $
                 "Y_ERRORS must have the same number of " + $
                 "values as X & Y."
        return, 0
     endif
  endif else if retain then begin
     if yflag && ny ne data.ndata then begin
        message, /continue, $
                 "Y_VALUES must not change the length of the " + $
                 "dataset if RETAIN_UNSET is present."
        return, 0
     endif
     if xflag && nx ne data.ndata then begin
        message, /continue, $
                 "X_VALUES must not change the length of the " + $
                 "dataset if RETAIN_UNSET is present."
        return, 0
     endif
     if xeflag && nxev ne data.ndata then begin
        message, /continue, $
                 "X_ERRORS must not change the length of the " + $
                 "dataset if RETAIN_UNSET is present."
        return, 0
     endif 
     if yeflag && nyev ne data.ndata then begin
        message, /continue, $
                 "Y_ERRORS must not change the length of the " + $
                 "dataset if RETAIN_UNSET is present."
        return, 0
     endif 
     if xflag && ~yflag && (xty eq 6 || $
                            xty eq 9) then cplxflag = 1b
     
  endif else if xflag then begin
     if xty ne 6 && xty ne 9 then begin
        message, /continue, $
                 "If X_VALUES are given without Y_VALUES, then " + $
                 "X_VALUES must be complex."
        return, 0
     endif else cplxflag = 1b
  endif else if xeflag || yeflag then begin
     if xeflag then begin
        if yflag && nxev ne ny then begin
           message, /continue, $
                    "X_ERRORS must have the same number of " + $
                    "values as Y."
           return, 0
        endif
        if nxev ne data.ndata then begin
           message, /continue, $
                    "If no Y values are given X_ERRORS must not " + $
                    "change the number of elements."
           return, 0
        endif
     endif
     if yeflag then begin
        if yflag && nyev ne ny then begin
           message, /continue, $
                    "Y_ERRORS must have the same number of " + $
                    "values as Y."
           return, 0
        endif
        if nyev ne data.ndata then begin
           message, /continue, $
                    "If no Y values are given Y_ERRORS must not " + $
                    "change the number of elements."
           return, 0
        endif
     endif
     
  endif

  if retain then begin
     xydata = *(data.xydata)
     
; Remove inapplicable errors if only X or Y is given

     if xflag then begin
        ptr_free, xydata.x, xydata.x_err
        if cmplxflag && ~yflag then begin
           ptr_free, xydata.y, xydata.y_err
           xydata.x = ptr_new(real_part(x_values))
           xydata.y = ptr_new(imaginary(x_values))
        endif else xydata.x = ptr_new(x_values)
        nxe = 0
     endif
     
     if yflag then begin
        ptr_free, xydata.y, xydata.y_err
        xydata.y = ptr_new(y_values)
        nye = 0
     endif
     
     if xeflag then begin
        ptr_free, xydata.x_err
        xydata.x_err = ptr_new(x_errors)
        nxe = nxerr
     endif
     if yeflag then begin
        ptr_free, xydata.y_err
        xydata.y_err = ptr_new(y_errors)
        nye = nyerr
     endif
     
     ntype = gr_err_type(nxerr, nyerr)

     *data.xydata = xydata
     data.type = ntype

     return, 1
  endif
  
  if yflag then begin           ; Values given
     xydata = {graff_xydata}

     xydata.y = ptr_new(y_values)
     
     if xflag then xydata.x = ptr_new(x_values) $
     else xydata.x = ptr_new(dindgen(ny))

     if xeflag then xydata.x_err = ptr_new(x_errors)
     if yeflag then xydata.y_err = ptr_new(y_errors)

     ntype = gr_err_type(nxerr, nyerr)

     ptr_free, (*data.xydata).x, (*data.xydata).y, $
               (*data.xydata).x_err, (*data.xydata).y_err
     ptr_free, data.xydata
     
     data.xydata = ptr_new(xydata)
     data.type = ntype
     data.ndata = ny

     return, 1
  endif

  if cplxflag then begin
     xydata = {graff_xydata}
     xydata.x = ptr_new(real_part(x_values))
     xydata.y = ptr_new(imaginary(y_values))
     
     if xeflag then xydata.x_err = ptr_new(x_errors)
     if yeflag then xydata.y_err = ptr_new(y_errors)

     ntype = gr_err_type(nxerr, nyerr)

     ptr_free, data.xydata.x, data.xydata.y, $
               data.xydata.x_err, data.xydata.y_err
     ptr_free, data.xydata
     data.xydata = ptr_new(xydata)
     data.type = ntype
     data.ndata = nx

     return, 1
  endif

  message, 'How did I get here?'

end
