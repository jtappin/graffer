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
;	errors	double	The new error values.
;	errtype	string	The error typecode.
;	retain	bool	Controls treatment of unspecified fields.
;
; Notes:
;	Not intended as a user-called routine, but as part of
;	graff_update.
;
; History:
;	Partially extracted from graff_update: 1/4/22; SJT
;-

function gr_update_xy, data, x_values, y_values, errors, errtype, $
                       retain


; Sanity checks.
  xflag = n_elements(x_values) ne 0
  yflag = n_elements(y_values) ne 0
  eflag = n_elements(errors) ne 0

  if eflag then begin
     se = size(errors)
     case se[0] of
        1: begin
           nerr = se[1]
           net = 1
        end
        2: begin
           nerr = se[2]
           if se[1] gt 6 then begin
              message, /continue, $
                       "ERRORS must have between 1 and 6 values per " + $
                       "point."
              return, 0
           endif
           net = se[1]
        end
        else: begin
           message, /continue, $
                    "ERRORS must be a 1 or 2 dimensional array, " + $
                    "not updating data."
           return, 0
        end
     endcase
  endif else begin
     nerr = 0
     net = 0
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
     if eflag && nerr ne ny then begin
        message, /continue, $
                 "ERRORS must have the same number of " + $
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
     if eflag && nerr ne data.ndata then begin
        message, /continue, $
                 "ERRORS must not change the length of the " + $
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
  endif else if eflag then begin
     if yflag && nerr ne ny then begin
        message, /continue, $
                 "ERRORS must have the same number of " + $
                 "values as Y."
        return, 0
     endif
     if nerr ne data.ndata then begin
        message, /continue, $
                 "If no Y values are given ERRORS must not " + $
                 "change the number of elements."
        return, 0
     endif
  endif

  if retain then begin
     xydata = *(data.xydata)

; Remove inapplicable errors if only X or Y is given
     if ~eflag then begin
        if xflag && yflag then begin
           ntype = 0
           xydata = xydata[0:1, *]
        endif else if xflag then begin
           case data.type of
              3: begin
                 ntype = 0
                 xydata = xydata[0:1, *]
              end
              4: begin
                 ntype = 0
                 xydata = xydata[0:1, *]
              end
              5: begin
                 ntype = 1
                 xydata = xydata[[0, 1, 3], *]
              end
              6: begin
                 ntype = 2
                 xydata = xydata[[0, 1, 3, 4], *]
              end
              7: begin
                 ntype = 1
                 xydata = xydata[[0, 1, 4], *]
              end
              8: begin
                 ntype = 2
                 xydata = xydata[[0, 1, 4, 5], *]
              end
              else: ntype = data.type
           endcase
        endif else if yflag then begin
           case data.type of
              1: begin
                 ntype = 0
                 xydata = xydata[0:1, *]
              end
              2: begin
                 ntype = 0
                 xydata = xydata[0:1, *]
              end
              5: begin
                 ntype = 1
                 xydata = xydata[0:2, *]
              end
              6: begin
                 ntype = 1
                 xydata = xydata[0:2, *]
              end
              7: begin
                 ntype = 2
                 xydata = xydata[0:3, *]
              end
              8: begin
                 ntype = 2
                 xydata = xydata[0:3, *]
              end
              else: ntype = data.type
           endcase
        endif
     endif else begin
        if keyword_set(errtype) then begin
           if strlen(errtype) ne net then $
              message, "ERRTYPE incompatible with specified errors"
           case strupcase(errtype) of
              'Y': ntype = 1
              'YY': ntype = 2
              'X': ntype = 3
              'XX': ntype = 4
              'XY': ntype = 5
              'XYY': ntype = 6
              'XXY': ntype = 7
              'XXYY': ntype = 8
           endcase
        endif else case net of
           1: ntype = 1
           2: ntype = 5
           3: ntype = 6
           4: ntype = 8
        endcase
        xydata = [xydata[0:1], reform(errors, net, nerr)]
     endelse
     if cplxflag then begin
        xydata[0, *] = real_part(x_values)
        xydata[1, *] = imaginary(x_values)
     endif else begin
        if xflag then xydata[0, *] = x_values
        if yflag then xydata[1, *] = y_values
     endelse

     ptr_free, data.xydata
     data.xydata = ptr_new(xydata)
     data.type = ntype
     
     return, 1
  endif
  
  if yflag then begin                 ; Values given
     xydata = dblarr(2+net, ny)

     xydata[1, *] = y_values
     
     if xflag then xydata[0, *] = x_values $
     else xydata[0, *] = dindgen(ny)

     if eflag then begin
        if keyword_set(errtype) then begin
           if strlen(errtype) ne net then $
              message, "ERRTYPE incompatible with specified errors"
           case strupcase(errtype) of
              'Y': ntype = 1
              'YY': ntype = 2
              'X': ntype = 3
              'XX': ntype = 4
              'XY': ntype = 5
              'XYY': ntype = 6
              'XXY': ntype = 7
              'XXYY': ntype = 8
           endcase
        endif else case net of
           1: ntype = 1
           2: ntype = 5
           3: ntype = 6
           4: ntype = 8
        endcase
        xydata[2:*, *] = errors
     endif else  ntype = 0

     ptr_free, data.xydata
     data.xydata = ptr_new(xydata)
     data.type = ntype
     data.ndata = ny

     return, 1
  endif

  if cplxflag then begin
     xydata = dblarr(2+net, nx)
     xydata[0, *] = real_part(x_values)
     xydata[1, *] = imaginary(y_values)
     
     if eflag then begin
        if keyword_set(errtype) then begin
           if strlen(errtype) ne net then $
              message, "ERRTYPE incompatible with specified errors"
           case strupcase(errtype) of
              'Y': ntype = 1
              'YY': ntype = 2
              'X': ntype = 3
              'XX': ntype = 4
              'XY': ntype = 5
              'XYY': ntype = 6
              'XXY': ntype = 7
              'XXYY': ntype = 8
           endcase
        endif else case net of
           1: ntype = 1
           2: ntype = 5
           3: ntype = 6
           4: ntype = 8
        endcase
        xydata[2:*, *] = errors
     endif else  ntype = 0

     ptr_free, data.xydata
     data.xydata = ptr_new(xydata)
     data.type = ntype
     data.ndata = ny

     return, 1
  endif

  message, 'How did I get here?'

end
