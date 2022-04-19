;+
; GR_UPDATE_XY_OLD
;	Update the data values in an X-Y dataset.
;
; Usage:
;	ok = gr_update_xy_old(data, x_values, y_values, errors, errtype, $
;                             retain)
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
;	graff_update. This is a wrapper to replace the old error block
;	with separate X & Y errors.
;
; History:
;	Partially extracted from graff_update: 1/4/22; SJT
;-

function gr_update_xy_old, data, x_values, y_values, errors, errtype, $
                       retain


  if n_elements(errors) ne 0 then begin
     se = size(errors)
     if se[0] eq 1 then begin
        errs = reform(errors, 1, se[1])
        nev = 1
     endif else begin
        errs = errors
        nev = se[1]
     endelse
     
     if n_elements(errtype) ne 0 then begin
        if size(errtype, /type) eq 7 then begin
           if strlen(errtype) ne se[1] then $
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
        endif else ntype = long(errtype)
     endif else case nev of
        1: ntype = 1
        2: ntype = 5
        3: ntype = 6
        4: ntype = 8
     endcase
     net = gr_n_errors(ntype)
     if net[0] ne 0 then x_errors = errs[0:net[0]-1, *]
     if net[1] ne 0 then y_errors = errs[net[0]:*, *]
  endif

  return, gr_update_xy(data,  x_values, y_values, x_errors, y_errors, $
                       retain)

end
