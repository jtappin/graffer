; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_fit_funct, pdefs, ftype, npt, slice, fset, pr, resid, $
                       nan = nan

;+
; GR_FIT_FUNCT
;	Make a least squares fit to a dataset
;
; Usage:
;	gr_fit_funct, pdefs
;
; Argument:
;	pdefs	struct	in/out	The GRAFFER plot structure.
;	ftype	int	in	Type of fit to perform
;	npt	int	in	Number of evaluations.
;	slice	string	in	Range selector for the fitting.
;	fset	int	in	The dataset index of the DS to be fitted.
;	pr	float	out	The probability of the fit.
;	resid	int	in	Widget ID of the residual box.
;
; Keyword:
;	/nan	If set, then skip invalid points.
;
; History:
;	Original: 28/8/96; SJT
;	Add support for median fits and simplify representation of fit
;	types: 23/9/96; SJT
;	Change to make this the inner routine & allow higher degree
;	fits: 22/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Add CHECK error handling for fitting calls thus eliminating
;	the need for local SVDFIT version and making safer: 29/1/97; SJT
;	Add options to do "piecewise linear" fits: 4/2/97; SJT
;	Change svdfit to poly_fit as the former seems to now be
;	broken: 22/4/04; SJT
;	Replace handles with pointers: 27/6/05; SJT
;	Only use those points that are "valid" for the fit type
;	(unless a slice is given): 17/12/09; SJT
;	Tidy slice handling, add nan keyword: 27/11/15; SJT
;-

  xy = *(*pdefs.data)[fset].xydata

  if (ftype[2] eq 0) then case ((*pdefs.data)[fset].type) of
     0:                             ; No errs
     1: wy = transpose(xy(2, *))    ; Y
     2: wy = total(xy(2:3, *), 1)/2. ; +-Y
     3: wx = transpose(xy(2, *))     ; X
     4: wx = total(xy(2:3, *), 1)/2. ; +-X
     5: begin
        wx = transpose(xy(2, *))
        wy = transpose(xy(3, *)) ; XY
     end
     6: begin
        wx = transpose(xy(2, *))
        wy = total(xy(3:4, *), 1)/2. ; X+-Y
     end
     7: begin
        wx = total(xy(2:3, *), 1)/2.
        wy = transpose(xy(4, *)) ; +-XY
     end
     8: begin
        wx = total(xy(2:3, *), 1)/2.
        wy = total(xy(4:5, *), 1)/2. ; +-X+-Y
     end
  endcase

  x = transpose(xy(0, *))
  y = transpose(xy(1, *))

  if (slice ne '') then begin
     if strpos(slice, '[') ne 0 && $
        strpos(slice, ']', /reverse_search) ne strlen(slice)-1 then $
           uslice = '['+slice+']' $
     else uslice = slice

     junk1 = execute('x = x'+uslice)
     junk2 = execute('y = y'+uslice)
     if (n_elements(w) ne 0) then junk3 = execute('w = w'+uslice) $
     else junk3 = 1
     if (junk1+junk2+junk3 ne 3) then begin
        graff_msg, resid, 'Bad slice specification -- fit ' + $
                   'not performed'
        pr = 0.
        return, ''
     endif
  endif

  if keyword_set(nan) then begin
     case ftype[0] of
        0: good = where(finite(x) and finite(y), ngood)         ; Polynomial
        4: good = where(finite(x) and finite(y), ngood)         ; Piecewise
        1: good = where(finite(x) and finite(y) and y gt 0, ngood) ; Exp
        2: good = where(finite(x) and finite(y) and x gt 0, ngood) ; Log
        3: good = where(finite(x) and finite(y) and x gt 0 and y gt 0, $
                        ngood)  ; Power-law
     endcase
     
     if ngood eq 0 then begin
        widget_control, resid, set_value = 'No valid data points!'
        pr = 0.
        return, ''
     endif
     x = x[good]
     y = y[good]
     if n_elements(w) ne 0 then w = w[good]
  endif

  if (ftype[1]) ne 0 then begin
     temp = temporary(x)
     x = temporary(y)
     y = temporary(temp)
     if (N_elements(wx) ne 0) then w = wx
     fftype = -2
     var = 'y'
  endif else begin
     fftype = -1
     if (N_elements(wy) ne 0) then w = wy
     var = 'x'
  endelse

  if (ftype[0] ne 4) then begin
     if (ftype[0] and 2) ne 0 then begin
        x = alog(x)
        var = 'alog('+var+')'
     endif
     
     if (ftype[0] and 1) ne 0 then begin
        if (n_elements(w) ne 0) then w = w/y
        y = alog(y)
     endif
  endif


;if (n_elements(w) ne 0) then w = 1./w

  sing = 0
  c2 = 0.

                                ; It is quite possible to make SVDFIT
                                ; do a belly up so we need an error
                                ; handler to catch the error otherwise
                                ; GRAFFER will crash ignominiously
                                ; with the probable loss of pdefs.

  catch, ecode
  if (ecode ne 0) then begin
     widget_control, resid, set_value = 'Fitting error - degree ' + $
                     'too high?'
     pr = 0.
     return, ''
  endif

  if (ftype[0] eq 4) then fit = gr_pwfit(x, y, w, ftype[3], c2) $
  else if (ftype[2]) then fit = ladfit(x, y) $
  else fit = poly_fit(x, y, ftype[3], measure_errors = w, chisq = c2)
;else fit = svdfit(x, y, ftype[3]+1, weight = w, singular = sing, chisq = c2)

  catch, /cancel                ; Should only need the Catcher for SVDFIT

  func = gr_str_fun(fit, var, pieces = ftype[0] eq 4)
  if (ftype[0] and 1) then func = 'exp('+func+')'


  xydata = {graff_funct}
  xydata.funct = func

  (*pdefs.data)[pdefs.cset].ndata = npt

  if (*pdefs.data)[pdefs.cset].type eq 9 then $      ; Overwriting a 2-D
     ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $ ; dataset
               (*(*pdefs.data)[pdefs.cset].xydata).y, $
               (*(*pdefs.data)[pdefs.cset].xydata).z

  ptr_free, (*pdefs.data)[pdefs.cset].xydata
  (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
  (*pdefs.data)[pdefs.cset].type = fftype

  free = n_elements(x) - n_elements(fit)

  catch, ecode                  ; CHISQR_PDF can also fall down
  if (ecode ne 0) then pr = 0. $
  else pr = 1. - chisqr_pdf(c2, free)
  catch, /cancel                ; Should only need the Catcher for CHISQR_PDF

  return, func

end
