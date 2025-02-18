; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_pwfit, x, y, w, nsg, c2

;+
; GR_PWFIT
;	Control use of CURVEFIT to make a piecewise linear fit to the
;	dataset
;
; Usage:
;	fit = gr_pwfit(x, y, w, nsg, c2)
;
; Return Value:
;	fit	float	The fitted funtion expressed in terms of the
;			GR_PIECES function
;
; Arguments:
;	x	float	input	The X values to be fitted
;	y	float	input	The Y values to be fitted
;	w	float	input	the weights to be assigned to each point
;	nsg	int	input	How many breaks to use
;	c2	float	output	The Chi^2 value of the resulting fit
;
; History:
;	Original: 4/2/97; SJT
;-

  if n_elements(w) eq 0 then w = replicate(1., n_elements(x))

  minx = min(x, max = maxx)

  bps = dindgen(nsg+2)*(maxx-minx)/(nsg+1.) + minx
  yb0 = fractile(y, dindgen(nsg+2)/(nsg+1))

  sl0 = (yb0(1:*)-yb0)/(bps(1:*)-bps)

  y0 = sl0(0) * bps(0) + yb0(0)

  a = dblarr((nsg+1)*2)
  a(0) = y0
  a(indgen(nsg+1)*2 + 1) = sl0
  a(indgen(nsg)*2 + 2) = bps(1:nsg)

  c2 = 0.

                                ; Note thet the weights in CURVEFIT
                                ; are Sigma^2 (c.f. SVDFIT)

  junk = curvefit(x, y, w^2, a, chi2 = c2, funct = 'gr_cf_pieces', /noderiv)

  c2 = c2*(n_elements(x)-n_elements(a)) ; Make it the same type of CHI^2
                                ; as in SVDFIT

  return, a

end
