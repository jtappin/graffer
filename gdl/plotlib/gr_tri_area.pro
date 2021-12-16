; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_tri_area, x, y, tri, usable = usable, $
                      threshold = threshold, nbad = nbad

;+
; GR_TRI_AREA
;	Compute the areas of the triangles found by triangulate.
;	
; Usage:
;	areas = gr_tri_area(x, y, tri)
;
; Arguments:
;	x	double	The x coordinates of the points in the data array
;	y	double	The y coordinates of the poitns in the data array
;	tri	long	The indices of the triangle vertices as
;			returned by IDL's triangulate routine
;
; Keywords:
;	usable	long	A named variable for the list of triangles
;			that are meaningful
;	threshold double If USABLE is given, then this is the
;			threshold for the smallest area deemed usable
;			(as a fraction of the largest triangle
;			present, default=1e-11).
;	nbad	long	A named variable for the number of non-usable
;			triangles.
; Notes:
;	Seems to be needed as the threshold setting for triangulate
;	can be fooled.
;
; History:
;	Original: 14/10/16; SJT
;-

  sz = size(tri, /dim)
  area = abs(x[tri[0, *]]*y[tri[1, *]] + $
             x[tri[1, *]]*y[tri[2, *]] + $
             x[tri[2, *]]*y[tri[0, *]] - $
             x[tri[0, *]]*y[tri[2, *]] - $
             x[tri[1, *]]*y[tri[0, *]] - $
             x[tri[2, *]]*y[tri[1, *]]) / 2.

  if arg_present(usable) then begin
     if ~keyword_set(threshold) then threshold = 1.d-11
     usable = where(area ge threshold*max(area), ngood,  ncomp = nbad)

  endif
     
  return, reform(area)

end
