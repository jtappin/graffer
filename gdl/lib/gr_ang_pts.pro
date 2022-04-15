; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_ang_pts, xydata, ndata, type, r, t

;+
; GR_ANG_PTS
;	Get all the points of polar dataset (with possible error bars)
;
; Usage:
;	gr_ang_pts, xydata, ndata, type, r, t
;
; Arguments
;	xydata	float	input	The data array.
;	ndata	int	input	The number of points (needed because
;				to preserve shape 1-point data sets
;				have a dummy point).
;	type	int	input	Interpretation of error bars.
;	r	float	output	The output R values
;	t	float	output	The output theta values
;
; History:
;	Original: 16/12/96; SJT
;	Rebuilt for new data structure: 14/4/22; SJT
;-

  r = (*xydata.x)[0:ndata-1]
  t = (*xydata.y)[0:ndata-1]

  if ptr_valid(xydata.x_err) then begin
     rerr = (*xydata.x_err)[*, 0:ndata-1]
     rerr and= finite(rerr)
  endif
  if ptr_valid(xydata.y_err) then begin
     terr = (*xydata.y_err)[*, 0:ndata-1]
     terr and=  finite(terr)
  endif
  
  case (type) of
     0: 
     1: begin
        r = [r, r]
        t = [t - terr[0, *], t + terr[0, *]]
     end
     2: begin
        r = [r, r]
        t = [t - terr[0, *], t + terr[1, *]]
     end
     3: begin
        r = [r - rerr[0, *], r + rerr[0, *]]
        t = [t, t]
     end
     4: begin
        r = [r - rerr[0, *], r + rerr[1, *]]
        t = [t, t]
     end
     5: begin
        r = [r, r, r - rerr[0, *], r + rerr[0, *]]
        t = [t - terr[0, *], t + terr[0, *], t, t]
     end
     6: begin
        r = [r, r, r - rerr[0, *], r + rerr[0, *]]
        t = [t - terr[0, *], t + terr[1, *], t, t]
     end
     7: begin
        r = [r, r, r - rerr[0, *], r + rerr[1, *]]
        t = [t - terr[0, *], t + terr[0, *], t, t]
     end
     8: begin
        r = [r, r, r - rerr[0, *], r + rerr[1, *]]
        t = [t - terr[0, *], t + terr[1, *], t, t]
     end
  endcase

end
