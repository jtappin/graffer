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
;	ndata	int	input	The numer of values in it.
;	type	int	input	Interpretation of error bars.
;	r	float	output	The output R values
;	t	float	output	The output theta values
;
; History:
;	Original: 16/12/96; SJT
;-

case (type) of
    0: begin
        r = xydata(0, 0:ndata-1)
        t = xydata(1, 0:ndata-1)
    end
    1: begin
        r = [xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1))]
    end
    2: begin
        r = [xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1))]
    end
    3: begin
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1)]
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1))]
    end
    4: begin
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1)]
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1))]
    end
    5: begin
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1)), $
             xydata(1, 0:ndata-1) - $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1))]
    end
    6: begin
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(4, 0:ndata-1)) and xydata(4, 0:ndata-1)), $
             xydata(1, 0:ndata-1) - $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1))]
    end
    7: begin
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1)), $
             xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(4, 0:ndata-1)) and xydata(4, 0:ndata-1)), $
             xydata(1, 0:ndata-1) - $
             (finite(xydata(4, 0:ndata-1)) and xydata(4, 0:ndata-1))]
    end
    8: begin
        r = [xydata(0, 0:ndata-1) - $
             (finite(xydata(2, 0:ndata-1)) and xydata(2, 0:ndata-1)), $
             xydata(0, 0:ndata-1) + $
             (finite(xydata(3, 0:ndata-1)) and xydata(3, 0:ndata-1)), $
             xydata(0, 0:ndata-1), $
             xydata(0, 0:ndata-1)]
        t = [xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1), $
             xydata(1, 0:ndata-1) + $
             (finite(xydata(5, 0:ndata-1)) and xydata(5, 0:ndata-1)), $
             xydata(1, 0:ndata-1) - $
             (finite(xydata(4, 0:ndata-1)) and xydata(4, 0:ndata-1))]
    end
endcase

end
