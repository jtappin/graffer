; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_reg2norm, xr, yr, xn, yn, invert=invert

;+
; GR_REG2NORM
;	Convert "region" coordinates to real normalized coordinates
;
; Usage:
;	gr_reg2norm, xr, yr, xn, yn
;
; Arguments:
;	xr, yr	float	input	X & Y coordinates in "region" system
;	xn, yn	float	output	X & Y coordinates in normalized
;				system.
;
; Keyword:
;	invert	??	input	If set, then take normalized
;				coordinates and return region coords
;
; History:
;	Original (after gr_fra2norm): 13/1/12; SJT
;-

; No region => no change
if !p.region[0] eq !p.region[2] then begin
    xn = xr
    yn = yr
    return
endif

if keyword_set(invert) then begin
    xn = (xr-!p.region[0])/(!p.region[2]-!p.region[0])
    yn = (yr-!p.region[1])/(!p.region[3]-!p.region[1])
endif else begin
    xn = xr*(!p.region[2]-!p.region[0])+!p.region[0]
    yn = yr*(!p.region[3]-!p.region[1])+!p.region[1]
endelse

end
