; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_make_levels, mn, mx, nl, map

;+
; GR_MAKE_LEVELS
;	Compute automatic contour levels, given limits etc.
;
; Usage:
;	levels = gr_make_levels(mn, mx, nl, map)
;
; Returns:
;	An array of contour levels.
;
; Arguments:
;	mn	double	The minimum of the data.
;	mx	double	The maximum of the data.
;	nl	int	The number of levels.
;	map	int	The mapping, 0=linear, 1=log, 2=sqrt (signed)
;
; History:
;	Original: 12/10/16; SJT
;-

  case map of
     0: begin                   ; Linear scaling
        rg = mx-mn
        return, rg * (dindgen(nl)+.5)/nl + mn
     end
     1: begin                   ; Log scaling
        lmx = alog10(mx)
        lmn = alog10(mn)
        if ~finite(lmn) || ~finite(lmx) then return, 0.d0        

        rg = lmx-lmn
        ll = rg * (dindgen(nl)+.5)/nl + lmn

        return, 10.d0^ll
     end
     2: begin                   ; Signed Sqrt scaling
        smx = mx eq 0 ? 0.d0 : mx/sqrt(abs(mx))
        smn = mn eq 0 ? 0.d0 : mn/sqrt(abs(mn))

        rg = smx-smn
        sl = rg * (dindgen(nl)+.5)/nl + smn
        ln = where(sl lt 0, nn)
        l = sl^2
        if nn ne 0 then l[ln] = -l[ln]

        return, l
     end
     else: begin
        message, /continue, $
                 "Invalid mapping type"
        return, 0.d0
     end
  endcase
end
