; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_coord_convert, x, y, xt, yt, data = data, normal = normal, $ 
                      region = region, frame = frame, $
                      device = device, $
                      to_data = to_data, to_normal = to_normal, $
                      to_region = to_region, to_frame = to_frame, $
                      to_device = to_device 

;+
; GR_COORD_CONVERT
; 	Convert between coordinate systems, with the addition of
; 	"frame" and "region" coordinates.
; 
; Usage:
;	gr_coord_convert, x, y, xt, yt, <selector keys>
;
; Arguments:
;	x	flt/dbl	input	The x-coordinate(s) to convert
;	y	flt/dbl	input	The y-coordinate(s) to convert
;	xt	flt/dbl	output	The converted X-coordinates
;	yt	flt/dbl	output	The converted Y-coordinates
;
; Keywords:
;	/data	If set the inputs are in data coordinates
;	/normal	If set the inputs are in normalized coordinates
;	/region	If set the inputs are in "region" coordinates
;		(normalized wrt !p.region)
;	/frame	If set the inputs are in "frame" coordinates
;		(normalized wrt !p.position or equivalent).
;	/device	If set the inputs are in device coordinates.
;	The corresponding to_ keys select the output system.
;
; Method:
;	Coordinates are converted to normalized and then to output.
;
; History:
;	Original: 13/1/12; SJT
;-

  ninput = keyword_set(data) + keyword_set(normal) + $
           keyword_set(region) + keyword_set(frame) + keyword_set(device)

  noutput = keyword_set(to_data) + keyword_set(to_normal) + $
            keyword_set(to_region) + keyword_set(to_frame) + $
            keyword_set(to_device)

  if (ninput ne 1 or noutput ne 1) then message, "Must give exactly one " + $
     "input and one output system"

; Handle trivial cases
  if keyword_set(data) && keyword_set(to_data) || $
     keyword_set(normal) && keyword_set(to_normal) || $
     keyword_set(region) && keyword_set(to_region) || $
     keyword_set(frame) && keyword_set(to_frame) || $
     keyword_set(device) && keyword_set(to_device) then begin
     xt = x
     yt = y
     return
  endif

; Convert inputs to normalized
  if keyword_set(data) then begin
     if !x.type then xx = alog10(x) else xx = x
     if !y.type then yy = alog10(y) else yy = y

     xn = !x.s[0] + !x.s[1]*xx
     yn = !y.s[0] + !y.s[1]*yy
  endif else if keyword_set(region) then $
     gr_reg2norm, x, y, xn, yn $
  else if keyword_set(frame) then $
     gr_fra2norm, x, y, xn, yn $
  else if keyword_set(device) then begin
                                ; Force values to be at double
     if (size(x, /type) ne 5) then xf = double(x) $
     else xf = x
     if (size(y, /type) ne 5) then yf = double(y) $
     else yf = y

     xn = xf/!d.x_vsize
     yn = yf/!d.y_vsize
  endif else begin
     xn = x
     yn = y
  endelse

; Convert normalized to output

  if keyword_set(to_data) then begin
     xt = (xn-!x.s[0])/!x.s[1]
     yt = (yn-!y.s[0])/!y.s[1]

     if !x.type then xt = 10.^xt
     if !y.type then yt = 10.^yt
  endif else if keyword_set(to_region) then $
     gr_reg2norm, /invert, xn, yn, xt, yt $
  else if keyword_set(to_frame) then $
     gr_fra2norm, /invert, xn, yn, xt, yt $
  else if keyword_set(to_device) then begin
     xt = xn*!d.x_vsize
     yt = yn*!d.y_vsize
  endif else begin
     xt = xn
     yt = yn
  endelse

end
