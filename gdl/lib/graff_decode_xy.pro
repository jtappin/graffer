; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Graff_decode_xy, txt, nt

;+
; GRAFF_DECODE_XY
;	Decode an array of X-Y[-err-err] values
;
; Usage:
;	data = graff_decode_xy(txt,nt)
;
; Return Value:
;	data	float	(nt x m) array of values & errors (nt = 2, 3 or
;			4)
;
; Arguments:
;	txt	string	input	String array of data values. 1 element
;				for each value with x y and optionally
;				1 or 2 error limits. The x value may
;				be a time in the form h:m:s.
;	nt	int	output	The number of fields in each element.
;
; History:
;	Original: 29/7/96; SJT
;	Make loop limit a long: 24/2/11; SJT
;-


  dtxt = strtrim(strcompress(txt), 2)
  junk = strsplit(dtxt[0], ' ',  count = nt)
  nact = n_elements(dtxt)

  xy_data = dblarr(nt > 2, nact)

  on_ioerror, badfloat

  for j = 0l, nact - 1 do begin
     dstxt = strsplit(dtxt[j], ' ',  /extr,  count = nl)
     if nl ne nt then goto, badfloat

     if nt eq 1 then begin
        xy_data[*, j] = [double(j), double(dstxt)]
     endif else if (strpos(dstxt[0], ':') ne -1) then begin
        tstxt = strsplit(dstxt[0], ':', /extr)
        xy_data[0, j] = total(double(tstxt)/[1., 60., 3600.])
        xy_data[1:*, j] = double(dstxt(1:*))
     endif else xy_data[*, j] = double(dstxt)
  endfor  

  return, xy_data

Badfloat:

  nt = -1
  return, 0

end



