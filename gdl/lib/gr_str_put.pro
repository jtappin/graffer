; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_str_put, iunit, tag, str

;+
; GR_STRPUT
;	Put a tag and string value to a binary GRAFFER file
;
; Usage:
;	gr_str_put, ilu, tag, str
;
;
; Argument:
;	iunit	int	input	The IDL file unit number
;	tag	string	input	The tag for the string
;	str	string	input	The actual string
;
; History:
;	Original: 15/1/96; SJT
;	Add array support: 23/6/97; SJT
;-

ll = strlen(str)
if (n_elements(str) eq 1) then  $
  writeu, iunit, tag, ll, str $
else begin
    writeu, iunit, tag, -n_elements(str) ; Note this is to allow
                                ; automatic detection of arrays on
                                ; read.
    for j = 0, n_elements(str)-1 do writeu, iunit, ll(j), str(j)
endelse


end
