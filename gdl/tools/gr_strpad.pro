; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_STRPAD
;	Pad a string to a given length.
;
; Usage:
;	lstr = gr_strpad(str, len)
;
; Returns:
;	The string padded with spaces.
;
; Argument:
;	str	string	The string to pad.
;	len	long	The required final length. If the string is
;			longer than len then the string is not modified.
;
; Keyword:
;	/at_left	If set, then the spaces are prepended.
;	/centred	If set, then the string is centred (any odd
;			space is to the right of the string unless
;			/at_left is also set).
;
; History:
;	Original: 17/9/21; SJT
;	Renamed (clash with solarsoft): 21/9/21; SJT
;-

function gr_strpad, str, len, at_left = at_left, centred = centred

  if size(str, /type) ne 7 then begin
     message, /continue, "Cannot extend a non-string."
     return, ''
  endif

  lstr = strlen(str)
  if lstr ge len then return, str

  npad = len-lstr
  if keyword_set(centred) then begin
     nl = npad/2
     nr = nl
     if npad mod 2 eq 1 then begin
        if keyword_set(at_left) then nl++ $
        else nr++
     endif

     return, string(replicate(32b, nl)) + str + string(replicate(32b, nr))
  endif else if keyword_set(at_left) then $
     return, string(replicate(32b, npad)) + str $
  else return, str + string(replicate(32b, npad))

end
     
