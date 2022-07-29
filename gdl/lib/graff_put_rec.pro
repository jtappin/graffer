; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_put_rec, ilu, tag, value, force_2d = force_2d

;+
; GRAFF_PUT_REC
;	Put a component of a Graffer V4+ file to the output file.
;
; Usage:
;	graff_put_rec, ilu, tag, value
;
; Arguments:
;	ilu	long	input	The logical unit to which to write.
;	tag	string	input	The name of the field tag for the variable.
;	value	any	input	The values to write in that tag.
;
; Keyword:
; 	/force_2d	If set, then force a 1-D array to be written
; 			as N×1, should allow the absurdity of having a
; 			dummy element for 1 point datasets to go.
;
; History:
;	Original: 5/1/12; SJT
;	Add list types: 6/10/16; SJT
;	Add /force_2d: 28/4/22; SJT
;-

  on_error, 2
  if n_params() lt 2 then message, "GRAFF_PUT_REC requires at least " + $
                                   '2 parameters' 

  sz = size(value)
  tcode = sz[sz[0]+1]

  if tcode eq 8 || tcode eq 10 || tcode eq 11 then begin
     message, /continue, "GRAFF_PUT_RECORD cannot write structures, " + $
              "pointers or objects"
     return
  endif

; Adjust the tag to 3 characters.

  if strlen(tag) eq 0 then message, "GRAFF_PUT_REC: tag is empty"
  if strlen(tag) gt 3 then $
     message, /continue, "GRAFF_PUT_RECORD Overlong tag truncated"
  wtag = '   '
  strput, wtag, tag

  if keyword_set(force_2d) && sz[0] eq 1 then $
     sz = [2l, sz[1], 1l, sz[2:*]]
  
  writeu, ilu, wtag, tcode, sz[0:sz[0]]
  if tcode eq 7 then writeu, ilu, strlen(value)
  if tcode ne 0 then writeu, ilu, value

end
