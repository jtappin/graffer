function truth, val, null_true = null_true

; LICENCE:
; Copyright (C) 2015-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   


;+
; TRUTH
;	Interpret a "truth" string.
;
; Usage:
;	lv = truth(val)
;
; Returns:
;	0b or 1b according to the value of the string.
;
; Argument:
;	val	string	A string with a "truth" value.
;
; Keyword:
;	/null_true	If set, then an empty string is treated as TRUE,
;			otherwise it is FALSE
; Notes:
;	Structures, pointers and objects return INVALID (2b).
;	Numeric types, return as IDL treats them.
;	Strings:
;	True, T, Yes, Y, 1 - return true.
;	False, F, No, N, 0 - return false.
;	Other values return 2b, Case doesn't matter.
;
; History:
;	Original: 3/8/15; SJT
;	Strip leading & trailing spaces: 14/11/22; SJT
;	Add /null_true and make null string valid: 13/3/23; SJT
;-

  type = size(val, /type)

  switch type of
     8:
     10:
     11: return, 2b             ; Structures, pointers & Objects are
                                ; invalid

     7: begin
        switch strupcase(strtrim(val, 2)) of
           '': return, keyword_set(null_true)
           
           'TRUE':
           'T':
           '.TRUE.':
           'YES':
           'Y':
           '1': return, 1b

           'FALSE':
           'F':
           '.FALSE.':
           'NO':
           'N':
           '0': return, 0b

           else: return, 2b
        endswitch
     end

     else: return, val ? 1b : 0b
  endswitch

end
