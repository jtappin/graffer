; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_int_val, tag_val, num

;+
; GR_INT_VAL
;	Return int value associated with a tag
;
; Return value:
;	value	int	The value associated with the tag.
;
; Arguments:
;	tag_val	string	input	The string containing the tag value.
;	num	int	input	How many elements the value has.
;
; History:
;	Original: 5/11/96; SJT
;-

if (num eq 1) then val = 0 $
else val = intarr(num)

reads, tag_val, val

return, val
end
