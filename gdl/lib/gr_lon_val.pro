; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_lon_val, tag_val, num

;+
; GR_LON_VAL
;	Return long value associated with a tag
;
; Return value:
;	value	long	The value associated with the tag.
;
; Arguments:
;	tag_val	string	input	The string containing the tag value.
;	num	int	input	How many elements the value has.
;
; History:
;	Original: 5/11/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;-

if (num eq 1) then val = 0l $
else val = lonarr(num)

reads, tag_val, val

return, val
end
