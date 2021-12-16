; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_dbl_val, tag_val, num

;+
; GR_DBL_VAL
;	Return double precision value associated with a tag
;
; Return value:
;	value	double	The value associated with the tag.
;
; Arguments:
;	tag_val	string	input	The string containing the tag value.
;	num	int	input	How many elements the value has.
;
; History:
;	Original (after flt version): 30/6/05; SJT
;-

if (num eq 1) then val = 0.d0 $
else val = dblarr(num)

reads, tag_val, val

return, val
end
