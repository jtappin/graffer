; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_str_val, line, tag

;+
; GR_STR_VAL
;	Return string value associated with a tag
;
; Return value:
;	value	string	The value associated with the tag.
;
; Arguments:
;	line	string	input	The string containing the tag
;	tag	string	input	The tag whose value is to be returned.
;
; History:
;	Original: 5/11/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;-

pos = strpos(line, tag+':')
if (pos eq -1) then return, ''

pos = pos+strlen(tag)+1
return, strmid(line, pos, strlen(line))

end
