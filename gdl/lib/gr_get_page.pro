; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_get_page, ps, ori

if (ps eq 0) then sz = [21., 29.7] $
else sz = [21.59, 27.94]
if (ori eq 0) then sz = sz([1, 0])

return, sz
end
