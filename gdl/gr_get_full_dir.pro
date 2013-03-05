; Copyright (C) 2013
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

function Gr_get_full_dir, dir

;+
; GR_GET_FULL_DIR
;	Get the full pathname of a directory
;
; Usage:
;	path = gr_get_full_dir(dir)
;
; Return value:
;	path	string	The full pathname of the specfied directory
;
; Argument:
;	dir	string	input	The directory to be interpreted.
;
; History:
;	Original: 17/1/97; SJT
;-

cd, dir, current = here
cd, here, current = path
separator = path_sep()

if (strpos(path, separator, /reverse_search) ne strlen(path)-1) then $
  path = path+separator 

return, path

end

