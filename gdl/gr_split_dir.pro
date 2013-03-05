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

pro Gr_split_dir, file, dir

;+
; GR_SPLIT_DIR
;	Separate a file name into a directory and a file name.
;
; Usage:
;	gr_split_dir, file, dir
;
; Arguments:
;	file	string	in/out	On entry the full filename, on exit
;				the filename part.
;	dir	string	output	The directory name.
;
; History:
;	Original: 18/8/95; SJT
;	Rename as GR_SPLIT_DIR (was split_dir): 18/9/96; SJT
;	Use file_dirname & file_basename: 15/2/12; SJT
;-

if (n_elements(dir) eq 0) then dir = ''

dir = file_dirname(file, /mark)
file = file_basename(file)

end
