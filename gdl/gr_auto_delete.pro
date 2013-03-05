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

pro Gr_auto_delete, pdefs

;+
; GR_AUTO_DELETE
;	Delete a GRAFFER autosave file.
;
; Usage:
;	gr_auto_delete, pdefs
;
; Argument:
;	pdefs	struct	input	The GRAFFER data structure.
;
; History:
;	Original (out of GRAFF_EVENT): 9/10/96; SJT
;-

on_ioerror, no_auto
openr, ilu, /get, /delete, $    ; Delete the auto save file.
  pdefs.dir+'#'+pdefs.name+'#'
free_lun, ilu

No_auto:
on_ioerror, null

end
