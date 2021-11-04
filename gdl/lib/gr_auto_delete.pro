; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

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
