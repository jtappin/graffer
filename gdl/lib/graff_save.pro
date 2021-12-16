; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_save, pdefs
;+
; NAME:
;	GRAFF_SAVE
;
;
; PURPOSE:
;	Save a graffer file in its current format.
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	graff_save,pdefs
;
;
; INPUTS:
;	pdefs	struct	The graffer plot object structure.
;
;
; MODIFICATION HISTORY:
;	Original: 1/7/05; SJT
;-

case pdefs.is_ascii of
    0b: gr_bin_save, pdefs
    1b: gr_asc_save, pdefs
endcase

end
