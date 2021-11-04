; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_opts__define
;+
; NAME:
;	graff_opts__define
;
;
; PURPOSE:
;	Define a graffer default options structure
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	implicit
;
;
; MODIFICATION HISTORY:
;	Extracted: 30/6/05; SJT
;	Remove (fully) colour_menu: 21/5/20; SJT
;	Add track item (for tracking events): 18/8/21; SJT
;-

  optblock = {graff_opts, $
              Auto_delay:  0., $
              S2d:         0b, $
              Mouse:       0b, $
              track:       0b, $
              bitmaps:     0b, $
              pdfviewer:   '' $
             }

end
