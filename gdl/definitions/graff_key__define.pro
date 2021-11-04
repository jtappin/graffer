; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_key__define
;+
; NAME:
;	graff_text__define
;
;
; PURPOSE:
;	Define the graffer text style structure
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
;	Add char size: 29/4/09; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Support for reversed key listing: 16/7/21
;-

Key = {graff_key, $
       X:         dblarr(2),  $
       Y:         dblarr(2), $
       csize:     0.d0, $
       Norm:      0, $ 
       cols:      0, $
       List:      ptr_new(),  $
       Frame:     0b, $
       One_point: 0b, $
       reverse:   0b, $
       Use:       0b, $
       side:      0b, $
       title: '' $
      }

end
