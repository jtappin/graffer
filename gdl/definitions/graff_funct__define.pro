; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_funct__define
;+
; NAME:
;	graff_funct__define
;
;
; PURPOSE:
;	Define structure for a graffer function
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
;	Extracted 29/6/05; SJT
;-

xydata = {graff_funct,  $
          Range:dblarr(2), $
          Funct:''}

end
