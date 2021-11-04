; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_zdata__define
;+
; NAME:
;	graff_zdata__define
;
;
; PURPOSE:
;	Define structure for  graffer 2D data.
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

xydata = {graff_zdata,  $
          Z: ptr_new(), $
          X: ptr_new(), $
          Y: ptr_new(), $
          x_is_2d: 0b, $
          y_is_2d: 0b $
         }
end
