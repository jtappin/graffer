; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_xydata__define
;+
; NAME:
;	graff_xydata__define
;
;
; PURPOSE:
;	Define structure for  graffer 1D data.
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
;	Original: 16/7/12; SJT
;-


  xydata = {graff_xydata, $
            x: ptr_new(), $
            y: ptr_new(), $
            x_err: ptr_new(), $
            y_err: ptr_new()}

end
