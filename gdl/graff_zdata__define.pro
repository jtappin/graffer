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
