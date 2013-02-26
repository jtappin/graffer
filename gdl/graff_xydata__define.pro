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
            xerr: ptr_new(), $
            yerr: ptr_new()}

end
