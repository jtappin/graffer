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
       Use:       0b, $
       side:      0b, $
       title: '' $
      }

end
