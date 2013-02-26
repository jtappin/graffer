pro graff_hard__define
;+
; NAME:
;	graff_hard__define
;
;
; PURPOSE:
;	Define the graffer hardcopy style structure
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
;-

Hardset = { graff_hard, $
            Colour: 0b, $
            Eps:    0b, $
            Orient: 0b, $
            Psize:  0b, $
            Timestamp: 0b, $
            cmyk: 0b, $
            Font:   {graff_hfont,  $
                     Family: 0, $
                     Wg_sl:  0}, $
            Size:   fltarr(2), $
            Off:    fltarr(2), $
            Action: strarr(2), $
            viewer: strarr(2), $
            name: '' $
          }

end
