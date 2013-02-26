pro graff_text__define
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
;	Add axis choice: 28/12/11; SJT
;	Add ID: 13/6/12; SJT
;-

Text_options = {graff_text, $
                id: '', $
                Text:    '', $
                Colour:  0, $
                Size:    0., $
                Orient:  0., $
                Align:   0., $
                ffamily: 0, $
                Font:    0, $
                Thick:   0., $
                X:       0.d0, $
                Y:       0.d0, $
                Norm:    0b, $
                axis:    0 $
               }

end
