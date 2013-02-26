pro graff_trans__define
;+
; NAME:
;	graff_hard__define
;
;
; PURPOSE:
;	Define the graffer transient properties structure
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
;	Add current only option: 26/1/12; SJT
;	Add number of discrete colours: 8/2/12; SJT
;-

Transient = { graff_trans, $
              Opos:     dblarr(2), $
              Imove:    0l, $
              mode:     0, $
              Changes:  0, $
              colmin:   0, $
              hairs:    0b, $
              Opflag:   0b, $
              backup:   0b, $
              current_only: 0b $
            }

end
