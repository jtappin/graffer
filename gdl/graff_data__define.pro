pro graff_data__define
;+
; NAME:
;	graff_data__define
;
;
; PURPOSE:
;	Create a GRAFFER dataset structure
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	data={graff_data}
;
;
; MODIFICATION HISTORY:
;	Extracted: 30/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;-

data =  {graff_data, $
         Ndata:    0l, $
         Ndata2:   0l, $        ; Only used in 2D Dss
         Type:     0, $
         Mode:     0, $
         Xydata:   ptr_new(), $
         Descript: "", $
         Pline:    0, $
         Psym:     0, $
         Symsize:  0., $
         Line:     0, $
         Colour:   0, $
         Thick:    0., $
         Zopts:    {graff_2d}, $
         y_axis:   0, $
         Sort:     0b, $
         Noclip:   0b, $
         Medit:    0b}
end
