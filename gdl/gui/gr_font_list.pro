; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_font_list, family, list, nos

;+
; GR_FONT_LIST
;	Return the valid fonts and indices for a given font family.
;
; Usage:
;	gr_font_list, family, list, nos
;
; Arguments:
; 	family	int	input	The family (as in !p.font).
; 	list	string	output	The names of the fonts.
; 	nos	int	output	The font numbers (as in the !<n> controls).
;
; History:
;	Original: 11/1/12; SJT
;-

case family of
    -1: begin                   ; Hershey (vector) fonts
        list = ['Simplex Roman', $
                'Simplex Greek',  $
                'Duplex Roman',  $
                'Complex Roman',  $
                'Complex Greek',  $
                'Complex Italic',  $
                'Maths & Special',  $
                'Special',  $
                'Gothic English',  $
                'Simplex Script',  $
                'Complex Script',  $
                'Gothic Italian',  $
                'Gothic German',  $
                'Cyrillic',  $
                'Triplex Roman',  $
                'Triplex Italic',  $
                'Miscellaneous']
        nos = [indgen(16)+3, 20] ; No font 19
    end
    0: begin                    ; Device (PS) fonts
        list = ['Helvetica', $
                'Helvetica Bold', $
                'Helvetica Italic', $
                'Helvetica B+I', $
                'Times', $
                'Times Italic', $
                'Symbol', $
                'Courier', $
                'Courier Italic', $
                'Courier Bold', $
                'Courier B+I', $
                'Times Bold', $
                'Times B+I']
        nos = [indgen(7)+3, indgen(6)+11]
    end
    1: begin
        list = ['Helvetica', $
                'Helvetica Bold', $
                'Helvetica Narrow', $
                'Helvetica N+B+O', $
                'Times Roman', $
                'Times B+I', $
                'Symbol', $
                'Zapf Dingbats', $
                'Courier', $
                'Courier Oblique', $
                'Palatino', $
                'Palatino Italic', $
                'Palatino Bold', $
                'Palatino B+I', $
                'Avant Garde Book', $
                'N.C. Schoolbook', $
                'N.C. Schoolbook Bold']
        nos = indgen(17)+3
    end
    else:                       ; Invalid family
endcase

end
