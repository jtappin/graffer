; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_cf_pieces, x, a, y

;+
; GR_CF_PIECES
;	A Procedure interface to GR_PIECES to keep CURVEFIT happy!
;
; As we don't know which will get called first they have to be in
; separate files!
;-

y = gr_pieces(x, a)
end
