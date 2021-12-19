; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_td_mode, value, pdefs

;+
; GR_TD_MODE
;	Swap between text and draw modes of the mouse
;
; Usage:
;	gr_td_mode, value, pdefs
;
; Arguments:
;	value	int	input	The mode (0 = draw, 1 = text)
;	pdefs	struct	input	The GRAFFER control structure
;
; History:
;	Original (extracted from GRAFFER): 27/1/97; SJT
;-

  common graffer_options, optblock


  gr_cross_hair, pdefs          ; Erase any existing cross hairs.

  if (value) then begin
     widget_control, pdefs.ids.draw, set_uvalue = 'WRITE', $
                     /draw_button_events, track = optblock.track
  endif else begin
     widget_control, pdefs.ids.draw, set_uvalue = 'DRAW', $
                     draw_button_events = $
                     (*pdefs.data)[pdefs.cset].medit, $
                     track = $
                     (*pdefs.data)[pdefs.cset].medit && optblock.track
  endelse
  pdefs.transient.mode = value

end
