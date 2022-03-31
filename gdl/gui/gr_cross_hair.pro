; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_cross_hair, pdefs, xy

;+
; Draw a cross hair according to the text or graphical mode.
;
; Usage:
;	gr_cross_hair, pdefs, xy
;
; Arguments:
;	pdefs	struct	input	The graffer control structure.
;	xy	double	input	The location of the interception.
; 
; Notes:
; 	If a position is not given, then the old-position is drawn,
; 	otherwise, the old is drawn, then the new, and the old
; 	coordinate is updated.
;
; History:
;	Original: 10/1/12; SJT
;	Add new mouse-edit modes: 26/1/12; SJT
;	Disable cross hairs in GDL: 31/3/22; SJT
;-

  if is_gdl() then return            ; GDL does not (currently)
                                ; support xor and invert modes due to
                                ; a cairo limitation. Should not get
                                ; called but this is a safety lock.
  
  device, set_graphics_function = 10 ; Set graphics function to
                                ; Invert mode

  case pdefs.transient.mode of
     0: begin                   ; Normal mode, draw solid
                                ; cross hairs out to the axes
        if (pdefs.y_right && (*pdefs.data)[pdefs.cset].y_axis eq 1) $
        then ytype = pdefs.ytype_r $
        else ytype = pdefs.ytype

        if pdefs.transient.opflag then begin
           if (pdefs.xtype) then plots, 10.^!X.crange, $
                                        replicate(pdefs.transient.opos[1], 2) $
           else plots, !X.crange, replicate(pdefs.transient.opos[1], 2)
           if (ytype) then plots, $
              replicate(pdefs.transient.opos[0], 2), 10.^!Y.crange $
           else plots, replicate(pdefs.transient.opos[0], 2), $
                       !Y.crange
           pdefs.transient.opflag = 0b
        endif
        if n_params() eq 2 and pdefs.transient.hairs then begin
           if (pdefs.xtype) then plots, 10.^!X.crange, $
                                        replicate(xy[1], 2) $
           else plots, !X.crange, replicate(xy[1], 2)
           if (ytype) then plots, replicate(xy[0], 2), $
                                  10.^!Y.crange $
           else plots, replicate(xy[0], 2), !Y.crange
           
           pdefs.transient.opos = xy[0:1]
           pdefs.transient.opflag = 1b
        endif
     end 
     1: begin                   ; Text annotation mode, draw
                                ; dashed cross hairs out the edge of
                                ; the window.
        if !p.region[0] eq !p.region[2] then begin
           xx = [0., 1.]
           yy = [0., 1.]
        endif else begin
           xx = !p.region[[0, 2]]
           yy = !p.region[[1, 3]]
        endelse
        if (pdefs.transient.opflag) then begin
           plots, /norm, xx, $
                  replicate(pdefs.transient.opos[1], 2), linesty = 2 
           plots, /norm, linesty = 2, $
                  replicate(pdefs.transient.opos[0], 2), yy
           pdefs.transient.opflag = 0b
        endif
        if n_params() eq 2 and pdefs.transient.hairs then begin
           plots, /norm, xx, replicate(xy[1], 2), linesty = 2
           plots, /norm, replicate(xy[0], 2), yy, linesty = 2
           
           pdefs.transient.opos = xy[0:1]
           pdefs.transient.opflag = 1b
        endif
     end
     2: begin                   ; Add point mode, draw the
                                ; new line segment
        lp = (*pdefs.data)[pdefs.cset].ndata-1
        xy0 = (*(*pdefs.data)[pdefs.cset].xydata)[0:1, lp]
        if (pdefs.transient.opflag) then begin
           plots, [xy0[0], pdefs.transient.opos[0]], $
                  [xy0[1], pdefs.transient.opos[1]]
           pdefs.transient.opflag = 0b
        endif
        if n_params() eq 2 then begin
           plots, [xy0[0], xy[0]], [xy0[1], xy[1]]
           pdefs.transient.opos = xy[0:1]
           pdefs.transient.opflag = 1b
        endif
     end
     4: begin                   ; Move point mode, draw the new line
                                ; segment(s).
        lp = (*pdefs.data)[pdefs.cset].ndata-1
        if pdefs.transient.imove eq 0 then begin
           xy0 = (*(*pdefs.data)[pdefs.cset].xydata)[0:1, 1]
           if (pdefs.transient.opflag) then begin
              plots, [xy0[0], pdefs.transient.opos[0]], $
                     [xy0[1], pdefs.transient.opos[1]]
              pdefs.transient.opflag = 0b
           endif
           if n_params() eq 2 then begin
              plots, [xy0[0], xy[0]], [xy0[1], xy[1]]
              pdefs.transient.opos = xy[0:1]
              pdefs.transient.opflag = 1b
           endif
        endif else if pdefs.transient.imove eq lp then begin
           xy0 = (*(*pdefs.data)[pdefs.cset].xydata)[0:1, lp-1]
           if (pdefs.transient.opflag) then begin
              plots, [xy0[0], pdefs.transient.opos[0]], $
                     [xy0[1], pdefs.transient.opos[1]]
              pdefs.transient.opflag = 0b
           endif
           if n_params() eq 2 then begin
              plots, [xy0[0], xy[0]], [xy0[1], xy[1]]
              pdefs.transient.opos = xy[0:1]
              pdefs.transient.opflag = 1b
           endif
        endif else begin 
           im = pdefs.transient.imove
           xy0 = (*(*pdefs.data)[pdefs.cset].xydata)[0:1, im-1]
           xy1 = (*(*pdefs.data)[pdefs.cset].xydata)[0:1, im+1]
           if (pdefs.transient.opflag) then begin
              plots, [xy0[0], pdefs.transient.opos[0], xy1[0]], $
                     [xy0[1], pdefs.transient.opos[1], xy1[1]]
              pdefs.transient.opflag = 0b
           endif
           if n_params() eq 2 then begin
              plots, [xy0[0], xy[0], xy1[0]], [xy0[1], xy[1], xy1[1]]
              pdefs.transient.opos = xy[0:1]
              pdefs.transient.opflag = 1b
           endif
        endelse
     end 
     8: begin                   ; Delete point mode, circle
                                ; the selected point.
        th = !dpi*dindgen(73)/36.
        dx = sin(th)*10.d
        dy = cos(th)*10.d
        if (pdefs.transient.opflag) then begin
           plots, /device, pdefs.transient.opos[0]+dx, $
                  pdefs.transient.opos[1]+dy 
           pdefs.transient.opflag = 0b
        endif
        if n_params() eq 2 then begin
           plots, /device, xy[0]+dx, xy[1]+dy
           pdefs.transient.opos = xy[0:1]
           pdefs.transient.opflag = 1b
        endif
     end

  endcase

  device, set_graphics_function = 3 ; Set graphics function
                                ; back to Source mode (normal)

end
