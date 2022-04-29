; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_DRAW
;	Process a draw event in draw mode
;
; Usage:
;	ichange = graff_draw(pdefs, event, track_flag)
;
; Argument:
;	pdefs	struct	in/out	The plot definition structure.
;	event	struct	input	The draw event that triggered this
;	track_flag byte	input	A flag to say if it's a tracking event.
;
; History:
;	More-or-less a full rewrite: 28/4/22 et sqq: SJT
;-

function Graff_draw, pdefs, event, track_flag

  fl1d = ((*pdefs.data)[pdefs.cset].type ge 0 &&  $
          (*pdefs.data)[pdefs.cset].type le 8)

  if (track_flag) then begin
     if fl1d then $ 
        graff_msg, pdefs.ids.hlptxt, $
                   ['Left = add point, Middle = edit point, Right = ' + $
                    'delete point', $
                    'C-Left = insert point, S-Left = add at ' + $
                    'nearer end, C|S @ release = cancel.']
     return, 0
  endif

  gr_coord_convert, event.x, event.y, x, y, /device, /to_data
  xy = [x, y]
  exy = double([event.x, event.y])

; Just moving the cross hairs, this should be the commonest
; case and doesn't need us to extract data values etc. Get it
; out of the way and then return.
  
  if event.type eq 2 && pdefs.transient.mode ne 8 then begin
     gr_cross_hair, pdefs, xy[0:1]
     
     widget_control, pdefs.ids.xcp, set_value = xy[0]
     widget_control, pdefs.ids.ycp, set_value = xy[1]

     return, 0
  endif

  if ~fl1d then begin
     graff_msg, pdefs.ids.message,  $
                "Can't manipulate function or 2-D data"
     event.type = 0
     return, 0
  endif
  
  gr_xy_extract, pdefs, xvals, yvals, xerrs, yerrs, ndata = ndata, $
                 nerrs = nerrs, status = status

  if status ne 1 then begin
     graff_msg, pdefs.ids.message,  $ 
                "Failed to extract data values."
     return, 0
  endif
  
; In case of polar coordinates convert to cartesian coordinates and
; convert the event position to real data coordinates.
  
  case ((*pdefs.data)[pdefs.cset].mode) of
     0: begin
        xyd = xy
        xdatar = xvals
        ydatar = yvals
     end
     1: begin
        xyd = [sqrt(total(xy^2)), atan(xy[1], xy[0])]
        xdatar = xvals * cos(yvals)
        ydatar = xvals * sin(yvals)
     end
     2: begin
        xyd = [sqrt(total(xy^2)), atan(xy[1], xy[0])*!Radeg]
        xdatar = xvals * cos(yvals*!dtor)
        ydatar = xvals * sin(yvals*!dtor)
     end
  endcase

  ichange = 0

; Handle the press events first.

  case event.press of
     2: begin                   ; Middle button, initiate move
        
        if (ndata ge 1) then begin
           gr_nearest, xdatar, ydatar, event.x, event.y, imin, md, $
                       max = 5.d

           if imin ge 0 then begin
              pdefs.transient.imove = imin 
              gr_cross_hair, pdefs
              pdefs.transient.mode = 4
           endif else graff_msg, pdefs.ids.message, $
                                 "No datum within 5 pixels of " + $
                                 "selected location" 
        endif
     end 
     
     1: begin                   ; Left button, initial insert.
        
        if (event.modifiers and 2l) eq 2l && $
           ndata ge 2 then begin

                                ; If control is pressed, then insert
                                ; in nearest line segment.

           gr_nearest, xdatar, ydatar, event.x, event.y, imin, md, $
                       max = 5.d, /segment

           if imin ge 0 then begin
              gr_cross_hair, pdefs
              pdefs.transient.imove = imin
              pdef.transient.mode = 2
           endif
        endif else if (event.modifiers and 2l) eq 1l && $
           ndata ge 2 then begin

                                ; If shift is pressed, then attach
                                ; point at nearer end.
            
           gr_coord_convert, xdatar[[0, -1]], ydatar[[0, -1]], ddx, $
                             ddy, /data, /to_device

           d0 = sqrt((ddx[0]-event.x)^2+(ddy[0]-event.y)^2)
           d1 = sqrt((ddx[1]-event.x)^2+(ddy[1]-event.y)^2)

           if d0 lt d1 then pdefs.transient.imove = 0 $
           else pdefs.transient.imove = ndata

           gr_cross_hair, pdefs
           pdef.transient.mode = 2

        endif else begin
           pdefs.transient.imove = ndata

           gr_cross_hair, pdefs
           pdef.transient.mode = 2
        endelse
     end

     4: begin                   ; Right button select point to delete.
         
        gr_nearest, xdatar, ydatar, event.x, event.y, imin, md, $
                    max = 5.d

        if imin ge 0 then begin
           pdefs.transient.imove = imin 
           gr_cross_hair, pdefs
           pdefs.transient.mode = 8
        endif else graff_msg, pdefs.ids.message, $
                              "No datum within 5 pixels of " + $
                              "selected location" 
     end

     else:                      ; Not a press event.
  endcase

; Release events, commit the changes.

  case event.release of
     2: if pdefs.transient.mode eq 4 then begin ; Commit a move.
        
        gr_cross_hair, pdefs
        if (event.modifiers and 3) eq 0 then begin ; Pressing CTRL or
                                ; shift before release is cancel.

           xvals[pdefs.transient.imove] = xyd[0]
           yvals[pdefs.transient.imove] = xyd[1]

           gr_xy_replace, pdefs, xvals, yvals
        endif
        pdefs.transient.mode = 0
        pdefs.transient.imove = -1
        ichange = 1
     endif

     1: if pdefs.transient.mode eq 2 then begin ; Commit an addition
        
        gr_cross_hair, pdefs
        if (event.modifiers and 3) eq 0 then begin ; Pressing CTRL or
                                ; shift before release is cancel.

           case pdefs.transient.imove of
              0: begin
                 xvals = [xyd[0], xvals]
                 yvals = [xyd[1], yvals]

                 if nerrs[0] ne 0 then $
                    xerrs = [[dblarr(nerrs[0])], [xerrs]]
                 if nerrs[1] ne 0 then $
                    yerrs = [[dblarr(nerrs[1])], [yerrs]]
              end
              ndata: begin
                 xvals = [xvals, xyd[0]]
                 yvals = [yvals, xyd[1], yvals]

                 if nerrs[0] ne 0 then $
                    xerrs = [[xerrs], [dblarr(nerrs[0])]]
                 if nerrs[1] ne 0 then $
                    yerrs = [[yerrs], [dblarr(nerrs[1])]]
              end
              else: begin
                 imm = pdefs.transient.imove-1
                 imp = pdefs.transient.imove
                 
                 xvals = [xvals[0:imm], xyd[0], xvals[imp:*]]
                 yvals = [yvals[0:imm], xyd[1], yvals[imp:*]]

                 if nerrs[0] ne 0 then $
                    xerrs = [[xerrs[*, 0:imm]], $
                             [dblarr(nerrs[0])], $
                             [xerrs[*, imp:*]]]
                 if nerrs[1] ne 0 then $
                    yerrs = [[yerrs[*, 0:imm]], $
                             [dblarr(nerrs[1])], $
                             [yerrs[*, imp:*]]]
              end
           endcase
 
           gr_xy_replace, pdefs, xvals, yvals, xerr = xerrs, $
                          yerr = yerrs

           ichange = 1
        endif
        pdefs.transient.imove = -1
        pdefs.transient.mode = 0
     endif

     4: if pdefs.transient.mode eq 8 then begin ; Commit a delete.

        gr_cross_hair, pdefs
        if (event.modifiers and 3) eq 0 && $
           pdefs.transient.imove ge 0 then begin ; Pressing CTRL or
                                ; shift before release is cancel.

           case pdefs.transient.imove of
              0: begin          ; First point
                 xvals = xvals[1:*]
                 yvals = yvals[1:*]

                 if nerrs[0] ne 0 then xerrs = xerrs[*, 1:*]
                 if nerrs[1] ne 0 then yerrs = yerrs[*, 1:*]
              end

              ndata-1: begin    ; Last point
                 xvals = xvals[0:ndata-2]
                 yvals = yvals[0:ndata-2]
                 
                 if nerrs[0] ne 0 then xerrs = xerrs[*, 0:ndata-2]
                 if nerrs[1] ne 0 then yerrs = yerrs[*, 0:ndata-2]
              end

              else: begin       ; any other
                 imm = pdefs.transient.imove-1
                 imp = pdefs.transient.imove
                 
                 xvals = [xvals[0:imm], xvals[imp:*]]
                 yvals = [yvals[0:imm], yvals[imp:*]]

                 if nerrs[0] ne 0 then $
                    xerrs = [[xerrs[*, 0:imm]], $
                             [xerrs[*, imp:*]]]
                 if nerrs[1] ne 0 then $
                    yerrs = [[yerrs[*, 0:imm]], $
                             [yerrs[*, imp:*]]]
              end
           endcase
           gr_xy_replace, pdefs, xvals, yvals, xerr = xerrs, $
                          yerr = yerrs

           ichange = 1
        endif
        pdefs.transient.imove = -1
        pdefs.transient.mode = 0
     endif

     else:                      ; Not a handled release event.
  endcase

  if event.type eq 2 && pdefs.transient.mode eq 8 then begin
     gr_nearest, xdatar, ydatar, xd, yd, ddx, ddy, max = 5.
        
     if imin eq -1 then begin
        pdefs.transient.imove = -1
        gr_cross_hair, pdefs
     endif else if imin ne pdefs.transient.imove then begin
        pdefs.transient.imove = imin
        gr_cross_hair, pdefs, [ddx[imin], ddy[imin]]
     endif
  endif
  
  widget_control, pdefs.ids.export, sensitive = $
                  (*pdefs.data)[pdefs.cset].type ge 0 && $ 
                  (*pdefs.data)[pdefs.cset].ndata gt 0

        
end
