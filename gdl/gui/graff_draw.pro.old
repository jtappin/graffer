; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Graff_draw, pdefs, event, track_flag

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
;	Carved from graffer: 17/8/95; SJT
;	Modify to cope with function data (by ignoring it): 18/8/95;
;	SJT
;	Add tracking event handling: 5/12/95; SJT
;	Handle data with error bars: 28/11/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Fix "crash" bug with polar functions: 21/1/09; SJT
;	Fix problem with single-point datasets entered by mouse:
;	18/11/11; SJT
;	Show new lines when adding or moving points by mouse and also
;	allow actions to be cancelled by pressing ctrl or shift
;	before release: 26/1/12; SJT
;	Add insert mode & improve deletion handling: 8/2/12; SJT
;	Fixed distance computation in insert mode (I think): 27/3/12; SJT
;-

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

  nxe = 0l
  nye = 0l
  if ptr_valid((*pdefs.data)[pdefs.cset].xydata) then begin
     xvals = *(*(*pdefs.data)[pdefs.cset].xydata).x
     yvals = *(*(*pdefs.data)[pdefs.cset].xydata).y
     if ptr_valid((*(*pdefs.data)[pdefs.cset].xydata).x_err) then begin
        xerrs = *(*(*pdefs.data)[pdefs.cset].xydata).x_err
        nxe = n_elements(xerrs[*, 0])
     endif
     if ptr_valid((*(*pdefs.data)[pdefs.cset].xydata).y_err) then begin
        yerrs = *(*(*pdefs.data)[pdefs.cset].xydata).y_err
        nye = n_elements(yerrs[*, 0])
     endif
  endif else begin
     xvals = dblarr(2)
     yvals = dblarr(2)
  endelse
  
  svflag = 0

  if ((*pdefs.data)[pdefs.cset].type lt 0 or $
      (*pdefs.data)[pdefs.cset].type ge 9) then begin ; Function, can't
                                ; manipulate it
                                ; Re-save and exit
     
     if (event.type ne 2) then begin
        graff_msg, pdefs.ids.message,  $
                   "Can't manipulate function or 2-D data"
        event.type = 0
        return, 0
     endif    
  endif else ncols = ([2, 3, 4, 3, 4, 4, 5, 5, $
                       6])((*pdefs.data)[pdefs.cset].type) 

  if fl1d then case ((*pdefs.data)[pdefs.cset].mode) of
     0: begin
        xyd = xy
        xdatar = xvals
        ydatar = yvals
     end
     1: begin
        xyd = [sqrt(total(xy^2)), atan(xy[1], xy[0])]
        xdatar = xdatar * cos(ydatar)
        ydatar = xdatar * sin(ydatar)
     end
     2: begin
        xyd = [sqrt(total(xy^2)), atan(xy[1], xy[0])*!Radeg]
        xdatar = xdatar * cos(ydatar*!dtor)
        ydatar = xdatar * sin(ydatar*!dtor)
     end
  endcase

  ndata = (*pdefs.data)[pdefs.cset].ndata
  ichange = 0
  case event.type of
     0: if fl1d then begin      ; Press event (only used for moves, 
                                ; and to change X-hair mode)
        case event.press of
           2: begin             ; Middle button, initiate move
              
              if (ndata ge 1) then begin
                 gr_coord_convert, xdatar, ydatar, $
                                   ddx, ddy, /data, /to_device
                 dist = sqrt((ddx-event.x)^2 + $
                             (ddy-event.y)^2)
                 md = min(dist, imin)
                 if (md lt 5) then begin
                    pdefs.transient.imove = imin 
                    gr_cross_hair, pdefs
                    pdefs.transient.mode = 4
                 endif else graff_msg, pdefs.ids.message, $
                                       "No datum within 5 pixels of " + $
                                       "selected location" 
              endif
           end 
           1: begin             ; Left button, place point.
                                ; (changes the cross hair mode,
                                ; and if the control key is pressed, then
                                ; selects insert mode)
              if (ndata ge 1) then begin
                 if (event.modifiers and 2l) eq 2 and ndata ge 2 then $
                    begin
                                ; Control, insert between points.
                    off = dblarr(ndata-1)
                    gr_coord_convert, xdatar, ydatar, ddx, ddy, $
                                      /data, /to_device
                    for j = 0, ndata-2 do begin
                       xl = ddx[j+1] < ddx[j]
                       xu = ddx[j+1] > ddx[j]
                       if (xu - xl lt 10) then begin
                          tmp = (xl+xu)/2.
                          xl = tmp-5.
                          xu = tmp+5.
                       endif
                       yl = ddy[j+1] < ddy[j]
                       yu = ddy[j+1] > ddy[j]
                       if (yu - yl lt 10) then begin
                          tmp = (yl+yu)/2.
                          yl = tmp-5.
                          yu = tmp+5.
                       endif
                       if (exy[0] ge xl && exy[0] le xu && $
                           exy[1] ge yl && exy[1] le yu) then begin
                          if (ddy[j+1] eq ddy[j]) then $
                             off[j] = abs(ddy[j]-exy[1]) $
                          else if (ddx[j+1] eq ddx[j]) then $
                             off[j] = abs(ddx[j]-exy[0]) $
                          else begin
                             grad = (ddy[j+1]-ddy[j])/ $
                                    (ddx[j+1]-ddx[j])
                             yint = ddy[j]-grad*ddx[j]
                             yp = exy[0]*grad + yint
                             xp = (exy[1]-yint)/grad
                             dy = yp-exy[1]
                             dx = xp-exy[0]
                             off[j] = abs(dx*dy)/sqrt(dx^2+dy^2)
                          endelse
                       endif else off[j] = !values.d_infinity
                    endfor
                    omin = min(off, imin)
                    if omin lt 5. then begin
                       xvals = [xvals[0:imin], xyd[0], xvals[imin+1:*]]
                       yvals = [yvals[0:imin], xyd[1], yvals[imin+1:*]]
                       if nxe gt 0 then $
                          xerrs = [[xerrs[*, 0:imin]], [dblarr(nxe)], $
                                   [xerrs[*, imin+1:*]]]
                       if nye gt 0 then $
                          yerrs = [[yerrs[*, 0:imin]], [dblarr(nye)], $
                                   [yerrs[*, imin+1:*]]]

                       xydata = {graff_xydata}
                       xydata.x = ptr_new(xvals)
                       xydata.y = ptr_new(yvals)
                       if nxe gt 0 then xydata.x_err = ptr_new(xerrs)
                       if nye gt 0 then xydata.y_err = ptr_new(yerrs)
                       
                       (*pdefs.data)[pdefs.cset].ndata ++
                       pdefs.transient.imove = imin+1
                       ptr_free, $
                          (*(*pdefs.data)[pdefs.cset].xydata).x, $ $
                          (*(*pdefs.data)[pdefs.cset].xydata).y, $ $
                          (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                          (*(*pdefs.data)[pdefs.cset].xydata).y_err
                       ptr_free, (*pdefs.data)[pdefs.cset].xydata
                       
                       (*pdefs.data)[pdefs.cset].xydata = $
                          ptr_new(xydata) 
                       gr_cross_hair, pdefs
                       pdefs.transient.mode = 4
                    endif else graff_msg, pdefs.ids.message, $
                                          "No segment is within 5 " + $
                                          "pixels of selected location" 
                 endif else if (event.modifiers and 1l) eq 1 and $
                    ndata ge 2 then begin
                                ; Shift at selected end
                    gr_coord_convert, xdatar, ydatar, ddx, ddy, $
                                      /data, /to_device
                    xy0 = [ddx[0],  ddy[0]]
                    xy1 = [ddx[ndata-1], ddy[ndata-1]]
                    d0 = sqrt(total((xy0-exy)^2))
                    d1 = sqrt(total((xy1-exy)^2))
                    if d0 lt d1 then begin
                       xvals = [xyd[0], xvals]
                       yvals = [xyd[1], yvals]
                       if nxe gt 0 then $
                          xerrs = [[dblarr(nxe)], [xerrs]]
                       if nye gt 0 then $
                          yerrs = [[dblarr(nye)], [yerrs]]
                       
                       xydata = {graff_xydata}
                       xydata.x = ptr_new(xvals)
                       xydata.y = ptr_new(yvals)
                       if nxe gt 0 then xydata.x_err = ptr_new(xerrs)
                       if nye gt 0 then xydata.y_err = ptr_new(yerrs)

                       (*pdefs.data)[pdefs.cset].ndata ++
                       pdefs.transient.imove = 0
                       
                       ptr_free, $
                          (*(*pdefs.data)[pdefs.cset].xydata).x, $ $
                          (*(*pdefs.data)[pdefs.cset].xydata).y, $ $
                          (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                          (*(*pdefs.data)[pdefs.cset].xydata).y_err
                       ptr_free, (*pdefs.data)[pdefs.cset].xydata

                       (*pdefs.data)[pdefs.cset].xydata = $
                          ptr_new(xydata) 
                       gr_cross_hair, pdefs
                       pdefs.transient.mode = 4
                    endif else begin
                                ; No mod at end.
                       gr_cross_hair, pdefs
                       pdefs.transient.mode = 2
                    endelse
                 endif else begin
                    gr_cross_hair, pdefs
                    pdefs.transient.mode = 2
                 endelse
              endif
           end
           4: begin 
              if (ndata ge 1) then begin
                 gr_coord_convert, xdatar, ydatar, $
                                   ddx, ddy, /data, /to_device
                 dist = sqrt((ddx-event.x)^2 + $
                             (ddy-event.y)^2)
                 md = min(dist, imin)
                 if (md lt 5) then $
                    pdefs.transient.imove = imin $
                 else  pdefs.transient.imove = -1
                 gr_cross_hair, pdefs
                 pdefs.transient.mode = 8
                 if pdefs.transient.imove ne -1 then $
                    gr_cross_hair, pdefs, [ddx[imin], ddy[imin]]
              endif
           end

           else:
        endcase
     end
     
     1: if fl1d then begin      ; Release event
        
        case event.release of
           1: begin             ; Left: add a point
              gr_cross_hair, pdefs

              if (event.modifiers and 3) eq 0 then begin ; Make the inset
                 ndl = dblarr(ncols)
                 ndl(0) = xyd
                 old_data = (*pdefs.data)[pdefs.cset]
                 ;; old_x = xvals
                 ;; old_y = yvals
                 ;; if nxe gt 0 then old_xerr = xerrs
                 ;; it nye gt 0 then old_yerr = yerrs
                 
                 if pdefs.transient.mode eq 4 then begin
                    xvals[pdefs.transient.imove] = xyd[0]
                    yvals[pdefs.transient.imove] = xyd[1]
                    line = pdefs.transient.imove
                 endif else begin
                    if (ndata le 1) then begin
                       xvals[ndata] = xyd[0]
                       yvals[ndata] = xyd[1]
                       if nxe gt 0 then xerrs[*, ndata] = 0.d
                       if nye gt 0 then yerrs[*, ndata] = 0.d
                    endif else begin
                       xvals = [xvals, xyd[0]]
                       yvals = [yvals, xyd[1]]
                       if nxe gt 0 then xerrs = [[xerrs], [dblarr(nxe)]]
                       if nye gt 0 then yerrs = [[yerrs], [dblarr(nye)]]
                    endelse
                    (*pdefs.data)[pdefs.cset].ndata ++
                    line = (*pdefs.data)[pdefs.cset].ndata-1
                 endelse
                 
                 if ((*pdefs.data)[pdefs.cset].type ne 0) then begin
                    xydata = {graff_xydata}
                    xydata.x = ptr_new(xvals)
                    xydata.y = ptr_new(yvals)
                    if nxe gt 0 then xydata.x_err = ptr_new(xerrs)
                    if nye gt 0 then xydata.y_err = ptr_new(yerrs)
                    
                    ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $
                              (*(*pdefs.data)[pdefs.cset].xydata).y, $
                              (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                              $
                              (*(*pdefs.data)[pdefs.cset].xydata).y_err
                    
                    ptr_free, (*pdefs.data)[pdefs.cset].xydata
                    
                    (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
                    svflag = 1
                    ichange = gr_xy_wid(pdefs, line = line)
                    if (ichange eq 0) then begin
;                       *old_data.xydata = old_xy
                       (*pdefs.data)[pdefs.cset] = old_data
                    endif
                 endif else ichange = 1
              endif else if (pdefs.transient.mode eq 4) then begin
                                ; Cancel an insert
                 (*pdefs.data)[pdefs.cset].ndata --
                 imove = pdefs.transient.imove
                 xydata = *(*pdefs.data)[pdefs.cset].xydata
                 if imove eq 0 then begin
                    *xydata.x = (*xydata.x)[1:*]
                    *xydata.y = (*xydata.y)[1:*]
                    if ptr_valid(xydata.x_err) then $
                       *xydata.x_err = (*xydata.x_err)[*, 1:*]
                    if ptr_valid(xydata.y_err) then $
                       *xydata.y_err = (*xydata.y_err)[*, 1:*]
                 endif else begin
                    *xydata.x = [(*xydata.x)[0:imove-1], $
                                 (*xydata.x)[imove+1:*]]
                    *xydata.y = [(*xydata.y)[0:imove-1], $
                                 (*xydata.y)[imove+1:*]]
                    if ptr_valid(xydata.x_err) then $
                       *xydata.x_err = [[(*xydata.x_err)[*, $
                                                         0:imove-1]], $
                                        [(*xydata.x_err)[*, imove+1:*]]]
                    if ptr_valid(xydata.y_err) then $
                       *xydata.y_err = [[(*xydata.y_err)[*, $
                                                         0:imove-1]], $
                                        [(*xydata.y_err)[*, imove+1:*]]]))
                 endelse
                 
                 ptr_free, (*(*pdefs.data)[pdefs.cset].xydata).x, $ $
                           (*(*pdefs.data)[pdefs.cset].xydata).y, $
                           (*(*pdefs.data)[pdefs.cset].xydata).x_err, $
                           (*(*pdefs.data)[pdefs.cset].xydata).y_err
                 ptr_free, (*pdefs.data)[pdefs.cset].xydata
                 (*pdefs.data)[pdefs.cset].xydata = $
                    ptr_new(xydata)
                 
                 gr_cross_hair, pdefs
                 pdefs.transient.imove = -1
              endif
              pdefs.transient.mode = 0
           end
           2: if (ndata ne 0 and $
                  pdefs.transient.imove ne -1) then begin
                                ; Centre: move one
              gr_cross_hair, pdefs

              if (event.modifiers and 3) eq 0 then begin
                 old_data = (*pdefs.data)[pdefs.cset]
                 xydata = *old_data.xydata
                 xo = *(xydata.x)[pdefs.transient.imove]
                 yo = *(xydata.y)[pdefs.transient.imove]
                 *(xydata.x)[pdefs.transient.imove] = xyd[0]
                 *(xydata.y)[pdefs.transient.imove] = xyd[1]

                 if ((*pdefs.data)[pdefs.cset].type ne 0) then begin
                    *(*pdefs.data)[pdefs.cset].xydata = xydata
                    ichange = gr_xy_wid(pdefs, $
                                        line = pdefs.transient.imove)
                    if (ichange eq 0) then begin
                       ;; *old_data.xydata = old_xy
                       ;; (*pdefs.data)[pdefs.cset] = old_data
                       *(xydata.x)[pdefs.transient.imove] = xo
                       *(xydata.y)[pdefs.transient.imove] = yo
                       *(*pdefs.data)[pdefs.cset].xydata = xydata
                    endif
                 endif else begin
                    ichange = 1
                    *(*pdefs.data)[pdefs.cset].xydata = xydata
                 endelse
                 svflag = 1
              endif
              pdefs.transient.imove = -1
              pdefs.transient.mode = 0
           end
           4: if (ndata ne 0 and $
                  (event.modifiers and 3) eq 0) then begin
                                ; Right: delete.
              if (pdefs.transient.imove ge 0) then begin
                 imin = pdefs.transient.imove
                 xydata = *(*pdefs.data)[pdefs.cset].xydata
                 if (ndata le 2) then begin
                    if (imin eq 0) then xydata(*, 0) = xydata(*, 1)
                 endif else if (imin eq 0) then  $
                    xydata = xydata(*, 1:*) $
                 else if (imin eq ndata-1) then $
                    xydata = xydata(*, 0:imin-1)  $
                 else xydata = [[xydata(*, 0:imin-1)], $
                                [xydata(*, imin+1:*)]]
                 
                 (*pdefs.data)[pdefs.cset].ndata = $
                    (*pdefs.data)[pdefs.cset].ndata-1 
                 ichange = 1
                 gr_cross_hair, pdefs
              endif else graff_msg, pdefs.ids.message,  $
                                    "No datum within 5 pixels of selected location"
              pdefs.transient.mode = 0
              pdefs.transient.imove = -1
           end
           else:                ; Ignore wheel
        endcase
        
     end
     2: begin                   ; Motion events
        if pdefs.transient.mode ne 8 then begin
           gr_cross_hair, pdefs, xy[0:1]
           
           widget_control, pdefs.ids.xcp, set_value = xy[0]
           widget_control, pdefs.ids.ycp, set_value = xy[1]
        endif else begin 
           gr_coord_convert, xydatar[0, *], xydatar[1, *], ddx, ddy, $
                             /data, /to_device
           dist = sqrt((ddx-event.x)^2 + $
                       (ddy-event.y)^2)

           md = min(dist, imin)
           if (md gt 5) then begin
              pdefs.transient.imove = -1
              gr_cross_hair, pdefs
           endif else if imin ne pdefs.transient.imove then begin
              pdefs.transient.imove = imin
              gr_cross_hair, pdefs, [ddx[imin], ddy[imin]]
           endif
        endelse
     end
     Else:                      ; Shouldn't be any scroll events but
                                ; it's here just in case...
  endcase

  if (ichange and not svflag) then begin
     ptr_free, (*pdefs.data)[pdefs.cset].xydata
     (*pdefs.data)[pdefs.cset].xydata = ptr_new(xydata)
     widget_control, pdefs.ids.export, sensitive = $
                     (*pdefs.data)[pdefs.cset].type ge 0 and $ 
                     ptr_valid((*pdefs.data)[pdefs.cset].xydata) 
  endif

  return, ichange

end   
