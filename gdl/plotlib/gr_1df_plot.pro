; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_1df_plot, pdefs, i, csiz

;+
; GR_1DF_PLOT
;	Plot a 1-D function in graffer
;
; Usage:
;	gr_1df_plot, pdefs, i
;
; Argument:
;	pdefs	struct	input	The Graffer control structure.
;	i	int	input	Which 
;	csiz	double	input	Charsize scaling (hardcopy only).
;
; History:
;	Farmed out from GR_PLOT_OBJECT: 10/12/96; SJT
;	Modify for extended symbol definitions: 20/1/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Convert handles to pointers: 27/6/05; SJT
;-

  xrange = !X.crange
  if (pdefs.xtype) then xrange = 10^xrange
  yrange = !Y.crange
  if (pdefs.ytype) then yrange = 10^yrange

  maxrange = sqrt(max(xrange^2)+max(yrange^2))

  data = (*pdefs.data)[i]

  if ~ptr_valid(data.xydata) || data.ndata eq 0 then return

  if (data.colour eq -1) then return
  if data.colour eq -2 then $
     lcolour = graff_colours(data.c_vals) $
  else lcolour = graff_colours(data.colour)

  xydata = *data.xydata

  case (data.type) of
     -1: begin                  ; Y=f(x) | theta=f(r)
        case (data.mode) of
           0: begin
              arange = xrange
              atype = pdefs.xtype
           end
           Else: begin
              arange = [0., maxrange]
              atype = 0
           end
        endcase
        exceed = 0
     end
     -2: begin                  ; x=f(y) | r = f(theta)
        case (data.mode) of
           0: begin
              arange = yrange
              atype = pdefs.ytype
              exceed = 0
           end 
           1: begin
              arange = [0., 2.*!Pi]
              atype = 0
              exceed = 1
           end
           2: begin
              arange = [0., 360.]
              atype = 0
              exceed = 1
           end
        endcase
     end
     -3: begin
        arange = xydata.range
        atype = 0
        exceed = 0
     end
  endcase

  if (xydata.range[0] ne xydata.range[1]) then begin
     if (exceed) then begin 
        amin = xydata.range[0]
        amax = xydata.range[1]
     endif else begin
        amin = arange[0] > xydata.range[0]
        amax = arange[1] < xydata.range[1]
     endelse
  endif else begin
     amin = arange[0]
     amax = arange[1]
  endelse

  if (atype) then begin
     amin = alog10(amin)
     amax = alog10(amax)
     t = 10^(dindgen(data.ndata) * (amax-amin) $
             /  double(data.ndata-1) + amin)
  endif else t = dindgen(data.ndata) * (amax-amin) $
                 /  double(data.ndata-1) + amin

  case (data.type) of
     -1: begin                  ;  y = f(x)
        x = t
        y = 0.                  ; Must make y defined before using it
                                ; in an execute

        iexe = execute('y = '+xydata.funct)
        s = size(y, /n_dimensions)
     end
     -2: begin                  ;  x = f(y)
        y = t
        x = 0.                  ; Must make x defined before using it
                                ; in an execute

        iexe = execute('x = '+xydata.funct)
        s = size(x, /n_dimensions)
     end
     -3: begin                  ;  x = f(t) & y = g(t)
        x = 0.
        y = 0.                  ; Must make y defined before using it
                                ; in an execute

        iexe = execute('x = '+xydata.funct[0])
        iexe = execute('y = '+xydata.funct[1])
        s = size(y, /n_dimensions) < size(x, /n_dimensions)
     end
  endcase

  if (s eq 0) then graff_msg, pdefs.ids.message, $
                              "Function:"+xydata.funct+ $
                              " does not return an array" $
  else begin
     if (data.mode eq 2) then pcf = !Dtor $
     else pcf = 1.0
     if data.pline ne 0 && data.ndata ge 2 then begin
        lps = ([0, 0, 10])(data.pline)
        oplot, x, y*pcf, color = lcolour, psym = $
               lps, linesty = data.line, thick = $
               data.thick, polar = (data.mode ne 0), noclip = $
               data.noclip
     endif
     
     if (data.psym ne 0) then begin
        if (data.psym ge 8) then gr_symdef, data.psym, thick = data.thick
        oplot, x, y*pcf, color = lcolour, psym = $
               data.psym < 8, thick = data.thick, symsize = $
               abs(data.symsize)*csiz, polar = (data.mode ne 0), $
               noclip = data.noclip  
     endif
  endelse

end
