; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_2df_plot, pdefs, i, csiz, grey_ps = grey_ps, shaded = shaded

;+
; GR_2DF_PLOT
;	Display a 2-d function in GRAFFER
;
; Usage:
;	gr_2df_plot, pdefs, i
;
; Argument:
;	pdefs	struct	input	The Graffer control structure.
;	i	int	input	Which 
;	csiz	double	input	Charsize scaling (hardcopy only).
;
; Keyword:
;	/grey_ps	input	If set & non-zero, then the plot is to
;				a PS device without the COLOUR option.
;	shaded	byte	output	Return 1 if an image or filled
;				contours are drawn
;
; History:
;	Original: 10/12/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Skip if inadequate colours: 8/5/97; SJT
;	Convert handles to pointers: 27/6/05; SJT
;	Support colour inversion: 26/6/07; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Contour levels etc. become pointers: 11/1/12; SJT
;	Handle hidden datasets: 26/1/12; SJT
;	Add "r" as well as x & y: 7/1/15; SJT
;	Return a flag if axes need redrawing: 20/1/16; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;-

  data = (*pdefs.data)[i]
  if ~ptr_valid(data.xydata) || $
     data.ndata eq 0 || data.ndata2 eq 0 then return
  if (data.zopts.format eq 2) then return

  yaxis = pdefs.y_right and data.y_axis eq 1

  xrange = !X.crange
  if (pdefs.xtype) then xrange = 10^xrange
  yrange = !Y.crange
  if (pdefs.ytype and yaxis eq 0) or (pdefs.ytype_r and yaxis eq 1) then $
     yrange = 10^yrange

  xydata = *data.xydata

  xmin = xrange[0]
  xmax = xrange[1]
  ymin = yrange[0]
  ymax = yrange[1]
  if (xydata.range[0, 0] ne xydata.range[1, 0]) then begin
     xmin = xmin > xydata.range[0, 0]
     xmax = xmax < xydata.range[1, 0]
  endif

  if (xydata.range[0, 1] ne xydata.range[1, 1]) then begin
     ymin = ymin > xydata.range[0, 1]
     ymax = ymax < xydata.range[1, 1]
  endif

  if (pdefs.xtype) then begin
     xmin = alog10(xmin)
     xmax = alog10(xmax)
     x = 10^(dindgen(data.ndata) * (xmax-xmin) $
             /  double(data.ndata-1) + xmin)
  endif else x = dindgen(data.ndata) * (xmax-xmin) $
                 /  double(data.ndata-1) + xmin

  if (pdefs.ytype) then begin
     ymin = alog10(ymin)
     ymax = alog10(ymax)
     y = 10^(dindgen(1, data.ndata2) * (ymax-ymin) $
             /  double(data.ndata2-1) + ymin)
  endif else y = dindgen(1, data.ndata2) * (ymax-ymin) $
                 /  double(data.ndata2-1) + ymin

  xx = x
  yy = y[*]

  x = x[*, intarr(data.ndata2)]
  y = y[intarr(data.ndata), *]

  r = sqrt(x^2+y^2)

  z = 0.                        ; Need to define z before we use it.

  iexe = execute('z = '+xydata.funct)
  s = size(z)

  if (s[0] ne 2) then graff_msg, pdefs.ids.message, $
                                 "Function:"+xydata.funct+ $
                                 " does not return a 2-D array" $
  else if (data.zopts.format eq 0) then begin
     if (data.zopts.set_levels) then begin
        levels = *(data.zopts.levels) 
        nl = n_elements(levels)
     endif else begin
        if (data.zopts.n_levels eq 0) then nl = 6 $
        else nl = data.zopts.n_levels
        if data.zopts.lmap eq 1 then $
           locs = where(finite(z) and z gt 0., nfin) $
        else locs = where(finite(z), nfin)
        
        if (nfin ne 0) then begin
           mx = max(z[locs], min = mn)
           rg = mx-mn
        endif else rg = 0.
        
        if (rg eq 0.) then begin
           graff_msg, pdefs.ids.message, 'Flat dataset - not able to ' + $
                      'contour'
           goto, restore
        endif
        levels = gr_make_levels(mn, mx, nl, data.zopts.lmap)
     endelse

     
     if (data.zopts.label ne 0 and n_elements(nl) eq 1) then begin
        labels = indgen(nl) mod data.zopts.label eq data.zopts.label_off
     endif else labels = 0
     
     if ptr_valid(data.zopts.colours) then begin
        ncol = data.zopts.n_cols
        lcolours = lonarr(ncol)
        for j = 0, ncol-1 do begin
           if (*data.zopts.colours)[j] eq -2 then $
              lcolours[j] = $
              graff_colours((*data.zopts.raw_colours)[*, j]) $
           else lcolours[j] = graff_colours((*data.zopts.colours)[j])
        endfor
     endif

     if ptr_valid(data.zopts.style) then linestyle = $
        *(data.zopts.style) 
     if ptr_valid(data.zopts.thick) then thick = $
        *(data.zopts.thick) 

     ccsize = pdefs.charsize*data.zopts.charsize*0.75*csiz

     contour, z, xx, yy, /overplot, /follow, $
              levels = levels, c_linestyle = linestyle, $
              c_colors = lcolours, c_thick = thick, $
              fill = data.zopts.fill eq 1b,  c_labels = labels, $
              c_charsize = ccsize
     if data.zopts.fill eq 1b then shaded = 1b
  endif else begin
     if (~keyword_set(grey_ps)) then begin
        if (data.zopts.ctable gt 0) then begin
           table = data.zopts.ctable-1
           gamma = data.zopts.gamma 
        endif else begin
           table = pdefs.ctable
           gamma = pdefs.gamma
        endelse
     endif
     gr_display_img, z, xx, yy, $
                     range = data.zopts.range, $
                     pixel_size = data.zopts.pxsize, $
                     scale_mode = data.zopts.ilog, $
                     inverted = data.zopts.invert, $
                     missing = data.zopts.missing, $
                     ps_grey = grey_ps, $
                     table = table, $
                     gamma = gamma
     
     shaded = 1b
  endelse

Restore:

end
