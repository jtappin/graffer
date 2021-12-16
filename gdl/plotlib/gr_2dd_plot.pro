; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_2dd_plot, pdefs, i, csiz, grey_ps = grey_ps, shaded = shaded

;+
; GR_2DD_PLOT
;	Plot a 2-D Data in graffer
;
; Usage:
;	gr_2dd_plot, pdefs, i
;
; Argument:
;	pdefs	struct	input	The Graffer control structure.
;	i	int	input	Which 
;	csiz	float	input	Charsize scaling (hardcopy only).
;
; Keyword:
;	/grey_ps	input	If set & non-zero, then the plot is to
;				a PS device without the COLOUR option.
;	shaded	byte	output	Returns 1 if filled contours or an
;				image are drawn
;
; History:
;	Original: 10/12/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Skip if inadequate colours: 8/5/97; SJT
;	Replace handles with pointers: 27/6/05; SJT
;	Support colour inversion: 26/6/07; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Contour levels etc. become pointers: 11/1/12; SJT
;	Handle hidden datasets: 26/1/12; SJT
;	Send back flag if axes need redrawing: 20/1/16; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;-

  data = (*pdefs.data)[i]
  if ~ptr_valid(data.xydata) || $
     data.ndata eq 0 || data.ndata2 eq 0 then return

  xydata = *data.xydata
  z = *xydata.z
  x = *xydata.x
  y = *xydata.y

  
  if (data.zopts.format eq 0) then begin
                                ; Work around as currently contour
                                ; with different dimensions for the
                                ; coordinates fails. gr_display_img
                                ; already does this.

     if is_gdl() then begin
        sx = size(x)
        sy = size(y)
        if sy[0] eq 2 && sy[1] eq 1 then begin
           y = reform(y)
           sy = size(y)
        endif
        
        if sx[0] eq 1 && sy[0] eq 2 then $
           x = x[*, intarr(sy[2])] $
        else if sx[0] eq 2 then begin
           if sy[0] eq 1 then $
              y = transpose(y[*, intarr(sx[1])]) $
           else if sy[1] eq 1 then $ ; 1×n Y array
              y = y[intarr(sx[1]), *]
        endif
     endif

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
           return
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

     contour, z, x, y, /overplot, /follow, $
              levels = levels, c_linestyle = linestyle, $
              c_colors = lcolours, c_thick = thick,  $
              fill = data.zopts.fill eq 1b, c_labels = labels, $
              c_charsize = ccsize
     if data.zopts.fill eq 1b then shaded = 1b ; Don't clear it.
  endif else if (data.zopts.format eq 1) then begin
     if (~keyword_set(grey_ps)) then begin
        if (data.zopts.ctable gt 0) then begin
           table = data.zopts.ctable-1
           gamma = data.zopts.gamma 
        endif else begin
           table = pdefs.ctable
           gamma = pdefs.gamma
        endelse
     endif
     gr_display_img, z, x, y, $
                     range = data.zopts.range, $
                     pixel_size = data.zopts.pxsize, $
                     scale_mode = data.zopts.ilog, $
                     inverted = data.zopts.invert, $
                     missing = data.zopts.missing, $
                     ps_grey = grey_ps, $
                     table = table, $
                     gamma = gamma

     shaded = 1b
  endif

end
