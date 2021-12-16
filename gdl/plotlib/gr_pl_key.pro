; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_pl_key, pdefs, csiz

;+
; GR_PL_KEY
;	Plot a key to the plot.
;
; Usage:
;	gr_pl_key, pdefs, csiz
;
; Argument:
;	pdefs	struct	input	The ubiquitous GRAFFER data & control
;				structure
;	csiz	double	input	The character size to use
;
; History:
;	Original: 29/1/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Fix problem when no datasets are selected: 21/2/97; SJT
;	Modify to include "single point" format: 15/5/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add char size: 29/4/09; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Allow reversed listing: 19/7/21; SJT
;-

;		Although the key MAY be specified in data coordinates,
;		it is actually much easier to draw it in normalized
;		coordinates as this allows us to get around the
;		problems of logarithmic axes when calculating where to
;		put the traces

  if not ptr_valid(pdefs.key.list) then return ; This is really a
                                ; safety valve in case there's
                                ; anywhere the key pointer gets freed
                                ; but the use flag isn't cleared.

  if pdefs.key.csize eq 0 then csize = csiz $
  else csize = pdefs.key.csize*csiz

  list = *pdefs.key.list

  if (n_elements(list) eq 0) then return

  gr_coord_convert, pdefs.key.x, pdefs.key.y, xn, yn, /to_norm, data = $
                    pdefs.key.norm eq 0, region = pdefs.key.norm eq 1, frame = $
                    pdefs.key.norm eq 2

  if (pdefs.key.frame) then plots, /norm, xn[[0, 1, 1, 0, 0]], $
                                   yn[[0, 0, 1, 1, 0]]

  locs = where((*pdefs.data)[list].colour ne -1, nkey)
  if (nkey eq 0) then begin
     graff_msg, pdefs.ids.message, "No drawn traces in key list"
     return
  endif else list = list[locs]
  
  nrows = ceil(double(nkey)/pdefs.key.cols)
  if (pdefs.key.title ne '') then begin
     lsp = (yn[1]-yn[0])/(nrows+1.2)
     xyouts, total(xn)/2, yn[1]-lsp*.8, $
             pdefs.key.title,  /norm, align = 0.5, charsize = 1.2*csize
  endif else lsp = (yn[1]-yn[0])/nrows

  y = (dindgen(nrows)+.2)*lsp + yn[0]

  x0 = xn[0] + (xn[1]-xn[0])*dindgen(pdefs.key.cols)/pdefs.key.cols

  if (pdefs.key.one_point) then begin
     x = [.05, .125, .2] * (xn[1]-xn[0])/pdefs.key.cols
     tx = .3 * (xn[1]-xn[0])/pdefs.key.cols
     ys = replicate(.3*lsp, 3)
  endif else begin
     x = [.05, .3] * (xn[1]-xn[0])/pdefs.key.cols
     tx = .4 * (xn[1]-xn[0])/pdefs.key.cols
     ys = [0, .5*lsp]
  endelse
  yoff = csize/2. * double(!d.y_ch_size)/double(!d.y_size)

  for j = 0, nkey-1 do begin

     if pdefs.key.reverse then begin
        i = list[nkey-j-1]
     endif else begin
        i = list[j]
     endelse
     
     irow = (nrows - (j mod nrows))-1
     icol = j/nrows
     if (*pdefs.data)[i].colour eq -2 then $
        lcolour = graff_colours((*pdefs.data)[i].c_vals) $
     else lcolour = graff_colours((*pdefs.data)[i].colour)

     if ((*pdefs.data)[i].pline eq 2 and not pdefs.key.one_point) then begin
        xx = [x(0), replicate(total(x)/2, 2), x(1)]
        plots, /norm, x0(icol)+xx, y(irow)+ys([0, 0, 1, 1]), color = $
               lcolour, linesty = (*pdefs.data)[i].line, $
               thick = (*pdefs.data)[i].thick
     endif else if ((*pdefs.data)[i].pline ne 0) then begin
        plots, /norm, x0(icol)+x, y(irow)+yoff, color = $
               lcolour, linesty = (*pdefs.data)[i].line, $
               thick = (*pdefs.data)[i].thick
     endif
     
     if ((*pdefs.data)[i].psym ne 0) then begin
        if ((*pdefs.data)[i].psym ge 8) then $
           gr_symdef, (*pdefs.data)[i].psym, thick = (*pdefs.data)[i].thick
        if (pdefs.key.one_point) then $
           plots, /norm, x0(icol)+x(1), y(irow)+yoff, color = $
                  lcolour, psym = (*pdefs.data)[i].psym $
                  < 8, thick = (*pdefs.data)[i].thick, symsize = $
                  abs((*pdefs.data)[i].symsize)*csiz $
        else $  
           plots, /norm, x0(icol)+x, y(irow)+ys, color = $
                  lcolour, psym = (*pdefs.data)[i].psym $
                  < 8, thick = (*pdefs.data)[i].thick, symsize = $
                  abs((*pdefs.data)[i].symsize)*csiz
     endif

     if (pdefs.y_right and pdefs.key.side) then begin
        if (*pdefs.data)[i].y_axis eq 0 then side = ' (l)' $
        else side = ' (r)'
        descr = (*pdefs.data)[i].descript+side
     endif else descr = (*pdefs.data)[i].descript
     xyouts, /norm, x0(icol)+tx, y(irow), descr, $
             charsize = csize
  endfor

end
