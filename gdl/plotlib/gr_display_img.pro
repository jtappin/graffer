; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_display_img, zin, xin, yin, range = range, $
                    colour_range = colour_range, $ 
                    pixel_size = pixel_size,  $
                    scale_mode = scale_mode, $
                    inverted = inverted,  $
                    missing = missing, $
                    logarithmic = logarithmic, $
                    table = table, gamma = gamma, $
                    ps_grey = ps_grey

;+
; GR_DISPLAY_IMG
;	Colour/greyscale image display for GRAFFER
;
; Usage:
;	gr_display_img, zi, xin, yin
;
; Arguments:
;	zin	float	input	The data to be displayed
;	xin	float	input	The X coordinates of the data.
;	yin	float	input	The Y coordinates of the data.
;
; Keywords:
;	range	float	input	The range from "black" to "white"
;	pixel.  float	input	For devices with scalable pixels, the
;				size of a displayed pixel.
;	scale_mode int	input	Scaling mode, 0/absent = linear, 1 =
;				log, 2 = square_root
;	/inverted	input	If set, then plot from "white" to "black".
;	missing	float	input	A value to use for output pixels that don't
;				map to input pixels.
;	table	int	input	The colour table to use.
;	gamma	float	input	The Gamma value to apply to the colour
;				table.
;	/ps_grey	input	If set, then the output is to a
;				greyscale postscript file and no
;				colour table will be used.
;
; History:
;	Original: 10/12/96; SJT
;	Add code to clip to the viewport: 12/12/96; SJT
;	Modify to handle 2-D X or Y arrays: 10/7/05; SJT
;	Support colour inversion: 26/6/07; SJT
;	Updated to use gr_coord_convert: 27/1/12; SJT
;	Add missing keyword to set a value if we are triangulating:
;	11/7/12; SJT
;	Fix failure to display when axis reversed: 24/8/12; SJT
;	Replace logarithmic with scale_mode: 18/11/15; SJT
;	Redesign colour handling for true-colour displays: 16/5/16; SJT
;	Work around buggy triangulate: 14/10/16; SJT
;-

  if n_elements(logarithmic) ne 0 then graff_msg, 0l, $
     "The LOGARITHMIC key is obsolete, please use SCALE_MODE=1 instead"
  if n_elements(colour_range) ne 0 then graff_msg, 0l, $
     "The COLOUR_RANGE key is obsolete and is ignored."

  if n_elements(scale_mode) ne 0 then mode = scale_mode $
  else if n_elements(logarithmic) ne 0 then mode = logarithmic $
  else mode = 0

  if (!D.flags and 1) then begin ; PS or similar with scalable pixels
     if (not keyword_set(pixel_size)) then pixel_size = 0.1 ; default
                                ; 0.1 mm pixels
     scfac = 10./([!D.x_px_cm, !D.y_px_cm] * pixel_size)
  endif else scfac = [1, 1]

;	If x &/| y are 1-D make them 2-D to unify everything.

  sx = size(xin)
  sy = size(yin)
  sz = size(zin)
  tflag = sx[0] eq 2 || (sy[0] eq 2 &&  sy[1] gt 1) ; Y can be 1xN

  if tflag then begin
     if sx[0] eq 1 then x = xin[*, intarr(sz[2])] $
     else x = xin
     if sy[0] eq 1 then y = transpose(yin[*, intarr(sz[1])]) $
     else if sy[1] eq 1 then y = yin[intarr(sz[1]), *] $
     else y = yin
  endif

  mnx = min(xin, max = mxx)
  mny = min(yin, max = mxy)


;	Select out those parts which are within the viewport

  cxmax = max(!x.crange, min = cxmin)
  cymax = max(!y.crange, min = cymin)

  if (!X.type eq 1) then begin
     locsx = where(xin ge 10^cxmin and xin le 10^cxmax, nx)
     mnx = mnx > 10^!x.crange[0]
     mxx = mxx < 10^!x.crange[1]
  endif else begin
     locsx = where(xin ge cxmin and xin le cxmax, nx)
     mnx = mnx > !x.crange[0]
     mxx = mxx < !x.crange[1]
  endelse

  if (!Y.type eq 1) then begin
     locsy = where(yin ge 10^cymin and yin le 10^cymax, ny)
     mny = mny > 10^!y.crange[0]
     mxy = mxy < 10^!y.crange[1]
  endif else begin
     locsy = where(yin ge cymin and yin le cymax, ny)
     mny = mny > !y.crange[0]
     mxy = mxy < !y.crange[1]
  endelse

  if (nx le 1 or ny le 1) then return ; Image is wholly outside the VP
                                ; (or just one row or column in it)

  xrange = [mnx, mxx]
  yrange = [mny, mxy]

  gr_coord_convert, xrange, yrange, xcorn, ycorn, /data, /to_device
  xcorn = round(xcorn*scfac[0])
  ycorn = round(ycorn*scfac[1])
  dvxsize = xcorn[1]-xcorn[0]
  dvysize = ycorn[1]-ycorn[0]
  cmxsize = dvxsize/(!d.x_px_cm*scfac[0])
  cmysize = dvysize/(!d.y_px_cm*scfac[1])
  cmxll = xcorn[0]/(!d.x_px_cm*scfac[0])
  cmyll = ycorn[0]/(!d.y_px_cm*scfac[1])

; Rescale the input data for log / sqrt display.

  case mode of
     0: ztmp = zin
     1: ztmp = alog10(zin)
     2: begin
        ztmp = sqrt(abs(zin))
        locs = where(zin lt 0, nn)
        if nn ne 0 then ztmp[locs] = -ztmp[locs]
     end
  endcase

  if tflag then begin
     if (total(~finite(x))+total(~finite(y)) ne 0) then begin
        junk = dialog_message(["Coordinates contain non-finite", $
                               "values. Cannot warp to a plane.", $
                               "please use contouring, or fix", $
                               "the coordinates"], $
                              /error)
        return
     endif
     triangulate, x, y, triangles ;, tol = 1e-12*(max(abs(x)) > $
                                ;               max(abs(y)))
     areas = gr_tri_area(x, y, triangles, usable = usable, nbad = $
                         nbad)
     if nbad ne 0 then triangles = triangles[*, usable]
     zz = trigrid(x, y, ztmp, triangles, $
                  [(mxx-mnx)/dvxsize, (mxy-mny)/dvysize], $
                  [mnx, mny, mxx, mxy],  missing = missing)

  endif else begin
     x = xin[locsx]
     y = yin[locsy]
     z = ztmp[locsx, *]
     z = z[*, locsy]
     gr_coord_convert, (dindgen(dvxsize)+xcorn[0]) / scfac[0], $
                       dblarr(dvxsize), xd, junk, /device, /to_data

     gr_coord_convert, dblarr(dvysize),  $
                       (dindgen(dvysize)+ycorn[0]) / scfac[1], $
                       junk, yd, /device, /to_data

     sz = size(z)
     xx = gr_interpol_ss(dindgen(sz[1]), x, xd)
     yy = gr_interpol_ss(dindgen(sz[2]), y, yd)
     zz = bilinear(z, xx, yy)
  endelse

  if n_elements(range) eq 0 || (range[0] eq range[1]) then begin
     zrange = [min(zz, max = mxz, /nan), mxz] 
  endif else begin
     case mode of
        0: zrange = range
        1: zrange = alog10(range)
        2: begin
           zrange = sqrt(abs(range))
           locs = where(zrange lt 0, nn)
           if nn ne 0 then zrange[locs] = -zrange[locs]
        end
     endcase
  endelse

  img = bytscl(zz, min = zrange[0], max = zrange[1], /nan)
  if keyword_set(inverted) then img =  255b-img

  if keyword_set(ps_grey) then begin
; Temporary (?) fix for GDL
     nxll = cmxll * !d.x_px_cm / double(!d.x_size)
     nxsize = cmxsize * !d.x_px_cm / double(!d.x_size)
     nyll = cmyll * !d.y_px_cm / double(!d.y_size)
     nysize = cmysize * !d.y_px_cm / double(!d.y_size)
     tv, img, nxll, nyll, xsize = nxsize, ysize = nysize, /norm
;;     tv, img, cmxll, cmyll, xsize = cmxsize, ysize = cmysize, /centi $
  endif else begin
     if n_elements(table) eq 0 then table = 0
     graff_ctable, table, cmap, gamma = gamma
     sz = size(img, /dim)
     img3 = bytarr([sz, 3])
     for j = 0, 2 do img3[*, *, j] = (cmap[*, j])[img]
     if (!d.flags and 1) then begin
; Temporary (?) fix for GDL
        nxll = cmxll * !d.x_px_cm / double(!d.x_size)
        nxsize = cmxsize * !d.x_px_cm / double(!d.x_size)
        nyll = cmyll * !d.y_px_cm / double(!d.y_size)
        nysize = cmysize * !d.y_px_cm / double(!d.y_size)
        tv, img3, nxll, nyll, xsize = nxsize, ysize = nysize, $
            /norm, true = 3

        ;; tv, img3, cmxll, cmyll, xsize = cmxsize, ysize = cmysize, $
;;            true = 3, /centi 
     endif else $
        tv, img3, xcorn[0], ycorn[0], true = 3
  endelse
end

