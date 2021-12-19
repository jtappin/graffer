; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_make_dump, file, png = png, tiff = tiff, nrif = nrif, $
                     dialogue = dialogue, xsize = xsize, $
                     ysize = ysize, variable = variable

;+
; GRAFF_MAKE_DUMP
;	User-callable interface to convert an graffer file to an
;	image.
;
; Usage:
;	graff_make_dump, file
;
; Arguments:
;	file	string	input	The graffer file to convert.
;
; Keywords:
;	/png		Save as a PNG (Portable Network Graphics) file
;	/tiff		Save as a TIFF (Tagged image file format?) file
;	/nrif		Save as an NRIF (NCAR raster image format) file
;	/dialogue	Select the save file with a dialogue.
;	variable string	Save as a variable at the main level.
;	xsize	int	The X dimension of the image
;	ysize	int	The Y dimension of the image
;
; History:
;	Original (ideas from graff_print): 6/3/17; SJT
;-

;  on_error, 2                   ; Return to caller on error

  if n_params() eq 0 then message, "Must specify a GRAFFER file"
  gr_state, /save

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

  if ~keyword_set(xsize) then xsize = 600
  if ~keyword_set(ysize) then ysize = 600

  set_plot, 'x'
  window, /free, /pixmap,  xsize = xsize, ysize = ysize
  device, decomposed = 1
  !p.color = graff_colours(1)
  !p.background = graff_colours(0)
  gr_plot_object, pdefs

  graff_dump, pdefs, png = png, tiff = tiff, nrif = nrif, $
              dialogue = dialogue, variable = variable

  wdelete

  graff_clear, pdefs
  gr_state

end
