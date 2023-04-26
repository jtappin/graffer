; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_dump, pdefs, png = png, tiff = tiff, nrif = nrif, $
                variable = variable, dialogue = dialogue

;+
; GRAFF_DUMP
;	Dump the contents of the draw widget to an image file.
;
; Usage:
;	graff_dump, pdefs[, <selector>]
;
; Argument:
;	Pdefs	struct	input	The graffer data structure (only used
;				for file and dir)
;
; Keywords:
;	/png		Save as a PNG (Portable Network Graphics) file
;	/tiff		Save as a TIFF (Tagged image file format?) file
;	/nrif		Save as an NRIF (NCAR raster image format) file
;	variable string	Save to a variable at the top level
;	/dialogue 	Select the save file with a dialogue.
;
; Restrictions:
;	Only one keyword may be given, also only default options are
;	supported for the more general file types.
;
; History:
;	Original: 8/9/95; SJT
;	Replace GIF with PNG: 23/6/05; SJT
;	Add dialogue: 10/1/12; SJT
;	Remove commented out code: 26/4/23
;-

  nkey = (keyword_set(png) + $
          keyword_set(tiff) + $
          keyword_set(nrif) + $
          keyword_set(variable) + $
          keyword_set(dialogue))
  if (nkey ne 1) then message, "Must give one and only one keyword."

  tname = pdefs.name
  if (((dp = strpos(tname, '.'))) ne -1) then  $
     tname = strmid(tname, 0, dp)
  tname = pdefs.dir+tname

  gr_cross_hair, pdefs          ; Erase the cross hairs if any

  if !d.n_colors gt 256  then begin ; True colour is possible
     image = tvrd(true = 1, $
                  order = ~keyword_set(variable)) ; Read the image
                                ; from the screen 

     if (keyword_set(png)) then begin
        write_png, tname+'.png', image
        graff_msg, pdefs.ids.message, 'Wrote PNG to: '+tname+'.png'
     endif else if (keyword_set(tiff)) then begin
        write_tiff, tname+'.tif', image, compress = 2
        graff_msg, pdefs.ids.message, 'Wrote TIFF to: '+tname+'.tif'
        
     endif else if keyword_set(variable) then begin
        if size(variable, /type) ne 7 then variable = 'grf_image'
        (scope_varfetch(variable, level = 1, /enter)) =  image

     endif else if keyword_set(dialogue) then begin
        gr_image_write, image, name = tname, group = pdefs.ids.graffer
     endif
  endif else begin              ; 8-bit display--False colour only
     tvlct, /get, r, g, b
     image = tvrd(order = ~keyword_set(variable)) ; Read the image
                                ; from the screen 

     if (keyword_set(png)) then begin
        write_png, tname+'.png', image, r, g, b 

     endif else if (keyword_set(tiff)) then begin

        write_tiff, tname+'.tif', image, compress = 2, red = r, green $
                    = g, $
                    blue = b
        
     endif else if keyword_set(variable) then begin
        if size(variable, /type) ne 7 then variable = 'grf_image'
        (scope_varfetch(variable, level = 1, /enter)) =  image

     endif else if (keyword_set(dialogue)) then begin
        gr_image_write, image, r, g, b, $
                        name = tname, group = pdefs.ids.graffer
     endif
  endelse
end
