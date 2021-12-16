; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_print, file, predraw = predraw, nosave = nosave, $
                 no_spawn = no_spawn, encapsulated = encapsulated, pdf $
                 = pdf, _extra = _extra

;+
; GRAFF_PRINT
;	User-callable interface to print a graffer file.
;
; Usage:
;	graff_print, file[, <graff_props keywords>]
;
;
; Arguments:
;	file	string	input	The graffer file to modify.
; 
;
; Keywords:
; 	/predraw	If set, then draw the plot to a pixmap window
; 			first (can be needed to get the scaling right)
; 	/nosave		If set, then do not save any changes to the
; 			GRAFFER structure.
; 	/no_spawn	If set, then only generate the file do not
; 			spawn any spooler or viewer.
; 	/encapsulated	If set, then generate an EPS or embeddable PDF file.
; 	/pdf		If set, then generate a PDF file.
;
;	Any keyword used by GRAFF_PROPS may be supplied.
;
; History:
;	Original: 18/5/05; SJT
;	Add PREDRAW keyword: 20/5/09; SJT
;	Fix pixmap size to get consistent charsizes: 3/11/15; SJT
;	Add NOSAVE key: 8/1/18; SJT
;	Add NO_SPAWN key: 8/5/18; SJT
;	Add /encapsulated and /pdf keywords, change graff_hard to
;	procedure: 3/11/21; SJT
;-

@graff_version

on_error, 2                     ; Return to caller on error

if n_params() eq 0 then message, "Must specify a GRAFFER file"

gr_state, /save

iflag = 1b
if keyword_set(_extra) then begin
   if keyword_set(nosave) then begin
      graff_props, file, pdefs, _extra = _extra
      iflag = 0b
   endif else graff_props, file, _extra = _extra
endif

;	Open the file

if iflag then begin
   f0 = file
   graff_init, pdefs, f0, version = version
   igot = graff_get(pdefs, f0, /no_set, /no_warn)
   if igot ne 1 then begin
      message, "Failed to open: "+f0
      return
   endif
endif

if keyword_set(predraw) then begin
    set_plot, 'x'
    window, /free, /pixmap,  xsize = 600, ysize = 600
    gr_plot_object, pdefs
    wdelete
endif

graff_hard, pdefs, no_spawn = no_spawn, encapsulated = $
            encapsulated, pdf = pdf

graff_clear, pdefs
gr_state

end
