; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_show, file, window = window, xsize = xsize, ysize = ysize, $
                _extra = _extra

;+
; GRAFF_SHOW
;	User-callable interface to display a graffer file in a regular
;	graphics window.
;
; Usage:
;	graff_show, file
;
; Argument:
;	file	string	input	The graffer file to display.
;
; Keywords:
;	window	long	input	The index of the window to use.
;	xsize	long	input	The x-size of the window to create.
;	ysize	long	input	The y-size of the window.
;	-- Any GRAFF_PROPS keyword may also be given.
;
; History:
;	Original: 13/5/09; SJT
;-

on_error, 2                     ; Return to caller on error

if n_params() eq 0 then message, "Must specify a GRAFFER file"

gr_state, /save

if keyword_set(_extra) then graff_props, file, _extra = _extra

;	Open the file

@graff_version

f0 = file
graff_init, pdefs, f0, version = version
igot = graff_get(pdefs, f0, /no_set, /no_warn)
if igot ne 1 then begin
   message, "Failed to open: "+f0
   return
endif

set_plot, 'x'
wo = !d.window
if keyword_set(xsize) || keyword_set(ysize) then begin
    if ~keyword_set(window) then window = 0
    window, window, xsize = xsize, ysize = ysize
endif else if keyword_set(window) then begin
    device, window_state = ws
    if ws[window] then wset, window $
    else window, window
endif

gr_plot_object, pdefs

if wo ge 0 then wset, wo
graff_clear, pdefs
gr_state

end
