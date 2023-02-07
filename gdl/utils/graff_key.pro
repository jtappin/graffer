;+
; GRAFF_KEY
;	Programmatically control the display of a key on a graffer plot.
;
; Usage:
;	graff_key, file, <setting keys>
;
; Argument:
;	file	string	The graffer file to update.
;
; Keywords:
;	/show_key	If set, then display a key (provided
;			coordinates and datasets are selected). Set
;			explicitly to zero to hide the key.
;	list	int	Specify a list of datasets to display (1 based).
;	/all		If set, then display all datasets in the key.
;	x_limits float	Specify the range of X in which to place the key
;	y_limits float	Specify the range of Y in which to place the key.
;	system	int	Specify the coordinate system for the key
;			boundaries.  0 = data, 1 = normalized, 2 =
;			frame (normalized coordinates wrt the plotting
;			area). Default = 2.
;	charsize float	Specify the character size for the
;			annotations. N.B. the actual size is not
;			consistent between IDL, GDL & Fortran.
;	columns	int	Specify how many columns for the key. Default 1.
;	/one_point	Display a single point (in the middle of the
;			line, if any). Set it explicitly to zero to
;			show two points (the historical default). 
;	/reverse	If set, then display the datasets from last to
;			first.
;	/frame		If set, then draw a box around the key.
;	/y_side		If set, then indicate which Y-axis the
;			datasets use (only if secondary axis is
;			enabled).
;	title	string	A title for the key.
;	/ascii		If set, then save the file in ASCII format.
;
; History:
;	Original (after GRAFF_PROPS & GR_KEY_DEF): 7/2/23; SJT
;-

; LICENCE:
; Copyright (C) 2023: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_key, file, show_key = show_key, list = list, all = all, $
               x_limits = x_limits, y_limits = y_limits,  $
               system = system, charsize = charsize, $
               columns = columns, one_point = one_point,  $
               reverse = reverse, y_side = y_side, title = title, $
               ascii = ascii

  
;	Check that the necessary inputs are present

  on_error, 2                   ; Return to caller on error

  if n_params() eq 0 || size(file, /type) ne 7 then $
     message, "Must specify a GRAFFER file"

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

  nflag = ~ptr_valid(pdefs.key.list) && ~pdefs.key.use && $
     pdefs.key.x[0] eq pdefs.key.x[1] && $ 
     pdefs.key.y[0] eq pdefs.key.y[1] ; Reasonable to assume that this
                                ; implies that no key has ever been
                                ; defined, so set non-null defaults
                                ; accordingly. 

  if nflag then begin
     pdefs.key.csize = 1.0d
     pdefs.key.norm = 2
     pdefs.key.cols = 1
     pdefs.key.one_point = 1b
  endif

  if n_elements(charsize) ne 0 then pdefs.key.csize = charsize
  
  if n_elements(system) ne 0 then begin
     cn = pdefs.key.norm
     if system ne cn then begin
        pdefs.key.norm = system
        gr_coord_convert, pdefs.key.x, pdefs.key.y, xt, yt, $
          to_data = system eq 0, to_region = system eq 1, $
          to_frame = system eq 2, data = cn eq 0, $
          region = cn eq 1, frame = cn eq 2

        pdefs.key.x = xt
        pdefs.key.y = yt
     endif
  endif

; Can clear position by setting limits to scalar zero.  
  if n_elements(x_limits) ne 0 then pdefs.key.x = x_limits
  if n_elements(y_limits) ne 0 then pdefs.key.y = y_limits

  
  if n_elements(all) ne 0 || n_elements(list) ne 0 then begin
     if ptr_valid(pdefs.key.list) then begin
        iuse = *(pdefs.key.list)
        ptr_free, pdefs.key.list
     endif else iuse = bytarr(pdefs.nsets)

     if keyword_set(all) then iuse[*] = 1b $
     else if n_elements(list) ne 0 then begin
        iuse[*] = 0b
        locs = where(list ge 1 and list le pdefs.nsets, nv)
        if nv ne 0 then $
           iuse[list[locs]-1] = 1b
     endif else if n_elements(all) ne 0 then iuse[*] = 0b
     if total(iuse) gt 0 then pdefs.key.list = ptr_new(iuse)
  endif

  if n_elements(columns) ne 0 then pdefs.key.cols = columns
  if n_elements(one_point) ne 0 then $
     pdefs.key.one_point = keyword_set(one_point)

  if n_elements(frame) ne 0 then pdefs.key.frame = keyword_set(frame)
  if n_elements(reverse) ne 0 then pdefs.key.reverse = $
     keyword_set(reverse)

  if pdefs.y_right && n_elements(y_side) ne 0 then $
     pdefs.key.side = keyword_set(y_side)

  if n_elements(title) ne 0 then pdefs.key.title = title

  if ~ptr_valid(*(pdefs.key.list)) || $
     pdefs.key.x[0] eq pdefs.key.x[1] || $
     pdefs.key.y[0] eq pdefs.key.y[1] then pdefs.key.use = 0b $
  else if n_elements(show_key) ne 0 then $
     pdefs.key.use = keyword_set(show_key)

  if (keyword_set(ascii)) then gr_asc_save, pdefs $
  else gr_bin_save, pdefs

  graff_clear, pdefs

end
