; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_update, file, idx, name = name, polar = polar, $
                  rescale = rescale, style = style, psym = psym, $
                  join = join, symsize = symsize, colour = colour, $
                  thick = thick, description = description, $
                  sort = sort, ascii = ascii, noclip = noclip, $
                  mouse = mouse, z_format = z_format, $
                  z_nlevels = z_nlevels, z_levels = z_levels, $
                  z_lmap = z_lmap, $
                  z_colours = z_colours, z_style = z_style, $
                  z_thick = z_thick, z_range = z_range, $
                  z_label = z_label, z_pxsize = z_pxsize, $
                  z_invert = z_invert, z_fill = z_fill, $
                  z_log = z_log, z_ctable = z_ctable, $
                  y_axis = y_axis, make_current = make_current, $
                  x_values = x_values, y_values = y_values, $
                  z_values = z_values, funcx = funcx, funcy = funcy, $
                  funcz = funcz, xy_file = xy_file, z_file = z_file, $
                  errors = errors, errtype = errtype, $
                  x_errors = x_errors, y_errors = y_errors, neval = neval, $
                  frange = frange, x_func = x_func, y_func = y_func, $
                  z_func = z_func, z_missing = z_missing, $
                  z_charsize = z_charsize, status = status, $
                  z_mode = z_mode, x_scale = x_scale, $
                  y_scale = y_scale, x_shift = x_shift, $
                  y_shift = y_shift, z_scale = z_scale, $
                  z_shift = z_shift, retain_unset = retain_unset, $
                  delete = delete

;+
; GRAFF_UPDATE
;	User-callable interface to update the properties of a
;	dataset.
;
; Usage:
;	graff_update, file, idx, name = name, polar = polar, $
;                  rescale = rescale, style = style, psym = psym, $
;                  join = join, symsize = symsize, colour = colour, $
;                  thick = thick, description = description, $
;                  sort = sort, ascii = ascii, noclip = noclip, $
;                  mouse = mouse, z_format = z_format, $
;                  z_nlevels = z_nlevels, z_levels = z_levels, $
;                  z_colours = z_colours, z_style = z_style, $
;                  z_thick = z_thick, z_range = z_range, $
;                  z_label = z_label, z_pxsize = z_pxsize, $
;                  z_invert = z_invert, z_fill = z_fill, $
;                  z_log = z_log, z_ctable = z_ctable, $
;                  y_axis = y_axis, make_current = make_current, $
;                  x_values = x_values, y_values = y_values, $
;                  z_values = z_values, xy_file = xy_file, $
;                  z_file = z_file, z_lmap=z_lmap, $
;                  errors = errors, errtype = errtype, , $
;                  x_errors = x_errors, y_errors = y_errors, neval = neval, $
;                  frange = frange, x_func = x_func, y_func = y_func, $
;                  z_func = z_func, z_missing = z_missing, $
;                  z_charsize = z_charsize, status = status, $
;                  z_mode = z_mode, x_scale = x_scale, $
;                  y_scale = y_scale, x_shift = x_shift, $
;                  y_shift = y_shift,  z_scale = z_scale, $
;                  z_shift = z_shift, retain_unset = retain_unset
;                  delete=delete
;
; Arguments:
;	file	string	input	The graffer file to modify.
;	idx	int	input	The dataset index to modify (starting
;				at 1).
;
; Keywords:
; 	name	string	input	Find the dataset to update by
; 				searching the descriptor fields to
; 				match the name.
;	polar	int	input	If unset or 0 rectangular, 1 = polar
;				in radians, 2 = polar in degrees.
;	rescale 	input	If set, then reset the scaling of the
;				plot with the autoscale routine.
;	style	int	input	The standard IDL linestyle codes
;	psym	int	input	The GRAFFER symbol code - extended IDL
;                                                         symbol
;                                                         codes.
;	join	int	input	The style of joining: 0 - none
;					 	      1 - sloping lines
;						      2 - histogram
;	symsize float	input	The size for the symbols (relative to
;				standard)
;	colour	int	input	Colour number - standard GRAFFER
;                                               colours (which may
;                                               well not work on
;                                               current device).
;				May also be a triple or a long value
;				with a "packed" colour
;	thick	float	input	Line thickness.
;	neval	int	input	The number of times to evaluate a
;				function. (2-elements for funcz)
;	description str input	A description of the data set.
;	sort		input	Whether to sort the values on the X
;				axis.
;	ascii		input	If set, then save as an ASCII GRAFFER
;				file (by default a binary graffer file
;				is generated).
;	noclip		input	If  set, then disable clipping to the
;				axis box. 
;	mouse	int	input	If explicitly set to zero then
;				disable mouse-editing of the dataset.
;	z_format int	input	For 2-D datasets, select display
;				format (0=contour, 1=colour image)
;	z_nlevels int	input	For 2D datasets, select number of
;				automatic contours
;	z_lmap	int	input	Select mapping for automatic contour
;				levels. 0=linear, 1=log, 2=sqrt
;	z_levels float	input	For 2D datasets, select levels for
;				explicit contours
;	z_colours int	input	For 2D datasets, select the colours
;				for the contours
;	z_style	int	input	For 2D datasets, select linestyles for contours
;	z_thick	float	input	For 2D datasets, select line
;				thicknesses for contours
;	z_label	int	input	Specify the interval of contours for labelling.
;	/z_fill		input	If set, then fill the contours.
;	z_range	int	input	For 2-D datasets, specify the cutoff
;				range for image displays
;	z_pxsize float	input	For 2D datasets, specify the pixel
;				size to use in images for PS device.
;	/z_invert	input	For image display, invert the colour
;				table if set.
;	z_mode	int	input	Z image scaling mode, 0=linear, 1=log, 2=sqrt.
;	z_ctable int	input	Select a colour table for 2-D display
;				of images.
;	y_axis	int	input	Specify which Y axis to use. (0 or 1)
;	/make_current	input	If set, then make the modified dataset
;				into the current dataset when the file
;				is next opened.
;	x_values double	input	New X values.
;	y_values double	input	New Y values.
;	z_values double	input	New Z values.
;	x_func	string	input	New x=f(y) or x=f(t).
;	y_func	string	input	New y=f(x) or y=f(t).
;	z_func	string	input	New f(x,y).
;	xy_file	string	input	File for new 1-D data.
;	z_file	string	input	File for new 2-D data.
;	errors	double	input	New error values (deprecated).
;	errtype	string	input	Specify error types as code
;				(e.g. "XXY" for asymmetrical errors in
;				X and symmetric errors in Y)
;	x_errors double	input	New X error values.
;	y_errors double	input	New Y error values.
;	frange  float	input	The range of x, y or t over which to
;				plot a function
;	z_missing float	input	A missing value to use for warped images.
;	x_scale	float	input	Scale X values by this factor
;	x_shift	float	input	Shift X values by this amount
;	y_scale	float	input	Scale Y values by this factor
;	y_shift	float	input	Shift Y values by this amount
;	z_scale	float	input	Scale Z values by this factor
;	z_shift	float	input	Shift Z values by this amount
;	status	int	output	A named variable to be set to 0 on
;				failure or 1 on success
;	/retain_unset	input	If set, then unspecified data fields
;				are retained when a subset of data
;				fields is given.
;	/delete			If set, then delete the dataset. Will
;				prompt for verification. Either an
;				index or a name must be explicitly
;				given.
;
; Restrictions:
;	Does not allow changing dataset type.
;	If neither the index or the name is given, then the current
;	dataset is modified.
;	Specifying both is an error.
;	If name is given then (1) If there are 2 datasets of the same
;	name, the first will be modified, (2) if the name is not found,
;	then the program exits without updating.
;	If shift & scale are given for the same axis, scale is applied
;	first.
;
; Notes:
; 	The /RETAIN_UNSET key allows single axes of a dataset to be
; 	changed without resetting the others (like specifying a dot in
; 	the GUI to read local variables).
; 	To work, the change must not affect the dimension of the
; 	dataset (for Z datasets a 1-D X or Y map can be replaced by
; 	2-D or vice-versa, provided Z is still matched).
; 	If specified, error limits must provide all errors.
; 	If a dataset has errors, but no new ones are given, then
; 	unchanged axes retain their errors, while changed axes lose theirs.
; 	If /delete is given, then all other keywords apart from /ascii
; 	and name are ignored.
; 	
; History:
;	Original (after graff_add): 20/12/11; SJT
;	Add option to select secondary Y-axis: 23/12/11; SJT
;	Add options to update data: 25/1/12; SJT
;	Allow matching by name: 26/1/12; SJT
;	Correct interpretation of FUNCX & FUNCY: 2/2/12; SJT
;	Deprecate func[xyz], replace with [xyz]_func: 3/2/12; SJT
;	Add STATUS keyword: 24/2/12; SJT
;	Add X & Y shifts and scale: 26/1/16; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Allow long/triple colours: 1/3/19; SJT
;	Start extraction of data updates: 1/4/22; SJT
;	/Retain_unset should now work: 2/4/22; SJT
;	Add Z shift & scale: 5/4/22; SJT
;	Add /delete: 4/5/22; SJT
;-

  on_error, 2                   ; Return to caller on error

  status = 0
  if ~file_test(file) then message, "File does not exist"

  if keyword_set(funcx) then begin 
     if keyword_set(x_func) then $
        message, /continue, $
                 "Both X_FUNC and the obsolete FUNCX are set, ignoring FUNCX" $
     else begin
        message, /continue, "FUNCX is obsolete, please use X_FUNC " + $
                 "instead"
        x_func = funcx
     endelse
  endif
  if keyword_set(funcy) then begin 
     if keyword_set(y_func) then $
        message, /continue, $
                 "Both Y_FUNC and the obsolete FUNCY are set, ignoring FUNCY" $
     else begin
        message, /continue, "FUNCY is obsolete, please use Y_FUNC " + $
                 "instead"
        y_func = funcy
     endelse
  endif
  if keyword_set(funcz) then begin 
     if keyword_set(z_func) then $
        message, /continue, $
                 "Both Z_FUNC and the obsolete FUNCZ are set, ignoring FUNCZ" $
     else begin
        message, /continue, "FUNCZ is obsolete, please use Z_FUNC " + $
                 "instead"
        z_func = funcz
     endelse
  endif

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

  if n_params() eq 2 then begin
     if keyword_set(name) then begin
        message, /continue, $
                 "May not specify dataset by index and by name"
        return
     endif
     index = idx-1
     if keyword_set(make_current) then pdefs.cset = index
  endif else if keyword_set(name) then begin
     locs = where((*pdefs.data).descript eq name, nname)
     if nname eq 0 then begin
        message, /continue, "No match for name "+name+" found in "+file
        return
     endif

     if nname gt 1 then message, /continue, $
                                 "Multiple matches for name "+name+ $
                                 " found in "+file
     index = locs[0]
     if keyword_set(make_current) then pdefs.cset = index
  endif else index = pdefs.cset

  if index lt 0 or index ge pdefs.nsets then begin
     message, "Dataset index out of range", /continue
     return
  endif

  if keyword_set(delete) then begin
     if n_params() eq 1 && ~keyword_set(name) then begin
        print, "Dataset to be deleted must be explicitly specified."
        status = 0
        return
     endif
     ans = ''
     desc = (*pdefs.data)[index].descript
     print, "Do you really want to delete dataset", index+1
     print, "containing: ", desc
     read, ans, prompt = "[y/N] :_"
     if ans ne '' && truth(ans) then $
        graff_dsdel, pdefs, index, /noprompt
     if (keyword_set(ascii)) then gr_asc_save, pdefs $
     else gr_bin_save, pdefs
     graff_clear, pdefs

     status = 1
     return
  endif
  
  dataflag = keyword_set(x_values) || keyword_set(y_values) || $
             keyword_set(z_values) || keyword_set(errors) || $
             keyword_set(x_errors) || keyword_set(y_errors)
  
  if (keyword_set(polar) && ((*pdefs.data)[index].type ge -3 && $
                             (*pdefs.data)[index].type le 8)) then $
                                (*pdefs.data)[index].mode = polar
  if (n_elements(psym) ne 0) then  (*pdefs.data)[index].psym = psym
  if (n_elements(join) ne 0) then (*pdefs.data)[index].pline = join
  if (n_elements(symsize) ne 0) then  (*pdefs.data)[index].symsize $
     = symsize
  if (n_elements(style) ne 0) then (*pdefs.data)[index].line = style
  if (n_elements(colour) ne 0) then  begin
     if n_elements(colour) eq 3 then begin
        (*pdefs.data)[index].colour = -2
        (*pdefs.data)[index].c_vals = colour
     endif else if colour gt 255 then begin
        (*pdefs.data)[index].colour = -2
        (*pdefs.data)[index].c_vals = $
           graff_colours(colour, /triple)
     endif else (*pdefs.data)[index].colour = colour
  endif

  if (n_elements(thick) ne 0) then  (*pdefs.data)[index].thick = thick
  if (keyword_set(sort)) then (*pdefs.data)[index].sort = 1
  if (n_elements(description) ne 0) then $
     (*pdefs.data)[index].descript = description 
  if (n_elements(noclip)  ne 0) then (*pdefs.data)[index].noclip = noclip
  if (n_elements(mouse) ne 0) then (*pdefs.data)[index].medit = mouse
  if (n_elements(y_axis) ne 0) then $
     (*pdefs.data)[index].y_axis = y_axis

  if (keyword_set(rescale)) then begin
     gr_autoscale, pdefs, /xaxis, /ignore
     gr_autoscale, pdefs, /yaxis, /ignore
  endif


  if n_elements(z_format) ne 0 then $
     (*pdefs.data)[index].zopts.format = keyword_set(z_format)
  if keyword_set(z_levels) then begin
     if ptr_valid((*pdefs.data)[index].zopts.levels) then $
        ptr_free, (*pdefs.data)[index].zopts.levels
     (*pdefs.data)[index].zopts.levels = ptr_new(z_levels)
     (*pdefs.data)[index].zopts.N_levels = $
        n_elements(z_levels)
     (*pdefs.data)[index].zopts.set_levels = 1b
  endif else if keyword_set(z_nlevels) then begin
     (*pdefs.data)[index].zopts.N_levels = z_nlevels 
     (*pdefs.data)[index].zopts.set_levels = 0b
  endif
  if keyword_set(z_lmap) then  (*pdefs.data)[index].zopts.lmap = $
     z_lmap

  if n_elements(z_colours) gt 0 then begin
     if ptr_valid((*pdefs.data)[index].zopts.Colours) then $
        ptr_free, (*pdefs.data)[index].zopts.Colours
     if ptr_valid((*pdefs.data)[index].zopts.raw_colours) then $
        ptr_free, (*pdefs.data)[index].zopts.raw_colours
     
     ncolss = n_elements(z_colours)
     (*pdefs.data)[index].zopts.N_cols = ncolss
     case size(z_colours, /type) of
        11:  begin
           (*pdefs.data)[pdefs.cset].zopts.colours = $
              ptr_new(intarr(ncolss))
           (*pdefs.data)[pdefs.cset].zopts.raw_colours = $
              ptr_new(intarr(3, ncolss))
           for j = 0, ncolss-1 do begin
              if n_elements(z_colours[j]) eq 1 then begin
                 (*(*pdefs.data)[pdefs.cset].zopts.colours)[j] = $
                    z_colours[j]
                 (*(*pdefs.data)[pdefs.cset].zopts.raw_colours)[*, $
                                                                j] $
                    = 0
              endif else begin
                 (*(*pdefs.data)[pdefs.cset].zopts.colours)[j] = $
                    -2
                 (*(*pdefs.data)[pdefs.cset].zopts.raw_colours)[*, $
                                                                j] $
                    = graff_colours(z_colours[j])
              endelse
           endfor
        end
        
        7: begin
           gr_cont_col_get, z_colours, icol, rcol
           (*pdefs.data)[pdefs.cset].zopts.N_cols = n_elements(icol)
           (*pdefs.data)[pdefs.cset].zopts.Colours = ptr_new(icol)
           (*pdefs.data)[pdefs.cset].zopts.raw_colours = ptr_new(rcol)
        end
        else: (*pdefs.data)[index].zopts.Colours = $
           ptr_new(fix(z_colours))
     endcase
  endif

  if n_elements(z_ctable) ne 0 then $
     (*pdefs.data)[index].zopts.ctable = z_ctable+1

  if keyword_set(z_style) then begin
     if ptr_valid((*pdefs.data)[index].zopts.style) then $
        ptr_free, (*pdefs.data)[index].zopts.style
     (*pdefs.data)[index].zopts.N_sty = n_elements(z_style)
     (*pdefs.data)[index].zopts.style = ptr_new(z_style)
  endif

  if keyword_set(z_thick) then begin
     if ptr_valid((*pdefs.data)[index].zopts.thick) then $
        ptr_free, (*pdefs.data)[index].zopts.thick
     (*pdefs.data)[index].zopts.N_thick = n_elements(z_thick)
     (*pdefs.data)[index].zopts.thick = ptr_new(float(z_thick))
  endif

  if n_elements(z_charsize) ne 0 then $
     (*pdefs.data)[index].zopts.charsize = z_charsize

  if keyword_set(z_range) then (*pdefs.data)[index].zopts.range $
     = z_range
  if keyword_set(z_label) then (*pdefs.data)[index].zopts.label $
     = z_label

  if keyword_set(z_pxsize) then $
     (*pdefs.data)[index].zopts.pxsize = z_pxsize $
  else (*pdefs.data)[index].zopts.Pxsize = 0.5

  if (n_elements(z_missing) ne 0) then $
     (*pdefs.data)[index].zopts.missing = z_missing
  if n_elements(z_invert) ne 0 then $
     (*pdefs.data)[index].zopts.invert = keyword_set(z_invert)
  if n_elements(z_fill) ne 0 then $
     (*pdefs.data)[index].zopts.fill = keyword_set(z_fill)
  if n_elements(z_mode) ne 0 then $
     (*pdefs.data)[index].zopts.ilog = z_mode $
  else if n_elements(z_log) ne 0 then begin
     (*pdefs.data)[index].zopts.ilog = z_log
     print, "Z_LOG is now deprecated, use Z_MODE"
  endif

; Data updates.

  type = (*pdefs.data)[index].type

  if n_elements(x_shift) ne 0 || n_elements(y_shift) ne 0 || $
     n_elements(x_scale) ne 0 || n_elements(y_scale) ne 0 || $
     n_elements(z_shift) ne 0 || n_elements(z_scale) ne 0 then begin
     if type ne 9 then $
        mscale = [1.d0, 0.d0, 1.d0, 0.d0] $
     else $
        mscale = [1.d0, 0.d0, 1.d0, 0.d0, 1.d0, 0.d0]

     
     if n_elements(x_scale) ne 0 then mscale[0] = x_scale
     if n_elements(x_shift) ne 0 then mscale[1] = x_shift
     if n_elements(y_scale) ne 0 then mscale[2] = y_scale
     if n_elements(y_shift) ne 0 then mscale[3] = y_shift
     if type eq 9 then begin
        if n_elements(z_scale) ne 0 then mscale[4] = z_scale
        if n_elements(z_shift) ne 0 then mscale[5] = z_shift
     endif
  endif

  if type ge 0 && type le 8 then begin ; A normal 1-D data set.
     if keyword_set(z_values) then $
        message, "Cannot convert a 1-D dataset to a 2-D dataset"
     if keyword_set(x_func) or keyword_set(y_func) or $
        keyword_set(z_func) then $
           message, "Cannot convert a data dataset to a function"

     if dataflag then begin
        data = (*pdefs.data)[index]
        if keyword_set(errors) && $
           ~(keyword_set(x_errors) || keyword_set(y_errors)) then begin
           message, /cont, "X_ERRORS & Y_ERRORS are now preferred to ERRORS."
           ok = gr_update_xy_old(data, x_values, y_values, errors, $
                                 errtype, keyword_set(retain_unset))
        endif else $
           ok = gr_update_xy(data, x_values, y_values, x_errors, $
                             y_errors, keyword_set(retain_unset))
        
        if ok then (*pdefs.data)[index] = data
     endif else if n_elements(mscale) eq 4 then begin

        xydata = *(*pdefs.data)[index].xydata

        *xydata.x = *xydata.x * mscale[0] + mscale[1]
        *xydata.y = *xydata.y * mscale[2] + mscale[3]

        if ptr_valid(xydata.x_err) then $
           *xydata.x_err = *xydata.x_err * mscale[0]
        if ptr_valid(xydata.y_err) then $
           *xydata.y_err = *xydata.y_err * mscale[2]
        
        *(*pdefs.data)[index].xydata = xydata

     endif
     
  endif else if type eq 9 then begin ; 2-D dataset
     if keyword_set(x_func) or keyword_set(y_func) or $
        keyword_set(z_func) then $
           message, "Cannot convert a data dataset to a function"

     if dataflag then begin
        data = (*pdefs.data)[index]
        ok = gr_update_z(data, z_values, x_values, y_values, $
                         keyword_set(retain_unset))
        if ok then (*pdefs.data)[index] = data
 
     endif else if n_elements(mscale) eq 6 then begin
        xydata = *(*pdefs.data)[index].xydata
        *xydata.x = *xydata.x*mscale[0] + mscale[1]
        *xydata.y = *xydata.y*mscale[2] + mscale[3]
        *xydata.z = *xydata.z*mscale[4] + mscale[5]

        *(*pdefs.data)[index].xydata = xydata
     endif
     
  endif else if type eq -4 then begin ; 2-D function
     if keyword_set(x_func) or keyword_set(y_func) then $
        message, "Cannot convert a 2-D function to 1-D"
     if keyword_set(x_values) or keyword_set(y_values) or $
        keyword_set(z_values) then $
           message, "Cannot convert a function dataset to data"

     if keyword_set(z_func) then $
        ((*pdefs.data)[index].xydata).funct = z_func
     if keyword_set(neval) then begin
        if n_elements(neval) eq 1 then begin
           (*pdefs.data)[index].ndata = neval
           (*pdefs.data)[index].ndata2 = neval
        endif else begin
           (*pdefs.data)[index].ndata = neval[0]
           (*pdefs.data)[index].ndata2 = neval[1]
        endelse
     endif
     if keyword_set(frange) then (*(*pdefs.data)[index].xydata).range $
        = frange
     
  endif else if type eq -3 then begin ; Parametric function
     if keyword_set(z_func) then message, $
        "Cannot convert a 1-D function to 2-D"
     if keyword_set(x_values) or keyword_set(y_values) or $
        keyword_set(z_values) then $
           message, "Cannot convert a function dataset to data"

     if keyword_set(x_func) then $
        (*(*pdefs.data)[index].xydata).funct[0] = x_func
     if keyword_set(y_func) then $
        (*(*pdefs.data)[index].xydata).funct[1] = y_func
     if keyword_set(frange) then (*(*pdefs.data)[index].xydata).range $
        = frange
     if keyword_set(neval) then (*pdefs.data)[index].ndata = neval
     
  endif else if type eq -2 then begin ; f(y)
     if keyword_set(z_func) then message, $
        "Cannot convert a 1-D function to 2-D"
     if keyword_set(x_values) or keyword_set(y_values) or $
        keyword_set(z_values) then $
           message, "Cannot convert a function dataset to data"
     if keyword_set(y_func) then message, $
        "Cannot convert f(y) to f(x)"

     if keyword_set(x_func) then $
        (*(*pdefs.data)[index].xydata).funct = x_func
     if keyword_set(frange) then (*(*pdefs.data)[index].xydata).range $
        = frange
     if keyword_set(neval) then (*pdefs.data)[index].ndata = neval
     
  endif else if type eq -1 then begin ; f(x)
     if keyword_set(z_func) then message, $
        "Cannot convert a 1-D function to 2-D"
     if keyword_set(x_values) or keyword_set(y_values) or $
        keyword_set(z_values) then $
           message, "Cannot convert a function dataset to data"
     if keyword_set(x_func) then message, $
        "Cannot convert f(x) to f(y)"

     if keyword_set(y_func) then $
        (*(*pdefs.data)[index].xydata).funct = y_func
     if keyword_set(frange) then (*(*pdefs.data)[index].xydata).range $
        = frange
     if keyword_set(neval) then (*pdefs.data)[index].ndata = neval
  endif


  if (keyword_set(ascii)) then gr_asc_save, pdefs $
  else gr_bin_save, pdefs
  graff_clear, pdefs

  status = 1

end
