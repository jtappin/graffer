; LICENCE:
; Copyright (C) 2022: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_DS_ERASE
;	Erase the contents and optionally the formatting of a dataset.
;
; Usage:
;	ok = gr_ds_erase(pdefs, index)
;
; Returns:
;	1 if the dataset could be erase, 0 if not (uninitialized)
;
; Arguments:
;	pdefs	struct	The top level GRAFFER structure.
;	index	long	Which dataset to transpose (defaults to
;			current DS).
;
; Keywords:
;	/full	If set then erase formatting information as well.
;
; History:
;	Original:  20/5/22; SJT
;-

function gr_ds_erase, pdefs, index, full = full
  common graffer_options, optblock

  if n_params() eq 1 then index = pdefs.cset
  
  if ~ptr_valid((*pdefs.data)[index].xydata) && ~keyword_set(full) then begin
     graff_msg, pdefs.ids.message, "Cannot erase an uninitialized dataset."
     return, 0
  endif
  if ptr_valid((*pdefs.data)[index].xydata) then begin
     if (*pdefs.data)[index].type eq 9 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).z $
     else if (*pdefs.data)[index].type ge 0 then ptr_free, $
        (*(*pdefs.data)[index].xydata).x, $
        (*(*pdefs.data)[index].xydata).y, $
        (*(*pdefs.data)[index].xydata).x_err, $
        (*(*pdefs.data)[index].xydata).y_err
     ptr_free, (*pdefs.data)[index].xydata
  endif

  (*pdefs.data)[index].ndata = 0
  (*pdefs.data)[index].ndata2 = 0
  (*pdefs.data)[index].type = 0

  if keyword_set(full) then begin
     (*pdefs.data)[index].mode = 0
     (*pdefs.data)[index].descript = ''
     (*pdefs.data)[index].pline = 1
     (*pdefs.data)[index].psym = 0
     (*pdefs.data)[index].symsize = 1.d
     (*pdefs.data)[index].line = 0
     (*pdefs.data)[index].colour = 1
     (*pdefs.data)[index].c_vals[*] = 0b
     (*pdefs.data)[index].thick = 1.d
     (*pdefs.data)[index].min_val = !values.d_nan
     (*pdefs.data)[index].max_val = !values.d_nan
     (*pdefs.data)[index].y_axis = 0
     (*pdefs.data)[index].sort = 0b
     (*pdefs.data)[index].noclip = 0b
     (*pdefs.data)[index].medit = optblock.mouse

     (*pdefs.data)[index].zopts.format = 0
     (*pdefs.data)[index].zopts.set_levels = 0b
     (*pdefs.data)[index].zopts.n_levels = 6
     (*pdefs.data)[index].zopts.lmap = 0
     (*pdefs.data)[index].zopts.n_cols = 0
     (*pdefs.data)[index].zopts.n_sty = 0
     (*pdefs.data)[index].zopts.range = dblarr(2)
     (*pdefs.data)[index].zopts.missing = 0.d
     (*pdefs.data)[index].zopts.pxsize = 0.1d
     (*pdefs.data)[index].zopts.charsize = 1.d
     (*pdefs.data)[index].zopts.label = 0
     (*pdefs.data)[index].zopts.label_off = 0
     (*pdefs.data)[index].zopts.ctable = 0
     (*pdefs.data)[index].zopts.gamma = 1.d
     (*pdefs.data)[index].zopts.fill = 0b
     (*pdefs.data)[index].zopts.ilog = 0b
     (*pdefs.data)[index].zopts.invert = 0b
     (*pdefs.data)[index].zopts.smooth = 0b
     (*pdefs.data)[index].zopts.shade_levels = 256l

     ptr_free, (*pdefs.data)[index].zopts.levels, $
               (*pdefs.data)[index].zopts.colours, $
               (*pdefs.data)[index].zopts.raw_colours, $
               (*pdefs.data)[index].zopts.style, $
               (*pdefs.data)[index].zopts.thick
     
  endif

  return, 1
end
