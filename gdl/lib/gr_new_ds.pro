; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_new_ds, pdefs, nds

;+
; GR_NEW_DS
;	Create a new properly initialized graffer dataset.
;
; Usage:
;	ds = gr_new_ds(pdefs[, nds])
;
; Argument:
;	pdefs	struct	input	The graffer control structure.
;	nds	int	input	The number of datasets to add (1 if
;				not present)
;
; Returns:
;	A {graff_data} structure with proper initializations.
;
; History:
; 	Original: 10/1/12; SJT
; 	Remove opts field from pdefs: 21/5/20; SJT
; 	Add min & max values: 4/3/15; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Move top level options out of PDEFS: 21/5/20; SJT
;	Set charsize for contour labels to 1 (not zero): 4/11/21; SJT
;-

  common graffer_options, optblock
  
  if n_params() eq 2 then $
     ds = replicate({graff_data}, nds) $
  else ds = {graff_data}

  ds[*].pline = 1
  ds[*].symsize =  1.
  ds[*].colour =   1
  ds[*].thick =    1.

  ds[*].zopts.n_levels = 6
  ds[*].zopts.lmap = 0
  ds[*].zopts.n_cols =  1
  ds[*].zopts.colours = ptr_new(1)
  ;; ds[*].zopts.raw_colours = ptr_new(intarr(3))
  ds[*].zopts.n_sty = 1
  ds[*].zopts.style = ptr_new(0)
  ds[*].zopts.n_thick =  1
  ds[*].zopts.thick = ptr_new(1.)
  ds[*].zopts.pxsize =  0.5
  ds[*].zopts.charsize = 1.0d0
  
  ds[*].medit = optblock.mouse

  ds[*].max_val = !values.d_nan
  ds[*].min_val = !values.d_nan

  return, ds

end
