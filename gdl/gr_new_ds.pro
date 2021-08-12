; Copyright (C) 2013-2020
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

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
;-

  if n_params() eq 2 then $
     ds = replicate({graff_data}, nds) $
  else ds = {graff_data}

  ds[*].Pline = 1
  ds[*].Symsize =  1.
  ds[*].Colour =   1
  ds[*].Thick =    1.
  ds[*].Zopts.N_levels = 6
  ds[*].Zopts.N_cols =  1
  ds[*].Zopts.Colours = ptr_new(1)
  ds[*].zopts.raw_colours = ptr_new(intarr(3))
  ds[*].Zopts.N_sty = 1
  ds[*].Zopts.style = ptr_new(0)
  ds[*].Zopts.N_thick =  1
  ds[*].Zopts.Thick = ptr_new(1.)
  ds[*].Zopts.Pxsize =  0.5

  ds[*].Medit = 0

  ds[*].max_val = !values.d_nan
  ds[*].min_val = !values.d_nan

  return, ds

end
