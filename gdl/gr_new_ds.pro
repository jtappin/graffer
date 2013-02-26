function gr_new_ds, pdefs

;+
; GR_NEW_DS
;	Create a new properly initialized graffer dataset.
;
; Usage:
;	ds = gr_new_ds(pdefs)
;
; Argument:
;	pdefs	struct	input	The graffer control structure.
;
; Returns:
;	A {graff_data} structure with proper initializations.
;
; History:
; 	Original: 10/1/12; SJT
;-

ds = {graff_data}

ds.Pline = 1
ds.Symsize =  1.
ds.Colour =   1
ds.Thick =    1.
ds.Zopts.N_levels = 6
ds.Zopts.N_cols =  1
ds.Zopts.Colours = ptr_new(1)
ds.Zopts.N_sty = 1
ds.Zopts.style = ptr_new(0)
ds.Zopts.N_thick =  1
ds.Zopts.Thick = ptr_new(1.)
ds.Zopts.Pxsize =  0.5

ds.Medit = pdefs.opts.mouse

return, ds

end
