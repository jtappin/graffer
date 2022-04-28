; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_CH_DSET
;	Change data set
;
; Usage:
;	graff_ch_dset, pdefs
;
; Argument:
;	pdefs	struct	in/out	The plot definition structure.
;
; History:
;	Original (essentially a widget version of graff_dset): 7/9/95; SJT
;	Add mouse-editing default option: 13/8/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Move top level options out of PDEFS: 21/5/20; SJT
;-

function Grf_ch_event, event

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, event.id, get_uvalue = but

  iexit = 0
  case (but) of
     'CHOOSE': begin
        uv.select = event.index
        iexit = 0
     end

     'DO': iexit = 1
     'DONT': iexit = -1
  end

  widget_control, base, set_uvalue = uv, /no_copy

  return, {id:event.handler, $
           top:event.top, $
           handler:event.handler, $
           Exit:iexit}
end

pro Graff_ch_dset, pdefs

  common graffer_options, optblock
  
  tc = ['F(XY)', 'PF', 'F(Y)', 'F(X)', 'XY', 'XYE', 'XYEE', 'XYF', $
        'XYFF', $
        'XYFE', 'XYFEE', 'XYFFE', 'XYFFEE', 'Z']

  dlist = [(*pdefs.data).descript, '<New>']

  nlist = string(indgen(n_elements(dlist))+1, $
                 format = "(I3,')')")

  llist = string([(*pdefs.data).ndata, 0], format = "(' <',I0,'> ')")
  lll = max(strlen(llist))
  fmt = "(A"+string(lll, format = "(I0)")+")"
  llist = string(llist, format = fmt)

  clist = replicate(' ', n_elements(llist))
  clist(pdefs.cset) = '*'

  nlist = nlist+llist+clist

  nlist = nlist+string([tc((*pdefs.data).type+4), ''], format = "(A6, ' - ')")

  dlist = nlist+dlist

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer data set select', $
                    group = pdefs.ids.graffer, $
                    resource = 'Graffer', $
                    /column)
  
  base = widget_base(tlb, $
                     /column)

  curr = widget_label(base, $
                      value = 'Data Sets')
  junk = widget_list(base, $
                     value = dlist, $
                     uvalue = 'CHOOSE',  $
                     ysize = (12 < n_elements(dlist)))
  widget_control, junk, set_list_select = pdefs.cset

  jb = widget_base(base, $
                   /row)
  
  junk = widget_button(jb, $
                       value = 'Apply', $
                       uvalue = 'DO')
  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  widget_control, base, set_uvalue = {dlist:dlist,  $
                                      Select:pdefs.cset}, $
                  event_func = 'grf_ch_event'

  widget_control, tlb, /real

;			DIY widget management here

  repeat begin
     ev = widget_event(base)
  endrep until ev.exit ne 0

  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exit eq -1) then begin
     return
  endif

  pdefs.cset = uv.select
  
  if (pdefs.cset eq pdefs.nsets) then begin ; Need to extend the data
                                ; structure
     
     (*pdefs.data) = [(*pdefs.data), {graff_data}]
     (*pdefs.data)[pdefs.cset].Pline =    1
     (*pdefs.data)[pdefs.cset].Symsize =  1.
     (*pdefs.data)[pdefs.cset].Colour =   1
     (*pdefs.data)[pdefs.cset].Thick =    1.
     (*pdefs.data)[pdefs.cset].Medit =    optblock.mouse

     (*pdefs.data)[pdefs.cset].zopts.N_levels =  6
     (*pdefs.data)[pdefs.cset].zopts.N_cols =    1
     (*pdefs.data)[pdefs.cset].zopts.Colours =   ptr_new(1)
     (*pdefs.data)[pdefs.cset].zopts.raw_colours = ptr_new(intarr(3))
     (*pdefs.data)[pdefs.cset].zopts.N_sty =     1
     (*pdefs.data)[pdefs.cset].zopts.style = ptr_new(0)
     (*pdefs.data)[pdefs.cset].zopts.N_thick =   1
     (*pdefs.data)[pdefs.cset].zopts.Thick =     ptr_new(1.)
     (*pdefs.data)[pdefs.cset].zopts.Pxsize =    0.5

     pdefs.nsets = pdefs.nsets+1
  endif

  graff_set_vals, pdefs, /set_only

end
