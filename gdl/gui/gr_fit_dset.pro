; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_FIT_DSET
;	Select a data set to do a least-squares fit.
;
; Usage:
;	ichange = graff_ch_dset(pdefs)
;
; Return value:
;	ichange	int	1 if the DO button was used, 0 if cancel
;
; Argument:
;	pdefs	struct	input	The plot definition structure.
;
; History:
;	Original (from GRAFF_CH_DSET): 28/8/96; SJT
;	Modify to support median fits and simplify internal
;	representations: 23/9/96; SJT
;	Change to make this the outer routine & allow higher degree
;	fits: 22/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add options to do "piecewise linear" fits: 4/2/97; SJT
;	Reorganize to give neater layout: 5/2/97; SJT
;	Add capture key to input boxes: 6/2/97; SJT
;	Change handles to pointers: 27/6/05; SJT
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

function Grf_fit_event, event

  base = widget_info(event.top, /child)
  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, event.id, get_uvalue = but

  iexit = 0                     ;	Change to make this the outer
                                ;	routine & allow higher
                                ;	degree 
                                ;	fits: 22/11/96; SJT

  case (but) of
     'CHOOSE': begin
        uv.chosen = event.index
        widget_control, uv.chid, set_value = uv.list(event.index)
        widget_control, uv.dobid, /sensitive
        widget_control, uv.upid, /sensitive
     end
     
     'FITT': uv.type[0] = event.index
     'FITD': if (event.index ne uv.type[1]) then begin
        if (uv.type[0] eq 1 or uv.type[0] eq 2) then begin
           uv.type[0] = uv.type[0] xor 3
           widget_control, uv.ftype, set_value = uv.type[0]
        endif
        uv.type[1] = event.index
     endif
     'FITN': begin
        uv.type[2] = event.index
        widget_control, uv.degid, sensitive = (event.index eq 0)
     end
     'NAN': uv.nan = event.select
     
     'DEGREE': uv.type[3] = event.value
     'NPTS': uv.nid = event.value
     'SLICE': uv.slice = event.value

     'DONT': iexit = -1
     'DO': iexit = 1
     'UPDATE': iexit = 2
       
  end

  widget_control, base, set_uvalue = uv, /no_copy

  return, {id:event.handler, top:event.top, handler:event.handler, $
           Exit:iexit}
end

function Gr_fit_dset, pdefs

  fflag = ((*pdefs.data)[pdefs.cset].type eq -1 or  $
           (*pdefs.data)[pdefs.cset].type eq -2 or $
           (*pdefs.data)[pdefs.cset].ndata eq 0)

  tc = ['PF', 'F(Y)', 'F(X)', 'XY', 'XYE', 'XYEE', 'XYF', 'XYFF', $
        'XYFE', 'XYFEE', 'XYFFE', 'XYFFEE']
  fitts = ['Polynomial', '"Exponential"', '"Logarithmic"', $
           '"Power-law"', $
           'Piecewise linear']
  fittd = pdefs.ytype + 2*pdefs.xtype

  fitn = ['Least Square', '"Median"']

  fitd = ['y = f(x)', 'x = f(y)']

  dlist = (*pdefs.data).descript

  nlist = string(indgen(n_elements(dlist)), $
                 format = "(I3,')')")

  llist = string((*pdefs.data).ndata, format = "(' <',I0,'> ')")
  lll = max(strlen(llist))
  fmt = "(A"+string(lll, format = "(I0)")+")"
  llist = string(llist, format = fmt)
  nlist = nlist+llist
  nlist = nlist+string([tc((*pdefs.data).type+3)], format = "(A6, ' - ')")

  dlist = nlist+dlist
  arexyd = where((*pdefs.data).type ge 0 and $
                 (*pdefs.data).type le 8 $ 
                 and (*pdefs.data).ndata ge 2, nxy) ; Only
                                ; X/Y datasets 

  if (nxy eq 0) then begin
     message = ['No suitable datasets', $
                'Cannot perform fitting']
     junk = dialog_message(message, $
                           title = 'Graffer: No XY Data', $
                           dialog_parent = pdefs.ids.graffer, $
                           resource = 'Graffer')

     return, 0
     
  endif 

  if (not fflag) then begin
     message = ['CURRENT DATA SET IS XY DATA OR A PARAMETRIC', $ $
                'OR 2-D FUNCTION. USING IT AS A FIT WILL ' + $
                'OVERWRITE', $
                'IT. DO YOU REALLY WANT TO DO THIS?' $
               ] 
     if (dialog_message(message, $
                        title = 'Graffer: No XY Data', $
                        dialog_parent = pdefs.ids.graffer, $
                        resource = 'Graffer', $
                        /question)) eq 'No' then return, 0
  endif

  dlist = dlist(arexyd)

  widget_control, pdefs.ids.graffer, sensitive = 0


  tlb = widget_base(title = 'Graffer data set select', $
                    group = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, $
                     /column)

  b1 = widget_base(base, $
                   /row)

  jb = widget_base(b1, $
                   /column)
  curr = widget_label(jb, $
                      value = 'Data Sets')
  junk = widget_list(jb, $
                     value = dlist, $
                     uvalue = 'CHOOSE', $
                     ysize = (8 < n_elements(dlist)))

  choice = cw_enter(jb, $
                    value = '', $
                    /text, $
                    /no_event, $
                    /display, $
                    xsize = max(strlen(dlist)), $
                    label = 'Current ' + $
                    'Selection', $
                    /column)


  jb = widget_base(b1, $
                   /column, $
                   /base_align_right, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  ftp = widget_droplist(jb, $
                        value = fitts, $
                        title = 'Fit type', $
                        uvalue = 'FITT')
  widget_control, ftp, set_droplist_select = fittd

  junk = widget_droplist(jb, $
                         value = fitn, $
                         title = 'Fit norm', $
                         uvalue = 'FITN')

  junk = widget_droplist(jb, $
                         value = fitd, $
                         title = 'Fit sense', $
                         uvalue = 'FITD')

  degid = cw_spin_box(jb, $
                      /int, $
                      value = 1, $
                      xsize = 3, $
                      label = 'Degree:', $
                      /capture, $
                      /simple, $
                      min = 1, $
                      uvalue = 'DEGREE') 

  npts = cw_spin_box(jb, $
                     /int, $
                     value = 25, $
                     xsize = 4, $
                     label = '# evaluations:', $
                     /capture, $
                     min = 2, $
                     step = 5, $
                     /simple, $
                     uvalue = 'NPTS')

  part = cw_enter(jb, $
                  /text, $
                  value = '', $
                  xsize = 15, $
                  label = 'Slice:', $
                  /capture, $
                  uvalue = 'SLICE', $
                  /all_events)

  jbb = widget_base(jb, $
                    /nonexclusive)

  junk = widget_button(jbb, $
                       value = "Ignore NaNs?", $
                       uvalue = "NAN")
  widget_control, junk, /set_button

  jb = widget_base(base, $
                   /column, $
                   /align_center, $
                   /base_align_center)
  resid = cw_enter(jb, $
                   /text, $
                   value = '', $
                   /no_event, $
                   /display, $
                   xsize = 40, $
                   label = 'Fit', $
                   /column)
  csqid = cw_enter(jb, $
                   /double, $
                   value = 0., $
                   /no_event, $
                   /display, $
                   xsize = 14, $
                   label = 'Prob chance:')

  jb = widget_base(base, /row, /align_center)
  dobid = widget_button(jb, $
                        value = '    Apply    ', $
                        uvalue = 'DO')
  upid = widget_button(jb, $
                       value = '   Update   ', $
                       uvalue = 'UPDATE')
  junk = widget_button(jb, $
                       value = '   Cancel   ', $
                       uvalue = 'DONT')
  widget_control, dobid, sensitive = 0
  widget_control, upid, sensitive = 0

  widget_control, base, set_uvalue = {chosen: 0, $
                                      List:   dlist, $
                                      Chid:   choice, $
                                      Degid:  degid, $
                                      Nid:    npts, $
                                      Dobid:  dobid, $
                                      Upid:   upid, $
                                      Part:   part, $
                                      Ftype:  ftp, $
                                      Np:     25, $
                                      nan:    1b, $
                                      Slice:  '', $
                                      Type:   [fittd, 0, 0, 1]}



  widget_control, base, event_func = 'grf_fit_event'

  widget_control, tlb, /real

  old_data = (*pdefs.data)[pdefs.cset] ; Save the old settings

;			DIY widget management here

  fitflag = 0
  repeat begin
     ev = widget_event(base) 
     if (ev.exit gt 0) then begin
        widget_control, base, get_uvalue = uv
        fitstr = gr_fit_funct(pdefs, uv.type, uv.np, uv.slice, $
                              arexyd(uv.chosen), pr, resid, $
                              nan = uv.nan)
        if (fitstr eq '') then begin
           ev.exit = 0
        endif else begin
           if (ev.exit eq 2) then begin
              widget_control, resid, set_value = fitstr
              widget_control, csqid, set_value = pr
           endif
           gr_plot_object, pdefs
           fitflag = 1
        endelse
     endif
  endrep  until (ev.exit and 1) ne 0

  widget_control, base, get_uvalue = uv, /no_copy
  widget_control, tlb, /destroy
  widget_control, pdefs.ids.graffer, sensitive = 1

  if (ev.exit eq -1) then begin
     if (fitflag) then begin
        (*pdefs.data)[pdefs.cset] = old_data
     endif
     return, 0
  endif

  return, 1

end

