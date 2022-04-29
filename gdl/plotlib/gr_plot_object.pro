; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_plot_object, pdefs, no_null = no_null, charsize = charsize, $
                    plot_all = plot_all, grey_ps = grey_ps

;+
; GR_PLOT_OBJECT
;	Plots the plot specified by pdefs
;
; Usage:
;	gr_plot_object, pdefs	; Not intended to be used directly by
;				; the user
;
; Argument:
;	pdefs	struct	input	The structure containing the plot
;				definition
;
; Keyword:
;	no_null		input	If set, then don't plot null text
;				strings as "<NULL>"
;	charsize float	input	The character size to use as a
;				multiplier to the pdefs sizes (for
;				hardcopies)
;	plot_all	input	If set, then don't supress 2-D plots
;				even when this has been requested.
;	grey_ps		input	If set & non-zero, then the plot is to
;				a PS device without the COLOUR option.
;
; Side Effect:
;	A plot is generated on the current device.
;
; History:
;	Original: 2/8/95; SJT
;	Add text handling: 16/8/95; SJT
;	Add facilities for string-type data: 18/8/95; SJT
;	Move log axis locks here: 22/8/95; SJT
;	add charsize key: 4/9/95; SJT
;	Change GR_ERRPLOT calls to allow use of limits: 11/6/96; SJT
;	Add code to allow (a) addition of axes at the origin and (b)
;	Polar plots: 12/8/96; SJT
;	Use !{XY}.CRANGE for function limits: 29/8/96; SJT
;	Rename as GR_PLOT_OBJECT (was plot_object): 18/9/96; SJT
;	Add call to empty to force an update of the screen: 18/3/98; SJT
;	Turn handles into pointers: 28/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add "current only" options: 26/1/12; SJT
;	Handle redrawn axes here: 20/1/16; SJT
;	Move top level options out of PDEFS: 21/5/20; SJT
;-

  common graffer_options, optblock
  
  if pdefs.xtype && min(pdefs.xrange) le 0 then begin
     graff_msg, pdefs.ids.message, ['WARNING: zero or negative limit in ' + $
                                    'log plot', $ 
                                    '         not plotting']
     return    
  endif

  if pdefs.ytype && min(pdefs.yrange) le 0 then begin
     graff_msg, pdefs.ids.message, ['WARNING: zero or negative limit in ' + $
                                    'log plot', $ 
                                    '         not plotting']
     return    
  endif

  if pdefs.y_right && pdefs.ytype_r && min(pdefs.yrange_r) le 0 then begin
     graff_msg, pdefs.ids.message, ['WARNING: zero or negative limit in ' + $
                                    'log plot', $ 
                                    '         not plotting']
     return    
  endif

  xrange = pdefs.xrange
  yrange = pdefs.yrange

  if (xrange[0] eq xrange[1]) then begin
     graff_msg, pdefs.ids.message, ['WARNING: degenerate x-axis ' + $
                                    'range', $
                                    '         not plotting']
     return    
  endif
  if (yrange[0] eq yrange[1]) then begin
     print, yrange
     graff_msg, pdefs.ids.message, ['WARNING: degenerate y-axis ' + $
                                    'range', $
                                    '         not plotting']
     return    
  endif

  if pdefs.y_right then begin
     yrange1 = pdefs.yrange_r
     if (yrange1[0] eq yrange1[1]) then begin
        graff_msg, pdefs.ids.message, ['WARNING: degenerate secondary ' + $
                                       'y-axis range', $ 
                                       '         not plotting']
        return    
     endif
  endif

  if (not keyword_set(charsize)) then csiz = 1.0 $
  else csiz = charsize

  gr_pl_axes, pdefs, csiz

  xrange = !X.crange
  if (pdefs.xtype) then xrange = 10^xrange
  yrange = !Y.crange
  if (pdefs.ytype) then yrange = 10^yrange
  pdefs.ytransform[0] = !y

  if pdefs.y_right then begin
     gr_pl_axes, pdefs, csiz, /secondary
     pdefs.ytransform[1] = !y
  endif

  ndata = (*pdefs.data).ndata
  type = (*pdefs.data).type

  plot2 = keyword_set(plot_all) || (~optblock.s2d)

  reaxis = 0b
  for i = 0, pdefs.nsets-1 do begin
     if pdefs.transient.current_only &&  (i ne pdefs.cset || $
                                          pdefs.transient.mode eq 1) $
     then continue

     if not ptr_valid((*pdefs.data)[i].xydata) then continue

     if (pdefs.y_right) then $
        gr_pl_axes, pdefs, csiz, secondary = (*pdefs.data)[i].y_axis, $
                    /setup
        ;; !y = pdefs.ytransform[(*pdefs.data)[i].y_axis]
     
     if (type[i] eq -4) then begin
        if (plot2) then $
           gr_2df_plot, pdefs, i, csiz, $
                        grey_ps = grey_ps, shaded = reaxis ; 2 D function
     endif else if (type[i] lt 0) then begin
        gr_1df_plot, pdefs, i, csiz             ; Function, data is struct
     endif else if (type[i] eq 9) then begin    ; 2 D data
        if (plot2) then $
           gr_2dd_plot, pdefs, i, csiz, grey_ps = grey_ps, $
                        shaded = reaxis
     endif else $                      ; Observations, xydata is
        gr_1dd_plot, pdefs, i, csiz    ; floating point

  endfor

  if reaxis then begin
     gr_pl_axes, pdefs, csiz, /overlay
     if pdefs.y_right then gr_pl_axes, pdefs, csiz, /overlay, /secondary
  endif

; Key or text strings (if plotted in data coordinates) always go by
; the primary Y-axis

  gr_pl_axes, pdefs, csiz, /setup

  if (pdefs.key.use && ~pdefs.transient.current_only) then $
     gr_pl_key, pdefs, csiz     ; Must follow
                                ; plotting the traces else grayscale
                                ; may obscure


;	Text strings

  show_text = pdefs.transient.mode eq 1 || ~pdefs.transient.current_only

  if (pdefs.ntext gt 0 && show_text) then begin
     
     opf = !P.font
     
     for j = 0, pdefs.ntext-1 do $
        gr_pl_text, (*pdefs.text)[i], csiz, no_null = $
                    keyword_set(no_null) 
     

     !P.font = opf
     
  endif

; Set the Y-transform appropriate to the current dataset.
  if pdefs.y_right then $
     gr_pl_axes, pdefs, csiz, $
                 secondary = (*pdefs.data)[pdefs.cset].y_axis, $
                 /setup
     ;; !y = pdefs.ytransform[(*pdefs.data)[pdefs.cset].y_axis]

  empty                         ; Force an update

  pdefs.transient.opflag = 0b

end
