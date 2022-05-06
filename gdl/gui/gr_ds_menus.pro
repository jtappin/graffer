; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_DS_MENUS
;	Make the menu to set the properties of an XY dataset
;
; Usage:
;	gr_ds_menus, optbb, pdefs
;
; Arguments:
;	optbb	long	input	The widget ID of the parent base
;	pdefs	struct	in/out	The Graffer data & control structure.
;
; History:
;	Farmed out from GRAFFER to facilitate maintenance: 6/12/96;
;	SJT
;	Moved event handler in: 17/1/97; SJT
;	Modify for extended symbol definitions + add compact key:
;	20/1/97; SJT
;	Add CAPTURE key to entry boxes, remove compact key: 6/2/97; SJT
;	Change handles to pointers: 27/6/05
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Add extra colours: 8/2/12; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Add display of current DS colour: 23/8/16; SJT
;	Replace cw_pdtsmenu & last cw_bbselector with cw_pdmenu_plus:
;	28/9/16; SJT
;	Use cw_spin_box for sizes and thicknesses: 29/9/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

pro Gr_dsp_event, event

  common graffer_options, optblock

  widget_control, event.id, get_uvalue = object

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy

  idraw_flag = 1
  ichange = 1b
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1

  if (track_flag) then begin
     idraw_flag = 0
     ichange = 0b
     
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case object of
     'PSYM': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select plotting symbol for current ' + $
                   'data set' $
     else if ~optblock.bitmaps then $
        (*pdefs.data)[pdefs.cset].psym = event.index $
     else (*pdefs.data)[pdefs.cset].psym = event.value
        
     
     'PLINE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select joining style for current ' + $
                   'data set' $
     else begin
        (*pdefs.data)[pdefs.cset].pline = event.index
        widget_control, pdefs.ids.line, sensitive = event.index ne 0
     endelse
     
     'SSIZE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Set size for plot symbol ' + $
                   'in current data set (floating point)' $
     else (*pdefs.data)[pdefs.cset].symsize = event.value
     
     
     'COLOUR': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select colour for current data set' $
     else begin
        if event.value eq graff_colours(/max_index)+2 then begin ; Custom colour
           ci = (*pdefs.data)[pdefs.cset].colour
           if ci eq -2 then $
              cc = gr_custom_colour((*pdefs.data)[pdefs.cset].c_vals, $
                                    pdefs.ids.windex) $
           else cc = gr_custom_colour(ci > 0, pdefs.ids.windex)
           if n_elements(cc) eq 1 then begin
              if ci ne -2 then $
                 widget_control, event.id, set_value = ci+1
           endif else begin
              (*pdefs.data)[pdefs.cset].colour = -2
              (*pdefs.data)[pdefs.cset].c_vals = cc
           endelse
        endif else (*pdefs.data)[pdefs.cset].colour = event.value-1
        gr_show_colour, pdefs
     endelse

     'STYLE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select line style for current data set' $
     else (*pdefs.data)[pdefs.cset].line = event.index
     
     'THICK': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Set line thickness for current ' + $
                   'data set (integer)' $
     else (*pdefs.data)[pdefs.cset].thick = event.value
     
     'POLAR': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select rectangular or polar mode' $
     else (*pdefs.data)[pdefs.cset].mode = event.index
     
     'MINVAL': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set min value to plot for current dataset' $
     else (*pdefs.data)[pdefs.cset].min_val = event.value
     'MAXVAL': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set max value to plot for current dataset' $
     else (*pdefs.data)[pdefs.cset].max_val = event.value

     'XTRA': begin
        val = strsplit(event.value, '.', /extr)
        if (n_elements(val) eq 1) then graff_msg, pdefs.ids.hlptxt, /help, $
                                                  'Set other dataset options' $
        else case val[1] of
           'Sort X axis': if track_flag then $
              graff_msg, pdefs.ids.hlptxt, /help, 'Toggle sorting of X axis' + $
                         ' values before plotting current data set' $
           else (*pdefs.data)[pdefs.cset].sort = event.select
           
           'Clip to box': if (track_flag) then $
              graff_msg, pdefs.ids.hlptxt, /help, 'Toggle clipping of' + $
                         ' current dataset to plot axes' $
           else   (*pdefs.data)[pdefs.cset].noclip = ~event.select
           
           'Mouse editing':  if track_flag then $
              graff_msg, pdefs.ids.hlptxt, /help, 'Toggle use of mouse to ' + $
                         'edit dataset' $
           else begin
              (*pdefs.data)[pdefs.cset].medit = event.select
              
              widget_control, pdefs.ids.draw, get_uvalue = dstate
              if (dstate eq 'DRAW') then widget_control, $
                 pdefs.ids.draw, draw_button_events = $
                 (*pdefs.data)[pdefs.cset].medit, track = $
                 (*pdefs.data)[pdefs.cset].medit 
           endelse
        endcase
     end
     
  endcase
  if (*pdefs.data)[pdefs.cset].type ge 0 then $
     widget_control, pdefs.ids.minmaxbase, sensitive $
                     = (*pdefs.data)[pdefs.cset].mode eq 0
 
  if (idraw_flag) then gr_plot_object, pdefs
  if (ichange) then begin
     pdefs.chflag = 1b
     pdefs.transient.changes = pdefs.transient.changes+1
     if (pdefs.transient.changes gt 20) then begin
        gr_bin_save, pdefs, /auto
     endif
  endif
  widget_control, pdefs.ids.chtick, map = pdefs.chflag

Miss_case:

  widget_control, base, set_uvalue = pdefs, /no_copy

end


pro Gr_ds_menus, optbb, pdefs

  common Gr_psym_maps, psym_bm, col_bm
  common graffer_options, optblock

  if  optblock.bitmaps && n_elements(psym_bm) eq 0 then gr_psym_bitm

  pdefs.ids.plopts[0] = widget_base(optbb, $
                                    /column, $
                                    event_pro = 'gr_dsp_event')

  jjb = widget_base(pdefs.ids.plopts[0], $
                    /row, $
                    /base_align_center)

  if optblock.bitmaps then begin
     maxi = graff_colours(/max)
     col_str = replicate({bitmap: ptr_new()}, maxi+3)
     col_str[0].bitmap = ptr_new(col_bm[*, *, 0])
     col_str[-1].bitmap = ptr_new(col_bm[*, *, 1])
     for j = 0, maxi do col_str[j+1].bitmap = $
        ptr_new(gr_mk_colour_bm(j, size = [80, 16]))
     
  endif else begin
     col_list = ['Omit', $
                 'White (bg)', $
                 'Black', $
                 'Red', $
                 'Green', $
                 'Blue', $
                 'Cyan', $
                 'Magenta', $
                 'Yellow', $
                 'Orange', $
                 '#7f ff 00', $
                 '#00 ff 7f', $
                 '#00 7f ff', $
                 '#7f 00 ff', $
                 'Mauve', $
                 'Dark Grey', $
                 'Light Grey', $
                 'Dark Red', $
                 'Light Red', $
                 'Dark Green', $
                 'Light Green', $
                 'Dark Blue', $
                 'Light Blue', $
                 'Dark Cyan', $
                 'Light Cyan', $
                 'Dark Magenta', $
                 'Light Magenta', $
                 'Dark Yellow', $
                 'Light Yellow', $
                 'Custom']
     col_str = replicate({label: ''}, n_elements(col_list))
     col_str.label = col_list
  endelse

  junk = widget_label(jjb, $
                      value = 'Colour:')
  
  pdefs.ids.colour = cw_pdmenu_plus(jjb, $
                                    col_str, $
                                    uvalue = 'COLOUR', $
                                    /selector, $
                                    track = optblock.track, $
                                    /pad_labels)

  pdefs.ids.dscolour_base = widget_base(jjb, $
                                        /frame)
  pdefs.ids.dscolour_show = widget_draw(pdefs.ids.dscolour_base, $
                                        xsize = 64, $
                                        ysize = 24)

                                ; Colour, linestyle, thickness etc.

  jjb = widget_base(pdefs.ids.plopts[0], /row)



  pdefs.ids.pline = widget_droplist(jjb, $
                                    value = ['None', $
                                             'Line', $
                                             'Histo'], $
                                    uvalue = 'PLINE', $
                                    title = 'Join:', $
                                    track = optblock.track)

  jjb = widget_base(pdefs.ids.plopts[0], /row)

  pdefs.ids.line = widget_droplist(jjb, $
                                   value = ['____',  $
                                            '....', $
                                            '_ _ ', $
                                            '_._.', $
                                            '_...', $
                                            '__  '], $
                                   uvalue = 'STYLE', $
                                   title = 'Style:', $
                                   track = optblock.track)

  jjb = widget_base(pdefs.ids.plopts[0], /row)
  pdefs.ids.thick = cw_spin_box(jjb, $
                                /double, $
                                /all_events, $
                                format = "(f0.1)", $
                                xsize = 6, $
                                value = 1., $
                                uvalue = 'THICK', $
                                label = 'Thickness:', $
                                track = optblock.track, $
                                /capture, $
                                min = 0., $
                                step = 1., $
                                /simple)

  jjb = widget_base(pdefs.ids.plopts[0], /row)

  sz = size(psym_bm, /dim)
  if ~optblock.bitmaps then begin
     symlist = ['No symbol', 'Plus', 'Asterisk', 'Dot', 'Diamond', $
                'Triangle', 'Square', 'Cross', 'Circle', 'Filled ' + $
                'Diamond', 'Filled Triangle', 'Filled Square', $
                'Filled Circle', 'Down Triangle', $
                'Filled Down Triangle', 'Hexagon', 'Filled Hexagon', $
                'Horizontal', 'Vertical']
     pdefs.ids.psym = widget_droplist(jjb, $
                                      value = symlist, $
                                      track = optblock.track, $
                                      title = 'Symbol:', $
                                      uvalue = 'PSYM')
  endif else begin
     junk = widget_label(jjb, $
                         value = 'Symbol:')

     
     bmstruct = replicate({bitmap: psym_bm[*, *, 0]}, sz[2])
     for j = 0, sz[2]-1 do bmstruct[j].bitmap = psym_bm[*, *, j]

     pdefs.ids.psym = cw_pdmenu_plus(jjb, $
                                     bmstruct, $
                                     uvalue = 'PSYM', $
                                     /selector, $
                                     track = optblock.track) 
  endelse
                                ; Change symbol size

  jjb = widget_base(pdefs.ids.plopts[0], /row)
  pdefs.ids.symsize = cw_spin_box(jjb, $
                                  /double, $
                                  /all_events, $
                                  value = '1.0', $
                                  uvalue = 'SSIZE', $
                                  xsize = 7, $
                                  label = 'Size:', $
                                  format = "(f0.2)", $
                                  track = optblock.track, $
                                  /capture, $
                                  min = 0., $
                                  step = 0.1, $
                                  /simple)


  jjb = widget_base(pdefs.ids.plopts[0], $
                    /row)

  pdefs.ids.mode = widget_droplist(jjb, $
                                   value = ['Rect', 'Polar', $
                                            'Polar (°)'], $
                                   uvalue = 'POLAR', $
                                   track = optblock.track, $
                                   title = 'Coords:')

  

  pdefs.ids.minmaxbase = widget_base(pdefs.ids.plopts[0], $
                                     /column)
  pdefs.ids.minval = cw_enter(pdefs.ids.minmaxbase, $
                              /double, $
                              format = "(g12.4)", $
                              /all_event, $
                              xsize = 13, $
                              uvalue = 'MINVAL', $
                              label = 'Min Value:', $
                              track = optblock.track, $
                              /capture, $
                              /empty_nan)
  pdefs.ids.maxval = cw_enter(pdefs.ids.minmaxbase, $
                              /double, $
                              format = "(g12.4)", $
                              /all_event, $
                              xsize = 13, $
                              uvalue = 'MAXVAL', $
                              label = 'Max Value:', $
                              track = optblock.track, $
                              /capture, $
                              /empty_nan)

  jjb = widget_base(pdefs.ids.plopts[0], $
                    /row)
  xtras = [{CW_PDSMENU_S, flag:3, label:'Extras', state:0b}, $
           {cw_pdsmenu_s, 4, 'Sort X axis', 0b}, $
           {cw_pdsmenu_s, 4, 'Clip to box', 0b}, $
           {cw_pdsmenu_s, 6, 'Mouse editing', 0b}]


  junk = cw_pdmenu_plus(jjb, $
                        xtras, $
                        return_type = 'full_name', $
                        uvalue = 'XTRA', $
                        track = optblock.track, $
                        ids = buts)

  pdefs.ids.dsxtra = buts[1:*]
  
end
