; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_CONT_MENUS
;	Make the menu to set the properties of a contoured ds.
;
; Usage:
;	gr_cont_menus, sb, pdefs
;
; Arguments:
; 	sb	long	input	The base widget into which to put the
; 				menu.
;	pdefs	struct	input	The Graffer data & control structure.
;
; History:
;	Original (after gr_cont_menus): 13/12/11; SJT
;	Revert to original name: 5/1/12; SJT
;	Levels etc. become pointers: 11/1/12; SJT
;	Make contour colours a LIST: 7/10/16; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;-



pro Cont_event, event

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy
  zopts = (*pdefs.data)[pdefs.cset].zopts
  widget_control, event.id, get_uvalue = but

  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  if (track_flag) then begin
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case but of
     'CMODE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Toggle explicit/automatic contour levels' $
     else begin
        zopts.set_levels = event.index
        if (event.index eq 1) then begin
           widget_control, pdefs.ids.zopts.c_levels, get_value = $
                           levels
           if ptr_valid(zopts.levels) then ptr_free, zopts.levels
           zopts.levels = ptr_new(levels[sort(levels)])
           zopts.n_levels = n_elements(levels)
        endif
        widget_control, pdefs.ids.zopts.cl_base[0], $
                        map = event.index eq 0
        widget_control, pdefs.ids.zopts.cl_base[1], $
                        map = event.index eq 1

        widget_control,  pdefs.ids.zopts.c_nlevels, $
                         set_value = zopts.n_levels
     end
     
     'LEVEL': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set explicit contour levels' $
     else begin
        widget_control, event.id, get_value = levels
        idx = uniq(levels, sort(levels))
        levels = levels[idx]
        if ptr_valid(zopts.levels) then ptr_free, zopts.levels
        zopts.levels = ptr_new(levels)
        zopts.n_levels = n_elements(levels)
     endelse

     'NLEVEL': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set number of automatic levels' $
     else begin
        widget_control, event.id, get_value = n_levels
        zopts.n_levels = n_levels
     endelse

     'CL_MAP': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   "Select mapping scheme for " + $
                   "automatic levels" $
     else zopts.lmap = event.index

     'COLOUR': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Set contour colours' $
     else begin
        widget_control, event.id, get_value = col
        gr_cont_col_get, col, icol, rcol, $
                         max_index = graff_colours(/max_index), $
                         status = is_valid
        if is_valid then begin
           if ptr_valid(zopts.colours) then ptr_free, zopts.colours
           if ptr_valid(zopts.raw_colours) then ptr_free, zopts.raw_colours
           zopts.colours = ptr_new(icol)
           zopts.raw_colours = ptr_new(rcol)
           zopts.n_cols = n_elements(col)
        endif
     endelse
     
     'THICK': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Set contour thicknesses' $
     else begin
        widget_control, event.id, get_value = thk
        if ptr_valid(zopts.thick) then ptr_free, zopts.thick
        zopts.thick = ptr_new(thk)
        zopts.n_thick = n_elements(thk)
     endelse
     
     'STYLE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Set contour line styles' $
     else begin
        widget_control, event.id, get_value = sty
        if ptr_valid(zopts.style) then ptr_free, zopts.style
        zopts.style = ptr_new(sty)
        zopts.n_sty = n_elements(sty)
     endelse
     
     'LABEL': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set contour labelling interval' $
     else begin
        widget_control, event.id, get_value = labi
        zopts.label = labi
        widget_control, pdefs.ids.zopts.c_charsize, sensitive = labi $
                        ne 0
        widget_control, pdefs.ids.zopts.c_label_off, sensitive = labi $
                        gt 1
        if labi gt 0 then $
           cw_spin_box_set_max, pdefs.ids.zopts.c_label_off, $
                                (labi-1) > 1
        if zopts.label_off ge labi then begin
           widget_control, pdefs.ids.zopts.c_label_off, set_value = 0
           zopts.label_off = 0
        endif
     endelse
     
     'LABEL_OFF': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   "Set contour labelling offset" $
     else begin
        widget_control, event.id, get_value = labo
        zopts.label_off = labo
     endelse

     'CCSIZE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Set contour labelling character size' $
     else begin
        widget_control, event.id, get_value = ccs
        zopts.charsize = ccs
     endelse

     'FILL': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Toggle filled/outline/feathered contours' $
     else zopts.fill = event.index
  endcase


  if ~track_flag then begin
     (*pdefs.data)[pdefs.cset].zopts = zopts
     if ~(zopts.set_levels and zopts.n_levels eq 0) then $
        gr_plot_object, pdefs
     pdefs.chflag = 1b
     pdefs.transient.changes = pdefs.transient.changes+1
     if (pdefs.transient.changes gt 20) then begin
        gr_bin_save, pdefs, /auto
     endif
     widget_control, pdefs.ids.chtick, map = pdefs.chflag
  endif

Miss_case:

  widget_control, base, set_uvalue = pdefs, /no_copy

end

pro Gr_cont_menus, sb, pdefs

  common graffer_options, optblock

  i = pdefs.cset
  fflag = 0b
  zopts = (*pdefs.data)[i].zopts
  if (zopts.n_cols eq 0) then begin
     zopts.n_cols = 1
     zopts.colours = ptr_new(1)
     zopts.raw_colours = ptr_new(intarr(3))
     fflag = 1b
  endif
  if (zopts.n_thick eq 0) then begin
     zopts.n_thick = 1
     zopts.thick = ptr_new(1.)
     fflag = 1b
  endif
  if (zopts.n_sty eq 0) then begin
     zopts.n_sty = 1
     zopts.style = ptr_new(0)
     fflag = 1b
  endif
  if fflag then (*pdefs.data)[i].zopts = zopts


  pdefs.ids.zopts.bases[0] = widget_base(sb, $
                                         /column, $
                                         event_pro = "cont_event", $
                                         map = ~zopts.format, $
                                         xpad = 0, $
                                         ypad = 0, $
                                         space = 0)

  base = pdefs.ids.zopts.bases[0] ; Just for convenience
  obase = widget_base(base, $
                      /row, $
                      xpad = 0, $
                      ypad = 0, $
                      space = 0)

  jb = widget_base(obase, $
                   /column, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)

  iexpl = zopts.set_levels
  pdefs.ids.zopts.c_auto = widget_droplist(jb, $
                                           value = ['Automatic', $
                                                    'Explicit'], $
                                           uvalue = 'CMODE', $
                                           track = optblock.track)
  widget_control, pdefs.ids.zopts.c_auto, set_droplist_select = iexpl

  if (iexpl and ptr_valid(zopts.levels)) then l0 = *(zopts.levels)  $
  else l0 = 0.d0

  mbase = widget_base(jb)

  pdefs.ids.zopts.cl_base[0] = widget_base(mbase, $
                                           /col, $
                                           map = ~iexpl)
  pdefs.ids.zopts.c_nlevels = cw_spin_box(pdefs.ids.zopts.cl_base[0], $
                                          /int, $
                                          track = optblock.track, $
                                          uvalue = 'NLEVEL', $
                                          value = zopts.n_levels, $
                                          xsize = 8, $
                                          /column, $
                                          label = '# Levels', $
                                          /capture, $
                                          /all_events, $
                                          min = 0, $
                                          /simple)

  maps = [{label: 'Linear'}, $
          {label: 'Logarithmic'}, $
          {label: 'Square Root'}]


  junk = widget_label(pdefs.ids.zopts.cl_base[0], $
                      value = 'Mapping:')
  pdefs.ids.zopts.c_map = $
     widget_droplist(pdefs.ids.zopts.cl_base[0], $
                     value = ['Linear', $
                              'Logarithmic', $
                              'Square Root'], $
                     uvalue = 'CL_MAP', $
                     track = optblock.track)
  widget_control, pdefs.ids.zopts.c_map, $
                  set_droplist_select = zopts.lmap

  pdefs.ids.zopts.cl_base[1] = widget_base(mbase, $
                                           /col, $
                                           map = iexpl)

  pdefs.ids.zopts.c_levels = cw_enter(pdefs.ids.zopts.cl_base[1], $
                                      /double, $
                                      /array, $
                                      track = optblock.track, $
                                      uvalue = 'LEVEL', $
                                      value = l0, $
                                      format = "(g11.4)", $
                                      xsize = 11, $
                                      ysize = 8, $
                                      /column, $ 
                                      label = 'Levels', $ 
                                      /capture, $ 
                                      /all_events)


  jbx = widget_base(obase, $
                    /column, $
                    xpad = 0, $
                    ypad = 0, $
                    space = 0)

  pdefs.ids.zopts.c_type = widget_droplist(jbx, $
                                           value = ['Outline', $
                                                    'Filled'], $
                                           uvalue = 'FILL', $
                                           track = optblock.track)
  widget_control, pdefs.ids.zopts.c_type, set_droplist_select = zopts.fill

  if ptr_valid(zopts.raw_colours) then $
     cont_cols =  gr_cont_col_set(*zopts.colours, $
                                  *zopts.raw_colours) $
  else cont_cols =  gr_cont_col_set(*zopts.colours)
  
  pdefs.ids.zopts.c_colour = cw_enter(jbx, $
                                      /text, $
                                      /array, $
                                      track = optblock.track, $
                                      uvalue = 'COLOUR', $
                                      /capture, $
                                      value = cont_cols, $
                                      xsize = 16, $
                                      ysize = 4, $
                                      /column, $
                                      label = 'Colours', $
                                      /all_events, $
                                      /ignore_empty)

  jby = widget_base(jbx, $
                    /row, $
                    xpad = 0, $
                    ypad = 0, $
                    space = 0)

  pdefs.ids.zopts.c_thick = cw_enter(jby, $
                                     /double, $ 
                                     format = "(f6.1)", $
                                     /array, $
                                     track = optblock.track, $
                                     uvalue = 'THICK', $
                                     /capture, $
                                     value = *(zopts.thick), $ 
                                     xsize = 8, $
                                     ysize = 4, $
;                                      /scroll, $
                                     /column, $
                                     label = 'Thicknesses', $
                                     /all_events)
  pdefs.ids.zopts.c_style = cw_enter(jby, $
                                     /int, $
                                     /array, $
                                     track = optblock.track, $
                                     uvalue = 'STYLE', $
                                     /capture, $
                                     value = *(zopts.style), $
                                     format = "(I0)", $
                                     xsize = 8, $
                                     ysize = 4, $
;                                      /scroll, $
                                     /column, $
                                     label = 'Styles', $
                                     /all_events)
  jby = widget_base(base, $
                    /row, $
                    xpad = 0, $
                    ypad = 0, $
                    space = 0)





  pdefs.ids.zopts.c_label = cw_spin_box(jby, $
                                        /int, $
                                        track = optblock.track, $
                                        uvalue = 'LABEL',  $
                                        value = zopts.label, $
                                        format = '(I0)', $
                                        xsize = 6, $
                                        label = 'Label', $
                                        /capture, $
                                        /column, $
                                        /all_events, $
                                        min = 0, $
                                        /simple)


  pdefs.ids.zopts.c_label_off = cw_spin_box(jby, $
                                            /int, $
                                            track = optblock.track, $
                                            uvalue = 'LABEL_OFF', $
                                            value = zopts.label_off, $
                                            format = "(i0)", $
                                            xsize = 6, $
                                            label = 'Offset', $
                                            /capture, $
                                            /column, $
                                            /all_events, $
                                            min = 0, $
                                            max = (zopts.label-1) > 1, $
                                            sensitive = zopts.label gt $
                                            1, $
                                            /simple)
                                            

  pdefs.ids.zopts.c_charsize = cw_spin_box(jby, $
                                           /double, $
                                           track = optblock.track, $
                                           uvalue = 'CCSIZE', $
                                           value = zopts.charsize, $
                                           format = "(F0.2)", $
                                           xsize = 6, $
                                           label = 'Charsize', $
                                           /capture, $
                                           /column, $
                                           /all_events, $
                                           step = 0.1, $
                                           min = 0., $
                                           sensitive = zopts.label ne $
                                           0, $
                                           /simple)

end

