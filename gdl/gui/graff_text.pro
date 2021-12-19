; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFF_TEXT
;	Add text to a graffer plot.
;
; Usage:
;	ichange = graff_text(pdefs)
;
; Return value
;	ichange	int	1 if changed, 0 if not
;
; Argument:
;	pdefs	struct	in/out	Structure containing all the info
;				about the plot.
;
; History:
;	Original: 16/8/95; SJT
;	Add timer event to push to front if obscured: 23/8/95; SJT
;	Add a tracker help window like that in the main panel: 9/8/96;
;	SJT
;	Add a cancel button: 13/9/96; SJT
;	Modify to display the text in a window on the pop-up (for
;	speed of response with complex plots): 6/11/96; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Add CAPTURE key to text inputs: 6/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Fix widget ID for colour selector: 26/1/12; SJT
;	Add extra colours: 8/2/12; SJT
;	Use cw_spin_box for sizes and thicknesses: 29/9/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

pro Gr_text_disp, text

  erase

  opf = !P.font
  !P.font = text.ffamily
  tf = '!'+string(text.font, format = "(I0)")+ $
       text.text+'!3'

  if text.colour eq -2 then $
     lcolour = graff_colours(text.c_vals) $
  else lcolour = graff_colours(text.colour)

  xyouts, /norm, .05+.9*text.align, .1, tf, color = lcolour, $
          charsize = text.size, charthick = text.thick, align = text.align

  !P.font = opf

  usersym, [-.5, 0., 0., 0., .5], [-.866, 0., -1.77, 0., -.866]
  plots, /norm, psym = 8, .05+.9*text.align, .1

end

function Grt_event, event

widget_control, event.id, get_uvalue = but
widget_control, event.handler, get_uvalue = pdefs, /no_copy

iexit = 0
idraw = 1

track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
if (track_flag) then begin
    idraw = 0
    if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.popmsg, ''
        goto, miss_case
    endif
endif

    
itxt = pdefs.transient.imove

case but of
    'DONE': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Text operation finished' $
    else begin
        iexit = 1
        idraw = 0b
    end
    
    'CANCEL': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Cancel text operation' $
    else begin
        iexit = -1
        idraw = 0b
    end
    
    'UPDATE': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Update screen' $
    else begin
        wset, pdefs.ids.windex
        gr_plot_object, pdefs
        wset, pdefs.ids.txwindex
    endelse
    
    'TEXT': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Enter the text string' $
    else (*pdefs.text)[itxt].text = event.value
    
    'ID': if (track_flag) then $
       graff_msg, pdefs.ids.popmsg, "Enter an identifying tag for the " + $
                  "annotation" $
    else (*pdefs.text)[itxt].id = event.value

    'CHS': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Set the character size' $
    else (*pdefs.text)[itxt].size = event.value

    'COL': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select the text colour' $
    else begin
       if event.value eq graff_colours(/max_index)+1 then begin
          ci = (*pdefs.text)[itxt].colour
          if ci eq -2 then $
             cc = gr_custom_colour((*pdefs.text)[itxt].c_vals, $
                                   pdefs.ids.windex) $
          else cc = gr_custom_colour(ci > 0, $
                                   pdefs.ids.windex)
          if n_elements(cc) eq 1 then begin
             if ci ne -2 then $
                widget_control, event.id, set_value = ci
          endif else begin
             (*pdefs.text)[itxt].colour = -2
             (*pdefs.text)[itxt].c_vals = cc
          endelse
       endif else (*pdefs.text)[itxt].colour = event.value
    endelse

    'JUST': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select text justification' $
    else if (event.index le 2) then $
      (*pdefs.text)[itxt].align = double(event.index)/2. $
    else (*pdefs.text)[itxt].align = $
      graff_just((*pdefs.text)[itxt].align, group = event.top) 
    
    'ORI': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Set orientation (Degrees anti clockwise)' $
    else (*pdefs.text)[itxt].orient = event.value
    'THI': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select line width to draw text' $
    else (*pdefs.text)[itxt].thick = event.value
    
    'TX': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Adjust X position of anchor' $
    else (*pdefs.text)[itxt].x = event.value
    'TY': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Adjust Y position of anchor' $
    else (*pdefs.text)[itxt].y = event.value
    
    'TNORM': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select data or viewport (normal) ' + $
      'coordinates' $
    else begin
        jb = widget_info(event.id, /parent)
        widget_control, jb, get_uvalue = poss
        cn = (*pdefs.text)[itxt].norm
        
        (*pdefs.text)[itxt].norm = event.index

        gr_coord_convert, (*pdefs.text)[itxt].x, $
          (*pdefs.text)[itxt].y, xt, yt, $
          data = cn eq 0, region = cn eq 1, frame = cn eq 2, $
          to_data = event.index eq 0, to_region = event.index eq 1, $
          to_frame = event.index eq 2
         
        (*pdefs.text)[itxt].x = xt
        (*pdefs.text)[itxt].y = yt
        widget_control, poss(0), set_value = xt
        widget_control, poss(1), set_value = yt
        axd = widget_info(event.id, /sibling)
        if widget_info(/valid, axd) then widget_control, axd, $
          sensitive = event.index eq 0
    end

    'AXIS': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select which to Y-axis to refer ' + $
      'the data coordinates' $ 

    else begin
        jb = widget_info(event.id, /parent)
        widget_control, jb, get_uvalue = poss
                                ; Convert to normalized coordinates.
        gr_coord_convert, (*pdefs.text)[itxt].x, $
          (*pdefs.text)[itxt].y, xt, yt, /data, /to_norm

                                ; Update the Y-transform
        !y = pdefs.ytransform[event.index]

                                ;  Convert back to new data coords
        gr_coord_convert, xt, yt, xtt, ytt, /norm, /to_data

        (*pdefs.text)[itxt].x = xtt
        (*pdefs.text)[itxt].y = ytt
        widget_control, poss(0), set_value = xtt
        widget_control, poss(1), set_value = ytt
    endelse

    'FAMILY': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select font family' $
    else begin
        (*pdefs.text)[itxt].ffamily = event.index-1
        gr_font_list, (*pdefs.text)[itxt].ffamily, flist, fnos
        locs = where((*pdefs.text)[itxt].font eq fnos, nf)
        if nf eq 0 then begin
            idx = 0
            (*pdefs.text)[itxt].font = fnos[0]
        endif else idx = locs[0]
        widget_control, pdefs.ids.fontmenu, set_value = flist, $
          set_droplist_select = idx
    endelse

    'FONT': if (track_flag) then  $
      graff_msg, pdefs.ids.popmsg, 'Select text font' $
    else begin
        gr_font_list, (*pdefs.text)[itxt].ffamily, flist, fnos
        (*pdefs.text)[itxt].font = fnos[event.index]
    endelse
endcase

if (idraw) then gr_text_disp, (*pdefs.text)[itxt]

Miss_case:

widget_control, event.handler, set_uvalue = pdefs, /no_copy

return, {id:event.id, $
         top:event.top, $
         handler:event.handler, $
         exited:iexit}

end

function Graff_text, pdefs, edit = edit, position = position

  common Gr_psym_maps, psym_bm, col_bm
  common graffer_options, optblock

  if optblock.bitmaps && n_elements(col_bm) eq 0 then gr_psym_bitm
  
  opy = !y
  if (pdefs.y_right) then $
     yaxis = (*pdefs.data)[pdefs.cset].y_axis $
  else yaxis = 0
  !y = pdefs.ytransform[yaxis]

  old_text = (*pdefs.text)      ; Save current settings
  old_ntext = pdefs.ntext       ; in case of a cancel

  if (n_elements(edit) ne 0) then itxt = edit $
  else begin
     if (pdefs.ntext ne 0) then (*pdefs.text) = [(*pdefs.text), $
                                                 pdefs.text_options] $
     else (*pdefs.text) = pdefs.text_options
     itxt = pdefs.ntext
     (*pdefs.text)[itxt].x = position(0)
     (*pdefs.text)[itxt].y = position(1)
     (*pdefs.text)[itxt].axis = yaxis
     pdefs.ntext = pdefs.ntext+1
     if (*pdefs.text)[itxt].font eq 0 then (*pdefs.text)[itxt].font = 3
  endelse
  pdefs.transient.imove = itxt

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer text', group_leader = $
                    pdefs.ids.graffer, resource = 'Graffer')
  base = widget_base(tlb, /column)

                                ; The actual text

  showit = widget_draw(base, ysize = 25, xsize = 320, /frame)

  junk = cw_enter(base, $
                  /all_ev, $
                  xsize = 40, $
                  value = (*pdefs.text)[itxt].text, $ 
                  uvalue = 'TEXT', $
                  label = 'Text:', $
                  track = optblock.track, $
                  /capture, $
                  /graphics)

                                ; ID tag, Size & colour

  jb = widget_base(base, /row)

  junk = cw_enter(jb, $
                  /text, $
                  /all_events, $
                  xsize = 15, $
                  value = (*pdefs.text)[itxt].id, $
                  uvalue = 'ID', $
                  label = 'ID:', $
                  track = optblock.track, $
                  /capture)

  junk = cw_spin_box(jb, $
                     /double, $
                     /all_ev, $
                     xsize = 7, $
                     value = (*pdefs.text)[itxt].size, $
                     format = "(f0.2)", $
                     uvalue = 'CHS', $
                     label = 'Charsize:', $
                     track = optblock.track, $
                     /capture, $
                     min = 0., $
                     step = 0.1, $
                     /simple)


  jb = widget_base(base, /row)

  if optblock.bitmaps then begin
     maxi = graff_colours(/max)
     col_str = replicate({bitmap: ptr_new()}, maxi+2)
     col_str[-1].bitmap = ptr_new(col_bm[*, *, 1])
     for j = 0, maxi do col_str[j].bitmap = $
        ptr_new(gr_mk_colour_bm(j, size = [80, 16]))
  endif else begin
     col_list = ['White (bg)', $
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
     maxi = n_elements(col_str)-2
  endelse

  junk = widget_label(jb, $
                      value = 'Colour:')
  
  if (*pdefs.text)[itxt].colour eq -2 then ci = maxi+1 $
  else ci = (*pdefs.text)[itxt].colour
  junk = cw_pdmenu_plus(jb, $
                        col_str, $
                        uvalue = 'COL', $
                        /selector, $
                        track = optblock.track, $
                        initial = ci)

  junk = cw_spin_box(jb, $
                     /double, $
                     format = "(f0.1)", $
                     /all_ev, $
                     xsize = 6, $
                     value = (*pdefs.text)[itxt].thick, $
                     uvalue = 'THI', $
                     label = 'Thickness:', $
                     track = optblock.track, $
                     /capture, $
                     min = 0., $
                     step = 1., $
                     /simple)

                                ; Alignment and orientation

  case ((*pdefs.text)[itxt].align) of
     0.: ial = 0
     0.5: ial = 1
     1.0: ial = 2
     Else: ial = 3
  end

  jb = widget_base(base, /row)
  junk = widget_droplist(jb, $
                         value = ['Left', 'Centre', $
                                  'Right', 'Other ...'], $
                         uvalue = 'JUST', $
                         title = 'Justification:', $
                         track = optblock.track)
  widget_control, junk, set_droplist_select = ial

  junk = cw_enter(jb, $
                  /double, $
                  /all_event, $
                  xsize = 6, $
                  uvalue = 'ORI', $
                  value = (*pdefs.text)[itxt].orient, $
                  format = "(f6.1)",  $
                  label = 'Orientation (°):', $
                  track = optblock.track, $
                  /capture)

                                ; Position


  cdbase = widget_base(base, $
                       /row)

  junk = widget_droplist(cdbase, $
                         value = ['Data', 'Normal', '"Frame"'], $
                         uvalue = 'TNORM', $
                         title = 'Coordinates:', $
                         track = optblock.track)
  widget_control, junk, set_droplist_select = (*pdefs.text)[itxt].norm

  if (pdefs.y_right ne 0) then begin
     junk = widget_droplist(cdbase, $
                            value = ["Main", "Secondary"], $
                            uvalue = "AXIS", $
                            title = "Y-axis:", $
                            track = optblock.track, $
                            sensitive = (*pdefs.text)[itxt].norm eq 0)
     
     widget_control, junk, set_droplist_select = (*pdefs.text)[itxt].axis
  endif

  pb = widget_base(base, /row)

  xpos = cw_enter(pb, $
                  /double, $
                  /all_event, $
                  xsize = 15, $
                  uvalue = 'TX', $
                  value = (*pdefs.text)[itxt].x, $
                  format = "(g15.8)", $
                  label = 'Position:  X:', $
                  track = optblock.track, $
                  /capture) 
  ypos = cw_enter(pb, $
                  /double, $
                  /all_event, $
                  xsize = 15, $
                  uvalue = 'TY', $
                  value = (*pdefs.text)[itxt].y, $
                  format = "(g15.8)", $
                  label = 'Y:', $
                  track = optblock.track, $
                  /capture)

  widget_control, cdbase, set_uvalue = [xpos, ypos]

                                ; Fonts

  gr_font_list, (*pdefs.text)[itxt].ffamily, font_list, fnos
  locs = where((*pdefs.text)[itxt].font eq fnos, count)
  if count eq 0 then begin
     idx = 0
     (*pdefs.text)[itxt].font = fnos[0]
  endif else idx = locs[0]

  jb = widget_base(base, $
                   /row)
  junk = widget_droplist(jb, $
                         value = ['Hershey', $
                                  'Device', $
                                  'TrueType'], $
                         uvalue = 'FAMILY', $
                         title = 'Font:', $
                         track = optblock.track)  
  widget_control, junk, set_droplist_select = $
                  (*pdefs.text)[itxt].ffamily+1

  pdefs.ids.fontmenu = widget_droplist(jb, $
                                       value = font_list, $
                                       uvalue = 'FONT', $
                                       title = ':', $
                                       track = optblock.track)  
  widget_control, pdefs.ids.fontmenu, set_droplist_select = idx

                                ; Messages (what does what)

  pdefs.ids.popmsg = widget_text(base, value = '', xsize = 45)

                                ; Done

  jb = widget_base(base, $
                   /row, $
                   /grid)
  junk = widget_button(jb, $
                       value = ' Apply  ', $
                       uvalue = 'DONE', $
                       track = optblock.track)
  junk = widget_button(jb, $
                       value = ' Update ', $
                       uvalue = 'UPDATE', $
                       track = optblock.track)
  junk = widget_button(jb, $
                       value = ' Cancel ', $
                       uvalue = 'CANCEL', $
                       track = optblock.track)

  txtemp = (*pdefs.text)[itxt]


  widget_control, tlb, /real

  widget_control, showit, get_value = shno
  wset, shno
  pdefs.ids.txwindex = shno

  widget_control, base, set_uvalue = pdefs, /no_copy
  gr_text_disp, txtemp

;	RYO widget management to allow us to get the values back from
;	the event handler without using a common block, even after the
;	hierarchy has been destroyed.

  widget_control, base, event_func = 'grt_event'

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = pdefs, /no_copy

  wset, pdefs.ids.windex

  widget_control, tlb, /destroy
  if (ev.exited eq -1) then begin
     *pdefs.text = old_text
     pdefs.ntext = old_ntext
     ichange = 0
  endif else begin
     pdefs.text_options = (*pdefs.text)[itxt]
     pdefs.text_options.text = ''
     pdefs.text_options.id = ''
     ichange = 1
  endelse

  !y = opy

  gr_plot_object, pdefs

  widget_control, pdefs.ids.graffer, sensitive = 1

  return, ichange

end
