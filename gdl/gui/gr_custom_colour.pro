; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_cc_event, event

  widget_control, event.id, get_uvalue = mnu
  widget_control, event.handler, get_uvalue = uvs

  display = 0b
  reset_s = 0b

  exit = 0
  case mnu of
     'ACTION': exit = event.value
        
     'RED_E': begin
        (*uvs).colour[0] = event.value
        (*uvs).img[*, *, 0] = event.value
        widget_control, (*uvs).rs_id, set_value = event.value
        display = 1b
        reset_s = 1b
     end
     'RED_S': begin
        if ~event.drag then begin
           (*uvs).colour[0] = event.value
           reset_s = 1b
        endif
        (*uvs).img[*, *, 0] = event.value
        display = 1b
        widget_control, (*uvs).re_id, set_value = event.value
     end
     'GRN_E': begin
        (*uvs).colour[1] = event.value
        (*uvs).img[*, *, 1] = event.value
        widget_control, (*uvs).gs_id, set_value = event.value
        display = 1b
        reset_s = 1b
     end
     'GRN_S': begin
        if ~event.drag then begin
           (*uvs).colour[1] = event.value
           reset_s = 1b
        endif
        (*uvs).img[*, *, 1] = event.value
        display = 1b
        widget_control, (*uvs).ge_id, set_value = event.value
     end
     'BLU_E': begin
        (*uvs).colour[2] = event.value
        (*uvs).img[*, *, 2] = event.value
        widget_control, (*uvs).bs_id, set_value = event.value
        display = 1b
        reset_s = 1b
     end
     'BLU_S': begin
        if ~event.drag then begin
           (*uvs).colour[2] = event.value
           reset_s = 1b
        endif
        (*uvs).img[*, *, 2] = event.value
        display = 1b
        widget_control, (*uvs).be_id, set_value = event.value
     end
     'SCALE_E': begin
        sfact =  double(event.value)/100.
        widget_control, (*uvs).scs_id, $
                        set_value = fix(event.value) < 200
        for j =  0, 2 do begin
           (*uvs).colour[j] = byte(((*uvs).colour[j] * sfact) < 255)
           (*uvs).img[*, *, j] = (*uvs).colour[j]
        endfor
        widget_control, (*uvs).rs_id, set_value = (*uvs).colour[0]
        widget_control, (*uvs).gs_id, set_value = (*uvs).colour[1]
        widget_control, (*uvs).bs_id, set_value = (*uvs).colour[2]
        widget_control, (*uvs).re_id, set_value = (*uvs).colour[0]
        widget_control, (*uvs).ge_id, set_value = (*uvs).colour[1]
        widget_control, (*uvs).be_id, set_value = (*uvs).colour[2]
        reset_s = 1b
        display = 1b
     end
    'SCALE_S': begin
        sfact =  double(event.value)/100.
        widget_control, (*uvs).sce_id, set_value = float(event.value)
        ncv = bytarr(3)
        for j =  0, 2 do begin
           ncv[j] = byte(((*uvs).colour[j] * sfact) < 255)
           (*uvs).img[*, *, j] = ncv[j]
        endfor
        widget_control, (*uvs).rs_id, set_value = ncv[0]
        widget_control, (*uvs).gs_id, set_value = ncv[1]
        widget_control, (*uvs).bs_id, set_value = ncv[2]
        widget_control, (*uvs).re_id, set_value = ncv[0]
        widget_control, (*uvs).ge_id, set_value = ncv[1]
        widget_control, (*uvs).be_id, set_value = ncv[2]
        display = 1b
        if ~event.drag then begin
           (*uvs).colour = ncv
           reset_s = 1b
        endif
     end

  endcase

  if display then tv, (*uvs).img, true = 3

  if reset_s then begin
     widget_control, (*uvs).scs_id, set_value = 100
     widget_control, (*uvs).sce_id, set_value = 100.
     widget_control, (*uvs).sc_b, sensitive = max((*uvs).colour) gt 0
  endif

  return, {id: event.id, $
           top: event.top, $
           handler: event.handler, $
           exit: exit, $
           value: (*uvs).colour}

end

function gr_custom_colour, index, w0, group = group

;+
; GR_CUSTOM_COLOUR
;	Define a custom colour for a line or text.
;
; Usage:
;	c3 = gr_custom_colour(index)
;
; Returns:
;	A 3-element RGB colour array or -1 if cancel was chosen.
;
; Argument:
;	index	int/byt	Either a scalar current colour index or a
;			3-element byte array for a current custom
;			colour.
;	w0	long	Window ID of the main plotting window.
;
; History:
;	Original: 17/5/16; SJT
;	Add scaler: 23/8/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;-

  if n_elements(index) eq 1 then begin
     lcolour = graff_colours(index)
     bcolour = bytarr(3)
     bcolour[0] = lcolour mod 256l
     bcolour[1] = (lcolour / 256l) mod 256l
     bcolour[2] = lcolour / 256l^2
  endif else bcolour = index

  if keyword_set(group) then widget_control, group, sensitive = 0
  
  tlb = widget_base(title = "Graffer: Custom colour", $
                    group = group, $
                    resource = 'Graffer', $
                    /column)

  base = widget_base(tlb, $
                     /column)


  junk = widget_label(base, $
                      value = "Graffer define custom colour")

  base1 = widget_base(base, $
                      /column, $
                      /base_align_center)

  jb = widget_base(base1, $
                   /row)

  jbb = widget_base(jb, $
                    /column, $
                    /base_align_center)

  re_id = cw_spin_box(jbb, $
                      label = 'Red', $
                      /column, $
                      format = "(I3)", $
                      /capture, $
                      xsize = 4, $
                      /int, $
                      value = bcolour[0], $
                      uvalue = 'RED_E', $
                      min = 0, $
                      max = 255, $
                      /simple)
  rs_id = widget_slider(jbb, $
                        /vertical, $
                        min = 0, $
                        max = 255, $
                        ysize = 300, $
                        value = bcolour[0], $
                        /suppress, $
                        /drag, $
                        uvalue = 'RED_S')

  jbb = widget_base(jb, $
                    /column, $
                    /base_align_center)

  ge_id = cw_spin_box(jbb, $
                      label = 'Green', $
                      /column, $
                      format = "(I3)", $
                      /capture, $
                      xsize = 4, $
                      /int, $
                      value = bcolour[1], $
                      uvalue = 'GRN_E', $
                      min = 0, $
                      max = 255, $
                      /simple)
  gs_id = widget_slider(jbb, $
                        /vertical, $
                        min = 0, $
                        max = 255, $
                        ysize = 300, $
                        value = bcolour[1], $
                        /suppress, $
                        /drag, $
                        uvalue = 'GRN_S')


  jbb = widget_base(jb, $
                    /column, $
                    /base_align_center)

  be_id = cw_spin_box(jbb, $
                      label = 'Blue', $
                      /column, $
                      format = "(I3)", $
                      /capture, $
                      xsize = 4, $
                      /int, $
                      value = bcolour[2], $
                      uvalue = 'BLU_E', $
                      min = 0, $
                      max = 255, $
                      /simple)
  bs_id = widget_slider(jbb, $
                        /vertical, $
                        min = 0, $
                        max = 255, $
                        ysize = 300, $
                        value = bcolour[2], $
                        /suppress, $
                        /drag, $
                        uvalue = 'BLU_S')

  sc_b = widget_base(jb, $
                     /column, $
                     /base_align_center, $
                     /frame)
  widget_control, sc_b, sensitive = max(bcolour) gt 0

  sce_id = cw_enter(sc_b, $
                       label = 'Scale (%)', $
                       /column, $
                       format = "(f0.1)", $
                       /capture, $
                       xsize = 5, $
                       /float, $
                       value = 100., $
                       uvalue = 'SCALE_E')

  scs_id = widget_slider(sc_b, $
                         /vertical, $
                         min = 0, $
                         max = 200, $
                         ysize = 300, $
                         value = 100, $
                         /suppress, $
                         /drag, $
                         uvalue = 'SCALE_S')


  dr_id = widget_draw(base1, $
                      xsize = 150, $
                      ysize = 20)

  junk = cw_bgroup(base, $
                   ['Apply', 'Cancel'], $
                   /row, $
                   uvalue = 'ACTION', $
                   button_uvalue = [1, -1])

  img = bytarr(150, 20, 3)
  for j = 0, 2 do img[*, *, j] = bcolour[j]

  uvs = ptr_new({re_id: re_id, $
                 rs_id: rs_id, $
                 ge_id: ge_id, $
                 gs_id: gs_id, $
                 be_id: be_id, $
                 bs_id: bs_id, $
                 sc_b: sc_b, $
                 sce_id: sce_id, $
                 scs_id: scs_id, $
                 colour: bcolour, $
                 img: img})

  widget_control, tlb, /real
  widget_control, dr_id, get_value = win
  wset, win
  tv, img, true = 3

  widget_control, base, set_uvalue = uvs, $
                  event_func = 'gr_cc_event'
  

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exit ne 0)

  if keyword_set(group) then widget_control, group, /sensitive

  wset, w0

  widget_control, tlb, /destroy

  if ev.exit eq -1 then return, -1
  return, ev.value

end
