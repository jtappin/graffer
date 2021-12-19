; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_OPT_SET
;	Set GRAFFER special options, e.g updating state etc.
;
; Usage:
;	gr_opt_set, pdefs
;
; Argument:
;	pdefs	struct	in/out	The ubiquitous GRAFFER structure.
;
; History:
;	Original: 4/7/97; SJT
;	Add mouse-editing default option: 13/8/97; SJT
;	Replace cw_bbselector with widget_droplist: 13/12/11; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;	Move top level options out of PDEFS: 21/5/20; SJT
;-

function Gr_opt_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.top, get_uvalue = opts

  iexit = 0
  sflag = 0b
  case but of
     'SH2D':  opts.s2d = event.index
     'TIME': opts.auto_delay = event.value > 10 ; Safety valve of 10
                                ; seconds.
     'MOUSE': opts.mouse = event.index

     'PDF':   begin
        opts.pdfviewer = event.str
        if event.index eq -1 then begin
           widget_control, event.id, get_value = pdfapps
           widget_control, event.id, set_value = [pdfapps, event.str]
        endif
     end
     'MPDF': begin
        spawn, /sh, 'which '+event.value, wh
        if strlen(wh[0]) ne 0 then $
           opts.pdfviewer = event.value $
        else junk = dialog_message(['Command: '+event.value,  $
                                    'not found'], $
                                   dialog_parent = event.top)
     end
     'DONT': iexit = -1
     'DO': iexit = 1
     'SAVE': sflag = 1b
     'SDO': begin
        sflag = 1b
        iexit = 1
     end
  endcase

  if sflag then begin
     home = getenv("HOME") 
     if strpos(home, path_sep(), /reverse_search) ne $
        strlen(home)-1 then home = home+path_sep()
     openw, ilu, /get, home+'.grafferrc'
     printf, ilu, 'Autosave: ', opts.auto_delay
     printf, ilu, 'Supp2D: ', opts.s2d
     printf, ilu, 'MouseEdit:', opts.mouse

     if opts.pdfviewer ne '' then printf, ilu, 'PDFView:', $
                                          opts.pdfviewer
     
; These are not settable in the GUI as they only apply at the creation
; of the top-level Graffer GUI.
     
     printf, ilu, 'Track:', opts.track
     printf, ilu, 'Bitmap:', opts.bitmaps
     
     free_lun, ilu
  endif

  widget_control, event.top, set_uvalue = opts

  return, {id:event.id, $
           top:event.top, $
           handler:0l, $
           exited:iexit}

end

pro Gr_opt_set, pdefs

  common gr_docs_common, pdfutil, docpath
  common graffer_options, optblock
  
  widget_control, pdefs.ids.graffer, sensitive = 0

  base = widget_base(resource = 'Graffer', title = 'Graffer Options', $
                     /column)

  junk = widget_label(base, value = 'Graffer special options')

  junk = widget_droplist(base, $
                         value = ['Display', 'Suppress'],  $
                         title = 'Show 2-D data?', $
                         uvalue = 'SH2D')
  widget_control, junk, set_droplist_select = optblock.s2d


  junk = widget_droplist(base, $
                         value = ['Disabled', 'Enabled'], $
                         title = 'Default mouse editing', $
                         uvalue = 'MOUSE')
  widget_control, junk, set_droplist_select = optblock.mouse

  junk = cw_enter(base, label = 'Autosave interval:', value = $
                  optblock.auto_delay, /float, /all_events, $
                  /capture, format = "(F6.1)", xsize = 7, uvalue = $
                  'TIME')
  jb = widget_base(base, $
                   /row)

  pdfapps = gr_find_viewer(/pdf, /all, count = napp)

  locs = where(pdfapps eq optblock.pdfviewer, npdf)
  if npdf eq 0 and optblock.pdfviewer ne '' then begin
     pdfapps = [pdfapps, optblock.pdfviewer]
     locs = n_elements(pdfapps)-1
     npdf = 1
  endif

  if napp eq 0 then begin
     junk = cw_enter(jb, $
                     /text, $
                     xsize = 12, $
                     /all_events, $
                     uvalue = 'MPDF', $
                     title = "PDF viewer")
  endif else begin
     junk = widget_label(jb, $
                         value = "PDF Viewer:")
     
     junk = widget_combobox(jb, $
                            value = pdfapps, $
                            uvalue = 'PDF', $
                            /editable)
     if npdf ne 0 then widget_control, junk, set_combobox_select = locs[0]
  endelse


  jb = widget_base(base, $
                   /row, $
                   /grid, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  junk = widget_button(jb, $
                       value = 'Apply', $
                       uvalue = 'DO')
  junk = widget_button(jb, $
                       value = 'Save', $
                       uvalue = 'SAVE')
  junk = widget_button(jb, $
                       value = 'Save && Apply', $
                       uvalue = 'SDO')
  junk = widget_button(jb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  widget_control, base, /real, event_fun = 'gr_opt_event', set_uvalue = $
                  optblock

  repeat begin
     ev = widget_event(base)
  end until (ev.exited ne 0)

  if (ev.exited eq 1) then begin
     widget_control, base, get_uvalue = opts, /no_copy
     optblock = opts
     if opts.pdfviewer ne '' then pdfutil = opts.pdfviewer
  endif

  widget_control, base, /destroy

  widget_control, pdefs.ids.graffer, sensitive = 1

  gr_plot_object, pdefs

end
