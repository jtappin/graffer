; LICENCE:
; Copyright (C) 1995-2022: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_HARDOPTS
;	Set hardcopy options
;
; Usage
;	istat = gr_hardopts(pdefs)	; Not intended for direct user usage
;
; Returns:
;	-1 for cancelled, 1 for options changed, 0 for options
;	unchanged.
;
; Arguments:
;	pdefs	struct	in/out	The graffer data structure.
;	
; History:
;	Original: 3/8/95; SJT
;	Add timer event to push to front if obscured: 23/8/95; SJT
;	Renamed as GR_HARDOPTS (was hardopts): 18/9/96; SJT
;	Made into a function: Unknown: SJT
;	Add font selection options: 10/10/96; SJT
;	Replace most cw_bbselectors with widget_droplist: 13/12/11; SJT
;	Allow selection of non-standard filename: 13/2/12; SJT
;	Allow PDF generation: 21/9/16; SJT
;	Replace last bbselector with cw_pdmenu_plus: 28/9/16; SJT
;	Replace graff_enter with cw_enter: 13/10/16; SJT
;	Detect if options changed: 30/11/16; SJT
;-


function Hopts_event, event

  widget_control, event.id, get_uvalue = but
  widget_control, event.handler, get_uvalue = settings, /no_copy

  iexit = 0

  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1

  if (track_flag) then begin
     if (event.enter eq 0) then begin
        graff_msg, settings.action, /help, ''
        goto, miss_case
     endif
  endif

  case but of
     'DO': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Save the settings & make the hardcopy' $
     else begin
        iexit = 1
        if settings.chflag then begin
           for j = 0, 1 do begin
              widget_control, settings.cmid[j, 0], get_value = cmd
              settings.opts.action[j] = cmd[0]
              widget_control, settings.cmid[j, 1], get_value = cmd
              settings.opts.viewer[j] = cmd[0]
              widget_control, settings.cmid[j, 2], get_value = cmd
              settings.opts.pdfviewer[j] = cmd[0]
           endfor
           widget_control, settings.fileid, get_value = file
           settings.opts.name = file
           for j = 0, 2 do settings.opts.prompt[j] = $
              widget_info(settings.promptid[j], /button_set)
        endif
     endelse

     'CANCEL': if (track_flag) then $
        graff_msg, settings.action, /help, "Forget new settings &" + $
                   " don't make a hardcopy" $
     else iexit = -1
     
     'COL': if (track_flag) then $
        graff_msg, settings.action, /help, 'Toggle use of colour PostScript' $
     else begin
        settings.chflag or=  (settings.opts.colour ne event.index)
        settings.opts.colour = event.index
        widget_control, settings.modid, sensitive = event.index
     endelse

     'CMYK': if (track_flag) then $
        graff_msg, settings.action, /help, 'Select RGB or CMYK colour model' $
     else begin
        settings.chflag or= (settings.opts.cmyk ne event.index)
        settings.opts.cmyk = event.index
     endelse

     
     'ORI': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Toggle landscape/portrait orientation' $
     else begin
        settings.chflag or= (settings.opts.orient ne event.index)
        settings.opts.orient = event.index
        settings.psize = gr_get_page(settings.opts.psize, $
                                     settings.opts.orient)
        
                                ; Swap over the X & Y sizes of the
                                ; draw page if it overlaps the edge of
                                ; the paper but not otherwise.
        
        if ((event.index eq 1 and $
             settings.opts.size(0) gt settings.psize(0)) or $
            (event.index eq 0 and $
             settings.opts.size(1) gt settings.psize(1))) then begin
           temp = settings.opts.size(0)
           settings.opts.size(0) = settings.opts.size(1)
           settings.opts.size(1) = temp
           widget_control, settings.xsid, set_value = settings.opts.size(0)
           widget_control, settings.ysid, set_value = settings.opts.size(1)
        endif
        
                                ; re-centre
        
        xs = (settings.psize(0)-settings.opts.size(0))/2.
        settings.opts.off(0) = xs
        ys = (settings.psize(1)-settings.opts.size(1))/2.
        settings.opts.off(1) = ys
        
        widget_control, settings.xoffid, set_value = xs
        widget_control, settings.xleftid, set_value = xs
        widget_control, settings.yoffid, set_value = ys
        widget_control, settings.yleftid, set_value = ys
     end
     
     'PSIZE': if (track_flag) then $
        graff_msg, settings.action, /help, 'Toggle A4/Letter sized paper' $
     else begin
        settings.chflag or= (settings.opts.psize ne event.index)
        settings.opts.psize = event.index
        settings.psize = gr_get_page(settings.opts.psize, $
                                     settings.opts.orient)
        xlft = settings.psize(0)-settings.opts.off(0)- $
               settings.opts.size(0)
        widget_control, settings.xleftid, set_value = xlft
        ylft = settings.psize(1)-settings.opts.off(1)- $
               settings.opts.size(1)
        widget_control, settings.yleftid, set_value = ylft
     end
     
     'CENTRE': if (track_flag) then $
        graff_msg, settings.action, /help, 'Centre the plot on the page' $
     else begin
        settings.chflag = 1
        xs = (settings.psize(0)-settings.opts.size(0))/2.
        settings.opts.off(0) = xs
        widget_control, settings.xleftid, set_value = xs
        widget_control, settings.xoffid, set_value = xs
        
        ys = (settings.psize(1)-settings.opts.size(1))/2.
        settings.opts.off(1) = ys
        widget_control, settings.yleftid, set_value = ys
        widget_control, settings.yoffid, set_value = ys
     end
     
     'XSI': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Set the X size of the plot (in cm)' $
     else begin
        settings.chflag = 1
        widget_control, event.id, get_value = sx
        settings.opts.size(0) = sx
        xlft = settings.psize(0)-sx-settings.opts.off(0)
        widget_control, settings.xleftid, set_value = xlft
     end
     'YSI': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Set the Y size of the plot (in cm)' $
     else begin
        settings.chflag = 1
        widget_control, event.id, get_value = sy
        settings.opts.size(1) = sy
        ylft = settings.psize(1)-sy-settings.opts.off(1)
        widget_control, settings.yleftid, set_value = ylft
     end
     'XOFF': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Set the X offset of the plot (in cm)' $
     else begin
        settings.chflag = 1
        widget_control, event.id, get_value = sx
        settings.opts.off(0) = sx
        xlft = settings.psize(0)-sx-settings.opts.size(0)
        widget_control, settings.xleftid, set_value = xlft
     end
     'YOFF': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Set the Y offset of the plot (in cm)' $
     else begin
        settings.chflag = 1
        widget_control, event.id, get_value = sy
        settings.opts.off(1) = sy
        ylft = settings.psize(1)-sy-settings.opts.size(1)
        widget_control, settings.yleftid, set_value = ylft
     end
     
     'FFAMILY': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Select the font family for plot annotation' $
     else begin
        settings.chflag or= (settings.opts.font.family ne event.index)
        settings.opts.font.family = event.index
        widget_control, settings.wsid, sensitive = event.index le 9
        for j = 1, 3, 2 do widget_control, settings.wsids(j), $
                                           sensitive = $
                                           event.index le 5
     end
     'FWS': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Select the weight & slant for plot annotation' $
     else begin
        settings.chflag or= (settings.opts.font.wg_sl ne event.value)
        settings.opts.font.wg_sl = event.value
     endelse
     
     'TIMEST': if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Toggle printing of a timestamp on the plot' $
     else begin
        settings.chflag or= (settings.opts.timestamp ne event.index)
        settings.opts.timestamp = event.index
     endelse
     
     'FILE': if track_flag then $
        graff_msg, settings.action, /help, 'Set the output file name' $
     else begin
        settings.chflag = 1
     endelse

     'PFILE': if track_flag then $
        graff_msg, settings.action, /help, 'Pick the output file name' $
     else begin
 
        filt = '*.ps;*.eps;*.pdf'

        file = dialog_pickfile(title = "Plot output file", $
                               /write, $
                               /overwrite_prompt, $
                               dialog_parent = event.top, $
                               filter = filt)
        if file ne '' then begin
           dname = file_dirname(file)
           cd, current = cdir
           if cd eq dname then file = file_basename(file)
           
           widget_control, settings.fileid, set_value = file
           settings.chflag = 1
        endif
     endelse

     'DFILE': if track_flag then $
        graff_msg, settings.action, /help, $
                   'Reset the output file name to the default' $ 
     else begin
        widget_control, settings.fileid, set_value = settings.tname
        settings.chflag = 1
     endelse
     
     'PSCMD':if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Enter the command for spooling a PS plot file' $ 
     else settings.chflag = 1
     'EPCMD':if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Enter the command for viewing an EPS plot file' $ 
     else settings.chflag = 1
     'PDCMD':if (track_flag) then $
        graff_msg, settings.action, /help, $
                   'Enter the command for viewing a PDF plot file' $ 
     else settings.chflag = 1

     'PSSFX':if (track_flag) then $
        graff_msg, settings.action, /help, 'Enter the part of the ' + $
                   'command for spooling a PS plot that comes after ' + $
                   'the filename' $ 
     else settings.chflag = 1
     'EPSFX':if (track_flag) then $
        graff_msg, settings.action, /help, 'Enter the part of the ' + $
                   'command for viewing a EPS plot that comes after ' + $
                   'the filename' $ 
     else settings.chflag = 1
     'PDSFX':if (track_flag) then $
        graff_msg, settings.action, /help, 'Enter the part of the ' + $
                   'command for viewing a PDF plot that comes after ' + $
                   'the filename' $ 
     else settings.chflag = 1
    
     'PSFVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to the default PS ' + $
                   'viewer' $
     else begin
        widget_control, settings.cmid[0, 0], set_value = 'lp'
        widget_control, settings.cmid[1, 0], set_value = ''
        settings.chflag = 1
     endelse
     'EPFVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to the default EPS ' + $
                   'viewer' $
     else begin
        widget_control, settings.cmid[0, 1], set_value = $
                        gr_find_viewer(/ps)
        widget_control, settings.cmid[1, 1], set_value = ''
        settings.chflag = 1
     endelse
     'PDFVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to the default PDF ' + $
                   'viewer' $
     else begin
        widget_control, settings.cmid[0, 2], set_value = $
                        gr_find_viewer(/pdf)
        widget_control, settings.cmid[1, 2], set_value = ''
        settings.chflag = 1
     endelse

     'PSNOVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to "no viewer"' $
     else begin
        widget_control, settings.cmid[0, 0], set_value = ''
        widget_control, settings.cmid[1, 0], set_value = ''
        settings.chflag = 1
     endelse
     'EPNOVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to "no viewer"' $
     else begin
        widget_control, settings.cmid[0, 1], set_value = ''
        widget_control, settings.cmid[1, 1], set_value = ''
        settings.chflag = 1
     endelse
     'PDNOVIEW': if (track_flag) then $
        graff_msg, settings.action, /help, 'Set viewer to "no viewer"' $
     else begin
        widget_control, settings.cmid[0, 2], set_value = ''
        widget_control, settings.cmid[1, 2], set_value = ''
        settings.chflag = 1
     endelse

     'PSPROMPT': if track_flag then $
        graff_msg, settings.action, /help, $
                   'Choose whether to prompt before action' $
     else settings.chflag = 1b
     'EPPROMPT': if track_flag then $
        graff_msg, settings.action, /help, $
                   'Choose whether to prompt before action' $
     else settings.chflag = 1b
     'PDFPROMPT': if track_flag then $
        graff_msg, settings.action, /help, $
                   'Choose whether to prompt before action' $
     else settings.chflag = 1b

     Else:     graff_msg, settings.action, "Whaat??????"       
  endcase

Miss_case:

  widget_control, event.handler, set_uvalue = settings, /no_copy

  return, {id:event.id, $
           top:event.top, $
           handler:event.handler, $
           exited:iexit}

end

function Gr_hardopts, pdefs

  common graffer_options, optblock

  h = pdefs.hardset

  tname = pdefs.name
  dp = strpos(tname, '.', /reverse_search)
  if dp ne -1 then tname = strmid(tname, 0, dp) 
  tname = pdefs.dir+tname

  uvs = { $
        Opts:h, $
        Cmid:lonarr(2, 3), $
        promptid: lonarr(3), $
        Wsid:0l, $
        Wsids:lonarr(4), $
        ctrid: 0l, $
        Xsid:0l, $
        Xoffid:0l, $
        Xleftid:0l, $
        Ysid:0l, $
        Yoffid:0l, $
        Yleftid:0l, $
        fileid: 0l, $
        modid: 0l, $
        paperid: 0l, $
        Action: 0l, $
        Psize: dblarr(2), $
        tname: tname, $
        chflag: 0 $
        }
  uvs.psize = gr_get_page(h.psize, h.orient)

  widget_control, pdefs.ids.graffer, sensitive = 0

  tlb = widget_base(title = 'Graffer Hard Copy', $
                    group_leader = pdefs.ids.graffer, $
                    resource = 'Graffer')
  base = widget_base(tlb, /column)

                                ; Basic toggle settings

  jb = widget_base(base, /row)

  junk = widget_droplist(jb, $
                         value = ['Monochrome', 'Colour'], $
                         uvalue = 'COL', $
                         track = optblock.track)
  widget_control, junk, set_droplist_select = h.colour

  junk = widget_droplist(jb, $
                         value = ['Landscape', 'Portrait'], $
                         uvalue = 'ORI', $
                         track = optblock.track)
  widget_control, junk, set_droplist_select = h.orient

  if ~is_gdl() then begin
     uvs.modid = widget_droplist(jb, $
                                 value = ['RGB', 'CMYK'], $
                                 uvalue = 'CMYK', $
                                 track = optblock.track)
     widget_control, uvs.modid, set_droplist_select = h.cmyk

     widget_control, uvs.modid, sensitive = h.colour
  endif else uvs.modid = 0l
  
                                ; Page size
  cl = widget_base(base, $
                   column = 3, $
                   /grid, $
                   /base_align_right)

  uvs.paperid = widget_droplist(cl, $
                                value = ['A4', 'Letter'], $
                                title = 'Paper size:', $
                                uvalue = 'PSIZE', $
                                track = optblock.track)
  widget_control, uvs.paperid, set_droplist_select = h.psize

  uvs.ctrid = widget_button(cl, $
                            value = 'Centre on page', $
                            uvalue = 'CENTRE', $
                            track = optblock.track)
  junk = widget_droplist(cl, $
                         value = ['Off', 'On'], $
                         title = "Plot timestamp", $
                         uvalue = 'TIMEST', $
                         track = optblock.track)
  widget_control, junk, set_droplist_select = h.timestamp

  uvs.xsid = cw_spin_box(cl, $
                         /double, $
                         /all, $
                         label = 'X Size (cm):', $
                         value = h.size[0], $
                         uvalue = 'XSI', $
                         format = "(F5.2)", $
                         xsize = 5, $
                         track = optblock.track, $
                         /capture, $
                         min = 0., $
                         step = 0.5, $
                         /simple)
  uvs.xoffid = cw_spin_box(cl, $
                           /double, $
                           /all, $
                           label = 'X offset:',  $
                           value = h.off[0], $
                           uvalue = 'XOFF', $
                           format = "(F5.2)", $
                           xsize = 5, $
                           track = optblock.track, $
                           /capture, $
                           min = 0., $
                           step = 0.5, $
                           /simple)
  uvs.xleftid = cw_enter(cl, $
                         /double, $
                         /display, $
                         label = 'X remain:', $
                         value = uvs.psize[0]-h.size[0]-h.off[0], $
                         format = "(F5.2)", $
                         xsize = 5)

;  jb = widget_base(cl, /column)
  uvs.ysid = cw_spin_box(cl, $
                         /double, $
                         /all, $
                         label = 'Y Size (cm):', $
                         value  = h.size[1], $
                         uvalue = 'YSI', $
                         format = "(F5.2)", $
                         xsize = 5, $
                         track = optblock.track, $
                         /capture, $
                         min = 0., $
                         step = 0.5, $
                         /simple)
  uvs.yoffid = cw_spin_box(cl, $
                           /double, $
                           /all, $
                           label = 'Y offset:',  $
                           value = h.off[1], $
                           uvalue = 'YOFF', $
                           format = "(F5.2)", $
                           xsize = 5, $
                           track = optblock.track, $
                           /capture, $
                           min = 0., $
                           step = 0.5, $
                           /simple)
  uvs.yleftid = cw_enter(cl, $
                         /double, $
                         /display, $
                         label = 'Y remain:', $
                         value = uvs.psize[1]-h.size[1]-h.off[1], $
                         format = "(F5.2)", $
                         xsize = 5)

  if ~is_gdl() then begin
     jb = widget_base(base, /row)
     junk = widget_droplist(jb, $
                            value = ['Courier',  $
                                     'Helvetica',  $
                                     'Helvetica Narrow',  $
                                     'NC Schoolbook',  $
                                     'Palatino',  $
                                     'Times', $
                                     'Avant Garde Book',  $
                                     'Avant Garde Demi',  $
                                     'Bookman Demi',  $
                                     'Bookman Light',  $
                                     'Zapf Chancery',  $
                                     'Zapf Dingbats', $
                                     'Symbol'], $
                            title = 'Font: Family:', $
                            uvalue = 'FFAMILY', $
                            track = optblock.track)
     widget_control, junk, set_droplist_select = h.font.family
     
     junk = widget_label(jb, $
                         value = 'Weight/slope:')
     
     swopt = [{label: 'Normal'},  $
              {label: 'Bold'}, $
              {label: 'Italic'}, $
              {label: 'Bold Italic'}]
     
     uvs.wsid = cw_pdmenu_plus(jb, $
                               swopt,  $
                               /selector, $
                               initial = h.font.wg_sl, $
                               uvalue = 'FWS', $
                               ids = bids, $
                               track = optblock.track)
     uvs.wsids = bids
     widget_control, uvs.wsid, sensitive = h.font.family le 9
     for j = 1, 3, 2 do widget_control, uvs.wsids[j], sensitive = $
                                        h.font.family le 5
  endif else begin
     uvs.wsid = 0l
     uvs.wsids[*] = 0l
  endelse
  
                                ; Filename
  jb = widget_base(base, $
                   /row)
  uvs.fileid = cw_enter(jb, $
                        label = 'File: ', $
                        value = h.name, $
                        xsize = 40, $
                        uvalue = 'FILE', $
                        track = optblock.track, $
                        /capture, $
                        /all_events)
  junk = widget_button(jb, $
                       value = 'Pick ...', $
                       uvalue = 'PFILE', $
                       track = optblock.track)
  junk = widget_button(jb, $
                       value = 'Default', $
                       uvalue = 'DFILE', $
                       track = optblock.track)

                                ; Spool command

  spbase = widget_base(base, $
                       /column)

  jb = widget_base(spbase, /row)
  uvs.cmid[0, 0] = cw_enter(jb, $
                         label = 'Spool Command:', $
                         value = h.action[0], $
                         uvalue = 'PSCMD', $
                         xsize = 12, $
                         track = optblock.track, $
                         /capture)

  uvs.cmid[1, 0]= cw_enter(jb, $
                         value = h.action[1], $
                         uvalue = 'PSSFX', $
                         xsize = 8, $
                         label = '<file>', $
                         track = optblock.track, $
                         /capture)

  junk = widget_button(jb, $
                       value = 'Default', $
                       uvalue = 'PSFVIEW')

  junk = widget_button(jb, $
                       value = 'None', $
                       uvalue = 'PSNOVIEW')

  jbb = widget_base(jb, $
                    /nonexclusive, $
                    /row)
  uvs.promptid[0] = widget_button(jbb, $
                                  value = 'Prompt?', $
                                  track = optblock.track, $
                                  uvalue = 'PSPROMPT')
  widget_control, uvs.promptid[0], set_button = h.prompt[0]
  

  jb = widget_base(spbase, /row)
  uvs.cmid[0, 1] = cw_enter(jb, $
                         label = 'EPS View Command:', $
                         value = h.viewer[0], $
                         uvalue = 'EPCMD', $
                         xsize = 12, $
                         track = optblock.track, $
                         /capture)

  uvs.cmid[1, 1]= cw_enter(jb, $
                         value = h.viewer[1], $
                         uvalue = 'EPSFX', $
                         xsize = 8, $
                         label = '<file>', $
                         track = optblock.track, $
                         /capture)

  junk = widget_button(jb, $
                       value = 'Default', $
                       uvalue = 'EPFVIEW')

  junk = widget_button(jb, $
                       value = 'None', $
                       uvalue = 'EPNOVIEW')

  jbb = widget_base(jb, $
                    /nonexclusive, $
                    /row)
  uvs.promptid[1] = widget_button(jbb, $
                                  value = 'Prompt?', $
                                  track = optblock.track, $
                                  uvalue = 'EPPROMPT')
  widget_control, uvs.promptid[1], set_button = h.prompt[1]
  
  jb = widget_base(spbase, /row)
  uvs.cmid[0, 2] = cw_enter(jb, $
                         label = 'PDF View Command:', $
                         value = h.pdfviewer[0], $
                         uvalue = 'PDCMD', $
                         xsize = 12, $
                         track = optblock.track, $
                         /capture)

  uvs.cmid[1, 2]= cw_enter(jb, $
                         value = h.pdfviewer[1], $
                         uvalue = 'PDSFX', $
                         xsize = 8, $
                         label = '<file>', $
                         track = optblock.track, $
                         /capture)

  junk = widget_button(jb, $
                       value = 'Default', $
                       uvalue = 'PDFVIEW')

  junk = widget_button(jb, $
                       value = 'None', $
                       uvalue = 'PDNOVIEW')

  jbb = widget_base(jb, $
                    /nonexclusive, $
                    /row)
  uvs.promptid[2] = widget_button(jbb, $
                                  value = 'Prompt?', $
                                  track = optblock.track, $
                                  uvalue = 'PDFPROMPT')
  widget_control, uvs.promptid[2], set_button = h.prompt[2]
  
  uvs.action = cw_enter(base, $
                        /text, $
                        /display, $
                        value = '', $
                        xsize = 65, $
                        label = 'Action:')

                                ; Quit button
  jb = widget_base(base, /row)
  junk = widget_button(jb, $
                       value = '    Apply    ', $
                       uvalue = 'DO', $
                       track = optblock.track)
  junk = widget_button(jb, $
                       value = '   Cancel   ', $
                       uvalue = 'CANCEL', $
                       track = optblock.track)

  widget_control, base, set_uvalue = uvs, /no_copy

  widget_control, tlb, /real

;	RYO widget management to allow us to get the values back from
;	the event handler without using a common block, even after the
;	hierarchy has been destroyed.

  widget_control, base, event_func = 'hopts_event'

  repeat begin
     ev = widget_event(base)
  endrep until (ev.exited ne 0)

  widget_control, base, get_uvalue = uvs, /no_copy
  if ev.exited eq 1 then pdefs.hardset = uvs.opts

  widget_control, tlb, /destroy

  widget_control, pdefs.ids.graffer, /sensitive

  if ev.exited eq -1 then return, 0 $
  else return, uvs.chflag

end
