; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_hard, pdefs, encapsulated = encapsulated, pdf = pdf, $
                redraw = redraw, no_spawn = no_spawn

;+
; GRAFF_HARD
;	Make a hardcopy
;
; Usage:
;	graff_hard, pdefs
;
; Argument:
;	pdefs	struct	in/out	The plot definition structure.
;
; Keyword:
;	/redraw	input	If set, then redraw the plot on the
;			screen device to ensure that the coordinate
;			system is reset properly.
;	/no_spawn	If set, then do not spawn any viewer or
;			spooling command for the generated file. This
;			does not affect the PS->PDF converter calls.
;	/encapsulated	If set, then generate an EPS file, or an
;			embeddable PDF file
;	/pdf		If set then generate a PDF file (since IDL/GDL
;			do not have a PDF device, a PS file is
;			generated and converted.)
;				
; History:
;	Carved from graffer: 17/8/95; SJT
;	Added no_set key: 8/9/95; SJT
;	Made to function returning "cancel" state: 18/12/96; SJT
;	Change STRPOS to RSTRPOS in filename generation: 23/3/98; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add dialogue for cases where only current dataset is being
;	shown: 26/1/12; SJT
;	Handle case when current-only is selected: 26/1/12; SJT
;	Support PDF output: 21/9/16; SJT
;	Add redraw key: 30/11/16; SJT
;	Make coordinates double: 24/5/17; SJT
;	Add NO_SPAWN: 8/5/18; SJT
;	Handle TT fonts: 12/2/20; SJT
;	Select device by keyword, convert to procedure: 3/11/21; SJT
;-

; For hard copy we don't normally want to show current DS only.
  tt = pdefs.transient.current_only
  if pdefs.transient.current_only then begin
     yn = dialog_message(['"Display current dataset only"', $
                          'is currently selected.', $
                          'Do you really want to print just', $
                          'the current dataset'], $
                         /question, $
                         /cancel, $
                         dialog_parent = pdefs.ids.graffer)

     case yn of
        "No": pdefs.transient.current_only = 0b
        "Cancel": return
        "Yes":
     endcase
  endif

  ido = 0
  
  if keyword_set(pdf) && ~gr_find_program('gs') then begin
     yn = dialog_message(['A PDF output format is requested', $
                          'but no "gs" executable could', $
                          'be found.', $
                          'Reset output to corresponding', $
                          'PostScript format?'], $
                         /cancel, $
                         dialog_parent = pdefs.ids.graffer)
     if yn eq 'Cancel' then return $
     else idev = keyword_set(encapsulated)
  endif else idev = keyword_set(encapsulated) + keyword_set(pdf)*2

  if pdefs.hardset.name eq '' then begin
     file = pdefs.name
     if (((dp = strpos(file, '.', /reverse_search))) ne -1) then  $
        file = strmid(file, 0, dp)
     case idev of 
        1: file = file+'.eps'
        0: file = file+'.ps'
        else: file = file+'.pdf'
     endcase
          
     hardname = pdefs.dir+file
  endif else begin
     file = pdefs.hardset.name
     if strpos(file, path_sep()) ge 0 then begin
        dd = file_dirname(file, /mark)
        file = file_basename(file)
        if ~file_test(dd, /direct) then begin
           dd = pdefs.dir
           pdefs.hardset.name = file
        endif
     endif else dd = pdefs.dir
     
     if (((dp = strpos(file, '.', /reverse_search))) ne -1) then  $
        file = strmid(file, 0, dp)
     case idev of 
        1: file = file+'.eps'
        0: file = file+'.ps'
        else: file = file+'.pdf'
     endcase
     hardname = dd+file
  endelse
  
  h = pdefs.hardset

  cpl = double(!D.x_size)/double(!D.x_ch_size)

  dev = !D.name
  set_plot, 'ps'


  if pdefs.fontopt eq 0 then !P.font = 0

  locs = where(((*pdefs.data).type eq -4 or (*pdefs.data).type eq 9) and $
               ((*pdefs.data).zopts.format eq 1 or $
                ((*pdefs.data).zopts.format eq 0 and $
                 (*pdefs.data).zopts.fill eq 1)), nim) 

  if (nim ne 0) then bits = 8 $
  else bits = 8*h.colour > 1

  case idev of
     0: begin
        psname = hardname
        ieps = 0
     end
     1: begin
        psname = hardname
        ieps = 1
     end
     2: begin
        psname = string(randomu(seed, /long), $
                        format = "('tmp_',z8.8,'.ps')")
        ieps = 0
     end
     3: begin
        psname = string(randomu(seed, /long), $
                        format = "('tmp_',z8.8,'.eps')")
        ieps = 1
     end
  endcase
  device, file = psname,  encapsulated = ieps, bits = bits, color = $
          h.colour
  if h.colour && ~is_gdl() then begin
     device, cmyk = h.cmyk
  endif
  device, /decomp
  !p.color = graff_colours(1)
  !p.background = graff_colours(0)


  psize = gr_get_page(h.psize, h.orient)

  if ieps then device, /portrait $
  else if (h.orient) then device, /portrait, xoffset = h.off[0], $
                                  yoffset = h.off[1] $
  else  device, /landscape, xoffset = h.off[1], yoffset = $
                psize[0]-h.off[0]

  device, xsize = h.size[0], ysize = h.size[1]

  bold = h.font.wg_sl and 1
  ital = (h.font.wg_sl and 2)/2

  if ~is_gdl() then case h.font.family of
     0: device, /courier, bold = bold, oblique = ital
     1: device, /helvetica, bold = bold, oblique = ital
     2: device, /helvetica, /narrow, bold = bold, oblique = ital
     3: device, /schoolbook, bold = bold, italic = ital
     4: device, /palatino, bold = bold, italic = ital
     5: device, /times, bold = bold, italic = ital
     6: device, /avantgarde, /book, oblique = ital
     7: device, /avantgarde, /demi, oblique = ital
     8: device, /bkman, /demi, italic = ital
     9: device, /bkman, /light, italic = ital
     10: device, /zapfchancery, /medium, /italic
     11: device, /zapfdingbats
     12: device, /symbol
  endcase

  ncpl = double(!D.x_size)/double(!D.x_ch_size)
  csiz = ncpl/cpl

  gr_plot_object, pdefs, /no_null, charsize = csiz, /plot_all, $
                  grey_ps = h.colour eq 0

  if (h.timestamp) then begin
     st = string(getenv('USER'), systime(), pdefs.version, $
                 format = "(A,' @ ',A,' from V',I0,'.',I2.2)")
     xyouts, /norm, .98, .01, st, charsize = 0.67, align = 1.0
  endif

  device, /close

  set_plot, dev

  if strpos(hardname, ' ') ne -1 then $
     chardname = "'"+hardname+"'" $
  else chardname = hardname

  case idev of
     1: begin
        if h.viewer[0] ne '' && ~keyword_set(no_spawn) then $
           spawn, h.viewer[0]+' '+chardname+' '+h.viewer[1]
        graff_msg, pdefs.ids.message, 'Output file is: '+hardname
     end
     0: begin
        if h.action[0] ne '' && ~keyword_set(no_spawn) then begin
           spawn, h.action[0]+' '+chardname+' '+h.action[1], cmdout
           graff_msg, pdefs.ids.message, cmdout
        endif else $
           graff_msg, pdefs.ids.message, 'Output file is: '+hardname
     end
     2: begin
        if h.psize then pssz = ' -sPAPERSIZE=a4 ' $
        else pssz = ' -sPAPERSIZE=letter '
        spawn, 'gs -q -dBATCH -dNOPAUSE -sDEVICE=pdfwrite '+ $
               '-sOutputFile='+chardname+ $
               pssz+psname
        if h.pdfviewer[0] ne '' && ~keyword_set(no_spawn) then $
           spawn, h.pdfviewer[0]+' '+chardname+' '+h.pdfviewer[1]
        graff_msg, pdefs.ids.message, 'Output file is: '+hardname
        file_delete, psname
     end
     3: begin
        xpts = round(72. * h.size[0] / 2.54)
        ypts = round(72. * h.size[1] / 2.54)
        pssz =  string(xpts, ypts, format = $
                       "(' -dDEVICEWIDTHPOINTS=',i0,"+ $
                       "' -dDEVICEHEIGHTPOINTS=',i0,' ')")
        spawn, 'gs -q -dBATCH -dNOPAUSE -sDEVICE=pdfwrite '+ $
               '-sOutputFile='+chardname+ $
               pssz+psname
        if h.pdfviewer[0] ne '' && ~keyword_set(no_spawn) then $
           spawn, h.pdfviewer[0]+' '+chardname+' '+h.pdfviewer[1]
        graff_msg, pdefs.ids.message, 'Output file is: '+hardname
        file_delete, psname
     end
  endcase

  if pdefs.fontopt eq 0 then !P.font = -1
  !p.color = graff_colours(1)
  !p.background = graff_colours(0)

  pdefs.transient.current_only = tt
  if keyword_set(redraw) then $
     gr_plot_object, pdefs      ; Redraw to ensure coordinates are
                                ; correct. 

end
