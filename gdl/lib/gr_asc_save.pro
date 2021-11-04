; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_asc_save, pdefs

;+
; GR_ASC_SAVE
;	Save a graffer dataset to a file
;
; Usage:
;	gr_asc_save, pdefs
;
; Argument:
;	pdefs	struct	input	The graffer data structure.
;
; History:
;	Original: 16/8/95; SJT
;	Remove file argument (use pdefs.name): 17/8/95; SJT
;	Add facilities for string-type data: 18/8/95; SJT
;	Add auto-save: 22/9/95; SJT
;	Complete rewrite for new (V2) file organization should
;	facilitate future extensions without some of the ghastly
;	coding needed to (say) read 1.04 files with 1.05 program:
;	1/11/96; SJT
;	Rename as GR_ASC_SAVE: 14/1/97; SJT
;	Drop CDF support: 10/2/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Add REM(arks) field: 23/6/97; SJT
;	Support colour inversion: 26/6/07; SJT
;	Put isotropic flag: 25/6/08; SJT
;	Add key charsize: 29/4/09; SJT
;	Add local colour table option: 17/11/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	New font handling and contour setting: 11/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Add PDF viewer: 21/9/16; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;	Font option: 11/2/20; SJT
;	Add log_bands values: 24/6/21; SJT
;-

  file = pdefs.dir+pdefs.name
  if ~pdefs.transient.backup && file_test(file) then begin
     file_copy, file, file+'~', /overwrite
     pdefs.transient.backup = 1b
  endif

  on_ioerror, no_open
  openw, ilu, /get, file
  on_ioerror, null

; Header & title information

  printf, ilu, pdefs.version, pdefs.dir, pdefs.name, systime(),  $
          format = "('Graffer V',I2,'.',I2.2,': ',2A,' : @ ',A)"

  printf, ilu, 'GT:', pdefs.title, format = "(2a)"
  printf, ilu, 'GS:', pdefs.subtitle, format = "(2a)"
  printf, ilu, 'GC:', pdefs.charsize, ':GA:', pdefs.axthick, format = $
          "(a,f8.4,a,f7.3)"
  printf, ilu, 'GP:', pdefs.position, ':GR:', pdefs.aspect,  ':GI:', $
          pdefs.isotropic, ':GHA:', pdefs.match, ':GF', $
          pdefs.fontopt, $
          format = "(a,4f8.5,a,f9.5,f8.5,2(a,I1),a,i2)"

; X-axis information

  printf, ilu, 'XR:', pdefs.xrange, ':XL:', pdefs.xtype, format = $
          "(a,2g19.12,a,i2)"
  printf, ilu, 'XSI:', pdefs.xsty.idl, ':XSE:', pdefs.xsty.extra, $
          ':XSG:', pdefs.xsty.grid, ':XST:', pdefs.xsty.time, $
          ':XSZ:', pdefs.xsty.tzero, format = "(5(a,i6))"
  printf, ilu, 'XMJ:', pdefs.xsty.major, ':XMN:', pdefs.xsty.minor, $
          format = "(2(a,i5))"
  printf, ilu, 'XFM:', pdefs.xsty.format, format = "(2a)"
  printf, ilu, 'XLL:', pdefs.xsty.log_bands, format = $
          "(a,3i4)"
  
  if ptr_valid(pdefs.xsty.values) then begin
     nvals =  n_elements(*pdefs.xsty.values)
     printf, ilu, 'XNV:', nvals, format = $
             "(a,I3)"
     fmt = string(nvals, format = "('(a,',i0,'g19.12)')")
     printf, ilu, 'XVL:', *pdefs.xsty.values, format = fmt
  endif
  printf, ilu, 'XT:', pdefs.xtitle, format = "(2a)"

; Y-axis information

  printf, ilu, 'YIR:', pdefs.y_right, format = "(a,i2)"
  printf, ilu, 'YR:', pdefs.yrange, ':YL:', pdefs.ytype, format = $
          "(a,2g19.12,a,i2)"
  printf, ilu, 'YSI:', pdefs.ysty.idl, ':YSE:', pdefs.ysty.extra, $
          ':YSG:', pdefs.ysty.grid, ':YST:', pdefs.ysty.time, $
          ':YSZ:', pdefs.ysty.tzero, format = "(5(a,i6))"
  printf, ilu, 'YMJ:', pdefs.ysty.major, ':YMN:', pdefs.ysty.minor, $
          format = "(2(a,i5))"
  printf, ilu, 'YFM:', pdefs.ysty.format, format = "(2a)"
  printf, ilu, 'YLL:', pdefs.ysty.log_bands, format = $
          "(a,3i4)"
  
  if ptr_valid(pdefs.ysty.values) then begin
     nvals =  n_elements(*pdefs.ysty.values)
     printf, ilu, 'YNV:', nvals, format = $
             "(a,I3)"
     fmt = string(nvals, format = "('(a,',i0,'g19.12)')")
     printf, ilu, 'YVL:', *pdefs.ysty.values, format = fmt
  endif
  printf, ilu, 'YT:', pdefs.ytitle, format = "(2a)"

; Secondary Y-axis information

  printf, ilu, 'RR:', pdefs.yrange_r, ':RL:', pdefs.ytype_r, format = $
          "(a,2g19.12,a,i2)"
  printf, ilu, 'RSI:', pdefs.ysty_r.idl, ':RSE:', pdefs.ysty_r.extra, $
          ':RSG:', pdefs.ysty_r.grid, ':RST:', pdefs.ysty_r.time, $
          ':RSZ:', pdefs.ysty_r.tzero, format = "(5(a,i6))"
  printf, ilu, 'RMJ:', pdefs.ysty_r.major, ':RMN:', pdefs.ysty_r.minor, $
          format = "(2(a,i5))"
  printf, ilu, 'RFM:', pdefs.ysty_r.format, format = "(2a)"
  printf, ilu, 'RLL:', pdefs.ysty_r.log_bands, format = $
          "(a,3i4)"
  
  if ptr_valid(pdefs.ysty_r.values) then begin
     nvals =  n_elements(*pdefs.ysty_r.values)
     printf, ilu, 'RNV:', nvals, format = $
             "(a,I3)"
     fmt = string(nvals, format = "('(a,',i0,'g19.12)')")
     printf, ilu, 'RVL:', *pdefs.ysty_r.values, format = fmt
  endif

  printf, ilu, 'RT:', pdefs.ytitle_r, format = "(2a)"

; Default Colour table for displayed Z data
  printf, ilu, 'ZT:', pdefs.ctable, ':ZG:', pdefs.gamma, format = $
          "(a,i4,a,f7.3)"

; Number of data sets.
  printf, ilu, 'DN:', pdefs.nsets, ':DC:', pdefs.cset, format = $
          "(2(a,i6))"

;	Output each dataset

  for j = 0, (pdefs.nsets-1) > 0 do begin
     n1 = (*pdefs.data)[j].ndata
     n2 = (*pdefs.data)[j].ndata2

     printf, ilu, 'DS:', j, format = "(a,i6)"
     printf, ilu, 'D:', (*pdefs.data)[j].descript, format = "(2a)"
     printf, ilu, 'T:', (*pdefs.data)[j].type, ':M:', $
             (*pdefs.data)[j].mode, ':Y:', (*pdefs.data)[j].y_axis, ':N:', $
             n1, ':N2:', n2,  $ 
             format = "(a,i3,2(a,i2),2(a,i8))"
     
     printf, ilu, 'J:', (*pdefs.data)[j].pline, ':P:', $
             (*pdefs.data)[j].psym, ':S:', $
             (*pdefs.data)[j].symsize, ':L:', (*pdefs.data)[j].line, $
             ':C:', $
             (*pdefs.data)[j].colour, $
             ':W:', (*pdefs.data)[j].thick, ':O:', $
             (*pdefs.data)[j].sort, ':K:', $
             (*pdefs.data)[j].noclip, ':E:', (*pdefs.data)[j].medit,  $
             format = "(2(a,i3),a,f8.3,a,i2,a,i3,a,f8.4,3(a,i1))"
     if (*pdefs.data)[j].colour eq -2 then $
        printf, ilu, 'CV:',  (*pdefs.data)[j].c_vals, format = "(a,3i4)"
     if finite((*pdefs.data)[j].min_val) then $
        printf, ilu, 'MN:', (*pdefs.data)[j].min_val, format = "(A,g19.12)"
     if finite((*pdefs.data)[j].max_val) then $
        printf, ilu, 'MX:', (*pdefs.data)[j].max_val, format = "(A,g19.12)"

     if not ptr_valid((*pdefs.data)[j].xydata) then begin
        printf, ilu, 'DE:', format = "(a)"
        continue
     endif

     if n1 ne 0 then begin
        xydata = *(*pdefs.data)[j].xydata
        
        if ((*pdefs.data)[j].type lt 0) then begin ; Function
           printf, ilu, 'R:', xydata.range, format = "(a,4g19.12)"
           if ((*pdefs.data)[j].type eq -3) then $
              printf, ilu, 'FX:', xydata.funct(0), 'FY:', $
                      xydata.funct(1), $
                      format = "(2a/2a)" $ ; Make 2 lines for parametric case
           else printf, ilu, 'F:', xydata.funct, format = "(2a)"
        endif else if ((*pdefs.data)[j].type eq 9) then begin ; 2-D data
           printf, ilu, 'ZX2:', xydata.x_is_2d, ':ZY2:', $
                   xydata.y_is_2d, $
                   format = "(2(a,i1))"
           printf, ilu, 'ZXS:', format = "(A)"
           printf, ilu, *xydata.x, format = "(6g19.12)"
           printf, ilu, 'ZXE:', format = "(A)"
           printf, ilu, 'ZYS:', format = "(A)"
           printf, ilu, *xydata.y, format = "(6g19.12)"
           printf, ilu, 'ZYE:', format = "(A)"
           printf, ilu, 'ZZS:', format = "(A)"
           printf, ilu, *xydata.z, format = "(6g19.12)"
           printf, ilu, 'ZZE:', format = "(A)"
           
           
        endif else begin        ; Ordinary data
           sxy = size(xydata)
           fmtd = '('+string(sxy(1), format = "(I0)")+'g19.12)'
           printf, ilu, 'VS:', sxy(1), format = "(A,I2)"
           printf, ilu, xydata, format = fmtd
           printf, ilu, 'VE:', format = "(a)"
        endelse
     endif
                                ; We only need to save the 2-D
                                ; specific options for 2-D data (types
                                ; 9 & -4)
     
     if ((*pdefs.data)[j].type eq 9 or (*pdefs.data)[j].type eq -4) $
     then begin
        zopts = (*pdefs.data)[j].zopts
        printf, ilu, 'ZF:', zopts.format, format = "(A,I2)"

        printf, ilu, 'ZNL:', zopts.n_levels, $
                ':ZNC:', zopts.n_cols, $
                ':ZNS:', zopts.n_sty, $ 
                ':ZNT:', zopts.n_thick, $
                ':ZCF:', zopts.fill, $
                ':ZLM:', zopts.lmap, $
                ':ZCT:', zopts.ctable, $
                ':ZLI:', zopts.label, $
                ':ZLO:', zopts.label_off, $
                ':ZCS:', zopts.charsize, $ 
                format = "(4(a,i3),3(a,i2),2(a,i3),a,f7.3)" 
        printf, ilu, 'ZCG:', zopts.gamma, format = "(a,f7.3)"

        if (zopts.set_levels and $
            ptr_valid(zopts.levels)) then begin ; Explicit levels
           if zopts.n_levels lt 5 then begin
              fmt = string(zopts.n_levels, format = $
                           "('(a,',i0,'g19.12)')")
              printf, ilu, 'ZL:', *(zopts.levels), $
                      format = fmt
           endif else begin
              printf, ilu, 'ZLL:', format = "(A)"
              printf, ilu, *(zopts.levels), format = "(5g19.12)"
           endelse
        endif

        if zopts.n_cols gt 15 then begin
           printf, ilu, 'ZCL:', format = "(A)"
           printf, ilu, *(zopts.colours), format = "(15I5)"
        endif else if zopts.n_cols gt 0 then begin
           fmt = string(zopts.n_cols, format = "('(a,',i0,'I10)')")
           printf, ilu, 'ZC:', *(zopts.colours), format = fmt
        endif

        if ptr_valid(zopts.raw_colours) then begin
           printf, ilu, 'ZCR:', format = "(A)"
           printf, ilu, *(zopts.raw_colours), format = "(15i5)"
        endif
        
        if zopts.n_sty gt 20 then begin
           printf, ilu, 'ZSL:', format = "(A)"
           printf, ilu, *(zopts.style),  format = "(20I4)"
        endif else if zopts.n_sty gt 0 then begin
           fmt = string(zopts.n_sty, format = "('(a,',i0,'I4)')")
           printf, ilu, 'ZS:', *(zopts.style), format = fmt
        endif

        if zopts.n_thick gt 10 then begin
           printf, ilu, 'ZTL:', format = "(A)"
           printf, ilu, *(zopts.thick),  format = "(10f7.3)"
        endif else if zopts.n_thick gt 0 then begin
           fmt = string(zopts.n_thick, format = "('(a,',i0,'f7.3)')")
           printf, ilu, 'ZT:', *(zopts.thick), format = fmt
        endif

        printf, ilu, 'ZR:', zopts.range, ':ZP:', $
                zopts.pxsize, ':ZIL:', $
                zopts.ilog, ':ZIN:', zopts.invert, ':ZM:', $
                zopts.missing, format = $ 
                "(a,2g19.12,a,f7.3,a,I1,a,I1,a,g19.12)"

     endif
     
     printf, ilu, 'DE:', format = "(a)"
     
  endfor


;	Note: don't save the widget ids.

  printf, ilu, 'TN:', pdefs.ntext, format = "(a,i6)" ; Number of text items

  for j = 0, pdefs.ntext-1 do begin
     printf, ilu, 'TS:', j, format = "(a,i6)"
     printf, ilu, 'TID:', (*pdefs.text)[j].id, format = "(2a)"
     printf, ilu, 'T:', (*pdefs.text)[j].text, format = "(2a)"
     printf, ilu, 'X:', (*pdefs.text)[j].x, ':Y:', (*pdefs.text)[j].y, $ 
             ':N:', (*pdefs.text)[j].norm, ':AX:', (*pdefs.text)[j].axis, $
             format = "(2(a,g19.12),2(a,i2))"
     printf, ilu, 'C:', (*pdefs.text)[j].colour, ':S:', $
             (*pdefs.text)[j].size, ':O:', $
             (*pdefs.text)[j].orient, ':A:', (*pdefs.text)[j].align, $
             ':FF:', (*pdefs.text)[j].ffamily, $
             ':F:', (*pdefs.text)[j].font, $
             ':W:', (*pdefs.text)[j].thick, format = $
             "(a,i3,a,f8.3,a,f9.4,a,f8.5,2(a,i3),a,f7.2)"
     if (*pdefs.text)[j].colour eq -2 then $
        printf, ilu, 'CV:', (*pdefs.text)[j].c_vals, format = "(a,3I4)"

     printf, ilu, 'TE:', format = "(a)"
  endfor

;	The text template (no need to dump the text string or position
;	here as it 
;	must be the null string, only included to simplify coding)

  printf, ilu, 'TTS:', format = "(a)"
  printf, ilu, 'C:', pdefs.text_options.colour, $
          ':S:', pdefs.text_options.size, $
          ':O:', pdefs.text_options.orient, $
          ':A:', pdefs.text_options.align, $
          ':F:', pdefs.text_options.font, $
          ':W:', pdefs.text_options.thick, $
          ':N:', pdefs.text_options.norm, $
          format = $
          "(a,i3,a,f8.3,a,f9.4,a,f8.5,a,i3,a,f7.3,a,i2)"
  if pdefs.text_options.colour eq -2 then $
     printf, ilu, 'CV:', pdefs.text_options.c_vals, format = "(A,3I4)"

  printf, ilu, 'TTE:', format = "(a)"


;	Specify the key information

  printf, ilu, 'KU:', pdefs.key.use, ':KN:', pdefs.key.norm, ':KC:', $
          pdefs.key.cols, ':KF:', pdefs.key.frame, ':KP:', $
          pdefs.key.one_point, ':KR:', pdefs.key.reverse, $
          format = "(a,i2,a,i2,a,i3,3(a,i2))"
  printf, ilu, 'KX:', pdefs.key.x, ':KY:', pdefs.key.y, ':KS:', $
          pdefs.key.csize, format =  "(3(a,2g19.12))"
  printf, ilu, 'KT:', pdefs.key.title, format = "(2a)"

  if ptr_valid(pdefs.key.list) then $
     printf, ilu, 'KLN:', n_elements(*pdefs.key.list), $  $
             ':KL:', *pdefs.key.list, format = "(a,i5,a," + $
             string(n_elements(*pdefs.key.list), $
                    format = "(I0)") + "I5)"

;	Any remarks associated with the file.

  if ptr_valid(pdefs.remarks) then begin
     printf, ilu, 'REM:', n_elements(*pdefs.remarks), format = "(a,i4)"
     printf, ilu, *pdefs.remarks, format = "(a)"
  endif

                                ; The hardcopy options

  printf, ilu, 'HC:', pdefs.hardset.colour,  $
          ':HO:', pdefs.hardset.orient, ':HP:', $
          pdefs.hardset.psize, ':HT:', $
          pdefs.hardset.timestamp, ':HY:', pdefs.hardset.cmyk, format = $
          "(5(a,i2))" 
  printf, ilu, 'HS:', pdefs.hardset.size, ':HD:', pdefs.hardset.off, $
          format = "(2(a,2f8.3))"

  printf, ilu, 'HAB:', pdefs.hardset.action[0], 'HAA:', $
          pdefs.hardset.action(1), format = "(2a/2a)"
  printf, ilu, 'HVB:', pdefs.hardset.viewer[0], 'HVA:', $
          pdefs.hardset.viewer[1], format = "(2a/2a)"
  printf, ilu, 'HPB:', pdefs.hardset.pdfviewer[0], 'HPA:', $
          pdefs.hardset.pdfviewer[1], format = "(2a/2a)"

  printf, ilu, 'HF:', pdefs.hardset.font.family, ':HWS:', $
          pdefs.hardset.font.wg_sl, format = "(a,i3,a,i2)"
  printf, ilu, 'HFN:', pdefs.hardset.name, format = "(2a)"

  printf, ilu, 'HPS:', pdefs.hardset.psdev, $
          'HEP:', pdefs.hardset.epsdev, 'HPD:', pdefs.hardset.pdfdev, $
          'HSV:', pdefs.hardset.svgdev
          
  free_lun, ilu

  pdefs.chflag = 0b             ; Clear change flag
  pdefs.transient.changes = 0

  pdefs.is_ascii = 1b

  return

No_open:
  graff_msg, pdefs.ids.message, ["Failed to open save file:"+file, $
                                 !Err_string]

end
