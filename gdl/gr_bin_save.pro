; Copyright (C) 2013-2020
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

pro Gr_bin_save, pdefs, auto = auto

;+
; GR_BIN_SAVE
;	Save a graffer dataset to a file in binary format
;
; Usage:
;	gr_bin_save, pdefs
;
; Argument:
;	pdefs	struct	input	The graffer data structure.
;
; Keyword:
;	auto		input	If set & non-zero, then this is an
;				autosave.
;				
; History:
;	Original: (from GR_SAVE_ASC) 14/1/96; SJT
;	Drop CDF support: 10/2/97; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Add REM(arks) field: 23/6/97; SJT
;	Replace handles with pointers: 27/6/05; SJT
;	Support colour inversion: 26/6/07; SJT
;	Put isotropic setting: 25/6/08; SJT
;	Add key charsize: 29/4/09; SJT
;	Add local colour table option: 17/11/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	New V4 format: 6/1/12; SJT
;	New font handling and contour setting: 11/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Add options for plplot drivers: 29/11/13; SJT
;-

  if (keyword_set(auto)) then begin
     file = pdefs.dir+'#'+pdefs.name+'#'
     graff_msg, pdefs.ids.message, 'Autosaving'
  endif else begin
     file = pdefs.dir+pdefs.name
     if ~pdefs.transient.backup && file_test(file) then begin
        file_copy, file, file+'~', /overwrite
        pdefs.transient.backup = 1b
     endif
  endelse
  on_ioerror, no_open
  openw, ilu, /get, file, /swap_if_big_endian ; V4 files are always
                                ; written in little-endian order.

  on_ioerror, null

; Header & title information

  dl = strlen(pdefs.dir)
  nl = strlen(pdefs.name)
  t = systime()
  tl = strlen(t)

  writeu, ilu, 'GRAFFER', pdefs.version, $
          dl, pdefs.dir, nl, pdefs.name, tl, t

  graff_put_rec, ilu, 'GT ', pdefs.title
  graff_put_rec, ilu, 'GS ', pdefs.subtitle
  graff_put_rec, ilu, 'GC ', pdefs.charsize
  graff_put_rec, ilu, 'GA ', pdefs.axthick
  graff_put_rec, ilu, 'GP ', pdefs.position
  graff_put_rec, ilu, 'GR ', pdefs.aspect
  graff_put_rec, ilu, 'GI ', pdefs.isotropic 
  graff_put_rec, ilu, 'GHA', pdefs.match

; X-axis information

  graff_put_rec, ilu, 'XR ', pdefs.xrange
  graff_put_rec, ilu, 'XL ', pdefs.xtype
  graff_put_rec, ilu, 'XSI', pdefs.xsty.idl
  graff_put_rec, ilu, 'XSE', pdefs.xsty.extra
  graff_put_rec, ilu, 'XSG', pdefs.xsty.grid
  graff_put_rec, ilu, 'XST', pdefs.xsty.time
  graff_put_rec, ilu, 'XSZ', pdefs.xsty.tzero
  graff_put_rec, ilu, 'XMJ', pdefs.xsty.major
  graff_put_rec, ilu, 'XFM', pdefs.xsty.format
  graff_put_rec, ilu, 'XMN', pdefs.xsty.minor
  if ptr_valid(pdefs.xsty.values) then $
     graff_put_rec, ilu, 'XVL', *pdefs.xsty.values
  graff_put_rec, ilu, 'XT ', pdefs.xtitle

; Y-axis information

  graff_put_rec, ilu, 'YIR', pdefs.y_right
  graff_put_rec, ilu, 'YR ', pdefs.yrange
  graff_put_rec, ilu, 'YL ', pdefs.ytype
  graff_put_rec, ilu, 'YSI', pdefs.ysty.idl
  graff_put_rec, ilu, 'YSE', pdefs.ysty.extra
  graff_put_rec, ilu, 'YSG', pdefs.ysty.grid
  graff_put_rec, ilu, 'YST', pdefs.ysty.time
  graff_put_rec, ilu, 'YSZ', pdefs.ysty.tzero
  graff_put_rec, ilu, 'YMJ', pdefs.ysty.major
  graff_put_rec, ilu, 'YFM', pdefs.ysty.format
  graff_put_rec, ilu, 'YMN', pdefs.ysty.minor
  if ptr_valid(pdefs.ysty.values) then $
     graff_put_rec, ilu, 'YVL', *pdefs.ysty.values
  graff_put_rec, ilu, 'YT ', pdefs.ytitle

; Secondary Y-axis information

  graff_put_rec, ilu, 'RR ', pdefs.yrange_r
  graff_put_rec, ilu, 'RL ', pdefs.ytype_r
  graff_put_rec, ilu, 'RSI', pdefs.ysty_r.idl
  graff_put_rec, ilu, 'RSE', pdefs.ysty_r.extra
  graff_put_rec, ilu, 'RSG', pdefs.ysty_r.grid
  graff_put_rec, ilu, 'RST', pdefs.ysty_r.time
  graff_put_rec, ilu, 'RSZ', pdefs.ysty_r.tzero
  graff_put_rec, ilu, 'RMJ', pdefs.ysty_r.major
  graff_put_rec, ilu, 'RFM', pdefs.ysty_r.format
  graff_put_rec, ilu, 'RMN', pdefs.ysty_r.minor
  if ptr_valid(pdefs.ysty_r.values) then $
     graff_put_rec.ilu, 'RVL', *pdefs.ysty_r.values
  graff_put_rec, ilu, 'RT ', pdefs.ytitle_r

; Colour table for displayed Z data
  graff_put_rec, ilu, 'ZT ', pdefs.ctable
  graff_put_rec, ilu, 'ZG ', pdefs.gamma

; Number of data sets.
  graff_put_rec, ilu, 'DN ', pdefs.nsets
  graff_put_rec, ilu, 'DC ', pdefs.cset

;	Output each dataset

  for j = 0, (pdefs.nsets-1) > 0 do begin
     n1 = (*pdefs.data)[j].ndata
     n2 = (*pdefs.data)[j].ndata2

     graff_put_rec, ilu, 'DS ', j
     graff_put_rec, ilu, 'D  ', (*pdefs.data)[j].descript
     graff_put_rec, ilu, 'T  ', (*pdefs.data)[j].type
     graff_put_rec, ilu, 'M  ', (*pdefs.data)[j].mode
     graff_put_rec, ilu, 'Y  ', (*pdefs.data)[j].y_axis
     graff_put_rec, ilu, 'N  ', n1
     graff_put_rec, ilu, 'N2 ', n2
     
     graff_put_rec, ilu, 'J  ', (*pdefs.data)[j].pline
     graff_put_rec, ilu, 'P  ', (*pdefs.data)[j].psym
     graff_put_rec, ilu, 'S  ', (*pdefs.data)[j].symsize
     graff_put_rec, ilu, 'L  ', (*pdefs.data)[j].line
     graff_put_rec, ilu, 'C  ', (*pdefs.data)[j].colour
     if (*pdefs.data)[j].colour eq -2 then $
        graff_put_rec, ilu, 'CV ', (*pdefs.data)[j].c_vals
     graff_put_rec, ilu, 'W  ', (*pdefs.data)[j].thick
     graff_put_rec, ilu, 'O  ', (*pdefs.data)[j].sort
     graff_put_rec, ilu, 'K  ', (*pdefs.data)[j].noclip
     graff_put_rec, ilu, 'E  ', (*pdefs.data)[j].medit
     
     if finite((*pdefs.data)[j].min_val) then $
        graff_put_rec, ilu, 'MN ', (*pdefs.data)[j].min_val
     if finite((*pdefs.data)[j].max_val) then $
        graff_put_rec, ilu, 'MX ', (*pdefs.data)[j].max_val

     if not ptr_valid((*pdefs.data)[j].xydata) then begin
        graff_put_rec, ilu, 'DE '
        continue
     endif

     if n1 ne 0 then begin
        xydata = *(*pdefs.data)[j].xydata

        if ((*pdefs.data)[j].type lt 0) then begin ; Function
           graff_put_rec, ilu, 'R  ', xydata.range
           if ((*pdefs.data)[j].type eq -3) then begin
              graff_put_rec, ilu, 'FX ', xydata.funct(0)
              graff_put_rec, ilu, 'FY ', xydata.funct(1)
           endif else graff_put_rec, ilu, 'F  ', xydata.funct
           
        endif else if ((*pdefs.data)[j].type eq 9) then begin ; 2-D data
           graff_put_rec, ilu, 'ZXS', *xydata.x
           graff_put_rec, ilu, 'ZYS', *xydata.y
           graff_put_rec, ilu, 'ZZS', *xydata.z
           
        endif else begin        ; Ordinary data
           sxy = size(xydata)
           graff_put_rec, ilu, 'VS ', xydata
        endelse
     endif
                                ; We only need to save the 2-D
                                ; specific options for 2-D data (types
                                ; 9 & -4)
     
     if ((*pdefs.data)[j].type eq 9 or (*pdefs.data)[j].type eq -4) $
     then begin
        zopts = (*pdefs.data)[j].zopts
        graff_put_rec, ilu, 'ZF ', zopts.format
        
        graff_put_rec, ilu, 'ZCF', zopts.fill
        graff_put_rec, ilu, 'ZLI', zopts.label
        graff_put_rec, ilu, 'ZLO', zopts.label_off
        graff_put_rec, ilu, 'ZCS', zopts.charsize
        graff_put_rec, ilu, 'ZCT', zopts.ctable
        graff_put_rec, ilu, 'ZCG', zopts.gamma
        
        if (zopts.set_levels and $
            ptr_valid(zopts.levels)) then $ ; Explicit levels
               graff_put_rec, ilu, 'ZL ', *(zopts.levels) $
        else graff_put_rec, ilu, 'ZNL', zopts.n_levels
        if zopts.n_cols gt 0 then begin
           graff_put_rec, ilu, 'ZC ', *(zopts.colours)
           if ptr_valid(zopts_raw_colours) then $
              graff_put_rec, ilu, 'ZCR', *(zopts.raw_colours)
        endif

        if zopts.n_sty gt 0 then graff_put_rec, ilu, 'ZS ', $
                                                *(zopts.style)
        if zopts.n_thick gt 0 then graff_put_rec, ilu, 'ZT ', $
                                                  *(zopts.thick)
        
        graff_put_rec, ilu, 'ZR ', zopts.range
        graff_put_rec, ilu, 'ZP ', zopts.pxsize
        graff_put_rec, ilu, 'ZIL', zopts.ilog
        graff_put_rec, ilu, 'ZIN', zopts.invert
        graff_put_rec, ilu, 'ZM ', zopts.missing
        graff_put_rec, ilu, 'ZSM', zopts.smooth
        graff_put_rec, ilu, 'ZSN', zopts.shade_levels
     endif
     
     graff_put_rec, ilu, 'DE '
     
  endfor

;	Note: don't save the widget ids.

  graff_put_rec, ilu, 'TN ', pdefs.ntext ; Number of text items

  for j = 0, pdefs.ntext-1 do begin
     graff_put_rec, ilu, 'TS ', j
     graff_put_rec, ilu, 'TID', (*pdefs.text)[j].id
     graff_put_rec, ilu, 'T  ', (*pdefs.text)[j].text
     graff_put_rec, ilu, 'X  ', (*pdefs.text)[j].x
     graff_put_rec, ilu, 'Y  ', (*pdefs.text)[j].y
     graff_put_rec, ilu, 'N  ', (*pdefs.text)[j].norm
     graff_put_rec, ilu, 'AX ', (*pdefs.text)[j].axis
     graff_put_rec, ilu, 'C  ', (*pdefs.text)[j].colour
     if (*pdefs.text)[j].colour eq -2 then $
        graff_put_rec, ilu, 'CV ', (*pdefs.text)[j].c_vals
     graff_put_rec, ilu, 'S  ', (*pdefs.text)[j].size
     graff_put_rec, ilu, 'O  ', (*pdefs.text)[j].orient
     graff_put_rec, ilu, 'A  ', (*pdefs.text)[j].align
     graff_put_rec, ilu, 'FF ', (*pdefs.text)[j].ffamily
     graff_put_rec, ilu, 'F  ', (*pdefs.text)[j].font
     graff_put_rec, ilu, 'W  ', (*pdefs.text)[j].thick
     graff_put_rec, ilu, 'TE '
  endfor

;	The text template (no need to dump the text string or position
;	here as it 
;	must be the null string, only included to simplify coding)

  graff_put_rec, ilu, 'TTS'
  graff_put_rec, ilu, 'C  ', pdefs.text_options.colour
  if pdefs.text_options.colour eq -2 then $
     graff_put_rec, ilu, 'CV ', pdefs.text_options.c_vals
  graff_put_rec, ilu, 'S  ', pdefs.text_options.size
  graff_put_rec, ilu, 'O  ', pdefs.text_options.orient
  graff_put_rec, ilu, 'A  ', pdefs.text_options.align
  graff_put_rec, ilu, 'F  ', pdefs.text_options.font
  graff_put_rec, ilu, 'W  ', pdefs.text_options.thick
  graff_put_rec, ilu, 'N  ', pdefs.text_options.norm
  graff_put_rec, ilu, 'TTE'

;	Specify the key information

  graff_put_rec, ilu, 'KU ', pdefs.key.use
  graff_put_rec, ilu, 'KX ', pdefs.key.x
  graff_put_rec, ilu, 'KY ', pdefs.key.y
  graff_put_rec, ilu, 'KN ', pdefs.key.norm
  graff_put_rec, ilu, 'KC ', pdefs.key.cols
  graff_put_rec, ilu, 'KF ', pdefs.key.frame
  graff_put_rec, ilu, 'KP ', pdefs.key.one_point
  graff_put_rec, ilu, 'KS ', pdefs.key.csize
  graff_put_rec, ilu, 'KT ', pdefs.key.title

  if ptr_valid(pdefs.key.list) then $
     graff_put_rec, ilu, 'KL ', *pdefs.key.list

;	Any remarks associated with the file.

  if ptr_valid(pdefs.remarks) then $
     graff_put_rec, ilu, 'REM', *pdefs.remarks

                                ; The hardcopy options

  graff_put_rec, ilu, 'HC ', pdefs.hardset.colour
  graff_put_rec, ilu, 'HE ', pdefs.hardset.eps
  graff_put_rec, ilu, 'HO ', pdefs.hardset.orient
  graff_put_rec, ilu, 'HY ', pdefs.hardset.cmyk
  graff_put_rec, ilu, 'HP ',  pdefs.hardset.psize
  graff_put_rec, ilu, 'HT ', pdefs.hardset.timestamp
  graff_put_rec, ilu, 'HS ', pdefs.hardset.size
  graff_put_rec, ilu, 'HD ', pdefs.hardset.off

  graff_put_rec, ilu, 'HAB', pdefs.hardset.action[0]
  graff_put_rec, ilu, 'HAA', pdefs.hardset.action[1]
  graff_put_rec, ilu, 'HVB', pdefs.hardset.viewer[0]
  graff_put_rec, ilu, 'HVA', pdefs.hardset.viewer[1]
  graff_put_rec, ilu, 'HPB', pdefs.hardset.pdfviewer[0]
  graff_put_rec, ilu, 'HPA', pdefs.hardset.pdfviewer[1]

  graff_put_rec, ilu, 'HF ', pdefs.hardset.font.family
  graff_put_rec, ilu, 'HWS', pdefs.hardset.font.wg_sl
  graff_put_rec, ilu, 'HFN', pdefs.hardset.name

  graff_put_rec, ilu, 'HPS', pdefs.hardset.psdev
  graff_put_rec, ilu, 'HEP', pdefs.hardset.epsdev
  graff_put_rec, ilu, 'HPD', pdefs.hardset.pdfdev
  graff_put_rec, ilu, 'HSV', pdefs.hardset.svgdev

  free_lun, ilu

  if (not keyword_set(auto)) then pdefs.chflag = 0b ; Clear change flag
  pdefs.transient.changes = 0

  pdefs.is_ascii = 0b

  return

No_open:
  graff_msg, pdefs.ids.message, ["Failed to open save file:"+file, $
                                 !Err_string]

end
