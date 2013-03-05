; Copyright (C) 2013
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

pro Gr_get_v1, pdefs, ilu, file_v

;+
; GR_GET_V1
;	Get a V1.x graffer dataset from a file
;
; Usage:
;	graff_get, pdefs, ilu
;
; Arguments:
;	pdefs	struct	in/out	The graffer data structure.
;	ilu	long	input	The unit number from which to read.
;	file_v	float	input	The version of the input file.
;
; History:
;	Original: 16/8/95; SJT
;	Remove file argument (use pdefs.name): 17/8/95; SJT
;	Add facilities for string-type data and restore filename: 18/8/95; SJT
;	Change to a function returning 0 or 1 & add NO_WARN key: 11/6/96; SJT
;	Rename as GRAFF_GET_V1; revert to procedure and modify to be
;	called from V2 GRAFF_GET: 5/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Drop CDF support: 10/2/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Advanced axis style settings: 21/8/12; SJT
;-

on_ioerror, nofile

rs = ''

readf, ilu, rs                  ; Plot headers
pdefs.title = rs
readf, ilu, rs
pdefs.subtitle = rs

fv = 0.
iv = 0
readf, ilu, fv, iv
pdefs.charsize = fv
pdefs.axthick = iv

axr = dblarr(2)
axs = intarr(2)
readf, ilu, axr, axs            ; X axis properties
pdefs.xrange = axr
pdefs.xtype = axs(0)
pdefs.xsty.idl = axs(1) and 31
pdefs.xsty.extra = 2*(axs(1)/128 and 1)
pdefs.xsty.minor = (axs(1)/32) and 1
pdefs.xsty.time = axs(1)/64 and 1
if ((axs(1) and 2048) ne 0) then pdefs.xsty.grid = (axs(1) and 2047)/256 + 1 $
else pdefs.xsty.grid = 0

 
readf, ilu, rs
pdefs.xtitle = rs

readf, ilu, axr, axs            ; Y axis properties
pdefs.yrange = axr
pdefs.ytype = axs(0)
pdefs.ysty.idl = axs(1) and 31
pdefs.ysty.extra = 2*(axs(1)/128 and 1)
pdefs.ysty.minor = (axs(1)/32 and 1)
pdefs.ysty.time = axs(1)/64 and 1
if ((axs(1) and 2048) ne 0) then pdefs.ysty.grid = (axs(1) and 2047)/256 + 1 $
else pdefs.ysty.grid = 0

readf, ilu, rs
pdefs.ytitle = rs

sets = intarr(2)
readf, ilu, sets                ; Number of data sets.
pdefs.nsets = sets(0)
pdefs.cset = sets(1)

nds = pdefs.nsets > 1
data = replicate({graff_data}, nds)

psym = 0 & symsize = 0. & line = 0 & colour = 0 & thick = 0
isort = 0b & ndata = 0 & ity = 0 & md = 0 & dsr = ''

for j = 0, nds-1 do begin
    readf, ilu, psym, symsize, line, colour, thick, isort, ndata
    if (psym eq 0) then begin
        data(j).psym = 0
        data(j).pline = 1
    endif else if (psym eq 10) then begin
        data(j).psym = 0
        data(j).pline = 2
    endif else begin
        data(j).psym = abs(psym)
        data(j).pline = psym lt 0
    endelse
    data(j).symsize = symsize
    data(j).line = line
    data(j).colour = colour
    data(j).thick = thick
    data(j).sort = isort
    data(j).ndata = ndata
    
    if (file_v[1] ge 1) then readf, ilu, dsr
    data(j).descript = dsr
    
    if (file_v[1] ge 5) then  $
      readf, ilu, ity, md $
    else readf, ilu, ity
    if (file_v le 1.01) then ity = ([0, -1, 1, 2])(ity) ;re-map type
                                ;codes 
    data(j).type = ity
    data(j).mode = md
    
    if (ity ge 0) then begin    ; Ordinary X-Y data
        xydata = dblarr(ity+2, ndata > 2)
        readf, ilu, xydata
    endif else begin            ; Function
        r = dblarr(2)
        if (ity eq -3) then f = strarr(2) $
        else f = ''
        readf, ilu, r
        readf, ilu, f
        if (ity eq -3) then  xydata = {graff_pfunct} $
        else xydata = {graff_funct}
    endelse
    data(j).xydata = ptr_new(xydata)
endfor

pdefs.data = ptr_new(data)


ntext = 0
readf, ilu, ntext               ; Number of text items
pdefs.ntext = ntext

ntext = ntext > 1

text = replicate({graff_text}, ntext)

colour = 0 & size = 0. & orient = 0. & align = 0. & font = 0 
thick = 0 & x = 0. & y = 0. & norm = 0b & texts = ''

for j = 0, ntext-1 do begin
    readf, ilu, colour, size, orient, align, font, thick, x, y, norm
    text(j).colour = colour
    text(j).size = size
    text(j).orient = orient
    text(j).align = align
    text(j).font = font
    text(j).thick = thick
    text(j).x = x
    text(j).y = y
    text(j).norm = norm
    

    readf, ilu, texts
    text(j).text = texts
endfor

pdefs.text = ptr_new(text)

;	The current text options

if (file_v ge 1.05) then begin
    readf, ilu, colour, size, orient, align, font, thick, x, y, norm
    
    pdefs.text_options.colour = colour
    pdefs.text_options.size = size
    pdefs.text_options.orient = orient
    pdefs.text_options.align = align
    pdefs.text_options.font = font
    pdefs.text_options.thick = thick
    pdefs.text_options.x = x
    pdefs.text_options.y = y
    pdefs.text_options.norm = norm
    pdefs.text_options.text = ''
endif                           ; No other - use the current values
                                ; for old format files.

;	CDF files options: although no longer used, still need to read
;	it through.

if (file_v[1] ge 5) then begin
    epoch = intarr(2) & file = '' & zero = 0.d0
    axt = 0b & rgf = 0b & ang = 0b
    rint = 0l & rstart = 0l
    readf, ilu, file
    readf, ilu, zero, epoch, axt, rgf, ang, rstart, rint
    
    if (file ne '') then graff_msg, pdefs.ids.message, $
      ["WARNING this V1 GRAFFER file contained CDF material", $
       "This will be lost by GRAFFER V2"]       
endif

;	Note: don't get the widget ids.

hf = bytarr(4)
hs = fltarr(2)
readf, ilu, hf, hs              ; The hardcopy options
pdefs.hardset.colour = hf(0)
pdefs.hardset.eps = hf(1)
pdefs.hardset.orient = hf(2)
pdefs.hardset.timestamp = hf(3)
pdefs.hardset.size = hs

readf, ilu, rs
pdefs.hardset.action(0) = rs
readf, ilu, rs
pdefs.hardset.action(1) = rs

if (not eof(ilu)) then begin    ; There are font definitions saved.
    fn = intarr(2)
    readf, ilu, fn
    pdefs.hardset.font.family = fn(0)
    pdefs.hardset.font.wg_sl = fn(1)
endif

free_lun, ilu

return

Nofile:

free_lun, ilu

graff_msg, pdefs.ids.message,  $
  ["Error reading file: May have incomplete file!", $
   !Err_string]

end
