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

pro gr_font_remap, pdefs

;+
; GR_FONT_REMAP
;	Remap IDL font settings to ones appropriate to plplot.
;
; Arguments:
;	pdefs	struct	in/out	The main GRAFFER data structure.
;
; History:
;	Original: 10/3/13; SJT
;-

  if (fvers[0] eq 4  && fvers[1] gt 6) then return

  ffam = [[1, 5, 1, 2, 5, 2, 5, 5, 2, 4, 4, 2, 2, 1, 2, 2, 1, 5], $
          [1, 1, 1, 1, 2, 2, 5, 5, 3, 3, 2, 2, 2, 2, 1, 4, 4, 1], $
          [1, 1, 1, 1, 2, 2, 5, 5, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1]]
  fshp = [[1, 1, 2, 1, 2, 3, 1, 1, 1, 1, 2, 1, 1, 1, 2, 4, 1, 1], $
          [1, 2, 1, 6, 1, 4, 1, 1, 1, 5, 1, 3, 2, 4, 1, 1, 2, 1], $
          [1, 2, 3, 4, 1, 3, 1, 1, 1, 3, 2, 4, 2, 4, 1, 1, 1, 1]]

  if (pdefs.hardset.font.wg_sl ge 3) then begin
     tff = pdefs.hardset.font.family + 1
     tsh = pdefs.hardset.font.wg_sl - 3
     pdefs.hardset.font.family =  ffam[tsh, tff]
     pdefs.hardset.font.wg_sl = fshp[tsh, tff]
  endif

  for j =  0, pdefs.ntext-1 do begin
     if (pdefs.text[j].font ge 3) then begin
        tff = pdefs.text[j].ffamily
        tsh = pdefs.text[j].font
        pdefs.text[j].ffamily =  ffam[tsh, tff]
        pdefs.text[j].font = fshp[tsh, tff]
     endif
  endfor

  if (pdefs.text_options.font ge 3) then begin
     tff = pdefs.text_options.ffamily
     tsh = pdefs.text_options.font
     pdefs.text_options.ffamily =  ffam[tsh, tff]
     pdefs.text_options.font = fshp[tsh, tff]
  endif

end
