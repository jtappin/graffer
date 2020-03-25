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

pro graff_hard__define
;+
; NAME:
;	graff_hard__define
;
;
; PURPOSE:
;	Define the graffer hardcopy style structure
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	implicit
;
;
; MODIFICATION HISTORY:
;	Extracted: 30/6/05; SJT
;	Add options for plplot drivers: 29/11/13; SJT
;	Add PDF viewer: 21/9/16; SJT
;-

Hardset = { graff_hard, $
            Colour: 0b, $
            Eps:    0b, $
            Orient: 0b, $
            Psize:  0b, $
            Timestamp: 0b, $
            cmyk: 0b, $
            Font:   {graff_hfont,  $
                     Family: 0, $
                     Wg_sl:  0}, $
            Size:   dblarr(2), $
            Off:    dblarr(2), $
            Action: strarr(2), $
            viewer: strarr(2), $
            pdfviewer: strarr(2), $
            psdev: '', $
            epsdev: '', $
            pdfdev: '', $
            svgdev: '', $
            name: '' $
          }

end
