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

pro graff_2d__define
;+
; NAME:
;	graff_2d__define
;
;
; PURPOSE:
;	Create a 2-d dataset settings structure for graffer
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
;	Support colour inversion: 26/6/07; SJT
;	Add local colour table option: 17/11/11; SJT
;-

Zopts = {graff_2d, $
         Format:      0, $
         set_levels:  0b, $
         N_levels:    0, $
         lmap:        0, $ 
         Levels:      ptr_new(), $
         N_cols:      0, $ 
         Colours:     list(), $
         N_sty:       0, $
         Style:       ptr_new(), $
         N_thick:     0, $
         Thick:       ptr_new(), $
         Range:       dblarr(2), $
         missing:     0.d0, $
         Pxsize:      0.d0, $
         charsize:    0.d0, $
         Label:       0, $
         Label_off:   0, $
         Ctable:      0, $
         Gamma:       0.d0, $
         Fill:        0b, $
         ilog:        0b, $
         invert:      0b $
        }

end
