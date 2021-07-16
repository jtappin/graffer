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

pro graff_key__define
;+
; NAME:
;	graff_text__define
;
;
; PURPOSE:
;	Define the graffer text style structure
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
;	Add char size: 29/4/09; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;-

Key = {graff_key, $
       X:         dblarr(2),  $
       Y:         dblarr(2), $
       csize:     0.d0, $
       Norm:      0, $ 
       cols:      0, $
       List:      ptr_new(),  $
       Frame:     0b, $
       One_point: 0b, $
       reverse:   0b, $
       Use:       0b, $
       side:      0b, $
       title: '' $
      }

end
