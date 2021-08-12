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

pro graff_text__define
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
;	Add axis choice: 28/12/11; SJT
;	Add ID: 13/6/12; SJT
;-

Text_options = {graff_text, $
                id: '', $
                Text:    '', $
                Colour:  0, $
                Size:    0., $
                Orient:  0., $
                Align:   0., $
                ffamily: 0, $
                Font:    0, $
                Thick:   0., $
                X:       0.d0, $
                Y:       0.d0, $
                Norm:    0b, $
                axis:    0 $
               }

end
