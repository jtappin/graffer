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

pro graff_data__define
;+
; NAME:
;	graff_data__define
;
;
; PURPOSE:
;	Create a GRAFFER dataset structure
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	data={graff_data}
;
;
; MODIFICATION HISTORY:
;	Extracted: 30/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;-

data =  {graff_data, $
         Ndata:    0l, $
         Ndata2:   0l, $        ; Only used in 2D Dss
         Type:     0, $
         Mode:     0, $
         Xydata:   ptr_new(), $
         Descript: "", $
         Pline:    0, $
         Psym:     0, $
         Symsize:  0., $
         Line:     0, $
         Colour:   0, $
         Thick:    0., $
         Zopts:    {graff_2d}, $
         y_axis:   0, $
         Sort:     0b, $
         Noclip:   0b, $
         Medit:    0b}
end
