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

pro graff_style__define
;+
; NAME:
;	graff_style__define
;
;
; PURPOSE:
;	Define the graffer axis style structure
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
;	Advanced axis style settings: 21/8/12; SJT
;	Remove Xmajor field: unused: 25/3/20; SJT
;-

Xsty = {graff_style, $
        Idl:     0, $
        Extra:   0, $
        Grid:    0, $
        Time:    0, $
        Tzero:   0l, $
        minor:   0, $
        major:   0, $
        format:  '', $
        values:  ptr_new()}

end
