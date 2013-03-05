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

pro graff_trans__define
;+
; NAME:
;	graff_hard__define
;
;
; PURPOSE:
;	Define the graffer transient properties structure
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
;	Add current only option: 26/1/12; SJT
;	Add number of discrete colours: 8/2/12; SJT
;-

Transient = { graff_trans, $
              Opos:     dblarr(2), $
              Imove:    0l, $
              mode:     0, $
              Changes:  0, $
              colmin:   0, $
              hairs:    0b, $
              Opflag:   0b, $
              backup:   0b, $
              current_only: 0b $
            }

end
