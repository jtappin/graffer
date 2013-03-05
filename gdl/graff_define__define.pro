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

pro graff_define__define
;+
; NAME:
;	graff_define__define
;
;
; PURPOSE:
;	Define the main graffer structure
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
;	Add isotropic option: 25/6/08; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;-
pdefs = {graff_define, $
         Version:   intarr(2), $
         Name:      '', $
         Dir:       '', $
         Title:     '', $
         Subtitle:  '', $
         Charsize:  .0, $
         Axthick:   0., $
         Position:  fltarr(4), $
         Aspect:    fltarr(2), $
         Isotropic: 0b, $
         match:     0b, $
         Xrange:    dblarr(2), $
         Xtitle:    '', $
         Xtype:     0, $
         Xsty:      {graff_style}, $
         Yrange:    dblarr(2), $
         Ytitle:    '', $
         Ytype:     0, $
         Ysty:      {graff_style}, $
         y_right:   0b, $
         Yrange_r:  dblarr(2), $
         Ytitle_r:  '', $
         Ytype_r:   0, $
         Ysty_r:    {graff_style}, $
         ytransform: replicate({!axis}, 2), $
         Ctable:    0, $
         Gamma:     .0, $
         Nsets:     0, $
         Cset:      0, $ $
         Data:      ptr_new(), $
         Ntext:     0, $
         Text:      ptr_new(), $
         Text_options: {graff_text}, $
         Key: {graff_key}, $
         Remarks:   ptr_new(), $
         Ids: { graff_ids}, $
         Hardset: { graff_hard}, $
         Transient: { graff_trans}, $
         Opts: {graff_opts}, $
         Ds_dir:       '', $
         Chflag:       0b, $
         Short_colour: 0b, $
         is_ascii:     0b $
        }

end
