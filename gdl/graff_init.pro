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

pro Graff_init, pdefs, file, version = version

;+
; GRAFF_INIT
;	Set up a new graffer plot structure
;
; Usage:
;	graff_init, pdefs
;
; Argument:
;	pdefs	struct	output	The graffer plot structure.
;	file	string	input	The filename for the new plot.
;
; Keywords:
;	version	int	input	The graffer version 2-elements, major
;				& minor version numbers, if pdefs exists,
;				then previous version is retained.
;
; Note:
;	If pdefs exists, it is assumed to be a pre-existing pdefs
;	structure and its associated handles are released.
;
; History:
;	Extracted from GRAFFER: 18/8/95; SJT
;	Axis style IDS added: 17/1/97; SJT
;	Modify for extended symbol definitions: 20/1/97; SJT
;	Drop CDF support: 10/2/97; SJT
;	Add flag for short colour table: 8/5/97; SJT
;	Add flag for "single point" key format: 15/5/97; SJT
;	Add REM(arks) field: 23/6/97; SJT
;	Several bug fixes to .grafferrc handling: 24/7/97; SJT
;	Add mouse-editing default option: 13/8/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Make colour PS the default: 14/2/12; SJT
;	Remove opts field from PDEFS: 21/5/20; SJT
;-

  if (n_elements(pdefs) ne 0) then begin
     version = pdefs.version
     idblock = pdefs.ids
     hblock = pdefs.hardset
     topid = pdefs.ids.graffer
     dir = pdefs.dir
     ds_dir = pdefs.ds_dir
     graff_clear, pdefs

  endif else gr_rc_get, optblock

  if (n_elements(file) eq 0) then begin
     fc = ''
     if (n_elements(dir) eq 0) then cd, curr = dir
  endif else begin
     fc = file
     gr_split_dir, fc, dir
     dir = gr_get_full_dir(dir)
     
  endelse

  if (n_elements(ds_dir) eq 0) then ds_dir = dir
  pdefs = {graff_define}
  pdefs.Version =   version
  pdefs.Name =      fc
  pdefs.Dir =       dir
  pdefs.Charsize =  1.0
  pdefs.Axthick =   1.
  pdefs.Xrange =    dindgen(2)
  pdefs.Yrange =    dindgen(2)
  pdefs.Yrange_r =  dindgen(2)
  pdefs.Gamma =     1.0
  pdefs.Nsets =     1

  pdefs.text_options.Colour = 1
  pdefs.text_options.Size =   1.0
  pdefs.text_options.Font =   3
  pdefs.text_options.Thick =  1.

  pdefs.key.Norm =     2
  pdefs.key.cols =     1

  pdefs.transient.Imove =   -1l
  pdefs.transient.hairs = 1b

  pdefs.Ds_dir =       ds_dir

; Set defaults for these hardcopy actions, but they will be
; overwritten if an old file existed.

  pdefs.hardset.Action = ['lp ', '']
; pdefs.hardset.viewer = [gr_find_viewer(/ps), ' &']
  pdefs.hardset.Size = [23., 18.]
  pdefs.hardset.Off = [3.35, 1.5]

  if (n_elements(idblock) ne 0) then pdefs.ids = idblock
  if (n_elements(hblock) ne 0) then pdefs.hardset = hblock

; Set the defaults for those hardcopy settings that we probably
; don't want to inherit these to a new file

  pdefs.hardset.font.family = 1
  pdefs.hardset.font.wg_sl = 1
  pdefs.hardset.colour = 1
  pdefs.hardset.name = ''

  data_str = gr_new_ds(pdefs)

  pdefs.data = ptr_new(data_str)


  pdefs.text = ptr_new({graff_text})
  (*pdefs.text).Colour = 1
  (*pdefs.text).Size = 1.0
  (*pdefs.text).Font = 1
  (*pdefs.text).Thick = 1.

end
