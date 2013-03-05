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

pro Gr_rc_get, optblock

;+
; GR_RC_GET
;	Read the user's .grafferrc file.
;
; Usage:
;	gr_rc_get, opts
;
; Argument:
;	opts	struct	output	The graffer options sub-structure.
;
; Note:
;	This is a procedure rather than a function as functions tend
;	to croak if asked to return undefined values.
;
; History:
;	Extracted from GRAFF_INIT: 21/8/97; SJT
;	Eliminate obsolete findfile call: 16/4/12; SJT
;-


  home = getenv('HOME')
  if strpos(home, path_sep(), /reverse_search) ne strlen(home)-1 then $
     rcfile = home+path_sep()+'.grafferrc' $
  else  rcfile = home+'.grafferrc'

  if ~file_test(rcfile) then return

  optblock = {graff_opts}
  optblock.Auto_delay = 300.
  optblock.Mouse = 0b

  openr, ilu, rcfile(0), /get
  inln = ''
  while not eof(ilu) do begin
     readf, ilu, inln
     kv = str_sep(inln, ':')
     case kv(0) of
        'Autosave': optblock.auto_delay = float(kv(1))
        'Supp2D': optblock.s2d = fix(kv(1))
        'MouseEdit': optblock.mouse = fix(kv(1))
        'PDFView': optblock.pdfviewer = kv[1]
        'ColourMenu': optblock.colour_menu = fix(kv[1])
        Else: print, "Warning: Unknown item in resource file"
     endcase
  endwhile
  free_lun, ilu

end
