; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

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
;	Remove colour_menu altogether: 21/5/20; SJT
;	Add tracking events control, make RC file case insensitive:
;	18/8/21; SJT
;-


  home = getenv('HOME')
  if strpos(home, path_sep(), /reverse_search) ne strlen(home)-1 then $
     rcfile = home+path_sep()+'.grafferrc' $
  else  rcfile = home+'.grafferrc'

  if ~file_test(rcfile) then return

  optblock = {graff_opts}
  optblock.Auto_delay = 300.
  optblock.Mouse = 0b
  optblock.track = 1b
  optblock.bitmaps = 0b
  
  if ~file_test(rcfile) then return

  openr, ilu, rcfile(0), /get
  inln = ''
  while not eof(ilu) do begin
     readf, ilu, inln
     kv = strsplit(inln, ':', /extr)
     case strupcase(kv(0)) of
        'AUTOSAVE': optblock.auto_delay = float(kv[1])
        'SUPP2D': optblock.s2d = truth(kv[1])
        'MOUSEEDIT': optblock.mouse = truth(kv[1])
        'PDFVIEW': optblock.pdfviewer = kv[1]
        'TRACK': optblock.track = fix(kv[1])
        'BITMAP': optblock.bitmaps = truth(kv[1])
        Else: print, "Warning: Unknown item in resource file."
     endcase
  endwhile
  free_lun, ilu

end
