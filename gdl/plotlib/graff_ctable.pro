; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro graff_ctable, table, cmap, gamma = gamma

;+
; GRAFF_CTABLE
;	Obtain full colour table (c.f. GRAFF_COLOURS) for a 2-D
;	dataset. 
;
; Usage:
;	graff_ctable, table, cmap
;
; Argument:
;	table	int	input	The table index to load.
;	cmap	byte	output	The colour map
;
; Keyword:
;	gamma	float	input	An optional gamma setting.
;
; Side effects:
;	No longer applicable
;
; History:
;	Original: 17/11/11; SJT
;	Restructure colour handling: 16/5/16; SJT
;	Replace with use of loadct & rgb_table key: 12/8/21; SJT
;-

  loadct, table, rgb_table = cmap

  if keyword_set(gamma) && gamma ne 1.0 then begin
     s = long(256*((findgen(256)/256.)^gamma))
     cmap = cmap[s, *]
  endif

  
end
