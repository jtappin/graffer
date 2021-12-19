; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_present

;+
; GR_PRESENT
;	Determine if there is already a graffer widget present in this
;	session 
;
; Usage:
;	is_gr = gr_present()
;
; Returns:
;	0 if no graffer widget already exists, the id of the widget if it does.
;
; History:
;	Original: 23/3/12; SJT
;-

; If no widgets cannot be a graffer widget present
  if ~widget_info(/active) then return, 0l

; Find how many widgets there can be. If base would be 1 then we will
; already have returned.

  base = widget_base()
  widget_control, base, /destroy
  ids = lindgen(base-1)+1
  
; Determine which really exist (if none then return false)

  isval = widget_info(ids, /valid)
  locs = where(isval, nvw)
  if nvw eq 0 then return, 0l
  ids = ids[locs]

; Find which are top-level

  pw = widget_info(ids, /parent)
  locs = where(pw eq 0)
  ids = ids[locs]               ; Must always be at least 1.

; Find which of those have a uname of "GRAFFER"

  gwids = widget_info(ids, find_by_uname = 'GRAFFER')
  locs = where(gwids ne 0, ngr)
  if ngr eq 0 then return, 0l
  return, gwids[locs[0]]

end
