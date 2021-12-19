; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Grf_tlv_get, vn, n_var

;+
; GRF_TLV_GET
;	Get a variable from the top level.
;
; Usage:
;	var = grf_tlv_get(vn, n_var)
;
; Return value:
;	var	any	The value or array of values returned from the
;			top level.
;
; Arguments:
;	VN	string	input	the variable name to get including
;				possible slice information 
; 	N_VAR 	int	output	the number of elements returned, this
; 				will be zero if the variable is not
; 				found or if it cannot be converted to
; 				a numeric type (N.B. Strings are
; 				considered non-convertible without
; 				attempting to do a conversion).
;
; Restrictions:
;	Subscript expressions must be constants, e.g. "[*,0]" or
;	"[0:18]".
;
; History:
;	Original: 21/9/95; SJT
;	Moved to its own file to allow use in multiple input routines:
;	6/12/96; SJT
;	Handle structure elements and [ subscripts: 21/4/04; SJT
;	Replace undocumented ROUTINE_NAMES with supported
;	SCOPE_VARNAMES and SCOPE_VARFETCH: 3/2/12; SJT
;	Add level processing: 6/2/12; SJT
;-

; Check if there is a level specifier. If there is interpret it.
  pc = strpos(vn, '\')
  if pc eq -1 then begin
     vname = vn
     level = 1
  endif else begin
     level = long(strmid(vn, 0, pc))
     vname = strmid(vn, pc+1)
  endelse

; Check for slide specifiers and separate them from the name.

  separators = ['.', '[', '(']

  sflag = 0b
  bp = -1
  for j = 0, n_elements(separators)-1 do begin
     bps = strpos(vname, separators[j])
     if (bps ge 0) then begin
        if bp eq -1 then bp = bps $
        else bp = bp < bps
     endif
  endfor
  if bp eq -1 then begin
     var = strupcase(vname)
  endif else begin
     var = strupcase(strmid(vname, 0, bp))
     slice = strmid(vname, bp)
  endelse

  vbls = scope_varname(level = level) ; Get all top-level variables.

  locs = where(vbls eq var, nv) ; Check that requested variable exists
  if nv eq 0 || n_elements(scope_varfetch(var, level = level)) eq 0 then begin
     n_var = 0
     return, -1l
  endif

  v = scope_varfetch(var, level = level) ; Retrieve the variable

  sv = size(v)
  svtype = sv[sv[0]+1]
  while svtype eq 10 do begin   ; Pointer (de-reference it)
     v1 = *v
     v = temporary(v1)
     sv = size(v)
     svtype = sv[sv[0]+1]
  endwhile

  if (svtype eq 0 || svtype eq 7 || svtype eq 11 || $
      (svtype eq 8 && n_elements(slice) eq 0)) then begin ; Non-numeric
     n_var = 0
     return, -1l
  endif

; If there is a slice, extract it and perform sanity checks on the
; result.

  if (n_elements(slice) gt 0) then begin
     v1 = 0                     ; EXECUTE can't create a variable.
     r = execute('v1 = v'+slice)
     if (r ne 1) then begin
        n_var = 0
        return, -1l
     endif

     sv = size(v1)
     svtype = sv[sv[0]+1]
     while svtype eq 10 do begin ; Pointer (de-reference it)
        v2 = *v1
        v1 = temporary(v2)
        sv = size(v1)
        svtype = sv[sv[0]+1]
     endwhile

     if (svtype eq 0 or svtype eq 7 or svtype eq 11 or svtype eq 8) $
     then begin                 ; Non-numeric 
        n_var = 0
        return, -1l
     endif

  endif else v1 = v

  n_var = n_elements(v1)
  return, reform(v1)

end

