; LICENCE:
; Copyright (C) 2022: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.

;+
; GR_N_ERRORS
;	Return the number of error columns for a given DS type
;
; Usage:
;	nerr = gr_n_errors(type)
;
; Returns:
;	The number of error bars. A 2-D array [Nx, Ny] unless /x or /y
;	is given.  
;
;
; Argument:
;	type	int	The dataset type.
;
; Keywords:
;	/x	If set, then only return the number of X error bars
;	/y	If set, then only return the number of Y error bars.
;
; History:
;	Original: 15/4/21; SJT
;-

function gr_n_errors, type, x = x, y = y

  if type lt 0 || type ge 9 then begin
     if keyword_set(x) || keyword_set(y) then return, 0l $
     else return, lonarr(2)
  endif

  nxe = [0l, 0l, 0l, 1l, 2l, 1l, 1l, 2l, 2l]
  nye = [0l, 1l, 2l, 0l, 0l, 1l, 2l, 1l, 2l]

  if keyword_set(x) then return nxe[type] 
  if keyword_set(y) then return, nye[type]
  return, [nxe[type], nye[type]]

end
