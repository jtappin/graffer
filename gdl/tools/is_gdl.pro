; LICENCE:
; Copyright (C) 2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; IS_GDL
;	Are we running GDL or IDL?
;
; Usage:
;	gdl = is_gdl()
;
; Returns:
;	1 if runnning GDL (the !GDL system variable is present), 0
;	otherwise.
;
; Note:
;	If you do something like defining a !GDL system variable in
;	IDL the routine will get the wrong answer.
; 
; History:
;	Original: 18/8/21; SJT
;-

function is_gdl

  defsysv, '!gdl', exist = rv
  return, rv

end
