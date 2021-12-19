; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_check_box, nx, ny, xbm = xbm

;+
; GR_CHECK_BOX
;	Read the bitmap for the checkbox image.
;
; Usage:
;	bitmap=gr_check_box(nx,ny)
;
; Return Value:
;	bitmap	byte	The bitmap
;
; Arguments:
;	nx, ny	int	output	The size of the bitmap
;
; Keyword:
; 	xbm	int	output	The number of extra bits to ignore.
; History:
;	Original: 29/1/97; SJT
;	Just return the bitmap: 20/2/97; SJT
;	Tidy: 12/1/12; SJT
;- 

  cstack = scope_traceback(/struct)
  bmpath = file_dirname(cstack[-1].filename, /mark) + $
           path_sep(/parent) + path_sep() + 'bitmaps' + path_sep()

  read_x11_bitmap, bmpath+'3floppy_unmount.xbm', fd3, nx, ny

  if arg_present(xbm) then begin
     xbm = nx mod 8
     if xbm ne 0 then xbm = 8-xbm
  endif
  
  return, fd3
end


