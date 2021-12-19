; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_psym_bitm                ;, ncols

;+
; GR_PSYM_BITM
;	Define the bitmaps for the PSYM menu.
;
; Usage:
;	gr_psym_bitm, ncols
;
; Argument:
;	ncols	int	The number of discrete colour defined.
;
; History:
;	Original: 1/8/95; SJT
;	Add byte-swap for little-endian machines: 7/8/96; SJT -
;	following a suggestion from Christian Marquardt.
;	Rename as GR_PSYM_BITM (was psym_bitmaps): 18/9/96; SJT
;	Make a user-defined disk symbol & remove connected dots:
;	16/1/97; SJT
;	Entirely rewritten to use new extended symbol range and to
;	read the bitmaps from the bitmap files: 20/1/97; SJT
;	Add the colour bitmaps as well: 5/7/05; SJT
;	Add number of colours: 8/2/12; SJT
;	Add bars: 25/5/17; SJT
;-

  common Gr_psym_maps, psym_bm, col_bm

  cstack = scope_traceback(/struct)
  bmpath = file_dirname(cstack[-1].filename, /mark) + $
           path_sep(/parent) + path_sep() + 'bitmaps' + path_sep()

  read_x11_bitmap, bmpath+'ps0.xbm', ps0
  read_x11_bitmap, bmpath+'ps1.xbm', ps1
  read_x11_bitmap, bmpath+'ps2.xbm', ps2
  read_x11_bitmap, bmpath+'ps3.xbm', ps3
  read_x11_bitmap, bmpath+'ps4.xbm', ps4
  read_x11_bitmap, bmpath+'ps5.xbm', ps5
  read_x11_bitmap, bmpath+'ps6.xbm', ps6
  read_x11_bitmap, bmpath+'ps7.xbm', ps7
  read_x11_bitmap, bmpath+'ps8.xbm', ps8
  read_x11_bitmap, bmpath+'ps9.xbm', ps9
  read_x11_bitmap, bmpath+'ps10.xbm', ps10
  read_x11_bitmap, bmpath+'ps11.xbm', ps11
  read_x11_bitmap, bmpath+'ps12.xbm', ps12
  read_x11_bitmap, bmpath+'ps13.xbm', ps13
  read_x11_bitmap, bmpath+'ps14.xbm', ps14
  read_x11_bitmap, bmpath+'ps15.xbm', ps15
  read_x11_bitmap, bmpath+'ps16.xbm', ps16
  read_x11_bitmap, bmpath+'ps17.xbm', ps17
  read_x11_bitmap, bmpath+'ps18.xbm', ps18

  psym_bm = [[[ps0]], [[ps1]], [[ps2]], [[ps3]], [[ps4]], [[ps5]], $
             [[ps6]], [[ps7]], [[ps8]], [[ps9]], [[ps10]], [[ps11]], $
             [[ps12]], [[ps13]], [[ps14]], [[ps15]], [[ps16]], $
             [[ps17]], [[ps18]]]

  read_x11_bitmap, bmpath+'omit.xbm', omit
  read_x11_bitmap, bmpath+'custom.xbm', custom

  col_bm = [[[omit]], [[custom]]]
  
end

