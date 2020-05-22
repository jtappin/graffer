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

;+
; GR_CONT_COL_GET
;	Convert a string describing a set of contour colours to a
;	list.
;
; Usage:
;	gr_cont_col_get, cstr, cindex, rawc
;
; Arguments:
;	cstr	string	An array of contour colours, either a single
;			integer value or 3 integers separated by
;			spaces and/or commas.
;	cindex	int	A variable to return the indexed colours (or
;			-2 for a raw colour).
;	rawc	int	A variable to return the raw (decomposed)
;			colours.
;
; Keyword:
;	status	int	A variable that will be set to 1 if the
;			conversion was successful, and 0 otherwise.
;
; History:
;	Original: 7/10/16; SJT
;	Switch to storing the colours as a LONG array. 1/4/20; SJT
;	Now a procedure returning a colour intex array and a raw
;	colour array: 3/4/20; SJT
;-

pro gr_cont_col_get, cstr, cindex, rawc, status = status
  ncols = n_elements(cstr)

  cindex = intarr(ncols)
  rawc = intarr(3, ncols)
  
  on_ioerror, fail

  status = 1
  
  for j = 0, ncols-1 do begin
     tmp = strsplit(cstr[j], '	 ,', /extr, count = nn)
     case nn of
        0:                      ; Skip blank lines
        1: cindex[j] = fix(tmp[0])
        3: begin
           rawc[*, j] = fix(tmp)
           cindex[j] = -2
        end
        else: begin
           status = 0
           return
        end
     endcase
  endfor

  return
  
fail:
  status = 0
  return

end
