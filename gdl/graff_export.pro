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

pro graff_export, file, index, name = name, outfile = outfile

;+
; GRAFF_EXPORT
;	Write a graffer dataset to a file (programatically).
;
; Usage:
;	graff_export, file[, index]
;
; Arguments:
;	file	string	The graffer file.
;	index	int	The index of the dataset to write.
;
; Keywords:
;	name	string	The descriptor of the dataset to write.
;	outfile	string	The file to which to write the dataset
;
; Notes:
;	If neither the index or the name is given, then the current
;	dataset is written.
;	Specifying both is an error.
;	If name is given then (1) If there are 2 datasets of the same
;	name, the first will be written, (2) if the name is not found,
;	then the program exits without writing.
;
; History:
;	Original: 25/1/12; SJT
;-

on_error, 2                     ; Return to caller on error

if ~file_test(file) then message, "File does not exist"
gr_state, /save

;	Open the file

@graff_version

f0 = file
graff_init, pdefs, f0, version = version
igot = graff_get(pdefs, f0, /no_set, /no_warn)
if igot ne 1 then begin
   message, "Failed to open: "+f0
   return
endif

if n_params() eq 2 then begin
    if keyword_set(name) then begin
        message, /continue, $
          "May not specify dataset by index and by name"
        return
    endif
    idx = index-1 
endif else if keyword_set(name) then begin
    locs = where((*pdefs.data).descript eq name, nname)
    if nname eq 0 then begin
        message, /continue, "No match for name "+name+" found in "+file
        return
    endif

    if nname gt 1 then message, /continue, $
      "Multiple matches for name "+name+" found in "+file
    idx = locs[0]
endif else index = pdefs.cset

if ~keyword_set(outfile) then $
  outfile = strmid(pdefs.name, 0, strpos(pdefs.name, '.', $
                                         /reverse_search))+ $
  string(idx+1, format = "('_',I0,'.dat')")

gr_write_ds, pdefs, outfile, index = idx

graff_clear, pdefs
gr_state

end
