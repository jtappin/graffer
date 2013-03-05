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

function graff_open, file, version, ascii, name = name, dir = $
                     dir, time = time

;+
; GR_GET_HDR
; 	Get the header record of a graffer file
;
; Usage:
;	status=gr_get_hdr(ilu, version, ascii)
;
; Returns:
;	A unit number if the file is a Graffer file, 0 if it isn't
;
; Arguments:
;	file	string	input	The file to open.
;	version	int	output	The major & minor version numbers of
;				the graffer that wrote the file.
;	ascii	bool	output	Whether the file is ascii (1) or
;				binary (0)
;
; Keywords:
;	name	string	output	The original name of the file
;	dir	string	output	The original directory of the file
;	time	string	output	The time of writing of the file.
;
; Notes:
;	The file will be opened with a byteswap flag if it is found to
;	be a binary file in non-native byte order.
;	The name, dir & time keys are not set for ASCII files.
;
; History:
;	Original (from various sources): 6/1/12; SJT
;-

on_ioerror, no_file
openr, ilu, /get, file
on_ioerror, null

rs = bytarr(7)
readu, ilu, rs                  ; read the overall header
rs = string(rs)

case (rs) of
    "GRAFFER": begin
        ascii = 0b
        version = intarr(2)
        readu, ilu, version
        if version[0] gt 255 then begin ; File likely swapped
            swap_endian_inplace, version
            if version[0] gt 255 then begin ;
                message, /continue, "File version is invalid"
                return, 0l
            endif
            free_lun, ilu
            openr, ilu, /get, file, /swap_endian
            point_lun, ilu, 11
        endif
        
        dir = gr_str_rd(ilu)
        name = gr_str_rd(ilu)
        time = gr_str_rd(ilu)

        return, ilu
    end

    "Graffer": begin
        ascii = 1b
        point_lun, ilu, 0

        inln = ''
        readf, ilu, inln
        vp = strpos(inln, 'V')+1
        file_v = float(strmid(inln, vp, 5))
        version = [fix(file_v), fix((100*file_v) mod 100)]
                                ; Note that since the name, directory
                                ; and time are not actually used, we
                                ; don't bother to decode them in ASCII
                                ; mode.
        return, ilu
    end

    else: begin
        message, /continue, "File is not a Graffer file"
        return, 0l
    end
endcase

; Should never get here
print, "This isn't possible"

no_file:
message, /continue, "Failed to open file"
return, 0l

end
