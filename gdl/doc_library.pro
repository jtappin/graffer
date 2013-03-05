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

;+
; NAME:
;	DOC_LIBRARY
;
; PURPOSE:
;	Extract and display documentation headers from a program or routine.
;
; CATEGORY:
;	Documentation
;
; CALLING SEQUENCE:
;	doc_library, procedure
;
; INPUTS:
;	procedure	string	The procedure to document.
;
; KEYWORD PARAMETERS:
;	/print	Set to print the output to the default printer.
;
; SIDE EFFECTS:
;	A file is created in /tmp and deleted after use.
;
; RESTRICTIONS:
;	Only one documentation block per file is handled.
;
; EXAMPLE:
;	doc_library, 'doc_library'
;
; MODIFICATION HISTORY:
;	Original: 28/2/13; SJT
;-
pro doc_library, proc, print = print

  on_error, 2

  if (!version.os_family ne 'unix') then begin
     print,  "DOC_LIBRARY is currently only available for Unix like " + $
             "systems"
     return
  endif

  if (keyword_set(print)) then begin
     less =  file_which(getenv('PATH'), 'lp')
     if (less eq '') then less = file_which(getenv('PATH'), 'lpr')
     if (less eq '') then begin
        print, "Neither lp nor lpr was found"
        return
     endif
  endif else begin
     less = file_which(getenv('PATH'), 'less')
     if (less eq '') then less = file_which(getenv('PATH'), 'more')
     if (less eq '') then begin
        print, "Neither more nor less was found"
        return
     endif
  endelse

  proc_path = file_which(proc+'.pro', /include_current)
  if (proc_path eq '') then begin
     print, proc, ' not found'
     return
  endif

  out_name = '/tmp/'+proc+'.txt'

  openr, ipu, proc_path, /get
  dflag = 0b
  inln = ''

  openw, isu, out_name, /get

  while (~eof(ipu)) do begin
     readf, ipu, inln
     inln = strtrim(inln, 2)
     if (strpos(inln, ';+') eq 0) then dflag = 1b
     if (strpos(inln, ';-') eq 0) then break

     if dflag then printf, isu, strmid(inln, 1)
  endwhile

  free_lun,  isu,  ipu

  spawn, less+' '+out_name

  file_delete, out_name
end
