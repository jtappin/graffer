; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Graff_get, pdefs, f, no_set = no_set, recover = recover, $
                    no_warn = no_warn, previous_name = previous_name

;+
; GRAFF_GET
;	Get a graffer dataset from a file
;
; Usage:
;	iopen = graff_get(pdefs, f)
;
; Return Value:
;	iopen	int	1 If file was opened; 0 on failure, 2 on file
;			not found.
;
; Argument:
;	pdefs	struct	in/out	The graffer data structure.
;	f	string	input	The file to read.
;
; Keywords:
;	no_set	input	If set, then don't try to set up the widget
;			values (because the widgets aren't there)
;	recover input	If set, then try to recover the file from the
;			autosave file
;	no_warn	input	If set, then don't print a warning message if
;			file is not opened sucessfully.
;	previous_name string	input	The name of the last file opened.
;
; History:
;	Original: 16/8/95; SJT
;	Remove file argument (use pdefs.name): 17/8/95; SJT
;	Add facilities for string-type data and restore filename: 18/8/95; SJT
;	Change to a function returning 0 or 1 & add NO_WARN key: 11/6/96; SJT
;	Reduce to a wrapper for an ASCII and a binary mode reader:
;	14/1/96; SJT
;	Do initialization of graffer structure here rather than at the
;	higher level (to allow cancel to work on restore): 20/5/10; SJT
;	Move initialization until after file has been successfully
;	opened: 16/3/12; SJT
;	Eliminate obsolete findfile call: 16/4/12; SJT
;	Handle TT font option: 12/2/20; SJT
;	Remove unused single argument case: 4/11/21; SJT
;-

if ~file_test(f) then return, 2

gr_split_dir, f, dir            ; Split the name and directory after
                                ; opening


recname = dir+'#'+f+'#'
fname = dir+f
irecf = file_test(recname)

if irecf && ~keyword_set(recover) then begin
    finfo = file_info(fname)
    rinfo = file_info(recname)
    if rinfo.mtime gt finfo.mtime then begin
        msg = ['There is an autosave file present',  $
               'which looks to be more recent than the',  $
               'regular file.',  $
               'Do you want to open it?']
        case dialog_message(msg, $
                            /cancel, $
                            dialog_parent = pdefs.ids.graffer, $
                            /question, $
                            title = 'GRAFFER Autosave', $
                            resource = 'Graffer') of  
            'Cancel': icont = -1
            'Yes': icont = 1
            'No': icont = 0
        endcase
    endif else icont = 0
endif else if ~irecf && keyword_set(recover) then begin
    msg = ['No autosave file found', $
           'Cannot do /RECOVER', $
           'Open the original file?']
    case dialog_message(msg, $
                        /question, $
                        dialog_parent = pdefs.ids.graffer,  $
                        title = 'GRAFFER Autosave', $
                        resource = 'Graffer') of
        'Yes': icont = 1
        'No': icont = -1
    endcase
endif else icont = keyword_set(recover)

case icont of
    0: ilu = graff_open(fname, fvers, ascii)
    1: begin
        if ~keyword_set(no_warn) then  $
          graff_msg, pdefs.ids.message, 'Opening autosave file'
        ilu = graff_open(recname, fvers, ascii)
    end
    -1: begin
        if ~keyword_set(no_warn) then  $
          graff_msg, pdefs.ids.message, ['Failed to open file: '+dir+f, $
                                         'User specified cancel']
        return, 0
    end
endcase

if ilu eq 0 then begin
    graff_msg, pdefs.ids.message, 'Failed to open file: '+dir+f
    return, 0
endif

if ~pdefs.chflag then gr_auto_delete, pdefs
graff_init, pdefs
pdefs.name = f
pdefs.dir = dir

if ascii then begin
    if fvers[0] eq 1 then $
      gr_get_v1, pdefs, ilu, fvers $
    else gr_get_asc, pdefs, ilu, no_set = no_set
endif else begin
    if fvers[0] ge 4 then $
      gr_get_bin, pdefs, ilu, no_set = no_set $ 
    else gr_get_bin_v3, pdefs, ilu, fvers, no_set = no_set
endelse

if pdefs.fontopt eq 1 then !p.font = 1 $
else !p.font = -1

if icont eq -1 || fvers[0] eq 1 then graff_save, pdefs

return, 1

end
