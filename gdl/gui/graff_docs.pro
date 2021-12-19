; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; NAME:
;	GRAFF_DOCS
;
;
; PURPOSE:
;	Find a suitable application and show the GRAFF PDF
;	documentation.
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	graff_docs
;
;
;
; ARGUMENT:
; 	pdefs	struct	The GRAFFER master structure.
;
;
;
; KEYWORD PARAMETERS:
;	/file_format	If set, then show the file format description
;			rather than the users' guide.
;
;
; MODIFICATION HISTORY:
;	Original (after SMEI_DOC): 5/7/05; SJT
;	Move top level options out of PDEFS: 21/5/20; SJT
;-
 
pro graff_docs, pdefs, file_format = file_format

  common gr_docs_common, pdfutil, docpath
  common graffer_options, optblock

  if n_elements(pdfutil) eq 0 then begin
     if optblock.pdfviewer ne '' then pdfutil = optblock.pdfviewer $
     else begin
        pdfutil = gr_find_viewer(/pdf)

        if pdfutil eq '' then begin
           graff_msg, pdefs.ids.msgid, $
                      ['No PDF display application found. Please ' + $
                       'contact', $
                       'your system administrator or specify a viewer ' + $
                       'manually.']
           return
        endif
     endelse
  endif

  if n_elements(docpath) eq 0 then begin
     rpath = routine_info('graffer', /source)
     docpath = file_dirname(rpath.path, /mark) + 'Docs'+path_sep()
  endif

  if keyword_set(file_format) then file = 'Format.pdf' $
  else file = 'Graffer.pdf'

  spawn, pdfutil+' '+docpath+file+' &'

end
