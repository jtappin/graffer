; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_find_viewer, pdf = pdf, ps = ps, all = all, count = count

;+
; GR_FIND_VIEWER
;	Find a suitable viewer for PDF or PostScript documents.
;
; Usage:
;	app = gr_find_viewer()
;
; Returns:
;	A suitable app to view the document or a list of those available.
;
; Keywords:
;	/pdf	If set, find app(s) for PDF files [Default]
;	/ps	If set, find app(s) for PS files.
;	/all	If set, return all found apps.
;	count	A named variable to return the nummber of apps found.
;
; Notes:
;	Uses a hard-coded list of candidates.
;
; History:
;	Extracted from gr_opt_set: 15/2/12; SJT
;-

  pdfapps = ['acroread', $
             'okular', $
             'evince', $
             'gv', $
             'kpdf', $
             'xpdf', $
             'kghostview']

  psapps = ['okular', $
            'evince', $
            'gv', $
            'kghostview', $
            'ghostview']

  if keyword_set(ps) then applist = psapps $
  else applist = pdfapps

  isapp = gr_find_program(applist)

  locs = where(isapp, napp)
  if napp eq 0 then applist = [''] $
  else applist = applist[locs]

  if arg_present(count) then count = napp

  if keyword_set(all) then return, applist
  return, applist[0]

end
