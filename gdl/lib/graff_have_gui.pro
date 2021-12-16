;+
; GRAFF_HAVE_GUI
;	Determine if the full graffer GUI is installed (or only the
;	tools).
;
; Usage:
;	have_gui = graff_have_gui()
;
; Returns:
;	1 if the procedure "graffer" is found, 0 otherwise.
;
; History:
;	Original: 8/12/21; SJT
;-

function graff_have_gui

  sp = strsplit(!path, path_sep(/search_path), /extract, $
                count = npath)

  for j = 0, npath-1 do begin
     g = file_search(sp[j]+path_sep()+'graffer.pro', count = ng)
     if ng gt 0 then return, 1b
  endfor
  return, 0b
end
