function Gr_get_full_dir, dir

;+
; GR_GET_FULL_DIR
;	Get the full pathname of a directory
;
; Usage:
;	path = gr_get_full_dir(dir)
;
; Return value:
;	path	string	The full pathname of the specfied directory
;
; Argument:
;	dir	string	input	The directory to be interpreted.
;
; History:
;	Original: 17/1/97; SJT
;-

cd, dir, current = here
cd, here, current = path
separator = path_sep()

if (strpos(path, separator, /reverse_search) ne strlen(path)-1) then $
  path = path+separator 

return, path

end

