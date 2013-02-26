pro Gr_split_dir, file, dir

;+
; GR_SPLIT_DIR
;	Separate a file name into a directory and a file name.
;
; Usage:
;	gr_split_dir, file, dir
;
; Arguments:
;	file	string	in/out	On entry the full filename, on exit
;				the filename part.
;	dir	string	output	The directory name.
;
; History:
;	Original: 18/8/95; SJT
;	Rename as GR_SPLIT_DIR (was split_dir): 18/9/96; SJT
;	Use file_dirname & file_basename: 15/2/12; SJT
;-

if (n_elements(dir) eq 0) then dir = ''

dir = file_dirname(file, /mark)
file = file_basename(file)

end
