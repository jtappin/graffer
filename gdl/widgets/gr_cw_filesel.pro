; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_CW_FILESEL
;	File selector widget, unlike DIALOG_PICKFILE, it is embedded
;	in a parent widget.
;
; Usage:
;	id = gr_cw_filesel([parent])
;
; Returns:
;	The widget ID of the created widget.
;
; Argument:
;	parent	long	The widget id of a parent base into which the
;			widget will be packed.
;
; Keywords:
;	filter	string	A string array (or scalar) specifying the file
;			types to be displayed (e.g. "*.pdf") the
;			special value "All files" does what it
;			says. Strings starting with a dot, or without
;			a dot at all are treated as suffixes.
;	label	string	A string array of the same length as FILTER
;			specifying the labels to be used on the
;			combobox (e.g. you might want to use the label
;			'JPEG' for the filter ".jpg, .JPG, .jpeg,
;			.JPEG"). 
;	/fix_filter	If set, then the filter combo is not
;			editable. N.B. Unlike IDL's version,
;			multiple filters are still allowed.
;	/frame		If set, then draw a frame around the widger
;	/multiple	If set, then multiple files may be selected.
;	path	string	Specify the path in which to look (default
;			current directory).
;	/save		If set then label the "Open" button as "Save"
;	uname	string	A user name for the widget.
;	uvalue	any	A user value for the widget.
;	/warn_exist	If set then issue a warning if the selected
;			file exists.
;	default	string	Specify a default filename stem.		
;
; Notes:
;	Pro tem. I've not implemented the /image_files and
;	/filename keywords.
;
;
; History:
;	Boiler plate: 14/9/21; SJT
;	Allow to diverge from IDL: 15/9/21; SJT
;-

; Expand a filter spec.
function gr_cw_fs_expand_filter, filter, count = count, $
                                 simple = simple
  
  if strpos(strupcase(filter), 'ALL') eq 0 then begin
     count = 1
     if keyword_set(simple) then return, '' $
     else return, '*'
  endif

  fts = strsplit(filter, ', ', /extr, count = count)

  if keyword_set(simple) then begin
     for j = 0, count-1 do $
        if strpos(fts[j], '.') eq -1 then fts[j] = '.'+fts[j]
  endif else begin
     for j = 0, count-1 do begin
        if strpos(fts[j], '.') eq 0 then fts[j] = '*'+fts[j] $
        else if strpos(fts[j], '.') eq -1 then fts[j] = '*.'+fts[j]
     endfor
  endelse
  
  return, fts
end

; Find directories in a path
pro gr_cw_fs_match_dir, path, dirs, count = count

  dirs = file_search(path+path_sep()+'*', count = count, /test_directory)
  if count eq 0 then dirs = '' $
  else dirs = file_basename(dirs)
     
end

; Find regular files matching a filter in a path
pro gr_cw_fs_match, path, filter, files, count = count

  fts = gr_cw_fs_expand_filter(filter, count = nff)
  nf = 0l
  for j = 0, nff-1 do begin
     ff = file_search(path+path_sep()+fts[j], count = nf1, /test_regular)

     if nf1 ne 0 then begin
        if nf eq 0 then files = ff $
        else files = [files, ff]
        nf += nf1
     endif
  endfor

  if nf ne 0 then begin
     files = file_basename(files)
     idx = sort(files)
     files = files[idx]
     count = nf
  endif else begin
     files = ''
     count = 0
  endelse

end

; Get value (full list of files)
function gr_cw_fs_get, id
  
  base = widget_info(id, /child)
  widget_control, base, get_uvalue = uvs

  sidx = widget_info(uvs.flistid, /list_select)
  if sidx[0] eq -1 || ~uvs.sflag then begin
     widget_control, uvs.fileid, get_value = cfile
     if cfile eq '' then return, ''

     cfile = uvs.path+path_sep()+cfile
     return, cfile
  endif
  
  flist = (*uvs.flist)[sidx]
  flist = uvs.path+path_sep()+flist

  return, flist

end

; Set value (a single name)
pro gr_cw_fs_set, id, name

  base = widget_info(id, /child)
  widget_control, base, get_uvalue = uvs

  if n_elements(name) ne 1 then $
     message, /continue, "Can only set a single name."
  cname = name[0]               ; Scalarizes safely

  if strpos(cname, path_sep()) ne -1 then begin
; Full path
     fname = file_basename(cname)
     dname = file_expand_path(file_dirname(cname))

     widget_control, uvs.dirid, set_value = dname
     uvs.path = dname

     gr_cw_fs_match, uvs.path, uvs.cfilt, files, count = nfile
     gr_cw_fs_match_dir, uvs.path, dirs, count = ndir

     if ptr_valid(uvs.flist) then ptr_free, uvs.flist
     uvs.flist = ptr_new(files)
     if ptr_valid(uvs.dirlist) then ptr_free, uvs.dirlist
     uvs.dirlist = ptr_new(dirs)
     
     if nfile ne 0 then $
        widget_control, uvs.flistid, set_value = files, /sensitive $
     else  widget_control, uvs.flistid, sensitive = 0

     if ndir ne 0 then $
        widget_control, uvs.dlistid, set_value = dirs, /sensitive $
     else widget_control, uvs.dlistid, sensitive = 0

     widget_control, uvs.fileid, set_value = fname
  endif else begin
     fname = cname
     widget_control, uvs.fileid, set_value = fname
  endelse
     
  locs = where(*uvs.flist eq fname, nm)
  if nm eq 0 then $
     widget_control, uvs.flistid, set_list_select = -1 $
  else  widget_control, uvs.flistid, set_list_select = locs[0]

  uvs.sflag = nm ne 0
  
  widget_control, uvs.dobut, sensitive = fname ne ''
  
  widget_control, base, set_uvalue = uvs

end
; Event handler
function gr_cw_fs_event, event

  widget_control, event.id, get_uvalue = but
  base = widget_info(event.handler, /child)
  widget_control, base, get_uvalue = uvs

  done = 0

  imods = 0b

  case but of
     'DO': done = 1
     'DONT': done = 2
     'PATH': begin
        widget_control, event.id, get_value = path
        path = file_expand_path(path)
        if path ne uvs.path then begin
           uvs.path = path
           imods = 1b
           widget_control, uvs.fileid, set_value = ''
           uvs.sflag = 0b
        endif
     end
     
     'UP': begin
        path = file_dirname(uvs.path)
        widget_control, uvs.dirid, set_value = path
        uvs.path = path
        widget_control, uvs.fileid, set_value = ''
        imods = 1b
        uvs.sflag = 0b
     end

     'DIRS': begin
        sdir = (*uvs.dirlist)[event.index]
        uvs.path = uvs.path+path_sep()+sdir
        widget_control, uvs.fileid, set_value = ''
        widget_control, uvs.dirid, set_value = uvs.path
        imods = 1b
        uvs.sflag = 0b
     end

     'FILES': begin
        cfile = (*uvs.flist)[event.index]
        widget_control, uvs.fileid, set_value = cfile
        if event.clicks eq 2 then done = 1
        uvs.sflag = 1b
     end
     'FILE': begin
        widget_control, event.id, get_value = cfile
        locs = where(*uvs.flist eq cfile, nm) 
        if nm eq 0 then widget_control, uvs.flistid, $
                                        set_list_select = -1 $
        else widget_control, uvs.flistid, $
                             set_list_select = locs[0]
        uvs.sflag = nm ne 0
     end

     'FILTER': begin
        if event.index eq -1 then begin
           uvs.cfilt = event.str
           imods = 1b
           uvs.sflag = 0b
        endif else if uvs.cfilt ne uvs.filter[event.index] then begin
           uvs.cfilt = uvs.filter[event.index]
           imods = 1b
           uvs.sflag = 0b
        endif
        uvs.clabel = event.str
        if imods then begin
           if uvs.default ne '' then begin
              ff = gr_cw_fs_expand_filter(uvs.cfilt, /simple)
              widget_control, uvs.fileid, set_value = uvs.default+ff[0]
           endif else widget_control, uvs.fileid, set_value = ''
        endif
     end
  endcase

  if imods then begin
     gr_cw_fs_match, uvs.path, uvs.cfilt, files, count = nfile
     gr_cw_fs_match_dir, uvs.path, dirs, count = ndir

     if ptr_valid(uvs.flist) then ptr_free, uvs.flist
     uvs.flist = ptr_new(files)
     if ptr_valid(uvs.dirlist) then ptr_free, uvs.dirlist
     uvs.dirlist = ptr_new(dirs)
     
     if nfile ne 0 then $
        widget_control, uvs.flistid, set_value = files, /sensitive $
     else  widget_control, uvs.flistid, sensitive = 0
     
     if ndir ne 0 then $
        widget_control, uvs.dlistid, set_value = dirs, /sensitive $
     else widget_control, uvs.dlistid, sensitive = 0
  endif

  widget_control, base, set_uvalue = uvs
  
  widget_control, uvs.fileid, get_value = cfile
  if cfile ne '' || done eq 2 then begin
     widget_control, uvs.dobut, /sensitive
     ffn = uvs.path+path_sep()+cfile
     if done eq 1 && uvs.warn && file_test(ffn) then begin
        status = dialog_message(["File: "+ffn, $
                                 "already exists, do you want", $
                                 "to overwrite it?"], $
                                dialog_parent = event.top, $
                                /question, $
                                title = "Overwrite file?")
        if status eq 'No' then return, 0l
     endif
     return, {GR_FILESEL_EVENT, $
              id: event.handler, $
              top: event.top, $
              handler: 0l, $
              value: ffn, $
              done: done, $
              filter: uvs.cfilt, $
              label: uvs.clabel}
  endif else begin
     widget_control, uvs.dobut, sensitive = 0
     return, 0l                 ; No event in these cases
  endelse
  
end

function gr_cw_filesel, parent, $
                        filter = filter, fix_filter = fix_filter, $
                        frame = frame, multiple = multiple, $
                        path = path, save = save, uname = uname, $
                        uvalue = uvalue, warn_exist = warn_exist, $
                        default = default, label = label

  if keyword_set(multiple) && keyword_set(warn_exist) then $
     message, "/MULTIPLE and /WARN_EXIST are in confict, will ignore " + $
              "/WARN_EXIST."
  if keyword_set(multiple) && keyword_set(save) then $
     message, "/MULTIPLE and /SAVE are in confict, will ignore " + $
              "/SAVE."
  
  if n_elements(filter) eq 0 then filter = "All files"
  nfilt = n_elements(filter)
  if n_elements(label) eq 0 then flabel = filter $ 
  else if n_elements(label) eq nfilt then $
     flabel = label $
  else if n_elements(label) gt nfilt then begin
     message, /continue, "Too many elements in LABEL, truncating."
     flabel = label[0:nfilt-1]
  endif else if n_elements(label) lt nfilt then begin
     message, /continue, "Too few elements in LABEL, padding."
     flabel = [label, filter[n_elements(label):*]]
  endif
  locs = where(flabel eq '', nb)
  if nb ne 0 then flabel[locs] = filter[locs]
  
  if n_elements(path) eq 0 then cd, current = cpath $
  else cpath = file_expand_path(path)

  if n_elements(default) ne 0 then dfn = default $
  else dfn = ''
  
  if n_params() eq 0 || ~widget_info(parent, /valid) then $
     tlb = widget_base(/column, $
                       uvalue = uvalue, $
                       uname = uname, $
                       frame = frame) $
  else  tlb = widget_base(parent, $q
                          /column, $
                          uvalue = uvalue, $
                          uname = uname, $
                          frame = frame)
  
  base = widget_base(tlb, $
                     /column)

  pb = widget_base(base, $
                   /row)
  junk = widget_label(pb, $
                      value = "Directory:")

  dirid = widget_text(pb, $
                      xsize = 40, $ ; TBC
                      /edit, $
                      value = cpath, $
                      uvalue = 'PATH')

  junk = widget_button(pb, $
                       value = 'Up', $
                       uvalue = 'UP')

  lb = widget_base(base, $
                   /row)

  jb = widget_base(lb, $
                   /col)
  junk = widget_label(jb, $
                      value = "Directories")
  
  dlistid = widget_list(jb, $
                        xsize = 20, $
                        ysize = 10, $
                        /scroll, $
                        uvalue = 'DIRS')

  jb = widget_base(lb, $
                   /col)
  
  junk = widget_label(jb, $
                      value = "Files")
  
  flistid = widget_list(jb, $
                        xsize = 35, $
                        ysize = 10, $
                        /scroll, $
                        uvalue = 'FILES', $
                        multiple = multiple)


  jb = widget_base(base, $
                   /row)

  jbb = widget_base(jb, $
                    /col)

  fb = widget_base(jbb, $
                   /row)

  junk = widget_label(fb, $
                      value = "File:")

  fileid = widget_text(fb, $
                       /edit, $
                       xsize = 40, $
                       uvalue = 'FILE')
  if dfn ne '' then begin
     ff = gr_cw_fs_expand_filter(filter[0], /simple)
     widget_control, fileid, set_value = dfn+ff[0]
  endif
  
  cb = widget_base(jbb, $
                   /row)

  junk = widget_label(cb, $
                      value = 'Filter:')

  filtid = widget_combobox(cb, $
                           value = flabel, $
                           editable = ~keyword_set(fix_filter), $
                           uvalue = 'FILTER')

  jbb = widget_base(jb, $
                    /col)

  dobut = widget_button(jbb, $
                        value = (keyword_set(save) && $
                                 ~keyword_set(multiple)) ? 'Save' : $
                        'Open', $
                        uvalue = 'DO', $
                        sensitive = keyword_set(default))

  junk = widget_button(jbb, $
                       value = 'Cancel', $
                       uvalue = 'DONT')

  gr_cw_fs_match, cpath, filter[0], files, count = nfile
  gr_cw_fs_match_dir, cpath, dirs, count = ndir

  if nfile ne 0 then $
     widget_control, flistid, set_value = files $
  else  widget_control, flistid, sensitive = 0

  if ndir ne 0 then $
     widget_control, dlistid, set_value = dirs $
  else widget_control, dlistid, sensitive = 0

  uvs = {path: cpath, $
         filter: filter, $
         default: dfn, $
         cfilt: filter[0], $
         clabel: flabel[0], $
         dirlist: ptr_new(dirs), $
         flist: ptr_new(files), $
         dirid: dirid, $
         dlistid: dlistid, $
         flistid: flistid, $
         fileid: fileid, $
         dobut: dobut, $
         warn: keyword_set(warn_exist) && ~keyword_set(multiple), $
         sflag: 0b}

  widget_control, tlb, func_get_value = 'gr_cw_fs_get', $
                  pro_set_value = 'gr_cw_fs_set', $
                  event_func = 'gr_cw_fs_event'
  
  widget_control, base, set_uvalue = uvs

  return, tlb

end
