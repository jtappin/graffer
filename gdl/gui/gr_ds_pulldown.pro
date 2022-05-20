; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_DS_PULLDOWN
;	Dataset selection pulldown
;
; Usage:
;	gr_ds_pulldown, base, pdefs
;
; Argument:
;	base	long	input	The ID of the parent base.
;	pdefs	struct	The graffer master data structure
;
; History:
;	Extracted from GRAFFER: 9/12/96; SJT
;	Move event handler here: 17/1/97; SJT
;	Reduce size by putting erase & delete under "Other": 27/1/97; SJT
;	Add mouse-editing default option: 13/8/97; SJT
;	Replace handles with pointers: 27/6/05; SJT
;	Make default name use 1-based index: 25/1/12; SJT
;	Add export: 3/2/12; SJT
;	Add copy: 7/2/12; SJT
;	Replace cw_pdtmenu with cw_pdmenu_plus: 28/9/16; SJT
;-

pro Gr_dss_event, event

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy

  idraw_flag = 1
  ichange = 1b
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  nch = 1

  if track_flag then begin
     idraw_flag = 0
     ichange = 0b
     
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case event.value of
     'Other': graff_msg, pdefs.ids.hlptxt, /help, 'More complex dataset ' + $
                         'operations'
     
     'Other/Select...': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Select current data set' $
     else begin
        graff_ch_dset, pdefs
        ichange = 0b
        idraw_flag = 0
     end
     
     'Other/Merge...': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Merge two data sets' $
     else begin
        gr_app_w, pdefs
        nch = 15
     end
     
     'Other/Sort...': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Change the order of the data sets' $
     else begin
        graff_sort_dss, pdefs
        nch = 15
     end
     
     'Other/Write...': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Save current dataset to a file' $
     else begin
        dir = pdefs.ds_dir
        wname = strmid(pdefs.name, 0, strpos(pdefs.name, '.', $
                                             /reverse_search))+ $
                string(pdefs.cset+1, format = "('_',I0,'.dat')")
        fc = dialog_pickfile(file = wname, path = dir, $
                             dialog_parent = $
                             pdefs.ids.graffer, title = 'Graffer ' + $
                             'dataset file', /overwrite_prompt, filter $
                             = "*.dat", /write, resource = 'Graffer')
        if (fc eq '') then graff_msg, pdefs.ids.message,  $
                                      'No save file specified' $
        else begin
           gr_write_ds, pdefs, fc
           pdefs.ds_dir = dir
        endelse
        ichange = 0b
        idraw_flag = 0
     end
     'Other/Export': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Send data from current DS to ' + $
                   '$MAIN$ level' $
     else begin
        graff_to_tlv, pdefs
        ichange = 0b
        idraw_flag = 0
     endelse
     
     'Next':if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Make next data set current' $
     else begin
        pdefs.cset = (pdefs.cset+1) mod pdefs.nsets
        graff_set_vals, pdefs, /set_only
        ichange = 0b
        idraw_flag = 0
     end
     
     'Previous':if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Make previous data set current' $
     else begin
        pdefs.cset = pdefs.cset-1
        if (pdefs.cset eq -1) then pdefs.cset = pdefs.nsets-1
        graff_set_vals, pdefs, /set_only
        ichange = 0b
        idraw_flag = 0
     end
     
     'New':if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help,  $
                   'Create a new data set and make it current' $
     else begin
        pdefs.cset = pdefs.nsets
        pdefs.nsets = pdefs.nsets+1
        
        *pdefs.data = [*pdefs.data, gr_new_ds(pdefs)]
        
        graff_set_vals, pdefs, /set_only
        ichange = 0b
        idraw_flag = 0
     end
     
     
     'Other/Delete': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, /help, 'Delete the current data set' $
     else graff_dsdel, pdefs

     'Other/Copy': if track_flag then $
        graff_msg, pdefs.ids.hlptxt, /help, $
                   'Copy the current dataset to a new ' + $
                   'dataset' $ 
     else begin
        current = pdefs.cset
        type = (*pdefs.data)[pdefs.cset].type
        pdefs.cset = pdefs.nsets
        pdefs.nsets = pdefs.nsets+1
        
        *pdefs.data = [*pdefs.data, gr_new_ds(pdefs)]

        if type eq 9 then ichange = gr_z_copy(pdefs, current) $
        else if type lt 0 then ichange = gr_func_copy(pdefs, current) $
        else ichange = gr_xy_copy(pdefs, current)
        
        graff_set_vals, pdefs, /set_only

        idraw_flag = ichange
     end

  endcase

  if (ichange) then begin
     if ((*pdefs.data)[pdefs.cset].type eq 9 or $
         (*pdefs.data)[pdefs.cset].type eq -4) then begin
        widget_control, pdefs.ids.plopts(0), map = 0
        widget_control, pdefs.ids.plopts(1), map = 1
        if (widget_info(/valid, pdefs.ids.zopts.bases[0])) then begin
           fmt = (*pdefs.data)[pdefs.cset].zopts.format
           widget_control, pdefs.ids.zopts.bases[0], map = ~fmt
           widget_control, pdefs.ids.zopts.bases[1], map = fmt
        endif
     endif else begin
        widget_control, pdefs.ids.plopts(1), map = 0     
        widget_control, pdefs.ids.plopts(0), map = 1     
     endelse
  endif

  if (idraw_flag) then gr_plot_object, pdefs
  if (ichange) then begin
     pdefs.chflag = 1b
     pdefs.transient.changes = pdefs.transient.changes+nch
     if (pdefs.transient.changes gt 20) then begin
        gr_bin_save, pdefs, /auto
     endif
  endif
  widget_control, pdefs.ids.chtick, map = pdefs.chflag

Miss_case:

  widget_control, base, set_uvalue = pdefs, /no_copy

end


pro Gr_ds_pulldown, base, pdefs

  common graffer_options, optblock

  dsops = [{ds_pd_opts, flag:0, label:'Next'}, $
           {ds_pd_opts, 0, 'Previous'}, $
           {ds_pd_opts, 0, 'New'}, $
           {ds_pd_opts, 3, 'Other'}, $
           {ds_pd_opts, 0, 'Select...'}, $
           {ds_pd_opts, 0, 'Merge...'},  $
           {ds_pd_opts, 0, 'Sort...'},  $
           {ds_pd_opts, 0, 'Delete'}, $
           {ds_pd_opts, 0, 'Write...'}, $
           {ds_pd_opts, 0, 'Copy'}, $
           {ds_pd_opts, 2, 'Export'}]

  jb = widget_base(base, /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0, $
                   event_pro = 'gr_dss_event', $
                   /align_center) 
  junk = cw_pdmenu_plus(jb, $
                        dsops, $
                        uvalue = 'DSOPS', $
                        track = optblock.track, $
                        return_type = 'full_name', $
                        delimiter = '/', $
                        /row, $
                        ids = pids)

  pdefs.ids.export = pids[10]

end
