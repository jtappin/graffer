; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_DS_CREATE
;	Make the pulldown menu to define &|  edit a dataset.
;
; Usage:
;	gr_ds_create, base
;
; Argument:
;	base	long	input	The ID of the parent base
;
; History:
;	Pulled out of GRAFFER: 9/12/96; SJT
;	Moved event handling in: 17/1/97; SJT
;	Drop CDF support: 10/2/97; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add "copy" as an option for functions and data: 7/2/12; SJT
;	Replace cw_pdtmenu with cw_pdmenu_plus: 28/9/16; SJT
;-

pro Gr_dsc_event, event

  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs, /no_copy
  widget_control, event.id, get_uvalue = but

  idraw_flag = 1
  ichange = 1b
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  nch = 1

  if (track_flag) then begin
     idraw_flag = 0
     ichange = 0b
     
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, /help, ''
        goto, miss_case
     endif
  endif

  case but of
     "YAXIS": begin             ; The y-axis selector
        if (track_flag) then graff_msg, pdefs.ids.hlptxt, /help, $
                                        'Select main or ' + $
                                        'alternate Y-axis' $
        else begin
           (*pdefs.data)[pdefs.cset].y_axis = event.index
           ichange = 1b
           draw_flag = 1b
        endelse
     end
     'EDITOR': case event.value of
        'Function': graff_msg, pdefs.ids.hlptxt, /help, $
                               'Enter data in the form ' + $
                               'of a function'
        
        'Function.y = f(x) ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Function with Y as a function of X' $
        else begin
           ichange = graff_funct(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse
        
        'Function.x = f(y) ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Function with X as a function of Y' $
        else begin
           ichange = graff_funct(pdefs, /y_funct)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse 
        
        'Function.x = f(t), y = g(t) ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Function with both X and Y ' + $
                      'functions of a parameter T' $
        else  begin
           ichange = graff_pfunct(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse
        
        'Function.z = f(x,y) ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Function with Z as a function of X and Y' $
        else begin
           ichange = graff_zfunct(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse
        
        'Function.From file ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Read a function definition from a file' $
        else  begin
           ichange = gr_fun_read(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 5
        endelse
        
        'Function.Copy ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      "Copy an existing function dataset" $
        else begin
           ichange = gr_fcopy_menu(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 5
        endelse
        
        'Function.Fit Dataset ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Do a linear, power or exponential ' + $
                      'fit to a specified dataset' $
        else begin
           ichange = gr_fit_dset(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse
        
        'XY data': graff_msg, pdefs.ids.hlptxt, /help, $
                              'Enter data for current ' + $
                              'data set'
        
        'XY data.From file ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Read X, Y, [error] data from an ' + $
                      'external file' $
        else begin
           ichange = gr_xy_read(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 5
        end
        
        'XY data.Top level variables ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Get X, Y, [error] values from IDL ' + $
                      'top-level variables' $
        else  begin
           ichange = graff_tlv(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        end
        
        'XY Data.Edit values ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Enter X, Y, [error] data from a ' + $
                      'widget entry box' $
        else begin
           ichange = gr_xy_wid(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse

        'XY data.Copy ...':if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Copy from another XY dataset' $
        else begin
           ichange = gr_xycopy_menu(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse
        
        'Z data': graff_msg, pdefs.ids.hlptxt, /help,  $
                             'Create a 2 dimensional dataset'
        
        'Z data.Top level variables ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Read 2D data from IDL top-level ' + $
                      'variables' $
        else begin
           ichange = gr_tlv_z(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        end
        
        'Z data.From file ...':if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Read 2D data from an external file' $
        else begin
           ichange = gr_z_read(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        end
        
        'Z data.Copy ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, 'Copy from another 2D dataset' $
        else begin
           ichange = gr_zcopy_menu(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        end

        'Operators': if track_flag then $
           graff_msg, pdefs.ids.hlptxt, /help, 'Common dataset operations'
        
        'Operators.Rescale Current ...': if (track_flag) then $
           graff_msg, pdefs.ids.hlptxt, /help, 'Rescale the current dataset' $
        else begin
           ichange = graff_rescale(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        end
        
        'Operators.Transpose': if track_flag then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      "Transpose the current dataset." $
        else begin
           ichange = gr_ds_transpose(pdefs)
           idraw_flag = ichange
           if (ichange) then nch = 21
        endelse

        'Operators.Erase': if track_flag then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Erase the values in the current ' + $
                      'data set' $
        else begin
           ichange = gr_ds_erase(pdefs)

           draw_flag = ichange
           if ichange then nch = 21
        end
        'Operators.Full Erase': if track_flag then $
           graff_msg, pdefs.ids.hlptxt, /help, $
                      'Erase the values and formatting in the current ' + $
                      'data set' $
        else begin
           ichange = gr_ds_erase(pdefs, /full)

           draw_flag = ichange
           if ichange then nch = 21
        end

     endcase
  endcase

  if (ichange) then begin
     if ((*pdefs.data)[pdefs.cset].type eq 9 or $
         (*pdefs.data)[pdefs.cset].type eq -4) then begin
        widget_control, pdefs.ids.plopts(0), map = 0
        widget_control, pdefs.ids.plopts(1), map = 1
     endif else begin
        widget_control, pdefs.ids.plopts(1), map = 0     
        widget_control, pdefs.ids.plopts(0), map = 1     
     endelse
     widget_control, pdefs.ids.export, sensitive = $
                     (*pdefs.data)[pdefs.cset].type ge 0 and $ 
                     ptr_valid((*pdefs.data)[pdefs.cset].xydata) 
     graff_set_vals, pdefs, /set_only
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

pro Gr_ds_create, base, pdefs

  common graffer_options, optblock

  datopts = [{ds_create_opts, flag:1, label:'XY data'}, $
             {ds_create_opts, 0, 'From file ...'}, $
             {ds_create_opts, 0, 'Edit values ...'}, $
             {ds_create_opts, 0, 'Top level variables ...'}, $
             {ds_create_opts, 2, 'Copy ...'}, $
             {ds_create_opts, 1, 'Z data'}, $
             {ds_create_opts, 0, 'From file ...'}, $
             {ds_create_opts, 0, 'Top level variables ...'}, $
             {ds_create_opts, 2, 'Copy ...'}, $
             {ds_create_opts, 1, 'Function'}, $
             {ds_create_opts, 0, 'y = f(x) ...'}, $
             {ds_create_opts, 0, 'x = f(y) ...'}, $
             {ds_create_opts, 0, 'x = f(t), y = g(t) ...'}, $
             {ds_create_opts, 0, 'z = f(x,y) ...'}, $
             {ds_create_opts, 0, 'From file ...'}, $
             {ds_create_opts, 0, 'Copy ...'}, $
             {ds_create_opts, 2, 'Fit Dataset ...'}, $
             {ds_create_opts, 3, 'Operators'}, $
             {ds_create_opts, 0, 'Rescale Current ...'}, $
             {ds_create_opts, 0, 'Transpose'}, $
             {ds_create_opts, 0, 'Erase'}, $
             {ds_create_opts, 2, 'Full Erase'}]

  jb = widget_base(base, $
                   /row, $
                   event_pro = 'gr_dsc_event')


  junk = cw_pdmenu_plus(jb, $
                        datopts, $
                        return_type = 'full_name', $
                        uvalue = 'EDITOR', $
                        track = optblock.track)

  pdefs.ids.y_axis = widget_droplist(jb, $
                                     value = ['Main', $
                                              'Right'], $
                                     uvalue = 'YAXIS', $
                                     sensitive = pdefs.y_right, $
                                     title = 'Y-axis:', $
                                     track = optblock.track)

  if ptr_valid((*pdefs.data)[pdefs.cset]) then $
     widget_control, pdefs.ids.y_axis, set_droplist_select = $
                     (*pdefs.data)[pdefs.cset].y_axis


end
