; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GRAFFER
;	Simple interactive data plotter.
;
; Usage:
;	graffer[, file]
;
; Argument:
;	file	string	input	The initial filename for the graph
;				file
;
; Keywords:
;	group	long	input	The group leader of the widget tree.
;	xsize	int	input	The x dimension of the draw widget
;	ysize	int	input	The y dimension of the draw widget
;	/noscroll	input	If set do not make the draw window
;				scrolling even if it is bigger than
;				600 pixels.
;	/debug		input	If set, then run in debugging mode.
;	/block			If set, then run the widgets in
;				blocking mode
;	/recover	input	If set, then attempt to recover from
;				an autosave file.
;	/ttype		input	If set, then make a new file use
;				TrueType fonts. (Opening an existing
;				file will still keep what that one
;				used).
;	/tracking_events	Set explicitly to zero to prevent
;				widget tracking events when running in
;				IDL. (Always disabled in GDL as
;				they don't work properly).
;	/bitmaps	input	If set, then the plot symbol and
;				colour selection menus use bitmaps
;				rather than descriptions. 
;
; History:
;	Original: 27/7/95; SJT
;		V2.00 - Start knocking ideas around, (1) break up the
;                       xsty and ysty tags of pdefs (which are horribly
;                       overloaded in V1.05) into structures.
;                       (2) Reform file into a TAG:VALUE format for
;                       greater flexibility.
;                     - Increase error bar possibilities
;                     - Upgrade function fitting routines
;                     - Rename some routines so that the first 8 chars
;                       are unique (for small-minded systems that only
;                       allow 8.3 filenames).
;                     - Add settings for POSITION (a precursor to
;                       multiple plots on one page?)
;                     - Start to farm out menu panels to procedures in
;                       order to facilitate keeping the standard &
;                       compact versions in step.
;                     - More farming out of bits of the menus
;                     - Modify compact format to put the graphics
;                       window in an independent base (suggestion from
;                       Phil Williams [Children's Hospital Medical
;                       Center, Cincinnati OH]) -- reviving the
;                       original intention of the compact mode.
;                     - Add support for 2-D datasets. Functions F(x,y)
;                       and Z datasets. Display by contours or as an
;                       "image". Also add options to re-order the
;                       datasets, and to write them to a file.
;                     - Add "resource='Graffer'" to all top-level
;                       widgets.
;		      - Make the colour selector in the text-editor
;                       widget into a pull-down like the one on the
;                       dataset menu.
;                     - Major improvements to PostScript options
;                       settings to allow offsets to be adjusted.
;		      - Add an on-line help system
;                     - Extensive changes to popups so that cancelled
;                       operations don't trigger redrawing or set the
;                       changed flag.
;                     - Add options to allow a dataset to be plotted
;                       "Unclipped"
;                     - Display index of current dataset as well as
;                       descriptor.
;                     - Add tracking events to the message box so that
;                       taking the cursor out of it clears the message.
;                     - Add support for Binary GRAFFER files and make
;                       autosave files always binary.
;                     - Improve file handling in several ways:
;                          1) Check if the agument to GRAFFER is a
;                             directory and if so then use it as a
;                             path in the picker.
;                          2) Don't try to find files in a
;                             non-existent or unscannable directory.
;                          3) Put up a prompt when "Save as" or "Open
;                             new" attempts to overwrite an existing
;                             file.
;                     - Solved problem of setting values in a "Stated"
;                       pulldown for the axis style settings. I think
;                       they now reflect correctly the settings of the
;                       current file.
;                     - Add code to allow draw window to popup as soon
;                       as it is entered in compact mode. Also force
;                       the two windows to be reasonably
;                       well-separated (>125 pixels if possible).
;                     - Start to move some of the event handling
;                       routines out.
;                     - Separate definition of symbol from joining
;                       method and provide extra symbols.
;                     - Move the restoration of the plot state to a
;                       "KILL_NOTIFY" procedure (keep the state in
;                       common block - not ideal). Also make the two
;                       windows of compact mode mutually destroy each
;                       other.
;                     - Make the generation code of GRAFFER into
;                       GRAFFER_ONE, thus eliminating a goto and
;                       saving some code duplication. Actually
;                       implement the GROUP key which always existed
;                       but doesn't appear to have been implemented.
;                     - Add new dataset property to define mouse
;                       editability (so that it is possible to shut
;                       out the possibility of accidentally changing a
;                       data set). Move that along with sorting &
;                       clipping into a "Extras" pulldown in the
;                       dataset menus. Also make the dataset selection
;                       menus into a 1-liner.
;                     - Modify fit dataset to make it safer and to
;                       return to "system" versions of routines.
;                     - Add a "changed" indicator in a suitable bit of
;                       unused space.
;                     - Fix compact mode event generation so that the
;                       menu panel auto-exposes properly?
;                     - Add a system for putting a key on the
;                       plot. Also in the process invent "Frame"
;                       coordinates (like normalized but measured
;                       relative to the corners of the axis box rather
;                       than the viewport). Make this system available
;                       for anchoring text as well.
;                     - Add "piecewise linear" fits to the "Fit
;                       dataset" options.
;                     - Add a CAPTURE_INPUT key to GRAFF_ENTER and use
;                       it in most input situations.
;                     - Extensive reconstruction of GR_PICKFILE to
;                       make it look & feel more like the rest of GRAFFER.
;                     - Improve the autosave facility in several ways:
;                        - Big operations (e.g. editing a dataset)
;                          count as more than one operation.
;                        - The handling of the files is improved so
;                          that a prompt is given if the requested
;                          operation isn't what GRAFFER thinks you
;                          should do.
;                        - The flagging of deleting the autosave file
;                          is fixed.
;		      - Add new key option.
;		      - Fix problem with unidentified tags in binary
;                       files.
;		Version 2.01:
;		      - Add space for a general comment on the plot.
;		      - Add GRAFF_PROPS to allow programs to set
;                       global values (e.g. axis ranges, titles etc.)
;		Version 2.02:
;		      - Make Ndata fields LONG.
;		      - Modify file picker to prevent duplicated
;                       directory name being returned in some cases.
;               Version 3.00:
;                     - Replace obsolete handles with pointers (28/6/05)
;                     - Make plotting variables double precision
;                     - Replace a number of "home-made" dialogues
;                       with the system ones.
;                     - Re-merge graff_one  
;		Version 3.01: ?
;		Version 3.02:
;		      - Add colour inversion option for freely scaled
;                       images.
;               Version 3.03:
;                     - Add option for Isotropic axes.
;               Version 3.04:
;                     - Add support for setting 2-D dataset properties
;                       (e.g. contour options) programatically in GRAFF_ADD
;                     - Add accelerators for principal main menu function.
;                     - Fix polar function crash.
;                     - Make isotropic axes work correctly from GUI.
;               Version 3.05:
;                     - Add character size for keys.
;               Version 3.06:
;                     - Fix spurious font=0.
;                     - Fix cross hairs in text mode for >8 bit displays
;                     - Make cross hairs options
;                     - Make screen dumps use true colour (hope it
;                       still works on 8-bit displays).
;                     - Colour inversion works when an explicit range
;                       is given.
;		      - Improve gr_xy_read.
;		Version 3.07:
;		      - Add local colour tables for 2-D datasets.
;		      - Fix problem with single-point datasets.
;		Version 3.08:
;		      - Layout tidying
;		      - Make 2-D choices embedded
;		      - Replace cw_bbselector with widget_droplist
;                       where possible
;                     - Add file input options to GRAFF_ADD
;                     - Several significant bug fixes.
;                     - Add new routine GRAFF_UPDATE to modify
;                       individual datasets from program or prompt.
;		Version 3.09:
;		      - Secondary Y-axis
;		Version 4.00:
;		      - Improved file format.
;		      - Internal changes especially for contours (to
;                       remove artificial limits no longer present in
;                       IDL).
;                     - Actually implement colour menu choice.
;                     - Improved font handling for annotations.
;                     - Allow plot to be displayed in the aspect
;                       ratio of the hard copy.
;                     - Rationalize coordinate transform management.
;                     - GRAFF_UPDATE can change data.
;                     - Add GRAFF_EXPORT
;                     - New lines now shown when adding/moving points
;                       by mouse.
;                     - Option to only display the current dataset.
;                     - Options to hide individual 2-D datasets
;                       (i.e. like colour=omit for ordinary datasets).
;		Version 4.01:
;		      - Rename FUNC[XYZ] keys in GRAFF_ADD and
;                       GRAFF_UPDATE as [XYZ]_FUNC (Also make both
;                       routines do the same thing with function
;                       keys).
;                     - Replace unsupported ROUTINE_NAMES calls with
;                       SCOPE_* calls.
;                     - Allow data to be exported to IDL top-level
;                       variables.
;                     - Add a chooser to the top-level variable import
;                       dialogues. And permit import of variables from
;                       other levels when applicable.
;                     - Add copy options to all the DS filling tools.
;                     - Make insertions possible for Mouse-editing,
;                       and improve deletion handling.
;                     - Allow custom plot file names.
;                     - Make colour PS the default
;                     - Allow specification of a view command for EPS
;                       files in hard copy set up.
;               Version 4.02:
;                     - Various bug fixes.
;                     - Prevent duplicate instances.
;                     - Improve graphics restore.
;               Version 4.02a:
;		      - Fix distance calculations in insert mode.
;		      - Add a "no spool" button to the hard copy options.
;		Version 4.03:      
;		      - Make line thicknesses floating point.
;		Version 4.04:
;		      - Add facilities for programmatic annotation.
;		      - Make text thicknesses FP as well.
;		      - Fix contouring bugs.
;		      - Add value to fill for warped images.
;		Version 4.05:
;		      - Add name selection to exports.
;		      - Add GRAFF_GET_DATA routine.
;		      - Advanced axis style settings.
;		      - Character size settings for contour labels.
;		      - Allow updating of only some variables in the
;                       get from top-level variables tools (by
;                       specifying a dot (.) as the name of any
;                       unchanged component).
;               Version 4.07:
;                     - Support min and max for 1D datasets (not functions)
;               Version 4.08:
;                     - Major changes to colour handling.
;               Version 4.09:
;                     - Add support for PDF output generation.
;                     - replace 3 different local pulldowns with a
;                       single cw_pdmeu_plus routine.
;                     - Make the contour colours into a LIST so that
;                       custom colours can be used.
;                     - Add option for log or sqrt mapping of
;                       automatic contours.
;                     - Convert graff_enter to cw_enter
;                     - Allow axis labelling etc. to use TT fonts.
;		Version 4.10:
;		      - Move general options out of pdefs (21/5/20)
;		      - Harmonization of IDL & Fortran versions
;		Version 5.00:
;		      - Mods to allow it to work in GDL as well as IDL.
;		      - Add subirectory structure.
;-


pro Graff_event, event

  common graffer_options, optblock
  
  base = widget_info(/child, event.top)
  widget_control, base, get_uvalue = pdefs

  sp = size(pdefs, /type)
  if (sp eq 3) then begin
     base = widget_info(/child, pdefs)
     widget_control, base, get_uvalue = pdefs
  end else if (sp ne 8) then begin
     message, /continue, "** O U C H ** Corrupted internal data - " + $
              "bailing out"
     widget_control, event.top, /destroy
     return
  endif

  if (base ne event.id) then widget_control, event.id, get_uvalue = object

  idraw_flag = 1
  ichange = 1b
  track_flag = strpos(tag_names(event, /struct), 'TRACK') ne -1
  nch = 1

  if (track_flag) then begin
     idraw_flag = 0
     ichange = 0b
     
     if (event.enter eq 0) then begin
        graff_msg, pdefs.ids.hlptxt, ''
        case object of          ; Special actions for exit events
           'AUTOSAVE': graff_msg, pdefs.ids.message, ''
           Else:
        endcase
        goto, miss_case
     endif
  endif

  case object of
     'TEXT': if (track_flag) then begin
        graff_msg, pdefs.ids.hlptxt, 'Toggle between drawing and text ' + $
                   'modes'
     endif else begin
        gr_td_mode, event.index, pdefs
        ichange = 0b
        idraw_flag = 0b
     end    
     
     'CROSS': if (track_flag) then begin
        graff_msg, pdefs.ids.hlptxt, "Toggle display of crosshairs"
     endif else begin
        gr_cross_hair, pdefs
        pdefs.transient.hairs = event.select
        idraw_flag = 0b
     endelse

     'QSAVE': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, 'Save plot to currently selected ' + $
                   'filename' $
     else begin
        graff_save, pdefs
        ichange = 0b
        idraw_flag = 0
     end
     
     'DRAW': begin              ; Draw widget in graph mode
        ichange = graff_draw(pdefs, event, track_flag)
        idraw_flag = ichange
        if (ichange) then nch = 21
     end
     
     'WRITE': begin             ; Draw widget in text mode
        ichange = graff_write(pdefs, event, track_flag)
        idraw_flag = ichange
        if (ichange) then nch = 21
     end
     
     
     'AUTOSAVE': if (not track_flag) then begin
        gr_bin_save, pdefs, /auto
        ichange = 0b
        idraw_flag = 0
        widget_control, event.id, timer = optblock.auto_delay
     end
     
     'TABS': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, "Select global settings menus or " + $
                   "dataset menus" $
     else begin
        ichange = 0b 
        if event.tab eq 1 then gr_show_colour, pdefs
     endelse

     'YTABS': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, "Select primary or secondary Y axis " + $
                   "settings" $
     else ichange = 0b          ; ignore
     
     'YRIGHT': if (track_flag) then $
        graff_msg, pdefs.ids.hlptxt, "Enable/disable secondary Y-axis" $
     else begin
        pdefs.y_right = event.select
        widget_control, pdefs.ids.ybase_r, sensitive = pdefs.y_right
        widget_control, pdefs.ids.y_box, sensitive = ~pdefs.y_right
        widget_control, pdefs.ids.y_axis, sensitive = pdefs.y_right
        widget_control, pdefs.ids.x_origin, sensitive = ~pdefs.y_right
     endelse
     
     Else: begin
        graff_msg, pdefs.ids.message, 'Unknown UVALUE: '+object
        help, /structure, event
        ichange = 0b
        idraw_flag = 0
     end
  endcase

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

  widget_control, base, set_uvalue = pdefs

end


pro Graffer, file, group = group, xsize = xsize, ysize = ysize, $
             debug = debug, noscroll = noscroll, $
             recover = recover, block = block, ttype = ttype, $
             tracking_events = tracking_events, bitmaps = bitmaps

  common Gr_psym_maps, psym_bm, col_bm
  common graffer_options, optblock
  
  gwid = gr_present()
  if (gwid gt 0) then begin
     junk = dialog_message(['There is already a GRAFFER window', $
                            'present in this IDL session', $
                            'please close it or open your file', $
                            'in that window'], $
                           title = 'GRAFFER already running', $
                           /error)
     return
  endif

  gr_state, /save               ; Save the plot state to restore on exit
  
; Get the version number.
@graff_version

; Read the resource file.
  
  gr_rc_get, optblock

  if n_elements(tracking_events) ne 0 then $
     optblock.track = keyword_set(tracking_events)

  if n_elements(bitmaps) ne 0 then $
     optblock.bitmaps = keyword_set(bitmaps)
  
  if (keyword_set(debug)) then begin
     !Quiet = 0 
     on_error, 0                ; stop at error
  endif else on_error, 2        ; Return to caller on error
  
  !P.multi = 0                  ; Clear plot positioning settings.
  !P.region = 0
  !P.position = 0
  if keyword_set(ttype) then !p.font = 1 $
  else !P.font = -1
  
  if (n_elements(file) eq 0) then $
     file = dialog_pickfile(/read, $
                            filter = '*.grf', $
                            title = 'Graffer '+vstring+' Select', $
                            resource = 'Graffer') $
  else if (file_test(file, /dir)) then begin
     file = dialog_pickfile(/read, $
                            filter = '*.grf', $
                            title = 'Graffer '+vstring+' Select', $
                            path = file, $
                            resource = 'Graffer')
  endif else if ((strpos(file, '*') > strpos(file, '?')) ne -1) then begin
     f = file
     gr_split_dir, f, dir
     file = dialog_pickfile(/read, $
                            filter = f, $
                            title = 'Graffer '+vstring+' Select', $
                            path = dir, $
                            resource = 'Graffer')
  endif

  if (file eq '') then return

;	Define the data control structure

  graff_init, pdefs, file, version = version, ttype = $
              keyword_set(ttype)
  
  igot = graff_get(pdefs, file, /no_set, recover = recover, /no_warn)
  if igot eq 0 then return      ; Note that here it is meaningful to
                                ; continue if the file doesn't exist.

  pdefs.ids.graffer = widget_base(title = 'Graffer ' + $
                                  'V'+string(pdefs.version, $
                                             format = "(I0,'.',I2.2)"), $
                                  /row, $
                                  xpad = 0, $
                                  ypad = 0, $
                                  space = 0, $
                                  resource = 'Graffer', $
                                  kill_notify = 'gr_state', $
                                  uname = 'GRAFFER')


  base = widget_base(pdefs.ids.graffer, $
                     /column, $
                     xpad = 0, $
                     ypad = 0, $
                     space = 0)

  cdbase = widget_base(base, $
                       /row, $
                       xpad = 0, $
                       ypad = 0, $
                       space = 0)
  cbase = widget_base(cdbase, $
                      /column, $
                      xpad = 0, $
                      ypad = 0, $
                      space = 0)

                                ; Exit etc.
  gr_control_menu, cbase


  if strpos(pdefs.dir, path_sep(), /reverse_search) ne $
     strlen(pdefs.dir)-1 then fullname = pdefs.dir+path_sep()+pdefs.name $
  else fullname = pdefs.dir+pdefs.name
  
  pdefs.ids.name = cw_enter(cbase, $
                            /display, $
                            /text, $
                            value = fullname, $
                            xsize = 30, $
                            label = "Name:")

  tbase = widget_tab(cbase, $
                     uvalue = 'TABS', $
                     track = optblock.track)

  cbase1 = widget_base(tbase, $
                       title = 'Global', $
                       /column)

                                ; Plot Title/Subtitle

  gr_mk_plmenus, cbase1, pdefs

                                ; X Axis controls

  gr_axis_menu, 'X', cbase1, pdefs

                                ; Y Axis controls
  yybase = widget_base(cbase1, $
                       /column, $
                       xpad = 0, $
                       ypad = 0, $
                       space = 0, $
                       /frame)
  jb = widget_base(yybase, $
                   /row, $
                   /nonexclusive)
  pdefs.ids.y_right = widget_button(jb, $
                                    value = "Enable secondary Y-axis?", $
                                    uvalue = 'YRIGHT', $
                                    track = optblock.track)
  widget_control, pdefs.ids.y_right, set_button = pdefs.y_right

  ytabs = widget_tab(yybase, $
                     uvalue = 'YTABS', $
                     track = optblock.track)
  jb = widget_base(ytabs, $
                   title = 'Main', $
                   /column)
  
  gr_axis_menu, 'Y', jb, pdefs

  pdefs.ids.ybase_r = widget_base(ytabs, $
                                  title = 'Secondary', $
                                  sensitive = pdefs.y_right, $
                                  /column)
  gr_axis_menu, 'Yr', pdefs.ids.ybase_r, pdefs

                                ; Setting the properties for the
                                ; current data set

  dsbase = widget_base(tbase, $
                       /column, $
                       title = 'Datasets')

  gr_ds_menu, dsbase, pdefs     ; Change Data Set

  gr_curr_menu, dsbase, pdefs   ; Options for the current DS

                                ; Add a text string

  jb =  widget_base(cbase, $
                    /row)
  pdefs.ids.textmode = widget_droplist(jb, $
                                       value = ['Draw', 'Text'], $
                                       uvalue = 'TEXT', $
                                       title = 'Draw/Text Mode:', $
                                       track = optblock.track)
  jbb = widget_base(jb, $
                    /nonexclusive)
  junk = widget_button(jbb, $
                       value = 'Cross Hairs', $
                       uvalue = 'CROSS', $
                       track = optblock.track)
  widget_control, junk, set_button = 1


                                ; Current cursor position


  jb = widget_base(cbase, $
                   /row, $
                   xpad = 0, $
                   ypad = 0, $
                   space = 0)
  
  pdefs.ids.xcp = cw_enter(jb, $
                           /double, $
                           /display, $
                           xsize = 14, $
                           value = 0., $
                           label = 'X:', $
                           format = "(g14.7)") 
  pdefs.ids.ycp = cw_enter(jb, $
                           /double, $
                           /display, $
                           xsize = 14, $
                           value = 0., $
                           label = 'Y:', $
                           format = "(g14.7)") 



                                ; The draw screen and the plot type
                                ; selections.

  cbase = widget_base(cdbase, $
                      /column)

  if (not keyword_set(xsize)) then xwsize = 800 $
  else xwsize = xsize > 800

  if (not keyword_set(ysize)) then ywsize = 800 $
  else ywsize = ysize > 800

  if ((xwsize >  ywsize) gt 800 && ~keyword_set(noscroll)) then $
     pdefs.ids.draw = widget_draw(cbase, $
                                  xsize = xwsize, $
                                  x_scroll_size = 800, $
                                  ysize = ywsize, $
                                  y_scroll_size = 800, $
                                  uvalue = 'DRAW', $
                                  /button_event, $
                                  /motion_event, $
                                  track = optblock.track, $
                                  /frame) $
  else $
     pdefs.ids.draw = widget_draw(cbase, $
                                  xsize = xwsize, $
                                  ysize = ywsize, $
                                  uvalue = 'DRAW', $
                                  /button_event, $
                                  /motion_event, $
                                  track = optblock.track, $
                                  /frame)


  tjb = widget_base(cbase, $
                    /row)

                                ; Message box, only put it here
                                ; because there's some space, give it
                                ; a UVALUE so that we can use it with
                                ; a timer event to control the autosave
  pdefs.ids.message = cw_enter(tjb, $
                               /display, $
                               xsize = 78, $
                               ysize = 2, $
                               value = '', $
                               label = 'Messages:', $
                               uvalue = 'AUTOSAVE', $
                               track = optblock.track, $
                               /array)

  pdefs.ids.hlptxt = pdefs.ids.message

                                ; A box to show if the plot is changed
                                ; since the last save.

  pdefs.ids.chtick = widget_base(tjb, $
                                 /column, $
                                 xpad = 0, $
                                 ypad = 0, $
                                 space = 0, $
                                 /align_center, $
                                 map = 0)
  cbm = gr_check_box(nx, ny, xbm = xbm)
  junk = widget_button(pdefs.ids.chtick, $
                       value = cbm, $
                       uvalue = 'QSAVE', $
                       track = optblock.track, $
                       x_bitmap_extra = xbm)
  
  widget_control, pdefs.ids.graffer, /real
  if (keyword_set(group)) then $
     widget_control, pdefs.ids.graffer, group = group

  widget_control, pdefs.ids.draw, get_value = windex
  wset, windex
  pdefs.ids.windex = windex

  widget_control, pdefs.ids.dscolour_show, get_value = cwin
  pdefs.ids.dscolour_win = cwin

  device, set_graphics_function = 3 ; Make sure we're in normal plot
                                ; mode
  !p.color = graff_colours(1)   ; Gets reset when the first window is
                                ; created. 
  graff_set_vals, pdefs         ; Set up the values of the text
                                ; widgets etc.

  tmid = pdefs.ids.message

  auto_delay = optblock.auto_delay

  widget_control, base, set_uvalue = pdefs, /no_copy

  widget_control, tmid, timer = auto_delay

  xmanager, 'graff', base, no_block = ~keyword_set(block)

end
