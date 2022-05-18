; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; CW_PDMENU_PLUS
;	An enhanced version of CW_PDMENU, supporting several extra
;	features.
;
; Usage:
;	id = cw_pdmenu_plus(parent, desc)
;
; Returns:
;	The ID of the generated compound widget.
;
; Arguments:
;	parent	long	The ID of the parent base widget.
;	desc	struct	A descriptor of the menu (see below for
;			details) 
;
; Keywords:
;	column	int	Make the top-level arrange the buttons in
;			column(s)
;	row	int	Make the top-level arrange the buttons in
;			row(s)
;	/mbar		If set and the parent is a menu bar base to
;			generate a menu bar. N.B. Owing to (a) the
;			limitations of menu bar bases and (b) the need
;			to assign an event function, CW_MENU_PLUS must
;			provide the whole menu system.
;	/help		If /mbar is set, then differentiate any help
;			button.
;	return_type str	Specify the type of value to return in the
;			value field of the event structure, may be any
;			of:
;				id: Return the widget id of the button
;				index: Return the index (location in desc).
;				name: Return the button label
;				full_name: Return the full name of the
;					button (i.e. prefixed with all
;					parent button labels).
;				uname: Return a user-specied name
;					(uname tag must be in the
;					descriptor)
;	ids	long	A named variable to return the button ids.
;	uvalue	any	A user-specified value for the widget.
;	uname	string	A user-specified name for the widget.
;	/align_*	The align_* keywords are appied to the
;			generated base widget
;	/tracking_events	If set, then return tracking events.
;	sensitive int	Set the sensitivity of the whole heirarchy.
;	delimiter str	Set the delimiter between the components of
;			the value if return_type is full_name, the
;			default is "."
;	/selector	If set, then the widget behaves like a
;			droplist. If this option is set, then there
;			may not be any submenus.
;	/pad_labels	If set and this is a selector, text labels are
;			padded with spaces to make sure the
;			long ones don't get truncated. If the
;			longest label has N charaters then a label
;			with n characters is padded with 2(N-n) blanks
;			(since blanks are usually smaller than typical
;			characters).
;	initial_selection
;		int	For a selector menu, specify the initially
;			selected item.
;	/simulate_check	If set, then simulate checkable buttons rather
;			than using a checked_menu button. This is not
;			possible if the buttons have bitmap labels.
;
;	Other keys are passed directly to the generated buttons.
;
; Notes:
; 	The descriptor is a structure, with the following tags
; 	recognized: 
;		flag  - Int - 1 = Start of menu sequence
;			      2 = End of menu sequence
;			      4 = Stateful button
;			Values may be or'ed. Buttons parented
;			to the base or with children may not be stateful.
;			'flags' will also be recognized for compatibility.
;		label - String with the button's label.
;		bitmap - byte array or a pointer to one for a bitmap
;                        label for a button
;		accelerator - An accelerator key combination.
;		handler - an event handler for the button and its
;                         children. 
;               uname - A user-defined name for the button.
;               state - Whether the button is initially
;                       selected. Only meaningful if flag and 4.
;               group - Set to non-zero value(s) to group together
;                       stateful buttons that form an exclusive group.        
;		sensitive - Whether a button is initially sensitive or
;                           not. 
;
;               At least one of the label and bitmap tags is
;               required. If both are present, then the bitmap tag
;               MUST be a pointer, which MUST be null if a text label
;               is to be used.
;               If the flag tag is absent, then the first element has
;               a flag of 1, the last has 2 and the rest 0.
;               For a selector menu, the group settings are
;               ignored (with a warning) and set to 1 and flag is
;               implicitly or'ed with 4. A return type of index
;               is also implied. Selectors may not mix bitmap and text
;               labels. 
;
;	The event returned may either be a standard tracking event or
;	has the structure:
;	event = {cw_pdmenu_plus_event_l, id: 0, top:0l, handler: 0l, $
;		value: 0L, select: 0b}
;	for return types of index or id	or:
;	event = {cw_pdmenu_plus_event_s, id: 0, top:0l, handler: 0l, $
;		value: '', select: 0b}
;	for return types of name, full_name or uname.	
;
;	For non-stateful buttons, state is always 0.
;
;	The align_* and row/column keys are ignored for a menu bar.
;
;
; CW_PDMENU_PLUS_SET
;	Set the state of a stateful button in a PDMENU_PLUS
;
; Usage:
;	cw_pdmenu_plus_set, id, state
;
; Arguments:
;	id	long	The id of the button to set, or the parent of
;			an exclusive group
;	state	bool	The state to which to set it.
;
; Keyword:
;	index	int	If this is present, then set the
;			index'th child of ID. Normally used to
;			set a button within an exclusive submenu. If
;			index is present, the state argument is
;			optional and defaults to 1b
;
;
; CW_PDMENU_PLUS_GET
;	Get which of a group is selected.
;
; Usage:
;	idx = cw_pdmenu_plus_get(wid[, group])
;
; Returns:
;	The index of the selected button within the intersection of
;	children of WID and members of GROUP.
;
; Arguments:
;	wid	long	The widget id to query (if this is the base of
;			the CW, then children of the first button will
;			be scanned, otherwise direct children of the
;			button are scanned).
;	group	int	Which button group to scan. If not given then 1
;			is used
;
; Keywords:
;	id	long	A named variable to get the widget ID of the
;			selected button.
; Notes:
;	This is mainly a slightly more flexible version of the
;	GET_VALUE function for selectors.
;
; History:
;	Merger of graffer's two extended pull downs: Sep 2016; SJT
;	Numerous adjustments to cover features not yet in GDL:
;	12-13/8/21; SJT
;-


function cw_pdmenu_has_kids, id
                                ; Return 1 if the widget has
                                ; children, 0 if it
                                ; doesn't.
  if ~widget_info(id, /valid) then return, 0
  
  k1 = widget_info(id, /child)
  return, k1 ne 0
end

function cw_pdmenu_plus_get, wid, group, index = index, id = id

  idb = wid
  if widget_info(idb, /type) eq 0 then $
     idb = widget_info(idb, /child)

  id = 0l

  if n_params() eq 1 then group = 1
  ids = widget_info(idb, /all_children)
  if ~widget_info(ids[0], /valid) then return, -1
  nc = n_elements(ids)
  
  if ~arg_present(index) then index = -1l
  for j = 0, nc-1 do begin
     widget_control, ids[j], get_uvalue = uv
     if cw_pdmenu_has_kids(ids[j]) then begin
        idx1 = cw_pdmenu_plus_get(ids[j], group, index = index, id = id)
        if idx1 ge 0 then return, idx1
     endif else begin
        if uv.group ne group then continue
        index++
        if uv.state then begin
           id = ids[j]
           return, index
        endif
     endelse
  endfor

  return, -1l
end

function cw_pdmenu_plus_get_selector, wid
  return, cw_pdmenu_plus_get(wid, 1)
end

pro cw_pdmenu_plus_set_exclusive, id, parent
  widget_control, id, get_uvalue = uvalue
  group = uvalue.group

  if group eq 0 then return     ; Not a member of a group.

  if n_params() eq 1 then begin
     parent = id
     repeat $
        parent = widget_info(parent, /parent) $
     until widget_info(parent, /type) eq 0
  endif

  idlist = widget_info(parent, /all_children)

  for j = 0, n_elements(idlist)-1 do begin
     if idlist[j] eq id then continue
     if cw_pdmenu_has_kids(idlist[j]) then $
        cw_pdmenu_plus_set_exclusive, id, idlist[j] $
     else begin
        widget_control, idlist[j], get_uvalue = uvg
        if uvg.group eq group then begin
           uvg.state = 0
           if ~uvg.is_selector then begin
              if uvg.check eq 1 then begin
                 widget_control, idlist[j], set_value = $
                                 uvg.label+' [ ]'
              endif else $
                 widget_control, idlist[j], set_button = 0
           endif
           widget_control, idlist[j], set_uvalue = uvg
        endif
     endelse
  endfor
end

pro cw_pdmenu_plus_set, id, state, index = index

  if n_elements(index) eq 1 then begin
     if n_params() eq 1 then state = 1b
     if widget_info(id, /type) eq 0 then $
        idb = widget_info(id, /child) $
     else idb = id
     idlist = widget_info(idb, /all_children)

     if idlist[0] eq 0 || index ge n_elements(idlist) then begin
        message, /continue, "Widget has no children, or fewer " + $
                 "then requested index"
        return
     endif
     cw_pdmenu_plus_set, idlist[index], state
     widget_control, idlist[index], get_uvalue = uvalue

  endif else begin
     widget_control, id, get_uvalue = uvalue
     if uvalue.state eq state then return

     uvalue.state = state

     if ~uvalue.is_selector then begin
        if uvalue.check eq 1 then begin
           if state then bvs = uvalue.label+' [*]' $
           else bvs = uvalue.label+' [ ]'
           widget_control, id, set_value = bvs
        endif else $
           widget_control, id, set_button = state
     endif else $
        widget_control, widget_info(id, /parent), set_value = uvalue.label

     widget_control, id, set_uvalue = uvalue

     if state && uvalue.group ne 0 then $
        cw_pdmenu_plus_set_exclusive, id
  endelse
end

pro cw_pdmenu_plus_set_selector, id, index
  cw_pdmenu_plus_set, id, index = index
end

function cw_pdmenu_plus_event, event

  widget_control, event.id, get_uvalue = uvalue

  if tag_names(event, /struct) eq 'WIDGET_TRACKING' then begin
     if size(uvalue.val, /type) eq 7 then $
        return, {cw_pdmenu_plus_track_event_s, $
                 id: event.handler, $
                 top: event.top, $
                 handler: 0l, $
                 enter: event.enter, $
                 value: uvalue.val} $
     else $
        return, {cw_pdmenu_plus_track_event_l, $
                 id: event.handler, $
                 top: event.top, $
                 handler: 0l, $
                 enter: event.enter, $
                 value: long(uvalue.val)}
  endif else begin

     if uvalue.check ne 0 then begin
        if uvalue.is_selector then begin
           uvalue.state = 1b
           widget_control, widget_info(event.id, /parent), $
                           set_value = uvalue.label
        endif else begin
           uvalue.state = ~uvalue.state
           
           if uvalue.check eq 1 then begin
              if uvalue.state then bvs = uvalue.label+' [*]' $
              else bvs = uvalue.label+' [ ]'
              widget_control, event.id, set_value = bvs
           endif else $
              widget_control, event.id, set_button = uvalue.state

        endelse

        widget_control, event.id, set_uvalue = uvalue

        if uvalue.state && uvalue.group ne 0 then $
           cw_pdmenu_plus_set_exclusive, event.id, event.handler
     endif

     if size(uvalue.val, /type) eq 7 then $
        return, {cw_pdmenu_plus_event_s, $
                 id: event.handler, $
                 top: event.top, $
                 handler: 0l, $
                 value: uvalue.val, $
                 select: uvalue.state} $
     else return, {cw_pdmenu_plus_event_l, $
                   id: event.handler, $
                   top: event.top, $
                   handler: 0l, $
                   value: long(uvalue.val), $
                   select: uvalue.state}
  endelse

end

pro cw_pdmenu_plus_build, parent, desc, idx, nbuttons, etype, is_mb, $
                          dhelp, delimiter, ids, isbitmap, chmenu, $
                          tracking_events = tracking_events, $
                          prefix = prefix, selector = selector, $
                          _extra = _extra

  base_parent = widget_info(parent, /type) eq 0

  while idx lt nbuttons do begin
     menu = (desc[idx].flag and 1b) ne 0
     if menu && idx ne 0 && keyword_set(selector) then $
        message, "A selector menu cannot have submenus" 
     if ~is_gdl() && menu && ~is_mb then menu = 2

     check = (desc[idx].flag and 4b) ne 0 
     if check ne 0 then check += chmenu
     
     if check ne 0 && (base_parent || menu ne 0) then $
        message, "Cannot create a checked menu at the top level " + $
                 "or with children"
     
     emenu = (desc[idx].flag and 2b) ne 0

     if desc[idx].ltype eq 1 then begin
        bv = *(desc[idx].bitmap)
        if desc[idx].label then $
           message, "Bitmap and text label specified for " + $
                    "button, ignoring text", /continue
     endif else bv = desc[idx].label

     case check of
        1: begin
           if keyword_set(selector) then bvs = bv $
           else if desc[idx].state then bvs = bv +' [*]' $
           else bvs = bv+' [ ]'
           but = widget_button(parent, $
                               value = bvs, $
                               tracking_events = tracking_events, $
                               sensitive = desc[idx].sensitive, $
                               uname = desc[idx].uname, $
                               accel = desc[idx].accelerator, $
                               _extra = _extra)
        end
        0: begin
           but = widget_button(parent, $
                               value = bv, $
                               menu = menu, $
                               tracking_events = tracking_events, $
                               sensitive = desc[idx].sensitive, $
                               uname = desc[idx].uname, $
                               accel = desc[idx].accelerator, $
                               _extra = _extra)
        end
        2: begin
           but = widget_button(parent, $
                               value = bv, $
                               /checked_menu, $
                               tracking_events = tracking_events, $
                               sensitive = desc[idx].sensitive, $
                               uname = desc[idx].uname, $
                               accel = desc[idx].accelerator, $
                               _extra = _extra)
           widget_control, but, set_button = desc[idx].state
        end
     endcase
     
     if keyword_set(selector) then vv = idx-1 $
     else case etype of
        0: vv = but
        1: vv = idx
        2: vv = desc[idx].label
        3: begin
           if keyword_set(prefix) then $
              vv = prefix+delimiter+desc[idx].label $
           else vv = desc[idx].label
           if menu ne 0 then pfx = vv
        end
        4: vv = desc[idx].uname
     endcase

     uv = {val: vv, $
           check: check, $
           is_selector: keyword_set(selector), $
           state: desc[idx].state, $
           group: desc[idx].group, $
           label: bv $
          } 

     if check ne 0 && ~is_gdl() then $
        widget_control, but, set_button = desc[idx].state

     widget_control, but, set_uvalue = uv 
     if desc[idx].handler ne '' then widget_control, $
        but, event_pro = desc[idx].handler

     ids[idx] = but
     idx++
     if menu ne 0 then $
        cw_pdmenu_plus_build, but, desc, idx, nbuttons, etype, is_mb, $
                              dhelp, delimiter, ids, isbitmap, $
                              chmenu, tracking_events = $
                              tracking_events, $
                              prefix = pfx, selector = selector, $
                              _extra = _extra
     
     if emenu then return

  endwhile

end

function cw_pdmenu_plus, parent, udesc, column = column, row = row, $
                         ids = ids, mbar = mbar, help = help, $
                         return_type = return_type, uvalue = uvalue, $
                         uname = uname, sensitive = sensitive, $
                         tracking_events = tracking_events, $
                         align_bottom = align_bottom, $
                         align_top = align_top, $
                         align_left = align_left, $
                         align_right = align_right, $
                         align_center = align_center, $
                         delimiter = delimiter, $
                         selector = selector, $
                         pad_labels = pad_labels, $
                         initial_selection = initial_selection, $
                         simulate_check = simulate_check, $
                         _extra = _extra

  if n_params() ne 2 then message, "Must give a parent and a menu " + $
                                   "descriptor"
  
  if keyword_set(row) && keyword_set(column) then $
     message, "Cannot set both row and column keys" $
  else if ~keyword_set(row) && ~keyword_set(column) then row = 1

  if size(udesc, /type) ne 8 then message, "Descriptor must be a " + $
                                           "structure"

  dtags = tag_names(udesc)
  have_fields = [where(dtags eq 'LABEL'), $
                 where(dtags eq 'BITMAP'), $
                 where(dtags eq 'FLAG' or dtags eq 'FLAGS'), $
                 where(dtags eq 'ACCELERATOR'), $
                 where(dtags eq 'HANDLER'), $
                 where(dtags eq 'UNAME'), $
                 where(dtags eq 'STATE'), $
                 where(dtags eq 'SENSITIVE'), $
                 where(dtags eq 'GROUP')] ne -1
  if ~have_fields[0] && ~have_fields[1] then $
     message, "Either the LABEL field or the BITMAP field is required " + $
              "in the descriptor."

  if have_fields[0] && have_fields[1] && keyword_set(selector) then $
     message, "Only one of the LABEL and BITMAP fields may be " + $
              "given for selector menus."

  isbitmap = have_fields[1]
  ioff = keyword_set(selector)
  
  nbuttons = n_elements(udesc) + ioff

  descr = replicate({cw_pdmenu_plus_descr, $
                     bitmap: ptr_new(), $
                     label: '', $
                     ltype: 0b, $
                     flag: 0b, $
                     accelerator: '', $
                     handler: '', $
                     uname: '', $
                     state: 0b, $
                     group: 0, $
                     sensitive: 0b},  nbuttons)
  if ~isbitmap then descr[ioff:*].label = udesc.label $
  else if size(udesc[0].bitmap, /type) eq 10 then begin
     if keyword_set(selector) then  descr[0].ltype = 1b
     for j = ioff, nbuttons-1 do begin
        if ptr_valid(udesc[j-ioff].bitmap) then begin
           if ptr_valid(descr[j].bitmap) then $
              ptr_free, descr[j].bitmap
           descr[j].bitmap = ptr_new(*(udesc[j-ioff].bitmap))
           descr[j].ltype = 1b
        endif else descr[j].label = udesc[j-ioff].label
     endfor
  endif else begin
     if have_fields[0] then $
        message, "To mix bitmap & text labels, the bitmap field " + $
                 "must be a pointer.", /continue
     for j = ioff, nbuttons-1 do descr[j].bitmap = $
        ptr_new(udesc[j-ioff].bitmap)
     descr.ltype = 1
  endelse
  if keyword_set(selector) then begin
     if keyword_set(initial_selection) then $
        ibm = initial_selection+1 $
     else ibm = 1
     if isbitmap then begin
        bm = *descr[ibm].bitmap
        descr[0].bitmap = ptr_new(*descr[ibm].bitmap)
     endif else begin
        descr[0].label = descr[ibm].label
        if keyword_set(pad_labels) then begin
           nchar = max(strlen(descr.label))
           for j = 0, n_elements(descr)-1 do begin
              nlong = 2*nchar - strlen(descr[j].label)
              descr[j].label = gr_strpad(descr[j].label, nlong, /centre)
           endfor
        endif
     endelse
  endif
  
  if have_fields[2] then begin
     junk = where(dtags eq 'FLAG', nf)
     if nf ne 0 then $
        descr[ioff:*].flag = udesc.flag $
     else $
        descr[ioff:*].flag = udesc.flags
  endif
  
                                ; Ensure that the first and last flags
                                ; are a start and an end
                                ; respectively. 
  descr[nbuttons-1].flag or= 2b
  if keyword_set(selector) then begin
     descr[0].flag = 1b
     descr[1:*].flag or= 4b
  endif

  junk = where((descr.flag and 4b) ne 0, nsb)
  if nsb eq 0 then chmenu = 0b $
  else begin
     if isbitmap && keyword_set(simulate_check) then begin
        message, "Simulated check menus and bitmap labels are " + $
                 "incompatible.", /cont
        return, 0l
     endif
        
     if isbitmap && is_gdl() && ~keyword_set(selector) then $
        message, "GDL does not yet fully support checkable bitmaps.", $
                 /continue
                 
     chmenu = ~keyword_set(selector) && ~keyword_set(simulate_check)
  endelse
  
  if have_fields[3] then descr[ioff:*].accelerator = udesc.accelerator
  if have_fields[4] then descr[ioff:*].handler = udesc.handler
  if have_fields[5] then descr[ioff:*].uname = udesc.uname
  if have_fields[6] then descr[ioff:*].state = udesc.state
  if have_fields[7] then begin
     descr[0].sensitive = 1b
     descr[ioff:*].sensitive = udesc.sensitive
  endif else descr.sensitive = 1b
  if keyword_set(selector) then begin
     descr[0].group = 0
     descr[1:*].group = 1
  endif else if have_fields[8] then $
     descr.group = udesc.group
  
  if ~keyword_set(return_type) || $
     keyword_set(selector) then etype = 1 $
  else case strlowcase(return_type) of
     'id': etype = 0
     'index': etype = 1
     'name': etype = 2
     'full_name': etype = 3
     'uname': etype = 4
     else: message, "Invalid return event type "+return_type
  endcase
  if (etype eq 2 || etype eq 3) && isbitmap then $
     message, "Cannot return names or full names with bitmap buttons."
  
  is_mb = keyword_set(mbar)
  dhelp = is_mb && keyword_set(help) 

  if ~keyword_set(delimiter) then delimiter = '.'

  if is_mb then begin
     base = parent
     widget_control, base, event_func = 'cw_pdmenu_plus_event'
  endif else begin
     if n_elements(sensitive) ne 0 then iss = keyword_set(sensitive) $
     else iss = 1
     base = widget_base(parent, $
                        row = row, $
                        column = column, $
                        uvalue = uvalue, $
                        uname = uname, $
                        sensitive = iss, $ 
                        align_bottom = align_bottom, $
                        align_top = align_top, $
                        align_left = align_left, $
                        align_right = align_right, $
                        align_center = align_center, $
                        event_func = 'cw_pdmenu_plus_event')
  endelse
  
  ids = lonarr(nbuttons)

  cw_pdmenu_plus_build, base, descr, 0l, nbuttons, etype, is_mb, $
                        dhelp, delimiter, ids, isbitmap, $
                        chmenu, selector = selector, $
                        tracking_events = tracking_events, $
                        use_checked_menus = use_checked_menus, $
                        _extra = _extra

  if keyword_set(selector) then begin
     ids = ids[1:*]
     widget_control, base, $ $
                     pro_set_value = 'cw_pdmenu_plus_set_selector', $
                     func_get_value = 'cw_pdmenu_plus_get_selector'
     if n_elements(initial_selection) ne 0 then $
        iset = initial_selection $
     else iset = 0
     widget_control, base, set_value = iset
  endif

  return, base

end


