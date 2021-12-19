; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; CW_ENTER
;	A labeled text entry field
;
; Usage:
;	id = cw_enter(parent, ...)
;
; Return:
;	id	long	The ID of the compound widget
;
; Argument:
;	parent	long	input	The ID of the base in which the widget
;				will sit.
;
; Keywords:
;	label	string	input	The label to be attached to the entry box
;	value	...	input	The initial value of the widget
;	uvalue	...	input	A user-value for the compound.
;	floating	input	If set then the values are floating point
;	double		input	If set then the values are double precision
;	integer		input	If set then the values are short
;				integers
;	long_int	input	If set then the values are long
;				integers
;	very_long	input	If set, then values are 64-bit integers.
;	unsigned	input	If set, then any integer type is
;				unsigned.
;	text		input	If set, then the values are text
;				strings (default action)
;	list_object	input	If set, then values will be returned
;				as an IDL list. In this case
;				pro_set_list and func_get_list must
;				both also be set. (N.B. /LIST_OBJECT implies
;				/ARRAY_VALUED).
;	format	string	input	The format for displaying the
;				value. (not used for LIST entries, see
;				SET_LIST).
;	xsize	int	input	The size of the text input box (chars)
;	ysize	int	input	The number of rows in the box
;	column		input	If set then put the label above the
;				text box (default is to the left)
;	frame		input	If set, then put a box around the
;				whole compound
;	box		input	If set, then put a box around the text
;				field.
;	all_events	input	If set, then return all events except
;				selection events
;	no_event	input	If set, then don't return events at
;				all.
;	select_events	input	If set and all_events is set, then
;				even return selection events
;	tracking_events	input	If set, then enable cursor tracking
;				events in the text window.
;	capture_focus	input	If set, then putting the cursor into
;				the text-box gives the focus to the widget.
;	array_valued	input	If set, then the widget can accept &
;				return an array of values (normally
;				only scalar values are accepted)
;	scroll		input	If set then make the text widget a
;				scrolling widget.
;	graphics	input	If set and this is a text input box,
;				don't return strings ending in a
;				single pling "!" (To avoid hershey
;				character errors).
;	/empty_nan	input	If set and this is a scalar float or
;				double box, then return a NaN value
;				for an empty box.
;	/ignore_empty	input	If set, and this is a text box, then
;				ignore blank lines (unless all lines
;				are blank when a single blank is
;				returned as the value). For numeric
;				types this is always the case unless
;				/empty_nan is set.
;	uname	string	input	A user-defined name for the widget.
;	set_list string	input	A user defined function to specify how
;				to convert the list to a string array.
;	get_list string	input	A user defined function to convert a
;				string array to a list.
;	/sensitive	input	Set whether the widget is sensitive to
;				inputs or not.
;	font	string	input	The font to use for the label.
;	fieldfont string input	The font to use for the entry box.
;
;	Other keywords are passed directly to the base that is
;	returned.
;
; Restrictions:
;	If the text window does not contain a valid value for the
;	given type, then the null string is returned by a get_value
;	call.
;
; Notes:
;	If widget_control, id, set_value='LABEL:...' is used, then the
;	label is reset to the part of the string after "LABEL:".
;
; List values:
;	A list-valued entry box is implicitly array valued. Each line
;	of the text widget is converted to an element of the list. The
;	formatting/decoding is done by the routines specified by the
;	SET_LIST and GET_LIST keywords.
;	SET_LIST -- Takes a single LIST argument and returns a
;                   string array. If it fails it should return a
;                   numeric value. 0 is the normal soft fail
;                   e.g. while the value is incomplete, non-zero is a
;                   hard fail.
;	GET_LIST -- Takes a single string array argument and returns a
;                   list. If it fails it should return a numeric
;                   value. 0 is the normal soft fail
;                   e.g. while the value is incomplete, non-zero is a
;                   hard fail.
;	The examples below are suitable get and set functions for an
;	entry for a list who's elements can contain an arbitrary number of
;	integer values.
;
;; function tlist_get, str
;;   nli = n_elements(str)
;;   rv = list()
;;   on_ioerror, fail
;
;;   for j = 0, nli-1 do begin
;;      junk = strsplit(str[j], ' 	,', count = nn)
;;      if nn eq 0 then continue
;;      el = intarr(nn)
;;      reads, str[j], el
;;      rv.add, el
;;   endfor
;
;;   if n_elements(rv) eq 0 then return, 0
;;   return, rv
;
;; fail:
;;   return, 0
;; end
;; function tlist_set, lis
;;   nli = n_elements(lis)
;
;;   if ~obj_valid(lis) || nli eq 0 then return, 0
;
;;   print, nli, lis
;;   rv = strarr(nli)
;
;;   for j = 0, nli-1 do begin
;;      nn = n_elements(lis[j])
;;      if nn gt 0 then begin
;;         fmt = string(nn, format = "('(',i0,'(1x,i0))')")
;;         rv[j] = string(lis[j], format = fmt)
;;      endif else rv[j] = ''
;;   endfor
;
;;   return, rv
;; end
;
; Events:
;	If tracking events are requested these are passed through with
;	updated ID and HANDLER fields.
;	Other events have the form:
;	{ID: 0l, TOP: 0l, HANDLER: 0l, VALUE: <type>, CR: 0B, TYPE:
;	0L}
;	VALUE is the contents of the widget converted to the type
;	specified. CR is set to 1 if a carriage return was entered or
;	if the widget lost focus. TYPE is type of the value as would
;	returned by size(value, /type), lists have type=11.
;
; History:
;	Original: 25/8/95; SJT
;	use decoders rather than internal reads: 29/8/95; SJT
;	Add tracking_events key: 4/12/95; SJT
;	Add array_valued and scroll keys: 9/12/96; SJT
;	Modify handler so tracking events can be returned by
;	"non-editable" or "non-event" widgets: 14/1/97; SJT
;	Add CAPTURE_FOCUS key: 6/2/97; SJT
;	Add GRAPHICS key: 12/2/97; SJT
;	Put in "event backlog" trapping to prevent the multiple
;	updating of the plot when a title is typed rapidly: 3/7/97; SJT
;	Add option to set the label by using widget_control: 15/2/12; SJT
;	Get/Set non-finite values as an empty field: 4/3/15; SJT
;	Few code modernizations, get rid of the no_copy's,
;	support 64-bit ints & unsigned: 4/10/16; SJT
;	Add support for list-valued entries: 6/10/16; SJT
;	Merge graff_enter and cw_ffield, and store value in the state
;	structure: 13/10/16; SJT
;	Modify sensitive handling for GDL: 6/10/20; SJT
;-


pro cw_enter_focus, id
                                ; Set input focus to the text widget
                                ; id.

  base = widget_info(id, /child)
  widget_control, base, get_uvalue = state

  widget_control, state.text, /input_focus

end

pro cw_enter_set, id, value
                                ; Set the value of a cw_enter
                                ; widget

  base = widget_info(id, /child)
  widget_control, base, get_uvalue = state

  ;; catch, an_error
  ;; if an_error ne 0 then begin
  ;;    message, /continue, "Could not convert value to appropriate type."
  ;;    catch, /cancel
  ;;    return
  ;; endif
  
  if ~state.array then v1 = value[0]  $
  else v1 = value

  sv = size(v1, /type)
  if (sv eq 7 && n_elements(value) eq 1 && $
      strpos(v1, 'LABEL:') eq 0) then begin
     widget_control, state.label, set_value = strmid(v1, 6)
     return
  endif

  if (sv ne state.type) then case state.type of
     4: vv = float(v1)
     5: vv = double(v1)
     2: vv = fix(v1)
     3: vv = long(v1)
     7: vv = strtrim(string(v1, /print), 2)
     12: vv = uint(v1)
     13: vv = ulong(v1)
     14: vv = long64(v1)
     15: vv = ulong64(v1)
     11: begin
        vv = call_function(state.get_list, v1)
        if size(vv, /type) ne 11 then begin
           message, continue = vv eq 0, $
                    "Unable to convert input to a list"
           return
        endif
     end
           
     Else: message, 'Unknown entry field type'
  endcase else vv = v1

  *state.value = vv
  
  case state.type of
     7: vs = vv
     11: begin
        vs = call_function(state.set_list, vv)
        if size(vs, /type) ne 7 then begin
           message, continue = vs eq 0, $
                    "Unable to convert input to a string"
           return
        endif
     end
     else: begin
        vs = string(vv, format = state.format)
        if state.empty_nan then begin
           locs = where(~finite(vv), ni)
           if ni ne 0 then vs[locs] = ''
        endif
     end
  endcase

  widget_control, state.text, set_value = vs

  widget_control, base, set_uvalue = state

end

function cw_enter_get, id
                                ; get the value of a cw_enter widget
                                ; from the value stored in the state
                                ; structure.

  base = widget_info(id, /child)
  widget_control, base, get_uvalue = state
  
  return, *(state.value)

end

function cw_enter_read, id
                                ; Get the value of a cw_enter
                                ; widget from the text widget (used by
                                ; the event handler). A user get_value
                                ; can access the stored value.

  base = widget_info(id, /child)
  widget_control, base, get_uvalue = state

  widget_control, state.text, get_value = txt
  if  ~state.array then txt = txt[0] ;$

  nv0 = n_elements(txt)
  ivv = replicate(1b, nv0)

  case state.type of
     4: begin                   ; Float
        val = fltarr(nv0)
        v0 = 0.
     end
     5: begin                   ; Double
        val = dblarr(nv0)
        v0 = 0.d0
     end
     2: begin                   ; Int
        val = intarr(nv0)
        v0 = 0
     end
     3: begin                   ; Long
        val = lonarr(nv0)
        v0 = 0l
     end
     12: begin                  ; UInt
        val = uintarr(nv0)
        v0 = 0u
     end
     13: begin                  ; ULong
        val = ulonarr(nv0)
        v0 = 0ul
     end
     14: begin                  ; Long64
        val = lon64arr(nv0)
        v0 = 0ll
     end
     15: begin                  ; ULong64
        val = ulon64arr(nv0)
        v0 = 0ull
     end
     7: begin                   ; Text
        val = txt
        if (state.graph) then for j = 0, nv0-1 do $
           ivv[j] = (strmid(txt(j), strlen(txt(j))-1, 1) ne '!') ||  $
                    (strmid(txt(j), strlen(txt(j))-2, 2) eq '!!') $
        else ivv[*] = 1b
     end
     11: begin                  ; LIST
        val = call_function(state.get_list, txt)
        if size(val, /type) ne 11 then begin
           if val ne 0 then message, "Invalid contents read"
           return, 0
        endif else return, val
     end
     Else: message, 'Unknown entry field type'
  endcase

; Note that list types will have returned by we get here

  if state.type ne 7 then begin
     for j = 0, nv0-1 do begin
        if strtrim(txt[j], 2) eq '' then begin
           if state.empty_nan then begin
              val[j] = !values.f_nan
              ivv[j] = 1b
           endif else begin
              ivv[j] = 0b
           endelse
        endif else begin
           catch, an_error
           if an_error ne 0 then begin
              ivv[j] = 0b
              catch, /cancel
              continue
           endif
           reads, txt[j], v0
           val[j] = v0
           catch, /cancel
        endelse
     endfor
  endif else if state.ignore_empty then begin
     locs = where(txt eq '', nv, ncomp = nf)
     if nv ne 0 then begin
        if nf ne 0 then ivv[locs] = 0b $
        else if n_elements(ivv) gt 1 then ivv[1:*] = 0b
     endif
  endif

;	Only return "valid" values. If there are no valid values then
;	return a null string for numeric types and zero for text types
;	(this allows a test that the type is right to test if a proper
;	value is present!)
;	For Lists, excluding invalid lines is the job of the decoder.

  locs = where(ivv, nv)

  if (nv gt 0) then val = val[locs] $
  else if (state.type eq 7) then val = 0 $
  else val = ''

  if (nv eq 1) then val = val[0] ; Make single value a
                                ; scalar

  return, val

end

function cw_enter_event, event
                                ; Process events from a cw_enter
                                ; widget

  if (event.id eq 0l) then return, 0l

  base = widget_info(event.handler, /child)
  widget_control, base, get_uvalue = state

  e_type = tag_names(event, /structure_name)

  case e_type  of
     'WIDGET_TRACKING': begin
        trkopt = state.track
        if ((trkopt and 2b) ne 0 and event.enter) then $
           widget_control, state.text, /input_focus

        event.id = event.handler
        event.handler = 0l
        if (trkopt) then return, event $
        else return, 0l
     end
     'WIDGET_KBRD_FOCUS': begin
        if event.enter || ~state.chflag || state.dead then return, 0l
        cr = 1b
     end

     else: begin
        if state.dead then return, 0l ; Not returning events
                                ; Dummy return

        if event.type eq 3 && ~state.select then $
           return, 0l           ; Dummy return

        if event.type eq 0 && event.ch eq 10b then begin
           cr = 1b
           state.chflag = 1b
        endif else begin
           cr = 0b
           if event.type ne 3 then state.chflag = 1b
        endelse

        if ~(state.all || cr) then begin
           widget_control, base, set_uvalue = state
           return, 0l
        endif
     end
  endcase

  val = cw_enter_read(event.handler)

  sv = size(val, /type)
  if (sv ne state.type) then return, 0l ; Value wasn't valid don't
                                ; return anything

  *(state.value) = val

;	Absorb any events that have come during the processing (I
;	suspect that this is a hangover from very old machines where
;	it was possible to type faster than the event handler could
;	cope).

  widget_control, base, set_uvalue = state
  new_event = widget_event(event.handler, /nowait)
  widget_control, base, get_uvalue = state

  ev = { $
       Id:      event.handler, $
       Top:     event.top, $
       Handler: 0l, $
       Value:   *(state.value), $
       cr:      cr, $
       changed: state.chflag, $
       Type:    state.type $
       }

  state.chflag = 0b
  widget_control, base, set_uvalue = state

  return, ev

end

pro cw_enter_cleanup, bid
  widget_control, bid, get_uvalue = state, /no_copy
  if n_elements(state) ne 0 &&  ptr_valid(state.value) then $
     ptr_free, state.value
end

function cw_enter, parent, label = label, value = value, $
                   uvalue = uvalue, floating = floating, $
                   integer = integer, text = text, $
                   long_int = long_int, very_long = very_long, $
                   unsigned = unsigned, double = double, $
                   format = format, xsize = xsize, ysize = ysize, $
                   column = column, frame = frame, box = box, $
                   all_events = all_events, no_events = no_events, $ $
                   select_events = select_events, display = display, $
                   tracking_events = tracking_events, $
                   array_valued = array_valued, scroll = scroll, $
                   capture_focus = capture_focus, $
                   graphics = graphics, empty_nan = empty_nan, $
                   ignore_empty = ignore_empty, $
                   list_object = list_object, set_list = set_list, $
                   get_list = get_list, sensitive = sensitive, $
                   uname = uname, font = font, $
                   fieldfont = fieldfont, $
                   _extra = _extra


                                ; First step: check that unset keys
                                ; are set to something if needed.

  if n_elements(ysize) eq 0 then ysize = 1

  txt_all_events = ~(keyword_set(no_events) || keyword_set(display))
  all = keyword_set(all_events) && txt_all_events

  edit = ~keyword_set(display)
  track = keyword_set(tracking_events) or $
          2b*keyword_set(capture_focus)

                                ; Set states according to the type

  sl = ''
  gl = ''

  return_nan =  0b

  if keyword_set(floating) then begin
     if ~keyword_set(format) then format = "(g10.3)"
     return_nan = keyword_set(empty_nan) && $
        ~keyword_set(array_valued)
     vtype = 4                  ; Use the codes from SIZE for
                                ; consistency
     if n_elements(value) eq 0 then $
        value = return_nan ? !values.f_nan : 0.0

  endif else if keyword_set(double) then begin
     if ~keyword_set(format) then format = "(g12.5)"
     return_nan = keyword_set(empty_nan) && $
        ~keyword_set(array_valued)
     vtype = 5
     if n_elements(value) eq 0 then $
        value = return_nan ? !values.d_nan : 0.0d0

  endif else if keyword_set(integer) then begin
     if ~keyword_set(format) then format = "(I0)"
     if keyword_set(unsigned) then begin
        vtype = 12
        if n_elements(value) eq 0 then value = 0u
     endif else begin
        vtype = 2
        if n_elements(value) eq 0 then value = 0
     endelse

  endif else if keyword_set(long_int) then begin
     if ~keyword_set(format) then format = "(I0)"
     if keyword_set(unsigned) then begin
        vtype = 13
        if n_elements(value) eq 0 then value = 0ul
     endif else begin
        vtype = 3
        if n_elements(value) eq 0 then value = 0l
     endelse

  endif else if keyword_set(very_long) then begin
     if ~keyword_set(format) then format = "(I0)"
     if keyword_set(unsigned) then begin
        vtype = 15
        if n_elements(value) eq 0 then value = 0ull
     endif else begin
        vtype = 14
        if n_elements(value) eq 0 then value = 0ll
     endelse

  endif else if keyword_set(list_object) then begin
     if ~keyword_set(set_list) || $
        ~keyword_set(get_list) then message, $
        "A LIST-valued entry requires both SET_LIST and " + $
        "GET_LIST"
     sl = set_list
     gl = get_list
     vtype = 11
     if n_elements(value) eq 0 then value = list()
     if keyword_set(format) then message, /continue, $
                                          "FORMAT should not be " + $
                                          "set for LIST entries"
     format = "(a)"

  endif else begin              ; No key is the same as /text
     if ~keyword_set(format) then format = "(A)"
     vtype = 7
     if n_elements(value) eq 0 then value = ''
  endelse

  if vtype ne 11 &&  (keyword_set(set_list) || $
                      keyword_set(get_list)) then $
                         message, /continue, $
                                  "SET_LIST and " + $
                                  "GET_LIST are ignored " + $
                                  "for non-list entries"


                                ; Define the heirarchy

                                ; This is the top-level base which the
                                ; user will see

  if (n_elements(parent) eq 0) then $
     tlb = widget_base(uvalue = uvalue, $
                       uname = uname, $
                       _extra = _extra) $
  else tlb = widget_base(parent, $
                         uvalue = uvalue, $
                         uname = uname, $
                         _extra = _extra)

                                ; This is the base to contain the
                                ; label and text box, and also has the
                                ; configuration structure as its
                                ; uvalue.

  if (keyword_set(column)) then  $
     base = widget_base(tlb, $
                        /column, $
                        frame = frame, $
                        kill_notify = 'cw_enter_cleanup') $
  else base = widget_base(tlb, $
                          /row, $
                          frame = frame, $
                          kill_notify = 'cw_enter_cleanup')

  if n_elements(label) ne 0 then $
     label = widget_label(base, $
                          value = label, $
                          /dynamic, $
                          font = font)

  tbox = widget_text(base, $
                     edit = edit, $
                     all_events = txt_all_events, $
                     kbrd_focus_events = txt_all_events, $
                     frame = box, $
                     xsize = xsize, $
                     ysize = ysize, $
                     tracking_events = (keyword_set(tracking_events) || $
                                        keyword_set(capture_focus)), $
                     scroll = keyword_set(scroll), $
                     font = fieldfont)

  state = { $
          text:   tbox, $
          label:  label, $
          all:    all, $
          dead:   keyword_set(no_events) || keyword_set(display), $
          type:   vtype, $
          format: format, $
          track:  track, $
          select: keyword_set(select_events), $
          array:  keyword_set(array_valued) || vtype eq 11, $
          graph:  keyword_set(graphics) && (vtype eq 7), $
          empty_nan: return_nan, $
          ignore_empty: keyword_set(ignore_empty), $
          set_list: sl, $
          get_list: gl, $
          value: ptr_new(value), $
          chflag: 0b $
          }

  widget_control, base, set_uvalue = state

; In GDL sensitive = null is equivalent to sensitive=0.
  
  if n_elements(sensitive) eq 0 then sensitive = 1
  
  widget_control, tlb, event_func = 'cw_enter_event', $
                  func_get_value = 'cw_enter_get', $
                  pro_set_value = 'cw_enter_set', $
                  sensitive = sensitive
  widget_control, tlb, set_value = value

  return, tlb

end
