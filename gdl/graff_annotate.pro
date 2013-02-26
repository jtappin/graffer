pro graff_annotate, file, value, id = id, index = index, $
                    substring = substring, case_squash = case_squash, $
                    text = text, newid = newid, $
                    colour = colour, size = size, orient = orient, $
                    align = align, ffamily = ffamily, $
                    font = font, x = x, y = y, norm = $
                    norm, axis = axis,  status = status

;+
; GRAFF_ANNOTATE
;	Add or modify an annotation to a graffer file.
;
; Usage:
;	graff_annotate, file[, value, <keywords>]
;
; Arguments:
;	file	string	The file to modify,
;	value	string	The text of the annotation to modify (This is
;			not generally a good way to identify the
;			annotation).
;
; Keywords:
;	id	string	The ID tag of the annotation to modify.
;	index	int	The index number (1-based) of the annotation
;			to modify
;	/substring	If set, then VALUE may match an initial substring.
;	/case_squash	If set, then VALUE is matched without regard
;			to case.
;	text	string	New text for the annotation.
;	newid	string	A new ID for the annotation.
;	colour	int	The colour for the annotation.
;	size	float	The font size for the annotation
;	orient	float	The orientation of the annotation
;			(anticlockwise, degrees)
;	align	float	The alignment (0=left, 1=right, 0.5=centre)
;	ffamily	int	The font style to use.
;	font	int	Set the font shape & weight for the desired font.
;	x	float	Set the X-coordinate of the origin
;	y	float	Set the Y-coordinate of the origin
;	norm	int	Set the coordinate system, 0=data,
;			1=normalized, 2 = "frame".
;	axis	int	For data coordinates, set which Y axis to use.
;	status	int	A named variable, set to 1 on success, 0 on
;			failure.
;
; Notes:
;	The behaviour if more than one font family keyword is set is
;	undefined).
;	Specifying more than one identification is an error, if no
;	identification is given then a new annotation is created.
;	For a new annotation, at least TEXT, X and Y must be given
;
; History:
;	Original: 13/6/12; SJT
;-

  on_error, 2                   ; Return to caller on error

  status = 0
  if ~file_test(file) then message, "File does not exist"

  is_update = (n_params() eq 2) + keyword_set(id) + keyword_set(index)

  if (is_update gt 1) then $
     message, "May only give one or zero ways of finding the annotation"

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin
     message, "Failed to open: "+f0
     return
  endif

  if (is_update) then begin
     if pdefs.ntext eq 0 then $
        message, "Annotation update requested, no annotations in " + $
                 "dataset"
     if keyword_set(index) then begin
        if pdefs.ntext lt index or index le 0 then $
           message, "Index is negative or greater than the number " + $
                    "of annotations found"

        idx = index-1
     endif else if keyword_set(id) then begin
        locs = where((*pdefs.text).id eq id, nm)
        if nm eq 0 then $
           message, "No annotations matching ID found"
        if nm gt 1 then $
           message, "Multiple ID matches found, using first",  /continue
        idx = locs[0]
     endif else begin
        textstrs = (*pdefs.text).text
        if keyword_set(substring) then $
           textstrs = strmid(textstrs, 0, strlen(value))
        if keyword_set(case_squash) then $
           locs = where(strupcase(textstrs) eq strupcase(value), nm) $
        else locs = where(textstrs eq value, nm)

        if nm eq 0 then $
           message, "No annotations matching text found"
        if nm gt 1 then $
           message, "Multiple text matches found, using first",  /continue
        idx = locs[0]

     endelse
  endif else begin
     if ~keyword_set(text) then $
        message, "Must have a text string for a new annotation"
     if ~keyword_set(x) || ~keyword_set(y) then $
        message, "Must give a position for a new annotation"

     (*pdefs.text) = [(*pdefs.text), pdefs.text_options]
     idx = pdefs.ntext
     pdefs.ntext++
  endelse

  if (keyword_set(text)) then (*pdefs.text)[idx].text = text
  if (keyword_set(newid)) then (*pdefs.text)[idx].id = newid

  if n_elements(colour) ne 0 then (*pdefs.text)[idx].colour = colour
  if n_elements(size) ne 0 then (*pdefs.text)[idx].size = size
  if n_elements(orient) ne 0 then (*pdefs.text)[idx].orient = orient
  if n_elements(align) ne 0 then (*pdefs.text)[idx].align = align

  if n_elements(ffamily) ne 0 then (*pdefs.text)[idx].ffamily = $
     ffamily
  
  if n_elements(font) ne 0 then (*pdefs.text)[idx].font = font

  if n_elements(x) ne 0 then (*pdefs.text)[idx].x = x
  if n_elements(y) ne 0 then (*pdefs.text)[idx].y = y
  if n_elements(norm) ne 0 then (*pdefs.text)[idx].norm = norm

  gr_bin_save, pdefs
  graff_clear, pdefs

  status = 1

end
