;+
; HAS_TAG
;	Return whether a structure has a specified tag.
;
; Usage:
;	present = has_tag(str, tag)
;
; Returns:
;	1 if the tag is present, 0 otherwise.
;
; Arguments:
;	str	struct	The structure to test.
;	tag	string	The tag to test (case insensitive)
;
; History:
;	Original: ca 2015; SJT
;	Documented: 6/11/2024; SJT
;-

function has_tag, str, tag

  t = size(str, /type)
  if t ne 8 then return, 0b

  list = tag_names(str)
  locs = where(list eq strupcase(tag), nm)
  return, nm ne 0
end
