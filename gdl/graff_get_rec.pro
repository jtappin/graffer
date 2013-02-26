pro graff_get_rec, ilu, tag, value, tcode, ndims = ndims, dims = $
                   dims, nvals = nvals

;+
; GRAFF_GET_REC
;	Read a record from a Graffer V4 file.
;
; Usage:
;	graff_get_rec, ilu, tag, value, tcode
;
; Arguments:
;	ilu	long	input	The file unit to read
;	tag	string	output	The 3-character field tag.
;	value	any	output	The value(s) read.
;	tcode	long	output	The type code for the type of value.
;
; Keywords:
;	ndims	long	output	The number of dimensions read.
;	dims	long	output	The size of each dimension.
;
; History:
;	Original: 5/1/12; SJT
;	Add ndims, nvals and dims keywords: 9/1/12; SJT
;-

on_error, 2

; Initialize the tag header information
tag = '   '
tcode = 0l
ndims = 0l

readu, ilu, tag, tcode, ndims

if tcode eq 0 then return       ; A tag with no values

if ndims gt 0 then begin
    dims = lonarr(ndims)
    readu, ilu, dims
endif else dims = 1

value = make_array(dims, type = tcode)
if arg_present(nvals) then nvals = n_elements(value)

if tcode eq 7 then begin
    lstr = lonarr(dims)
    readu, ilu, lstr
    for j = 0, n_elements(lstr)-1 do  $
      if lstr[j] ne 0 then value[j] = string(replicate(32b, lstr[j]))
endif

if ndims eq 0 then value = value[0]

readu, ilu, value

end
