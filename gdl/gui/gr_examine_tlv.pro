; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function gr_examine_tlv, name, level = level, exclude = exclude

;+
; GR_EXAMINE_TLV
;	Get information about all top-level variables.
;
; Usage:
;	descr=gr_examine_tlv([name])
;
; Returns:
;	A string or array of strings with the properties of the
;	variables.
;
; Argument:
;	name	string	The name of one or more variables to
;			examine. If not given then all variables are
;			examined.
;
; Keywords:
;	level	long	The scope level to examine ($MAIN$ [1] if not set).
;	/exclude	If set, then exclude variables of a type never
;			suitable for plotting (undefined, strings and
;			objects). Ignored if a name is given.
;
; History:
;	Original: 6/2/12; SJT
;-

if n_elements(level) eq 0 then level = 1
namelist = scope_varname(level = level, count=nvars)

if n_params() eq 0 then names = namelist $
else names = strupcase(name)

outtype = strarr(n_elements(names))
outsize = strarr(n_elements(names))
use = replicate(1b, n_elements(names))
iexcl = n_params() eq 0 && keyword_set(exclude)

for j = 0, nvars-1 do begin
    locs = where(names[j] eq namelist, nm)
    if nm eq 0 then outtype[j] = "<Not Found>" $
    else begin
        props = size(scope_varfetch(names[j], level = level), /struct)
        outtype[j] = props.type_name
        if props.structure_name ne '' then $
          outtype[j] += '{'+props.structure_name+'}'
        if props.type_name ne "UNDEFINED" then begin
            if props.n_dimensions eq 0 then outsize[j] = "Scalar" $
            else begin
                outsize[j] = $
                  string(props.dimensions[0:props.n_dimensions-1], $
                         format = "('[',8(i0,:,','))")
                outsize[j] += "]"
            endelse
        endif
        if iexcl && (props.type eq 0 or $
                     props.type eq 7 or $
                     props.type eq 11) then use[j] = 0b
    endelse
endfor

fmt = string(max(strlen(names)), max(strlen(outtype)), $
             max(strlen(outsize)), format = $
             "('(A-',I0,',1x,A-',I0,',1x,A-',I0,')')") 
output = strarr(n_elements(names))
for j = 0, n_elements(names)-1 do output[j] = string(names[j], $
                                                     outtype[j], $
                                                     outsize[j], $
                                                     format = fmt)

if iexcl then begin
    locs = where(use, nu)
    if nu eq 0 then return, ''
    output = output[locs]
endif
return, output

end
