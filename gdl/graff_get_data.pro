pro graff_get_data, file, idx, name = name, $
                    xval = xval, yval = yval, zval = zval, $
                    xerr = xerr, yerr = yerr

;+
; GRAFF_GET_DATA
;	Extract data from a graffer file.
;
; Usage:
;	graff_get_data, file[, index, <extraction keys>]
;
; Argument:
;	file	string	The file to extract.
;	idx	int	The dataset index to extract
;
; Keywords:
;	name	string	Specify the dataset by name.
;	xval	double	A variable to contain the X values.
;	yval	double	A variable to contain the Y values.
;	zval	double	A variable to contain the Z values.
;	xerr	double	A variable to contain the X errors.
;	yerr	double	A variable to contain the Y errors.
;
; History:
;	Original: 12/7/12
;-

  on_error, 2                   ; Return to caller on error

  if n_params() eq 0 then begin
     message, "Must specify a file"
     return
  endif

  gr_state, /save

;	Open the file

@graff_version

  f0 = file
  graff_init, pdefs, f0, version = version
  igot = graff_get(pdefs, f0, /no_set, /no_warn)
  if igot ne 1 then begin       
     message, "Failed to open: "+f0
     return
  endif

  if n_params() eq 2 then begin
     if keyword_set(name) then begin
        message, /continue, $
                 "May not specify dataset by index and by name"
        return
     endif
     index = idx-1
  endif else if keyword_set(name) then begin
     locs = where((*pdefs.data).descript eq name, nname)
     if nname eq 0 then begin
        message, /continue, "No match for name "+name+" found in "+file
        return
     endif

     if nname gt 1 then message, /continue, $
                                 "Multiple matches for name "+name+ $
                                 " found in "+file
     index = locs[0]
  endif else index = pdefs.cset

  if index lt 0 or index ge pdefs.nsets then begin
     message, "Dataset index out of range", /continue
     return
  endif

  data = (*pdefs.data)[index]

  if not ptr_valid(data.xydata) then return
  if data.type lt 0 then return ; Not applicable to functions.
  
  xydata = *data.xydata
  if data.type eq 9 then begin
     zval = *xydata.z
     xval = *xydata.x
     yval = *xydata.y
  endif else begin 
     xval = reform(xydata[0, *])
     yval = reform(xydata[1, *])

     case data.type of
        1: yerr = reform(xydata[2, *])
        3: xerr = reform(xydata[2, *])
        2: yerr = transpose(xydata[2:3, *])
        4: xerr = transpose(xydata[2:3, *])
        5: begin
           xerr = reform(xydata[2, *])
           yerr = reform(xydata[3, *])
        end

        6: begin
           xerr = reform(xydata[2, *])
           yerr = transpose(xydata[3:4, *])
        end

        7: begin
           xerr = transpose(xydata[2:3, *])
           yerr = reform(xydata[4, *])
        end

        
        8: begin
           xerr = transpose(xydata[2:3, *])
           yerr = transpose(xydata[4:5, *])
        end
        else:
     endcase
  endelse

  graff_clear, pdefs
  gr_state

end
