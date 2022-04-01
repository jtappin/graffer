;+
; GR_UPDATE_Z
;	Update the data values for a 2-D dataset.
;
; Usage:
;	ok = gr_update_z(data, z_values, x_values, y_values, retain)
;
; Return value:
;	1 for success, 0 for failure.
;
; Arguments:
;	data	struct	The dataset data structure.
;	z_values double	The new Z values, must be a 2-D array.
;	x_values double	The new X values, 1 or 2-D array
;	y_values double	The new Y values, 1 or 2-D array.
;	retain	bool	Whether to retain unspecified X or Y values.
;
; History:
;	Extract from GRAFF_UPDATE: 1/4/22; SJT
;-

function gr_update_z, data, z_values, x_values, y_values, retain

; Sanity checks
  
  zflag = n_elements(z_values) ne 0
  if ~zflag && ~retain then begin
     message, /continue, $
              "Must specify Z values unless /RETAIN_UNSET is " + $
              "specified."
     return, 0
  endif
  if zflag then begin
     sz = size(z_values)
     if sz[0] ne 2 then begin
        message, /continue, $
                 "Z_VALUES must be a 2-D array."
        return, 0
     endif
     nz = sz[1:2]
  endif else nz = lonarr(2)

  xflag = n_elements(x_values) ne 0
  if xflag then begin
     sx = size(x)
     case sx[0] of
        1: nx = [sx[1], 0]
        2: nx = sx[1:2]
        else: begin
           message, /continue, $
                    "X_VALUES must be a 1 or 2-D array"
           return, 0
        end
     endcase
  endif else nx = lonarr(2)
           
  yflag = n_elements(y_values) ne 0
  if yflag then begin
     sy = size(y)
     case sy[0] of
        1: ny = [0, sy[1]]
        2: if sy[1] eq 1 then ny = [0, sy[2]] $
        else ny = sy[1:2]
        else: begin
           message, /continue, $
                    "Y_VALUES must be a 1 or 2-D array"
           return, 0
        end
     endcase
  endif else ny = lonarr(2)
           
  if retain then begin
     xydata = *(data.xydata)
     
     if zflag then begin
        if nz[0] ne data.ndata || nz[1] ne data.ndata2 then begin
           message, /continue, $
                    "If /RETAIN_UNSET is specified Z_VALUES may " + $
                    "not change the dataset size."
           return, 0
        endif
        ptr_free, xydata.z
        xydata.z = ptr_new(z_values)
     endif

     if xflag then begin 
        if nx[0] ne data.ndata || (nx[1] ne data.ndata2 && $
                                   nx[1] ne 0) then begin
           message, /continue, $
                    "If /RETAIN_UNSET is specified X_VALUES may " + $
                    "not change the dataset size."
           return, 0
        endif
        ptr_free, xydata.x
        xydata.x = ptr_new(x_values)

        xydata.x_is_2d = nx[1] gt 0
     endif
     if yflag then begin 
        if (ny[0] ne data.ndata && ny[0] ne 0) || $
           ny[1] ne data.ndata2 then begin
           message, /continue, $
                    "If /RETAIN_UNSET is specified Y_VALUES may " + $
                    "not change the dataset size."
           return, 0
        endif
        ptr_free, xydata.y
        xydata.y = ptr_new(y_values)

        xydata.y_is_2d = ny[0] gt 0
     endif

     ptr_free, data.xydata
     data.xydata = ptr_new(xydata)
     return, 1

  endif

  xydata = {graff_zdata}

  if xflag then begin
     if nx[0] ne nz[0] || (nx[1] ne nz[1] && nx[1] ne 0) then begin
        message, /continue, $
                 "X array must be same length as X dimension of " + $
                 "Z or the same size as Z."
        return, 0
     endif
     xydata.x = ptr_new(x_values)
     xydata.x_is_2d = nx[1] gt 0
  endif else begin
     xydata.x = ptr_new(dindgen(nz[0]))
     xydata.x_is_2d = 0b
  endelse

  if yflag then begin
     if (ny[0] ne nz[0] && ny[0] ne 0) || ny[1] ne nz[1] then begin
        message, /continue, $
                 "X array must be same length as X dimension of " + $
                 "Z or the same size as Z."
        return, 0
     endif
     xydata.y = ptr_new(reform(y_values)) ; Ensures 1xn is correctly
                                ; stored.
     xydata.y_is_2d = ny[0] gt 0
  endif else begin
     xydata.y = ptr_new(dindgen(nz[1]))
     xydata.y_is_2d = 0b
  endelse

  xydata.z = ptr_new(z_values)
  
  data.ndata = nz[0]
  data.ndata2 = nz[1]
  
  ptr_free, *(data.xydata).x, $
            *(data.xydata).y, $
            *(data.xydata).z, $
            data.xydata

  data.xydata = ptr_new(xydata)

  return, 1
  
end
