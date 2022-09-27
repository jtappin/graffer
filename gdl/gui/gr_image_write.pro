; LICENCE:
; Copyright (C) 2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; GR_IMAGE_WRITE
;	Replacement for dialog_write_image.
;
; Usage:
;	gr_image_write, image[, r, g, b]
;
; Arguments:
;	image	byte	The image to write (3×n×m) or (n×m)
;	r,g,b	byte	A colour table for false colour images.
;
; Keyword:
;	name	string	The default name stem for the output file.
;
; History:
; 	Original: 15/9/21; SJT
;-

function gr_im_wrt_event, event

  widget_control, event.id, get_uvalue = uv

  if uv ne 'SELECT' then return, 0l
  
  event.id = event.handler
  event.handler = 0l
  return, event
  
end

pro gr_image_write, image, r, g, b, name = name, group = group

  sz = size(image)
  case sz[0] of
     2: begin
        allowed = ['png', 'jpeg', 'tiff', 'bmp', 'gif']
        filter = ['.png, .PNG', '.jpg, .JPG, .jpeg, .JPEG', $
                  '.tif, .tiff, .TIF, .TIFF', '.bmp .BMP', $
                  '.gif .GIF']
     end        
     3: begin
        allowed = ['png', 'jpeg', 'tiff', 'bmp']
        filter = ['.png, .PNG', '.jpg, .JPG, .jpeg, .JPEG', $
                  '.tif, .tiff, .TIF, .TIFF', '.bmp .BMP']
     end
     
     else: message, "IMAGE must be a 2 or 3 dimensional array."
  endcase

  if keyword_set(name) then begin
     bname = file_basename(name)
     if bname ne name then path = file_dirname(name)
  endif
  
  if keyword_set(group) &&  widget_info(group, /valid) then $
     widget_control, group, sensitive = 0
  
  base = widget_base(/column, $
                     group = group)

  junk = widget_label(base, $
                      value = "Graffer screen dump")

  selid = gr_cw_filesel(base, $
                        filter = filter, $
                        /save, $
                        /warn, $
                        default = bname, $
                        path = path, $
                        label = allowed, $
                        /fix_filter, $
                        uvalue = 'SELECT')

  widget_control, base, /real, event_func = 'gr_im_wrt_event'

  repeat begin
     ev = widget_event(base)
  endrep until (ev.done ne 0)

  help, /str, ev
  if ev.done eq 1 then begin
     case ev.label of
        'png': if n_params() eq 4 then $
           write_png, ev.value, image, r, g, b $
        else write_png, ev.value, image

        'jpeg': write_jpeg, ev.value, image, true = sz[0] eq 3

        'tiff': if n_params() eq 4 then $
           write_tiff, ev.value, reverse(image, 2), $
                       red = r, green = g, blue = b $
        else write_tiff, ev.value, reverse(image, 3)

        'bmp': if n_params() eq 4 then $
           write_bmp, ev.value, image, r, g, b $
        else write_bmp, ev.value, image

        'gif': if n_params() eq 4 then $
           write_gif, ev.value, image, r, g, b $
        else write_gif, ev.value, image
     endcase
  endif

  widget_control, ev.top, /destroy
  if keyword_set(group) &&  widget_info(group, /valid) then $
     widget_control, group, /sensitive

end
