;+
; GR_CVTTOBM
;	Convert a normal byte greyscale image into a binary bitmap for
;	button labels.
;
; Usage:
;	bitmap = gr_cvttobm(bytemap)
;
; Returns:
;	A bitmap suitable to use as a button label.
;
; Argument:
;	bytemap	byte	The image to convert.
;
; Keyword:
;	threshold	byte	The threshold for the levels. Anything
;				above this is returned as 1, below as
;				0. If not given then the mean of the
;				bytemap is used.
;
; History:
;	Original: 16/8/21; SJT
;-

function gr_cvttobm, bytemap, threshold = threshold

  on_error, 2
  
  sz = size(bytemap)

  if sz[-2] ne 1 then message, "GR_CVTTOBM can only convert byte arrays."
  if sz[0] ne 2 then message, "GR_CVTTOBM can only convert 2-D arrays."

  ny = sz[2]
  nx = ceil(sz[1]/8.)

  if n_elements(threshold) eq 0 then threshold = mean(bytemap)

  rv = bytarr(nx, ny)

  tmap = bytarr(nx*8, ny)
  tmap[0, 0] = reverse(bytemap, 2)
  for j = 0, 7 do rv += 2b^j * (tmap[j:*:8, *] gt threshold)

  return, rv

end
