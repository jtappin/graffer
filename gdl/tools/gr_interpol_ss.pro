;+
; GR_INTERPOL_SS
;	A wrapper for interpol, that copes with monotonically
;	decreasing inputs as well as monotonically increasing values.
;
; Usage:
;	vp = gr_interpol_ss(v, x, xout)
;
; Returns:
;	The values of V interpolated to Xout.
;
; Arguments:
;	v	real	The array to be interpolated
;	x	real	The locations at which v is tabulated.
;	xout	real	The locations of the interpolates.
;
; Keywords:
;	Any keywords accepted by interpol are passed directly.
;
; History:
;	Original: 27/9/21; SJT
;-

function gr_interpol_ss, v, x, xout, _extra = _extra

  dx = x[1:*]-x
  locs = where(dx lt 0, nn)
  if nn eq n_elements(dx) then begin
     vp = reverse(v)
     xp = reverse(x)
  endif else if nn eq 0 then begin
     vp = v
     xp = x
  endif else message, "X must be monotonic."

  return, interpol(vp, xp, xout, _extra = _extra)

end
