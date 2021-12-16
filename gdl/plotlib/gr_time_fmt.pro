; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

function Gr_time_fmt, axis, index, value, options = options, range = range

;+
; GR_TIME_FMT
;	Formatter for X-axis to display in time values.
;
; Usage:
;	plot, t, y, xtickformat='gr_time_fmt' ; To do the labelling
;	gr_time_format, axis, options=options, range=range ; initialize
;
; Return Value:
;	A string with the time in it.
;
; Arguments:
;	axis	int	input	The Axis (0=x, 1=y, Z not supported).
;	index	int	input	The tick index (ignored)
;	value	float	input	The value of the time at the tick.
;
; Keywords:
;	options	struct	input	The PDEFS.[xy]STYLE structure.
;	range	float	input	The axis range.
;
; Restrictions:
;	This routine only performs a 1-step decomposition. I.E. it
;	assumes that either the time is in hours and you want hh:mm
;	format or that it is in minutes and you want mm:ss
;	format. Winding into days (or hours) is not performed.
;
; History:
;	Original: 10/5/96; SJT
;	Renamed as GR_TIME_FMT (was time_format): 18/9/96
;-

  common Graff_time_options, xopt, xr, xz, yopt, yr, yz, top_last
  
  if (n_elements(options) ne 0) then begin ; Setting up
     if (axis eq 0) then begin
        xopt = options.time
        xz = options.tzero
        xr = range
     endif else if (axis eq 1) then begin
        yopt = options.time
        yz = options.tzero
        yr = range
     endif
     
     vs = ''
     
     top_last = !Pi             ; Any non integer will do actually!
     
  endif else begin
     if (axis eq 0) then begin
        unit = (xopt/2) and 3
        munit = (xopt/8) and 3
        ar = xr
        zero = xz
     endif else if (axis eq 1) then begin
        unit = (yopt/2) and 3
        munit = (yopt/8) and 3
        ar = yr
        zero = yz
     endif
     
     ucf = [1./3600., 1./60., 1.0, 24.]
     cf = ucf(unit)/ucf(munit)
     t = value*cf
     
     case munit of
        0: begin
           if (ar(1)-ar(0))*ucf(unit)*3600. gt 5. then  $
              vs = string(t+zero, format = "(I0)") $
           else vs = strtrim(string(t+zero, format = "(F4.1)"), 2)
        end
        1: begin
           m = floor(t)
           s = round(60*(t-m))
           if (index eq 0 or m ne top_last) then  $
              vs = string(m+zero, s, format = "(I0.2,':',I2.2)") $
           else vs = string(s, format = "(I2.2)")
           top_last = m
        end
        2: begin
           h = floor(t)
           m = 60.*(t-h)
           if (index eq 0 or h ne top_last) then begin
              if (ar(1)-ar(0))*ucf(unit) gt 1. then $
                 vs = string(h+zero, round(m), format = "(I0.2,':',I2.2)") $
              else begin
                 s = 60*(m-fix(m))
                 vs = string(h+zero, fix(m), round(s), $
                             format = "(I0.2,':',I2.2,':',I2.2)")
              endelse
           endif else begin
              if (ar(1)-ar(0))*ucf(unit) gt 1. then $
                 vs = string(round(m), format = "(I2.2)") $
              else begin
                 s = 60*(m-fix(m))
                 vs = string(fix(m), round(s), $
                             format = "(I2.2,':',I2.2)")
              endelse
           endelse
           top_last = h
        end
        3: begin
           d = floor(t)
           h = 24.*(t-d)
           if (index eq 0 or d ne top_last) then begin
              if (ar(1)-ar(0))*ucf(unit)/24. gt 1. then $
                 vs = string(d+zero, round(h), format = "(I0,'/',I2.2)") $
              else begin
                 m = 60.*(h-fix(h))
                 vs = string(d+zero, fix(h), round(m), $
                             format = "(I0,'/',I2.2,':',I2.2)")
              endelse
           endif else begin
              if (ar(1)-ar(0))*ucf(unit)/24. gt 1. then $
                 vs = string(round(h), format = "(I2.2)") $
              else begin
                 m = 60.*(h-fix(h))
                 vs = string(fix(h), round(m), $
                             format = "(I2.2,':',I2.2)")
              endelse
           endelse
           top_last = d
        end
     endcase
  endelse

  return, vs

end
