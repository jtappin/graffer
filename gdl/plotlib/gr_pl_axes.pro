; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro gr_pl_axes, pdefs, csiz, overlay = overlay, $
                secondary = secondary, $
                setup = setup

;+
; GR_PL_AXES
;	Plot axes for GRAFFER
;
; Usage:
;	gr_pl_axes, pdefs, csiz[, /overlay]
;
; Argument:
;	pdefs	struct	input	The ubiqitous GRAFFER control
;				structure
;	csiz	float	input	Charsize scaling (hardcopy only).
;
; Keyword:
;	/overlay	input	If set, then don't erase before plotting
;				(used to replace the ticks afer doing
;				a colour plot).
;	/setup		input	If set, then don't do any
;				drawing at all, just establish the
;				coordinates (basically GDL ignores !x,
;				!y and does what IDL says it does).
;
; History:
;	Cut from GR_PLOT_OBJECT and overlay added: 11/12/96; SJT
;	Made unique in 8.3: 11/2/97; SJT
;	Add isotropic option: 25/6/08; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add annotation suppression, tidy origin axis settings and fix
;	Y-time labelling: 12/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Don't plot annotations if overlay: 2/10/17; SJT
;	Add /setup, and tidy up logical vs. bitwiase ops: 13/12/21; SJT
;-

  lcolor = 0l

  if ~keyword_set(setup) then begin ; Should be safe to leave
                                ; undefined if not drawing.
     if pdefs.xsty.time and 1 then begin
        xtf = 'gr_time_fmt'
        junk = gr_time_fmt(0, range = pdefs.xrange, options = $
                           pdefs.xsty)
     endif else xtf = pdefs.xsty.format

     if keyword_set(secondary) then begin
        if pdefs.ysty_r.time and 1 then begin
           ytf = 'gr_time_fmt'
           junk = gr_time_fmt(1, range = pdefs.yrange_r, options = $
                              pdefs.ysty_r)
        endif else ytf = pdefs.ysty_r.format
     endif else begin
        if pdefs.ysty.time and 1 then begin
           ytf = 'gr_time_fmt'
           junk = gr_time_fmt(1, range = pdefs.yrange, options = $
                           pdefs.ysty)
        endif else  ytf = pdefs.ysty.format
     endelse
  
     if pdefs.xsty.grid ne 0  && (pdefs.xsty.idl and 12) eq 0 then begin
        xtickl = 0.5
        xtickst = pdefs.xsty.grid-1
     endif else begin
        xtickl = 0.
        xtickst = 0
     endelse

     if keyword_set(secondary) && pdefs.y_right && $
        pdefs.ysty_r.grid ne 0 && $
        (pdefs.ysty_r.idl and 12) eq 0 then begin
        ytickl = 1.
        ytickst = pdefs.ysty_r.grid-1
     endif else if ~keyword_set(secondary) && $
                    pdefs.ysty.grid ne 0 && $
                    (pdefs.ysty.idl and 12) eq 0 then begin
        if pdefs.y_right then ytickl = 1.0 $
        else ytickl = 0.5
        ytickst = pdefs.ysty.grid-1
     endif else begin
        ytickl = 0.
        ytickst = 0
     endelse
  endif
  
  isotropic = pdefs.isotropic && ~pdefs.y_right
  mflag = pdefs.match && !d.name ne 'PS' && $
     widget_info(pdefs.ids.draw, /valid)

  if mflag then begin
     hs = pdefs.hardset.size
     ah = hs[0]/hs[1]
     dg = widget_info(pdefs.ids.draw, /geometry)
     as = dg.xsize/dg.ysize

     if as gt ah then begin     ; screen is more elongated in X, trim
                                ; X-region. 
        xd = (1.-ah/as)/2.
        !p.region = [xd, 0., 1.-xd, 1.] 
     endif else if ah gt as then begin ; Screen is more elongated in y,
                                ; trim Y region
        yd = (1.-as/ah)/2.
        !p.region = [0., yd, 1., 1.-yd]
     endif else mflag = 0b
     if pdefs.y_right then xmargin = [10, 10] $
     else xmargin = [10, 3]

  endif else !p.region = 0.

  if ~isotropic then begin
     if (pdefs.aspect(0) gt 0.) then $
        !P.position = aspect(pdefs.aspect(0), margin = pdefs.aspect(1)) $
     else if (pdefs.position(2) gt pdefs.position(0) and $
              pdefs.position(3) gt pdefs.position(1)) then begin
        !P.position = pdefs.position 
        if mflag then begin
           xr = !p.region[2]-!p.region[0]
           yr = !p.region[3]-!p.region[1]
           !p.position *= [xr, yr, xr, yr]
           !p.position += !p.region[[0, 1, 0, 1]]
        endif
     endif else if pdefs.y_right then xmargin = [10, 10] $
     else xmargin = [10, 3]
  endif

  if pdefs.y_right then xmargin = [10, 10] $
  else xmargin = [10, 3]

  if (pdefs.xsty.idl and 4) ne 0 then $
     xzat = pdefs.xtitle $
  else xzat = ''

  if ~pdefs.y_right then begin
     ysty = pdefs.ysty.idl
     xsty = pdefs.xsty.idl
     yrange = pdefs.yrange
     ytype = pdefs.ytype
     ytitle = pdefs.ytitle
     yminor = pdefs.ysty.minor
     if ptr_valid(pdefs.ysty.values) then begin
        ytvals = *pdefs.ysty.values
        ymajor = n_elements(ytvals)-1
     endif else ymajor = pdefs.ysty.major
     if (pdefs.ysty.extra and 4) ne 0 then ynames = replicate(' ', 30)
     if ((pdefs.ysty.idl and 4) ne 0) then yzat = pdefs.ytitle $
     else yzat = ''
  endif else if keyword_set(secondary) then begin
     ysty = pdefs.ysty_r.idl or 4
     xsty = pdefs.xsty.idl or 4
     yrange = pdefs.yrange_r
     ytype = pdefs.ytype_r
     ytitle = pdefs.ytitle_r
     yminor = pdefs.ysty_r.minor
     if ptr_valid(pdefs.ysty_r.values) then begin
        ytvals = *pdefs.ysty_r.values
        ymajor = n_elements(ytvals)-1
     endif else ymajor = pdefs.ysty_r.major
     if (pdefs.ysty_r.extra and 4) ne 0 then ynames = replicate(' ', 30)
     yzat = ''
  endif else begin
     ysty = pdefs.ysty.idl or 4
     xsty = pdefs.xsty.idl or 4
     yrange = pdefs.yrange
     ytype = pdefs.ytype
     ytitle = pdefs.ytitle
     yminor = pdefs.ysty.minor
     if ptr_valid(pdefs.ysty.values) then begin
        ytvals = *pdefs.ysty.values
        ymajor = n_elements(ytvals)-1
     endif else ymajor = pdefs.ysty.major
     if (pdefs.ysty.extra and 4) ne 0 then ynames = replicate(' ', 30)
     yzat = ''
  endelse
  if (pdefs.xsty.extra and 4) ne 0 then xnames = replicate(' ', 30)

  if ptr_valid(pdefs.xsty.values) then begin
     xtvals = *pdefs.xsty.values
     xmajor = n_elements(xtvals)-1
  endif else xmajor = pdefs.xsty.major

  noerase = (pdefs.y_right && keyword_set(secondary)) || $
            keyword_set(overlay) || keyword_set(setup)

  if keyword_set(overlay) || keyword_set(setup) then begin
     ynames = replicate(' ', 30)
     xnames = replicate(' ', 30)
     ytitle = ''
     xtitle = ''
     title = ''
     subtitle = ''
     xzat = ''
     yzat = ''
     if keyword_set(setup) then begin
        xsty or= 4
        ysty or= 4
     endif
  endif else begin
     xtitle = pdefs.xtitle
     title = pdefs.title
     subtitle = pdefs.subtitle
  endelse

  plot, /nodata, dblarr(2), title = title, subtitle = $
        subtitle, xrange = pdefs.xrange, xtitle = $
        xtitle, xlog = pdefs.xtype, xsty = xsty, $
        yrange = yrange, ytitle = ytitle, ylog = $
        ytype, ysty = ysty, charsize = $
        pdefs.charsize*csiz, xthick = pdefs.axthick, ythick = $
        pdefs.axthick, xminor = pdefs.xsty.minor, yminor = $
        yminor, xtickformat = xtf, xticklen = $
        xtickl, xgridsty = xtickst, yticklen = ytickl, ygridsty = $ 
        ytickst, noerase = noerase, isotropic = isotropic, xmargin = $
        xmargin, xtickname = xnames, ytickname = ynames, $
        ytickformat = ytf, xticks = xmajor, yticks = ymajor, $
        xtickv = xtvals, ytickv $
        = ytvals, color = lcolour

  if keyword_set(setup) then return
  
  if ~keyword_set(secondary) && mflag then $
     plots, /norm, !p.region[[0, 0, 2, 2, 0]], $
            !p.region[[1, 3, 3, 1, 1]], color = lcolour

  if pdefs.y_right then begin
     if keyword_set(secondary) then begin
        if (pdefs.ysty_r.idl and 8) eq 0 then $
           axis, yaxis = 1, yminor = yminor, ytitle = ytitle, ylog = $
                 ytype, ysty = pdefs.ysty_r.idl, ythick = $
                 pdefs.axthick, $
                 yticklen = ytickl, ygridsty = ytickst, yrange = $
                 yrange, $
                 ycharsize = pdefs.charsize*csiz, ytickname = ynames, $
                 yticks = ymajor, ytickv = ytvals, ytickformat = ytf, $
                 color = lcolour
        
     endif else begin
        if (pdefs.xsty.idl and 4) eq 0 then $
           axis, xaxis = 0, xminor = pdefs.xsty.minor, $
                 xtickformat = xtf, xrange = pdefs.xrange, $
                 xsty = pdefs.xsty.idl, xthick = $
                 pdefs.axthick, xtitle = xtitle, xticklen = $
                 xtickl, xgridsty = xtickst, xlog = pdefs.xtype, $
                 xcharsize = pdefs.charsize*csiz, xtickname = xnames, $
                 xticks = xmajor, xtickv = xtvals, $
                 color = lcolour
        if (pdefs.xsty.idl and (4 or 8)) eq 0 then $
           axis, xaxis = 1, xminor = pdefs.xsty.minor, $
                 xtickformat = xtf, xrange = pdefs.xrange, xsty = $
                 pdefs.xsty.idl, xthick = $ 
                 pdefs.axthick, xticklen = xtickl, $
                 xgridsty = xtickst, $
                 xlog = pdefs.xtype, xtickname = replicate(' ', 30), $
                 xcharsize = pdefs.charsize*csiz, xticks = xmajor, $
                 xtickv = xtvals, $
                 color = lcolour
        if (pdefs.ysty.idl and 4) eq 0 then $
           axis, yaxis = 0, yminor = yminor, ytitle = ytitle, ylog = $
                 ytype, ysty = pdefs.ysty.idl, ythick = pdefs.axthick, $
                 yticklen = ytickl, ygridsty = ytickst, yrange = $
                 yrange, $
                 ycharsize = pdefs.charsize*csiz, ytickname = ynames, $
                 ytickformat = ytf, yticks = ymajor, ytickv = ytvals, $
                 color = lcolour
     endelse
  endif

  if keyword_set(secondary) then begin
     if (pdefs.ysty.extra and 2) ne 0 || (pdefs.ysty_r.extra and 8) eq 0 $
     then ynames = replicate(' ', 30)
     if (pdefs.ysty_r.extra and 2) ne 0 then axis, yaxis = 1, 0., $
        pdefs.yrange_r(0), yrange = yrange, $
        ysty = pdefs.ysty_r.idl and (not 12), $
        ythick = pdefs.axthick, yminor = yminor, ytitle = $
        yzat, ytickname = ynames, ycharsize = pdefs.charsize*csiz, $
        yticks = ymajor, ytickv = ytvals, $
        color = lcolour
  endif else begin
     if pdefs.y_right && (pdefs.ysty_r.extra and 2) ne 0 or $
        (pdefs.ysty.extra and 8) eq 0 then ynames = $
        replicate(' ', 30)
     if (pdefs.xsty.extra and 8) eq 0 then xnames = replicate(' ', 30)
     if (pdefs.xsty.extra and 2) ne 0 && ~pdefs.y_right then $
        axis, xaxis = 0, pdefs.xrange(0), $
              0., xminor = pdefs.xsty.minor, xtickformat = xtf, $
              xrange = $
              pdefs.xrange, xsty = pdefs.xsty.idl and (not 12), $
              xthick = pdefs.axthick, xtitle = xzat, xtickname = $
              xnames, $
              xcharsize = pdefs.charsize*csiz, xticks = xmajor, xtickv $
              = xtvals, $
              color = lcolour
     if (pdefs.ysty.extra and 2) ne 0 then axis, yaxis = 0, 0., $
        pdefs.yrange(0), yrange = yrange, $
        ysty = pdefs.ysty.idl and (not 12), $
        ythick = pdefs.axthick, yminor = yminor, ytitle = $
        yzat, ytickname = ynames, ytickformat = ytf, ycharsize = $
        pdefs.charsize*csiz, yticks = ymajor, ytickv = ytvals, $
        color = lcolour
  endelse

end
