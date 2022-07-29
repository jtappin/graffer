;+
; GR_NEAREST
;	Find the nearest data point to a click
;
; Usage:
;	gr_nearest, xp, yp, xc, yc, imin, md[, xdm, ydm]
;
; Arguments:
;	xp	double	Array of X-positions to check.
;	yp	double	Array of Y-positions to check
;	xc	long	The device X coordinate of the click
;	yc	long	The device Y coordanate of the click
;	imin	long	A variable for the index of the closest
;			point. 
;	md	double	A variable for the distance from the click to
;			the point in device pixels.
;	xdm	double	A variable for the device X location of the
;			nearest point.
;	ydm	double	A variable for the device Y location of the
;			nearest point.
;
; Keywords:
;	nsys	int	Specify the coordinate system for each input
;			position, 0 = data, 1 = NDC, 2 = Frame
;	max	double	A maximum distance threshold, if md is larger
;			than max then -1 and NaN are returned. For a
;			segment search, the default is 5.0
;	/segment	If set, then find the closest approach of the
;			line segments joining the points.
;
; Notes:
;	The default coordinates for XP & YP are data coordinates.
;	If /segment is set, then xdm and ydm are not set.
;
; History:
;	Original (from GRAFF_DRAW/WRITE): 29/4/22; SJT
;-

pro gr_nearest, xp, yp, xc, yc, imin, md, xdm, ydm, $
                nsys = nsys, max = max, $
                segment = segment
  
  gr_coord_convert, xp, yp, xd, yd, /data, /to_device
  if keyword_set(nsys) then begin
     locs = where(nsys eq 1, nn)
     if nn ne 0 then begin
        gr_coord_convert, xp[locs], yp[locs], xdt, ydt, $
                          /region, /to_device
        xd[locs] = xdt
        yd[locs] = ydt
     endif
     locs = where(nsys eq 2, nf)
     if nf ne 0 then begin
        gr_coord_convert, xp[locs], yp[locs], xdt, ydt, $
                          /frame, /to_device
        xd[locs] = xdt
        yd[locs] = ydt
     endif
  endif

; Finding the nearest point.
  
  if ~keyword_set(segment) then begin
     dist = sqrt((xd-xc)^2+(yd-yc)^2)

     md = min(dist, imin)

     if keyword_set(max) && md gt max then begin
        md = !values.d_nan
        imin = -1
        xdm = !values.d_nan
        ydm = !values.d_nan
     endif else begin
        xdm = xd[imin]
        ydm = yd[imin]
     endelse

; Finding the nearest line segment.
     
  endif else begin
     ndata = n_elements(xp)
     if ndata lt 2 then begin
        md = !values.d_nan
        imin = -1
        return
     endif

     off = dblarr(ndata-1)

     if keyword_set(max) then max = 5.d
     segmin = 2*max
     
     for j = 0, ndata-2 do begin
        xl = xd[j+1] < xd[j]
        xu = xd[j+1] > xd[j]
        if (xu - xl lt segmin) then begin
           tmp = (xl+xu)/2.
           xl = tmp-max
           xu = tmp+max
        endif
        yl = yd[j+1] < yd[j]
        yu = yd[j+1] > yd[j]
        if (yu - yl lt 10) then begin
           tmp = (yl+yu)/2.
           yl = tmp-max
           yu = tmp+max
        endif
        if (xc ge xl && xc le xu && $
            yc ge yl && yc le yu) then begin
           if (yd[j+1] eq yd[j]) then $
              off[j] = abs(yd[j]-yc) $
           else if (xd[j+1] eq xd[j]) then $
              off[j] = abs(xd[j]-xc) $
           else begin
              grad = (yd[j+1]-yd[j])/ $
                     (xd[j+1]-xd[j])
              yint = yd[j]-grad*xd[j]
              yp = xc*grad + yint
              xp = (yc-yint)/grad
              dy = yp-yc
              dx = xp-xc
              off[j] = abs(dx*dy)/sqrt(dx^2+dy^2)
           endelse
        endif else off[j] = !values.d_infinity
     endfor
     md = min(off, imin)

     if md gt max then begin
        imin = -1
        md = !values.d_nan
     endif else imin++          ; Imin is the point at the end of the
                                ; segment, so imin=0 can work as prepend.
  endelse
  
end
