; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

; $Id: errplot.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Pro Gr_err_y, X, Y, Low, High, Width = width, min_value = min_value, $
              max_value = max_value, _extra = extra

;+
; NAME:
;	ERRPLOT
;
; PURPOSE:
;	Plot error bars over a previously drawn plot.
;
; CATEGORY:
;	J6 - plotting, graphics, one dimensional.
;
; CALLING SEQUENCE:
;	gr_err_y, x, y, low, high	;To explicitly specify abscissae.
;
; INPUTS:
;	X:	A vector with the abscissæ
;	Y:	A vector with the data values
;	Low:	A vector of lower estimates, equal to  - error.
;	High:	A vector of upper estimates, equal to  + error. (If
;		omitted, then +/- errors are equal.
;
; KEYWORD Parameters:
;	WIDTH:	The width of the error bars.  The default is 1% of plot width.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	An overplot is produced.
;
; RESTRICTIONS:
;	Logarithmic restriction removed.
;
; PROCEDURE:
;	Error bars are drawn for each element.
;
; EXAMPLES:
;	To plot symmetrical error bars where Y = data values and 
;	ERR = symmetrical error estimates, enter:
;
;		PLOT, Y			;Plot data
;		ERRPLOT, Y-ERR, Y+ERR	;Overplot error bars.
;
;	If error estimates are non-symetrical, enter:
;
;		PLOT,Y
;		ERRPLOT, Upper, Lower	;Where Upper & Lower are bounds.
;
;	To plot versus a vector of abscissae:
;
;		PLOT, X, Y		  ;Plot data (X versus Y).
;		ERRPLOT, X, Y-ERR, Y+ERR  ;Overplot error estimates.
;
; MODIFICATION HISTORY:
;	DMS, RSI, June, 1983.
;
;	Joe Zawodney, LASP, Univ of Colo., March, 1986. Removed logarithmic
;	restriction.
;
;	DMS, March, 1989.  Modified for Unix IDL.
;	SJT (Sep 1995) Add _EXTRA keyword (call gr_errplot)
;	SJT, June 1996. Change arguments and modify to allow plotting
;	of limits as well as real values.
;	Shorten name: 25/11/96; SJT
;-

  if (n_params() eq 4) then up = y+high $
  else up = y+low
  down = y-low
  xx = x

;	Check if we have limits

  ul = finite(up) eq 0
  ll = finite(down) eq 0
  islim = ul or ll

  locs = where(ul, nl)
  if (nl ne 0) then up(locs) = y(locs)
  locs = where(ll, nl)
  if (nl ne 0) then down(locs) = y(locs)


  if n_elements(width) eq 0 then width = .01 ;Default width
  width = width/2                            ;Centered

;
  n = n_elements(up) < n_elements(down) < n_elements(xx) ;# of pnts

  xxmin = min(!X.crange)        ;X range
  xxmax = max(!X.crange)
  yymax = max(!Y.crange)        ;Y range
  yymin = min(!Y.crange)


  wid = (xxmax - xxmin) * width       ; bars = .01 of plot wide.
  ywid = (yymax -yymin) * (width > .01) ; Shift for limits
  xwid = (xxmax -xxmin) * (width > .01) ; Shift for limits

  for i = 0, n-1 do begin       ;do each point.
     
     if keyword_set(min_value) && y[i] lt min_value then continue
     if keyword_set(max_value) && y[i] gt max_value then continue

     if (!X.type) then begin
        if (xx(i) gt 0) then begin
           xxx = alog10(xx(i))  ;x value
           xeb = 10^[xxx-wid, xxx, xxx+wid, xxx, xxx, xxx-wid, xxx, $
                     xxx+wid]
           if (islim(i)) then  $
              xebl = 10^[xxx-xwid, xxx, xxx+xwid, xxx, xxx, xxx-xwid, $
                         xxx, xxx+xwid]
        endif else continue
     endif else begin
        xxx = xx(i)             ;x value
        xeb = [xxx-wid, xxx, xxx+wid, xxx, xxx, xxx-wid, xxx, xxx+wid]
        if (islim(i)) then  $
           xebl = [xxx-xwid, xxx, xxx+xwid, xxx, xxx, xxx-xwid, xxx, $
                   xxx+xwid]
     endelse
     
     if (xxx ge xxmin) and (xxx le xxmax) then begin
        if (ll(i)) then begin
           ddown = [4, 5, 4, 5]*ywid
           xeb(0:3) = xebl(0:3)
        endif else ddown = [0, 0, 0, 0]
        if (ul(i)) then begin
           dup = [5, 4, 5, 4]*ywid
           xeb(4:7) = xebl(4:7)
        endif else dup = [0, 0, 0, 0]
        
        if (!Y.type) then begin
           yyy0 = alog10(down(i))
           yyy1 = alog10(up(i))
           yeb = 10^[yyy0-ddown, yyy1+dup]
        endif else begin
           yyy0 = down(i)
           yyy1 = up(i)
           yeb = [yyy0-ddown, yyy1+dup]
        endelse
        
        
        plots, xeb, yeb, _extra = extra
     endif
     
  endfor
  return
end

