; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

; $Id: errplot.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Pro Gr_err_x, X, Y, Low, High, Width = width, min_value = min_value, $
              max_value = max_value, _extra = extra

;+
; NAME:
;	ERRPLOT
;
; PURPOSE:
;	Plot horizontal error bars over a previously drawn plot.
;
; CATEGORY:
;	J6 - plotting, graphics, one dimensional.
;
; CALLING SEQUENCE:
;	gr_err_x, x, y, low, high	;To explicitly specify abscissae.
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
;	SJT, Nov 1996, Horizontal version.
;	Shorten name: 25/11/96; SJT
;	Support min & max values: 28/7/15; SJT
;-

  if (n_params() eq 4) then up = x+high $
  else up = x+low
  down = x-low
  yy = y

;	Check if we have limits

  ul = finite(up) eq 0
  ll = finite(down) eq 0
  islim = ul or ll

  locs = where(ul, nl)
  if (nl ne 0) then up(locs) = x(locs)
  locs = where(ll, nl)
  if (nl ne 0) then down(locs) = x(locs)


  if n_elements(width) eq 0 then width = .01 ;Default width
  width = width/2                            ;Centered

;
  n = n_elements(up) < n_elements(down) < n_elements(yy) ;# of pnts

  xxmin = min(!X.crange)        ;X range
  xxmax = max(!X.crange)
  yymax = max(!Y.crange)        ;Y range
  yymin = min(!Y.crange)


  wid = (yymax - yymin) * width       ; bars = .01 of plot wide.
  ywid = (yymax -yymin) * (width > .01) ; Shift for limits
  xwid = (xxmax -xxmin) * (width > .01) ; Shift for limits

  for i = 0, n-1 do begin       ;do each point.
     
     if keyword_set(min_value) && yy[i] lt min_value then continue
     if keyword_set(max_value) && yy[i] gt max_value then continue

     if (!y.type) then begin
        if (yy(i) gt 0) then begin
           yyy = alog10(yy(i))  ;y value
           yeb = 10^[yyy-wid, yyy, yyy+wid, yyy, yyy, yyy-wid, yyy, $
                     yyy+wid]
           if (islim(i)) then  $
              yebl = 10^[yyy-ywid, yyy, yyy+ywid, yyy, yyy, yyy-ywid, $
                         yyy, yyy+ywid]
        endif else continue
     endif else begin
        yyy = yy(i)             ;x value
        yeb = [yyy-wid, yyy, yyy+wid, yyy, yyy, yyy-wid, yyy, yyy+wid]
        if (islim(i)) then  $
           yebl = [yyy-ywid, yyy, yyy+ywid, yyy, yyy, yyy-ywid, yyy, $
                   yyy+ywid]
     endelse
     
     if (yyy ge yymin) and (yyy le yymax) then begin
        if (ll(i)) then begin
           ddown = [4, 5, 4, 5]*xwid
           yeb(0:3) = yebl(0:3)
        endif else ddown = [0, 0, 0, 0]
        if (ul(i)) then begin
           dup = [5, 4, 5, 4]*xwid
           yeb(4:7) = yebl(4:7)
        endif else dup = [0, 0, 0, 0]
        
        if (!x.type) then begin
           xxx0 = alog10(down(i))
           xxx1 = alog10(up(i))
           xeb = 10^[xxx0-ddown, xxx1+dup]
        endif else begin
           xxx0 = down(i)
           xxx1 = up(i)
           xeb = [xxx0-ddown, xxx1+dup]
        endelse
        
        
        plots, xeb, yeb, _extra = extra
     endif
  endfor

  return
end
