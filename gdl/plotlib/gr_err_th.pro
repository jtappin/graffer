; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

; $Id: errplot.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

Pro Gr_err_th, r, th, Low, High, Width=width, mode=mode, _extra=extra

;+
; NAME:
;	ERRPLOT
;
; PURPOSE:
;	Plot radial error bars over a previously drawn plot.
;
; CATEGORY:
;	J6 - plotting, graphics, one dimensional.
;
; CALLING SEQUENCE:
;	gr_err_th, r, th, low, high	;To explicitly specify abscissae.
;
; INPUTS:
;	R:	A vector with the radii
;	Y:	A vector with the data values
;	Low:	A vector of lower estimates, equal to  - error.
;	High:	A vector of upper estimates, equal to  + error. (If
;		omitted, then +/- errors are equal.
;
; KEYWORD Parameters:
;	WIDTH:	The width of the error bars.  The default is 0.5 degree.
;	mode:	1 for radians, 2 for degrees
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
;-

if (!X.type or !Y.type) then return ; r & theta errors not allowed in
                                ; log plots.

if (n_params() eq 4) then up = th+high $
else up = th+low
down = th-low

yy = th
xx = r

if (mode eq 2) then begin
    yy = yy*!Dtor
    up = up*!Dtor
    down = down*!Dtor
endif

;	Check if we have limits

ul = finite(up) eq 0
ll = finite(down) eq 0
islim = ul or ll

locs = where(ul, nl1)
if (nl1 ne 0) then up(locs) = r(locs)
locs = where(ll, nl2)
if (nl2 ne 0) then down(locs) = r(locs)
if (nl1+nl2 ne 0) then message, /continue, "Limits not supported in " + $
  "angular errors"

if n_elements(width) eq 0 then width = .01 ;Default width

width = width/2                 ;Centered

;
n = n_elements(up) < n_elements(down) < n_elements(xx) ;# of pnts

xxmin = min(!X.crange)          ;X range
xxmax = max(!X.crange)
yymax = max(!Y.crange)          ;Y range
yymin = min(!Y.crange)

rrg = (xxmax-xxmin) < (yymax-yymin)

wid = width*rrg

for i = 0, n-1 do begin         ;do each point.
    
    xxx = xx(i)                 ;x value
    reb = [xxx-wid, xxx+wid, xxx, xxx, xxx, xxx-wid, xxx+wid]
    
        
    teb = [replicate(down(i), 3), yy(i), replicate(up(i), 3)]
        
    gr_pol_rect, reb, teb, xeb, yeb
    
    plots, xeb, yeb, _extra = extra
    
Next:
    
endfor
return
end
