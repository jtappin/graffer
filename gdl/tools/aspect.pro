; LICENCE:
; Copyright (C) 1996: D Fanning
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

;+
; NAME:
;       ASPECT
;
; PURPOSE:
;       This function calculates and returns the normalized position
;       coordinates necessary to put a plot with a specified aspect ratio
;       into the currently active graphics window. It works on the display
;       output window as well as in a PostScript output window.
;
; CATEGORY:
;       Graphics
;
; CALLING SEQUENCE:
;
;       position = ASPECT(aspectRatio)
;
; INPUTS:
;       aspectRatio: A floating point value that is the desired aspect
;       ratio (ratio of heigth to width) of the plot in the current 
;       graphics output window. If this parameter is missing, an aspect
;       ratio of 1.0 (a square plot) is assumed.
;
; KEYWORD PARAMETERS:
;       MARGIN: The margin around the edges of the plot. The value must be
;       a floating point value between 0.0 and 0.5. It is expressed in
;       normalized coordinate units. The default margin is 0.15.
;
; OUTPUTS:
;       position: A four-element floating array of normalized coordinates.
;       The order of the elements is [x0, y0, x1, y1], similar to the
;       !P.POSITION system variable or the POSITION keyword on any IDL
;       graphic command.
;
; EXAMPLE:
;       To create a plot with an aspect ratio of 1:2 and a margin of
;       0.10 around the edge of the output window, do this:
;
;          plotPosition = ASPECT(0.5, Margin=0.10)
;          PLOT, Findgen(11), POSITION=plotPosition
;       
;       Notice this can be done in a single IDL command, like this:
;       
;          PLOT, Findgen(11), POSITION=ASPECT(0.5, Margin=0.10)
;
; MODIFICATION HISTORY:
;       Written by:     David Fanning, Novermber 1996.
;-

FUNCTION ASPECT, aspectRatio, MARGIN=margin

ON_ERROR, 1

   ; Check for aspect ratio parameter.
   
IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0

   ; Check for margins.
   
IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15

   ; Error checking.
   
IF margin LT 0 OR margin GE 0.5 THEN $
   MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'
 
   ; Calculate the aspect ratio of the current window.
   
wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Calculate normalized positions in window.
   
IF (aspectRatio LE wAspectRatio) THEN BEGIN
   xstart = margin
   ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
   xend = 1.0 - margin
   yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
ENDIF ELSE BEGIN
   xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
   ystart = margin
   xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
   yend = 1.0 - margin
ENDELSE

position = [xstart, ystart, xend, yend]

RETURN, position

end
