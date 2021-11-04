; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_bin_txt_v3, text, nset, ilu, msgid, version, template=template

;+
; GR_BIN_TXT
;	Read an individual Text string from a binary V2.x GRAFFER file.
;
; Arguments:
;	text	struct	in/out	The graffer text string structure
;	nset	int	input	The serial number of the current
;				dataset.
;	ilu	int	input	File unit number to read
;	msgid	long	input	ID of message window (if created).
;	version	int[2]	input	Version of dataset begin read.	
;
; Keyword:
;	template	input	If set, then we are getting a template
;
;
; History:
;	Original (binary version): 15/1/97; SJT
;	Add version argument and convert data to doubles: 30/6/05; SJT
;	Add axis choice: 28/12/11; SJT
;	Remove obsolete swap keyword: 6/1/12; SJT
;	New font handling: 11/1/12; SJT
;-

single = version[0] eq 2

tag = '   '
while (not eof(ilu)) do begin
    
    readu, ilu, tag

    case (strtrim(tag)) of
        
                                ; Recognized tags for Text items
                                ; C - colour
                                ; S - character size
                                ; O - orientation (degrees
                                ;     anticlockwise from the normal
                                ;     L->R orientation.
                                ; A - justification (from 0 (left
                                ;     aligned) to 1 (right aligned))
                                ; F - Font (IDL hershey font number).
                                ; W - line thickness to draw with.
                                ; X,Y - position of anchor (not used
                                ;       in template).
                                ; N - is the above in normalized or
                                ;     data coordinates.
                                ; T - The actual string (not used in
                                ;     template)
                                ; AX - Which Y axis to use (if
                                ;      multiple axes are in use)
                                ; TE, TTE - End
        
        'C': text[nset].colour = gr_int_rd(ilu, 1)
        'S': text[nset].size = gr_flt_rd(ilu, 1)
        'O': text[nset].orient = gr_flt_rd(ilu, 1)
        'A': text[nset].align = gr_flt_rd(ilu, 1)
        'F': begin
            font = gr_int_rd(ilu, 1)
            if font ge 3 then begin
                text[nset].font = font
                text[nset].ffamily = -1
            endif else begin
                text[nset].font = 3 ; Default HW font
                text[nset].ffamily = 0 ; There was no TT before V4
            endelse
        end
        'W': text[nset].thick = gr_int_rd(ilu, 1)
        'X': text[nset].x = gr_dbl_rd(ilu, 1, single = single) 
        'Y': text[nset].y = gr_dbl_rd(ilu, 1, single = single)
        'N': text[nset].norm = gr_byt_rd(ilu, 1)
        
        'T': text[nset].text = gr_str_rd(ilu)
        'AX': text[nset].axis = gr_int_rd(ilu, 1)

        'TE': return
        'TTE': return
        
        Else: begin
            graff_msg, msgid, "Unknown text tag: " + $
                       tag + "Ignoring."
            stop
        end
    endcase
endwhile


end
