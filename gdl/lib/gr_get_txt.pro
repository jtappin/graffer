; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_get_txt, text, nset, ilu, msgid, template=template

;+
; GR_GET_TXT
;	Read an individual Text string from a V2.x GRAFFER file.
;
; Arguments:
;	text	struct	in/out	The graffer text string structure
;	nset	int	input	The serial number of the current
;				dataset.
;	ilu	int	input	File unit number to read
;	msgid	long	input	ID of message window (if created).
;
; Keyword:
;	template	input	If set, then we are getting a template
;				not a full text string.
;
; History:
;	Original: 5/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Add axis choice: 28/12/11; SJT
;-

inline = ''
ffflag = 0b

while (not eof(ilu)) do begin
    
    readf, ilu, inline
    tag_val = strsplit(inline, ':', /extr)
    for itag = 0, n_elements(tag_val) - 2, 2 do begin
        case (tag_val(itag)) of
            
                                ; Recognized tags for Text items
                                ; C - colour
                                ; CV - Custom colour.
                                ; S - character size
                                ; O - orientation (degrees
                                ;     anticlockwise from the normal
                                ;     L->R orientation.
                                ; A - justification (from 0 (left
                                ;     aligned) to 1 (right aligned))
                                ; FF - Font type
                                ; F - Font (IDL font number).
                                ; W - line thickness to draw with.
                                ; X,Y - position of anchor (not used
                                ;       in template).
                                ; N - is the above in normalized or
                                ;     data coordinates.
                                ; T - The actual string (not used in
                                ;     template)
                                ; TID - An ID tag for the text string.
                                ; AX - Which Y axis to use (if
                                ;      multiple axes are in use)
                                ; TE, TTE - End
            
            'C': text(nset).colour = gr_int_val(tag_val(itag+1), 1)
            'CV': text[nset].c_vals = gr_byt_val(tag_val(itag+1), 3)
            'S': text(nset).size = gr_flt_val(tag_val(itag+1), 1)
            'O': text(nset).orient = gr_flt_val(tag_val(itag+1), 1)
            'A': text(nset).align = gr_flt_val(tag_val(itag+1), 1)
            'FF': begin
                text[nset].ffamily = gr_int_val(tag_val(itag+1), 1)
                ffflag = 1b
            end
            'F': begin
                font = gr_int_val(tag_val(itag+1), 1)
                if font ge 3 then begin
                    text(nset).font = font
                    if ~ffflag then text[nset].ffamily = -1
                endif else if font eq 1 then begin ; TrueType
                    text(nset).font = 3
                    text[nset].ffamily = 1
                endif else begin
                    text(nset).font = 3
                    text[nset].ffamily = 0
                endelse
            end

            'W': text(nset).thick = gr_flt_val(tag_val(itag+1), 1)
            'X': begin
                if (keyword_set(template)) then graff_msg, msgid,  $
                  "Location not used in template." $
                else text(nset).x = gr_dbl_val(tag_val(itag+1), 1)
            end
            'Y': begin
                if (keyword_set(template)) then graff_msg, msgid,  $
                  "Location not used in template." $
                else text(nset).y = gr_dbl_val(tag_val(itag+1), 1)
            end
            'N': text(nset).norm = gr_byt_val(tag_val(itag+1), 1)
    
            'T': text(nset).text = gr_str_val(inline, 'T')

            'TID': text[nset].id = gr_str_val(inline, 'TID')

            'AX': text[nset].axis = gr_int_val(tag_val[itag+1], 1)

            'TE': begin
                if (keyword_set(template)) then graff_msg, msgid,  $
                  "WARNING template ended with TE tag."
                return
            end
            'TTE': begin
                if (not keyword_set(template)) then graff_msg, msgid,  $
                  "WARNING text ended with TTE tag."
                return
            end
        endcase
    endfor
endwhile

                
end
