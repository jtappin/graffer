; Copyright (C) 2013-2020
; James Tappin

; This is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3, or (at your option)
; any later version.

; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License along with
; this program; see the files COPYING3 and COPYING.RUNTIME respectively.
; If not, see <http://www.gnu.org/licenses/>.

pro Gr_bin_txt, text, nset, ilu, msgid, template=template

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
;
; Keyword:
;	template	input	If set, then we are getting a template
;	swap	If set, then the file is byte-swapped relative to the
;		platform
;
;
; History:
;	Original (binary version): 15/1/97; SJT
;	Add version argument and convert data to doubles: 30/6/05; SJT
;	Add axis choice: 28/12/11; SJT
;	V4 version: 6/1/12; SJT
;	New font handling: 11/1/12; SJT
;-

tag = '   '
ffflag = 0b

while (not eof(ilu)) do begin
    
    graff_get_rec, ilu, tag, value, tcode

    case (strtrim(tag)) of
        
                                ; Recognized tags for Text items
                                ; C - colour
                                ; CV - custom colour
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
                                ; TID - An ID string for the annotation.
                                ; AX - Which Y axis to use (if
                                ;      multiple axes are in use)
                                ; TE, TTE - End
        
        'C': text(nset).colour = value
        'CV': text[nset].c_vals = value
        'S': text(nset).size = value
        'O': text(nset).orient = value
        'A': text(nset).align = value
        'FF': begin
            text[nset].ffamily = value
            ffflag = 1b
        end
        'F': begin
            if value ge 3 then begin
                text(nset).font = value
                if ~ffflag then text[nset].ffamily = -1
            endif else if value eq 1 then begin
                text[nset].font = 3
                text[nset].ffamily = 1
            endif else begin
                text[nset].font = 3
                text[nset].family = 0
            endelse
        end
        'W': text(nset).thick = value
        'X': text(nset).x = value
        'Y': text(nset).y = value
        'N': text(nset).norm = value
        
        'T': text(nset).text = value
        'TID': text[nset].id = value

        'AX': text[nset].axis = value

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
