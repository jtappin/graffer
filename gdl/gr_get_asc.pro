; Copyright (C) 2013
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

pro Gr_get_asc, pdefs, ilu, no_set = no_set

;+
; GR_GET_ASC
;	Get an ASCII graffer dataset from a file
;
; Usage:
;	gr_get_asc, pdefs, ilu
;
;
; Argument:
;	pdefs	struct	in/out	The graffer data structure.
;	ilu	long	input	The file unit to read.
;	
; Keywords:
;	no_set	input	If set, then don't try to set up the widget
;			values (because the widgets aren't there)
;
; History:
;	Original: 16/8/95; SJT
;	Remove file argument (use pdefs.name): 17/8/95; SJT
;	Add facilities for string-type data and restore filename: 18/8/95; SJT
;	Change to a function returning 0 or 1 & add NO_WARN key:
;	11/6/96; SJT
;	Make this the ASCII only version GR_GET_ASC and make GRAFF_GET
;	a wrapper: 14/1/96; SJT
;	Drop CDF support: 10/2/97; SJT
;	Add REM(arks) field: 23/6/97; SJT
;	Replace handles with pointers: 28/6/05; SJT
;	Add "GI" for isotropic: 25/6/08; SJT
;	Add key charsize: 29/4/09; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Convert to procedure for Graffer V4: 6/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Add options for plplot drivers: 29/11/13; SJT
;-


dflag = 0b
tflag = 0b

inline = ''
ctflag = 0b
nxt = 0l & nyt = 0l & nrt = 0l

while (not eof(ilu)) do begin
    
    readf, ilu, inline
    tag_val = str_sep(inline, ':')
    
    for itag = 0, n_elements(tag_val) - 2, 2 do begin
        case (tag_val(itag)) of
            
                                ; The G keys are general graffer keys
                                ; GT - plot title
                                ; GS - Plot subtitle
                                ; GC - Annotation charcter size
                                ; GA - line thickness for AXES
                                ; GP - Positions of corners
                                ; GR - Aspect of plot.
                                ; GI - Is plot isotropic?
                                ; GHA- Plot to match hw aspect ratio.
            
            'GT': begin
                pdefs.title = gr_str_val(inline, 'GT')
                goto, new_line
            end
            'GS': begin
                pdefs.subtitle = gr_str_val(inline, 'GS')
                goto, new_line
            end
            'GC': pdefs.charsize = gr_flt_val(tag_val[itag+1], 1)
            'GA': pdefs.axthick = gr_flt_val(tag_val[itag+1], 1)
            'GP': pdefs.position = gr_flt_val(tag_val[itag+1], 4)
            'GR': pdefs.aspect = gr_flt_val(tag_val[itag+1], 2)
            'GI': pdefs.isotropic = gr_byt_val(tag_val[itag+1], 1)
            'GHA': pdefs.match = gr_byt_val(tag_val[itag+1], 1)

                                ; The X, Y and R keys are items relating
                                ; to the X, Y and right-hand Y axes
                                ; respectively  
                                ; XR, YR, RR - axis range
                                ; XL, YL, RL - axis log/linear
                                ; XSI, YSI, RSI - axis style (the IDL
                                ;                 STYLE key) 
                                ; XSE, YSE, RSE - extra style items
                                ; XSG, YSG, RSG - Grid linestyle (IDL
                                ;                 linesyle+1) 
                                ; XST (YST, RST) - Time labelling options
                                ; XSZ (YSZ, RSZ) - Time zero.
                                ; XT, YT, RT - Axis label.
                                ; YIR - There is a right-hand y-axis

            'XR': pdefs.xrange = gr_dbl_val(tag_val[itag+1], 2)
            'XL': pdefs.xtype = gr_int_val(tag_val[itag+1], 1)
            'XSI': pdefs.xsty.idl = gr_int_val(tag_val[itag+1], 1)
            'XSE': begin
               pdefs.xsty.extra = gr_int_val(tag_val[itag+1], 1)
               if (pdefs.xsty.extra and 1) then begin
                  pdefs.xsty.minor = 1
                  pdefs.xsty.extra and= (not 1)
               endif
            end
            'XMJ': pdefs.xsty.major = gr_int_val(tag_val[itag+1], 1)
            'XMN': pdefs.xsty.minor = gr_int_val(tag_val[itag+1], 1)
            'XNV': nxt = gr_int_val(tag_val[itag+1], 1)
            'XFM': begin
               pdefs.xsty.format = gr_str_val(inline, 'XFM')
               goto, new_line
            end

            'XVL': begin
               if nxt eq 0 then goto, new_line
               vals = gr_dbl_val(tag_val[itag+1], nxt)
               if ptr_valid(pdefs.xsty.values) then $
                  ptr_free, pdefs.xsty.values
               pdefs.xsty.values = ptr_new(vals)
            end

            'XSG': pdefs.xsty.grid = gr_int_val(tag_val[itag+1], 1)
            'XST': pdefs.xsty.time = gr_int_val(tag_val[itag+1], 1)
            'XSZ': pdefs.xsty.tzero = gr_int_val(tag_val[itag+1], 1)
            'XT': begin
                pdefs.xtitle = gr_str_val(inline, 'XT')
                goto, new_line
            end
            
            'YR': pdefs.yrange = gr_dbl_val(tag_val[itag+1], 2)
            'YL': pdefs.ytype = gr_int_val(tag_val[itag+1], 1)
            'YSI': pdefs.ysty.idl = gr_int_val(tag_val[itag+1], 1)
            'YSE': begin
               pdefs.ysty.extra = gr_int_val(tag_val[itag+1], 1)
               if (pdefs.ysty.extra and 1) then begin
                  pdefs.ysty.minor = 1
                  pdefs.ysty.extra and= (not 1)
               endif
            end
            'YMJ': pdefs.ysty.major = gr_int_val(tag_val[itag+1], 1)
            'YMN': pdefs.ysty.minor = gr_int_val(tag_val[itag+1], 1)
            'YFM': begin
               pdefs.ysty.format = gr_str_val(inline, 'YFM')
               goto, new_line
            end
            'YNV': nyt = gr_int_val(tag_val[itag+1], 1)
            'YVL': begin
               if nyt eq 0 then goto, new_line
               vals = gr_dbl_val(tag_val[itag+1], nyt)
               if ptr_valid(pdefs.ysty.values) then $
                  ptr_free, pdefs.ysty.values
               pdefs.ysty.values = ptr_new(vals)
            end
            'YSG': pdefs.ysty.grid = gr_int_val(tag_val[itag+1], 1)
            'YST': pdefs.ysty.time = gr_int_val(tag_val[itag+1], 1)
            'YSZ': pdefs.ysty.tzero = gr_int_val(tag_val[itag+1], 1)
            'YT': begin
                pdefs.ytitle = gr_str_val(inline, 'YT')
                goto, new_line
            end
            'YIR': pdefs.y_right = gr_byt_val(tag_val[itag+1], 1)

            'RR': pdefs.yrange_r = gr_dbl_val(tag_val[itag+1], 2)
            'RL': pdefs.ytype_r = gr_int_val(tag_val[itag+1], 1)
            'RSI': pdefs.ysty_r.idl = gr_int_val(tag_val[itag+1], 1) 
            'RSE': begin
               pdefs.ysty_r.extra = gr_int_val(tag_val[itag+1], 1) 
               if (pdefs.ysty_r.extra and 1) then begin
                  pdefs.ysty_r.minor = 1
                  pdefs.ysty_r.extra and= (not 1)
               endif
            end
            'RMJ': pdefs.ysty_r.major = gr_int_val(tag_val[itag+1], 1)
            'RMN': pdefs.ysty_r.minor = gr_int_val(tag_val[itag+1], 1)
            'RFM': begin
               pdefs.ysty_r.format = gr_str_val(inline, 'RFM')
               goto, new_line
            end
            'RNV': nrt = gr_int_val(tag_val[itag+1], 1)
            'RVL': begin
               if nrt eq 0 then goto, new_line
               vals = gr_dbl_val(tag_val[itag+1], nrt)
               if ptr_valid(pdefs.ysty_r.values) then $
                  ptr_free, pdefs.ysty_r.values
               pdefs.ysty_r.values = ptr_new(vals)
            end
            
            'RSG': pdefs.ysty_r.grid = gr_int_val(tag_val[itag+1], 1)
            'RST': pdefs.ysty_r.time = gr_int_val(tag_val[itag+1], 1)
            'RSZ': pdefs.ysty_r.tzero = gr_int_val(tag_val[itag+1], 1)
            'RT': begin
                pdefs.ytitle_r = gr_str_val(inline, 'RT')
                goto, new_line
            end

                                ; ZT - specifies the colour table to
                                ;      be used by the image format for
                                ;      displaying 2-D data
                                ; ZG - The gamma value for same.
            
            'ZT': begin
                pdefs.ctable = gr_int_val(tag_val[itag+1], 1)
                ctflag = 1b
            end
            'ZG': pdefs.gamma = gr_flt_val(tag_val[itag+1], 1)
            
                                ; DN - total number of datasets in the
                                ;      file. This MUST come before any
                                ;      datasets are defined.
                                ; DC - The currently selected dataset
                                ;      (N.B. This is ZERO-based while
                                ;      the dataset index showing via
                                ;      the GRAFFER interface is
                                ;      ONE-based)
            
            'DN': begin
                if (dflag) then  $
                  graff_msg, pdefs.ids.message, $
                  ["Datasets have already been defined", $
                   "these will be destroyed."]
                
                pdefs.nsets = gr_int_val(tag_val[itag+1], 1)
                nds = pdefs.nsets > 1
                data = replicate({graff_data}, nds)
                dflag = 1b
            end
            'DC': pdefs.cset = gr_int_val(tag_val[itag+1], 1)
            
                                ; TN - The total number of text
                                ;      strings in the file. This must
                                ;      come before any strings are
                                ;      actually defined.
            
            'TN': begin
                if (tflag) then  $
                  graff_msg, pdefs.ids.message, $
                  ["Text strings have already been defined", $
                   "these will be destroyed."]
                
                pdefs.ntext = gr_int_val(tag_val[itag+1], 1)
                ntext = pdefs.ntext > 1
                text = replicate({graff_text}, ntext)
                tflag = 1b
            end
            
                                ; DS - Start the definition of a
                                ;      dataset.
            
            'DS': begin
                rset = gr_int_val(tag_val[itag+1], 1)
                if (not dflag) then begin
                    graff_msg, pdefs.ids.message, $ $
                      ["Dataset read before number of sets defined", $
                       "may be destroyed if number of sets found later"]
                    data = replicate({graff_data}, rset)
                    dflag = 1b
                    pdefs.nsets = rset+1
                endif else if (rset ge pdefs.nsets) then begin
                    data = [data, replicate({graff_data}, $
                                            rset-pdefs.nsets+1)]
                endif
                gr_get_ds, data, rset, ilu, pdefs.ids.message
                goto, new_line
            end
            
                                ; TS - start the definition of a text
                                ;      string 
                                ; TTS - start the definition of the
                                ;       text template (current default
                                ;       text options).
            
            'TS': begin
                tset = gr_int_val(tag_val[itag+1], 1)
                if (not tflag) then begin
                    graff_msg, pdefs.ids.message, $
                      ["Text string read before number of sets " + $
                       "defined", $
                       "may be destroyed if number of strings found later"]
                    text = replicate({graff_text}, tset)
                    tflag = 1b
                    pdefs.ntext = tset+1
                endif else if (tset ge pdefs.ntext) then begin
                    text = [text, replicate({graff_text}, $
                                            tset-pdefs.ntext+1)]
                endif
                gr_get_txt, text, tset, ilu, pdefs.ids.message 
                goto, new_line
            end
            
            'TTS': begin
                topts = {graff_text}
                gr_get_txt, topts, 0, ilu, pdefs.ids.message, /template
                pdefs.text_options = topts
                goto, new_line
            end
            
                                ; The H options refer to the options
                                ; for genration PostScript hardcopy
                                ; files.
                                ; HC - Colour or monchrome
                                ; HE - Eps or normal
                                ; HO - landscape or portrait
                                ;      (Orientation)
                                ; HY - CMYK colour model or not.
                                ; HP - Paper size (A4 or letter)
                                ; HT - whether to put a timestapm on
                                ;      the plot.
                                ; HS - size x & y in cm.
                                ; HD - Page offset in cm.
                                ; HAB - The spooling command (up to
                                ;       the filename)
                                ; HAA - Any part of the spooling
                                ;       command which follows the
                                ;       filename. 
                                ; HVB - The view command (up to
                                ;       the filename)
                                ; HVA - Any part of the view
                                ;       command which follows the
                                ;       filename. 
                                ; HF - Font family.
                                ; HWS - Font weight and slant (bit 0 is
                                ;       on for bold, bit 1 for
                                ;       oblique/italic)
                                ; HFN - Plot file name
             
            'HC': pdefs.hardset.colour = gr_byt_val(tag_val[itag+1], 1)
            'HE': pdefs.hardset.eps = gr_byt_val(tag_val[itag+1], 1) 
            'HO': pdefs.hardset.orient = gr_byt_val(tag_val[itag+1], 1) 
            'HP': pdefs.hardset.psize = gr_byt_val(tag_val[itag+1], 1)
            'HY': pdefs.hardset.cmyk = gr_byt_val(tag_val[itag+1], 1)
            'HT': pdefs.hardset.timestamp = $
              gr_byt_val(tag_val[itag+1], 1) 
            'HS' :pdefs.hardset.size = gr_flt_val(tag_val[itag+1], 2)
            'HD' :pdefs.hardset.off = gr_flt_val(tag_val[itag+1], 2)

            'HAB': begin
                pdefs.hardset.action[0] = gr_str_val(inline, 'HAB')
                goto, new_line
            end
            'HAA': begin
                pdefs.hardset.action[1] = gr_str_val(inline, 'HAA')
                goto, new_line
            end
            'HVB': begin
                pdefs.hardset.viewer[0] = gr_str_val(inline, 'HVB')
                goto, new_line
            end
            'HVA': begin
                pdefs.hardset.viewer[1] = gr_str_val(inline, 'HVA')
                goto, new_line
            end
            'HPB': begin
                pdefs.hardset.pdfviewer[0] = gr_str_val(inline, 'HPB')
                goto, new_line
            end
            'HPA': begin
                pdefs.hardset.pdfviewer[1] = gr_str_val(inline, 'HPA')
                goto, new_line
            end

            'HF': pdefs.hardset.font.family = $
              gr_int_val(tag_val[itag+1], 1)
            'HWS': pdefs.hardset.font.wg_sl = $
              gr_int_val(tag_val[itag+1], 1)
            'HFN':  begin
                pdefs.hardset.name = gr_str_val(inline, 'HFN')
                goto, new_line
             end
            'HPS': begin
               pdefs.hardset.psdev = gr_str_val(inline, 'HPS')
               goto, new_line
            end
            'HEP': begin
               pdefs.hardset.epsdev = gr_str_val(inline, 'HEP')
               goto, new_line
            end
            'HPD': begin
               pdefs.hardset.pdfdev = gr_str_val(inline, 'HPD')
               goto, new_line
            end
            'HSV': begin
               pdefs.hardset.svgdev = gr_str_val(inline, 'HSV')
               goto, new_line
            end


                                ; The K tags relate to the plotting of
                                ; a key on the plot.
                                ; KU - Plot a key
                                ; KX - X coordinates of the corners
                                ; KY - Y coordinates of the corners
                                ; KN - System they are given in.
                                ; KC - How many columns
                                ; KF - Frame?
                                ; KS - Charater size
                                ; KT - Title of key
                                ; KNL - Number of items in key (only
                                ;       used in an ascii save
                                ; KL - The indices of the datasets to
                                ;      display
                                ; KP - Whether to plot 1 or 2 points.
             
            'KU': pdefs.key.use = gr_byt_val(tag_val[itag+1], 1)
            'KX': pdefs.key.x = gr_dbl_val(tag_val[itag+1], 2)
            'KY': pdefs.key.y = gr_dbl_val(tag_val[itag+1], 2)
            'KS': pdefs.key.csize = gr_dbl_val(tag_val[itag+1], 1)
            'KN': pdefs.key.norm = gr_int_val(tag_val[itag+1], 1)
            'KC': pdefs.key.cols = gr_int_val(tag_val[itag+1], 1)
            'KF': pdefs.key.frame = gr_byt_val(tag_val[itag+1], 1)
            'KP': pdefs.key.one_point = gr_byt_val(tag_val[itag+1], 1)
            'KT': begin
                pdefs.key.title = gr_str_val(inline, 'KT')
                goto, new_line
            end
            'KLN': n_key_list = gr_lon_val(tag_val[itag+1], 1)
            'KL': begin
                list = gr_lon_val(tag_val[itag+1], n_key_list)
                pdefs.key.list = ptr_new(list)
            end
             
                                ; REM - Remarks attached to the file
             
            'REM': begin
                nrem = gr_lon_val(tag_val[itag+1], 1)
                remarks = strarr(nrem)
                for j = 0, nrem-1 do begin
                    readf, ilu, inline
                    remarks(j) = inline
                endfor
                pdefs.remarks = ptr_new(remarks)
            end
            
                                ; This probably means that the file is
                                ; corrupted. 
             
            Else: graff_msg, pdefs.ids.message, "Unknown tag: " + $
              tag_val(itag) + "Ignoring."
        endcase
    endfor
     
New_line:
     
endwhile

ptr_free, pdefs.data, pdefs.text

pdefs.data = ptr_new(data)
pdefs.text = ptr_new(text)
pdefs.is_ascii = 1b


free_lun, ilu

pdefs.chflag = 0                ; Clear changes flag

if (not keyword_set(no_set)) then begin
    graff_set_vals, pdefs
endif
if ctflag then graff_colours, pdefs

end

