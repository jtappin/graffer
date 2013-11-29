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

pro gr_get_bin, pdefs, ilu, no_set = no_set

;+
; GR_GET_BIN
;	Get an BINARY graffer dataset from a file (V4)
;
; Usage:
;	gr_get_bin, pdefs, ilu
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
;	Original: (from GR_GET_ASC) 15/1/96; SJT
;	Drop CDF support: 10/2/97; SJT
;	Fix bad tag problem (I hope): 19/5/97; SJT
;	Add REM(arks) field: 23/6/97; SJT
;	Convert handles to pointers:28/6/05; SJT
;	Add "GI" for isotropic: 25/6/08; SJT
;	Add key charsize: 29/4/09; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	V4 version: 6/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;	Add options for plplot drivers: 29/11/13; SJT
;-


  dflag = 0b
  tflag = 0b

  tag = '   '
  ctflag = 0b

  while (not eof(ilu)) do begin
     
     graff_get_rec, ilu, tag, value, tcode

     case (tag) of
        
                                ; The G keys are general graffer keys
                                ; GT - plot title
                                ; GS - Plot subtitle
                                ; GC - Annotation charcter size
                                ; GA - line thickness for AXES
                                ; GP - Positions of corners
                                ; GR - Aspect of plot.
                                ; GI - Is plot isotropic?

        'GT ': pdefs.title = value
        'GS ': pdefs.subtitle = value
        'GC ': pdefs.charsize = value
        'GA ': pdefs.axthick = value
        'GP ': pdefs.position = value
        'GR ': pdefs.aspect = value
        'GI ': pdefs.isotropic = value
        'GHA': pdefs.match = value

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

        'XR ': pdefs.xrange = value
        'XL ': pdefs.xtype = value
        'XSI': pdefs.xsty.idl = value
        'XSE': begin
           pdefs.xsty.extra = value
           if (pdefs.xsty.extra and 1) then begin
              pdefs.xsty.minor = 1
              pdefs.xsty.extra and= (not 1)
           endif
        end
        'XMN': pdefs.xsty.minor = value
        'XMJ': pdefs.xsty.major = value
        'XMS': pdefs.xsty.xmajor = value
        'XFM': pdefs.xsty.format = value

        'XVL': begin
           if ptr_valid(pdefs.xsty.values) then $
              ptr_free, pdefs.xsty.values 
           pdefs.xsty.values = ptr_new(value)
        end
        'XSG': pdefs.xsty.grid = value
        'XST': pdefs.xsty.time = value
        'XSZ': pdefs.xsty.tzero = value
        'XT ': pdefs.xtitle = value
        
        'YR ': pdefs.yrange = value
        'YL ': pdefs.ytype = value
        'YSI': pdefs.ysty.idl = value
        'YSE': begin
           pdefs.ysty.extra = value
           if (pdefs.ysty.extra and 1) then begin
              pdefs.ysty.minor = 1
              pdefs.ysty.extra and= (not 1)
           endif
        end
        'YMN': pdefs.ysty.minor = value
        'YMJ': pdefs.ysty.major = value
        'YMS': pdefs.ysty.xmajor = value
        'YFM': pdefs.ysty.format = value
        'YVL': begin
           if ptr_valid(pdefs.ysty.values) then $
              ptr_free, pdefs.ysty.values 
           pdefs.ysty.values = ptr_new(value)
        end
        'YSG': pdefs.ysty.grid = value
        'YST': pdefs.ysty.time = value
        'YSZ': pdefs.ysty.tzero = value
        'YT ': pdefs.ytitle = value
        'YIR': pdefs.y_right = value

        'RR ': pdefs.yrange_r = value
        'RL ': pdefs.ytype_r = value
        'RSI': pdefs.ysty_r.idl = value
        'RSE': begin
           pdefs.ysty_r.extra = value
           if (pdefs.ysty_r.extra and 1) then begin
              pdefs.ysty_r.minor = 1
              pdefs.ysty_r.extra and= (not 1)
           endif
        end
        'RMN': pdefs.ysty_r.minor = value
        'RMJ': pdefs.ysty_r.major = value
        'RMS': pdefs.ysty_r.xmajor = value
        'RFM': pdefs.ysty_r.format = value
        'RVL': begin
           if ptr_valid(pdefs.ysty_r.values) then $
              ptr_free, pdefs.ysty_r.values 
           pdefs.ysty_r.values = ptr_new(value)
        end
        'RSG': pdefs.ysty_r.grid = value
        'RST': pdefs.ysty_r.time = value
        'RSZ': pdefs.ysty_r.tzero = value
        'RT ': pdefs.ytitle_r = value

                                ; ZT - specifies the colour table to
                                ;      be used by the image format for
                                ;      displaying 2-D data
                                ; ZG - The gamma value for same.
        
        'ZT ': begin
           pdefs.ctable = value
           ctflag = 1b
        end
        'ZG ': pdefs.gamma = value
        
                                ; DN - total number of datasets in the
                                ;      file. This MUST come before any
                                ;      datasets are defined.
                                ; DC - The currently selected dataset
                                ;      (N.B. This is ZERO-based while
                                ;      the dataset index showing via
                                ;      the GRAFFER interface is
                                ;      ONE-based)
        
        'DN ': begin
           pdefs.nsets = value
           nds = pdefs.nsets > 1
           data = replicate({graff_data}, nds)
           dflag = 1b
        end
        'DC ': pdefs.cset = value
        
                                ; TN - The total number of text
                                ;      strings in the file. This must
                                ;      come before any strings are
                                ;      actually defined.
        
        'TN ': begin
           pdefs.ntext = value
           ntext = pdefs.ntext > 1
           text = replicate({graff_text}, ntext)
           tflag = 1b
        end
        
                                ; DS - Start the definition of a
                                ;      dataset.
        
        'DS ': begin
           rset = value
           gr_bin_ds, data, rset, ilu, pdefs.ids.message
        end
        
                                ; TS - start the definition of a text
                                ;      string 
                                ; TTS - start the definition of the
                                ;       text template (current default
                                ;       text options).
        
        'TS ': begin
           tset = value
           gr_bin_txt, text, tset, ilu, pdefs.ids.message
        end
        
        'TTS': begin
           topts = {graff_text}
           gr_bin_txt, topts, 0, ilu, pdefs.ids.message, /template
           pdefs.text_options = topts
        end
        
                                ; The H options refer to the options
                                ; for genration PostScript hardcopy
                                ; files.
                                ; HC - Colour or monchrome
                                ; HE - Eps or normal
                                ; HO - landscape or portrait
                                ;      (Orientation)
                                ; HY - Use CMYK colour model or not.
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
        
        'HC ': pdefs.hardset.colour = value
        'HE ': pdefs.hardset.eps = value
        'HO ': pdefs.hardset.orient = value
        'HY ': pdefs.hardset.cmyk = value
        'HP ': pdefs.hardset.psize = value
        'HT ': pdefs.hardset.timestamp = value
        'HS ': pdefs.hardset.size = value
        'HD ': pdefs.hardset.off = value

        'HAB': pdefs.hardset.action[0] = value
        'HAA': pdefs.hardset.action[1] = value
        'HVB': pdefs.hardset.viewer[0] = value
        'HVA': pdefs.hardset.viewer[1] = value
        
        'HF ': pdefs.hardset.font.family = value
        'HWS': pdefs.hardset.font.wg_sl = value
        'HFN': pdefs.hardset.name = value
        'HPS': pdefs.hardset.psdev = value
        'HEP': pdefs.hardset.epsdev = value
        'HPD': pdefs.hardset.pdfdev = value
        'HSV': pdefs.hardset.svgdev = value


                                ; The K tags relate to the plotting of
                                ; a key on the plot.
                                ; KU - Plot a key
                                ; KX - X coordinates of the corners
                                ; KY - Y coordinates of the corners
                                ; KN - System they are given in.
                                ; KC - How many columns
                                ; KS - Character size
                                ; KF - Frame?
                                ; KT - Title of key
                                ; KNL - Number of items in key (only
                                ;       used in an ascii save
                                ; KL - The indices of the datasets to
                                ;      display
                                ; KP - Whether to plot 1 or 2 points.
        
        'KU ': pdefs.key.use = value
        'KX ': pdefs.key.x = value
        'KY ': pdefs.key.y = value
        'KS ': pdefs.key.csize = value
        'KN ': pdefs.key.norm = value
        'KC ': pdefs.key.cols = value
        'KF ': pdefs.key.frame = value
        'KP ': pdefs.key.one_point = value
        'KT ': pdefs.key.title = value
        'KL ': begin
           list = long(value)   ; Make sure the type is right
           pdefs.key.list = ptr_new(list)
        end
        
                                ; REM - Remarks attached to the file
        
        'REM': begin
           remarks = string(value) ; Make sure the type is right
           pdefs.remarks = ptr_new(remarks)
        end
        
                                ; This probably means that the file is
                                ; corrupted. 
        
        Else: begin
           graff_msg, "Unknown tag: " + $
                      tag + " Ignoring."
        end
     endcase
     
New_line:
     
  endwhile

  ptr_free, pdefs.data, pdefs.text

  pdefs.data = ptr_new(data)
  pdefs.text = ptr_new(text)
  pdefs.is_ascii = 0b


  free_lun, ilu

  pdefs.chflag = 0              ; Clear changes flag



end
