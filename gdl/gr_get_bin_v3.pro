pro gr_get_bin_v3, pdefs, ilu, file_v, no_set = no_set

;+
; GR_GET_BIN_V3
;	Get an BINARY graffer dataset from a file (Version 3 format)
;
; Usage:
;	gr_get_bin_v3, pdefs, ilu, file_v
;
; Argument:
;	pdefs	struct	in/out	The graffer data structure.
;	ilu	long	input	The file unit to read.
;	file_v	int	input	The version of the file.
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
;	Converted to procedure, for Graffer V4: 6/1/12; SJT
;	Advanced axis style settings: 21/8/12; SJT
;-


single = file_v[0] eq 2 

dflag = 0b
tflag = 0b

tag = '   '
ctflag = 0b

while (not eof(ilu)) do begin
    
    readu, ilu, tag

    case (tag) of
        
                                ; The G keys are general graffer keys
                                ; GT - plot title
                                ; GS - Plot subtitle
                                ; GC - Annotation charcter size
                                ; GA - line thickness for AXES
                                ; GP - Positions of corners
                                ; GR - Aspect of plot.
                                ; GI - Is plot isotropic?

        'GT ': pdefs.title = gr_str_rd(ilu)
        'GS ': pdefs.subtitle = gr_str_rd(ilu)
        'GC ': pdefs.charsize = gr_flt_rd(ilu, 1)
        'GA ': pdefs.axthick = gr_int_rd(ilu, 1)
        'GP ': pdefs.position = gr_flt_rd(ilu, 4)
        'GR ': pdefs.aspect = gr_flt_rd(ilu, 2)
        'GI ': pdefs.isotropic = gr_byt_rd(ilu, 1)

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

        'XR ': pdefs.xrange = gr_dbl_rd(ilu, 2, single = single)
        'XL ': pdefs.xtype = gr_int_rd(ilu, 1)
        'XSI': pdefs.xsty.idl = gr_int_rd(ilu, 1)
        'XSE': begin
           pdefs.xsty.extra = gr_int_rd(ilu, 1)
           pdefs.xsty.minor = pdefs.xsty.extra and 1
           pdefs.xsty.extra and= (not 1)
        end
        'XSG': pdefs.xsty.grid = gr_int_rd(ilu, 1)
        'XST': pdefs.xsty.time = gr_int_rd(ilu, 1)
        'XSZ': pdefs.xsty.tzero = gr_int_rd(ilu, 1)
        'XT ': pdefs.xtitle = gr_str_rd(ilu)
        
        'YR ': pdefs.yrange = gr_dbl_rd(ilu, 2, single = single)
        'YL ': pdefs.ytype = gr_int_rd(ilu, 1)
        'YSI': pdefs.ysty.idl = gr_int_rd(ilu, 1)
        'YSE': begin
           pdefs.ysty.extra = gr_int_rd(ilu, 1)
           pdefs.ysty.minor = pdefs.ysty.extra and 1
           pdefs.ysty.extra and= (not 1)
        end
        'YSG': pdefs.ysty.grid = gr_int_rd(ilu, 1)
        'YST': pdefs.ysty.time = gr_int_rd(ilu, 1)
        'YSZ': pdefs.ysty.tzero = gr_int_rd(ilu, 1)
        'YT ': pdefs.ytitle = gr_str_rd(ilu)
        'YIR': pdefs.y_right = gr_byt_rd(ilu, 1)

        'RR ': pdefs.yrange_r = gr_dbl_rd(ilu, 2, single = single)
        'RL ': pdefs.ytype_r = gr_int_rd(ilu, 1)
        'RSI': pdefs.ysty_r.idl = gr_int_rd(ilu, 1)
        'RSE': begin
           pdefs.ysty_r.extra = gr_int_rd(ilu, 1)
           pdefs.ysty_r.minor = pdefs.ysty_r.extra and 1
           pdefs.ysty_r.extra and= (not 1)
        end
        'RSG': pdefs.ysty_r.grid = gr_int_rd(ilu, 1)
        'RST': pdefs.ysty_r.time = gr_int_rd(ilu, 1)
        'RSZ': pdefs.ysty_r.tzero = gr_int_rd(ilu, 1)
        'RT ': pdefs.ytitle_r = gr_str_rd(ilu)

                                ; ZT - specifies the colour table to
                                ;      be used by the image format for
                                ;      displaying 2-D data
                                ; ZG - The gamma value for same.
        
        'ZT ': begin
            pdefs.ctable = gr_int_rd(ilu, 1)
            ctflag = 1b
        end
        'ZG ': pdefs.gamma = gr_flt_rd(ilu, 1)
        
                                ; DN - total number of datasets in the
                                ;      file. This MUST come before any
                                ;      datasets are defined.
                                ; DC - The currently selected dataset
                                ;      (N.B. This is ZERO-based while
                                ;      the dataset index showing via
                                ;      the GRAFFER interface is
                                ;      ONE-based)
        
        'DN ': begin
            pdefs.nsets = gr_int_rd(ilu, 1)
            nds = pdefs.nsets > 1
            data = replicate({graff_data}, nds)
            dflag = 1b
        end
        'DC ': pdefs.cset = gr_int_rd(ilu, 1)
        
                                ; TN - The total number of text
                                ;      strings in the file. This must
                                ;      come before any strings are
                                ;      actually defined.
        
        'TN ': begin
            pdefs.ntext = gr_int_rd(ilu, 1)
            ntext = pdefs.ntext > 1
            text = replicate({graff_text}, ntext)
            tflag = 1b
        end
        
                                ; DS - Start the definition of a
                                ;      dataset.
        
        'DS ': begin
            rset = gr_int_rd(ilu, 1)
            gr_bin_ds_v3, data, rset, ilu, pdefs.ids.message, file_v
        end
        
                                ; TS - start the definition of a text
                                ;      string 
                                ; TTS - start the definition of the
                                ;       text template (current default
                                ;       text options).
        
        'TS ': begin
            tset = gr_int_rd(ilu, 1)
            gr_bin_txt_v3, text, tset, ilu, pdefs.ids.message, file_v
        end
        
        'TTS': begin
            topts = {graff_text}
            gr_bin_txt_v3, topts, 0, ilu, pdefs.ids.message, file_v, $
              /template
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
                                ; HF - Font family.
                                ; HWS - Font weight and slant (bit 0 is
                                ;       on for bold, bit 1 for
                                ;       oblique/italic) 
        
        'HC ': pdefs.hardset.colour = gr_byt_rd(ilu, 1)
        'HE ': pdefs.hardset.eps = gr_byt_rd(ilu, 1)
        'HO ': pdefs.hardset.orient = gr_byt_rd(ilu, 1)
        'HY ': pdefs.hardset.cmyk = gr_byt_rd(ilu, 1)
        'HP ': pdefs.hardset.psize = gr_byt_rd(ilu, 1)
        'HT ': pdefs.hardset.timestamp = gr_byt_rd(ilu, 1)
        'HS ': pdefs.hardset.size = gr_flt_rd(ilu, 2)
        'HD ': pdefs.hardset.off = gr_flt_rd(ilu, 2)

        'HAB': pdefs.hardset.action(0) = gr_str_rd(ilu)
        'HAA': pdefs.hardset.action(1) = gr_str_rd(ilu)
 
        'HF ': pdefs.hardset.font.family = $
          gr_int_rd(ilu, 1)
        'HWS': pdefs.hardset.font.wg_sl = $
          gr_int_rd(ilu, 1)
        
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
        
        'KU ': pdefs.key.use = gr_byt_rd(ilu, 1)
        'KX ': pdefs.key.x = gr_dbl_rd(ilu, 2, single = single)
        'KY ': pdefs.key.y = gr_dbl_rd(ilu, 2, single = single)
        'KS ': pdefs.key.csize = gr_dbl_rd(ilu, 1)
        'KN ': pdefs.key.norm = gr_int_rd(ilu, 1)
        'KC ': pdefs.key.cols = gr_int_rd(ilu, 1)
        'KF ': pdefs.key.frame = gr_byt_rd(ilu, 1)
        'KP ': pdefs.key.one_point = gr_byt_rd(ilu, 1)
        'KT ': pdefs.key.title = gr_str_rd(ilu)
        'KL ': begin
            nl = gr_lon_rd(ilu, 1)
            list = gr_lon_rd(ilu, nl)
            pdefs.key.list = ptr_new(list)
        end
        
                                ; REM - Remarks attached to the file
        
        'REM': begin
            remarks = gr_str_rd(ilu)
            pdefs.remarks = ptr_new(remarks)
        end
        
                                ; This probably means that the file is
                                ; corrupted. 
        
        Else: begin
            graff_msg, pdefs.ids.message, "Unknown tag: " + $
              tag + "Ignoring."
;            stop
            point_lun, -ilu, ipos 
            point_lun, ilu, ipos-2 ; Move back 2 bytes
        end
    endcase
    
New_line:
    
endwhile

ptr_free, pdefs.data, pdefs.text

pdefs.data = ptr_new(data)
pdefs.text = ptr_new(text)
pdefs.is_ascii = 0b


free_lun, ilu

pdefs.chflag = 0                ; Clear changes flag

if (not keyword_set(no_set)) then begin
    graff_set_vals, pdefs
    vm = total(file_v ne pdefs.version)
    if (vm ne 0.) then graff_msg, pdefs.ids.message, $
      ['File and program versions differ', $
       'File: '+string(file_v, format = "(I2,'.',I2.2)")+ $
       '  Program: '+string(pdefs.version, format = "(I2,'.',I2.2)")]
endif

if (keyword_set(resave)) then begin
    gr_bin_save, pdefs
    graff_msg, pdefs.ids.message, "Resaving"
endif

end

