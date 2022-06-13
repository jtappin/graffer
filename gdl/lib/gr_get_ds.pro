; LICENCE:
; Copyright (C) 1995-2021: SJT
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.                                   

pro Gr_get_ds, data, nset, ilu, msgid

;+
; GR_GET_DS
;	Read an individual dataset from a V2.x GRAFFER file.
;
; Arguments:
;	data	struct	in/out	The graffer dataset structure
;	nset	int	input	The serial number of the current
;				dataset.
;	ilu	int	input	File unit number to read
;	msgid	long	input	ID of message window (if created).
;
; History:
;	Original: 5/11/96; SJT
;	Shorten name: 25/11/96; SJT
;	Add version argument: 14/5/99; SJT
;	Support colour inversion: 26/6/07; SJT
;	Add local colour table option: 17/11/11; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
; 	Add min & max values: 4/3/15; SJT
; 	Fix init of min & max: 2/6/15; SJT
;	Add non-linear contour level maps: 12/10/16; SJT
;	Add labelling offset: 2/5/17; SJT
;-

  inline = ''

  nflag = 0b
  nflag2 = 0b
  tflag = 0b
  dflag = 0b
  cflag = bytarr(4)
  jflag = 0b

  elements = [2, 3, 4, 3, 4, 4, 5, 5, 6]

  data[nset].min_val = !values.d_nan
  data[nset].max_val = !values.d_nan

  while (not eof(ilu)) do begin
     
     readf, ilu, inline
     tag_val = strsplit(inline, ':', /extr, /preserve_null)
     for itag = 0, n_elements(tag_val) - 1, 2 do begin
        
                                ; Recognised tags:
                                ; J - Joining option
                                ; P - symbol
                                ; S - symbol size
                                ; L - line style
                                ; C - colour
                                ; CV - Custom colour (C==-2)
                                ; W - thickness (width)
                                ; O - sorted? (Order)
                                ; D - description
                                ; N - number of points (or
                                ;     evaluations)
                                ; N2- Number of points in y-direction
                                ;     for 2-D data.
                                ; T - type
                                ; M - Mode
                                ; MN - Min value to plot
                                ; MX - Max value to plot
                                ; K - noclip (both C & N are already
                                ;     bagged)
                                ; E - Mouse editing
                                ; R - function range
                                ; F, FX, FY - function specifiers
                                ; VS, VE - start & end XY data.
                                ; DE - end dataset
        
        case tag_val[itag] of
           
           'J': begin
              data[nset].pline = gr_int_val(tag_val[itag+1], 1)
              jflag = 1b
           end
           
           'P': data[nset].psym = gr_int_val(tag_val[itag+1], 1)
           'S': data[nset].symsize = gr_flt_val(tag_val[itag+1], 1)
           'L': data[nset].line = gr_int_val(tag_val[itag+1], 1)
           'C': data[nset].colour = gr_int_val(tag_val[itag+1], 1)
           'CV': data[nset].c_vals = gr_byt_val(tag_val[itag+1], 3)
           'W': data[nset].thick = gr_int_val(tag_val[itag+1], 1)
           'O': data[nset].sort = gr_byt_val(tag_val[itag+1], 1)
           'K': data[nset].noclip = gr_byt_val(tag_val[itag+1], 1)
           'E': data[nset].medit = gr_byt_val(tag_val[itag+1], 1)
           'D': begin
              data[nset].descript = gr_str_val(inline, 'D')
              goto, new_line
           end

           
           'N': begin
              data[nset].ndata = gr_lon_val(tag_val[itag+1], 1)
              nflag = 1b
           end
           'N2': begin
              data[nset].ndata2 = gr_lon_val(tag_val[itag+1], 1)
              nflag2 = 1b
           end
           'T': begin
              data[nset].type = gr_int_val(tag_val[itag+1], 1)
              tflag = 1b
           end

           'Y': data[nset].y_axis = gr_int_val(tag_val[itag+1], 1)

           'M': data[nset].mode = gr_int_val(tag_val[itag+1], 1)
           'MN': data[nset].min_val = gr_dbl_val(tag_val[itag+1], 1)
           'MX': data[nset].max_val = gr_dbl_val(tag_val[itag+1], 1)

           'ZF':  data[nset].zopts.format = $
              gr_int_val(tag_val[itag+1], 1)
           
           'ZNL': begin
              data[nset].zopts.n_levels = $
                 abs(gr_int_val(tag_val[itag+1], 1))
              cflag[0] = 1b
           end
           'ZLM': data[nset].zopts.lmap = $
              gr_int_val(tag_val[itag+1], 1)

           'ZL': if (cflag[0]) then begin
              levels = gr_dbl_val(tag_val[itag+1], $
                                  data[nset].zopts.n_levels) 
              data[nset].zopts.levels = ptr_new(levels)
              data[nset].zopts.set_levels = 1b
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour level " + $
                         "list given without count - ignored"
           'ZLL': if (cflag[0]) then begin
              levels = dblarr(data[nset].zopts.n_levels)
              readf, ilu, levels
              data[nset].zopts.levels = ptr_new(levels)
              data[nset].zopts.set_levels = 1b
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour level " + $
                         "list given without count - ignored"

           'ZNC': begin
              data[nset].zopts.n_cols = gr_int_val(tag_val[itag+1], 1)
              cflag[1] = 1b
           end
           'ZC': if (cflag[1]) then begin
              cols = gr_int_val(tag_val[itag+1], data[nset].zopts.n_cols)
              data[nset].zopts.colours = ptr_new(cols)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour colour " + $
                         "list given without count - ignored"
           
           'ZCL': if (cflag[1]) then begin
              cols = intarr(data[nset].zopts.n_cols)
              readf, ilu, cols
              data[nset].zopts.colours = ptr_new(cols)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour colour " + $
                         "list given without count - ignored"
           
           'ZCR': if (cflag[1]) then begin
              rcols = intarr(3, data[nset].zopts.n_cols)
              readf, ilu, rcols
              data[nset].zopts.colours = ptr_new(rcols)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour raw colour " + $
                         "list given without count - ignored"
           
           'ZCX': if cflag[1] then begin
              nce = gr_int_val(tag_val[itag+1], $
                               data[nset].zopts.n_cols)
              cols = intarr(data[nset].zopts.n_cols)
              rcols = intarr(3, data[nset].zopts.n_cols)
              cc = 0
              ccr = intarr(3)
              for j = 0, data[nset].zopts.n_cols do begin
                 if nce[j] eq 1 then begin
                    readf, ilu, cc
                    cols[j] = cc
                 endif else begin
                    readf, ilu, ccr
                    rcols[*, j] = ccr
                    cols[j] = -2
                 endelse
              endfor
              data[nset].zopts.colours = ptr_new(cols)
              data[nset].zopts.raw_colours = ptr_new(rcols)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour colour " + $
                         "list given without count - ignored"

           'ZCT': data[nset].zopts.ctable = $
              gr_int_val(tag_val[itag+1], 1)

           'ZCG': data[nset].zopts.gamma = $
              gr_flt_val(tag_val[itag+1], 1)

           'ZNS': begin
              data[nset].zopts.n_sty = gr_int_val(tag_val[itag+1], 1)
              cflag[2] = 1b
           end
           'ZS': if (cflag[2]) then begin
              sty = gr_int_val(tag_val[itag+1], data[nset].zopts.n_sty) 
              data[nset].zopts.style = ptr_new(sty)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour style " + $
                         "list given without count - ignored"
           'ZSL': if (cflag[2]) then begin
              sty = intarr(data[nset].zopts.n_sty) 
              readf, ilu, sty
              data[nset].zopts.style = ptr_new(sty)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour style " + $
                         "list given without count - ignored"
           
           'ZNT': begin
              data[nset].zopts.n_thick = gr_int_val(tag_val[itag+1], 1)
              cflag[3] = 1b
           end
           'ZT': if (cflag[3]) then begin
              thick = gr_int_val(tag_val[itag+1], data[nset].zopts.n_thick) 
              data[nset].zopts.thick = ptr_new(thick)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour thickness " + $
                         "list given without count - ignored"
           'ZTL': if (cflag[3]) then begin
              thick = intarr(data[nset].zopts.n_thick) 
              readf, ilu, thick
              data[nset].zopts.thick = ptr_new(thick)
           endif else $
              graff_msg, msgid, "** W A R N I N G ** Contour thickness " + $
                         "list given without count - ignored"
           
           'ZCF': begin
              data[nset].zopts.fill = gr_int_val(tag_val[itag+1], 1)
              if data[nset].zopts.fill gt 1 then begin
                 graff_msg, msgid, $
                            "Downhill tick contours no longer " + $
                            "supported, using standard."
                 data[nset].zopts.fill = 0
              endif
           end
           'ZLI': data[nset].zopts.label = $
              gr_int_val(tag_val[itag+1], 1)
           'ZLO': data[nset].zopts.label_off = $
              gr_int_val(tag_val[itag+1], 1)
           'ZCS': data[nset].zopts.charsize = $
              gr_flt_val(tag_val[itag+1], 1)

           'ZR': data[nset].zopts.range = gr_dbl_val(tag_val[itag+1], 2)
           'ZP': data[nset].zopts.pxsize = gr_flt_val(tag_val[itag+1], $
                                                      1)
           'ZIL': data[nset].zopts.ilog = gr_byt_val(tag_val[itag+1], $
                                                     1)
           'ZIN': data[nset].zopts.invert = $
              gr_byt_val(tag_val[itag+1], 1)
           'ZSM': data[nset].zopts.smooth = $
              gr_byt_val(tag_val[itag+1], 1)
           'ZSN': data[nset].zopts.shade_levels = $ 
              gr_lon_val(tag_val[itag+1], 1)

           'ZM': data[nset].zopts.missing = $
              gr_dbl_val(tag_val[itag+1], 1)

           'R': begin
              if (not dflag) then $
                 graff_msg, msgid, "Range found before type defined " + $
                            "- ignored" $
              else if (data[nset].type ge 0) then $
                 graff_msg, msgid, "Range found in XY data set - ignored" $
              else if (data[nset].type eq -4) then $
                 xydata.range = gr_dbl_val(tag_val[itag+1], 4) $
              else $
                 xydata.range = gr_dbl_val(tag_val[itag+1], 2)
           end
           'F': begin
              if (not dflag) then $
                 graff_msg, msgid, "Function found before type defined " + $
                            "- ignored" $
              else if (data[nset].type ge 0) then $
                 graff_msg, msgid, "Function found in XY data set - ignored" $
              else if (data[nset].type eq -3) then $
                 graff_msg, msgid, "Plain function found in " + $
                            "parametric set - ignored" $ 
              else xydata.funct = gr_str_val(inline, 'F')
              goto, new_line
           end
           'FX': begin
              if (not dflag) then $
                 graff_msg, msgid, "Function found before type defined " + $
                            "- ignored" $
              else if (data[nset].type ge 0) then $
                 graff_msg, msgid, "Function found in XY data set - ignored" $
              else if (data[nset].type ne -3) then $
                 graff_msg, msgid, "X function found in " + $
                            "plain function - ignored" $ 
              else xydata.funct(0) = gr_str_val(inline, 'FX')
              goto, new_line
           end
           'FY': begin
              if (not dflag) then $
                 graff_msg, msgid, "Function found before type defined " + $
                            "- ignored" $
              else if (data[nset].type ge 0) then $
                 graff_msg, msgid, "Function found in XY data set - ignored" $
              else if (data[nset].type ne -3) then $
                 graff_msg, msgid, "Y function found in " + $
                            "plain function - ignored" $ 
              else xydata.funct(1) = gr_str_val(inline, 'FY')
              goto, new_line
           end
           
           'VS': begin
              if (not dflag) then begin
                 graff_msg, msgid, "Data found before type defined " + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'VE:') ne -1
              endif else if (data[nset].type lt 0) then begin
                 graff_msg, msgid, "Data found in function dataset " + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'VE:') ne -1
              endif else if (data[nset].type gt 8) then begin
                 graff_msg, msgid, '1-D Data found in 2-D dataset ' + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'VE:') ne -1
              endif else begin
                 ncols = gr_int_val(tag_val[itag+1], 1)
                 if (ncols ne elements(data[nset].type)) then $
                    graff_msg, msgid,  $
                               "WARNING Data columns wrong, could get corrupt " + $
                               "DS"
                 xyvals = dblarr(ncols, data[nset].ndata)
                 readf, ilu, xyvals
                 nerr = gr_n_errors(data[nset].type)
                 xydata.x = ptr_new(reform(xyvals[0, *]))
                 xydata.y = ptr_new(reform(xyvals[1, *]))
                 if nerr[0] ne 0 then xydata.x_err = $
                    ptr_new(xyvals[2:1+nerr[0], *])
                 if nerr[1] ne 0 then xydata.y_err = $
                    ptr_new(xyvals[2+nerr[0]:*, *])
                 
                 readf, ilu, inline
                 if (strpos(inline, 'VE:') eq -1) then begin
; Older files had a dummy line for 1-element datasets (to trick
; IDL's incompetent handling of N×1 arrays).
                    if data[nset].ndata eq 1 then readf, ilu, inline
                    if strpos(inline, 'VE:') eq -1 then $
                       graff_msg, msgid, $
                                  "WARNING Data rows wrong could get corrupt " + $
                                  "DS"
                 endif
              endelse
           end
           'ZXS': begin
              if (not dflag) then begin
                 graff_msg, msgid, "Data found before type defined " + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZXE:') ne -1
              endif else if (data[nset].type ne 9) then begin
                 graff_msg, msgid, $
                            '2-D Data found in function Or 1-D dataset - ignored'
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZXE:') ne -1
              endif else begin
                 if xydata.x_is_2d then xv = dblarr(data[nset].ndata, $
                                                    data[nset].ndata2) $ 
                 else xv = dblarr(data[nset].ndata)
                 readf, ilu, xv
                 xydata.x = ptr_new(xv)
                 readf, ilu, inline
                 if (strpos(inline, 'ZXE:') eq -1) then $
                    graff_msg, msgid, $
                               "WARNING Data X count wrong could get corrupt " + $
                               "DS"
              endelse
           end
           'ZYS': begin
              if (not dflag) then begin
                 graff_msg, msgid, "Data found before type defined " + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZYE:') ne -1
              endif else if (data[nset].type ne 9) then begin
                 graff_msg, msgid, $
                            '2-D Data found in function Or 1-D dataset - ignored'
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZYE:') ne -1
              endif else begin
                 if xydata.y_is_2d then yv = dblarr(data[nset].ndata, $
                                                    data[nset].ndata2) $ $
                 else yv = dblarr(data[nset].ndata2)
                 readf, ilu, yv
                 xydata.y = ptr_new(yv)
                 readf, ilu, inline
                 if (strpos(inline, 'ZYE:') eq -1) then $
                    graff_msg, msgid, $
                               "WARNING Data Y count wrong could get corrupt " + $
                               "DS"
              endelse
           end
           'ZZS': begin
              if (not dflag) then begin
                 graff_msg, msgid, "Data found before type defined " + $
                            "- ignored"
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZZE:') ne -1
              endif else if (data[nset].type ne 9) then begin
                 graff_msg, msgid, $
                            '2-D Data found in function Or 1-D dataset - ignored'
                 repeat readf, ilu, inline  $
                 until strpos(inline, 'ZZE:') ne -1
              endif else begin
                 zv = dblarr(data[nset].ndata, data[nset].ndata2)
                 readf, ilu, zv
                 xydata.z = ptr_new(zv)
                 readf, ilu, inline
                 if (strpos(inline, 'ZZE:') eq -1) then $
                    graff_msg, msgid, $
                               "WARNING Data Z count wrong could get corrupt " + $
                               "DS"
              endelse
           end
           
           'ZX2': if ptr_valid(xydata.x) then graff_msg, msgid, $
              "WARNING: 2-D X data requested after X data " + $
              "acquired" $
           else xydata.x_is_2d = gr_byt_val(tag_val[itag+1], 1)
           'ZY2': if ptr_valid(xydata.y) then graff_msg, msgid, $
              "WARNING: 2-D Y data requested after Y data " + $
              "acquired" $
           else xydata.y_is_2d = gr_byt_val(tag_val[itag+1], 1)

           'DE': begin
              goto, ds_read
           end
           
           Else: graff_msg, msgid, $
                            "Unknown Dataset tag "+tag_val[itag]+" - ignored"
        endcase
        
        
        if nflag && tflag && ~dflag then begin
           dflag = 1b
           case data[nset].type of
              -4: xydata = {graff_zfunct}
              -3: xydata = {graff_pfunct}
              -2: xydata = {graff_funct}
              -1: xydata = {graff_funct}
              9: if (nflag2) then begin
                 xydata = {graff_zdata}
              endif else dflag = 0b
              
              Else: xydata = {graff_xydata}
           endcase
        endif
     endfor
     
     New_line:
     
  endwhile

Ds_read:

  if (not jflag) then begin
     if (data[nset].psym eq 10) then begin
        data[nset].pline = 2
        data[nset].psym = 0
     endif else if (data[nset].psym eq 0) then begin
        data[nset].pline = 1
     endif else if (data[nset].psym lt 0) then begin
        data[nset].pline = 1
        data[nset].psym = abs(data[nset].psym)
     endif else data[nset].pline = 0
  endif

  data[nset].xydata = ptr_new(xydata)
  
end
