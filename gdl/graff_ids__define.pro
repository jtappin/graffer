pro graff_ids__define
;+
; NAME:
;	graff_ids__define
;
;
; PURPOSE:
;	Define the graffer widget IDs structure
;
;
; CATEGORY:
;	graffer
;
;
; CALLING SEQUENCE:
;	implicit
;
;
; MODIFICATION HISTORY:
;	Extracted: 30/6/05; SJT
;	Add support for a second Y-scale: 22/12/11; SJT
;	Add "current" ds only id: 26/1/12; SJT
;-

Ids = { graff_ids, $
        Graffer: 0l, $
        Name:    0l, $
        Title:   0l, $
        Subtitle:0l, $
        Charsize:0l, $
        Axthick: 0l, $
        textmode:0l, $
        Mode:    0l, $
        Xtitle:  0l, $
        Xmin:    0l, $
        Xmax:    0l, $
        Xlog:    0l, $
        x_origin:0l, $
        Xsty:    lonarr(9), $
        y_right: 0l, $
        ybase_r: 0l, $
        y_box:   0l, $
        y_axis:  0l, $
        Ytitle:  0l, $
        Ymin:    0l, $
        Ymax:    0l, $
        Ylog:    0l, $
        Ysty:    lonarr(9), $
        Ytitle_r:  0l, $
        Ymin_r:    0l, $
        Ymax_r:    0l, $
        Ylog_r:    0l, $
        Ysty_r:    lonarr(9), $
        Plopts:  lonarr(2), $
        zopts:   {graff_zids, $
                  bases: lonarr(2), $
                  c_auto: 0l, $
                  c_levels: 0l, $
                  c_nlevels: 0l, $
                  c_colour: 0l, $
                  c_thick: 0l, $
                  c_style: 0l, $
                  c_type: 0l, $
                  c_label: 0l, $
                  c_charsize: 0l, $
                  i_range: lonarr(2), $
                  i_log: 0l, $
                  i_invert: 0l, $
                  i_pxsz: 0l, $
                  i_ctable: 0l, $
                  i_gamma: 0l, $
                  i_missid: 0l}, $
        export:  0l, $
        Cset:    0l, $
        Descr:   0l, $
        Pline:   0l, $
        Psym:    0l, $
        Symsize: 0l, $
        Colour:  0l, $
        Line:    0l, $
        Thick:   0l, $
        Dsxtra:  lonarr(3), $
        Clip:    0l, $
        Zmode:   0l, $
        current: 0l, $
        Xcp:     0l, $
        Ycp:     0l, $
        Draw:    0l, $
        Windex:  0l, $
        Message: 0l, $
        Hlptxt:  0l, $
        Popmsg:  0l, $
        fontmenu:0l, $
        Chtick:  0l $
      }

end
