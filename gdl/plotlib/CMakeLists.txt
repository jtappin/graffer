# LICENCE:
# Copyright (C) 1995-2021: SJT
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or     
#(at your option) any later version.                                   

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_1dd_plot.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_1df_plot.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_2dd_plot.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_2df_plot.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_ctable.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_coord_convert.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_display_img.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_err_r.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_err_th.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_err_x.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_err_y.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_fra2norm.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_pl_axes.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_pl_key.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_plot_object.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_pl_text.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_symdef.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_time_fmt.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_tri_area.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib/plotlib
  )
