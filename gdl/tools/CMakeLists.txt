# LICENCE:
# Copyright (C) 1995-2021: SJT
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or     
#(at your option) any later version.                                   

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/aspect.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/fractile.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_cvttobm.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_interpol_ss.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_min_nz.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_strpad.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/is_gdl.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/match_in.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/truth.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/has_tag.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib/tools
  )

