# LICENCE:
# Copyright (C) 1995-2021: SJT
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or     
#(at your option) any later version.                                   

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/cw_enter.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/cw_pdmenu_plus.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/cw_spin_box.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_cw_filesel.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib/widgets
  )
