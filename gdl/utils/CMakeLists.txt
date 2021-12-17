# LICENCE:
# Copyright (C) 1995-2021: SJT
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or     
#(at your option) any later version.                                   

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_add.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_annotate.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_convert.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_export.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_get_data.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_info.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_print.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_props.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_show.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_update.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib/utils
)
