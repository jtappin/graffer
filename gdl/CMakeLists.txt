# LICENCE:
# Copyright (C) 1995-2021: SJT
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or     
#(at your option) any later version.                                   

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/init.sh.in"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.sh"
  @ONLY
  )
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/init.csh.in"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.csh"
  @ONLY
  )

add_subdirectory(definitions)
add_subdirectory(lib)
add_subdirectory(plotlib)
add_subdirectory(tools)
add_subdirectory(utils)
if (GDL_GUI)
  add_subdirectory(widgets)
  add_subdirectory(gui)
  add_subdirectory(bitmaps)
endif()

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_version.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib
  )

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.sh"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.csh"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer
  )
