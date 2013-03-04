configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/graffer.man.in"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer.1"
  @ONLY
  )

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/c_tables.info"
  "${CMAKE_CURRENT_SOURCE_DIR}/c_tables.tab"
  "${CMAKE_CURRENT_SOURCE_DIR}/graffer.rc"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer
  )
install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/graffer.desktop"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/applications
  )
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/graffer.1"
  DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
  )
