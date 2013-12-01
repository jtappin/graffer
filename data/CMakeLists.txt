# Copyright (C) 2013
# James Tappin

# This is free software# you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/graffer.man.in"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer.1"
  @ONLY
  )

install(FILES
#  "${CMAKE_CURRENT_SOURCE_DIR}/c_tables.info"
#  "${CMAKE_CURRENT_SOURCE_DIR}/c_tables.tab"
  "${CMAKE_CURRENT_SOURCE_DIR}/colours.table"
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
