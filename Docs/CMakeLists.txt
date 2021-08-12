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

set(images 
  logo.png 
  Main.png 
  Main-2.png 
  key.png 
  Axis-adv.png 
  text.png 
  hardcopy.png 
  options.png
  Sort.png
  Rescale.png
)

add_custom_command(
  OUTPUT ${images}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND cp -v ${images} ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(docimages ALL DEPENDS ${images})

add_custom_command(
  OUTPUT Graffer.pdf
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Graffer.tex
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Graffer.tex
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Graffer.tex
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Graffer.tex ${images}
  )
  
add_custom_command(
  OUTPUT Format.pdf
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Format.tex
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Format.tex
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/Format.tex
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Format.tex
  )

add_custom_target(alldocs ALL DEPENDS Graffer.pdf Format.pdf docimages)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/Graffer.pdf"
  "${CMAKE_CURRENT_BINARY_DIR}/Format.pdf"
  DESTINATION ${CMAKE_INSTALL_DOCDIR}
  )
