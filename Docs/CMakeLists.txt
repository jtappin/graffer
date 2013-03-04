set(images 
  logo.png 
  main.png 
  main-2d.png 
  key.png 
  axis_adv.png 
  Text.png 
  hardopts.png 
  options.png
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
