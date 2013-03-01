 
cmake_minimum_required(VERSION 2.8.5)
project(graffer Fortran)

# Note that we override the lib directory here because most
# 64-bit systems don't actually work if we use the default.
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
set(CMAKE_BUILD_TYPE debug)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
endif()
message(STATUS "Build type is: ${CMAKE_BUILD_TYPE}")

# Removed -ffpe-trap=invalid,zero,overflow as it gives a lot of crashes deep
# in the call stack in Gtk3
set(CMAKE_Fortran_FLAGS_DEBUG
"-g -pthread -Wall -Wtabs -fcheck=all -fbacktrace -Wno-unused-dummy-argument")
set(CMAKE_Fortran_FLAGS_RELEASE
"-pthread -O3 -mtune=native -march=native")

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# uninstall target
configure_file(
	"${PROJECT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)
add_custom_target(uninstall COMMAND ${CMAKE_COMMAND} -P
${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

# Find Gtk-Fortran, if Gtk2 is preferred, then switch the order of
# the scans below.

if (GTK EQUAL 2)
  find_package(Gtk2Fortran)
  if (NOT GTK2FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk2Fortran not found, aborting")
  endif()
elseif (GTK EQUAL 3) 
  find_package(Gtk3Fortran)
  if (NOT GTK3FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk3Fortran not found, aborting")
  endif()
else ()
  find_package(Gtk3Fortran)
  if (NOT GTK3FORTRAN_FOUND)
    message(STATUS "Gtk3Fortran not found trying Gtk3Fortran")
    find_package(Gtk2Fortran)
    if (NOT GTK2FORTRAN_FOUND)
      message (FATAL_ERROR "Gtk2Fortran not found either, aborting")
    endif()
  endif()
endif()
find_package(PlplotF95)
if(NOT PLPLOT_FOUND)
  message(FATAL_ERROR "PLPLOT not found")
endif()

find_program(GDL "gdl")
if (NOT GDL)
  find_program(GDL "idl")
  if (NOT GDL)
    message("Neither 'gdl' not 'idl' was found, graffer will not be able to evaluate functions")
  endif()
endif()

include_directories(${GTKFORTRAN_MODULE_DIR})
include_directories(${PLPLOT_MODULE_DIR})
link_directories(${GTKFORTRAN_LIBRARY_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})
link_directories(/usr/local/lib)


add_subdirectory(src)
add_subdirectory(data)
add_subdirectory(gdl)
