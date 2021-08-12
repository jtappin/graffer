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

 
cmake_minimum_required(VERSION 2.8.5)
project(graffer Fortran C)
include(CheckCSourceCompiles)

# Note that we override the lib directory here because most
# 64-bit systems don't actually work if we use the default.
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
# set(CMAKE_BUILD_TYPE debug)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
endif()
message(STATUS "Build type is: ${CMAKE_BUILD_TYPE}")

# Removed -ffpe-trap=invalid,zero,overflow as it gives a lot of crashes deep
# in the call stack in Gtk3
set(CMAKE_Fortran_FLAGS_DEBUG
"-g -pthread -Wall -Wtabs -fcheck=all -fbacktrace -Wno-unused-dummy-argument -no-pie")
set(CMAKE_Fortran_FLAGS_RELEASE
"-pthread -O3 -mtune=native -march=native -no-pie")

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# uninstall target
configure_file(
	"${PROJECT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)
add_custom_target(uninstall COMMAND ${CMAKE_COMMAND} -P
${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

# Find Gtk-Fortran, if Gtk3 is preferred, then switch the order of
# the scans below, or to use only one version use -DGTK=2 or -DGTK=3

if (GTK EQUAL 2)
  find_package(Gtk2Fortran)
  if (NOT GTK2FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk2Fortran not found, aborting")
  endif()
  add_definitions("-DGTK2")
elseif (GTK EQUAL 3) 
  find_package(Gtk3Fortran)
  if (NOT GTK3FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk3Fortran not found, aborting")
  endif()
  add_definitions("-DGTK3")
else ()
  find_package(Gtk2Fortran)
  if (NOT GTK2FORTRAN_FOUND)
    message(STATUS "Gtk2Fortran not found, trying Gtk3Fortran")
    find_package(Gtk3Fortran)
    if (NOT GTK3FORTRAN_FOUND)
      message (FATAL_ERROR "Gtk3Fortran not found either, aborting")
    endif()
    add_definitions("-DGTK3")
  else (GTK2FORTRAN_FOUND)
    add_definitions("-DGTK2")
  endif()
endif()
find_package(PlplotF95)
if(NOT PLPLOT_FOUND)
  message(FATAL_ERROR "PLPLOT not found")
endif()
set(CMAKE_REQUIRED_LIBRARIES "${PLPLOT_LIBRARIES}")
set(CMAKE_REQUIRED_INCLUDES "${PLPLOT_INCLUDE_DIR}")
check_c_source_compiles("
#include <plplot/plplot.h> 
int main(int argc, void *argv[])
{
  PLFLT w = 0.5;
  plinit();
  plwidth(w);
}" HAVE_PLWIDTH)
if(HAVE_PLWIDTH)
  add_definitions("-DHAVE_PLWIDTH")
endif(HAVE_PLWIDTH)

find_program(GDL "gdl")
if (NOT GDL)
  find_program(GDL "idl")
  if (NOT GDL)
    message("Neither 'gdl' nor 'idl' was found, graffer will not be able to evaluate functions")
  endif()
endif()

find_program(PDFLATEX "pdflatex")
if (NOT PDFLATEX)
  message(WARNING "pdflatex, not found cannot generate PDF manuals")
endif()

include_directories(${GTKFORTRAN_MODULE_DIR})
include_directories(${PLPLOT_MODULE_DIR})
link_directories(${GTKFORTRAN_LIBRARY_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})
link_directories(/usr/local/lib)


add_subdirectory(src)
add_subdirectory(data)
add_subdirectory(gdl)
if (PDFLATEX)
  add_subdirectory(Docs)
endif()
