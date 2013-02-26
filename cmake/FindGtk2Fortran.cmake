# copyright : (c) 2013 James Tappin
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#

# As well as returning the standard variables expected from a pkg-config based
# find, the main variables are returned as "unversioned" forms that will allow 
# a top-level CMakeLists.txt to select whichever version is available and
# use that.

find_package(PkgConfig)
pkg_check_modules(GTK2FORTRAN QUIET gtk-2-fortran)

find_path(GTK2FORTRAN_MODULE_DIR NAMES gtk.mod 
  PATHS ${GTK2FORTRAN_INCLUDE_DIRS}) 

# We set these to allow automatic fail-overs from Gtk2->Gtk3 or vice-versa.

set(GTKFORTRAN_LIBRARIES ${GTK2FORTRAN_LIBRARIES})
set(GTKFORTRAN_LIBRARY_DIRS ${GTK2FORTRAN_LIBRARY_DIRS})
set(GTKFORTRAN_INCLUDE_DIRS ${GTK2FORTRAN_INCLUDE_DIRS})
set(GTKFORTRAN_MODULE_DIR ${GTK2FORTRAN_MODULE_DIR})

mark_as_advanced(
  GTK2FORTRAN_MODULE_DIR
  GTKFORTRAN_LIBRARIES
  GTKFORTRAN_LIBRARY_DIRS
  GTKFORTRAN_INCLUDE_DIR
  GTKFORTRAN_MODULE_DIR
)
