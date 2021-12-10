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
add_subdirectory(utils)

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_as_xa.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_as_xr.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_as_ya.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_as_yr.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_auto_delete.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_autoscale.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_bin_ds.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_bin_ds_v3.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_bin_save.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_bin_txt.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_bin_txt_v3.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_byt_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_dbl_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_flt_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_font_remap.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_asc.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_bin.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_bin_v3.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_ds.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_full_dir.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_get_v1.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_int_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_lon_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_new_ds.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_rc_get.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_split_dir.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/gr_str_rd.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_2d__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_add.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_annotate.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_clear.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_convert.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_data__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_define__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_export.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_funct__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_get.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_get_data.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_get_rec.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_hard__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_ids__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_init.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_key__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_msg.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_open.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_opts__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_pfunct__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_props.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_put_rec.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_style__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_text__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_trans__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_version.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_xydata__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_zdata__define.pro"
  "${CMAKE_CURRENT_SOURCE_DIR}/graff_zfunct__define.pro"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer/lib
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.sh"
  "${CMAKE_CURRENT_BINARY_DIR}/graffer_gdl_init.csh"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/graffer
)
