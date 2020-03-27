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

set(sources
  "graffer.f90"
  "gr_1d_opts.f90"
  "gr_2d_opts.f90"
  "gr_axis.f90"
  "gr_axis_widgets.f90"
  "gr_axis_adv_widgets.f90"
  "gr_axis_time_widgets.f90"
  "gr_axis_autoscale.f90"
  "gr_cb_common.f90"
  "gr_colours.f90"
  "gr_drawing.f90"
  "gr_drawing_buttons.f90"
  "gr_ds_data.f90"
  "gr_ds_selector.f90"
  "gr_eval.f90"
  "gr_file.f90"
  "gr_file_ascii.f90"
  "gr_general.f90"
  "gr_gui.f90"
  "gr_idl_types.f90"
  "gr_menubar.f90"
  "gr_mode.f90"
  "gr_record.f90"
  "gr_utils.f90"
  "graff_globals.f90"
  "graff_types.f90"
  "graff_version.f90"
  "gr_plot.f90"
  "gr_plot_utils.f90"
  "gr_plot_tools.f90"
  "gr_plot_procs.f90"
  "gr_ds_tools.f90"
  "gr_ds_widgets.f90"
  "gr_ds_editor_widgets.f90"
  "gr_ds_function_widgets.f90"
  "gr_ds_selector_widgets.f90"
  "gr_ds_merge_widgets.f90"
  "gr_ds_reorder_widgets.f90"
  "gr_ds_copy_widgets.f90"
  "gr_ds_rescale_widgets.f90"
  "gr_ds_fit_widgets.f90"
  "gr_menu_widgets.f90"
  "gr_menu_hc_widgets.f90"
  "gr_menu_opt_widgets.f90"
  "gr_general_widgets.f90"
  "gr_general_corner_widgets.f90"
  "gr_general_key_widgets.f90"
  "gr_general_comment_widgets.f90"
  "gr_fitting.f90"
  "gr_opt_init.f90"
  "graff_init.f90"
  "gr_sort.f90"
  "gr_text_widgets.f90"
  "gr_text_pick_widgets.f90"
  "gr_text_utils.f90"
  "gr_msg.f90"
  "gr_shading.f90"
  )

add_executable(graffer ${sources})
target_link_libraries(graffer ${GTKFORTRAN_LIBRARIES})
target_link_libraries(graffer ${PLPLOT_LIBRARIES})

install(PROGRAMS
  "${CMAKE_CURRENT_BINARY_DIR}/graffer"
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
