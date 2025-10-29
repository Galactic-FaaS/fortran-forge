cmake_minimum_required(VERSION 3.20)

# Example programs for ForGE

# Helper function to add examples
function(add_forge_example example_name source_file)
    add_executable(${example_name} ${example_name}/${source_file})
    target_link_libraries(${example_name} PRIVATE forge)
    target_include_directories(${example_name} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
endfunction()

# Add all examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/hello_world/hello_world.f90)
    add_forge_example(hello_world hello_world.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/button_demo/button_demo.f90)
    add_forge_example(button_demo button_demo.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/menu_demo/menu_demo.f90)
    add_forge_example(menu_demo menu_demo.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/text_editor/text_editor.f90)
    add_forge_example(text_editor text_editor.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_demo/graphics_demo.f90)
    add_forge_example(graphics_demo graphics_demo.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/form_builder/form_builder.f90)
    add_forge_example(form_builder form_builder.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/multi_window/multi_window.f90)
    add_forge_example(multi_window multi_window.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/theme_showcase/theme_showcase.f90)
    add_forge_example(theme_showcase theme_showcase.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/custom_window/custom_window.f90)
    add_forge_example(custom_window custom_window.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/cairo_rendering/cairo_rendering.f90)
    add_forge_example(cairo_rendering cairo_rendering.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/interactive_button/interactive_button.f90)
    add_forge_example(interactive_button interactive_button.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/signals_demo/signals_demo.f90)
    add_forge_example(signals_demo signals_demo.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/calculator/calculator.f90)
    add_forge_example(calculator calculator.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/todo_app/todo_app.f90)
    add_forge_example(todo_app todo_app.f90)
endif()

# Basic Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_label/basic_label.f90)
    add_forge_example(basic_label basic_label.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_entry/basic_entry.f90)
    add_forge_example(basic_entry basic_entry.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_checkbox/basic_checkbox.f90)
    add_forge_example(basic_checkbox basic_checkbox.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_radio/basic_radio.f90)
    add_forge_example(basic_radio basic_radio.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_combo/basic_combo.f90)
    add_forge_example(basic_combo basic_combo.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_slider/basic_slider.f90)
    add_forge_example(basic_slider basic_slider.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_spin/basic_spin.f90)
    add_forge_example(basic_spin basic_spin.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_textview/basic_textview.f90)
    add_forge_example(basic_textview basic_textview.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_layout_vertical/basic_layout_vertical.f90)
    add_forge_example(basic_layout_vertical basic_layout_vertical.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_layout_horizontal/basic_layout_horizontal.f90)
    add_forge_example(basic_layout_horizontal basic_layout_horizontal.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/basic_layout_grid/basic_layout_grid.f90)
    add_forge_example(basic_layout_grid basic_layout_grid.f90)
endif()

# Widget Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_groupbox/widget_groupbox.f90)
    add_forge_example(widget_groupbox widget_groupbox.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_tabwidget/widget_tabwidget.f90)
    add_forge_example(widget_tabwidget widget_tabwidget.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_listview/widget_listview.f90)
    add_forge_example(widget_listview widget_listview.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_tablewidget/widget_tablewidget.f90)
    add_forge_example(widget_tablewidget widget_tablewidget.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_menubar/widget_menubar.f90)
    add_forge_example(widget_menubar widget_menubar.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_progressbar/widget_progressbar.f90)
    add_forge_example(widget_progressbar widget_progressbar.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_messagebox/widget_messagebox.f90)
    add_forge_example(widget_messagebox widget_messagebox.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_filedialog/widget_filedialog.f90)
    add_forge_example(widget_filedialog widget_filedialog.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_scrollarea/widget_scrollarea.f90)
    add_forge_example(widget_scrollarea widget_scrollarea.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_splitter/widget_splitter.f90)
    add_forge_example(widget_splitter widget_splitter.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/widget_stackedwidget/widget_stackedwidget.f90)
    add_forge_example(widget_stackedwidget widget_stackedwidget.f90)
endif()

# Event Handling Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_basic_signals/event_basic_signals.f90)
    add_forge_example(event_basic_signals event_basic_signals.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_mouse_events/event_mouse_events.f90)
    add_forge_example(event_mouse_events event_mouse_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_keyboard_events/event_keyboard_events.f90)
    add_forge_example(event_keyboard_events event_keyboard_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_custom_events/event_custom_events.f90)
    add_forge_example(event_custom_events event_custom_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_event_filters/event_event_filters.f90)
    add_forge_example(event_event_filters event_event_filters.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_async_events/event_async_events.f90)
    add_forge_example(event_async_events event_async_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_timer_events/event_timer_events.f90)
    add_forge_example(event_timer_events event_timer_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_focus_events/event_focus_events.f90)
    add_forge_example(event_focus_events event_focus_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_paint_events/event_paint_events.f90)
    add_forge_example(event_paint_events event_paint_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/event_resize_events/event_resize_events.f90)
    add_forge_example(event_resize_events event_resize_events.f90)
endif()

# Graphics Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_basic_drawing/graphics_basic_drawing.f90)
    add_forge_example(graphics_basic_drawing graphics_basic_drawing.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_shapes/graphics_shapes.f90)
    add_forge_example(graphics_shapes graphics_shapes.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_colors/graphics_colors.f90)
    add_forge_example(graphics_colors graphics_colors.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_text_rendering/graphics_text_rendering.f90)
    add_forge_example(graphics_text_rendering graphics_text_rendering.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/graphics_animation_basic/graphics_animation_basic.f90)
    add_forge_example(graphics_animation_basic graphics_animation_basic.f90)
endif()

# Networking Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_http_client/network_http_client.f90)
    add_forge_example(network_http_client network_http_client.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_tcp_server/network_tcp_server.f90)
    add_forge_example(network_tcp_server network_tcp_server.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_tcp_client/network_tcp_client.f90)
    add_forge_example(network_tcp_client network_tcp_client.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_ssl_client/network_ssl_client.f90)
    add_forge_example(network_ssl_client network_ssl_client.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_websocket/network_websocket.f90)
    add_forge_example(network_websocket network_websocket.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/network_rest_api/network_rest_api.f90)
    add_forge_example(network_rest_api network_rest_api.f90)
endif()

# Threading Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/thread_basic_worker/thread_basic_worker.f90)
    add_forge_example(thread_basic_worker thread_basic_worker.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/thread_concurrent_tasks/thread_concurrent_tasks.f90)
    add_forge_example(thread_concurrent_tasks thread_concurrent_tasks.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/thread_thread_pool/thread_thread_pool.f90)
    add_forge_example(thread_thread_pool thread_thread_pool.f90)
endif()

# Database Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_connections/database_connections.f90)
    add_forge_example(database_connections database_connections.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_sql_queries/database_sql_queries.f90)
    add_forge_example(database_sql_queries database_sql_queries.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_model_view/database_model_view.f90)
    add_forge_example(database_model_view database_model_view.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_prepared_statements/database_prepared_statements.f90)
    add_forge_example(database_prepared_statements database_prepared_statements.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_transactions/database_transactions.f90)
    add_forge_example(database_transactions database_transactions.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_error_handling/database_error_handling.f90)
    add_forge_example(database_error_handling database_error_handling.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/database_crud_operations/database_crud_operations.f90)
    add_forge_example(database_crud_operations database_crud_operations.f90)
endif()

# Advanced Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/advanced_text_editor/advanced_text_editor.f90)
    add_forge_example(advanced_text_editor advanced_text_editor.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/advanced_calculator_app/advanced_calculator_app.f90)
    add_forge_example(advanced_calculator_app advanced_calculator_app.f90)
endif()

# Platform-Specific Examples
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/platform_windows_registry/platform_windows_registry.f90)
    add_forge_example(platform_windows_registry platform_windows_registry.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/platform_linux_system_info/platform_linux_system_info.f90)
    add_forge_example(platform_linux_system_info platform_linux_system_info.f90)
endif()

