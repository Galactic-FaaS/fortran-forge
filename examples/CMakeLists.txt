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

