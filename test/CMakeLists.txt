cmake_minimum_required(VERSION 3.20)

# Test suite for ForGE

# Helper function to add tests
function(add_forge_test test_name source_file)
    add_executable(${test_name} ${source_file})
    target_link_libraries(${test_name} PRIVATE forge)
    target_include_directories(${test_name} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
    add_test(NAME ${test_name} COMMAND ${test_name})
endfunction()

# Add tests
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_types.f90)
    add_forge_test(test_types test_types.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_signals.f90)
    add_forge_test(test_signals test_signals.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_window.f90)
    add_forge_test(test_window test_window.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_widgets.f90)
    add_forge_test(test_widgets test_widgets.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_events.f90)
    add_forge_test(test_events test_events.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_containers.f90)
    add_forge_test(test_containers test_containers.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_string_utils.f90)
    add_forge_test(test_string_utils test_string_utils.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_json.f90)
    add_forge_test(test_json test_json.f90)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test_network.f90)
    add_forge_test(test_network test_network.f90)
endif()

