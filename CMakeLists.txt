cmake_minimum_required(VERSION 3.20)

project(ForGE
    VERSION 1.0.0
    DESCRIPTION "Modern Fortran GUI Environment"
    LANGUAGES Fortran C
)

# Set C++11 for any C++ components if needed
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Fortran standards
set(CMAKE_Fortran_STANDARD 08)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Options for backend selection
option(FORGE_BUILD_SHARED "Build shared library" ON)
option(FORGE_BUILD_EXAMPLES "Build example programs" ON)
option(FORGE_BUILD_TESTS "Build test suite" ON)
option(FORGE_BACKEND_TCL_TK "Build Tcl/Tk backend" ON)
option(FORGE_BACKEND_GTK4 "Build GTK4 backend" OFF)
option(FORGE_BACKEND_QT "Build Qt backend" OFF)
option(FORGE_BACKEND_CUSTOM "Build custom backend" OFF)

# Build type defaults
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -Og -Wall -Wextra -pedantic -fcheck=all -fbacktrace -std=f2018")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -ffast-math -funroll-loops")
    if(WIN32)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static-libgcc -static-libgfortran")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -warn all -check all -traceback -stand f18")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost -ipo")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -Wall -Wextra")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
endif()

# Platform-specific settings
if(WIN32)
    add_definitions(-DWIN32)
    if(MSVC)
        add_compile_options(/W4)
    endif()
elseif(UNIX)
    add_definitions(-DUNIX)
    if(APPLE)
        add_definitions(-DAPPLE)
    else()
        add_definitions(-DLINUX)
    endif()
endif()

# Include paths
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# Core library sources
set(FORGE_CORE_SOURCES
    src/forge.f90
    src/forge_types.f90
    src/forge_errors.f90
    src/forge_window.f90
    src/forge_widgets.f90
    src/forge_events.f90
    src/forge_layout.f90
    src/forge_backend.f90
    src/forge_signals.f90
    src/core/forge_containers.f90
    src/core/forge_string_utils.f90
    src/core/forge_json_parser.f90
    src/core/forge_json.f90
)

# Network sources
set(FORGE_NETWORK_SOURCES
    src/network/forge_http_protocol.f90
    src/network/forge_http.f90
    src/network/forge_socket.f90
    src/network/forge_winsock.f90
)

# Concurrent sources
set(FORGE_CONCURRENT_SOURCES
    src/concurrent/forge_future.f90
    src/concurrent/forge_thread.f90
    src/concurrent/forge_thread_windows.f90
)

# GUI widget sources
set(FORGE_GUI_SOURCES
    src/gui/widgets/forge_checkbox.f90
    src/gui/widgets/forge_combobox.f90
    src/gui/widgets/forge_dateedit.f90
    src/gui/widgets/forge_dialogs.f90
    src/gui/widgets/forge_groupbox.f90
    src/gui/widgets/forge_listview.f90
    src/gui/widgets/forge_menubar.f90
    src/gui/widgets/forge_messagebox.f90
    src/gui/widgets/forge_radiobutton.f90
    src/gui/widgets/forge_scrollarea.f90
    src/gui/widgets/forge_slider.f90
    src/gui/widgets/forge_spinbox.f90
    src/gui/widgets/forge_statusbar.f90
    src/gui/widgets/forge_tabwidget.f90
)

# Backend sources (conditional)
set(FORGE_BACKEND_SOURCES "")

if(FORGE_BACKEND_TCL_TK)
    find_package(TCL 8.6)
    find_package(TK 8.6)
    if(TCL_FOUND AND TK_FOUND)
        list(APPEND FORGE_BACKEND_SOURCES src/backends/forge_tcl_tk.f90)
        message(STATUS "Tcl/Tk backend enabled")
    else()
        message(WARNING "Tcl/Tk not found, disabling Tcl/Tk backend")
        set(FORGE_BACKEND_TCL_TK OFF)
    endif()
endif()

if(FORGE_BACKEND_GTK4)
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GTK4 gtk4)
    if(GTK4_FOUND)
        list(APPEND FORGE_BACKEND_SOURCES src/backends/forge_gtk4.f90)
        message(STATUS "GTK4 backend enabled")
    else()
        message(WARNING "GTK4 not found, disabling GTK4 backend")
        set(FORGE_BACKEND_GTK4 OFF)
    endif()
endif()

if(FORGE_BACKEND_QT)
    find_package(Qt6 COMPONENTS Core Widgets)
    if(Qt6_FOUND)
        list(APPEND FORGE_BACKEND_SOURCES src/backends/forge_qt.f90)
        message(STATUS "Qt backend enabled")
    else()
        message(WARNING "Qt6 not found, disabling Qt backend")
        set(FORGE_BACKEND_QT OFF)
    endif()
endif()

if(FORGE_BACKEND_CUSTOM)
    # Find Cairo library
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(CAIRO cairo)
    if(CAIRO_FOUND)
        list(APPEND FORGE_BACKEND_SOURCES
            src/backends/custom/forge_custom_backend.f90
            src/backends/custom/forge_cairo_bindings.f90
            src/backends/custom/forge_rendering.f90
            src/backends/custom/forge_platform.f90
            src/backends/custom/forge_platform_windows.f90
            src/backends/custom/forge_platform_null.f90
            src/backends/custom/forge_input.f90
        )
        message(STATUS "Custom backend enabled with Cairo")
    else()
        message(WARNING "Cairo not found, disabling Custom backend")
        set(FORGE_BACKEND_CUSTOM OFF)
    endif()
endif()

# Stub backend (fallback)
list(APPEND FORGE_BACKEND_SOURCES src/backends/forge_stub.f90)

# Build library
set(FORGE_ALL_SOURCES
    ${FORGE_CORE_SOURCES}
    ${FORGE_NETWORK_SOURCES}
    ${FORGE_CONCURRENT_SOURCES}
    ${FORGE_GUI_SOURCES}
    ${FORGE_BACKEND_SOURCES}
)

if(FORGE_BUILD_SHARED)
    add_library(forge SHARED ${FORGE_ALL_SOURCES})
    set_target_properties(forge PROPERTIES
        VERSION ${PROJECT_VERSION}
        SOVERSION 1
        EXPORT_NAME forge
    )
else()
    add_library(forge STATIC ${FORGE_ALL_SOURCES})
    set_target_properties(forge PROPERTIES EXPORT_NAME forge)
endif()

# Add alias for consistent usage
add_library(ForGE::forge ALIAS forge)

# Export the target
install(TARGETS forge
    EXPORT ForGETargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

# Link backend libraries
if(FORGE_BACKEND_TCL_TK AND TCL_FOUND AND TK_FOUND)
    target_include_directories(forge PRIVATE ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
    target_link_libraries(forge ${TCL_LIBRARY} ${TK_LIBRARY})
endif()

if(FORGE_BACKEND_GTK4 AND GTK4_FOUND)
    target_include_directories(forge PRIVATE ${GTK4_INCLUDE_DIRS})
    target_link_libraries(forge ${GTK4_LIBRARIES})
endif()

if(FORGE_BACKEND_QT AND Qt6_FOUND)
    target_link_libraries(forge Qt6::Core Qt6::Widgets)
endif()

if(FORGE_BACKEND_CUSTOM AND CAIRO_FOUND)
    target_include_directories(forge PRIVATE ${CAIRO_INCLUDE_DIRS})
    target_link_libraries(forge ${CAIRO_LIBRARIES})
    if(WIN32)
        target_link_libraries(forge ws2_32)
    endif()
endif()

# Install library (removed duplicate install command)

# Install module files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION include/forge
    FILES_MATCHING PATTERN "*.mod"
)

# Install header files
install(DIRECTORY src/
    DESTINATION include/forge
    FILES_MATCHING PATTERN "*.h"
)

# Install pkg-config file
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/forge.pc
    DESTINATION lib/pkgconfig
)

# Install CMake config files
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/ForGEConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/ForGEConfig.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/ForGEConfig.cmake
    INSTALL_DESTINATION lib/cmake/ForGE
)

install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/ForGEConfig.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/ForGEConfigVersion.cmake
    DESTINATION lib/cmake/ForGE
)

# Create export set first
export(EXPORT ForGETargets
    FILE ${CMAKE_CURRENT_BINARY_DIR}/ForGETargets.cmake
    NAMESPACE ForGE::
)

# Export targets for CMake config
install(EXPORT ForGETargets
    FILE ForGETargets.cmake
    NAMESPACE ForGE::
    DESTINATION lib/cmake/ForGE
)

# Build examples
if(FORGE_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Build tests
if(FORGE_BUILD_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

# Generate pkg-config file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/forge.pc.in
    ${CMAKE_CURRENT_BINARY_DIR}/forge.pc
    @ONLY
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/forge.pc
    DESTINATION lib/pkgconfig
)

# Print configuration summary
message(STATUS "")
message(STATUS "ForGE Configuration Summary:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Build shared: ${FORGE_BUILD_SHARED}")
message(STATUS "  Build examples: ${FORGE_BUILD_EXAMPLES}")
message(STATUS "  Build tests: ${FORGE_BUILD_TESTS}")
message(STATUS "  Backends:")
message(STATUS "    Tcl/Tk: ${FORGE_BACKEND_TCL_TK}")
message(STATUS "    GTK4: ${FORGE_BACKEND_GTK4}")
message(STATUS "    Qt: ${FORGE_BACKEND_QT}")
message(STATUS "    Custom: ${FORGE_BACKEND_CUSTOM}")
message(STATUS "")

