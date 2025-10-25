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
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -warn all -check all -traceback -stand f18")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost -ipo")
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
    list(APPEND FORGE_BACKEND_SOURCES src/backends/forge_custom.f90)
    message(STATUS "Custom backend enabled")
endif()

# Build library
set(FORGE_ALL_SOURCES ${FORGE_CORE_SOURCES} ${FORGE_BACKEND_SOURCES})

if(FORGE_BUILD_SHARED)
    add_library(forge SHARED ${FORGE_ALL_SOURCES})
    set_target_properties(forge PROPERTIES VERSION ${PROJECT_VERSION})
    set_target_properties(forge PROPERTIES SOVERSION 1)
else()
    add_library(forge STATIC ${FORGE_ALL_SOURCES})
endif()

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

# Install library
install(TARGETS forge
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

# Install module files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION include/forge
    FILES_MATCHING PATTERN "*.mod"
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

