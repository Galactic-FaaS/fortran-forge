!> @brief Project Template Generator for ForGE Qt
!> @details Generates project skeletons with Qt-style tooling integration
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_project_generator
    use iso_c_binding
    use iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none

    character(len=:), allocatable :: project_name
    character(len=:), allocatable :: project_type
    character(len=:), allocatable :: output_dir
    logical :: verbose = .false.
    logical :: with_tests = .true.
    logical :: with_examples = .true.
    integer :: i, argc

    ! Command line argument parsing
    argc = command_argument_count()

    if (argc < 1) then
        call print_usage()
        stop 1
    end if

    ! Parse arguments
    i = 1
    do while (i <= argc)
        call get_command_argument(i, project_name)
        if (project_name(1:1) == '-') then
            select case (project_name)
            case ('-t', '--type')
                i = i + 1
                if (i > argc) then
                    write(error_unit, *) "Error: Missing project type after ", trim(project_name)
                    stop 1
                end if
                call get_command_argument(i, project_type)
            case ('-o', '--output')
                i = i + 1
                if (i > argc) then
                    write(error_unit, *) "Error: Missing output directory after ", trim(project_name)
                    stop 1
                end if
                call get_command_argument(i, output_dir)
            case ('--no-tests')
                with_tests = .false.
            case ('--no-examples')
                with_examples = .false.
            case ('-v', '--verbose')
                verbose = .true.
            case ('-h', '--help')
                call print_usage()
                stop 0
            case default
                write(error_unit, *) "Error: Unknown option ", trim(project_name)
                call print_usage()
                stop 1
            end select
        else
            ! Project name
            if (.not. allocated(project_name)) then
                project_name = project_name
            end if
        end if
        i = i + 1
    end do

    if (.not. allocated(project_name)) then
        write(error_unit, *) "Error: No project name specified"
        call print_usage()
        stop 1
    end if

    ! Set defaults
    if (.not. allocated(project_type)) project_type = "application"
    if (.not. allocated(output_dir)) output_dir = project_name

    ! Generate project
    call generate_project(project_name, project_type, output_dir, with_tests, with_examples, verbose)

contains

    subroutine print_usage()
        write(output_unit, *) "ForGE Project Template Generator"
        write(output_unit, *) "Usage: forge_project_generator [options] <project_name>"
        write(output_unit, *) ""
        write(output_unit, *) "Options:"
        write(output_unit, *) "  -t, --type <type>     Project type (application, library, plugin) [default: application]"
        write(output_unit, *) "  -o, --output <dir>    Output directory [default: <project_name>]"
        write(output_unit, *) "  --no-tests            Don't generate test files"
        write(output_unit, *) "  --no-examples         Don't generate example files"
        write(output_unit, *) "  -v, --verbose         Verbose output"
        write(output_unit, *) "  -h, --help            Show this help"
        write(output_unit, *) ""
        write(output_unit, *) "Generates a complete ForGE Qt project with build system integration."
    end subroutine print_usage

    subroutine generate_project(project_name, project_type, output_dir, with_tests, with_examples, verbose)
        character(len=*), intent(in) :: project_name, project_type, output_dir
        logical, intent(in) :: with_tests, with_examples, verbose
        character(len=:), allocatable :: project_dir

        project_dir = output_dir

        if (verbose) then
            write(output_unit, *) "Generating ", trim(project_type), " project: ", trim(project_name)
            write(output_unit, *) "Output directory: ", trim(project_dir)
        end if

        ! Create project directory structure
        call create_directory_structure(project_dir, verbose)

        ! Generate project files based on type
        select case (trim(project_type))
        case ('application')
            call generate_application_project(project_name, project_dir, with_tests, with_examples, verbose)
        case ('library')
            call generate_library_project(project_name, project_dir, with_tests, with_examples, verbose)
        case ('plugin')
            call generate_plugin_project(project_name, project_dir, with_tests, with_examples, verbose)
        case default
            write(error_unit, *) "Error: Unknown project type ", trim(project_type)
            stop 1
        end select

        if (verbose) then
            write(output_unit, *) "Project generation completed successfully"
        end if

    end subroutine generate_project

    subroutine create_directory_structure(base_dir, verbose)
        character(len=*), intent(in) :: base_dir
        logical, intent(in) :: verbose

        ! Create main directories
        call create_dir(base_dir)
        call create_dir(base_dir // "/src")
        call create_dir(base_dir // "/include")
        call create_dir(base_dir // "/resources")
        call create_dir(base_dir // "/translations")
        call create_dir(base_dir // "/docs")

        if (verbose) then
            write(output_unit, *) "Created directory structure"
        end if

    end subroutine create_directory_structure

    subroutine create_dir(dir_path)
        character(len=*), intent(in) :: dir_path
        integer :: ios

        ! Use system command to create directory
        call execute_command_line('mkdir -p "' // trim(dir_path) // '"', wait=.true.)

    end subroutine create_dir

    subroutine generate_application_project(project_name, project_dir, with_tests, with_examples, verbose)
        character(len=*), intent(in) :: project_name, project_dir
        logical, intent(in) :: with_tests, with_examples, verbose

        ! Generate CMakeLists.txt
        call generate_cmake_lists(project_name, project_dir, "application", with_tests, with_examples)

        ! Generate FPM manifest
        call generate_fpm_toml(project_name, project_dir, "application", with_tests, with_examples)

        ! Generate main application file
        call generate_main_app(project_name, project_dir)

        ! Generate resource file
        call generate_resource_file(project_name, project_dir)

        ! Generate translation file
        call generate_translation_file(project_name, project_dir)

        ! Generate README
        call generate_readme(project_name, project_dir, "application")

        if (with_tests) then
            call create_dir(project_dir // "/test")
            call generate_test_files(project_name, project_dir)
        end if

        if (with_examples) then
            call create_dir(project_dir // "/examples")
            call generate_example_files(project_name, project_dir)
        end if

    end subroutine generate_application_project

    subroutine generate_library_project(project_name, project_dir, with_tests, with_examples, verbose)
        character(len=*), intent(in) :: project_name, project_dir
        logical, intent(in) :: with_tests, with_examples, verbose

        ! Generate CMakeLists.txt for library
        call generate_cmake_lists(project_name, project_dir, "library", with_tests, with_examples)

        ! Generate FPM manifest for library
        call generate_fpm_toml(project_name, project_dir, "library", with_tests, with_examples)

        ! Generate library module
        call generate_library_module(project_name, project_dir)

        ! Generate README
        call generate_readme(project_name, project_dir, "library")

        if (with_tests) then
            call create_dir(project_dir // "/test")
            call generate_test_files(project_name, project_dir)
        end if

        if (with_examples) then
            call create_dir(project_dir // "/examples")
            call generate_example_files(project_name, project_dir)
        end if

    end subroutine generate_library_project

    subroutine generate_plugin_project(project_name, project_dir, with_tests, with_examples, verbose)
        character(len=*), intent(in) :: project_name, project_dir
        logical, intent(in) :: with_tests, with_examples, verbose

        ! Generate CMakeLists.txt for plugin
        call generate_cmake_lists(project_name, project_dir, "plugin", with_tests, with_examples)

        ! Generate FPM manifest for plugin
        call generate_fpm_toml(project_name, project_dir, "plugin", with_tests, with_examples)

        ! Generate plugin module
        call generate_plugin_module(project_name, project_dir)

        ! Generate README
        call generate_readme(project_name, project_dir, "plugin")

        if (with_tests) then
            call create_dir(project_dir // "/test")
            call generate_test_files(project_name, project_dir)
        end if

    end subroutine generate_plugin_project

    subroutine generate_cmake_lists(project_name, project_dir, project_type, with_tests, with_examples)
        character(len=*), intent(in) :: project_name, project_dir, project_type
        logical, intent(in) :: with_tests, with_examples
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/CMakeLists.txt"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create CMakeLists.txt"
            return
        end if

        write(unit, '(A)') "cmake_minimum_required(VERSION 3.16)"
        write(unit, '(A)') "project(" // trim(project_name) // " LANGUAGES Fortran)"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Set Fortran standard"
        write(unit, '(A)') "set(CMAKE_Fortran_STANDARD 2008)"
        write(unit, '(A)') "set(CMAKE_Fortran_STANDARD_REQUIRED ON)"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Find ForGE Qt"
        write(unit, '(A)') "find_package(ForGE REQUIRED)"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Include directories"
        write(unit, '(A)') "include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Source files"
        write(unit, '(A)') "set(SOURCES"
        write(unit, '(A)') "    src/" // trim(project_name) // ".f90"
        write(unit, '(A)') ")"

        if (trim(project_type) == "application") then
            write(unit, '(A)') ""
            write(unit, '(A)') "# Create executable"
            write(unit, '(A)') "add_executable(" // trim(project_name) // " ${SOURCES})"
            write(unit, '(A)') "target_link_libraries(" // trim(project_name) // " ForGE::ForGE)"
        else if (trim(project_type) == "library") then
            write(unit, '(A)') ""
            write(unit, '(A)') "# Create library"
            write(unit, '(A)') "add_library(" // trim(project_name) // " ${SOURCES})"
            write(unit, '(A)') "target_link_libraries(" // trim(project_name) // " ForGE::ForGE)"
            write(unit, '(A)') ""
            write(unit, '(A)') "# Install library"
            write(unit, '(A)') "install(TARGETS " // trim(project_name) // " DESTINATION lib)"
            write(unit, '(A)') "install(FILES include/" // trim(project_name) // ".f90 DESTINATION include)"
        else if (trim(project_type) == "plugin") then
            write(unit, '(A)') ""
            write(unit, '(A)') "# Create shared library plugin"
            write(unit, '(A)') "add_library(" // trim(project_name) // " SHARED ${SOURCES})"
            write(unit, '(A)') "target_link_libraries(" // trim(project_name) // " ForGE::ForGE)"
            write(unit, '(A)') ""
            write(unit, '(A)') "# Set plugin properties"
            write(unit, '(A)') "set_target_properties(" // trim(project_name) // " PROPERTIES"
            write(unit, '(A)') "    PREFIX """""
            write(unit, '(A)') "    SUFFIX """""
            write(unit, '(A)') ")"
        end if

        write(unit, '(A)') ""
        write(unit, '(A)') "# Custom targets for ForGE tooling"
        write(unit, '(A)') "find_program(FORGE_MOC forge_moc)"
        write(unit, '(A)') "find_program(FORGE_RCC forge_rcc)"
        write(unit, '(A)') "find_program(FORGE_UIC forge_uic)"
        write(unit, '(A)') "find_program(FORGE_LUPDATE forge_lupdate)"
        write(unit, '(A)') "find_program(FORGE_LRELEASE forge_lrelease)"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Generate meta-object code"
        write(unit, '(A)') "add_custom_command("
        write(unit, '(A)') "    OUTPUT moc_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    COMMAND ${FORGE_MOC} src/" // trim(project_name) // ".f90 -o moc_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    DEPENDS src/" // trim(project_name) // ".f90"
        write(unit, '(A)') "    COMMENT ""Generating meta-object code"""
        write(unit, '(A)') ")"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Generate resource code"
        write(unit, '(A)') "add_custom_command("
        write(unit, '(A)') "    OUTPUT qrc_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    COMMAND ${FORGE_RCC} resources/" // trim(project_name) // ".qrc -o qrc_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    DEPENDS resources/" // trim(project_name) // ".qrc"
        write(unit, '(A)') "    COMMENT ""Generating resource code"""
        write(unit, '(A)') ")"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Generate UI code"
        write(unit, '(A)') "add_custom_command("
        write(unit, '(A)') "    OUTPUT ui_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    COMMAND ${FORGE_UIC} resources/" // trim(project_name) // ".ui -o ui_" // trim(project_name) // ".f90"
        write(unit, '(A)') "    DEPENDS resources/" // trim(project_name) // ".ui"
        write(unit, '(A)') "    COMMENT ""Generating UI code"""
        write(unit, '(A)') ")"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Update translations"
        write(unit, '(A)') "add_custom_target(update_translations"
        write(unit, '(A)') "    COMMAND ${FORGE_LUPDATE} src -ts translations/" // trim(project_name) // "_en.ts"
        write(unit, '(A)') "    COMMENT ""Updating translation files"""
        write(unit, '(A)') ")"
        write(unit, '(A)') ""
        write(unit, '(A)') "# Release translations"
        write(unit, '(A)') "add_custom_command("
        write(unit, '(A)') "    OUTPUT " // trim(project_name) // "_en.qm"
        write(unit, '(A)') "    COMMAND ${FORGE_LRELEASE} translations/" // trim(project_name) // "_en.ts -qm " // trim(project_name) // "_en.qm"
        write(unit, '(A)') "    DEPENDS translations/" // trim(project_name) // "_en.ts"
        write(unit, '(A)') "    COMMENT ""Releasing translation files"""
        write(unit, '(A)') ")"

        if (with_tests) then
            write(unit, '(A)') ""
            write(unit, '(A)') "# Tests"
            write(unit, '(A)') "enable_testing()"
            write(unit, '(A)') "add_subdirectory(test)"
        end if

        if (with_examples) then
            write(unit, '(A)') ""
            write(unit, '(A)') "# Examples"
            write(unit, '(A)') "add_subdirectory(examples)"
        end if

        close(unit)

    end subroutine generate_cmake_lists

    subroutine generate_fpm_toml(project_name, project_dir, project_type, with_tests, with_examples)
        character(len=*), intent(in) :: project_name, project_dir, project_type
        logical, intent(in) :: with_tests, with_examples
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/fpm.toml"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create fpm.toml"
            return
        end if

        write(unit, '(A)') "name = """ // trim(project_name) // """"
        write(unit, '(A)') "version = ""0.1.0"""
        write(unit, '(A)') "license = ""MIT"""
        write(unit, '(A)') "author = ""Your Name"""
        write(unit, '(A)') "maintainer = ""your.email@example.com"""
        write(unit, '(A)') "copyright = ""2025 Your Name"""
        write(unit, '(A)') ""
        write(unit, '(A)') "[dependencies]"
        write(unit, '(A)') "forge = { git = ""https://github.com/your-org/fortran-forge.git"" }"
        write(unit, '(A)') ""
        write(unit, '(A)') "[build]"
        write(unit, '(A)') "auto-executables = true"
        write(unit, '(A)') "auto-tests = true"
        write(unit, '(A)') "auto-examples = true"
        write(unit, '(A)') ""
        write(unit, '(A)') "[fortran]"
        write(unit, '(A)') "implicit-typing = false"
        write(unit, '(A)') "implicit-external = false"
        write(unit, '(A)') ""
        write(unit, '(A)') "[prebuild]"
        write(unit, '(A)') "# Prebuild commands for ForGE tooling"
        write(unit, '(A)') "forge-moc = ""forge_moc src/" // trim(project_name) // ".f90 -o build/generated/moc_" // trim(project_name) // ".f90"""
        write(unit, '(A)') "forge-rcc = ""forge_rcc resources/" // trim(project_name) // ".qrc -o build/generated/qrc_" // trim(project_name) // ".f90"""
        write(unit, '(A)') "forge-uic = ""forge_uic resources/" // trim(project_name) // ".ui -o build/generated/ui_" // trim(project_name) // ".f90"""
        write(unit, '(A)') "forge-lrelease = ""forge_lrelease translations/" // trim(project_name) // "_en.ts -qm build/generated/" // trim(project_name) // "_en.qm"""

        close(unit)

    end subroutine generate_fpm_toml

    subroutine generate_main_app(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/src/" // trim(project_name) // ".f90"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create main application file"
            return
        end if

        write(unit, '(A)') "!> @brief " // trim(project_name) // " - A ForGE Qt Application"
        write(unit, '(A)') "!> @author Generated by ForGE Project Generator"
        write(unit, '(A)') "!> @date 2025"
        write(unit, '(A)') ""
        write(unit, '(A)') "program " // trim(project_name)
        write(unit, '(A)') "    use forge_qobject"
        write(unit, '(A)') "    use forge_widgets"
        write(unit, '(A)') "    use forge_app"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') ""
        write(unit, '(A)') "    type(forge_application) :: app"
        write(unit, '(A)') "    type(main_window) :: window"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Initialize application"
        write(unit, '(A)') "    call app%init()"
        write(unit, '(A)') "    call app%set_application_name(""" // trim(project_name) // """)"
        write(unit, '(A)') "    call app%set_application_version(""0.1.0"")"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Create main window"
        write(unit, '(A)') "    call window%init()"
        write(unit, '(A)') "    call window%set_window_title(""" // trim(project_name) // """)"
        write(unit, '(A)') "    call window%resize(800, 600)"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Show window"
        write(unit, '(A)') "    call window%show()"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Run application event loop"
        write(unit, '(A)') "    call app%exec()"
        write(unit, '(A)') ""
        write(unit, '(A)') "end program " // trim(project_name)
        write(unit, '(A)') ""
        write(unit, '(A)') "!> @brief Main window class"
        write(unit, '(A)') "type, extends(forge_mainwindow) :: main_window"
        write(unit, '(A)') "    ! Q_OBJECT  ! Enable meta-object features"
        write(unit, '(A)') "contains"
        write(unit, '(A)') "    procedure :: setup_ui"
        write(unit, '(A)') "end type main_window"
        write(unit, '(A)') ""
        write(unit, '(A)') "subroutine setup_ui(this)"
        write(unit, '(A)') "    class(main_window), intent(inout) :: this"
        write(unit, '(A)') "    ! Setup your UI here"
        write(unit, '(A)') "end subroutine setup_ui"

        close(unit)

    end subroutine generate_main_app

    subroutine generate_library_module(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/src/" // trim(project_name) // ".f90"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create library module file"
            return
        end if

        write(unit, '(A)') "!> @brief " // trim(project_name) // " - A ForGE Qt Library"
        write(unit, '(A)') "!> @author Generated by ForGE Project Generator"
        write(unit, '(A)') "!> @date 2025"
        write(unit, '(A)') ""
        write(unit, '(A)') "module " // trim(project_name)
        write(unit, '(A)') "    use forge_qobject"
        write(unit, '(A)') "    use forge_widgets"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Public API"
        write(unit, '(A)') "    public :: " // trim(project_name) // "_version"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Version information"
        write(unit, '(A)') "    character(len=*), parameter :: " // trim(project_name) // "_version = ""0.1.0"""
        write(unit, '(A)') ""
        write(unit, '(A)') "contains"
        write(unit, '(A)') ""
        write(unit, '(A)') "    !> @brief Get library version"
        write(unit, '(A)') "    function get_version() result(version)"
        write(unit, '(A)') "        character(len=:), allocatable :: version"
        write(unit, '(A)') "        version = " // trim(project_name) // "_version"
        write(unit, '(A)') "    end function get_version"
        write(unit, '(A)') ""
        write(unit, '(A)') "end module " // trim(project_name)

        close(unit)

    end subroutine generate_library_module

    subroutine generate_plugin_module(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/src/" // trim(project_name) // ".f90"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create plugin module file"
            return
        end if

        write(unit, '(A)') "!> @brief " // trim(project_name) // " - A ForGE Qt Plugin"
        write(unit, '(A)') "!> @author Generated by ForGE Project Generator"
        write(unit, '(A)') "!> @date 2025"
        write(unit, '(A)') ""
        write(unit, '(A)') "module " // trim(project_name)
        write(unit, '(A)') "    use forge_qobject"
        write(unit, '(A)') "    use forge_plugin_interface"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Plugin interface implementation"
        write(unit, '(A)') "    type, extends(forge_plugin) :: " // trim(project_name) // "_plugin"
        write(unit, '(A)') "    contains"
        write(unit, '(A)') "        procedure :: initialize => plugin_initialize"
        write(unit, '(A)') "        procedure :: shutdown => plugin_shutdown"
        write(unit, '(A)') "        procedure :: name => plugin_name"
        write(unit, '(A)') "        procedure :: version => plugin_version"
        write(unit, '(A)') "    end type " // trim(project_name) // "_plugin"
        write(unit, '(A)') ""
        write(unit, '(A)') "contains"
        write(unit, '(A)') ""
        write(unit, '(A)') "    subroutine plugin_initialize(this)"
        write(unit, '(A)') "        class(" // trim(project_name) // "_plugin), intent(inout) :: this"
        write(unit, '(A)') "        ! Plugin initialization code"
        write(unit, '(A)') "    end subroutine plugin_initialize"
        write(unit, '(A)') ""
        write(unit, '(A)') "    subroutine plugin_shutdown(this)"
        write(unit, '(A)') "        class(" // trim(project_name) // "_plugin), intent(inout) :: this"
        write(unit, '(A)') "        ! Plugin shutdown code"
        write(unit, '(A)') "    end subroutine plugin_shutdown"
        write(unit, '(A)') ""
        write(unit, '(A)') "    function plugin_name(this) result(name)"
        write(unit, '(A)') "        class(" // trim(project_name) // "_plugin), intent(in) :: this"
        write(unit, '(A)') "        character(len=:), allocatable :: name"
        write(unit, '(A)') "        name = """ // trim(project_name) // """"
        write(unit, '(A)') "    end function plugin_name"
        write(unit, '(A)') ""
        write(unit, '(A)') "    function plugin_version(this) result(version)"
        write(unit, '(A)') "        class(" // trim(project_name) // "_plugin), intent(in) :: this"
        write(unit, '(A)') "        character(len=:), allocatable :: version"
        write(unit, '(A)') "        version = ""0.1.0"""
        write(unit, '(A)') "    end function plugin_version"
        write(unit, '(A)') ""
        write(unit, '(A)') "end module " // trim(project_name)

        close(unit)

    end subroutine generate_plugin_module

    subroutine generate_resource_file(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/resources/" // trim(project_name) // ".qrc"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create resource file"
            return
        end if

        write(unit, '(A)') "<!DOCTYPE RCC>"
        write(unit, '(A)') "<RCC version=""1.0"">"
        write(unit, '(A)') "<qresource>"
        write(unit, '(A)') "    <!-- Add your resource files here -->"
        write(unit, '(A)') "    <!-- <file>images/icon.png</file> -->"
        write(unit, '(A)') "    <!-- <file>translations/" // trim(project_name) // "_en.qm</file> -->"
        write(unit, '(A)') "</qresource>"
        write(unit, '(A)') "</RCC>"

        close(unit)

    end subroutine generate_resource_file

    subroutine generate_translation_file(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/translations/" // trim(project_name) // "_en.ts"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create translation file"
            return
        end if

        write(unit, '(A)') "<?xml version=""1.0"" encoding=""utf-8""?>"
        write(unit, '(A)') "<!DOCTYPE TS>"
        write(unit, '(A)') "<TS version=""2.1"" language=""en"">"
        write(unit, '(A)') "    <context>"
        write(unit, '(A)') "        <name>" // trim(project_name) // "</name>"
        write(unit, '(A)') "        <!-- Translation messages will be added here by lupdate -->"
        write(unit, '(A)') "    </context>"
        write(unit, '(A)') "</TS>"

        close(unit)

    end subroutine generate_translation_file

    subroutine generate_readme(project_name, project_dir, project_type)
        character(len=*), intent(in) :: project_name, project_dir, project_type
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/README.md"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create README file"
            return
        end if

        write(unit, '(A)') "# " // trim(project_name)
        write(unit, '(A)') ""
        write(unit, '(A)') "A ForGE Qt " // trim(project_type) // " project."
        write(unit, '(A)') ""
        write(unit, '(A)') "## Building"
        write(unit, '(A)') ""
        write(unit, '(A)') "### With CMake"
        write(unit, '(A)') "```bash"
        write(unit, '(A)') "mkdir build && cd build"
        write(unit, '(A)') "cmake .."
        write(unit, '(A)') "make"
        write(unit, '(A)') "```"
        write(unit, '(A)') ""
        write(unit, '(A)') "### With FPM"
        write(unit, '(A)') "```bash"
        write(unit, '(A)') "fpm build"
        write(unit, '(A)') "```"
        write(unit, '(A)') ""
        write(unit, '(A)') "## ForGE Tooling"
        write(unit, '(A)') ""
        write(unit, '(A)') "This project uses ForGE Qt tooling:"
        write(unit, '(A)') ""
        write(unit, '(A)') "- `forge_moc`: Meta-Object Compiler for signal/slot introspection"
        write(unit, '(A)') "- `forge_rcc`: Resource Compiler for embedding resources"
        write(unit, '(A)') "- `forge_uic`: UI Compiler for generating widget code"
        write(unit, '(A)') "- `forge_lupdate`: Translation Update tool"
        write(unit, '(A)') "- `forge_lrelease`: Translation Release tool"
        write(unit, '(A)') ""
        write(unit, '(A)') "## Project Structure"
        write(unit, '(A)') ""
        write(unit, '(A)') "```"
        write(unit, '(A)') trim(project_name) // "/"
        write(unit, '(A)') "├── src/                 # Source files"
        write(unit, '(A)') "├── include/             # Header files"
        write(unit, '(A)') "├── resources/           # Resource files (.qrc, .ui)"
        write(unit, '(A)') "├── translations/        # Translation files (.ts, .qm)"
        write(unit, '(A)') "├── test/                # Test files"
        write(unit, '(A)') "├── examples/            # Example files"
        write(unit, '(A)') "├── docs/                # Documentation"
        write(unit, '(A)') "├── CMakeLists.txt       # CMake build configuration"
        write(unit, '(A)') "├── fpm.toml            # FPM build configuration"
        write(unit, '(A)') "└── README.md           # This file"
        write(unit, '(A)') "```"

        close(unit)

    end subroutine generate_readme

    subroutine generate_test_files(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/test/test_" // trim(project_name) // ".f90"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create test file"
            return
        end if

        write(unit, '(A)') "!> @brief Tests for " // trim(project_name)
        write(unit, '(A)') ""
        write(unit, '(A)') "program test_" // trim(project_name)
        write(unit, '(A)') "    use forge_test"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') ""
        write(unit, '(A)') "    type(test_suite) :: suite"
        write(unit, '(A)') ""
        write(unit, '(A)') "    call suite%init(""" // trim(project_name) // " tests"")"
        write(unit, '(A)') ""
        write(unit, '(A)') "    ! Add your tests here"
        write(unit, '(A)') "    ! call suite%add_test(test_function)"
        write(unit, '(A)') ""
        write(unit, '(A)') "    call suite%run()"
        write(unit, '(A)') ""
        write(unit, '(A)') "end program test_" // trim(project_name)

        close(unit)

    end subroutine generate_test_files

    subroutine generate_example_files(project_name, project_dir)
        character(len=*), intent(in) :: project_name, project_dir
        character(len=:), allocatable :: file_path
        integer :: unit, ios

        file_path = project_dir // "/examples/example_" // trim(project_name) // ".f90"

        open(newunit=unit, file=file_path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create example file"
            return
        end if

        write(unit, '(A)') "!> @brief Example usage of " // trim(project_name)
        write(unit, '(A)') ""
        write(unit, '(A)') "program example_" // trim(project_name)
        write(unit, '(A)') "    ! Add example code here"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') ""
        write(unit, '(A)') "    write(*,*) ""Example for " // trim(project_name) // """"
        write(unit, '(A)') ""
        write(unit, '(A)') "end program example_" // trim(project_name)

        close(unit)

    end subroutine generate_example_files

end program forge_project_generator