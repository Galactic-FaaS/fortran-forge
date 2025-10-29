!> @brief UI Compiler (uic) for ForGE Qt
!> @details Generates widget code from UI files (.ui)
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_uic
    use iso_c_binding
    use iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none

    character(len=:), allocatable :: input_file
    character(len=:), allocatable :: output_file
    character(len=:), allocatable :: class_name
    logical :: verbose = .false.
    logical :: generate_header = .false.
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
        call get_command_argument(i, input_file)
        if (input_file(1:1) == '-') then
            select case (input_file)
            case ('-o', '--output')
                i = i + 1
                if (i > argc) then
                    write(error_unit, *) "Error: Missing output file after ", trim(input_file)
                    stop 1
                end if
                call get_command_argument(i, output_file)
            case ('-h', '--header')
                generate_header = .true.
            case ('-v', '--verbose')
                verbose = .true.
            case ('--help')
                call print_usage()
                stop 0
            case default
                write(error_unit, *) "Error: Unknown option ", trim(input_file)
                call print_usage()
                stop 1
            end select
        else
            ! Input file
            if (.not. allocated(input_file)) then
                input_file = input_file
            end if
        end if
        i = i + 1
    end do

    if (.not. allocated(input_file)) then
        write(error_unit, *) "Error: No input file specified"
        call print_usage()
        stop 1
    end if

    ! Generate default output file if not specified
    if (.not. allocated(output_file)) then
        if (generate_header) then
            output_file = "ui_" // trim(input_file) // ".f90"
        else
            output_file = "ui_" // trim(input_file) // ".f90"
        end if
    end if

    ! Process the input file
    call process_ui_file(input_file, output_file, generate_header, verbose)

contains

    subroutine print_usage()
        write(output_unit, *) "ForGE UI Compiler (uic)"
        write(output_unit, *) "Usage: forge_uic [options] <input_file.ui>"
        write(output_unit, *) ""
        write(output_unit, *) "Options:"
        write(output_unit, *) "  -o, --output <file>    Output file (default: ui_<input>.f90)"
        write(output_unit, *) "  -h, --header           Generate header file"
        write(output_unit, *) "  -v, --verbose          Verbose output"
        write(output_unit, *) "  --help                 Show this help"
        write(output_unit, *) ""
        write(output_unit, *) "Generates Fortran widget code from Qt UI files."
    end subroutine print_usage

    subroutine process_ui_file(input_file, output_file, generate_header, verbose)
        character(len=*), intent(in) :: input_file, output_file
        logical, intent(in) :: generate_header, verbose
        integer :: input_unit, output_unit, ios
        character(len=1024) :: line
        character(len=:), allocatable :: content
        type(ui_widget), dimension(:), allocatable :: widgets
        integer :: num_widgets

        if (verbose) then
            write(output_unit, *) "Processing UI file: ", trim(input_file)
            write(output_unit, *) "Output file: ", trim(output_file)
        end if

        ! Open input file
        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot open input file ", trim(input_file)
            stop 1
        end if

        ! Read entire file
        content = ""
        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            content = content // trim(line) // new_line('A')
        end do
        close(input_unit)

        ! Parse UI file
        call parse_ui_content(content, widgets, num_widgets, verbose)

        ! Generate code
        if (generate_header) then
            call generate_header_file(widgets, num_widgets, output_file, verbose)
        else
            call generate_implementation_file(widgets, num_widgets, output_file, verbose)
        end if

        ! Cleanup
        if (allocated(widgets)) deallocate(widgets)

    end subroutine process_ui_file

    subroutine parse_ui_content(content, widgets, num_widgets, verbose)
        character(len=*), intent(in) :: content
        type(ui_widget), dimension(:), allocatable, intent(out) :: widgets
        integer, intent(out) :: num_widgets
        logical, intent(in) :: verbose
        integer :: pos, start_pos, end_pos
        character(len=256) :: widget_type, widget_name

        num_widgets = 0
        allocate(widgets(10))  ! Initial capacity

        pos = 1
        do
            ! Look for widget definitions
            start_pos = index(content(pos:), '<widget')
            if (start_pos == 0) exit
            start_pos = start_pos + pos - 1

            ! Extract widget class
            start_pos = index(content(start_pos:), 'class="') + start_pos
            if (start_pos > pos) then
                start_pos = start_pos + 7
                end_pos = index(content(start_pos:), '"') + start_pos - 1
                widget_type = content(start_pos:end_pos-1)

                ! Extract widget name
                start_pos = index(content(end_pos:), 'name="') + end_pos
                if (start_pos > end_pos) then
                    start_pos = start_pos + 6
                    end_pos = index(content(start_pos:), '"') + start_pos - 1
                    widget_name = content(start_pos:end_pos-1)

                    ! Add widget
                    call add_widget(widgets, num_widgets, widget_type, widget_name, verbose)
                end if
            end if

            pos = end_pos + 1
        end do

        if (verbose) then
            write(output_unit, *) "Found ", num_widgets, " widgets in UI file"
        end if

    end subroutine parse_ui_content

    subroutine add_widget(widgets, num_widgets, widget_type, widget_name, verbose)
        type(ui_widget), dimension(:), allocatable, intent(inout) :: widgets
        integer, intent(inout) :: num_widgets
        character(len=*), intent(in) :: widget_type, widget_name
        logical, intent(in) :: verbose
        type(ui_widget), dimension(:), allocatable :: temp

        ! Resize array if needed
        if (num_widgets >= size(widgets)) then
            allocate(temp(size(widgets) * 2))
            temp(1:size(widgets)) = widgets
            call move_alloc(temp, widgets)
        end if

        num_widgets = num_widgets + 1
        widgets(num_widgets)%widget_type = widget_type
        widgets(num_widgets)%widget_name = widget_name

        if (verbose) then
            write(output_unit, *) "Found widget: ", trim(widget_type), " named ", trim(widget_name)
        end if

    end subroutine add_widget

    subroutine generate_header_file(widgets, num_widgets, output_file, verbose)
        type(ui_widget), dimension(:), intent(in) :: widgets
        integer, intent(in) :: num_widgets
        character(len=*), intent(in) :: output_file
        logical, intent(in) :: verbose
        integer :: output_unit, ios, i

        ! Open output file
        open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create output file ", trim(output_file)
            stop 1
        end if

        ! Write header
        write(output_unit, '(A)') "! Generated by ForGE UI Compiler (header)"
        write(output_unit, '(A)') "! Do not edit this file manually"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "module ui_" // trim(output_file)
        write(output_unit, '(A)') "    use forge_widgets"
        write(output_unit, '(A)') "    implicit none"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    ! UI widget declarations"
        do i = 1, num_widgets
            call write_widget_declaration(output_unit, widgets(i))
        end do
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "contains"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    subroutine setup_ui(this)"
        write(output_unit, '(A)') "        class(*), intent(inout) :: this"
        write(output_unit, '(A)') "        ! Setup UI widgets and connections"
        do i = 1, num_widgets
            call write_widget_setup(output_unit, widgets(i))
        end do
        write(output_unit, '(A)') "    end subroutine setup_ui"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "end module ui_" // trim(output_file)

        close(output_unit)

        if (verbose) then
            write(output_unit, *) "Generated UI header file with ", num_widgets, " widgets"
        end if

    end subroutine generate_header_file

    subroutine generate_implementation_file(widgets, num_widgets, output_file, verbose)
        type(ui_widget), dimension(:), intent(in) :: widgets
        integer, intent(in) :: num_widgets
        character(len=*), intent(in) :: output_file
        logical, intent(in) :: verbose
        integer :: output_unit, ios, i

        ! Open output file
        open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create output file ", trim(output_file)
            stop 1
        end if

        ! Write implementation
        write(output_unit, '(A)') "! Generated by ForGE UI Compiler (implementation)"
        write(output_unit, '(A)') "! Do not edit this file manually"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "submodule (ui_" // trim(output_file) // ") ui_" // trim(output_file) // "_impl"
        write(output_unit, '(A)') "    implicit none"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "contains"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    module subroutine setup_ui(this)"
        write(output_unit, '(A)') "        class(*), intent(inout) :: this"
        write(output_unit, '(A)') "        ! Implementation of UI setup"
        do i = 1, num_widgets
            call write_widget_implementation(output_unit, widgets(i))
        end do
        write(output_unit, '(A)') "    end subroutine setup_ui"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "end submodule ui_" // trim(output_file) // "_impl"

        close(output_unit)

        if (verbose) then
            write(output_unit, *) "Generated UI implementation file with ", num_widgets, " widgets"
        end if

    end subroutine generate_implementation_file

    subroutine write_widget_declaration(unit, widget)
        integer, intent(in) :: unit
        type(ui_widget), intent(in) :: widget

        select case (trim(widget%widget_type))
        case ('QPushButton')
            write(unit, '(A)') "    type(forge_button) :: " // trim(widget%widget_name)
        case ('QLabel')
            write(unit, '(A)') "    type(forge_label) :: " // trim(widget%widget_name)
        case ('QLineEdit')
            write(unit, '(A)') "    type(forge_lineedit) :: " // trim(widget%widget_name)
        case ('QTextEdit')
            write(unit, '(A)') "    type(forge_textedit) :: " // trim(widget%widget_name)
        case ('QCheckBox')
            write(unit, '(A)') "    type(forge_checkbox) :: " // trim(widget%widget_name)
        case ('QRadioButton')
            write(unit, '(A)') "    type(forge_radiobutton) :: " // trim(widget%widget_name)
        case ('QComboBox')
            write(unit, '(A)') "    type(forge_combobox) :: " // trim(widget%widget_name)
        case ('QSpinBox')
            write(unit, '(A)') "    type(forge_spinbox) :: " // trim(widget%widget_name)
        case ('QSlider')
            write(unit, '(A)') "    type(forge_slider) :: " // trim(widget%widget_name)
        case ('QGroupBox')
            write(unit, '(A)') "    type(forge_groupbox) :: " // trim(widget%widget_name)
        case ('QTabWidget')
            write(unit, '(A)') "    type(forge_tabwidget) :: " // trim(widget%widget_name)
        case ('QTableWidget')
            write(unit, '(A)') "    type(forge_tablewidget) :: " // trim(widget%widget_name)
        case ('QTreeWidget')
            write(unit, '(A)') "    type(forge_treewidget) :: " // trim(widget%widget_name)
        case ('QListWidget')
            write(unit, '(A)') "    type(forge_listwidget) :: " // trim(widget%widget_name)
        case ('QScrollArea')
            write(unit, '(A)') "    type(forge_scrollarea) :: " // trim(widget%widget_name)
        case ('QMenuBar')
            write(unit, '(A)') "    type(forge_menubar) :: " // trim(widget%widget_name)
        case ('QStatusBar')
            write(unit, '(A)') "    type(forge_statusbar) :: " // trim(widget%widget_name)
        case default
            write(unit, '(A)') "    type(forge_widget) :: " // trim(widget%widget_name)
        end select

    end subroutine write_widget_declaration

    subroutine write_widget_setup(unit, widget)
        integer, intent(in) :: unit
        type(ui_widget), intent(in) :: widget

        write(unit, '(A)') "        ! Setup " // trim(widget%widget_name)
        write(unit, '(A)') "        call " // trim(widget%widget_name) // "%init()"
        write(unit, '(A)') "        call " // trim(widget%widget_name) // "%set_object_name('" // trim(widget%widget_name) // "')"

    end subroutine write_widget_setup

    subroutine write_widget_implementation(unit, widget)
        integer, intent(in) :: unit
        type(ui_widget), intent(in) :: widget

        write(unit, '(A)') "        ! Initialize " // trim(widget%widget_name)
        write(unit, '(A)') "        call " // trim(widget%widget_name) // "%init()"

    end subroutine write_widget_implementation

end program forge_uic

! UI widget type
type :: ui_widget
    character(len=:), allocatable :: widget_type
    character(len=:), allocatable :: widget_name
end type ui_widget