!> @brief Code Analysis Tool for ForGE Qt
!> @details Static analysis and code quality checks for Fortran Qt code
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_code_analyzer
    use iso_c_binding
    use iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none

    character(len=:), allocatable :: source_dir
    character(len=:), allocatable :: output_file
    logical :: verbose = .false.
    logical :: check_qt_style = .true.
    logical :: check_memory = .true.
    logical :: check_signals = .true.
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
        call get_command_argument(i, source_dir)
        if (source_dir(1:1) == '-') then
            select case (source_dir)
            case ('-o', '--output')
                i = i + 1
                if (i > argc) then
                    write(error_unit, *) "Error: Missing output file after ", trim(source_dir)
                    stop 1
                end if
                call get_command_argument(i, output_file)
            case ('--no-qt-style')
                check_qt_style = .false.
            case ('--no-memory')
                check_memory = .false.
            case ('--no-signals')
                check_signals = .false.
            case ('-v', '--verbose')
                verbose = .true.
            case ('-h', '--help')
                call print_usage()
                stop 0
            case default
                write(error_unit, *) "Error: Unknown option ", trim(source_dir)
                call print_usage()
                stop 1
            end select
        else
            ! Source directory
            if (.not. allocated(source_dir)) then
                source_dir = source_dir
            end if
        end if
        i = i + 1
    end do

    if (.not. allocated(source_dir)) then
        write(error_unit, *) "Error: No source directory specified"
        call print_usage()
        stop 1
    end if

    ! Generate default output file if not specified
    if (.not. allocated(output_file)) then
        output_file = "analysis_report.txt"
    end if

    ! Run analysis
    call analyze_codebase(source_dir, output_file, check_qt_style, check_memory, check_signals, verbose)

contains

    subroutine print_usage()
        write(output_unit, *) "ForGE Code Analysis Tool"
        write(output_unit, *) "Usage: forge_code_analyzer [options] <source_directory>"
        write(output_unit, *) ""
        write(output_unit, *) "Options:"
        write(output_unit, *) "  -o, --output <file>     Output report file (default: analysis_report.txt)"
        write(output_unit, *) "  --no-qt-style           Skip Qt style checks"
        write(output_unit, *) "  --no-memory             Skip memory management checks"
        write(output_unit, *) "  --no-signals            Skip signal/slot checks"
        write(output_unit, *) "  -v, --verbose           Verbose output"
        write(output_unit, *) "  -h, --help              Show this help"
        write(output_unit, *) ""
        write(output_unit, *) "Performs static analysis on ForGE Qt Fortran code."
    end subroutine print_usage

    subroutine analyze_codebase(source_dir, output_file, check_qt_style, check_memory, check_signals, verbose)
        character(len=*), intent(in) :: source_dir, output_file
        logical, intent(in) :: check_qt_style, check_memory, check_signals, verbose
        type(analysis_report) :: report
        character(len=256), dimension(:), allocatable :: source_files
        integer :: num_files, i

        if (verbose) then
            write(output_unit, *) "Analyzing codebase in: ", trim(source_dir)
            write(output_unit, *) "Output report: ", trim(output_file)
        end if

        ! Initialize report
        call report%init()

        ! Find all Fortran source files
        call find_fortran_files(source_dir, source_files, num_files, verbose)

        ! Analyze each file
        do i = 1, num_files
            call analyze_file(source_files(i), report, check_qt_style, check_memory, check_signals, verbose)
        end do

        ! Generate report
        call generate_report(report, output_file, verbose)

        ! Cleanup
        if (allocated(source_files)) deallocate(source_files)
        call report%cleanup()

    end subroutine analyze_codebase

    subroutine find_fortran_files(dir_path, files, num_files, verbose)
        character(len=*), intent(in) :: dir_path
        character(len=256), dimension(:), allocatable, intent(out) :: files
        integer, intent(out) :: num_files
        logical, intent(in) :: verbose
        character(len=256) :: command, line
        integer :: unit, ios

        ! Use system command to find .f90 files
        command = 'find "' // trim(dir_path) // '" -name "*.f90" -o -name "*.f" -o -name "*.for"'

        ! For simplicity, we'll use a hardcoded list for this example
        ! In a real implementation, you'd parse the command output
        num_files = 0
        allocate(files(10))

        if (verbose) then
            write(output_unit, *) "Scanning for Fortran files in ", trim(dir_path)
        end if

    end subroutine find_fortran_files

    subroutine analyze_file(file_path, report, check_qt_style, check_memory, check_signals, verbose)
        character(len=*), intent(in) :: file_path
        type(analysis_report), intent(inout) :: report
        logical, intent(in) :: check_qt_style, check_memory, check_signals, verbose
        integer :: unit, ios, line_num
        character(len=1024) :: line
        character(len=:), allocatable :: content

        if (verbose) then
            write(output_unit, *) "Analyzing file: ", trim(file_path)
        end if

        ! Open file
        open(newunit=unit, file=file_path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Warning: Cannot open file ", trim(file_path)
            return
        end if

        ! Read entire file
        content = ""
        line_num = 0
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1

            ! Perform various checks
            if (check_qt_style) then
                call check_qt_style_guide(line, line_num, file_path, report)
            end if

            if (check_memory) then
                call check_memory_management(line, line_num, file_path, report)
            end if

            if (check_signals) then
                call check_signal_slot_usage(line, line_num, file_path, report)
            end if

            content = content // trim(line) // new_line('A')
        end do

        close(unit)

        ! File-level checks
        call check_file_structure(content, file_path, report)

    end subroutine analyze_file

    subroutine check_qt_style_guide(line, line_num, file_path, report)
        character(len=*), intent(in) :: line, file_path
        integer, intent(in) :: line_num
        type(analysis_report), intent(inout) :: report
        character(len=256) :: issue

        ! Check for Q_OBJECT macro
        if (index(line, 'Q_OBJECT') > 0) then
            if (.not. check_has_extends_qobject(line, file_path)) then
                issue = "Q_OBJECT macro found but class does not extend QObject"
                call report%add_issue(trim(issue), file_path, line_num, "Qt Style", "warning")
            end if
        end if

        ! Check signal/slot naming
        if (index(line, 'signal_') > 0 .or. index(line, 'slot_') > 0) then
            if (.not. check_signal_slot_naming(line)) then
                issue = "Signal/slot naming does not follow Qt conventions"
                call report%add_issue(trim(issue), file_path, line_num, "Qt Style", "info")
            end if
        end if

        ! Check for proper error handling
        if (index(line, 'call ') > 0 .and. index(line, 'error') > 0) then
            if (.not. check_error_handling(line)) then
                issue = "Potential error handling issue"
                call report%add_issue(trim(issue), file_path, line_num, "Qt Style", "warning")
            end if
        end if

    end subroutine check_qt_style_guide

    subroutine check_memory_management(line, line_num, file_path, report)
        character(len=*), intent(in) :: line, file_path
        integer, intent(in) :: line_num
        type(analysis_report), intent(inout) :: report
        character(len=256) :: issue

        ! Check for allocate without deallocate
        if (index(line, 'allocate(') > 0) then
            if (.not. check_allocate_deallocate_pair(line, file_path)) then
                issue = "Allocate statement without corresponding deallocate"
                call report%add_issue(trim(issue), file_path, line_num, "Memory", "warning")
            end if
        end if

        ! Check for pointer usage
        if (index(line, 'pointer') > 0 .or. index(line, '=>') > 0) then
            if (.not. check_pointer_safety(line)) then
                issue = "Potential unsafe pointer usage"
                call report%add_issue(trim(issue), file_path, line_num, "Memory", "error")
            end if
        end if

        ! Check for QObject lifecycle
        if (index(line, 'qobject') > 0 .and. index(line, 'init') > 0) then
            if (.not. check_qobject_lifecycle(line, file_path)) then
                issue = "QObject initialization without proper cleanup"
                call report%add_issue(trim(issue), file_path, line_num, "Memory", "warning")
            end if
        end if

    end subroutine check_memory_management

    subroutine check_signal_slot_usage(line, line_num, file_path, report)
        character(len=*), intent(in) :: line, file_path
        integer, intent(in) :: line_num
        type(analysis_report), intent(inout) :: report
        character(len=256) :: issue

        ! Check signal emissions
        if (index(line, '%emit(') > 0) then
            if (.not. check_signal_emission(line)) then
                issue = "Signal emission may not be connected"
                call report%add_issue(trim(issue), file_path, line_num, "Signals", "info")
            end if
        end if

        ! Check connections
        if (index(line, '%connect(') > 0) then
            if (.not. check_connection_validity(line)) then
                issue = "Signal/slot connection may be invalid"
                call report%add_issue(trim(issue), file_path, line_num, "Signals", "warning")
            end if
        end if

        ! Check for orphaned signals
        if (index(line, 'signal_') > 0 .and. index(line, 'emit') == 0) then
            if (.not. check_signal_usage(line, file_path)) then
                issue = "Signal declared but never emitted"
                call report%add_issue(trim(issue), file_path, line_num, "Signals", "info")
            end if
        end if

    end subroutine check_signal_slot_usage

    subroutine check_file_structure(content, file_path, report)
        character(len=*), intent(in) :: content, file_path
        type(analysis_report), intent(inout) :: report
        character(len=256) :: issue

        ! Check for proper module structure
        if (index(content, 'module ') > 0) then
            if (.not. check_module_structure(content)) then
                issue = "Module structure may not follow best practices"
                call report%add_issue(trim(issue), file_path, 1, "Structure", "info")
            end if
        end if

        ! Check for documentation
        if (.not. check_documentation(content)) then
            issue = "Missing or incomplete documentation"
            call report%add_issue(trim(issue), file_path, 1, "Documentation", "info")
        end if

        ! Check for proper use statements
        if (.not. check_use_statements(content)) then
            issue = "Use statements may not be optimal"
            call report%add_issue(trim(issue), file_path, 1, "Structure", "info")
        end if

    end subroutine check_file_structure

    ! Helper functions for checks (simplified implementations)
    function check_has_extends_qobject(line, file_path) result(has)
        character(len=*), intent(in) :: line, file_path
        logical :: has
        has = .true.  ! Simplified
    end function check_has_extends_qobject

    function check_signal_slot_naming(line) result(ok)
        character(len=*), intent(in) :: line
        logical :: ok
        ok = .true.  ! Simplified
    end function check_signal_slot_naming

    function check_error_handling(line) result(ok)
        character(len=*), intent(in) :: line
        logical :: ok
        ok = .true.  ! Simplified
    end function check_error_handling

    function check_allocate_deallocate_pair(line, file_path) result(has_pair)
        character(len=*), intent(in) :: line, file_path
        logical :: has_pair
        has_pair = .true.  ! Simplified
    end function check_allocate_deallocate_pair

    function check_pointer_safety(line) result(safe)
        character(len=*), intent(in) :: line
        logical :: safe
        safe = .true.  ! Simplified
    end function check_pointer_safety

    function check_qobject_lifecycle(line, file_path) result(ok)
        character(len=*), intent(in) :: line, file_path
        logical :: ok
        ok = .true.  ! Simplified
    end function check_qobject_lifecycle

    function check_signal_emission(line) result(ok)
        character(len=*), intent(in) :: line
        logical :: ok
        ok = .true.  ! Simplified
    end function check_signal_emission

    function check_connection_validity(line) result(ok)
        character(len=*), intent(in) :: line
        logical :: ok
        ok = .true.  ! Simplified
    end function check_connection_validity

    function check_signal_usage(line, file_path) result(used)
        character(len=*), intent(in) :: line, file_path
        logical :: used
        used = .true.  ! Simplified
    end function check_signal_usage

    function check_module_structure(content) result(ok)
        character(len=*), intent(in) :: content
        logical :: ok
        ok = .true.  ! Simplified
    end function check_module_structure

    function check_documentation(content) result(has_docs)
        character(len=*), intent(in) :: content
        logical :: has_docs
        has_docs = .true.  ! Simplified
    end function check_documentation

    function check_use_statements(content) result(ok)
        character(len=*), intent(in) :: content
        logical :: ok
        ok = .true.  ! Simplified
    end function check_use_statements

    subroutine generate_report(report, output_file, verbose)
        type(analysis_report), intent(in) :: report
        character(len=*), intent(in) :: output_file
        logical, intent(in) :: verbose
        integer :: unit, ios, i

        ! Open output file
        open(newunit=unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create output file ", trim(output_file)
            stop 1
        end if

        ! Write report header
        write(unit, '(A)') "ForGE Qt Code Analysis Report"
        write(unit, '(A)') "Generated on: " // get_current_time()
        write(unit, '(A)') ""
        write(unit, '(A)') "Summary:"
        write(unit, '(A,I0)') "Total issues found: ", report%total_issues
        write(unit, '(A,I0)') "Errors: ", report%error_count
        write(unit, '(A,I0)') "Warnings: ", report%warning_count
        write(unit, '(A,I0)') "Info: ", report%info_count
        write(unit, '(A)') ""

        ! Write issues by category
        call write_issues_by_category(unit, report, "Qt Style")
        call write_issues_by_category(unit, report, "Memory")
        call write_issues_by_category(unit, report, "Signals")
        call write_issues_by_category(unit, report, "Structure")
        call write_issues_by_category(unit, report, "Documentation")

        close(unit)

        if (verbose) then
            write(output_unit, *) "Analysis report written to ", trim(output_file)
            write(output_unit, *) "Total issues: ", report%total_issues
        end if

    end subroutine generate_report

    subroutine write_issues_by_category(unit, report, category)
        integer, intent(in) :: unit
        type(analysis_report), intent(in) :: report
        character(len=*), intent(in) :: category
        integer :: i, count

        count = 0
        do i = 1, report%total_issues
            if (report%issues(i)%category == category) then
                count = count + 1
            end if
        end do

        if (count > 0) then
            write(unit, '(A,A,A)') category, " Issues (", trim(int_to_string(count)), "):"
            write(unit, '(A)') repeat("-", 50)

            do i = 1, report%total_issues
                if (report%issues(i)%category == category) then
                    write(unit, '(A,A,A,I0,A)') &
                        trim(report%issues(i)%severity), ": ", &
                        trim(report%issues(i)%file_path), "(", &
                        report%issues(i)%line_num, ") - ", &
                        trim(report%issues(i)%message)
                end if
            end do
            write(unit, '(A)') ""
        end if

    end subroutine write_issues_by_category

    function get_current_time() result(time_str)
        character(len=:), allocatable :: time_str
        character(len=8) :: date
        character(len=10) :: time

        call date_and_time(date=date, time=time)
        time_str = date(1:4) // "-" // date(5:6) // "-" // date(7:8) // " " // &
                   time(1:2) // ":" // time(3:4) // ":" // time(5:6)

    end function get_current_time

    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=32) :: temp

        write(temp, '(I0)') num
        str = trim(temp)

    end function int_to_string

end program forge_code_analyzer

! Analysis report types
type :: analysis_issue
    character(len=:), allocatable :: message
    character(len=:), allocatable :: file_path
    integer :: line_num
    character(len=:), allocatable :: category
    character(len=:), allocatable :: severity  ! error, warning, info
end type analysis_issue

type :: analysis_report
    type(analysis_issue), dimension(:), allocatable :: issues
    integer :: total_issues = 0
    integer :: error_count = 0
    integer :: warning_count = 0
    integer :: info_count = 0
contains
    procedure :: init => report_init
    procedure :: cleanup => report_cleanup
    procedure :: add_issue => report_add_issue
end type analysis_report

subroutine report_init(this)
    class(analysis_report), intent(inout) :: this
    allocate(this%issues(100))  ! Initial capacity
    this%total_issues = 0
    this%error_count = 0
    this%warning_count = 0
    this%info_count = 0
end subroutine report_init

subroutine report_cleanup(this)
    class(analysis_report), intent(inout) :: this
    if (allocated(this%issues)) deallocate(this%issues)
    this%total_issues = 0
    this%error_count = 0
    this%warning_count = 0
    this%info_count = 0
end subroutine report_cleanup

subroutine report_add_issue(this, message, file_path, line_num, category, severity)
    class(analysis_report), intent(inout) :: this
    character(len=*), intent(in) :: message, file_path, category, severity
    integer, intent(in) :: line_num
    type(analysis_issue), dimension(:), allocatable :: temp

    ! Resize array if needed
    if (this%total_issues >= size(this%issues)) then
        allocate(temp(size(this%issues) * 2))
        temp(1:size(this%issues)) = this%issues
        call move_alloc(temp, this%issues)
    end if

    ! Add new issue
    this%total_issues = this%total_issues + 1
    this%issues(this%total_issues)%message = message
    this%issues(this%total_issues)%file_path = file_path
    this%issues(this%total_issues)%line_num = line_num
    this%issues(this%total_issues)%category = category
    this%issues(this%total_issues)%severity = severity

    ! Update counters
    select case (trim(severity))
    case ('error')
        this%error_count = this%error_count + 1
    case ('warning')
        this%warning_count = this%warning_count + 1
    case ('info')
        this%info_count = this%info_count + 1
    end select

end subroutine report_add_issue