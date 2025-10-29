!> @brief lrelease tool for compiling .ts files to .qm files
!> @details Command-line tool to convert Qt translation source files to binary format
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_lrelease
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_ts_parser, only: TSParser
    use forge_qm_format, only: QMWriter
    use forge_translator, only: QTranslator
    implicit none

    character(len=:), allocatable :: input_file, output_file
    logical :: verbose = .false.
    integer :: i, argc
    character(len=256) :: arg

    ! Parse command line arguments
    argc = command_argument_count()

    if (argc < 2) then
        call print_usage()
        stop
    end if

    i = 1
    do while (i <= argc)
        call get_command_argument(i, arg)

        select case (trim(arg))
        case ('-h', '--help')
            call print_usage()
            stop
        case ('-v', '--verbose')
            verbose = .true.
        case ('-o', '--output')
            i = i + 1
            if (i <= argc) then
                call get_command_argument(i, output_file)
            else
                write(*,*) "Error: Missing output file after -o"
                stop
            end if
        case default
            if (.not. allocated(input_file)) then
                input_file = trim(arg)
            else
                write(*,*) "Error: Multiple input files specified"
                stop
            end if
        end select

        i = i + 1
    end do

    if (.not. allocated(input_file)) then
        write(*,*) "Error: No input file specified"
        stop
    end if

    ! Generate default output file if not specified
    if (.not. allocated(output_file)) then
        output_file = change_extension(input_file, '.qm')
    end if

    ! Process the file
    call process_file(input_file, output_file, verbose)

contains

    !> @brief Process a single .ts file
    subroutine process_file(ts_file, qm_file, verbose)
        character(len=*), intent(in) :: ts_file, qm_file
        logical, intent(in) :: verbose
        type(TSParser) :: parser
        type(QMWriter) :: writer
        type(QTranslator) :: translator
        logical :: success

        if (verbose) then
            write(*,*) "Processing ", trim(ts_file), " -> ", trim(qm_file)
        end if

        ! Parse TS file
        success = parser%parse(translator, ts_file)
        if (.not. success) then
            write(*,*) "Error: Failed to parse ", trim(ts_file)
            return
        end if

        if (verbose) then
            write(*,*) "Parsed ", translator%count(), " messages"
        end if

        ! Write QM file
        success = writer%write(translator, qm_file)
        if (.not. success) then
            write(*,*) "Error: Failed to write ", trim(qm_file)
            return
        end if

        if (verbose) then
            write(*,*) "Successfully created ", trim(qm_file)
        end if
    end subroutine process_file

    !> @brief Change file extension
    function change_extension(filename, new_ext) result(new_filename)
        character(len=*), intent(in) :: filename, new_ext
        character(len=:), allocatable :: new_filename
        integer :: dot_pos

        dot_pos = scan(filename, '.', back=.true.)
        if (dot_pos > 0) then
            new_filename = filename(1:dot_pos-1) // new_ext
        else
            new_filename = filename // new_ext
        end if
    end function change_extension

    !> @brief Print usage information
    subroutine print_usage()
        write(*,*) "ForGE lrelease - Qt translation file compiler"
        write(*,*) ""
        write(*,*) "Usage: forge_lrelease [options] <input.ts>"
        write(*,*) ""
        write(*,*) "Options:"
        write(*,*) "  -o, --output <file>    Specify output .qm file"
        write(*,*) "  -v, --verbose          Verbose output"
        write(*,*) "  -h, --help             Show this help"
        write(*,*) ""
        write(*,*) "Examples:"
        write(*,*) "  forge_lrelease app_en.ts"
        write(*,*) "  forge_lrelease -o app.qm app_en.ts"
        write(*,*) "  forge_lrelease -v app_en.ts"
    end subroutine print_usage

end program forge_lrelease