!> @brief lupdate tool for extracting translatable strings from source code
!> @details Command-line tool to scan source files and generate .ts translation files
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_lupdate
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_ts_parser, only: TSGenerator
    use forge_translator, only: QTranslator, QTranslatorMessage
    implicit none

    character(len=:), allocatable :: output_file
    character(len=:), allocatable :: source_files(:)
    logical :: verbose = .false.
    integer :: i, argc, num_sources
    character(len=256) :: arg

    ! Parse command line arguments
    argc = command_argument_count()

    if (argc < 2) then
        call print_usage()
        stop
    end if

    allocate(character(len=256) :: source_files(argc))  ! Pre-allocate
    num_sources = 0

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
            num_sources = num_sources + 1
            source_files(num_sources) = trim(arg)
        end select

        i = i + 1
    end do

    if (num_sources == 0) then
        write(*,*) "Error: No source files specified"
        stop
    end if

    if (.not. allocated(output_file)) then
        output_file = "app.ts"  ! Default output
    end if

    ! Process the files
    call process_files(source_files(1:num_sources), output_file, verbose)

contains

    !> @brief Process source files and generate .ts file
    subroutine process_files(sources, ts_file, verbose)
        character(len=*), intent(in) :: sources(:), ts_file
        logical, intent(in) :: verbose
        type(QTranslator) :: translator
        type(TSGenerator) :: generator
        integer :: i

        if (verbose) then
            write(*,*) "Processing ", size(sources), " source files -> ", trim(ts_file)
        end if

        ! Scan each source file for translatable strings
        do i = 1, size(sources)
            call scan_source_file(sources(i), translator, verbose)
        end do

        if (verbose) then
            write(*,*) "Found ", translator%count(), " translatable strings"
        end if

        ! Generate TS file
        call generate_ts_file(translator, ts_file, generator, verbose)
    end subroutine process_files

    !> @brief Scan a source file for translatable strings
    subroutine scan_source_file(filename, translator, verbose)
        character(len=*), intent(in) :: filename
        type(QTranslator), intent(inout) :: translator
        logical, intent(in) :: verbose
        integer :: unit, iostat
        character(len=1000) :: line
        integer :: line_num

        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            if (verbose) write(*,*) "Warning: Cannot open ", trim(filename)
            return
        end if

        line_num = 0
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            line_num = line_num + 1
            call extract_translatable_strings(line, filename, line_num, translator)
        end do

        close(unit)

        if (verbose) then
            write(*,*) "Scanned ", trim(filename), ": ", line_num, " lines"
        end if
    end subroutine scan_source_file

    !> @brief Extract translatable strings from a line
    subroutine extract_translatable_strings(line, filename, line_num, translator)
        character(len=*), intent(in) :: line, filename
        integer, intent(in) :: line_num
        type(QTranslator), intent(inout) :: translator
        integer :: tr_pos, quote_start, quote_end
        character(len=:), allocatable :: source_text, context

        ! Look for tr( calls
        tr_pos = index(line, 'tr(')
        if (tr_pos == 0) tr_pos = index(line, 'tr (')  ! Handle spacing

        if (tr_pos > 0) then
            ! Find the quoted string argument
            quote_start = index(line(tr_pos:), '"')
            if (quote_start > 0) then
                quote_start = tr_pos + quote_start
                quote_end = index(line(quote_start+1:), '"')
                if (quote_end > 0) then
                    quote_end = quote_start + quote_end
                    source_text = line(quote_start+1:quote_end-1)

                    ! Extract context (simplified - assume "MainWindow")
                    context = "MainWindow"

                    ! Add to translator
                    call add_message(translator, context, source_text, filename, line_num)
                end if
            end if
        end if
    end subroutine extract_translatable_strings

    !> @brief Add a message to the translator
    subroutine add_message(translator, context, source_text, filename, line_num)
        type(QTranslator), intent(inout) :: translator
        character(len=*), intent(in) :: context, source_text, filename
        integer, intent(in) :: line_num
        type(QTranslatorMessage) :: message

        call message%set_context(context)
        call message%set_source_text(source_text)
        call message%set_file_name(filename)
        call message%set_line_number(line_num)

        call translator%insert(message)
    end subroutine add_message

    !> @brief Generate TS file from translator
    subroutine generate_ts_file(translator, ts_file, generator, verbose)
        type(QTranslator), intent(in) :: translator
        character(len=*), intent(in) :: ts_file
        type(TSGenerator), intent(inout) :: generator
        logical, intent(in) :: verbose
        logical :: success

        success = generator%generate(translator, ts_file, verbose)
        if (.not. success) then
            write(*,*) "Error: Failed to generate ", trim(ts_file)
        else if (verbose) then
            write(*,*) "Successfully created ", trim(ts_file)
        end if
    end subroutine generate_ts_file

    !> @brief Print usage information
    subroutine print_usage()
        write(*,*) "ForGE lupdate - Qt translation file updater"
        write(*,*) ""
        write(*,*) "Usage: forge_lupdate [options] <source files...>"
        write(*,*) ""
        write(*,*) "Options:"
        write(*,*) "  -o, --output <file>    Specify output .ts file (default: app.ts)"
        write(*,*) "  -v, --verbose          Verbose output"
        write(*,*) "  -h, --help             Show this help"
        write(*,*) ""
        write(*,*) "Examples:"
        write(*,*) "  forge_lupdate *.f90"
        write(*,*) "  forge_lupdate -o translations.ts src/*.f90"
        write(*,*) "  forge_lupdate -v -o app.ts main.f90 gui.f90"
    end subroutine print_usage

end program forge_lupdate