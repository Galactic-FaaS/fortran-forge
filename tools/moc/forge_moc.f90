!> @brief Meta-Object Compiler (moc) for ForGE Qt
!> @details Generates meta-object code for signal/slot introspection
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_moc
    use iso_c_binding
    use iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none

    character(len=:), allocatable :: input_file
    character(len=:), allocatable :: output_file
    character(len=:), allocatable :: class_name
    logical :: verbose = .false.
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
            case ('-v', '--verbose')
                verbose = .true.
            case ('-h', '--help')
                call print_usage()
                stop 0
            case default
                write(error_unit, *) "Error: Unknown option ", trim(input_file)
                call print_usage()
                stop 1
            end select
        else
            ! Input file
            if (.not. allocated(class_name)) then
                class_name = input_file
            end if
        end if
        i = i + 1
    end do

    if (.not. allocated(class_name)) then
        write(error_unit, *) "Error: No input file specified"
        call print_usage()
        stop 1
    end if

    ! Generate default output file if not specified
    if (.not. allocated(output_file)) then
        output_file = "moc_" // trim(class_name) // ".f90"
    end if

    ! Process the input file
    call process_file(class_name, output_file, verbose)

contains

    subroutine print_usage()
        write(output_unit, *) "ForGE Meta-Object Compiler (moc)"
        write(output_unit, *) "Usage: forge_moc [options] <input_file>"
        write(output_unit, *) ""
        write(output_unit, *) "Options:"
        write(output_unit, *) "  -o, --output <file>    Output file (default: moc_<input>.f90)"
        write(output_unit, *) "  -v, --verbose          Verbose output"
        write(output_unit, *) "  -h, --help             Show this help"
        write(output_unit, *) ""
        write(output_unit, *) "The input file should contain Fortran code with Q_OBJECT macro"
    end subroutine print_usage

    subroutine process_file(input_file, output_file, verbose)
        character(len=*), intent(in) :: input_file, output_file
        logical, intent(in) :: verbose
        integer :: input_unit, output_unit, ios
        character(len=1024) :: line
        character(len=:), allocatable :: content
        logical :: in_class, has_qobject
        integer :: line_num

        if (verbose) then
            write(output_unit, *) "Processing file: ", trim(input_file)
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
        line_num = 0
        in_class = .false.
        has_qobject = .false.

        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1

            ! Check for Q_OBJECT macro
            if (index(line, 'Q_OBJECT') > 0) then
                has_qobject = .true.
                if (verbose) then
                    write(output_unit, *) "Found Q_OBJECT at line ", line_num
                end if
            end if

            ! Check for type/class definition
            if (index(line, 'type') > 0 .and. index(line, 'extends') > 0) then
                in_class = .true.
            end if

            content = content // trim(line) // new_line('A')
        end do

        close(input_unit)

        if (.not. has_qobject) then
            if (verbose) then
                write(output_unit, *) "No Q_OBJECT macro found, skipping generation"
            end if
            return
        end if

        ! Generate meta-object code
        call generate_meta_code(content, output_file, verbose)

    end subroutine process_file

    subroutine generate_meta_code(content, output_file, verbose)
        character(len=*), intent(in) :: content, output_file
        logical, intent(in) :: verbose
        integer :: output_unit, ios
        character(len=:), allocatable :: class_name, signals, slots
        integer :: start_pos, end_pos

        ! Extract class name
        start_pos = index(content, 'type,')
        if (start_pos > 0) then
            start_pos = index(content(start_pos:), '::') + start_pos
            end_pos = index(content(start_pos:), new_line('A')) + start_pos - 1
            class_name = trim(adjustl(content(start_pos+2:end_pos-1)))
        else
            class_name = "UnknownClass"
        end if

        ! Extract signals and slots (simplified parsing)
        signals = extract_signals(content)
        slots = extract_slots(content)

        ! Open output file
        open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create output file ", trim(output_file)
            stop 1
        end if

        ! Write generated code
        write(output_unit, '(A)') "! Generated by ForGE Meta-Object Compiler"
        write(output_unit, '(A)') "! Do not edit this file manually"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "module moc_" // trim(class_name)
        write(output_unit, '(A)') "    use forge_qobject"
        write(output_unit, '(A)') "    implicit none"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    ! Meta-object data for " // trim(class_name)
        write(output_unit, '(A)') "    type(forge_meta_object), parameter :: meta_" // trim(class_name) // " = &"
        write(output_unit, '(A)') "        forge_meta_object(class_name='" // trim(class_name) // "', &"
        write(output_unit, '(A)') "                        super_class_name='QObject', &"
        write(output_unit, '(A)') "                        property_count=0, &"
        write(output_unit, '(A,I0,A)') "                        method_count=0, &"
        write(output_unit, '(A,I0,A)') "                        signal_count=", count_signals(signals), ", &"
        write(output_unit, '(A,I0,A)') "                        slot_count=", count_slots(slots), ", &"
        write(output_unit, '(A)') "                        property_names=[forge_string::], &"
        write(output_unit, '(A)') "                        method_names=[forge_string::], &"
        write(output_unit, '(A)') "                        signal_names=" // trim(signals) // ", &"
        write(output_unit, '(A)') "                        slot_names=" // trim(slots) // ")"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "contains"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    subroutine init_meta_" // trim(class_name) // "(obj)"
        write(output_unit, '(A)') "        class(forge_qobject), intent(inout) :: obj"
        write(output_unit, '(A)') "        obj%meta_object = meta_" // trim(class_name)
        write(output_unit, '(A)') "    end subroutine init_meta_" // trim(class_name)
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "end module moc_" // trim(class_name)

        close(output_unit)

        if (verbose) then
            write(output_unit, *) "Generated meta-object code for class ", trim(class_name)
        end if

    end subroutine generate_meta_code

    function extract_signals(content) result(signals)
        character(len=*), intent(in) :: content
        character(len=:), allocatable :: signals
        integer :: pos, start_pos, end_pos
        character(len=256) :: signal_name

        signals = "[forge_string::"
        pos = 1

        do
            start_pos = index(content(pos:), "signal_")
            if (start_pos == 0) exit
            start_pos = start_pos + pos - 1

            ! Find signal name
            end_pos = index(content(start_pos:), " ") + start_pos - 1
            if (end_pos == start_pos - 1) then
                end_pos = index(content(start_pos:), new_line('A')) + start_pos - 1
            end if

            signal_name = content(start_pos:end_pos-1)
            signals = signals // "forge_string('" // trim(signal_name) // "'), "

            pos = end_pos
        end do

        if (len(signals) > 15) then
            signals = signals(:len(signals)-2) // "]"
        else
            signals = signals // "]"
        end if
    end function extract_signals

    function extract_slots(content) result(slots)
        character(len=*), intent(in) :: content
        character(len=:), allocatable :: slots
        integer :: pos, start_pos, end_pos
        character(len=256) :: slot_name

        slots = "[forge_string::"
        pos = 1

        do
            start_pos = index(content(pos:), "slot_")
            if (start_pos == 0) exit
            start_pos = start_pos + pos - 1

            ! Find slot name
            end_pos = index(content(start_pos:), " ") + start_pos - 1
            if (end_pos == start_pos - 1) then
                end_pos = index(content(start_pos:), new_line('A')) + start_pos - 1
            end if

            slot_name = content(start_pos:end_pos-1)
            slots = slots // "forge_string('" // trim(slot_name) // "'), "

            pos = end_pos
        end do

        if (len(slots) > 15) then
            slots = slots(:len(slots)-2) // "]"
        else
            slots = slots // "]"
        end if
    end function extract_slots

    function count_signals(signals) result(count)
        character(len=*), intent(in) :: signals
        integer :: count, pos
        count = 0
        pos = 1
        do
            if (index(signals(pos:), "forge_string") > 0) then
                count = count + 1
                pos = index(signals(pos:), "forge_string") + pos + 10
            else
                exit
            end if
        end do
    end function count_signals

    function count_slots(slots) result(count)
        character(len=*), intent(in) :: slots
        integer :: count, pos
        count = 0
        pos = 1
        do
            if (index(slots(pos:), "forge_string") > 0) then
                count = count + 1
                pos = index(slots(pos:), "forge_string") + pos + 10
            else
                exit
            end if
        end do
    end function count_slots

end program forge_moc