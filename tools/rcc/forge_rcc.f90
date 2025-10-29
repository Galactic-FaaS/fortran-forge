!> @brief Resource Compiler (rcc) for ForGE Qt
!> @details Compiles Qt resource files (.qrc) into binary resources
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program forge_rcc
    use iso_c_binding
    use iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none

    character(len=:), allocatable :: input_file
    character(len=:), allocatable :: output_file
    character(len=:), allocatable :: name
    logical :: verbose = .false.
    logical :: compress = .true.
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
            case ('-name')
                i = i + 1
                if (i > argc) then
                    write(error_unit, *) "Error: Missing name after ", trim(input_file)
                    stop 1
                end if
                call get_command_argument(i, name)
            case ('-no-compress')
                compress = .false.
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
        output_file = "qrc_" // trim(input_file) // ".f90"
    end if

    ! Generate default name if not specified
    if (.not. allocated(name)) then
        name = input_file
        ! Remove .qrc extension
        if (len(name) > 4 .and. name(len(name)-3:) == '.qrc') then
            name = name(:len(name)-4)
        end if
    end if

    ! Process the input file
    call process_qrc_file(input_file, output_file, name, compress, verbose)

contains

    subroutine print_usage()
        write(output_unit, *) "ForGE Resource Compiler (rcc)"
        write(output_unit, *) "Usage: forge_rcc [options] <input_file.qrc>"
        write(output_unit, *) ""
        write(output_unit, *) "Options:"
        write(output_unit, *) "  -o, --output <file>    Output file (default: qrc_<input>.f90)"
        write(output_unit, *) "  -name <name>           Resource name (default: input filename without .qrc)"
        write(output_unit, *) "  -no-compress           Disable compression"
        write(output_unit, *) "  -v, --verbose          Verbose output"
        write(output_unit, *) "  -h, --help             Show this help"
        write(output_unit, *) ""
        write(output_unit, *) "Compiles Qt resource files into Fortran modules with embedded resources."
    end subroutine print_usage

    subroutine process_qrc_file(input_file, output_file, name, compress, verbose)
        character(len=*), intent(in) :: input_file, output_file, name
        logical, intent(in) :: compress, verbose
        integer :: input_unit, output_unit, ios
        character(len=1024) :: line
        character(len=:), allocatable :: content
        type(resource_entry), dimension(:), allocatable :: resources
        integer :: num_resources

        if (verbose) then
            write(output_unit, *) "Processing QRC file: ", trim(input_file)
            write(output_unit, *) "Output file: ", trim(output_file)
            write(output_unit, *) "Resource name: ", trim(name)
        end if

        ! Open input file
        open(newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot open input file ", trim(input_file)
            stop 1
        end if

        ! Parse QRC file
        call parse_qrc(input_unit, resources, num_resources, verbose)
        close(input_unit)

        ! Generate resource code
        call generate_resource_code(resources, num_resources, output_file, name, compress, verbose)

        ! Cleanup
        if (allocated(resources)) deallocate(resources)

    end subroutine process_qrc_file

    subroutine parse_qrc(unit, resources, num_resources, verbose)
        integer, intent(in) :: unit
        type(resource_entry), dimension(:), allocatable, intent(out) :: resources
        integer, intent(out) :: num_resources
        logical, intent(in) :: verbose
        character(len=1024) :: line
        integer :: ios, pos
        logical :: in_resources

        num_resources = 0
        allocate(resources(10))  ! Initial capacity
        in_resources = .false.

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = trim(adjustl(line))

            ! Skip comments and empty lines
            if (len(line) == 0 .or. line(1:1) == '<') continue

            ! Look for file entries
            pos = index(line, '<file')
            if (pos > 0) then
                call parse_file_entry(line, resources, num_resources, verbose)
            end if
        end do

        if (verbose) then
            write(output_unit, *) "Found ", num_resources, " resource files"
        end if

    end subroutine parse_qrc

    subroutine parse_file_entry(line, resources, num_resources, verbose)
        character(len=*), intent(in) :: line
        type(resource_entry), dimension(:), allocatable, intent(inout) :: resources
        integer, intent(inout) :: num_resources
        logical, intent(in) :: verbose
        integer :: start_pos, end_pos
        character(len=:), allocatable :: alias, file_path
        type(resource_entry), dimension(:), allocatable :: temp

        ! Extract file path
        start_pos = index(line, '>')
        end_pos = index(line, '</file>')
        if (start_pos > 0 .and. end_pos > start_pos) then
            file_path = trim(adjustl(line(start_pos+1:end_pos-1)))

            ! Check for alias
            start_pos = index(line, 'alias="')
            if (start_pos > 0) then
                start_pos = start_pos + 7
                end_pos = index(line(start_pos:), '"') + start_pos - 1
                alias = line(start_pos:end_pos-1)
            else
                alias = file_path
            end if

            ! Resize array if needed
            if (num_resources >= size(resources)) then
                allocate(temp(size(resources) * 2))
                temp(1:size(resources)) = resources
                call move_alloc(temp, resources)
            end if

            num_resources = num_resources + 1
            resources(num_resources)%file_path = file_path
            resources(num_resources)%alias = alias

            if (verbose) then
                write(output_unit, *) "Found resource: ", trim(file_path), " -> ", trim(alias)
            end if
        end if

    end subroutine parse_file_entry

    subroutine generate_resource_code(resources, num_resources, output_file, name, compress, verbose)
        type(resource_entry), dimension(:), intent(in) :: resources
        integer, intent(in) :: num_resources
        character(len=*), intent(in) :: output_file, name
        logical, intent(in) :: compress, verbose
        integer :: output_unit, ios, i
        character(len=32) :: hex_str

        ! Open output file
        open(newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Error: Cannot create output file ", trim(output_file)
            stop 1
        end if

        ! Write header
        write(output_unit, '(A)') "! Generated by ForGE Resource Compiler"
        write(output_unit, '(A)') "! Do not edit this file manually"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "module qrc_" // trim(name)
        write(output_unit, '(A)') "    use iso_c_binding"
        write(output_unit, '(A)') "    implicit none"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    ! Resource data"
        write(output_unit, '(A)') "    type :: qresource_data"
        write(output_unit, '(A)') "        character(len=:), allocatable :: name"
        write(output_unit, '(A)') "        integer :: size = 0"
        write(output_unit, '(A)') "        integer :: compressed_size = 0"
        write(output_unit, '(A)') "        integer(kind=c_int8_t), dimension(:), allocatable :: data"
        write(output_unit, '(A)') "        logical :: compressed = .false."
        write(output_unit, '(A)') "    end type qresource_data"
        write(output_unit, '(A)') ""

        ! Write resource data
        do i = 1, num_resources
            call write_resource_data(output_unit, resources(i), i, compress, verbose)
        end do

        ! Write resource registry
        write(output_unit, '(A)') "    ! Resource registry"
        write(output_unit, '(A,I0,A)') "    type(qresource_data), dimension(", num_resources, "), target :: qresources = [&"
        do i = 1, num_resources
            write(output_unit, '(A,I0,A)') "        qresource_", i, "_data"
            if (i < num_resources) write(output_unit, '(A)') ","
        end do
        write(output_unit, '(A)') "    ]"
        write(output_unit, '(A)') ""

        ! Write accessor functions
        write(output_unit, '(A)') "contains"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    function qresource_get(name) result(data)"
        write(output_unit, '(A)') "        character(len=*), intent(in) :: name"
        write(output_unit, '(A)') "        type(qresource_data), pointer :: data"
        write(output_unit, '(A)') "        integer :: i"
        write(output_unit, '(A)') "        do i = 1, size(qresources)"
        write(output_unit, '(A)') "            if (qresources(i)%name == name) then"
        write(output_unit, '(A)') "                data => qresources(i)"
        write(output_unit, '(A)') "                return"
        write(output_unit, '(A)') "            end if"
        write(output_unit, '(A)') "        end do"
        write(output_unit, '(A)') "        data => null()"
        write(output_unit, '(A)') "    end function qresource_get"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "    subroutine qresource_list()"
        write(output_unit, '(A)') "        integer :: i"
        write(output_unit, '(A)') "        write(*,*) 'Available resources:'"
        write(output_unit, '(A)') "        do i = 1, size(qresources)"
        write(output_unit, '(A)') "            write(*,*) '  ', trim(qresources(i)%name), ' (', qresources(i)%size, ' bytes)'"
        write(output_unit, '(A)') "        end do"
        write(output_unit, '(A)') "    end subroutine qresource_list"
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "end module qrc_" // trim(name)

        close(output_unit)

        if (verbose) then
            write(output_unit, *) "Generated resource module with ", num_resources, " resources"
        end if

    end subroutine generate_resource_code

    subroutine write_resource_data(unit, resource, index, compress, verbose)
        integer, intent(in) :: unit, index
        type(resource_entry), intent(in) :: resource
        logical, intent(in) :: compress, verbose
        integer :: file_unit, ios, file_size, i
        character(len=:), allocatable :: file_content
        integer(kind=c_int8_t), dimension(:), allocatable :: binary_data

        ! Read resource file
        open(newunit=file_unit, file=resource%file_path, status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) then
            write(error_unit, *) "Warning: Cannot open resource file ", trim(resource%file_path)
            return
        end if

        ! Get file size
        inquire(file_unit, size=file_size)

        ! Read file content
        allocate(binary_data(file_size))
        read(file_unit, iostat=ios) binary_data
        close(file_unit)

        if (ios /= 0) then
            write(error_unit, *) "Warning: Error reading resource file ", trim(resource%file_path)
            deallocate(binary_data)
            return
        end if

        ! Write resource data
        write(unit, '(A,I0,A)') "    type(qresource_data) :: qresource_", index, "_data = &"
        write(unit, '(A)') "        qresource_data(name='" // trim(resource%alias) // "', &"
        write(unit, '(A,I0,A)') "                     size=", file_size, ", &"
        write(unit, '(A,I0,A)') "                     compressed_size=", file_size, ", &"
        write(unit, '(A)') "                     compressed=.false., &"
        write(unit, '(A)') "                     data=[ &"

        ! Write binary data as hex
        do i = 1, file_size
            write(unit, '(A,Z2.2)', advance='no') "        int(z'", binary_data(i)
            if (i < file_size) then
                write(unit, '(A)') ","
            else
                write(unit, '(A)') " &"
            end if
        end do
        write(unit, '(A)') "                     ])"

        deallocate(binary_data)

        if (verbose) then
            write(output_unit, *) "Embedded resource: ", trim(resource%alias), " (", file_size, " bytes)"
        end if

    end subroutine write_resource_data

end program forge_rcc

! Resource entry type
type :: resource_entry
    character(len=:), allocatable :: file_path
    character(len=:), allocatable :: alias
end type resource_entry