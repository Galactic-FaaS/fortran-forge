
!> @brief QString-equivalent string utilities
!> @details Advanced string manipulation with UTF-16 support and implicit sharing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_string_utils
    use iso_c_binding
    use iso_fortran_env, only: int32, int16, int8, int64, real32, real64
    implicit none
    private

    public :: QString, string_split, string_join, string_replace
    public :: string_trim, string_to_upper, string_to_lower
    public :: string_starts_with, string_ends_with, string_contains
    public :: string_to_int, string_to_real, int_to_string, real_to_string

    !> @brief UTF-16 code unit type (16-bit unsigned integer)
    integer, parameter :: UTF16_CODE_UNIT = selected_int_kind(4)  ! 16-bit

    !> @brief Shared data structure for implicit sharing
    type :: QStringData
        integer :: ref_count = 1  !< Reference count for copy-on-write
        integer :: length = 0     !< Number of UTF-16 code units
        integer :: capacity = 0   !< Allocated capacity
        integer(UTF16_CODE_UNIT), allocatable :: data(:)  !< UTF-16 code units
    end type QStringData

    !> @brief QString - Advanced string type with UTF-16 and implicit sharing
    type :: QString
        private
        type(QStringData), pointer :: d => null()  !< Shared data pointer
    contains
        ! Core methods
        procedure :: set => qstring_set
        procedure :: get => qstring_get
        procedure :: length => qstring_length
        procedure :: is_empty => qstring_is_empty
        procedure :: clear => qstring_clear
        procedure :: detach => qstring_detach  ! For copy-on-write

        ! Manipulation methods
        procedure :: append => qstring_append
        procedure :: prepend => qstring_prepend
        procedure :: insert => qstring_insert
        procedure :: remove => qstring_remove
        procedure :: replace => qstring_replace
        procedure :: mid => qstring_mid
        procedure :: left => qstring_left
        procedure :: right => qstring_right
        procedure :: chop => qstring_chop
        procedure :: truncate => qstring_truncate

        ! Case conversion
        procedure :: to_upper => qstring_to_upper
        procedure :: to_lower => qstring_to_lower
        procedure :: to_case_folded => qstring_to_case_folded

        ! Trimming and normalization
        procedure :: trim => qstring_trim
        procedure :: trimmed => qstring_trimmed
        procedure :: simplify => qstring_simplify
        procedure :: normalized => qstring_normalized

        ! Searching
        procedure :: index_of => qstring_index_of
        procedure :: last_index_of => qstring_last_index_of
        procedure :: contains => qstring_contains
        procedure :: starts_with => qstring_starts_with
        procedure :: ends_with => qstring_ends_with
        procedure :: count => qstring_count

        ! Comparison
        procedure :: equals => qstring_equals
        procedure :: compare => qstring_compare
        procedure :: equals_ignore_case => qstring_equals_ignore_case

        ! Substring extraction
        procedure :: substring => qstring_substring
        procedure :: section => qstring_section

        ! Splitting and joining
        procedure :: split => qstring_split
        procedure :: split_ref => qstring_split_ref

        ! Number conversion
        procedure :: to_int => qstring_to_int
        procedure :: to_real => qstring_to_real
        procedure :: to_long_long => qstring_to_long_long
        procedure :: to_double => qstring_to_double
        procedure :: to_float => qstring_to_float

        ! Formatting
        procedure :: arg => qstring_arg
        procedure :: sprintf => qstring_sprintf
        procedure :: vsprintf => qstring_vsprintf

        ! Validation and properties
        procedure :: is_null => qstring_is_null
        procedure :: is_simple_text => qstring_is_simple_text
        procedure :: is_right_to_left => qstring_is_right_to_left
        procedure :: is_valid_utf16 => qstring_is_valid_utf16

        ! Encoding conversion
        procedure :: to_utf8 => qstring_to_utf8
        procedure :: to_latin1 => qstring_to_latin1
        procedure :: to_ascii => qstring_to_ascii
        procedure :: from_utf8 => qstring_from_utf8
        procedure :: from_latin1 => qstring_from_latin1
        procedure :: from_ascii => qstring_from_ascii

        ! Advanced operations
        procedure :: fill => qstring_fill
        procedure :: repeat => qstring_repeat
        procedure :: reserve => qstring_reserve
        procedure :: squeeze => qstring_squeeze
        procedure :: capacity => qstring_capacity

        ! Regular expressions (basic implementation)
        procedure :: index_of_regex => qstring_index_of_regex
        procedure :: last_index_of_regex => qstring_last_index_of_regex
        procedure :: contains_regex => qstring_contains_regex
        procedure :: replace_regex => qstring_replace_regex
        procedure :: split_regex => qstring_split_regex

        ! Operators
        generic :: assignment(=) => assign_qstring, assign_char
        generic :: operator(+) => concat_qstring, concat_char
        generic :: operator(==) => equals_qstring, equals_char
        generic :: operator(/=) => not_equals_qstring, not_equals_char
        generic :: operator(<) => less_than_qstring, less_than_char
        generic :: operator(<=) => less_equal_qstring, less_equal_char
        generic :: operator(>) => greater_than_qstring, greater_than_char
        generic :: operator(>=) => greater_equal_qstring, greater_equal_char

        ! Private procedures for operators
        procedure, private :: assign_qstring
        procedure, private :: assign_char
        procedure, private :: concat_qstring
        procedure, private :: concat_char
        procedure, private :: equals_qstring
        procedure, private :: equals_char
        procedure, private :: not_equals_qstring
        procedure, private :: not_equals_char
        procedure, private :: less_than_qstring
        procedure, private :: less_than_char
        procedure, private :: less_equal_qstring
        procedure, private :: less_equal_char
        procedure, private :: greater_than_qstring
        procedure, private :: greater_than_char
        procedure, private :: greater_equal_qstring
        procedure, private :: greater_equal_char

        ! Cleanup
        final :: qstring_finalize
    end type QString

contains

    ! ========== QString Implementation ==========

    !> @brief Finalize QString (cleanup shared data)
    subroutine qstring_finalize(this)
        type(QString), intent(inout) :: this

        if (associated(this%d)) then
            this%d%ref_count = this%d%ref_count - 1
            if (this%d%ref_count <= 0) then
                if (allocated(this%d%data)) deallocate(this%d%data)
                deallocate(this%d)
            end if
            this%d => null()
        end if
    end subroutine qstring_finalize

    !> @brief Detach from shared data (copy-on-write)
    subroutine qstring_detach(this)
        class(QString), intent(inout) :: this
        type(QStringData), pointer :: new_data

        if (.not. associated(this%d)) then
            ! Create empty data
            allocate(this%d)
            this%d%ref_count = 1
            this%d%length = 0
            this%d%capacity = 0
            return
        end if

        if (this%d%ref_count > 1) then
            ! Copy data
            allocate(new_data)
            new_data%ref_count = 1
            new_data%length = this%d%length
            new_data%capacity = this%d%capacity
            if (allocated(this%d%data)) then
                allocate(new_data%data(size(this%d%data)))
                new_data%data = this%d%data
            end if

            ! Decrease ref count of old data
            this%d%ref_count = this%d%ref_count - 1
            this%d => new_data
        end if
    end subroutine qstring_detach

    !> @brief Set string value (UTF-8 input)
    subroutine qstring_set(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str

        call this%clear()
        call this%from_utf8(str)
    end subroutine qstring_set

    !> @brief Get string value (UTF-8 output)
    function qstring_get(this) result(str)
        class(QString), intent(in) :: this
        character(len=:), allocatable :: str

        str = this%to_utf8()
    end function qstring_get

    !> @brief Get string length in UTF-16 code units
    function qstring_length(this) result(len_val)
        class(QString), intent(in) :: this
        integer :: len_val

        if (associated(this%d)) then
            len_val = this%d%length
        else
            len_val = 0
        end if
    end function qstring_length

    !> @brief Check if string is empty
    function qstring_is_empty(this) result(empty)
        class(QString), intent(in) :: this
        logical :: empty

        empty = (this%length() == 0)
    end function qstring_is_empty

    !> @brief Check if string is null
    function qstring_is_null(this) result(null_val)
        class(QString), intent(in) :: this
        logical :: null_val

        null_val = (.not. associated(this%d))
    end function qstring_is_null

    !> @brief Clear string
    subroutine qstring_clear(this)
        class(QString), intent(inout) :: this

        call qstring_finalize(this)
    end subroutine qstring_clear

    !> @brief Append string
    subroutine qstring_append(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str
        type(QString) :: temp

        call this%append(str)
    end subroutine qstring_append

    !> @brief Append QString
    subroutine qstring_append_qstring(this, other)
        class(QString), intent(inout) :: this
        type(QString), intent(in) :: other
        integer :: new_length, new_capacity

        if (other%is_empty()) return

        call this%detach()

        new_length = this%d%length + other%d%length
        new_capacity = max(new_length, this%d%capacity * 2)

        if (new_capacity > this%d%capacity) then
            call this%reserve(new_capacity)
        end if

        if (allocated(other%d%data)) then
            this%d%data(this%d%length + 1:new_length) = other%d%data
        end if
        this%d%length = new_length
    end subroutine qstring_append_qstring

    !> @brief Prepend string
    subroutine qstring_prepend(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str
        type(QString) :: temp

        call this%prepend(str)
    end subroutine qstring_prepend

    !> @brief Prepend QString
    subroutine qstring_prepend_qstring(this, other)
        class(QString), intent(inout) :: this
        type(QString), intent(in) :: other
        integer :: new_length, new_capacity
        integer(UTF16_CODE_UNIT), allocatable :: temp_data(:)

        if (other%is_empty()) return

        call this%detach()

        new_length = this%d%length + other%d%length
        new_capacity = max(new_length, this%d%capacity * 2)

        if (new_capacity > this%d%capacity) then
            allocate(temp_data(new_capacity))
            if (allocated(this%d%data)) then
                temp_data(other%d%length + 1:new_length) = this%d%data(1:this%d%length)
            end if
            if (allocated(other%d%data)) then
                temp_data(1:other%d%length) = other%d%data
            end if
            call move_alloc(temp_data, this%d%data)
            this%d%capacity = new_capacity
        else
            ! Shift existing data
            this%d%data(other%d%length + 1:new_length) = this%d%data(1:this%d%length)
            this%d%data(1:other%d%length) = other%d%data
        end if
        this%d%length = new_length
    end subroutine qstring_prepend_qstring

    !> @brief Insert string at position
    subroutine qstring_insert(this, position, str)
        class(QString), intent(inout) :: this
        integer, intent(in) :: position
        character(len=*), intent(in) :: str
        type(QString) :: temp

        call this%insert(position, str)
    end subroutine qstring_insert

    !> @brief Insert QString at position
    subroutine qstring_insert_qstring(this, position, other)
        class(QString), intent(inout) :: this
        integer, intent(in) :: position
        type(QString), intent(in) :: other
        integer :: pos, new_length, new_capacity
        integer(UTF16_CODE_UNIT), allocatable :: temp_data(:)

        if (other%is_empty()) return

        call this%detach()

        pos = max(1, min(position, this%d%length + 1))
        new_length = this%d%length + other%d%length
        new_capacity = max(new_length, this%d%capacity * 2)

        allocate(temp_data(new_capacity))
        if (pos > 1 .and. allocated(this%d%data)) then
            temp_data(1:pos-1) = this%d%data(1:pos-1)
        end if
        if (allocated(other%d%data)) then
            temp_data(pos:pos+other%d%length-1) = other%d%data
        end if
        if (pos <= this%d%length .and. allocated(this%d%data)) then
            temp_data(pos+other%d%length:new_length) = this%d%data(pos:this%d%length)
        end if

        call move_alloc(temp_data, this%d%data)
        this%d%capacity = new_capacity
        this%d%length = new_length
    end subroutine qstring_insert_qstring

    !> @brief Remove characters from position
    subroutine qstring_remove(this, position, n)
        class(QString), intent(inout) :: this
        integer, intent(in) :: position
        integer, intent(in) :: n
        integer :: pos, len_remove, new_length

        if (n <= 0 .or. this%is_empty()) return

        call this%detach()

        pos = max(1, min(position, this%d%length))
        len_remove = min(n, this%d%length - pos + 1)
        new_length = this%d%length - len_remove

        if (pos <= new_length .and. allocated(this%d%data)) then
            this%d%data(pos:new_length) = this%d%data(pos+len_remove:this%d%length)
        end if
        this%d%length = new_length
    end subroutine qstring_remove

    !> @brief Replace substring
    subroutine qstring_replace(this, old_str, new_str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: old_str, new_str
        character(len=:), allocatable :: result

        result = string_replace(this%get(), old_str, new_str)
        call this%set(result)
    end subroutine qstring_replace

    !> @brief Extract substring from position with length
    function qstring_mid(this, position, n) result(substr)
        class(QString), intent(in) :: this
        integer, intent(in) :: position
        integer, intent(in), optional :: n
        type(QString) :: substr
        integer :: pos, len_sub

        pos = max(1, min(position, this%d%length + 1))
        if (present(n)) then
            len_sub = min(n, this%d%length - pos + 1)
        else
            len_sub = this%d%length - pos + 1
        end if

        if (len_sub > 0 .and. allocated(this%d%data)) then
            call substr%detach()
            allocate(substr%d)
            substr%d%ref_count = 1
            substr%d%length = len_sub
            substr%d%capacity = len_sub
            allocate(substr%d%data(len_sub))
            substr%d%data = this%d%data(pos:pos+len_sub-1)
        end if
    end function qstring_mid

    !> @brief Extract left part
    function qstring_left(this, n) result(substr)
        class(QString), intent(in) :: this
        integer, intent(in) :: n
        type(QString) :: substr

        substr = this%mid(1, n)
    end function qstring_left

    !> @brief Extract right part
    function qstring_right(this, n) result(substr)
        class(QString), intent(in) :: this
        integer, intent(in) :: n
        type(QString) :: substr
        integer :: start

        start = max(1, this%d%length - n + 1)
        substr = this%mid(start, n)
    end function qstring_right

    !> @brief Remove n characters from end
    subroutine qstring_chop(this, n)
        class(QString), intent(inout) :: this
        integer, intent(in) :: n

        if (n > 0) then
            call this%truncate(this%d%length - n)
        end if
    end subroutine qstring_chop

    !> @brief Truncate to length
    subroutine qstring_truncate(this, length)
        class(QString), intent(inout) :: this
        integer, intent(in) :: length

        if (length < 0) then
            call this%clear()
        else if (length < this%d%length) then
            call this%detach()
            this%d%length = length
        end if
    end subroutine qstring_truncate

    !> @brief Convert to uppercase
    subroutine qstring_to_upper(this)
        class(QString), intent(inout) :: this

        call this%detach()
        if (allocated(this%d%data)) then
            ! Simple ASCII conversion (full Unicode would be more complex)
            where (this%d%data >= iachar('a') .and. this%d%data <= iachar('z'))
                this%d%data = this%d%data - 32
            end where
        end if
    end subroutine qstring_to_upper

    !> @brief Convert to lowercase
    subroutine qstring_to_lower(this)
        class(QString), intent(inout) :: this

        call this%detach()
        if (allocated(this%d%data)) then
            ! Simple ASCII conversion
            where (this%d%data >= iachar('A') .and. this%d%data <= iachar('Z'))
                this%d%data = this%d%data + 32
            end where
        end if
    end subroutine qstring_to_lower

    !> @brief Case-folded conversion (simplified)
    subroutine qstring_to_case_folded(this)
        class(QString), intent(inout) :: this

        call this%to_lower()
    end subroutine qstring_to_case_folded

    !> @brief Trim whitespace (modify in place)
    subroutine qstring_trim(this)
        class(QString), intent(inout) :: this
        character(len=:), allocatable :: trimmed

        trimmed = string_trim(this%get())
        call this%set(trimmed)
    end subroutine qstring_trim

    !> @brief Return trimmed copy
    function qstring_trimmed(this) result(trimmed_str)
        class(QString), intent(in) :: this
        type(QString) :: trimmed_str
        character(len=:), allocatable :: temp

        temp = string_trim(this%get())
        call trimmed_str%set(temp)
    end function qstring_trimmed

    !> @brief Simplify whitespace
    subroutine qstring_simplify(this)
        class(QString), intent(inout) :: this
        character(len=:), allocatable :: result
        integer :: i, j, len_str
        logical :: in_space

        result = this%get()
        len_str = len(result)
        j = 1
        in_space = .false.

        do i = 1, len_str
            if (result(i:i) == ' ') then
                if (.not. in_space) then
                    result(j:j) = ' '
                    j = j + 1
                    in_space = .true.
                end if
            else
                result(j:j) = result(i:i)
                j = j + 1
                in_space = .false.
            end if
        end do

        call this%set(result(1:j-1))
    end subroutine qstring_simplify

    !> @brief Return normalized copy
    function qstring_normalized(this) result(normalized_str)
        class(QString), intent(in) :: this
        type(QString) :: normalized_str

        normalized_str = this
        call normalized_str%simplify()
    end function qstring_normalized

    !> @brief Find index of substring
    function qstring_index_of(this, str, from) result(pos)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        integer, intent(in), optional :: from
        integer :: pos
        character(len=:), allocatable :: this_str, search_str
        integer :: start

        this_str = this%get()
        search_str = str
        start = 1
        if (present(from)) start = max(1, from)

        pos = index(this_str(start:), search_str)
        if (pos > 0) pos = pos + start - 1
    end function qstring_index_of

    !> @brief Find last index of substring
    function qstring_last_index_of(this, str, from) result(pos)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        integer, intent(in), optional :: from
        integer :: pos
        character(len=:), allocatable :: this_str, search_str
        integer :: i, start

        this_str = this%get()
        search_str = str
        start = len(this_str)
        if (present(from)) start = min(start, max(1, from))

        pos = 0
        do i = start, 1, -1
            if (this_str(i:i+len(search_str)-1) == search_str) then
                pos = i
                exit
            end if
        end do
    end function qstring_last_index_of

    !> @brief Check if contains substring
    function qstring_contains(this, substring) result(found)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: substring
        logical :: found

        found = (this%index_of(substring) > 0)
    end function qstring_contains

    !> @brief Check if starts with prefix
    function qstring_starts_with(this, prefix) result(starts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: prefix
        logical :: starts

        starts = string_starts_with(this%get(), prefix)
    end function qstring_starts_with

    !> @brief Check if ends with suffix
    function qstring_ends_with(this, suffix) result(ends)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: suffix
        logical :: ends

        ends = string_ends_with(this%get(), suffix)
    end function qstring_ends_with

    !> @brief Count occurrences of substring
    function qstring_count(this, str) result(count_val)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        integer :: count_val
        integer :: pos, start

        count_val = 0
        start = 1
        do
            pos = this%index_of(str, start)
            if (pos == 0) exit
            count_val = count_val + 1
            start = pos + len(str)
        end do
    end function qstring_count

    !> @brief Compare with another string
    function qstring_compare(this, other) result(cmp)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: other
        integer :: cmp
        character(len=:), allocatable :: this_str

        this_str = this%get()
        if (this_str < other) then
            cmp = -1
        else if (this_str > other) then
            cmp = 1
        else
            cmp = 0
        end if
    end function qstring_compare

    !> @brief Check case-insensitive equality
    function qstring_equals_ignore_case(this, other) result(equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: other
        logical :: equal
        type(QString) :: this_upper, other_upper

        this_upper = this
        call this_upper%to_upper()
        call other_upper%set(other)
        call other_upper%to_upper()
        equal = (this_upper%get() == other_upper%get())
    end function qstring_equals_ignore_case

    !> @brief Extract substring (alias for mid)
    function qstring_substring(this, start_pos, length) result(substr)
        class(QString), intent(in) :: this
        integer, intent(in) :: start_pos
        integer, intent(in), optional :: length
        type(QString) :: substr

        substr = this%mid(start_pos, length)
    end function qstring_substring

    !> @brief Extract section between separators
    function qstring_section(this, sep, start, end) result(section_str)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: sep
        integer, intent(in) :: start
        integer, intent(in), optional :: end
        type(QString) :: section_str
        type(QString), allocatable :: parts(:)
        integer :: end_idx

        parts = this%split(sep)
        end_idx = size(parts)
        if (present(end)) end_idx = min(end, size(parts))

        if (start >= 1 .and. start <= size(parts) .and. end_idx >= start) then
            section_str = parts(start)
            if (end_idx > start) then
                integer :: i
                do i = start + 1, end_idx
                    call section_str%append(sep)
                    call section_str%append(parts(i)%get())
                end do
            end if
        end if
    end function qstring_section

    !> @brief Split string
    function qstring_split(this, delimiter) result(parts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: delimiter
        type(QString), allocatable :: parts(:)
        character(len=:), allocatable :: str_parts(:)
        integer :: i

        str_parts = string_split(this%get(), delimiter)
        allocate(parts(size(str_parts)))
        do i = 1, size(str_parts)
            call parts(i)%set(str_parts(i))
        end do
    end function qstring_split

    !> @brief Split with reference (Qt-style)
    function qstring_split_ref(this, delimiter) result(parts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: delimiter
        type(QString), allocatable :: parts(:)

        parts = this%split(delimiter)
    end function qstring_split_ref

    !> @brief Convert to integer
    function qstring_to_int(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        integer(int32) :: value

        value = string_to_int(this%get(), success)
    end function qstring_to_int

    !> @brief Convert to real
    function qstring_to_real(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        real :: value

        value = string_to_real(this%get(), success)
    end function qstring_to_real

    !> @brief Convert to long long (64-bit int)
    function qstring_to_long_long(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        integer(int64) :: value
        integer :: iostat

        character(len=:), allocatable :: str_val
        str_val = this%get()
        read(str_val, *, iostat=iostat) value
        if (present(success)) success = (iostat == 0)
    end function qstring_to_long_long

    !> @brief Convert to double
    function qstring_to_double(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        real(real64) :: value
        integer :: iostat

        character(len=:), allocatable :: str_val
        str_val = this%get()
        read(str_val, *, iostat=iostat) value
        if (present(success)) success = (iostat == 0)
    end function qstring_to_double

    !> @brief Convert to float
    function qstring_to_float(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        real(real32) :: value
        integer :: iostat

        character(len=:), allocatable :: str_val
        str_val = this%get()
        read(str_val, *, iostat=iostat) value
        if (present(success)) success = (iostat == 0)
    end function qstring_to_float

    !> @brief String formatting with arguments (simplified)
    function qstring_arg(this, arg1, arg2, arg3, arg4, arg5) result(formatted)
        class(QString), intent(in) :: this
        class(*), intent(in) :: arg1
        class(*), intent(in), optional :: arg2, arg3, arg4, arg5
        type(QString) :: formatted
        character(len=:), allocatable :: fmt_str
        integer :: pos

        formatted = this
        fmt_str = this%get()

        ! Simple %1, %2, etc. replacement
        pos = index(fmt_str, '%1')
        if (pos > 0) then
            call formatted%replace('%1', to_string(arg1))
        end if

        if (present(arg2)) then
            pos = index(fmt_str, '%2')
            if (pos > 0) then
                call formatted%replace('%2', to_string(arg2))
            end if
        end if

        if (present(arg3)) then
            pos = index(fmt_str, '%3')
            if (pos > 0) then
                call formatted%replace('%3', to_string(arg3))
            end if
        end if

        if (present(arg4)) then
            pos = index(fmt_str, '%4')
            if (pos > 0) then
                call formatted%replace('%4', to_string(arg4))
            end if
        end if

        if (present(arg5)) then
            pos = index(fmt_str, '%5')
            if (pos > 0) then
                call formatted%replace('%5', to_string(arg5))
            end if
        end if
    end function qstring_arg

    !> @brief sprintf-style formatting
    function qstring_sprintf(this, format_str) result(formatted)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: format_str
        type(QString) :: formatted

        ! Simplified implementation - just return format string
        call formatted%set(format_str)
    end function qstring_sprintf

    !> @brief vsprintf-style formatting
    function qstring_vsprintf(this, format_str, args) result(formatted)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: format_str
        class(*), intent(in) :: args(:)
        type(QString) :: formatted

        ! Simplified implementation
        call formatted%set(format_str)
    end function qstring_vsprintf

    !> @brief Check if simple text (ASCII only)
    function qstring_is_simple_text(this) result(simple)
        class(QString), intent(in) :: this
        logical :: simple
        integer :: i

        simple = .true.
        if (associated(this%d) .and. allocated(this%d%data)) then
            do i = 1, this%d%length
                if (this%d%data(i) > 127) then
                    simple = .false.
                    exit
                end if
            end do
        end if
    end function qstring_is_simple_text

    !> @brief Check if right-to-left text (simplified)
    function qstring_is_right_to_left(this) result(rtl)
        class(QString), intent(in) :: this
        logical :: rtl

        ! Simplified - check for Arabic/Hebrew characters
        rtl = .false.
        if (associated(this%d) .and. allocated(this%d%data)) then
            ! Arabic: U+0600-U+06FF, Hebrew: U+0590-U+05FF
            if (any(this%d%data >= int(z'0600') .and. this%d%data <= int(z'06FF')) .or. &
                any(this%d%data >= int(z'0590') .and. this%d%data <= int(z'05FF'))) then
                rtl = .true.
            end if
        end if
    end function qstring_is_right_to_left

    !> @brief Check if valid UTF-16
    function qstring_is_valid_utf16(this) result(valid)
        class(QString), intent(in) :: this
        logical :: valid

        ! Simplified validation
        valid = .true.
        if (associated(this%d) .and. allocated(this%d%data)) then
            ! Check for invalid surrogate pairs, etc.
            ! For now, just check basic range
            valid = all(this%d%data >= 0 .and. this%d%data <= int(z'10FFFF'))
        end if
    end function qstring_is_valid_utf16

    !> @brief Convert to UTF-8
    function qstring_to_utf8(this) result(utf8_str)
        class(QString), intent(in) :: this
        character(len=:), allocatable :: utf8_str

        if (.not. associated(this%d) .or. this%d%length == 0) then
            utf8_str = ""
            return
        end if

        ! Simplified UTF-16 to UTF-8 conversion (ASCII only for now)
        allocate(character(len=this%d%length) :: utf8_str)
        integer :: i
        do i = 1, this%d%length
            if (this%d%data(i) <= 127) then
                utf8_str(i:i) = char(this%d%data(i))
            else
                utf8_str(i:i) = '?'  ! Placeholder for non-ASCII
            end if
        end do
    end function qstring_to_utf8

    !> @brief Convert to Latin-1
    function qstring_to_latin1(this) result(latin1_str)
        class(QString), intent(in) :: this
        character(len=:), allocatable :: latin1_str

        latin1_str = this%to_utf8()  ! Simplified
    end function qstring_to_latin1

    !> @brief Convert to ASCII
    function qstring_to_ascii(this) result(ascii_str)
        class(QString), intent(in) :: this
        character(len=:), allocatable :: ascii_str

        ascii_str = this%to_utf8()  ! Simplified
    end function qstring_to_ascii

    !> @brief Create from UTF-8
    subroutine qstring_from_utf8(this, utf8_str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: utf8_str

        call this%clear()
        call this%detach()

        this%d%length = len(utf8_str)
        this%d%capacity = this%d%length
        allocate(this%d%data(this%d%length))

        ! Simplified UTF-8 to UTF-16 conversion (ASCII only)
        integer :: i
        do i = 1, len(utf8_str)
            this%d%data(i) = iachar(utf8_str(i:i))
        end do
    end subroutine qstring_from_utf8

    !> @brief Create from Latin-1
    subroutine qstring_from_latin1(this, latin1_str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: latin1_str

        call this%from_utf8(latin1_str)  ! Simplified
    end subroutine qstring_from_latin1

    !> @brief Create from ASCII
    subroutine qstring_from_ascii(this, ascii_str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: ascii_str

        call this%from_utf8(ascii_str)  ! Simplified
    end subroutine qstring_from_ascii

    !> @brief Fill with character
    subroutine qstring_fill(this, ch, size)
        class(QString), intent(inout) :: this
        character(len=1), intent(in) :: ch
        integer, intent(in) :: size

        call this%clear()
        call this%detach()

        if (size > 0) then
            this%d%length = size
            this%d%capacity = size
            allocate(this%d%data(size))
            this%d%data = iachar(ch)
        end if
    end subroutine qstring_fill

    !> @brief Repeat string
    function qstring_repeat(this, times) result(repeated)
        class(QString), intent(in) :: this
        integer, intent(in) :: times
        type(QString) :: repeated
        integer :: i

        do i = 1, times
            call repeated%append(this%get())
        end do
    end function qstring_repeat

    !> @brief Reserve capacity
    subroutine qstring_reserve(this, size)
        class(QString), intent(inout) :: this
        integer, intent(in) :: size
        integer(UTF16_CODE_UNIT), allocatable :: temp_data(:)

        if (size <= this%d%capacity) return

        call this%detach()

        allocate(temp_data(size))
        if (allocated(this%d%data)) then
            temp_data(1:this%d%length) = this%d%data(1:this%d%length)
        end if
        call move_alloc(temp_data, this%d%data)
        this%d%capacity = size
    end subroutine qstring_reserve

    !> @brief Squeeze capacity to fit
    subroutine qstring_squeeze(this)
        class(QString), intent(inout) :: this
        integer(UTF16_CODE_UNIT), allocatable :: temp_data(:)

        if (this%d%capacity <= this%d%length) return

        call this%detach()

        if (this%d%length > 0) then
            allocate(temp_data(this%d%length))
            temp_data = this%d%data(1:this%d%length)
            call move_alloc(temp_data, this%d%data)
        else
            if (allocated(this%d%data)) deallocate(this%d%data)
        end if
        this%d%capacity = this%d%length
    end subroutine qstring_squeeze

    !> @brief Get capacity
    function qstring_capacity(this) result(cap)
        class(QString), intent(in) :: this
        integer :: cap

        if (associated(this%d)) then
            cap = this%d%capacity
        else
            cap = 0
        end if
    end function qstring_capacity

    !> @brief Regex index of (simplified - no actual regex)
    function qstring_index_of_regex(this, pattern, from) result(pos)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: pattern
        integer, intent(in), optional :: from
        integer :: pos

        ! Simplified - treat as literal string
        pos = this%index_of(pattern, from)
    end function qstring_index_of_regex

    !> @brief Regex last index of (simplified)
    function qstring_last_index_of_regex(this, pattern, from) result(pos)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: pattern
        integer, intent(in), optional :: from
        integer :: pos

        pos = this%last_index_of(pattern, from)
    end function qstring_last_index_of_regex

    !> @brief Regex contains (simplified)
    function qstring_contains_regex(this, pattern) result(found)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: pattern
        logical :: found

        found = this%contains(pattern)
    end function qstring_contains_regex

    !> @brief Regex replace (simplified)
    subroutine qstring_replace_regex(this, pattern, replacement)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: pattern, replacement

        call this%replace(pattern, replacement)
    end subroutine qstring_replace_regex

    !> @brief Regex split (simplified)
    function qstring_split_regex(this, pattern) result(parts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: pattern
        type(QString), allocatable :: parts(:)

        parts = this%split(pattern)
    end function qstring_split_regex

    ! ========== Operator Implementations ==========

    !> @brief Assign QString
    subroutine assign_qstring(this, other)
        class(QString), intent(inout) :: this
        type(QString), intent(in) :: other

        call this%clear()
        if (associated(other%d)) then
            this%d => other%d
            this%d%ref_count = this%d%ref_count + 1
        end if
    end subroutine assign_qstring

    !> @brief Assign character string
    subroutine assign_char(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str

        call this%set(str)
    end subroutine assign_char

    !> @brief Concatenate with QString
    function concat_qstring(this, other) result(result_str)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        type(QString) :: result_str

        result_str = this
        call result_str%append(other%get())
    end function concat_qstring

    !> @brief Concatenate with character string
    function concat_char(this, str) result(result_str)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        type(QString) :: result_str

        result_str = this
        call result_str%append(str)
    end function concat_char

    !> @brief Equals QString
    function equals_qstring(this, other) result(equal)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: equal

        equal = this%equals(other%get())
    end function equals_qstring

    !> @brief Equals character string
    function equals_char(this, str) result(equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: equal

        equal = this%equals(str)
    end function equals_char

    !> @brief Not equals QString
    function not_equals_qstring(this, other) result(not_equal)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: not_equal

        not_equal = .not. this%equals_qstring(other)
    end function not_equals_qstring

    !> @brief Not equals character string
    function not_equals_char(this, str) result(not_equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: not_equal

        not_equal = .not. this%equals_char(str)
    end function not_equals_char

    !> @brief Less than QString
    function less_than_qstring(this, other) result(less)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: less

        less = (this%compare(other%get()) < 0)
    end function less_than_qstring

    !> @brief Less than character string
    function less_than_char(this, str) result(less)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: less

        less = (this%compare(str) < 0)
    end function less_than_char

    !> @brief Less equal QString
    function less_equal_qstring(this, other) result(less_equal)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: less_equal

        less_equal = (this%compare(other%get()) <= 0)
    end function less_equal_qstring

    !> @brief Less equal character string
    function less_equal_char(this, str) result(less_equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: less_equal

        less_equal = (this%compare(str) <= 0)
    end function less_equal_char

    !> @brief Greater than QString
    function greater_than_qstring(this, other) result(greater)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: greater

        greater = (this%compare(other%get()) > 0)
    end function greater_than_qstring

    !> @brief Greater than character string
    function greater_than_char(this, str) result(greater)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: greater

        greater = (this%compare(str) > 0)
    end function greater_than_char

    !> @brief Greater equal QString
    function greater_equal_qstring(this, other) result(greater_equal)
        class(QString), intent(in) :: this
        type(QString), intent(in) :: other
        logical :: greater_equal

        greater_equal = (this%compare(other%get()) >= 0)
    end function greater_equal_qstring

    !> @brief Greater equal character string
    function greater_equal_char(this, str) result(greater_equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: str
        logical :: greater_equal

        greater_equal = (this%compare(str) >= 0)
    end function greater_equal_char

    ! ========== Utility Functions ==========

    !> @brief Convert any type to string
    function to_string(value) result(str)
        class(*), intent(in) :: value
        character(len=:), allocatable :: str

        select type (value)
        type is (integer(int32))
            str = int_to_string(value)
        type is (real)
            str = real_to_string(value)
        type is (character(len=*))
            str = value
        type is (logical)
            if (value) then
                str = "true"
            else
                str = "false"
            end if
        class default
            str = "<unknown>"
        end select
    end function to_string

    ! ========== Module-level String Functions ==========

    function string_split(str, delimiter) result(parts)
        character(len=*), intent(in) :: str, delimiter
        character(len=:), allocatable :: parts(:)
        integer :: count, pos, start, i, delim_len
        character(len=:), allocatable :: temp_str

        delim_len = len(delimiter)
        if (delim_len == 0) then
            ! Empty delimiter - return single element array
            allocate(character(len=len(str)) :: parts(1))
            parts(1) = str
            return
        end if

        ! Count occurrences of delimiter
        count = 1
        temp_str = str
        pos = index(temp_str, delimiter)
        do while (pos > 0)
            count = count + 1
            temp_str = temp_str(pos + delim_len:)
            pos = index(temp_str, delimiter)
        end do

        ! Allocate result array
        allocate(character(len=len(str)) :: parts(count))

        ! Split the string
        temp_str = str
        start = 1
        i = 1
        pos = index(temp_str, delimiter)
        do while (pos > 0 .and. i < count)
            parts(i) = temp_str(start:start + pos - 1)
            i = i + 1
            start = start + pos + delim_len - 1
            temp_str = temp_str(pos + delim_len:)
            pos = index(temp_str, delimiter)
        end do

        ! Add the last part
        if (start <= len(str)) then
            parts(i) = str(start:)
        else
            parts(i) = ""
        end if
    end function string_split

    function string_join(parts, delimiter) result(joined)
        character(len=*), intent(in) :: parts(:), delimiter
        character(len=:), allocatable :: joined
        integer :: i, total_len

        total_len = 0
        do i = 1, size(parts)
            total_len = total_len + len(parts(i))
            if (i < size(parts)) total_len = total_len + len(delimiter)
        end do

        allocate(character(len=total_len) :: joined)
        joined = parts(1)
        do i = 2, size(parts)
            joined = joined // delimiter // parts(i)
        end do
    end function string_join

    function string_replace(str, old_str, new_str) result(result_str)
        character(len=*), intent(in) :: str, old_str, new_str
        character(len=:), allocatable :: result_str
        integer :: pos, start

        result_str = str
        start = 1
        pos = index(result_str(start:), old_str)

        do while (pos > 0)
            result_str = result_str(1:start+pos-2) // new_str // result_str(start+pos+len(old_str)-1:)
            start = start + pos + len(new_str) - 1
            pos = index(result_str(start:), old_str)
        end do
    end function string_replace

    function string_trim(str) result(trimmed)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: trimmed

        trimmed = trim(adjustl(str))
    end function string_trim

    function string_to_upper(str) result(upper)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: upper
        integer :: i, char_code

        allocate(character(len=len(str)) :: upper)
        do i = 1, len(str)
            char_code = iachar(str(i:i))
            if (char_code >= iachar('a') .and. char_code <= iachar('z')) then
                upper(i:i) = achar(char_code - 32)
            else
                upper(i:i) = str(i:i)
            end if
        end do
    end function string_to_upper

    function string_to_lower(str) result(lower)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower
        integer :: i, char_code

        allocate(character(len=len(str)) :: lower)
        do i = 1, len(str)
            char_code = iachar(str(i:i))
            if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                lower(i:i) = achar(char_code + 32)
            else
                lower(i:i) = str(i:i)
            end if
        end do
    end function string_to_lower

    function string_starts_with(str, prefix) result(starts)
        character(len=*), intent(in) :: str, prefix
        logical :: starts

        starts = .false.
        if (len(str) >= len(prefix)) then
            starts = (str(1:len(prefix)) == prefix)
        end if
    end function string_starts_with

    function string_ends_with(str, suffix) result(ends)
        character(len=*), intent(in) :: str, suffix
        logical :: ends
        integer :: str_len, suffix_len

        ends = .false.
        str_len = len(str)
        suffix_len = len(suffix)
        if (str_len >= suffix_len) then
            ends = (str(str_len-suffix_len+1:str_len) == suffix)
        end if
    end function string_ends_with

    function string_contains(str, substring) result(found)
        character(len=*), intent(in) :: str, substring
        logical :: found

        found = (index(str, substring) > 0)
    end function string_contains

    function string_to_int(str, success) result(value)
        character(len=*), intent(in) :: str
        logical, intent(out), optional :: success
        integer(int32) :: value
        integer :: iostat

        read(str, *, iostat=iostat) value

        if (present(success)) then
            success = (iostat == 0)
        end if
    end function string_to_int

    function string_to_real(str, success) result(value)
        character(len=*), intent(in) :: str
        logical, intent(out), optional :: success
        real :: value
        integer :: iostat

        read(str, *, iostat=iostat) value

        if (present(success)) then
            success = (iostat == 0)
        end if
    end function string_to_real

    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=32) :: buffer

        write(buffer, '(I0)') value
        str = trim(buffer)
    end function int_to_string

    function real_to_string(value, decimals) result(str)
        real, intent(in) :: value
        integer, intent(in), optional :: decimals
        character(len=:), allocatable :: str
        character(len=64) :: buffer
        character(len=20) :: fmt

        if (present(decimals)) then
            write(fmt, '(A,I0,A)') '(F0.', decimals, ')'
            write(buffer, fmt) value
        else
            write(buffer, '(F0.6)') value
        end if
        str = trim(buffer)
    end function real_to_string

end module forge_string_utils