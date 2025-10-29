!> @brief QString-equivalent string utilities
!> @details Advanced string manipulation with UTF-8 support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_string_utils
    use iso_c_binding
    use iso_fortran_env, only: int32
    implicit none
    private

    public :: QString, string_split, string_join, string_replace
    public :: string_trim, string_to_upper, string_to_lower
    public :: string_starts_with, string_ends_with, string_contains
    public :: string_to_int, string_to_real, int_to_string, real_to_string

    !> @brief QString - Advanced string type
    type :: QString
        private
        character(len=:), allocatable :: data
    contains
        procedure :: set => qstring_set
        procedure :: get => qstring_get
        procedure :: length => qstring_length
        procedure :: is_empty => qstring_is_empty
        procedure :: clear => qstring_clear
        procedure :: append => qstring_append
        procedure :: prepend => qstring_prepend
        procedure :: split => qstring_split
        procedure :: replace => qstring_replace
        procedure :: to_upper => qstring_to_upper
        procedure :: to_lower => qstring_to_lower
        procedure :: trim => qstring_trim
        procedure :: starts_with => qstring_starts_with
        procedure :: ends_with => qstring_ends_with
        procedure :: contains => qstring_contains
        procedure :: substring => qstring_substring
        procedure :: to_int => qstring_to_int
        procedure :: to_real => qstring_to_real
        procedure :: equals => qstring_equals
    end type QString

contains

    ! ========== QString Methods ==========

    subroutine qstring_set(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str
        
        if (allocated(this%data)) deallocate(this%data)
        allocate(character(len=len(str)) :: this%data)
        this%data = str
    end subroutine qstring_set

    function qstring_get(this) result(str)
        class(QString), intent(in) :: this
        character(len=:), allocatable :: str
        
        if (allocated(this%data)) then
            allocate(character(len=len(this%data)) :: str)
            str = this%data
        else
            allocate(character(len=0) :: str)
        end if
    end function qstring_get

    function qstring_length(this) result(len_val)
        class(QString), intent(in) :: this
        integer :: len_val
        
        if (allocated(this%data)) then
            len_val = len(this%data)
        else
            len_val = 0
        end if
    end function qstring_length

    function qstring_is_empty(this) result(empty)
        class(QString), intent(in) :: this
        logical :: empty
        
        empty = (.not. allocated(this%data)) .or. (len(this%data) == 0)
    end function qstring_is_empty

    subroutine qstring_clear(this)
        class(QString), intent(inout) :: this
        
        if (allocated(this%data)) deallocate(this%data)
    end subroutine qstring_clear

    subroutine qstring_append(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: temp
        
        if (allocated(this%data)) then
            temp = this%data // str
            call this%set(temp)
        else
            call this%set(str)
        end if
    end subroutine qstring_append

    subroutine qstring_prepend(this, str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: temp
        
        if (allocated(this%data)) then
            temp = str // this%data
            call this%set(temp)
        else
            call this%set(str)
        end if
    end subroutine qstring_prepend

    function qstring_split(this, delimiter) result(parts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: delimiter
        type(QString), allocatable :: parts(:)
        character(len=:), allocatable :: str
        integer :: count, pos, start, i
        
        str = this%get()
        count = 1
        pos = index(str, delimiter)
        do while (pos > 0)
            count = count + 1
            str = str(pos+len(delimiter):)
            pos = index(str, delimiter)
        end do
        
        allocate(parts(count))
        str = this%get()
        start = 1
        i = 1
        
        pos = index(str, delimiter)
        do while (pos > 0)
            call parts(i)%set(str(start:start+pos-1))
            i = i + 1
            start = start + pos + len(delimiter) - 1
            str = str(pos+len(delimiter):)
            pos = index(str, delimiter)
        end do
        
        if (start <= len(str)) then
            call parts(i)%set(str(start:))
        end if
    end function qstring_split

    subroutine qstring_replace(this, old_str, new_str)
        class(QString), intent(inout) :: this
        character(len=*), intent(in) :: old_str, new_str
        character(len=:), allocatable :: result
        
        result = string_replace(this%get(), old_str, new_str)
        call this%set(result)
    end subroutine qstring_replace

    subroutine qstring_to_upper(this)
        class(QString), intent(inout) :: this
        character(len=:), allocatable :: result
        
        result = string_to_upper(this%get())
        call this%set(result)
    end subroutine qstring_to_upper

    subroutine qstring_to_lower(this)
        class(QString), intent(inout) :: this
        character(len=:), allocatable :: result
        
        result = string_to_lower(this%get())
        call this%set(result)
    end subroutine qstring_to_lower

    subroutine qstring_trim(this)
        class(QString), intent(inout) :: this
        
        if (allocated(this%data)) then
            call this%set(trim(this%data))
        end if
    end subroutine qstring_trim

    function qstring_starts_with(this, prefix) result(starts)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: prefix
        logical :: starts
        
        starts = string_starts_with(this%get(), prefix)
    end function qstring_starts_with

    function qstring_ends_with(this, suffix) result(ends)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: suffix
        logical :: ends
        
        ends = string_ends_with(this%get(), suffix)
    end function qstring_ends_with

    function qstring_contains(this, substring) result(found)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: substring
        logical :: found
        
        found = string_contains(this%get(), substring)
    end function qstring_contains

    function qstring_substring(this, start_pos, length) result(substr)
        class(QString), intent(in) :: this
        integer, intent(in) :: start_pos
        integer, intent(in), optional :: length
        type(QString) :: substr
        character(len=:), allocatable :: str
        integer :: end_pos
        
        str = this%get()
        if (present(length)) then
            end_pos = min(start_pos + length - 1, len(str))
        else
            end_pos = len(str)
        end if
        
        if (start_pos >= 1 .and. start_pos <= len(str)) then
            call substr%set(str(start_pos:end_pos))
        end if
    end function qstring_substring

    function qstring_to_int(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        integer(int32) :: value
        
        value = string_to_int(this%get(), success)
    end function qstring_to_int

    function qstring_to_real(this, success) result(value)
        class(QString), intent(in) :: this
        logical, intent(out), optional :: success
        real :: value
        
        value = string_to_real(this%get(), success)
    end function qstring_to_real

    function qstring_equals(this, other) result(equal)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: other
        logical :: equal
        
        equal = (this%get() == other)
    end function qstring_equals

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

