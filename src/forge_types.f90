!> @brief Core type definitions for ForGE
!> @details Defines fundamental types used throughout the ForGE library
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_types
    use iso_c_binding, only: c_int, c_bool, c_double, c_char, c_ptr, c_null_ptr, c_null_char
    implicit none
    private

    public :: c_int, c_bool, c_double, c_char, c_ptr, c_null_ptr, c_null_char
    public :: forge_string, forge_color, forge_size, forge_position, forge_rect, forge_size_policy

    !> Maximum length for dynamic strings
    integer, parameter, public :: FORGE_MAX_STRING_LEN = 1024

    !> @brief Dynamic string type for Fortran-C interop
    type :: forge_string
        character(len=:), allocatable :: str
    contains
        procedure :: set => forge_string_set
        procedure :: get => forge_string_get
        procedure :: to_c => forge_string_to_c
        procedure :: from_c => forge_string_from_c
        procedure :: length => forge_string_length
    end type forge_string

    !> @brief Color representation (RGBA, 0.0-1.0)
    type :: forge_color
        real(c_double) :: r = 0.0_c_double  !< Red component
        real(c_double) :: g = 0.0_c_double  !< Green component
        real(c_double) :: b = 0.0_c_double  !< Blue component
        real(c_double) :: a = 1.0_c_double  !< Alpha component
    contains
        procedure :: set_rgb => forge_color_set_rgb
        procedure :: set_rgba => forge_color_set_rgba
    end type forge_color

    !> @brief Size specification (width, height)
    type :: forge_size
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
    contains
        procedure :: set => forge_size_set
    end type forge_size

    !> @brief Position specification (x, y)
    type :: forge_position
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
    contains
        procedure :: set => forge_position_set
    end type forge_position

    !> @brief Rectangle specification
    type :: forge_rect
        type(forge_position) :: pos
        type(forge_size) :: size
    end type forge_rect

    !> @brief Size policy for widgets (Qt-style)
    type :: forge_size_policy
        integer :: horizontal_policy = 0  !< 0=Fixed, 1=Minimum, 2=Maximum, 3=Preferred, 4=Expanding, 5=MinimumExpanding, 6=Ignored
        integer :: vertical_policy = 0
        integer :: horizontal_stretch = 0
        integer :: vertical_stretch = 0
    contains
        procedure :: set_horizontal_policy => forge_size_policy_set_horizontal
        procedure :: set_vertical_policy => forge_size_policy_set_vertical
        procedure :: set_horizontal_stretch => forge_size_policy_set_horizontal_stretch
        procedure :: set_vertical_stretch => forge_size_policy_set_vertical_stretch
    end type forge_size_policy

contains

    !> @brief Set string value
    subroutine forge_string_set(this, str)
        class(forge_string), intent(inout) :: this
        character(len=*), intent(in) :: str
        
        if (allocated(this%str)) deallocate(this%str)
        allocate(character(len=len(str)) :: this%str)
        this%str = str
    end subroutine forge_string_set

    !> @brief Get string value
    function forge_string_get(this) result(str)
        class(forge_string), intent(in) :: this
        character(len=:), allocatable :: str
        
        if (allocated(this%str)) then
            allocate(character(len=len(this%str)) :: str)
            str = this%str
        else
            allocate(character(len=0) :: str)
        end if
    end function forge_string_get

    !> @brief Convert to C string (null-terminated)
    function forge_string_to_c(this) result(c_str)
        class(forge_string), intent(in) :: this
        character(len=:), allocatable :: c_str
        
        if (allocated(this%str)) then
            c_str = this%str // c_null_char
        else
            c_str = c_null_char
        end if
    end function forge_string_to_c

    !> @brief Create from C string
    subroutine forge_string_from_c(this, c_str)
        class(forge_string), intent(inout) :: this
        character(len=*), intent(in) :: c_str
        integer :: null_pos
        
        null_pos = index(c_str, c_null_char)
        if (null_pos > 0) then
            call this%set(c_str(1:null_pos-1))
        else
            call this%set(c_str)
        end if
    end subroutine forge_string_from_c

    !> @brief Get string length
    pure function forge_string_length(this) result(string_len)
        class(forge_string), intent(in) :: this
        integer :: string_len
        
        if (allocated(this%str)) then
            string_len = len(this%str)
        else
            string_len = 0
        end if
    end function forge_string_length

    !> @brief Set RGB color components
    subroutine forge_color_set_rgb(this, r, g, b)
        class(forge_color), intent(inout) :: this
        real(c_double), intent(in) :: r, g, b
        
        this%r = r
        this%g = g
        this%b = b
        this%a = 1.0_c_double
    end subroutine forge_color_set_rgb

    !> @brief Set RGBA color components
    subroutine forge_color_set_rgba(this, r, g, b, a)
        class(forge_color), intent(inout) :: this
        real(c_double), intent(in) :: r, g, b, a
        
        this%r = r
        this%g = g
        this%b = b
        this%a = a
    end subroutine forge_color_set_rgba

    !> @brief Set size dimensions
    subroutine forge_size_set(this, width, height)
        class(forge_size), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        
        this%width = width
        this%height = height
    end subroutine forge_size_set

    !> @brief Set position coordinates
    subroutine forge_position_set(this, x, y)
        class(forge_position), intent(inout) :: this
        integer(c_int), intent(in) :: x, y

        this%x = x
        this%y = y
    end subroutine forge_position_set

    !> @brief Set horizontal size policy
    subroutine forge_size_policy_set_horizontal(this, policy)
        class(forge_size_policy), intent(inout) :: this
        integer, intent(in) :: policy
        this%horizontal_policy = policy
    end subroutine forge_size_policy_set_horizontal

    !> @brief Set vertical size policy
    subroutine forge_size_policy_set_vertical(this, policy)
        class(forge_size_policy), intent(inout) :: this
        integer, intent(in) :: policy
        this%vertical_policy = policy
    end subroutine forge_size_policy_set_vertical

    !> @brief Set horizontal stretch factor
    subroutine forge_size_policy_set_horizontal_stretch(this, stretch)
        class(forge_size_policy), intent(inout) :: this
        integer, intent(in) :: stretch
        this%horizontal_stretch = stretch
    end subroutine forge_size_policy_set_horizontal_stretch

    !> @brief Set vertical stretch factor
    subroutine forge_size_policy_set_vertical_stretch(this, stretch)
        class(forge_size_policy), intent(inout) :: this
        integer, intent(in) :: stretch
        this%vertical_stretch = stretch
    end subroutine forge_size_policy_set_vertical_stretch

end module forge_types

