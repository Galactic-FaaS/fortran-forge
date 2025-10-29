!> @brief QPen-like module for line drawing properties
!> @details Implements Qt-style pen for controlling line appearance
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qpen
    use iso_c_binding
    use forge_types
    implicit none
    private

    public :: forge_qpen_t
    public :: PEN_STYLE_SOLID, PEN_STYLE_DASH, PEN_STYLE_DOT, PEN_STYLE_DASHDOT
    public :: PEN_STYLE_DASHDOTDOT, PEN_STYLE_CUSTOM
    public :: PEN_CAP_FLAT, PEN_CAP_SQUARE, PEN_CAP_ROUND
    public :: PEN_JOIN_MITER, PEN_JOIN_BEVEL, PEN_JOIN_ROUND

    ! Pen style constants (matching Qt)
    integer, parameter :: PEN_STYLE_SOLID = 1
    integer, parameter :: PEN_STYLE_DASH = 2
    integer, parameter :: PEN_STYLE_DOT = 3
    integer, parameter :: PEN_STYLE_DASHDOT = 4
    integer, parameter :: PEN_STYLE_DASHDOTDOT = 5
    integer, parameter :: PEN_STYLE_CUSTOM = 6

    ! Pen cap style constants
    integer, parameter :: PEN_CAP_FLAT = 1
    integer, parameter :: PEN_CAP_SQUARE = 2
    integer, parameter :: PEN_CAP_ROUND = 3

    ! Pen join style constants
    integer, parameter :: PEN_JOIN_MITER = 1
    integer, parameter :: PEN_JOIN_BEVEL = 2
    integer, parameter :: PEN_JOIN_ROUND = 3

    !> @brief QPen-like class for controlling line drawing properties
    type :: forge_qpen_t
        private
        type(forge_color) :: color_ = forge_color(0.0_c_double, 0.0_c_double, 0.0_c_double, 1.0_c_double)
        real(c_double) :: width_ = 1.0_c_double
        integer :: style_ = PEN_STYLE_SOLID
        integer :: cap_style_ = PEN_CAP_FLAT
        integer :: join_style_ = PEN_JOIN_MITER
        real(c_double) :: miter_limit_ = 2.0_c_double
        real(c_double), allocatable :: dash_pattern_(:)
        real(c_double) :: dash_offset_ = 0.0_c_double
    contains
        procedure :: set_color => forge_qpen_set_color
        procedure :: color => forge_qpen_get_color
        procedure :: set_width => forge_qpen_set_width
        procedure :: width => forge_qpen_get_width
        procedure :: set_style => forge_qpen_set_style
        procedure :: style => forge_qpen_get_style
        procedure :: set_cap_style => forge_qpen_set_cap_style
        procedure :: cap_style => forge_qpen_get_cap_style
        procedure :: set_join_style => forge_qpen_set_join_style
        procedure :: join_style => forge_qpen_get_join_style
        procedure :: set_miter_limit => forge_qpen_set_miter_limit
        procedure :: miter_limit => forge_qpen_get_miter_limit
        procedure :: set_dash_pattern => forge_qpen_set_dash_pattern
        procedure :: dash_pattern => forge_qpen_get_dash_pattern
        procedure :: set_dash_offset => forge_qpen_set_dash_offset
        procedure :: dash_offset => forge_qpen_get_dash_offset
        procedure :: apply_to_cairo => forge_qpen_apply_to_cairo
        procedure :: is_cosmetic => forge_qpen_is_cosmetic
    end type forge_qpen_t

contains

    !> @brief Set pen color
    subroutine forge_qpen_set_color(this, color)
        class(forge_qpen_t), intent(inout) :: this
        type(forge_color), intent(in) :: color
        this%color_ = color
    end subroutine forge_qpen_set_color

    !> @brief Get pen color
    function forge_qpen_get_color(this) result(color)
        class(forge_qpen_t), intent(in) :: this
        type(forge_color) :: color
        color = this%color_
    end function forge_qpen_get_color

    !> @brief Set pen width
    subroutine forge_qpen_set_width(this, width)
        class(forge_qpen_t), intent(inout) :: this
        real(c_double), intent(in) :: width
        this%width_ = max(0.0_c_double, width)
    end subroutine forge_qpen_set_width

    !> @brief Get pen width
    function forge_qpen_get_width(this) result(width)
        class(forge_qpen_t), intent(in) :: this
        real(c_double) :: width
        width = this%width_
    end function forge_qpen_get_width

    !> @brief Set pen style
    subroutine forge_qpen_set_style(this, style)
        class(forge_qpen_t), intent(inout) :: this
        integer, intent(in) :: style
        this%style_ = style
        ! Clear custom dash pattern when changing to predefined style
        if (style /= PEN_STYLE_CUSTOM .and. allocated(this%dash_pattern_)) then
            deallocate(this%dash_pattern_)
        end if
    end subroutine forge_qpen_set_style

    !> @brief Get pen style
    function forge_qpen_get_style(this) result(style)
        class(forge_qpen_t), intent(in) :: this
        integer :: style
        style = this%style_
    end function forge_qpen_get_style

    !> @brief Set pen cap style
    subroutine forge_qpen_set_cap_style(this, cap_style)
        class(forge_qpen_t), intent(inout) :: this
        integer, intent(in) :: cap_style
        this%cap_style_ = cap_style
    end subroutine forge_qpen_set_cap_style

    !> @brief Get pen cap style
    function forge_qpen_get_cap_style(this) result(cap_style)
        class(forge_qpen_t), intent(in) :: this
        integer :: cap_style
        cap_style = this%cap_style_
    end function forge_qpen_get_cap_style

    !> @brief Set pen join style
    subroutine forge_qpen_set_join_style(this, join_style)
        class(forge_qpen_t), intent(inout) :: this
        integer, intent(in) :: join_style
        this%join_style_ = join_style
    end subroutine forge_qpen_set_join_style

    !> @brief Get pen join style
    function forge_qpen_get_join_style(this) result(join_style)
        class(forge_qpen_t), intent(in) :: this
        integer :: join_style
        join_style = this%join_style_
    end function forge_qpen_get_join_style

    !> @brief Set miter limit for join style
    subroutine forge_qpen_set_miter_limit(this, limit)
        class(forge_qpen_t), intent(inout) :: this
        real(c_double), intent(in) :: limit
        this%miter_limit_ = max(1.0_c_double, limit)
    end subroutine forge_qpen_set_miter_limit

    !> @brief Get miter limit
    function forge_qpen_get_miter_limit(this) result(limit)
        class(forge_qpen_t), intent(in) :: this
        real(c_double) :: limit
        limit = this%miter_limit_
    end function forge_qpen_get_miter_limit

    !> @brief Set custom dash pattern
    subroutine forge_qpen_set_dash_pattern(this, pattern, offset)
        class(forge_qpen_t), intent(inout) :: this
        real(c_double), intent(in) :: pattern(:)
        real(c_double), intent(in), optional :: offset

        if (allocated(this%dash_pattern_)) deallocate(this%dash_pattern_)
        allocate(this%dash_pattern_(size(pattern)))
        this%dash_pattern_ = pattern
        this%style_ = PEN_STYLE_CUSTOM

        if (present(offset)) then
            this%dash_offset_ = offset
        end if
    end subroutine forge_qpen_set_dash_pattern

    !> @brief Get dash pattern
    function forge_qpen_get_dash_pattern(this) result(pattern)
        class(forge_qpen_t), intent(in) :: this
        real(c_double), allocatable :: pattern(:)

        if (allocated(this%dash_pattern_)) then
            allocate(pattern(size(this%dash_pattern_)))
            pattern = this%dash_pattern_
        end if
    end function forge_qpen_get_dash_pattern

    !> @brief Set dash offset
    subroutine forge_qpen_set_dash_offset(this, offset)
        class(forge_qpen_t), intent(inout) :: this
        real(c_double), intent(in) :: offset
        this%dash_offset_ = offset
    end subroutine forge_qpen_set_dash_offset

    !> @brief Get dash offset
    function forge_qpen_get_dash_offset(this) result(offset)
        class(forge_qpen_t), intent(in) :: this
        real(c_double) :: offset
        offset = this%dash_offset_
    end function forge_qpen_get_dash_offset

    !> @brief Apply pen properties to Cairo context
    subroutine forge_qpen_apply_to_cairo(this, cr)
        use forge_cairo_bindings
        class(forge_qpen_t), intent(in) :: this
        type(c_ptr), intent(in) :: cr

        ! Set line width
        call cairo_set_line_width(cr, this%width_)

        ! Set line cap style
        select case (this%cap_style_)
        case (PEN_CAP_FLAT)
            call cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT)
        case (PEN_CAP_SQUARE)
            call cairo_set_line_cap(cr, CAIRO_LINE_CAP_SQUARE)
        case (PEN_CAP_ROUND)
            call cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND)
        end select

        ! Set line join style
        select case (this%join_style_)
        case (PEN_JOIN_MITER)
            call cairo_set_line_join(cr, CAIRO_LINE_JOIN_MITER)
            call cairo_set_miter_limit(cr, this%miter_limit_)
        case (PEN_JOIN_BEVEL)
            call cairo_set_line_join(cr, CAIRO_LINE_JOIN_BEVEL)
        case (PEN_JOIN_ROUND)
            call cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND)
        end select

        ! Set dash pattern
        if (this%style_ == PEN_STYLE_SOLID) then
            call cairo_set_dash(cr, c_null_ptr, 0, 0.0_c_double)
        else if (this%style_ == PEN_STYLE_CUSTOM .and. allocated(this%dash_pattern_)) then
            call cairo_set_dash(cr, c_loc(this%dash_pattern_), &
                               size(this%dash_pattern_, kind=c_int), this%dash_offset_)
        else
            ! Predefined dash patterns
            select case (this%style_)
            case (PEN_STYLE_DASH)
                call cairo_set_dash(cr, c_loc([4.0_c_double, 2.0_c_double]), 2, 0.0_c_double)
            case (PEN_STYLE_DOT)
                call cairo_set_dash(cr, c_loc([1.0_c_double, 2.0_c_double]), 2, 0.0_c_double)
            case (PEN_STYLE_DASHDOT)
                call cairo_set_dash(cr, c_loc([4.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double]), 4, 0.0_c_double)
            case (PEN_STYLE_DASHDOTDOT)
                call cairo_set_dash(cr, c_loc([4.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double, 1.0_c_double, 2.0_c_double]), 6, 0.0_c_double)
            end select
        end if
    end subroutine forge_qpen_apply_to_cairo

    !> @brief Check if pen is cosmetic (width independent of transformations)
    function forge_qpen_is_cosmetic(this) result(cosmetic)
        class(forge_qpen_t), intent(in) :: this
        logical :: cosmetic
        ! For now, assume all pens are not cosmetic
        ! In Qt, cosmetic pens maintain constant width regardless of transformations
        cosmetic = .false.
    end function forge_qpen_is_cosmetic

end module forge_qpen