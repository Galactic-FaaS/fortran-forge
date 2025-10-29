!> @brief QBrush-like module for fill patterns and colors
!> @details Implements Qt-style brush for controlling fill appearance
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qbrush
    use iso_c_binding
    use forge_types
    implicit none
    private

    public :: forge_qbrush_t
    public :: BRUSH_STYLE_SOLID, BRUSH_STYLE_DENSE1, BRUSH_STYLE_DENSE2, BRUSH_STYLE_DENSE3
    public :: BRUSH_STYLE_DENSE4, BRUSH_STYLE_DENSE5, BRUSH_STYLE_DENSE6, BRUSH_STYLE_DENSE7
    public :: BRUSH_STYLE_NO_BRUSH, BRUSH_STYLE_HORIZONTAL, BRUSH_STYLE_VERTICAL
    public :: BRUSH_STYLE_CROSS, BRUSH_STYLE_BDIAGONAL, BRUSH_STYLE_FDIAGONAL
    public :: BRUSH_STYLE_DIAGCROSS, BRUSH_STYLE_LINEAR_GRADIENT, BRUSH_STYLE_RADIAL_GRADIENT
    public :: BRUSH_STYLE_CONICAL_GRADIENT, BRUSH_STYLE_TEXTURE

    ! Brush style constants (matching Qt)
    integer, parameter :: BRUSH_STYLE_SOLID = 1
    integer, parameter :: BRUSH_STYLE_DENSE1 = 2
    integer, parameter :: BRUSH_STYLE_DENSE2 = 3
    integer, parameter :: BRUSH_STYLE_DENSE3 = 4
    integer, parameter :: BRUSH_STYLE_DENSE4 = 5
    integer, parameter :: BRUSH_STYLE_DENSE5 = 6
    integer, parameter :: BRUSH_STYLE_DENSE6 = 7
    integer, parameter :: BRUSH_STYLE_DENSE7 = 8
    integer, parameter :: BRUSH_STYLE_NO_BRUSH = 9
    integer, parameter :: BRUSH_STYLE_HORIZONTAL = 10
    integer, parameter :: BRUSH_STYLE_VERTICAL = 11
    integer, parameter :: BRUSH_STYLE_CROSS = 12
    integer, parameter :: BRUSH_STYLE_BDIAGONAL = 13
    integer, parameter :: BRUSH_STYLE_FDIAGONAL = 14
    integer, parameter :: BRUSH_STYLE_DIAGCROSS = 15
    integer, parameter :: BRUSH_STYLE_LINEAR_GRADIENT = 16
    integer, parameter :: BRUSH_STYLE_RADIAL_GRADIENT = 17
    integer, parameter :: BRUSH_STYLE_CONICAL_GRADIENT = 18
    integer, parameter :: BRUSH_STYLE_TEXTURE = 19

    !> @brief Gradient stop structure
    type :: gradient_stop_t
        real(c_double) :: position = 0.0_c_double
        type(forge_color) :: color = forge_color(0.0_c_double, 0.0_c_double, 0.0_c_double, 1.0_c_double)
    end type gradient_stop_t

    !> @brief QBrush-like class for controlling fill patterns and colors
    type :: forge_qbrush_t
        private
        type(forge_color) :: color_ = forge_color(0.0_c_double, 0.0_c_double, 0.0_c_double, 1.0_c_double)
        integer :: style_ = BRUSH_STYLE_SOLID
        ! For gradients
        real(c_double) :: gradient_x1_ = 0.0_c_double
        real(c_double) :: gradient_y1_ = 0.0_c_double
        real(c_double) :: gradient_x2_ = 0.0_c_double
        real(c_double) :: gradient_y2_ = 0.0_c_double
        real(c_double) :: gradient_cx_ = 0.0_c_double
        real(c_double) :: gradient_cy_ = 0.0_c_double
        real(c_double) :: gradient_radius_ = 0.0_c_double
        real(c_double) :: gradient_angle_ = 0.0_c_double
        type(gradient_stop_t), allocatable :: gradient_stops_(:)
        ! For texture
        type(c_ptr) :: texture_surface_ = c_null_ptr
        integer(c_int) :: texture_width_ = 0
        integer(c_int) :: texture_height_ = 0
    contains
        procedure :: set_color => forge_qbrush_set_color
        procedure :: color => forge_qbrush_get_color
        procedure :: set_style => forge_qbrush_set_style
        procedure :: style => forge_qbrush_get_style
        procedure :: set_linear_gradient => forge_qbrush_set_linear_gradient
        procedure :: set_radial_gradient => forge_qbrush_set_radial_gradient
        procedure :: set_conical_gradient => forge_qbrush_set_conical_gradient
        procedure :: add_gradient_stop => forge_qbrush_add_gradient_stop
        procedure :: set_texture => forge_qbrush_set_texture
        procedure :: apply_to_cairo => forge_qbrush_apply_to_cairo
        procedure :: is_opaque => forge_qbrush_is_opaque
        procedure :: cleanup => forge_qbrush_cleanup
    end type forge_qbrush_t

contains

    !> @brief Set brush color
    subroutine forge_qbrush_set_color(this, color)
        class(forge_qbrush_t), intent(inout) :: this
        type(forge_color), intent(in) :: color
        this%color_ = color
        this%style_ = BRUSH_STYLE_SOLID
    end subroutine forge_qbrush_set_color

    !> @brief Get brush color
    function forge_qbrush_get_color(this) result(color)
        class(forge_qbrush_t), intent(in) :: this
        type(forge_color) :: color
        color = this%color_
    end function forge_qbrush_get_color

    !> @brief Set brush style
    subroutine forge_qbrush_set_style(this, style)
        class(forge_qbrush_t), intent(inout) :: this
        integer, intent(in) :: style
        this%style_ = style
        ! Clear gradient stops when changing to non-gradient style
        if (style < BRUSH_STYLE_LINEAR_GRADIENT .and. allocated(this%gradient_stops_)) then
            deallocate(this%gradient_stops_)
        end if
    end subroutine forge_qbrush_set_style

    !> @brief Get brush style
    function forge_qbrush_get_style(this) result(style)
        class(forge_qbrush_t), intent(in) :: this
        integer :: style
        style = this%style_
    end function forge_qbrush_get_style

    !> @brief Set linear gradient parameters
    subroutine forge_qbrush_set_linear_gradient(this, x1, y1, x2, y2)
        class(forge_qbrush_t), intent(inout) :: this
        real(c_double), intent(in) :: x1, y1, x2, y2
        this%gradient_x1_ = x1
        this%gradient_y1_ = y1
        this%gradient_x2_ = x2
        this%gradient_y2_ = y2
        this%style_ = BRUSH_STYLE_LINEAR_GRADIENT
    end subroutine forge_qbrush_set_linear_gradient

    !> @brief Set radial gradient parameters
    subroutine forge_qbrush_set_radial_gradient(this, cx, cy, radius)
        class(forge_qbrush_t), intent(inout) :: this
        real(c_double), intent(in) :: cx, cy, radius
        this%gradient_cx_ = cx
        this%gradient_cy_ = cy
        this%gradient_radius_ = radius
        this%style_ = BRUSH_STYLE_RADIAL_GRADIENT
    end subroutine forge_qbrush_set_radial_gradient

    !> @brief Set conical gradient parameters
    subroutine forge_qbrush_set_conical_gradient(this, cx, cy, angle)
        class(forge_qbrush_t), intent(inout) :: this
        real(c_double), intent(in) :: cx, cy, angle
        this%gradient_cx_ = cx
        this%gradient_cy_ = cy
        this%gradient_angle_ = angle
        this%style_ = BRUSH_STYLE_CONICAL_GRADIENT
    end subroutine forge_qbrush_set_conical_gradient

    !> @brief Add gradient color stop
    subroutine forge_qbrush_add_gradient_stop(this, position, color)
        class(forge_qbrush_t), intent(inout) :: this
        real(c_double), intent(in) :: position
        type(forge_color), intent(in) :: color
        type(gradient_stop_t), allocatable :: temp_stops(:)
        integer :: n

        if (.not. allocated(this%gradient_stops_)) then
            allocate(this%gradient_stops_(1))
            this%gradient_stops_(1)%position = position
            this%gradient_stops_(1)%color = color
        else
            n = size(this%gradient_stops_)
            allocate(temp_stops(n + 1))
            temp_stops(1:n) = this%gradient_stops_
            temp_stops(n + 1)%position = position
            temp_stops(n + 1)%color = color
            deallocate(this%gradient_stops_)
            allocate(this%gradient_stops_(n + 1))
            this%gradient_stops_ = temp_stops
            deallocate(temp_stops)
        end if
    end subroutine forge_qbrush_add_gradient_stop

    !> @brief Set texture pattern
    subroutine forge_qbrush_set_texture(this, surface, width, height)
        class(forge_qbrush_t), intent(inout) :: this
        type(c_ptr), intent(in) :: surface
        integer(c_int), intent(in) :: width, height
        this%texture_surface_ = surface
        this%texture_width_ = width
        this%texture_height_ = height
        this%style_ = BRUSH_STYLE_TEXTURE
    end subroutine forge_qbrush_set_texture

    !> @brief Apply brush properties to Cairo context
    subroutine forge_qbrush_apply_to_cairo(this, cr)
        use forge_cairo_bindings
        class(forge_qbrush_t), intent(in) :: this
        type(c_ptr), intent(in) :: cr
        type(c_ptr) :: pattern
        integer :: i

        select case (this%style_)
        case (BRUSH_STYLE_SOLID)
            call cairo_set_source_rgba(cr, this%color_%r, this%color_%g, this%color_%b, this%color_%a)

        case (BRUSH_STYLE_NO_BRUSH)
            ! No fill
            return

        case (BRUSH_STYLE_LINEAR_GRADIENT)
            if (allocated(this%gradient_stops_)) then
                pattern = cairo_pattern_create_linear(this%gradient_x1_, this%gradient_y1_, &
                                                     this%gradient_x2_, this%gradient_y2_)
                do i = 1, size(this%gradient_stops_)
                    call cairo_pattern_add_color_stop_rgba(pattern, &
                        this%gradient_stops_(i)%position, &
                        this%gradient_stops_(i)%color%r, &
                        this%gradient_stops_(i)%color%g, &
                        this%gradient_stops_(i)%color%b, &
                        this%gradient_stops_(i)%color%a)
                end do
                call cairo_set_source(cr, pattern)
                call cairo_pattern_destroy(pattern)
            end if

        case (BRUSH_STYLE_RADIAL_GRADIENT)
            if (allocated(this%gradient_stops_)) then
                pattern = cairo_pattern_create_radial(this%gradient_cx_, this%gradient_cy_, 0.0_c_double, &
                                                     this%gradient_cx_, this%gradient_cy_, this%gradient_radius_)
                do i = 1, size(this%gradient_stops_)
                    call cairo_pattern_add_color_stop_rgba(pattern, &
                        this%gradient_stops_(i)%position, &
                        this%gradient_stops_(i)%color%r, &
                        this%gradient_stops_(i)%color%g, &
                        this%gradient_stops_(i)%color%b, &
                        this%gradient_stops_(i)%color%a)
                end do
                call cairo_set_source(cr, pattern)
                call cairo_pattern_destroy(pattern)
            end if

        case (BRUSH_STYLE_TEXTURE)
            if (c_associated(this%texture_surface_)) then
                pattern = cairo_pattern_create_for_surface(this%texture_surface_)
                call cairo_pattern_set_extend(pattern, CAIRO_EXTEND_REPEAT)
                call cairo_set_source(cr, pattern)
                call cairo_pattern_destroy(pattern)
            end if

        case default
            ! For pattern styles, use solid color for now
            ! TODO: Implement hatch patterns
            call cairo_set_source_rgba(cr, this%color_%r, this%color_%g, this%color_%b, this%color_%a)
        end select
    end subroutine forge_qbrush_apply_to_cairo

    !> @brief Check if brush is opaque
    function forge_qbrush_is_opaque(this) result(opaque)
        class(forge_qbrush_t), intent(in) :: this
        logical :: opaque

        select case (this%style_)
        case (BRUSH_STYLE_SOLID)
            opaque = (this%color_%a >= 1.0_c_double)
        case (BRUSH_STYLE_NO_BRUSH)
            opaque = .false.
        case (BRUSH_STYLE_LINEAR_GRADIENT, BRUSH_STYLE_RADIAL_GRADIENT, BRUSH_STYLE_CONICAL_GRADIENT)
            ! Check if all gradient stops are opaque
            if (allocated(this%gradient_stops_)) then
                opaque = all(this%gradient_stops_%color%a >= 1.0_c_double)
            else
                opaque = .false.
            end if
        case (BRUSH_STYLE_TEXTURE)
            ! Assume texture might have transparency
            opaque = .false.
        case default
            opaque = .true.
        end select
    end function forge_qbrush_is_opaque

    !> @brief Cleanup resources
    subroutine forge_qbrush_cleanup(this)
        class(forge_qbrush_t), intent(inout) :: this
        if (allocated(this%gradient_stops_)) deallocate(this%gradient_stops_)
        ! Note: texture_surface_ is managed externally
    end subroutine forge_qbrush_cleanup

end module forge_qbrush