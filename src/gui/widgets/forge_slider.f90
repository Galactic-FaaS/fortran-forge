!> @brief Slider widget (value selection)
!> @details Slider for selecting numeric values
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_slider
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QSlider, Qt_Horizontal, Qt_Vertical

    integer, parameter :: Qt_Horizontal = 1
    integer, parameter :: Qt_Vertical = 2

    !> @brief Slider widget
    type, extends(forge_widget) :: QSlider
        private
        integer :: value = 0
        integer :: minimum = 0
        integer :: maximum = 99
        integer :: single_step = 1
        integer :: page_step = 10
        integer :: tick_interval = 0
        integer :: orientation = Qt_Horizontal
        logical :: inverted_appearance = .false.
        logical :: inverted_controls = .false.
        type(signal_int) :: value_changed
        type(signal_int) :: slider_moved
        type(signal_void) :: slider_pressed
        type(signal_void) :: slider_released
    contains
        procedure :: set_value => slider_set_value
        procedure :: get_value => slider_get_value
        procedure :: set_minimum => slider_set_minimum
        procedure :: set_maximum => slider_set_maximum
        procedure :: set_range => slider_set_range
        procedure :: set_single_step => slider_set_single_step
        procedure :: set_page_step => slider_set_page_step
        procedure :: set_orientation => slider_set_orientation
        procedure :: get_orientation => slider_get_orientation
        procedure :: set_tick_interval => slider_set_tick_interval
        procedure :: set_inverted => slider_set_inverted
    end type QSlider

contains

    subroutine slider_set_value(this, value)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: value
        integer :: clamped_value, old_value
        
        old_value = this%value
        clamped_value = max(this%minimum, min(this%maximum, value))
        this%value = clamped_value
        
        if (old_value /= clamped_value) then
            call this%value_changed%emit(clamped_value)
        end if
    end subroutine slider_set_value

    function slider_get_value(this) result(value)
        class(QSlider), intent(in) :: this
        integer :: value
        value = this%value
    end function slider_get_value

    subroutine slider_set_minimum(this, minimum)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: minimum
        this%minimum = minimum
        if (this%value < minimum) call this%set_value(minimum)
    end subroutine slider_set_minimum

    subroutine slider_set_maximum(this, maximum)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: maximum
        this%maximum = maximum
        if (this%value > maximum) call this%set_value(maximum)
    end subroutine slider_set_maximum

    subroutine slider_set_range(this, minimum, maximum)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: minimum, maximum
        this%minimum = minimum
        this%maximum = maximum
        call this%set_value(this%value)  ! Re-clamp
    end subroutine slider_set_range

    subroutine slider_set_single_step(this, step)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: step
        this%single_step = step
    end subroutine slider_set_single_step

    subroutine slider_set_page_step(this, step)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: step
        this%page_step = step
    end subroutine slider_set_page_step

    subroutine slider_set_orientation(this, orientation)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: orientation
        this%orientation = orientation
    end subroutine slider_set_orientation

    function slider_get_orientation(this) result(orientation)
        class(QSlider), intent(in) :: this
        integer :: orientation
        orientation = this%orientation
    end function slider_get_orientation

    subroutine slider_set_tick_interval(this, interval)
        class(QSlider), intent(inout) :: this
        integer, intent(in) :: interval
        this%tick_interval = interval
    end subroutine slider_set_tick_interval

    subroutine slider_set_inverted(this, inverted)
        class(QSlider), intent(inout) :: this
        logical, intent(in) :: inverted
        this%inverted_appearance = inverted
        this%inverted_controls = inverted
    end subroutine slider_set_inverted

end module forge_slider

