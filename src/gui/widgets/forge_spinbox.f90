!> @brief SpinBox widget (numeric input with buttons)
!> @details Numeric input with increment/decrement buttons
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_spinbox
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QSpinBox, QDoubleSpinBox

    !> @brief Integer spin box
    type, extends(forge_widget) :: QSpinBox
        private
        integer :: value = 0
        integer :: minimum = 0
        integer :: maximum = 99
        integer :: single_step = 1
        type(forge_string) :: prefix
        type(forge_string) :: suffix
        type(signal_int) :: value_changed
    contains
        procedure :: set_value => spinbox_set_value
        procedure :: get_value => spinbox_get_value
        procedure :: set_minimum => spinbox_set_minimum
        procedure :: set_maximum => spinbox_set_maximum
        procedure :: set_range => spinbox_set_range
        procedure :: set_single_step => spinbox_set_single_step
        procedure :: set_prefix => spinbox_set_prefix
        procedure :: set_suffix => spinbox_set_suffix
        procedure :: step_up => spinbox_step_up
        procedure :: step_down => spinbox_step_down
    end type QSpinBox

    !> @brief Double-precision spin box
    type, extends(forge_widget) :: QDoubleSpinBox
        private
        real(c_double) :: value = 0.0_c_double
        real(c_double) :: minimum = 0.0_c_double
        real(c_double) :: maximum = 99.0_c_double
        real(c_double) :: single_step = 1.0_c_double
        integer :: decimals = 2
        type(forge_string) :: prefix
        type(forge_string) :: suffix
        ! Note: Would need signal_double type
    contains
        procedure :: set_value => double_spinbox_set_value
        procedure :: get_value => double_spinbox_get_value
        procedure :: set_decimals => double_spinbox_set_decimals
    end type QDoubleSpinBox

contains

    ! ========== QSpinBox Implementation ==========

    subroutine spinbox_set_value(this, value)
        class(QSpinBox), intent(inout) :: this
        integer, intent(in) :: value
        integer :: clamped_value, old_value
        
        old_value = this%value
        clamped_value = max(this%minimum, min(this%maximum, value))
        this%value = clamped_value
        
        if (old_value /= clamped_value) then
            call this%value_changed%emit(clamped_value)
        end if
    end subroutine spinbox_set_value

    function spinbox_get_value(this) result(value)
        class(QSpinBox), intent(in) :: this
        integer :: value
        value = this%value
    end function spinbox_get_value

    subroutine spinbox_set_minimum(this, minimum)
        class(QSpinBox), intent(inout) :: this
        integer, intent(in) :: minimum
        this%minimum = minimum
        if (this%value < minimum) call this%set_value(minimum)
    end subroutine spinbox_set_minimum

    subroutine spinbox_set_maximum(this, maximum)
        class(QSpinBox), intent(inout) :: this
        integer, intent(in) :: maximum
        this%maximum = maximum
        if (this%value > maximum) call this%set_value(maximum)
    end subroutine spinbox_set_maximum

    subroutine spinbox_set_range(this, minimum, maximum)
        class(QSpinBox), intent(inout) :: this
        integer, intent(in) :: minimum, maximum
        this%minimum = minimum
        this%maximum = maximum
        call this%set_value(this%value)  ! Re-clamp
    end subroutine spinbox_set_range

    subroutine spinbox_set_single_step(this, step)
        class(QSpinBox), intent(inout) :: this
        integer, intent(in) :: step
        this%single_step = step
    end subroutine spinbox_set_single_step

    subroutine spinbox_set_prefix(this, prefix)
        class(QSpinBox), intent(inout) :: this
        character(len=*), intent(in) :: prefix
        call this%prefix%set(prefix)
    end subroutine spinbox_set_prefix

    subroutine spinbox_set_suffix(this, suffix)
        class(QSpinBox), intent(inout) :: this
        character(len=*), intent(in) :: suffix
        call this%suffix%set(suffix)
    end subroutine spinbox_set_suffix

    subroutine spinbox_step_up(this)
        class(QSpinBox), intent(inout) :: this
        call this%set_value(this%value + this%single_step)
    end subroutine spinbox_step_up

    subroutine spinbox_step_down(this)
        class(QSpinBox), intent(inout) :: this
        call this%set_value(this%value - this%single_step)
    end subroutine spinbox_step_down

    ! ========== QDoubleSpinBox Implementation ==========

    subroutine double_spinbox_set_value(this, value)
        class(QDoubleSpinBox), intent(inout) :: this
        real(c_double), intent(in) :: value
        real(c_double) :: clamped_value
        
        clamped_value = max(this%minimum, min(this%maximum, value))
        this%value = clamped_value
    end subroutine double_spinbox_set_value

    function double_spinbox_get_value(this) result(value)
        class(QDoubleSpinBox), intent(in) :: this
        real(c_double) :: value
        value = this%value
    end function double_spinbox_get_value

    subroutine double_spinbox_set_decimals(this, decimals)
        class(QDoubleSpinBox), intent(inout) :: this
        integer, intent(in) :: decimals
        this%decimals = decimals
    end subroutine double_spinbox_set_decimals

end module forge_spinbox

