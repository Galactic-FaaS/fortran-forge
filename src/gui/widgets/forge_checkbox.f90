!> @brief CheckBox widget
!> @details Checkbox with checked/unchecked states
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_checkbox
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QCheckBox

    !> @brief Checkbox widget
    type, extends(forge_widget) :: QCheckBox
        private
        type(forge_string) :: text
        logical :: checked = .false.
        logical :: tristate = .false.  ! Allow indeterminate state
        type(signal_bool) :: toggled  ! Emitted when state changes
        type(signal_void) :: clicked  ! Emitted on click regardless of state
    contains
        procedure :: set_text => checkbox_set_text
        procedure :: get_text => checkbox_get_text
        procedure :: set_checked => checkbox_set_checked
        procedure :: is_checked => checkbox_is_checked
        procedure :: toggle => checkbox_toggle
        procedure :: set_tristate => checkbox_set_tristate
    end type QCheckBox

contains

    subroutine checkbox_set_text(this, text)
        class(QCheckBox), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine checkbox_set_text

    function checkbox_get_text(this) result(text)
        class(QCheckBox), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function checkbox_get_text

    subroutine checkbox_set_checked(this, checked)
        class(QCheckBox), intent(inout) :: this
        logical, intent(in) :: checked
        logical :: old_state
        
        old_state = this%checked
        this%checked = checked
        
        if (old_state .neqv. checked) then
            call this%toggled%emit(checked)
        end if
    end subroutine checkbox_set_checked

    function checkbox_is_checked(this) result(checked)
        class(QCheckBox), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function checkbox_is_checked

    subroutine checkbox_toggle(this)
        class(QCheckBox), intent(inout) :: this
        call this%set_checked(.not. this%checked)
        call this%clicked%emit()
    end subroutine checkbox_toggle

    subroutine checkbox_set_tristate(this, tristate)
        class(QCheckBox), intent(inout) :: this
        logical, intent(in) :: tristate
        this%tristate = tristate
    end subroutine checkbox_set_tristate

end module forge_checkbox

