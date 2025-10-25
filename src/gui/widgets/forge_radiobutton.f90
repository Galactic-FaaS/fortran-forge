!> @brief RadioButton widget
!> @details Radio button for mutually exclusive selections
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_radiobutton
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QRadioButton, QButtonGroup

    !> @brief Radio button widget
    type, extends(forge_widget) :: QRadioButton
        private
        type(forge_string) :: text
        logical :: checked = .false.
        type(c_ptr) :: button_group = c_null_ptr  ! Reference to group
        type(signal_bool) :: toggled
        type(signal_void) :: clicked
    contains
        procedure :: set_text => radiobutton_set_text
        procedure :: get_text => radiobutton_get_text
        procedure :: set_checked => radiobutton_set_checked
        procedure :: is_checked => radiobutton_is_checked
        procedure :: toggle => radiobutton_toggle
    end type QRadioButton

    !> @brief Button group for managing radio buttons
    type :: QButtonGroup
        private
        type(QRadioButton), pointer :: buttons(:) => null()
        integer :: count = 0
        integer :: checked_id = -1
        type(signal_int) :: button_clicked
    contains
        procedure :: add_button => buttongroup_add_button
        procedure :: remove_button => buttongroup_remove_button
        procedure :: checked_button => buttongroup_checked_button
        procedure :: set_exclusive => buttongroup_set_exclusive
    end type QButtonGroup

contains

    subroutine radiobutton_set_text(this, text)
        class(QRadioButton), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine radiobutton_set_text

    function radiobutton_get_text(this) result(text)
        class(QRadioButton), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function radiobutton_get_text

    subroutine radiobutton_set_checked(this, checked)
        class(QRadioButton), intent(inout) :: this
        logical, intent(in) :: checked
        logical :: old_state
        
        old_state = this%checked
        this%checked = checked
        
        if (old_state .neqv. checked) then
            call this%toggled%emit(checked)
        end if
    end subroutine radiobutton_set_checked

    function radiobutton_is_checked(this) result(checked)
        class(QRadioButton), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function radiobutton_is_checked

    subroutine radiobutton_toggle(this)
        class(QRadioButton), intent(inout) :: this
        call this%set_checked(.true.)  ! Radio buttons only turn on
        call this%clicked%emit()
    end subroutine radiobutton_toggle

    ! ========== QButtonGroup Implementation ==========

    subroutine buttongroup_add_button(this, button, id)
        class(QButtonGroup), intent(inout) :: this
        type(QRadioButton), target, intent(in) :: button
        integer, intent(in), optional :: id
        ! TODO: Implement button group management
    end subroutine buttongroup_add_button

    subroutine buttongroup_remove_button(this, button)
        class(QButtonGroup), intent(inout) :: this
        type(QRadioButton), intent(in) :: button
        ! TODO: Implement
    end subroutine buttongroup_remove_button

    function buttongroup_checked_button(this) result(id)
        class(QButtonGroup), intent(in) :: this
        integer :: id
        id = this%checked_id
    end function buttongroup_checked_button

    subroutine buttongroup_set_exclusive(this, exclusive)
        class(QButtonGroup), intent(inout) :: this
        logical, intent(in) :: exclusive
        ! TODO: Implement exclusive mode
    end subroutine buttongroup_set_exclusive

end module forge_radiobutton

