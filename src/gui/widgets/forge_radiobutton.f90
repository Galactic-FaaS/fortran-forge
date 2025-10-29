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
        integer, allocatable :: button_ids(:)
        integer :: count = 0
        integer :: checked_id = -1
        logical :: exclusive = .true.
        type(signal_int) :: button_clicked
    contains
        procedure :: add_button => buttongroup_add_button
        procedure :: remove_button => buttongroup_remove_button
        procedure :: checked_button => buttongroup_checked_button
        procedure :: set_exclusive => buttongroup_set_exclusive
        procedure :: button_toggled => buttongroup_button_toggled  ! Internal handler
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
        integer :: button_id
        type(QRadioButton), pointer :: temp_buttons(:)
        integer, allocatable :: temp_ids(:)

        ! Use provided ID or auto-assign
        if (present(id)) then
            button_id = id
        else
            button_id = this%count + 1
        end if

        ! Expand arrays if necessary
        if (.not. associated(this%buttons)) then
            allocate(this%buttons(5))
            allocate(this%button_ids(5))
        else if (this%count >= size(this%buttons)) then
            allocate(temp_buttons(size(this%buttons) * 2))
            allocate(temp_ids(size(this%button_ids) * 2))
            temp_buttons(1:this%count) = this%buttons(1:this%count)
            temp_ids(1:this%count) = this%button_ids(1:this%count)
            deallocate(this%buttons)
            deallocate(this%button_ids)
            this%buttons => temp_buttons
            this%button_ids = temp_ids
        end if

        ! Add button to group
        this%count = this%count + 1
        this%buttons(this%count)%text = button%text
        this%buttons(this%count)%checked = button%checked
        this%buttons(this%count)%button_group = c_loc(this)
        this%button_ids(this%count) = button_id

        ! Connect to toggled signal for group management
        call this%buttons(this%count)%toggled%connect(this%button_toggled)
    end subroutine buttongroup_add_button

    subroutine buttongroup_remove_button(this, button)
        class(QButtonGroup), intent(inout) :: this
        type(QRadioButton), intent(in) :: button
        integer :: i, j

        if (.not. associated(this%buttons)) return

        ! Find and remove the button
        do i = 1, this%count
            if (associated(this%buttons(i)%button_group, c_loc(this))) then
                ! Disconnect signal
                call this%buttons(i)%toggled%disconnect(this%button_toggled)

                ! Shift remaining buttons
                do j = i, this%count - 1
                    this%buttons(j) = this%buttons(j + 1)
                    this%button_ids(j) = this%button_ids(j + 1)
                end do
                this%count = this%count - 1

                ! Update checked_id if necessary
                if (this%checked_id == this%button_ids(i)) then
                    this%checked_id = -1
                end if
                exit
            end if
        end do
    end subroutine buttongroup_remove_button

    function buttongroup_checked_button(this) result(id)
        class(QButtonGroup), intent(in) :: this
        integer :: id
        id = this%checked_id
    end function buttongroup_checked_button

    subroutine buttongroup_set_exclusive(this, exclusive)
        class(QButtonGroup), intent(inout) :: this
        logical, intent(in) :: exclusive
        this%exclusive = exclusive
    end subroutine buttongroup_set_exclusive

    !> @brief Internal handler for button toggles
    subroutine buttongroup_button_toggled(this, checked)
        class(QButtonGroup), intent(inout) :: this
        logical, intent(in) :: checked
        integer :: i

        if (.not. this%exclusive) return

        ! In exclusive mode, uncheck all other buttons when one is checked
        if (checked) then
            do i = 1, this%count
                if (this%buttons(i)%checked .and. this%checked_id /= this%button_ids(i)) then
                    call this%buttons(i)%set_checked(.false.)
                end if
            end do
            ! Find which button was checked
            do i = 1, this%count
                if (this%buttons(i)%checked) then
                    this%checked_id = this%button_ids(i)
                    call this%button_clicked%emit(this%checked_id)
                    exit
                end if
            end do
        end if
    end subroutine buttongroup_button_toggled

end module forge_radiobutton

