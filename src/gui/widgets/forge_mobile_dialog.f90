!> @brief Mobile-optimized dialog widgets
!> @details Action sheets, alerts, and modal dialogs for mobile
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_mobile_dialog
    use iso_c_binding
    use forge_types
    use forge_widgets
    use forge_dialogs
    implicit none
    private

    public :: forge_action_sheet, forge_alert_dialog
    public :: action_sheet_create, action_sheet_add_action, action_sheet_show
    public :: alert_dialog_create, alert_dialog_add_button, alert_dialog_show

    !> @brief Action sheet action
    type :: action_sheet_action
        character(len=64) :: title = ""
        integer :: style = 0  ! 0=default, 1=cancel, 2=destructive
        logical :: enabled = .true.
        procedure(action_callback_interface), pointer, nopass :: callback => null()
    end type action_sheet_action

    !> @brief Action sheet (iOS-style bottom sheet)
    type, extends(forge_widget) :: forge_action_sheet
        private
        character(len=128) :: title = ""
        type(action_sheet_action), allocatable :: actions(:)
        integer :: action_count = 0
    contains
        procedure :: add_action => action_sheet_add_action
        procedure :: show => action_sheet_show
        procedure :: dismiss => action_sheet_dismiss
    end type forge_action_sheet

    !> @brief Alert dialog button
    type :: alert_button
        character(len=32) :: title = ""
        integer :: style = 0  ! 0=default, 1=cancel, 2=destructive
        procedure(action_callback_interface), pointer, nopass :: callback => null()
    end type alert_button

    !> @brief Alert dialog
    type, extends(forge_widget) :: forge_alert_dialog
        private
        character(len=128) :: title = ""
        character(len=256) :: message = ""
        type(alert_button), allocatable :: buttons(:)
        integer :: button_count = 0
    contains
        procedure :: add_button => alert_dialog_add_button
        procedure :: show => alert_dialog_show
        procedure :: dismiss => alert_dialog_dismiss
    end type forge_alert_dialog

    !> @brief Action callback interface
    abstract interface
        subroutine action_callback_interface()
        end subroutine action_callback_interface
    end interface

contains

    !> @brief Create action sheet
    function action_sheet_create(title) result(sheet)
        character(len=*), intent(in) :: title
        type(forge_action_sheet) :: sheet

        ! Initialize base widget
        sheet%rect%x = 0
        sheet%rect%y = 0
        sheet%rect%width = 320  ! Default mobile width
        sheet%rect%height = 200  ! Will be adjusted based on content
        sheet%visible = .false.  ! Initially hidden
        sheet%enabled = .true.

        sheet%title = trim(title)
        sheet%action_count = 0
        allocate(sheet%actions(5))  ! Initial capacity
    end function action_sheet_create

    !> @brief Add action to action sheet
    subroutine action_sheet_add_action(this, title, style, callback)
        class(forge_action_sheet), intent(inout) :: this
        character(len=*), intent(in) :: title
        integer, intent(in) :: style
        procedure(action_callback_interface), optional :: callback

        if (this%action_count >= size(this%actions)) then
            call resize_action_array(this)
        end if

        this%action_count = this%action_count + 1
        this%actions(this%action_count)%title = trim(title)
        this%actions(this%action_count)%style = style
        this%actions(this%action_count)%enabled = .true.

        if (present(callback)) then
            this%actions(this%action_count)%callback => callback
        end if
    end subroutine action_sheet_add_action

    !> @brief Show action sheet
    subroutine action_sheet_show(this)
        class(forge_action_sheet), intent(inout) :: this

        this%visible = .true.
        ! Position at bottom of screen and animate up
        ! This would integrate with the platform's animation system
    end subroutine action_sheet_show

    !> @brief Dismiss action sheet
    subroutine action_sheet_dismiss(this)
        class(forge_action_sheet), intent(inout) :: this

        this%visible = .false.
        ! Animate down and remove from view hierarchy
    end subroutine action_sheet_dismiss

    !> @brief Create alert dialog
    function alert_dialog_create(title, message) result(dialog)
        character(len=*), intent(in) :: title
        character(len=*), intent(in), optional :: message
        type(forge_alert_dialog) :: dialog

        ! Initialize base widget
        dialog%rect%x = 50
        dialog%rect%y = 200
        dialog%rect%width = 280
        dialog%rect%height = 150  ! Will be adjusted based on content
        dialog%visible = .false.
        dialog%enabled = .true.

        dialog%title = trim(title)
        if (present(message)) then
            dialog%message = trim(message)
        end if
        dialog%button_count = 0
        allocate(dialog%buttons(3))  ! Initial capacity
    end function alert_dialog_create

    !> @brief Add button to alert dialog
    subroutine alert_dialog_add_button(this, title, style, callback)
        class(forge_alert_dialog), intent(inout) :: this
        character(len=*), intent(in) :: title
        integer, intent(in) :: style
        procedure(action_callback_interface), optional :: callback

        if (this%button_count >= size(this%buttons)) then
            call resize_button_array(this)
        end if

        this%button_count = this%button_count + 1
        this%buttons(this%button_count)%title = trim(title)
        this%buttons(this%button_count)%style = style

        if (present(callback)) then
            this%buttons(this%button_count)%callback => callback
        end if
    end subroutine alert_dialog_add_button

    !> @brief Show alert dialog
    subroutine alert_dialog_show(this)
        class(forge_alert_dialog), intent(inout) :: this

        this%visible = .true.
        ! Center on screen and show with modal presentation
    end subroutine alert_dialog_show

    !> @brief Dismiss alert dialog
    subroutine alert_dialog_dismiss(this)
        class(forge_alert_dialog), intent(inout) :: this

        this%visible = .false.
        ! Remove from view hierarchy
    end subroutine alert_dialog_dismiss

    !> @brief Resize action array
    subroutine resize_action_array(this)
        class(forge_action_sheet), intent(inout) :: this
        type(action_sheet_action), allocatable :: new_actions(:)
        integer :: new_size

        new_size = size(this%actions) * 2
        allocate(new_actions(new_size))
        new_actions(1:size(this%actions)) = this%actions

        deallocate(this%actions)
        this%actions = new_actions
    end subroutine resize_action_array

    !> @brief Resize button array
    subroutine resize_button_array(this)
        class(forge_alert_dialog), intent(inout) :: this
        type(alert_button), allocatable :: new_buttons(:)
        integer :: new_size

        new_size = size(this%buttons) * 2
        allocate(new_buttons(new_size))
        new_buttons(1:size(this%buttons)) = this%buttons

        deallocate(this%buttons)
        this%buttons = new_buttons
    end subroutine resize_button_array

end module forge_mobile_dialog