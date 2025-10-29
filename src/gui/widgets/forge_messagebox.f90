!> @brief MessageBox dialog
!> @details Standard message dialogs (QMessageBox equivalent)
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_messagebox
    use forge_widgets
    use forge_window
    use forge_types
    use forge_string_utils
    use forge_signals
    implicit none
    private

    public :: QMessageBox, show_information, show_warning, show_error, show_question
    public :: MessageIcon, Information, Warning, Critical, Question
    public :: StandardButton, Ok, Cancel, Yes, No, Abort, Retry, Ignore

    !> Message icon types
    integer, parameter :: NoIcon = 0
    integer, parameter :: Information = 1
    integer, parameter :: Warning = 2
    integer, parameter :: Critical = 3
    integer, parameter :: Question = 4

    type :: MessageIcon
        integer :: value = Information
    end type MessageIcon

    !> Standard button types
    integer, parameter :: NoButton = 0
    integer, parameter :: Ok = 1
    integer, parameter :: Cancel = 2
    integer, parameter :: Yes = 4
    integer, parameter :: No = 8
    integer, parameter :: Abort = 16
    integer, parameter :: Retry = 32
    integer, parameter :: Ignore = 64

    type :: StandardButton
        integer :: value = Ok
    end type StandardButton

    !> @brief Message box dialog
    type, extends(forge_window_t) :: QMessageBox
        private
        type(QString) :: text
        type(QString) :: informative_text
        type(QString) :: detailed_text
        type(MessageIcon) :: icon
        integer :: standard_buttons = Ok
        integer :: default_button = Ok
        type(signal_int) :: button_clicked
    contains
        procedure :: set_text => messagebox_set_text
        procedure :: get_text => messagebox_get_text
        procedure :: set_informative_text => messagebox_set_informative
        procedure :: set_detailed_text => messagebox_set_detailed
        procedure :: set_icon => messagebox_set_icon
        procedure :: set_standard_buttons => messagebox_set_buttons
        procedure :: exec => messagebox_exec
    end type QMessageBox

contains

    subroutine messagebox_set_text(this, text)
        class(QMessageBox), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine messagebox_set_text

    function messagebox_get_text(this) result(text)
        class(QMessageBox), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function messagebox_get_text

    subroutine messagebox_set_informative(this, text)
        class(QMessageBox), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%informative_text%set(text)
    end subroutine messagebox_set_informative

    subroutine messagebox_set_detailed(this, text)
        class(QMessageBox), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%detailed_text%set(text)
    end subroutine messagebox_set_detailed

    subroutine messagebox_set_icon(this, icon)
        class(QMessageBox), intent(inout) :: this
        integer, intent(in) :: icon
        this%icon%value = icon
    end subroutine messagebox_set_icon

    subroutine messagebox_set_buttons(this, buttons)
        class(QMessageBox), intent(inout) :: this
        integer, intent(in) :: buttons
        this%standard_buttons = buttons
    end subroutine messagebox_set_buttons

    function messagebox_exec(this) result(button)
        class(QMessageBox), intent(inout) :: this
        integer :: button
        type(forge_event) :: event
        logical :: dialog_closed

        ! Show the dialog modally
        call this%show()

        ! Run modal event loop until dialog is closed
        dialog_closed = .false.
        do while (.not. dialog_closed)
            ! Process events (this would be handled by the backend)
            ! For now, simulate waiting for user input
            ! In a real implementation, this would block until a button is clicked

            ! Check for button click events
            ! This is a placeholder - real implementation would integrate with event system
            if (this%button_clicked%has_connections()) then
                ! Wait for button click signal
                ! For now, assume Ok is clicked
                button = Ok
                dialog_closed = .true.
            end if
        end do

        ! Hide the dialog
        call this%hide()
    end function messagebox_exec

    ! ========== Convenience Functions ==========

    function show_information(title, text, buttons) result(clicked)
        character(len=*), intent(in) :: title, text
        integer, intent(in), optional :: buttons
        integer :: clicked
        type(QMessageBox) :: box
        
        call box%set_title(title)
        call box%set_text(text)
        call box%set_icon(Information)
        if (present(buttons)) call box%set_standard_buttons(buttons)
        clicked = box%exec()
    end function show_information

    function show_warning(title, text, buttons) result(clicked)
        character(len=*), intent(in) :: title, text
        integer, intent(in), optional :: buttons
        integer :: clicked
        type(QMessageBox) :: box
        
        call box%set_title(title)
        call box%set_text(text)
        call box%set_icon(Warning)
        if (present(buttons)) call box%set_standard_buttons(buttons)
        clicked = box%exec()
    end function show_warning

    function show_error(title, text, buttons) result(clicked)
        character(len=*), intent(in) :: title, text
        integer, intent(in), optional :: buttons
        integer :: clicked
        type(QMessageBox) :: box
        
        call box%set_title(title)
        call box%set_text(text)
        call box%set_icon(Critical)
        if (present(buttons)) call box%set_standard_buttons(buttons)
        clicked = box%exec()
    end function show_error

    function show_question(title, text, buttons) result(clicked)
        character(len=*), intent(in) :: title, text
        integer, intent(in), optional :: buttons
        integer :: clicked
        type(QMessageBox) :: box
        
        call box%set_title(title)
        call box%set_text(text)
        call box%set_icon(Question)
        if (present(buttons)) then
            call box%set_standard_buttons(buttons)
        else
            call box%set_standard_buttons(ior(Yes, No))
        end if
        clicked = box%exec()
    end function show_question

end module forge_messagebox

