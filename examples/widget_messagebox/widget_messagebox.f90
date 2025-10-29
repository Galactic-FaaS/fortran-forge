!> @brief Message Box Widget Example
!> @details Demonstrates message box dialogs for user notifications
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_messagebox_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_message_box) :: msgbox
    type(forge_button) :: info_button, warning_button, error_button, question_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Message Box Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating message box dialogs"
    print '(A)', "  - Different message types (info, warning, error, question)"
    print '(A)', "  - Custom message text and titles"
    print '(A)', "  - Standard buttons (OK, Cancel, Yes, No)"
    print '(A)', "  - Modal vs modeless dialogs"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - dialogs won't actually appear"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Message Box Widget Example")
        call builder%set_size(500, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create buttons to trigger different message boxes
    print '(A)', "Creating message box trigger buttons..."

    call info_button%set_label("Show Info Message")
    call info_button%set_name("button_info")

    call warning_button%set_label("Show Warning Message")
    call warning_button%set_name("button_warning")

    call error_button%set_label("Show Error Message")
    call error_button%set_name("button_error")

    call question_button%set_label("Show Question Message")
    call question_button%set_name("button_question")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Click buttons above to show different message boxes")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with message box buttons..."
    call window%show()

    ! Simulate showing different message boxes
    print '(A)', ""
    print '(A)', "Simulating message box displays:"

    ! Info message box
    print '(A)', ""
    print '(A)', "Showing Information Message Box:"
    call msgbox%set_icon(1)  ! Information icon
    call msgbox%set_window_title("Information")
    call msgbox%set_text("This is an informational message.")
    call msgbox%set_informative_text("Additional details about the information.")
    call msgbox%set_standard_buttons(1)  ! OK button
    print '(A,A)', "  Title: ", trim(msgbox%get_window_title())
    print '(A,A)', "  Text: ", trim(msgbox%get_text())
    print '(A,A)', "  Icon: Information"
    print '(A)', "  Buttons: OK"

    ! Warning message box
    print '(A)', ""
    print '(A)', "Showing Warning Message Box:"
    call msgbox%set_icon(2)  ! Warning icon
    call msgbox%set_window_title("Warning")
    call msgbox%set_text("This is a warning message.")
    call msgbox%set_informative_text("Something might go wrong if you continue.")
    call msgbox%set_standard_buttons(3)  ! OK | Cancel
    print '(A,A)', "  Title: ", trim(msgbox%get_window_title())
    print '(A,A)', "  Text: ", trim(msgbox%get_text())
    print '(A,A)', "  Icon: Warning"
    print '(A)', "  Buttons: OK, Cancel"

    ! Error message box
    print '(A)', ""
    print '(A)', "Showing Error Message Box:"
    call msgbox%set_icon(3)  ! Critical icon
    call msgbox%set_window_title("Error")
    call msgbox%set_text("An error has occurred.")
    call msgbox%set_informative_text("The operation could not be completed.")
    call msgbox%set_detailed_text("Technical details: File not found at /path/to/file")
    call msgbox%set_standard_buttons(1)  ! OK button
    print '(A,A)', "  Title: ", trim(msgbox%get_window_title())
    print '(A,A)', "  Text: ", trim(msgbox%get_text())
    print '(A,A)', "  Icon: Critical"
    print '(A)', "  Buttons: OK"

    ! Question message box
    print '(A)', ""
    print '(A)', "Showing Question Message Box:"
    call msgbox%set_icon(4)  ! Question icon
    call msgbox%set_window_title("Confirm Action")
    call msgbox%set_text("Do you want to save your changes?")
    call msgbox%set_informative_text("Unsaved changes will be lost.")
    call msgbox%set_standard_buttons(6)  ! Yes | No
    print '(A,A)', "  Title: ", trim(msgbox%get_window_title())
    print '(A,A)', "  Text: ", trim(msgbox%get_text())
    print '(A,A)', "  Icon: Question"
    print '(A)', "  Buttons: Yes, No"

    ! Simulate user responses
    print '(A)', ""
    print '(A)', "Simulating user responses:"
    print '(A)', "  Info message: User clicked OK"
    print '(A)', "  Warning message: User clicked Cancel"
    print '(A)', "  Error message: User clicked OK"
    print '(A)', "  Question message: User clicked Yes"

    ! Update status
    call update_status()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    subroutine update_status()
        character(len=100) :: status_text

        write(status_text, '(A)') &
            "Message boxes demonstrated: Info, Warning, Error, Question"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_messagebox_example