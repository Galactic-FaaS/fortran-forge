!> @brief Text Entry Event Example
!> @details Demonstrates handling text entry change events
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_entry_text_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_entry) :: name_entry, email_entry, password_entry
    type(forge_label) :: status_label, validation_label
    type(forge_button) :: validate_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: name_valid = .false., email_valid = .false., password_valid = .false.

    print '(A)', "=== Text Entry Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Text entry change events"
    print '(A)', "  - Real-time text validation"
    print '(A)', "  - Password field handling"
    print '(A)', "  - Input feedback and error states"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - events won't actually fire"
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
        call builder%set_title("Text Entry Events")
        call builder%set_size(500, 350)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create text entries
    print '(A)', "Creating text entries with change handlers..."

    ! Name entry
    call name_entry%set_name("name_entry")
    call name_entry%set_placeholder_text("Enter your full name")
    call name_entry%on_text_changed(on_name_changed)

    ! Email entry
    call email_entry%set_name("email_entry")
    call email_entry%set_placeholder_text("your.email@example.com")
    call email_entry%on_text_changed(on_email_changed)

    ! Password entry
    call password_entry%set_name("password_entry")
    call password_entry%set_placeholder_text("Enter password (min 8 chars)")
    call password_entry%set_password_mode(.true.)
    call password_entry%on_text_changed(on_password_changed)

    ! Control button
    call validate_button%set_label("Validate All")
    call validate_button%set_name("validate_button")
    call validate_button%on_click(on_validate_clicked)

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Fill in the fields to see validation events")

    call validation_label%set_name("validation_label")
    call validation_label%set_text("Validation: Name=?, Email=?, Password=?")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with event-handling text entries..."
    call window%show()

    ! Simulate text entry changes
    print '(A)', ""
    print '(A)', "Simulating text entry changes:"

    ! Enter name
    print '(A)', "  Typing name: 'John Doe'..."
    call name_entry%set_text("John Doe")
    call on_name_changed(create_test_event())

    ! Enter email
    print '(A)', "  Typing email: 'john.doe@example.com'..."
    call email_entry%set_text("john.doe@example.com")
    call on_email_changed(create_test_event())

    ! Enter password
    print '(A)', "  Typing password: 'secret123'..."
    call password_entry%set_text("secret123")
    call on_password_changed(create_test_event())

    ! Try invalid inputs
    print '(A)', "  Trying invalid email: 'invalid-email'..."
    call email_entry%set_text("invalid-email")
    call on_email_changed(create_test_event())

    print '(A)', "  Trying short password: '123'..."
    call password_entry%set_text("123")
    call on_password_changed(create_test_event())

    ! Fix inputs
    print '(A)', "  Fixing email: 'john.doe@example.com'..."
    call email_entry%set_text("john.doe@example.com")
    call on_email_changed(create_test_event())

    print '(A)', "  Fixing password: 'mysecretpassword'..."
    call password_entry%set_text("mysecretpassword")
    call on_password_changed(create_test_event())

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

    !> @brief Create a test event for simulation
    function create_test_event() result(event)
        type(forge_event) :: event
        ! In a real implementation, this would be created by the event system
        ! For simulation, we just return a dummy event
    end function create_test_event

    !> @brief Handler for name text change
    subroutine on_name_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        name_valid = len_trim(name_entry%get_text()) >= 2

        write(status_text, '(A,A,A,L1)') &
            "Name changed: '", trim(name_entry%get_text()), "', Valid: ", name_valid
        call status_label%set_text(trim(status_text))

        print '(A,A,L1)', "  → Name changed: ", trim(name_entry%get_text()), name_valid

        call update_validation()
    end subroutine on_name_changed

    !> @brief Handler for email text change
    subroutine on_email_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        email_valid = index(email_entry%get_text(), '@') > 0 .and. &
                     index(email_entry%get_text(), '.') > index(email_entry%get_text(), '@')

        write(status_text, '(A,A,A,L1)') &
            "Email changed: '", trim(email_entry%get_text()), "', Valid: ", email_valid
        call status_label%set_text(trim(status_text))

        print '(A,A,L1)', "  → Email changed: ", trim(email_entry%get_text()), email_valid

        call update_validation()
    end subroutine on_email_changed

    !> @brief Handler for password text change
    subroutine on_password_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        password_valid = len_trim(password_entry%get_text()) >= 8

        write(status_text, '(A,A,A,L1)') &
            "Password changed: [", trim(password_entry%get_text()), "], Valid: ", password_valid
        call status_label%set_text(trim(status_text))

        print '(A,I0,A,L1)', "  → Password length: ", len_trim(password_entry%get_text()), ", Valid: ", password_valid

        call update_validation()
    end subroutine on_password_changed

    !> @brief Handler for validate button click
    subroutine on_validate_clicked(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: result_text

        if (name_valid .and. email_valid .and. password_valid) then
            result_text = "✓ All fields are valid!"
        else
            result_text = "✗ Some fields are invalid. Please check and correct."
        end if

        call status_label%set_text(trim(result_text))
        print '(A,A)', "  → Validation result: ", trim(result_text)
    end subroutine on_validate_clicked

    !> @brief Update validation display
    subroutine update_validation()
        character(len:100) :: validation_text

        write(validation_text, '(A,L1,A,L1,A,L1)') &
            "Validation: Name=", name_valid, &
            ", Email=", email_valid, &
            ", Password=", password_valid
        call validation_label%set_text(trim(validation_text))
    end subroutine update_validation

end program event_entry_text_example