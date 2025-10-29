!> @brief Basic Entry Widget Example
!> @details Demonstrates text entry widgets and input handling
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_entry_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_entry) :: entry1, entry2, entry3
    type(forge_label) :: label1, label2, label3
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Entry Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating text entry widgets"
    print '(A)', "  - Setting placeholder text"
    print '(A)', "  - Password entry fields"
    print '(A)', "  - Input validation"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - no actual GUI will appear"
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
        call builder%set_title("Basic Entry Example")
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

    ! Create labels
    call label1%set_text("Name:")
    call label2%set_text("Email:")
    call label3%set_text("Password:")

    ! Create entry widgets
    print '(A)', "Creating entry widgets..."

    ! Regular text entry
    call entry1%set_name("entry_name")
    call entry1%set_placeholder_text("Enter your name")
    call entry1%set_text("John Doe")
    print '(A,A)', "  Entry 1 (Name): ", trim(entry1%get_text())

    ! Email entry with validation
    call entry2%set_name("entry_email")
    call entry2%set_placeholder_text("Enter your email")
    call entry2%set_text("john@example.com")
    print '(A,A)', "  Entry 2 (Email): ", trim(entry2%get_text())

    ! Password entry (masked)
    call entry3%set_name("entry_password")
    call entry3%set_placeholder_text("Enter password")
    call entry3%set_password_mode(.true.)
    call entry3%set_text("secret123")
    print '(A,A)', "  Entry 3 (Password): ", trim(entry3%get_text()), " (masked)"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate input changes
    print '(A)', ""
    print '(A)', "Simulating user input:"
    call entry1%set_text("Jane Smith")
    print '(A,A)', "  Name changed to: ", trim(entry1%get_text())

    call entry2%set_text("jane.smith@company.com")
    print '(A,A)', "  Email changed to: ", trim(entry2%get_text())

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program basic_entry_example