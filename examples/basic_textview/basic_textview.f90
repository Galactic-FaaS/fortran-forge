!> @brief Basic Text View Example
!> @details Demonstrates text view widgets for multi-line text display and editing
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_textview_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_text_view) :: textview1, textview2
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Text View Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating text view widgets"
    print '(A)', "  - Setting and getting text content"
    print '(A)', "  - Read-only vs editable text views"
    print '(A)', "  - Multi-line text handling"
    print '(A)', "  - Text formatting options"
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
        call builder%set_title("Basic Text View Example")
        call builder%set_size(600, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create text views
    print '(A)', "Creating text views..."

    ! Read-only text view with formatted content
    call textview1%set_name("textview_readonly")
    call textview1%set_editable(.false.)
    call textview1%set_text("This is a read-only text view.\n\n" // &
                          "It can display multiple lines of text,\n" // &
                          "and supports basic formatting.\n\n" // &
                          "Features:\n" // &
                          "• Multi-line display\n" // &
                          "• Scroll bars when needed\n" // &
                          "• Word wrapping\n" // &
                          "• Read-only mode")
    print '(A,I0,A)', "  Text View 1: ", textview1%get_line_count(), " lines, read-only"

    ! Editable text view
    call textview2%set_name("textview_editable")
    call textview2%set_editable(.true.)
    call textview2%set_text("This text view is editable.\n\nYou can type and modify the text here.\n\nTry adding your own content!")
    print '(A,I0,A)', "  Text View 2: ", textview2%get_line_count(), " lines, editable"

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Text views created - edit the bottom one to see changes")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate text editing
    print '(A)', ""
    print '(A)', "Simulating text editing:"

    call textview2%set_text(textview2%get_text() // "\n\nAdded this line programmatically!")
    print '(A,I0,A)', "  Text View 2 now has ", textview2%get_line_count(), " lines"

    ! Demonstrate text operations
    print '(A)', ""
    print '(A)', "Text operations:"
    print '(A,A)', "  Read-only text length: ", trim(int_to_string(textview1%get_text_length()))
    print '(A,A)', "  Editable text length: ", trim(int_to_string(textview2%get_text_length()))

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

        write(status_text, '(A,I0,A,I0,A)') &
            "Status: Read-only has ", textview1%get_line_count(), &
            " lines, Editable has ", textview2%get_line_count(), " lines"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

    function int_to_string(val) result(str)
        integer, intent(in) :: val
        character(len=20) :: str
        write(str, '(I0)') val
        str = trim(adjustl(str))
    end function int_to_string

end program basic_textview_example