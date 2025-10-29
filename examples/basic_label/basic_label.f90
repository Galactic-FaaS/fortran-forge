!> @brief Basic Label Example
!> @details Demonstrates creating and using a simple label widget
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_label_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: label1, label2, label3
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Label Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating label widgets"
    print '(A)', "  - Setting label text and properties"
    print '(A)', "  - Different label styles"
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
        call builder%set_title("Basic Label Example")
        call builder%set_size(400, 300)
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
    print '(A)', "Creating labels..."

    ! Simple text label
    call label1%set_name("label_simple")
    call label1%set_text("Hello, ForGE!")
    print '(A,A)', "  Label 1: ", trim(label1%get_text())

    ! Multi-line label
    call label2%set_name("label_multiline")
    call label2%set_text("This is a multi-line\nlabel example.\nIt demonstrates\ntext wrapping.")
    print '(A,A)', "  Label 2: ", trim(label2%get_text())

    ! Label with markup (if supported)
    call label3%set_name("label_markup")
    call label3%set_text("<b>Bold Text</b> and <i>Italic Text</i>")
    print '(A,A)', "  Label 3: ", trim(label3%get_text())

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program basic_label_example