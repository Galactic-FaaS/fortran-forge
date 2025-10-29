!> @brief Basic Horizontal Layout Example
!> @details Demonstrates horizontal box layout for arranging widgets
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_layout_horizontal_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout
    type(forge_label) :: label1, label2, label3
    type(forge_button) :: button1, button2, button3
    type(forge_entry) :: entry1
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Horizontal Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating a horizontal box layout"
    print '(A)', "  - Adding widgets side by side"
    print '(A)', "  - Layout spacing and alignment"
    print '(A)', "  - Widget arrangement in horizontal order"
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
        call builder%set_title("Basic Horizontal Layout Example")
        call builder%set_size(500, 150)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create horizontal box layout
    print '(A)', "Creating horizontal layout..."
    call layout%set_orientation(0)  ! Horizontal
    call layout%set_spacing(15)
    call layout%set_margin(20)

    ! Create widgets
    print '(A)', "Creating widgets..."

    call label1%set_text("Name:")
    call label1%set_name("label_name")

    call entry1%set_placeholder_text("Enter name")
    call entry1%set_name("entry_name")

    call button1%set_label("OK")
    call button1%set_name("button_ok")

    call button2%set_label("Cancel")
    call button2%set_name("button_cancel")

    call button3%set_label("Help")
    call button3%set_name("button_help")

    ! Add widgets to layout (horizontal order)
    print '(A)', "Adding widgets to layout..."
    call layout%add_widget(label1)
    call layout%add_widget(entry1)
    call layout%add_widget(button1)
    call layout%add_widget(button2)
    call layout%add_widget(button3)

    print '(A,I0,A)', "  Layout contains ", layout%get_widget_count(), " widgets"
    print '(A,I0)', "  Spacing: ", layout%get_spacing()
    print '(A,I0)', "  Margin: ", layout%get_margin()

    ! Set layout on window
    call window%set_layout(layout)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with horizontal layout..."
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

end program basic_layout_horizontal_example