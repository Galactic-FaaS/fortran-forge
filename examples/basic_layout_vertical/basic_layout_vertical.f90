!> @brief Basic Vertical Layout Example
!> @details Demonstrates vertical box layout for arranging widgets
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_layout_vertical_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout
    type(forge_label) :: title_label, label1, label2, label3
    type(forge_button) :: button1, button2
    type(forge_entry) :: entry1
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Vertical Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating a vertical box layout"
    print '(A)', "  - Adding widgets to the layout"
    print '(A)', "  - Setting layout properties (spacing, margins)"
    print '(A)', "  - Widget arrangement in vertical order"
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
        call builder%set_title("Basic Vertical Layout Example")
        call builder%set_size(300, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create vertical box layout
    print '(A)', "Creating vertical layout..."
    call layout%set_orientation(1)  ! Vertical
    call layout%set_spacing(10)
    call layout%set_margin(20)

    ! Create widgets
    print '(A)', "Creating widgets..."

    call title_label%set_text("Vertical Layout Demo")
    call title_label%set_name("title")

    call label1%set_text("This is a label above the entry field.")
    call label1%set_name("label1")

    call entry1%set_placeholder_text("Enter some text here")
    call entry1%set_name("entry1")

    call label2%set_text("Buttons below:")
    call label2%set_name("label2")

    call button1%set_label("Button 1")
    call button1%set_name("button1")

    call button2%set_label("Button 2")
    call button2%set_name("button2")

    call label3%set_text("End of layout")
    call label3%set_name("label3")

    ! Add widgets to layout (vertical order)
    print '(A)', "Adding widgets to layout..."
    call layout%add_widget(title_label)
    call layout%add_widget(label1)
    call layout%add_widget(entry1)
    call layout%add_widget(label2)
    call layout%add_widget(button1)
    call layout%add_widget(button2)
    call layout%add_widget(label3)

    print '(A,I0,A)', "  Layout contains ", layout%get_widget_count(), " widgets"
    print '(A,I0)', "  Spacing: ", layout%get_spacing()
    print '(A,I0)', "  Margin: ", layout%get_margin()

    ! Set layout on window
    call window%set_layout(layout)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with vertical layout..."
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

end program basic_layout_vertical_example