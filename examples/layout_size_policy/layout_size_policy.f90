!> @brief Size Policy Layout Example
!> @details Demonstrates widget size policies in layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_size_policy_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout
    type(forge_label) :: label1, label2, label3, label4
    type(forge_button) :: button1, button2, button3, button4
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Size Policy Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Widget size policies"
    print '(A)', "  - Fixed, minimum, preferred, expanding sizes"
    print '(A)', "  - How size policies affect layout"
    print '(A)', "  - Size policy combinations"
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
        call builder%set_title("Size Policy Layout Example")
        call builder%set_size(600, 200)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create horizontal layout
    print '(A)', "Creating horizontal layout with different size policies..."
    call layout%set_orientation(0)  ! Horizontal
    call layout%set_spacing(10)
    call layout%set_margin(20)

    ! Fixed size policy
    call label1%set_text("Fixed Size:")
    call label1%set_name("label_fixed")

    call button1%set_label("Fixed Button")
    call button1%set_name("button_fixed")
    call button1%set_size_policy(0, 0)  ! Fixed, Fixed
    call button1%set_minimum_size(100, 30)
    call button1%set_maximum_size(100, 30)

    ! Minimum size policy
    call label2%set_text("Minimum Size:")
    call label2%set_name("label_minimum")

    call button2%set_label("Minimum Button")
    call button2%set_name("button_minimum")
    call button2%set_size_policy(1, 0)  ! Minimum, Fixed
    call button2%set_minimum_size(80, 30)

    ! Preferred size policy
    call label3%set_text("Preferred Size:")
    call label3%set_name("label_preferred")

    call button3%set_label("Preferred Button")
    call button3%set_name("button_preferred")
    call button3%set_size_policy(4, 0)  ! Preferred, Fixed

    ! Expanding size policy
    call label4%set_text("Expanding Size:")
    call label4%set_name("label_expanding")

    call button4%set_label("Expanding Button")
    call button4%set_name("button_expanding")
    call button4%set_size_policy(7, 0)  ! Expanding, Fixed

    ! Add widgets to layout
    call layout%add_widget(label1)
    call layout%add_widget(button1)
    call layout%add_widget(label2)
    call layout%add_widget(button2)
    call layout%add_widget(label3)
    call layout%add_widget(button3)
    call layout%add_widget(label4)
    call layout%add_widget(button4)

    ! Set layout on window
    call window%set_layout(layout)

    print '(A)', "  Size policies demonstrated:"
    print '(A)', "    - Fixed: Button stays exactly 100x30 pixels"
    print '(A)', "    - Minimum: Button can grow from 80x30 minimum"
    print '(A)', "    - Preferred: Button uses preferred size, can shrink/grow"
    print '(A)', "    - Expanding: Button expands to fill available space"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with size policy layout..."
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

end program layout_size_policy_example