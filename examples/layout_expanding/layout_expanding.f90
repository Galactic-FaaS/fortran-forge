!> @brief Expanding Layout Example
!> @details Demonstrates expanding widgets in layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_expanding_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout
    type(forge_label) :: fixed_label1, expanding_label, fixed_label2
    type(forge_button) :: fixed_button1, expanding_button, fixed_button2
    type(forge_entry) :: expanding_entry
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Expanding Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Fixed vs expanding widgets"
    print '(A)', "  - How expanding widgets fill space"
    print '(A)', "  - Size policies for expansion"
    print '(A)', "  - Layout space distribution"
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
        call builder%set_title("Expanding Layout Example")
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
    print '(A)', "Creating horizontal layout with expanding widgets..."
    call layout%set_orientation(0)  ! Horizontal
    call layout%set_spacing(10)
    call layout%set_margin(20)

    ! Fixed size widgets
    call fixed_label1%set_text("Fixed:")
    call fixed_label1%set_name("label_fixed1")
    call fixed_label1%set_size_policy(0, 0)  ! Fixed

    call fixed_button1%set_label("Fixed Button")
    call fixed_button1%set_name("button_fixed1")
    call fixed_button1%set_size_policy(0, 0)  ! Fixed

    ! Expanding widgets
    call expanding_label%set_text("Expanding Label (takes available space)")
    call expanding_label%set_name("label_expanding")
    call expanding_label%set_size_policy(7, 0)  ! Expanding horizontally

    call expanding_button%set_label("Expanding Button")
    call expanding_button%set_name("button_expanding")
    call expanding_button%set_size_policy(7, 0)  ! Expanding horizontally

    call expanding_entry%set_placeholder_text("This entry expands to fill space")
    call expanding_entry%set_name("entry_expanding")
    call expanding_entry%set_size_policy(7, 0)  ! Expanding horizontally

    ! More fixed widgets
    call fixed_label2%set_text("Fixed:")
    call fixed_label2%set_name("label_fixed2")
    call fixed_label2%set_size_policy(0, 0)  ! Fixed

    call fixed_button2%set_label("Fixed Button")
    call fixed_button2%set_name("button_fixed2")
    call fixed_button2%set_size_policy(0, 0)  ! Fixed

    ! Add widgets to layout
    call layout%add_widget(fixed_label1)
    call layout%add_widget(fixed_button1)
    call layout%add_widget(expanding_label)
    call layout%add_widget(expanding_button)
    call layout%add_widget(expanding_entry)
    call layout%add_widget(fixed_label2)
    call layout%add_widget(fixed_button2)

    ! Set layout on window
    call window%set_layout(layout)

    print '(A)', "  Layout contains:"
    print '(A)', "    - Fixed label and button (left side)"
    print '(A)', "    - Expanding label, button, and entry (middle - fills space)"
    print '(A)', "    - Fixed label and button (right side)"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with expanding layout..."
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

end program layout_expanding_example