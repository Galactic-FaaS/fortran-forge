!> @brief Layout Alignment Example
!> @details Demonstrates widget alignment options in layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_alignment_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout1, layout2, layout3, layout4
    type(forge_label) :: title1, title2, title3, title4
    type(forge_button) :: button1, button2, button3, button4
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Layout Alignment Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Widget alignment in layouts"
    print '(A)', "  - Left, center, right alignment"
    print '(A)', "  - Top, middle, bottom alignment"
    print '(A)', "  - Alignment combinations"
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
        call builder%set_title("Layout Alignment Example")
        call builder%set_size(500, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create layouts with different alignments
    print '(A)', "Creating layouts with different alignments..."

    ! Layout 1: Left aligned
    call layout1%set_orientation(0)  ! Horizontal
    call layout1%set_spacing(10)
    call layout1%set_margin(10)

    call title1%set_text("Left Aligned")
    call title1%set_name("title_left")

    call button1%set_label("Left Button")
    call button1%set_name("button_left")
    call button1%set_size_policy(0, 0)  ! Fixed size

    call layout1%add_widget(title1)
    call layout1%add_widget(button1)
    call layout1%set_alignment(button1, 1)  ! Align to left

    ! Layout 2: Center aligned
    call layout2%set_orientation(0)  ! Horizontal
    call layout2%set_spacing(10)
    call layout2%set_margin(10)

    call title2%set_text("Center Aligned")
    call title2%set_name("title_center")

    call button2%set_label("Center Button")
    call button2%set_name("button_center")
    call button2%set_size_policy(0, 0)  ! Fixed size

    call layout2%add_widget(title2)
    call layout2%add_widget(button2)
    call layout2%set_alignment(button2, 4)  ! Align to center

    ! Layout 3: Right aligned
    call layout3%set_orientation(0)  ! Horizontal
    call layout3%set_spacing(10)
    call layout3%set_margin(10)

    call title3%set_text("Right Aligned")
    call title3%set_name("title_right")

    call button3%set_label("Right Button")
    call button3%set_name("button_right")
    call button3%set_size_policy(0, 0)  ! Fixed size

    call layout3%add_widget(title3)
    call layout3%add_widget(button3)
    call layout3%set_alignment(button3, 2)  ! Align to right

    ! Layout 4: Justified (fill space)
    call layout4%set_orientation(0)  ! Horizontal
    call layout4%set_spacing(10)
    call layout4%set_margin(10)

    call title4%set_text("Justified")
    call title4%set_name("title_justified")

    call button4%set_label("Justified Button")
    call button4%set_name("button_justified")
    call button4%set_size_policy(7, 0)  ! Expanding horizontally

    call layout4%add_widget(title4)
    call layout4%add_widget(button4)
    call layout4%set_alignment(button4, 8)  ! Justified

    print '(A)', "  Layout 1: Left alignment"
    print '(A)', "  Layout 2: Center alignment"
    print '(A)', "  Layout 3: Right alignment"
    print '(A)', "  Layout 4: Justified alignment"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with different alignments..."
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

end program layout_alignment_example