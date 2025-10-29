!> @brief Layout Margins Example
!> @details Demonstrates different margin settings in layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_margins_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout1, layout2, layout3
    type(forge_label) :: title1, title2, title3
    type(forge_button) :: buttons1(3), buttons2(3), buttons3(3)
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i
    character(len=20) :: btn_text

    print '(A)', "=== Layout Margins Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Different margin values in layouts"
    print '(A)', "  - Visual spacing from layout edges"
    print '(A)', "  - Margin impact on layout appearance"
    print '(A)', "  - Consistent margin usage"
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
        call builder%set_title("Layout Margins Example")
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

    ! Create three layouts with different margins
    print '(A)', "Creating layouts with different margins..."

    ! Layout 1: Small margins (5px)
    call layout1%set_orientation(0)  ! Horizontal
    call layout1%set_spacing(10)
    call layout1%set_margin(5)

    call title1%set_text("Small Margins (5px)")
    call title1%set_name("title_small")

    do i = 1, 3
        write(btn_text, '("Btn", I1)') i
        call buttons1(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_small_", I1)') i
        call buttons1(i)%set_name(trim(btn_text))
        call layout1%add_widget(buttons1(i))
    end do

    ! Layout 2: Medium margins (15px)
    call layout2%set_orientation(0)  ! Horizontal
    call layout2%set_spacing(10)
    call layout2%set_margin(15)

    call title2%set_text("Medium Margins (15px)")
    call title2%set_name("title_medium")

    do i = 1, 3
        write(btn_text, '("Btn", I1)') i
        call buttons2(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_medium_", I1)') i
        call buttons2(i)%set_name(trim(btn_text))
        call layout2%add_widget(buttons2(i))
    end do

    ! Layout 3: Large margins (30px)
    call layout3%set_orientation(0)  ! Horizontal
    call layout3%set_spacing(10)
    call layout3%set_margin(30)

    call title3%set_text("Large Margins (30px)")
    call title3%set_name("title_large")

    do i = 1, 3
        write(btn_text, '("Btn", I1)') i
        call buttons3(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_large_", I1)') i
        call buttons3(i)%set_name(trim(btn_text))
        call layout3%add_widget(buttons3(i))
    end do

    print '(A)', "  Layout 1 margin: ", layout1%get_margin()
    print '(A)', "  Layout 2 margin: ", layout2%get_margin()
    print '(A)', "  Layout 3 margin: ", layout3%get_margin()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with different margin layouts..."
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

end program layout_margins_example