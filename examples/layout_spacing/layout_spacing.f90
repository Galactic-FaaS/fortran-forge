!> @brief Layout Spacing Example
!> @details Demonstrates different spacing options in layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_spacing_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: layout1, layout2, layout3
    type(forge_label) :: title1, title2, title3
    type(forge_button) :: buttons1(5), buttons2(5), buttons3(5)
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i
    character(len=20) :: btn_text

    print '(A)', "=== Layout Spacing Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Different spacing values in layouts"
    print '(A)', "  - Visual impact of spacing"
    print '(A)', "  - Spacing consistency"
    print '(A)', "  - Layout readability"
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
        call builder%set_title("Layout Spacing Example")
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

    ! Create three layouts with different spacing
    print '(A)', "Creating layouts with different spacing..."

    ! Layout 1: Tight spacing (2px)
    call layout1%set_orientation(0)  ! Horizontal
    call layout1%set_spacing(2)
    call layout1%set_margin(10)

    call title1%set_text("Tight Spacing (2px)")
    call title1%set_name("title_tight")

    do i = 1, 5
        write(btn_text, '("Btn", I1)') i
        call buttons1(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_tight_", I1)') i
        call buttons1(i)%set_name(trim(btn_text))
        call layout1%add_widget(buttons1(i))
    end do

    ! Layout 2: Normal spacing (10px)
    call layout2%set_orientation(0)  ! Horizontal
    call layout2%set_spacing(10)
    call layout2%set_margin(10)

    call title2%set_text("Normal Spacing (10px)")
    call title2%set_name("title_normal")

    do i = 1, 5
        write(btn_text, '("Btn", I1)') i
        call buttons2(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_normal_", I1)') i
        call buttons2(i)%set_name(trim(btn_text))
        call layout2%add_widget(buttons2(i))
    end do

    ! Layout 3: Loose spacing (20px)
    call layout3%set_orientation(0)  ! Horizontal
    call layout3%set_spacing(20)
    call layout3%set_margin(10)

    call title3%set_text("Loose Spacing (20px)")
    call title3%set_name("title_loose")

    do i = 1, 5
        write(btn_text, '("Btn", I1)') i
        call buttons3(i)%set_label(trim(btn_text))
        write(btn_text, '("btn_loose_", I1)') i
        call buttons3(i)%set_name(trim(btn_text))
        call layout3%add_widget(buttons3(i))
    end do

    print '(A)', "  Layout 1 spacing: ", layout1%get_spacing()
    print '(A)', "  Layout 2 spacing: ", layout2%get_spacing()
    print '(A)', "  Layout 3 spacing: ", layout3%get_spacing()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with different spacing layouts..."
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

end program layout_spacing_example