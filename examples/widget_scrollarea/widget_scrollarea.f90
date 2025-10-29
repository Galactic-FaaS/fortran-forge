!> @brief Scroll Area Widget Example
!> @details Demonstrates scroll area widgets for scrolling content
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_scrollarea_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_scroll_area) :: scroll_area
    type(forge_label) :: content_label(20)
    type(forge_button) :: scroll_to_top_button, scroll_to_bottom_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i
    character(len=50) :: label_text

    print '(A)', "=== Scroll Area Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating scroll area widgets"
    print '(A)', "  - Adding scrollable content"
    print '(A)', "  - Horizontal and vertical scroll bars"
    print '(A)', "  - Programmatic scrolling"
    print '(A)', "  - Scroll area policies"
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
        call builder%set_title("Scroll Area Widget Example")
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

    ! Create scroll area
    print '(A)', "Creating scroll area..."
    call scroll_area%set_name("main_scroll_area")
    call scroll_area%set_widget_resizable(.true.)
    call scroll_area%set_horizontal_scroll_bar_policy(1)  ! As needed
    call scroll_area%set_vertical_scroll_bar_policy(1)    ! As needed

    ! Create content labels (more than can fit in viewport)
    print '(A)', "Creating scrollable content..."
    do i = 1, 20
        write(label_text, '("Content Line ", I2, ": This is a long line of text that demonstrates horizontal scrolling capability in the scroll area widget.")') i
        call content_label(i)%set_text(trim(label_text))
        write(label_text, '("content_label_", I2)') i
        call content_label(i)%set_name(trim(label_text))

        ! Add to scroll area
        call scroll_area%set_widget(content_label(i))
    end do

    ! Control buttons
    call scroll_to_top_button%set_label("Scroll to Top")
    call scroll_to_top_button%set_name("button_scroll_top")

    call scroll_to_bottom_button%set_label("Scroll to Bottom")
    call scroll_to_bottom_button%set_name("button_scroll_bottom")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Use scroll bars or buttons to navigate content")

    print '(A,I0,A)', "  Scroll area contains ", scroll_area%get_widget_count(), " content items"
    print '(A,I0,A,I0)', "  Scroll bar policies: H=", scroll_area%get_horizontal_scroll_bar_policy(), ", V=", scroll_area%get_vertical_scroll_bar_policy()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with scroll area..."
    call window%show()

    ! Simulate scrolling operations
    print '(A)', ""
    print '(A)', "Simulating scrolling operations:"

    ! Scroll to top
    call scroll_area%scroll_to_top()
    print '(A)', "  Scrolled to top"

    ! Scroll to specific position
    call scroll_area%vertical_scroll_bar_set_value(50)
    print '(A,I0)', "  Vertical scroll bar set to: ", scroll_area%vertical_scroll_bar_get_value()

    ! Scroll to bottom
    call scroll_area%scroll_to_bottom()
    print '(A)', "  Scrolled to bottom"

    ! Ensure widget is visible
    call scroll_area%ensure_widget_visible(content_label(10))
    print '(A)', "  Ensured content line 10 is visible"

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
            "Scroll position: V=", scroll_area%vertical_scroll_bar_get_value(), &
            ", H=", scroll_area%horizontal_scroll_bar_get_value(), &
            " (0-100 range)"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_scrollarea_example