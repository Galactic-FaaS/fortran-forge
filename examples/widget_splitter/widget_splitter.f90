!> @brief Splitter Widget Example
!> @details Demonstrates splitter widgets for resizable panels
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_splitter_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_splitter) :: splitter
    type(forge_text_view) :: left_panel, right_panel
    type(forge_label) :: status_label
    type(forge_button) :: reset_sizes_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Splitter Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating splitter widgets"
    print '(A)', "  - Adding resizable panels"
    print '(A)', "  - Horizontal and vertical splitters"
    print '(A)', "  - Setting splitter sizes and proportions"
    print '(A)', "  - Collapsible splitter handles"
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
        call builder%set_title("Splitter Widget Example")
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

    ! Create horizontal splitter
    print '(A)', "Creating horizontal splitter..."
    call splitter%set_name("main_splitter")
    call splitter%set_orientation(0)  ! Horizontal
    call splitter%set_opaque_resize(.true.)
    call splitter%set_children_collapsible(.true.)

    ! Create left panel content
    print '(A)', "Creating left panel content..."
    call left_panel%set_name("left_text_view")
    call left_panel%set_editable(.true.)
    call left_panel%set_text("Left Panel Content\n\n" // &
                            "This is the left panel of a horizontal splitter.\n" // &
                            "You can resize this panel by dragging the splitter handle.\n\n" // &
                            "• Panel can be resized\n" // &
                            "• Content scrolls if needed\n" // &
                            "• Minimum sizes can be set")

    ! Create right panel content
    print '(A)', "Creating right panel content..."
    call right_panel%set_name("right_text_view")
    call right_panel%set_editable(.true.)
    call right_panel%set_text("Right Panel Content\n\n" // &
                             "This is the right panel of a horizontal splitter.\n" // &
                             "The splitter allows both panels to be resized dynamically.\n\n" // &
                             "• Symmetric resizing\n" // &
                             "• Proportional sizing\n" // &
                             "• Collapsible panels")

    ! Add panels to splitter
    call splitter%add_widget(left_panel)
    call splitter%add_widget(right_panel)

    ! Set initial sizes (40% left, 60% right)
    call splitter%set_sizes([240, 360])  ! Based on 600px total width

    ! Control button
    call reset_sizes_button%set_label("Reset Sizes")
    call reset_sizes_button%set_name("button_reset_sizes")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Drag splitter handle to resize panels")

    print '(A,I0,A)', "  Splitter has ", splitter%get_count(), " panels"
    print '(A,I0)', "  Orientation: ", splitter%get_orientation()
    print '(A,2(I0,A))', "  Panel sizes: ", splitter%get_sizes()(1), ", ", splitter%get_sizes()(2)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with splitter..."
    call window%show()

    ! Simulate splitter operations
    print '(A)', ""
    print '(A)', "Simulating splitter operations:"

    ! Resize panels
    call splitter%set_sizes([300, 300])  ! Equal sizes
    print '(A,2(I0,A))', "  Resized to equal: ", splitter%get_sizes()(1), ", ", splitter%get_sizes()(2)

    ! Make left panel larger
    call splitter%set_sizes([400, 200])
    print '(A,2(I0,A))', "  Made left panel larger: ", splitter%get_sizes()(1), ", ", splitter%get_sizes()(2)

    ! Reset to original proportions
    call splitter%set_sizes([240, 360])
    print '(A,2(I0,A))', "  Reset to original: ", splitter%get_sizes()(1), ", ", splitter%get_sizes()(2)

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

        write(status_text, '(A,I0,A,I0,A,F5.1,A)') &
            "Panel sizes: Left=", splitter%get_sizes()(1), &
            ", Right=", splitter%get_sizes()(2), &
            " (", real(splitter%get_sizes()(1)) / real(sum(splitter%get_sizes())) * 100.0, "%)"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_splitter_example