!> @brief Dock Widget Example
!> @details Demonstrates dock widgets for floating/dockable panels
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_dockwidget_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_dock_widget) :: dock1, dock2
    type(forge_label) :: content1, content2
    type(forge_button) :: toggle_dock1_button, toggle_dock2_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Dock Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating dock widget panels"
    print '(A)', "  - Docking and undocking widgets"
    print '(A)', "  - Dock widget areas (left, right, top, bottom)"
    print '(A)', "  - Floating dock widgets"
    print '(A)', "  - Dock widget features (close, float buttons)"
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
        call builder%set_title("Dock Widget Example")
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

    ! Create dock widgets
    print '(A)', "Creating dock widgets..."

    ! Left dock widget
    call dock1%set_name("dock_properties")
    call dock1%set_window_title("Properties")
    call dock1%set_allowed_areas(15)  ! All areas
    call dock1%set_features(7)        ! All features (dock, float, close)

    ! Right dock widget
    call dock2%set_name("dock_tools")
    call dock2%set_window_title("Tools")
    call dock2%set_allowed_areas(15)  ! All areas
    call dock2%set_features(7)        ! All features

    ! Create content for dock widgets
    print '(A)', "Creating dock content..."

    call content1%set_name("properties_content")
    call content1%set_text("Properties Panel\n\n" // &
                          "This dock widget can be:\n" // &
                          "• Docked to any edge\n" // &
                          "• Floated as separate window\n" // &
                          "• Closed and reopened\n\n" // &
                          "Dock widgets provide flexible\n" // &
                          "layout management.")

    call content2%set_name("tools_content")
    call content2%set_text("Tools Panel\n\n" // &
                          "Toolbox content here.\n\n" // &
                          "Multiple dock widgets can\n" // &
                          "be arranged around the\n" // &
                          "central widget area.\n\n" // &
                          "Try docking/undocking!")

    ! Set content in dock widgets
    call dock1%set_widget(content1)
    call dock2%set_widget(content2)

    ! Add dock widgets to main window
    call window%add_dock_widget(dock1, 1)  ! Left area
    call window%dock_widget_set_floating(dock1, .false.)

    call window%add_dock_widget(dock2, 2)  ! Right area
    call window%dock_widget_set_floating(dock2, .false.)

    ! Control buttons
    call toggle_dock1_button%set_label("Toggle Properties Dock")
    call toggle_dock1_button%set_name("button_toggle_dock1")

    call toggle_dock2_button%set_label("Toggle Tools Dock")
    call toggle_dock2_button%set_name("button_toggle_dock2")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Both dock widgets are currently docked")

    print '(A,A,A,I0)', "  Dock 1: ", trim(dock1%get_window_title()), ", area: ", window%dock_widget_area(dock1)
    print '(A,A,A,I0)', "  Dock 2: ", trim(dock2%get_window_title()), ", area: ", window%dock_widget_area(dock2)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with dock widgets..."
    call window%show()

    ! Simulate dock operations
    print '(A)', ""
    print '(A)', "Simulating dock operations:"

    ! Float dock widget 1
    call window%dock_widget_set_floating(dock1, .true.)
    print '(A,A)', "  Made ", trim(dock1%get_window_title()), " floating"

    ! Move dock widget 2 to top
    call window%add_dock_widget(dock2, 4)  ! Top area
    print '(A,A)', "  Moved ", trim(dock2%get_window_title()), " to top area"

    ! Hide dock widget 1
    call dock1%set_visible(.false.)
    print '(A,A)', "  Hid ", trim(dock1%get_window_title())

    ! Show dock widget 1 again
    call dock1%set_visible(.true.)
    print '(A,A)', "  Showed ", trim(dock1%get_window_title()), " again"

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
        character(len:100) :: status_text
        character(len:20) :: dock1_state, dock2_state

        if (dock1%is_visible()) then
            if (window%dock_widget_is_floating(dock1)) then
                dock1_state = "floating"
            else
                dock1_state = "docked"
            end if
        else
            dock1_state = "hidden"
        end if

        if (dock2%is_visible()) then
            if (window%dock_widget_is_floating(dock2)) then
                dock2_state = "floating"
            else
                dock2_state = "docked"
            end if
        else
            dock2_state = "hidden"
        end if

        write(status_text, '(A,A,A,A,A)') &
            "Properties: ", trim(dock1_state), &
            ", Tools: ", trim(dock2_state)
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_dockwidget_example