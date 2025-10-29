!> @brief System Tray Example
!> @details Demonstrates system tray icons and notifications
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_systemtray_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_system_tray_icon) :: tray_icon
    type(forge_menu) :: tray_menu
    type(forge_action) :: show_action, hide_action, quit_action
    type(forge_button) :: show_notification_button, change_icon_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== System Tray Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating system tray icons"
    print '(A)', "  - Tray icon menus and actions"
    print '(A)', "  - System notifications"
    print '(A)', "  - Minimizing to tray"
    print '(A)', "  - Tray icon tooltips"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - tray icons won't actually appear"
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
        call builder%set_title("System Tray Example")
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

    ! Create system tray icon
    print '(A)', "Creating system tray icon..."
    call tray_icon%set_name("tray_icon")
    call tray_icon%set_tool_tip("ForGE Application - Click to show menu")
    call tray_icon%set_visible(.true.)

    ! Create tray menu
    print '(A)', "Creating tray menu..."
    call tray_menu%set_title("Tray Menu")
    call tray_menu%set_name("tray_menu")

    ! Tray menu actions
    call show_action%set_text("Show Window")
    call show_action%set_name("action_show")

    call hide_action%set_text("Hide Window")
    call hide_action%set_name("action_hide")

    call quit_action%set_text("Quit")
    call quit_action%set_name("action_quit")

    ! Add actions to tray menu
    call tray_menu%add_action(show_action)
    call tray_menu%add_separator()
    call tray_menu%add_action(hide_action)
    call tray_menu%add_separator()
    call tray_menu%add_action(quit_action)

    ! Set menu on tray icon
    call tray_icon%set_context_menu(tray_menu)

    ! Control buttons
    call show_notification_button%set_label("Show Notification")
    call show_notification_button%set_name("button_show_notification")

    call change_icon_button%set_label("Change Tray Icon")
    call change_icon_button%set_name("button_change_icon")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("System tray icon is active - check your system tray")

    print '(A,A)', "  Tray icon tooltip: ", trim(tray_icon%get_tool_tip())
    print '(A,L1)', "  Tray icon visible: ", tray_icon%is_visible()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with system tray..."
    call window%show()

    ! Simulate tray operations
    print '(A)', ""
    print '(A)', "Simulating tray operations:"

    ! Show notification
    call tray_icon%show_message("ForGE Application", &
                               "This is a system tray notification!", &
                               1)  ! Information type
    print '(A)', "  Showed notification: 'This is a system tray notification!'"

    ! Change tooltip
    call tray_icon%set_tool_tip("ForGE Application - Updated tooltip")
    print '(A,A)', "  Updated tooltip to: ", trim(tray_icon%get_tool_tip())

    ! Simulate window hide/show via tray
    print '(A)', "  User clicked 'Hide Window' from tray menu"
    call window%set_visible(.false.)
    print '(A)', "  Window hidden"

    print '(A)', "  User clicked 'Show Window' from tray menu"
    call window%set_visible(.true.)
    print '(A)', "  Window shown again"

    ! Another notification
    call tray_icon%show_message("Application Update", &
                               "A new version is available for download.", &
                               2)  ! Warning type
    print '(A)', "  Showed warning notification: 'A new version is available'"

    ! Update status
    call update_status()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call tray_icon%set_visible(.false.)
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    subroutine update_status()
        character(len:100) :: status_text

        write(status_text, '(A,L1)') &
            "Tray icon visible: ", tray_icon%is_visible()
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_systemtray_example