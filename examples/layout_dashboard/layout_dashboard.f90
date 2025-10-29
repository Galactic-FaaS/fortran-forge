!> @brief Dashboard Layout Example
!> @details Demonstrates a dashboard-style layout with multiple panels
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_dashboard_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: main_layout, top_layout, bottom_layout
    type(forge_grid_layout) :: center_grid
    type(forge_group_box) :: stats_group, chart_group, tasks_group, notifications_group
    type(forge_label) :: title_label, stats_content, chart_content, tasks_content, notifications_content
    type(forge_button) :: refresh_button, settings_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Dashboard Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Dashboard-style layout design"
    print '(A)', "  - Grid layout for panel arrangement"
    print '(A)', "  - Group boxes for panel organization"
    print '(A)', "  - Mixed layout types in complex UI"
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
        call builder%set_title("Dashboard Layout Example")
        call builder%set_size(800, 600)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create main vertical layout
    print '(A)', "Creating dashboard layout..."
    call main_layout%set_orientation(1)  ! Vertical
    call main_layout%set_spacing(10)
    call main_layout%set_margin(15)

    ! Title and toolbar
    call title_label%set_text("Application Dashboard")
    call title_label%set_name("title")

    call top_layout%set_orientation(0)  ! Horizontal
    call top_layout%set_spacing(10)

    call refresh_button%set_label("Refresh")
    call refresh_button%set_name("button_refresh")

    call settings_button%set_label("Settings")
    call settings_button%set_name("button_settings")

    call top_layout%add_widget(title_label)
    call top_layout%add_stretch()
    call top_layout%add_widget(refresh_button)
    call top_layout%add_widget(settings_button)

    ! Center grid layout for panels
    call center_grid%set_rows(2)
    call center_grid%set_columns(2)
    call center_grid%set_spacing(15)
    call center_grid%set_margin(10)

    ! Statistics panel
    call stats_group%set_title("Statistics")
    call stats_group%set_name("group_stats")

    call stats_content%set_text("Total Users: 1,234\nActive Sessions: 89\nRevenue: $12,345\nGrowth: +15%")
    call stats_content%set_name("content_stats")

    call stats_group%set_widget(stats_content)

    ! Chart panel
    call chart_group%set_title("Performance Chart")
    call chart_group%set_name("group_chart")

    call chart_content%set_text("[Chart Area]\n\nMonthly performance metrics\nwould be displayed here.\n\nLine charts, bar graphs,\nand trend analysis.")
    call chart_content%set_name("content_chart")

    call chart_group%set_widget(chart_content)

    ! Tasks panel
    call tasks_group%set_title("Recent Tasks")
    call tasks_group%set_name("group_tasks")

    call tasks_content%set_text("• Complete user onboarding\n• Review quarterly reports\n• Update system documentation\n• Schedule team meeting\n• Deploy version 2.1")
    call tasks_content%set_name("content_tasks")

    call tasks_group%set_widget(tasks_content)

    ! Notifications panel
    call notifications_group%set_title("Notifications")
    call notifications_group%set_name("group_notifications")

    call notifications_content%set_text("• Server maintenance: 2 hours\n• New user registrations: +12\n• System backup completed\n• Security update available\n• Database optimization done")
    call notifications_content%set_name("content_notifications")

    call notifications_group%set_widget(notifications_content)

    ! Add panels to grid (2x2 layout)
    call center_grid%add_widget_at(stats_group, 1, 1)
    call center_grid%add_widget_at(chart_group, 1, 2)
    call center_grid%add_widget_at(tasks_group, 2, 1)
    call center_grid%add_widget_at(notifications_group, 2, 2)

    ! Bottom status/layout info
    call bottom_layout%set_orientation(0)  ! Horizontal
    call bottom_layout%set_spacing(10)

    ! Assemble main layout
    call main_layout%add_layout(top_layout)
    call main_layout%add_layout(center_grid)
    call main_layout%add_layout(bottom_layout)

    ! Set main layout on window
    call window%set_layout(main_layout)

    print '(A)', "  Dashboard layout structure:"
    print '(A)', "    ├── Top Bar (Title + Buttons)"
    print '(A)', "    ├── Center Grid (2x2)"
    print '(A)', "    │     ├── Statistics Panel"
    print '(A)', "    │     ├── Performance Chart"
    print '(A)', "    │     ├── Recent Tasks"
    print '(A)', "    │     └── Notifications"
    print '(A)', "    └── Bottom Bar (Status)"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with dashboard layout..."
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

end program layout_dashboard_example