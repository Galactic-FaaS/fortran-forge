!> @brief Group Box Widget Example
!> @details Demonstrates group box widgets for organizing related controls
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_groupbox_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_group_box) :: group1, group2
    type(forge_checkbox) :: check1, check2, check3
    type(forge_radiobutton) :: radio1, radio2, radio3, radio4
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Group Box Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating group box widgets"
    print '(A)', "  - Adding controls to group boxes"
    print '(A)', "  - Group box titles and borders"
    print '(A)', "  - Organizing related widgets"
    print '(A)', "  - Checkable group boxes"
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
        call builder%set_title("Group Box Widget Example")
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

    ! Create group boxes
    print '(A)', "Creating group boxes..."

    ! Options group box
    call group1%set_title("Display Options")
    call group1%set_name("group_options")
    call group1%set_checkable(.false.)

    ! Add checkboxes to first group
    call check1%set_label("Show toolbar")
    call check1%set_name("check_toolbar")
    call check1%set_checked(.true.)
    call group1%add_widget(check1)

    call check2%set_label("Show status bar")
    call check2%set_name("check_statusbar")
    call check2%set_checked(.true.)
    call group1%add_widget(check2)

    call check3%set_label("Show grid")
    call check3%set_name("check_grid")
    call check3%set_checked(.false.)
    call group1%add_widget(check3)

    ! Theme selection group box
    call group2%set_title("Theme Selection")
    call group2%set_name("group_theme")
    call group2%set_checkable(.true.)
    call group2%set_checked(.true.)

    ! Add radio buttons to second group
    call radio1%set_label("Light theme")
    call radio1%set_name("radio_light")
    call radio1%set_group_id(1)
    call radio1%set_checked(.true.)
    call group2%add_widget(radio1)

    call radio2%set_label("Dark theme")
    call radio2%set_name("radio_dark")
    call radio2%set_group_id(1)
    call radio2%set_checked(.false.)
    call group2%add_widget(radio2)

    call radio3%set_label("System theme")
    call radio3%set_name("radio_system")
    call radio3%set_group_id(1)
    call radio3%set_checked(.false.)
    call group2%add_widget(radio3)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Group boxes organize related controls")

    print '(A,A)', "  Group 1 title: ", trim(group1%get_title())
    print '(A,I0,A)', "  Group 1 contains ", group1%get_widget_count(), " widgets"
    print '(A,A)', "  Group 2 title: ", trim(group2%get_title())
    print '(A,I0,A)', "  Group 2 contains ", group2%get_widget_count(), " widgets"
    print '(A,L1)', "  Group 2 is checkable: ", group2%is_checkable()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with group boxes..."
    call window%show()

    ! Simulate interactions
    print '(A)', ""
    print '(A)', "Simulating user interactions:"

    call check3%set_checked(.true.)
    print '(A)', "  Checked 'Show grid' option"

    call radio2%set_checked(.true.)
    print '(A)', "  Selected 'Dark theme'"

    call group2%set_checked(.false.)
    print '(A)', "  Unchecked theme group box (disables all theme options)"

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
        character(len=20) :: theme

        if (.not. group2%is_checked()) then
            theme = "Disabled"
        else if (radio1%is_checked()) then
            theme = "Light"
        else if (radio2%is_checked()) then
            theme = "Dark"
        else if (radio3%is_checked()) then
            theme = "System"
        else
            theme = "Unknown"
        end if

        write(status_text, '(A,L1,A,L1,A,L1,A,A)') &
            "Toolbar: ", check1%is_checked(), &
            ", Status: ", check2%is_checked(), &
            ", Grid: ", check3%is_checked(), &
            ", Theme: ", trim(theme)
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_groupbox_example