!> @brief Tab Widget Example
!> @details Demonstrates tab widgets for organizing content in tabs
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_tabwidget_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_tab_widget) :: tabwidget
    type(forge_label) :: label1, label2, label3
    type(forge_button) :: button1, button2, button3
    type(forge_entry) :: entry1, entry2
    type(forge_checkbox) :: check1
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Tab Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating tab widgets"
    print '(A)', "  - Adding tabs with different content"
    print '(A)', "  - Tab navigation and selection"
    print '(A)', "  - Setting tab icons and labels"
    print '(A)', "  - Closable and movable tabs"
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
        call builder%set_title("Tab Widget Example")
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

    ! Create tab widget
    print '(A)', "Creating tab widget..."
    call tabwidget%set_name("main_tabs")
    call tabwidget%set_tabs_closable(.true.)
    call tabwidget%set_tabs_movable(.true.)

    ! Create content for Tab 1: General Settings
    print '(A)', "Creating tab content..."

    call label1%set_text("General Application Settings")
    call label1%set_name("label_general")

    call entry1%set_placeholder_text("Application name")
    call entry1%set_text("My ForGE App")
    call entry1%set_name("entry_appname")

    call check1%set_label("Enable auto-save")
    call check1%set_name("check_autosave")
    call check1%set_checked(.true.)

    ! Add Tab 1
    call tabwidget%add_tab("General", label1)
    call tabwidget%add_tab("General", entry1)
    call tabwidget%add_tab("General", check1)

    ! Create content for Tab 2: Appearance
    call label2%set_text("Appearance Settings")
    call label2%set_name("label_appearance")

    call button1%set_label("Choose Font")
    call button1%set_name("button_font")

    call button2%set_label("Choose Theme")
    call button2%set_name("button_theme")

    ! Add Tab 2
    call tabwidget%add_tab("Appearance", label2)
    call tabwidget%add_tab("Appearance", button1)
    call tabwidget%add_tab("Appearance", button2)

    ! Create content for Tab 3: Advanced
    call label3%set_text("Advanced Configuration")
    call label3%set_name("label_advanced")

    call entry2%set_placeholder_text("Configuration file path")
    call entry2%set_text("/etc/myapp/config.ini")
    call entry2%set_name("entry_config")

    call button3%set_label("Reset to Defaults")
    call button3%set_name("button_reset")

    ! Add Tab 3
    call tabwidget%add_tab("Advanced", label3)
    call tabwidget%add_tab("Advanced", entry2)
    call tabwidget%add_tab("Advanced", button3)

    print '(A,I0,A)', "  Tab widget has ", tabwidget%get_tab_count(), " tabs"
    print '(A,I0)', "  Current tab index: ", tabwidget%get_current_index()
    print '(A,A)', "  Current tab text: ", trim(tabwidget%get_tab_text(tabwidget%get_current_index()))

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with tab widget..."
    call window%show()

    ! Simulate tab navigation
    print '(A)', ""
    print '(A)', "Simulating tab navigation:"

    call tabwidget%set_current_index(2)  ! Switch to tab 2 (0-based)
    print '(A,A)', "  Switched to tab: ", trim(tabwidget%get_tab_text(tabwidget%get_current_index()))

    call tabwidget%set_current_index(3)  ! Switch to tab 3
    print '(A,A)', "  Switched to tab: ", trim(tabwidget%get_tab_text(tabwidget%get_current_index()))

    ! Simulate content changes
    call entry1%set_text("Updated App Name")
    print '(A)', "  Updated application name in General tab"

    call check1%set_checked(.false.)
    print '(A)', "  Disabled auto-save in General tab"

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program widget_tabwidget_example