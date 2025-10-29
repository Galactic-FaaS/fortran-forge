!> @brief Menu Bar Widget Example
!> @details Demonstrates menu bar widgets with menus and actions
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_menubar_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_menu_bar) :: menubar
    type(forge_menu) :: file_menu, edit_menu, view_menu, help_menu
    type(forge_action) :: new_action, open_action, save_action, exit_action
    type(forge_action) :: cut_action, copy_action, paste_action
    type(forge_action) :: zoom_in_action, zoom_out_action
    type(forge_action) :: about_action
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Menu Bar Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating menu bar widgets"
    print '(A)', "  - Adding menus and menu items"
    print '(A)', "  - Keyboard shortcuts and accelerators"
    print '(A)', "  - Menu separators"
    print '(A)', "  - Submenus and context menus"
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
        call builder%set_title("Menu Bar Widget Example")
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

    ! Create menu bar
    print '(A)', "Creating menu bar..."
    call menubar%set_name("main_menubar")

    ! Create File menu
    print '(A)', "Creating File menu..."
    call file_menu%set_title("&File")
    call file_menu%set_name("menu_file")

    ! File menu actions
    call new_action%set_text("&New")
    call new_action%set_shortcut("Ctrl+N")
    call new_action%set_name("action_new")

    call open_action%set_text("&Open...")
    call open_action%set_shortcut("Ctrl+O")
    call open_action%set_name("action_open")

    call save_action%set_text("&Save")
    call save_action%set_shortcut("Ctrl+S")
    call save_action%set_name("action_save")

    call exit_action%set_text("E&xit")
    call exit_action%set_shortcut("Ctrl+Q")
    call exit_action%set_name("action_exit")

    ! Add actions to File menu
    call file_menu%add_action(new_action)
    call file_menu%add_action(open_action)
    call file_menu%add_separator()
    call file_menu%add_action(save_action)
    call file_menu%add_separator()
    call file_menu%add_action(exit_action)

    ! Create Edit menu
    print '(A)', "Creating Edit menu..."
    call edit_menu%set_title("&Edit")
    call edit_menu%set_name("menu_edit")

    ! Edit menu actions
    call cut_action%set_text("Cu&t")
    call cut_action%set_shortcut("Ctrl+X")
    call cut_action%set_name("action_cut")

    call copy_action%set_text("&Copy")
    call copy_action%set_shortcut("Ctrl+C")
    call copy_action%set_name("action_copy")

    call paste_action%set_text("&Paste")
    call paste_action%set_shortcut("Ctrl+V")
    call paste_action%set_name("action_paste")

    ! Add actions to Edit menu
    call edit_menu%add_action(cut_action)
    call edit_menu%add_action(copy_action)
    call edit_menu%add_action(paste_action)

    ! Create View menu
    print '(A)', "Creating View menu..."
    call view_menu%set_title("&View")
    call view_menu%set_name("menu_view")

    ! View menu actions
    call zoom_in_action%set_text("Zoom &In")
    call zoom_in_action%set_shortcut("Ctrl++")
    call zoom_in_action%set_name("action_zoom_in")

    call zoom_out_action%set_text("Zoom &Out")
    call zoom_out_action%set_shortcut("Ctrl+-")
    call zoom_out_action%set_name("action_zoom_out")

    ! Add actions to View menu
    call view_menu%add_action(zoom_in_action)
    call view_menu%add_action(zoom_out_action)

    ! Create Help menu
    print '(A)', "Creating Help menu..."
    call help_menu%set_title("&Help")
    call help_menu%set_name("menu_help")

    ! Help menu actions
    call about_action%set_text("&About")
    call about_action%set_name("action_about")

    ! Add actions to Help menu
    call help_menu%add_action(about_action)

    ! Add menus to menu bar
    call menubar%add_menu(file_menu)
    call menubar%add_menu(edit_menu)
    call menubar%add_menu(view_menu)
    call menubar%add_menu(help_menu)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Menu bar with File, Edit, View, and Help menus")

    print '(A,I0,A)', "  Menu bar has ", menubar%get_menu_count(), " menus"
    print '(A,I0,A)', "  File menu has ", file_menu%get_action_count(), " actions"

    ! Set menu bar on window
    call window%set_menu_bar(menubar)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with menu bar..."
    call window%show()

    ! Simulate menu interactions
    print '(A)', ""
    print '(A)', "Simulating menu interactions:"

    ! Simulate triggering actions
    print '(A)', "  Triggered New action (Ctrl+N)"
    print '(A)', "  Triggered Copy action (Ctrl+C)"
    print '(A)', "  Triggered Zoom In action (Ctrl++)"
    print '(A)', "  Triggered About action"

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

        write(status_text, '(A,I0,A)') &
            "Menu bar ready with ", menubar%get_menu_count(), " menus"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_menubar_example