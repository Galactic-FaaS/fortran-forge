!> @brief Stacked Widget Example
!> @details Demonstrates stacked widgets for layered content display
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_stackedwidget_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_stacked_widget) :: stacked_widget
    type(forge_label) :: page1_label, page2_label, page3_label
    type(forge_button) :: next_button, prev_button
    type(forge_combo_box) :: page_selector
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Stacked Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating stacked widget containers"
    print '(A)', "  - Adding multiple pages/layers"
    print '(A)', "  - Switching between pages programmatically"
    print '(A)', "  - Navigation controls"
    print '(A)', "  - Page indexing and counting"
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
        call builder%set_title("Stacked Widget Example")
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

    ! Create stacked widget
    print '(A)', "Creating stacked widget..."
    call stacked_widget%set_name("main_stacked_widget")

    ! Create page content
    print '(A)', "Creating page content..."

    ! Page 1: Welcome
    call page1_label%set_name("page1_label")
    call page1_label%set_text("Welcome Page\n\n" // &
                             "This is the first page in the stacked widget.\n\n" // &
                             "Use the navigation buttons or combo box to switch pages.\n\n" // &
                             "Each page can contain different widgets and layouts.")

    ! Page 2: Settings
    call page2_label%set_name("page2_label")
    call page2_label%set_text("Settings Page\n\n" // &
                             "This is the second page.\n\n" // &
                             "Here you would typically place:\n" // &
                             "• Configuration options\n" // &
                             "• Preference controls\n" // &
                             "• System settings")

    ! Page 3: About
    call page3_label%set_name("page3_label")
    call page3_label%set_text("About Page\n\n" // &
                             "This is the third and final page.\n\n" // &
                             "Information about the application:\n" // &
                             "• Version information\n" // &
                             "• Credits\n" // &
                             "• License details")

    ! Add pages to stacked widget
    call stacked_widget%add_widget(page1_label)
    call stacked_widget%add_widget(page2_label)
    call stacked_widget%add_widget(page3_label)

    ! Create navigation controls
    call prev_button%set_label("Previous")
    call prev_button%set_name("button_prev")

    call next_button%set_label("Next")
    call next_button%set_name("button_next")

    ! Page selector combo box
    call page_selector%set_name("page_selector")
    call page_selector%set_editable(.false.)
    call page_selector%add_item("Welcome")
    call page_selector%add_item("Settings")
    call page_selector%add_item("About")
    call page_selector%set_current_index(0)  ! Start on first page

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Page 1 of 3: Welcome")

    print '(A,I0,A)', "  Stacked widget has ", stacked_widget%get_count(), " pages"
    print '(A,I0)', "  Current page index: ", stacked_widget%get_current_index()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with stacked widget..."
    call window%show()

    ! Simulate page navigation
    print '(A)', ""
    print '(A)', "Simulating page navigation:"

    ! Switch to page 2
    call stacked_widget%set_current_index(1)
    call page_selector%set_current_index(1)
    print '(A,I0,A,A)', "  Switched to page ", stacked_widget%get_current_index() + 1, ": ", trim(page_selector%get_current_text())

    ! Switch to page 3
    call stacked_widget%set_current_index(2)
    call page_selector%set_current_index(2)
    print '(A,I0,A,A)', "  Switched to page ", stacked_widget%get_current_index() + 1, ": ", trim(page_selector%get_current_text())

    ! Back to page 1
    call stacked_widget%set_current_index(0)
    call page_selector%set_current_index(0)
    print '(A,I0,A,A)', "  Switched back to page ", stacked_widget%get_current_index() + 1, ": ", trim(page_selector%get_current_text())

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
        integer :: current_page

        current_page = stacked_widget%get_current_index() + 1
        write(status_text, '(A,I0,A,I0,A,A)') &
            "Page ", current_page, " of ", stacked_widget%get_count(), &
            ": ", trim(page_selector%get_current_text())
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_stackedwidget_example