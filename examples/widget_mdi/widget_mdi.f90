!> @brief MDI (Multiple Document Interface) Example
!> @details Demonstrates MDI windows for managing multiple documents
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_mdi_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_mdi_area) :: mdi_area
    type(forge_mdi_subwindow) :: subwin1, subwin2, subwin3
    type(forge_label) :: content1, content2, content3
    type(forge_button) :: new_window_button, cascade_button, tile_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== MDI (Multiple Document Interface) Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating MDI area widgets"
    print '(A)', "  - Adding MDI subwindows"
    print '(A)', "  - Window management (cascade, tile)"
    print '(A)', "  - MDI window activation"
    print '(A)', "  - Subwindow properties"
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
        call builder%set_title("MDI Example")
        call builder%set_size(700, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create MDI area
    print '(A)', "Creating MDI area..."
    call mdi_area%set_name("mdi_area")
    call mdi_area%set_tabs_closable(.true.)
    call mdi_area%set_tabs_movable(.true.)

    ! Create MDI subwindows
    print '(A)', "Creating MDI subwindows..."

    ! Document 1
    call subwin1%set_name("document1")
    call subwin1%set_window_title("Document 1 - Untitled.txt")
    call subwin1%set_window_icon_text("txt")

    call content1%set_name("content1")
    call content1%set_text("Document 1 Content\n\n" // &
                          "This is the first document.\n\n" // &
                          "MDI windows allow multiple\n" // &
                          "documents to be open simultaneously\n" // &
                          "within a single application window.\n\n" // &
                          "Features:\n" // &
                          "• Multiple document support\n" // &
                          "• Window management\n" // &
                          "• Tabbed or windowed view")

    call subwin1%set_widget(content1)

    ! Document 2
    call subwin2%set_name("document2")
    call subwin2%set_window_title("Document 2 - Report.docx")
    call subwin2%set_window_icon_text("docx")

    call content2%set_name("content2")
    call content2%set_text("Document 2 Content\n\n" // &
                          "This is the second document.\n\n" // &
                          "You can arrange MDI windows by:\n" // &
                          "• Cascading windows\n" // &
                          "• Tiling windows\n" // &
                          "• Minimizing/maximizing\n\n" // &
                          "Each window maintains its\n" // &
                          "own state and content.")

    call subwin2%set_widget(content2)

    ! Document 3
    call subwin3%set_name("document3")
    call subwin3%set_window_title("Document 3 - Spreadsheet.xlsx")
    call subwin3%set_window_icon_text("xlsx")

    call content3%set_name("content3")
    call content3%set_text("Document 3 Content\n\n" // &
                          "This is the third document.\n\n" // &
                          "MDI applications are useful for:\n" // &
                          "• Text editors\n" // &
                          "• IDEs\n" // &
                          "• Image editors\n" // &
                          "• Document processors")

    call subwin3%set_widget(content3)

    ! Add subwindows to MDI area
    call mdi_area%add_subwindow(subwin1)
    call mdi_area%add_subwindow(subwin2)
    call mdi_area%add_subwindow(subwin3)

    ! Control buttons
    call new_window_button%set_label("New Window")
    call new_window_button%set_name("button_new_window")

    call cascade_button%set_label("Cascade")
    call cascade_button%set_name("button_cascade")

    call tile_button%set_label("Tile")
    call tile_button%set_name("button_tile")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("3 MDI windows open - use controls to arrange")

    print '(A,I0,A)', "  MDI area has ", mdi_area%get_subwindow_count(), " subwindows"
    print '(A,A)', "  Active window: ", trim(mdi_area%get_active_subwindow() % get_window_title())

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with MDI area..."
    call window%show()

    ! Simulate MDI operations
    print '(A)', ""
    print '(A)', "Simulating MDI operations:"

    ! Activate different windows
    call mdi_area%set_active_subwindow(subwin2)
    print '(A,A)', "  Activated: ", trim(mdi_area%get_active_subwindow() % get_window_title())

    call mdi_area%set_active_subwindow(subwin3)
    print '(A,A)', "  Activated: ", trim(mdi_area%get_active_subwindow() % get_window_title())

    ! Cascade windows
    call mdi_area%cascade_subwindows()
    print '(A)', "  Cascaded all windows"

    ! Tile windows
    call mdi_area%tile_subwindows()
    print '(A)', "  Tiled all windows"

    ! Close a window
    call subwin1%close()
    print '(A,I0,A)', "  Closed a window, ", mdi_area%get_subwindow_count(), " remaining"

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

        write(status_text, '(A,I0,A,A)') &
            "", mdi_area%get_subwindow_count(), &
            " windows open, active: ", trim(mdi_area%get_active_subwindow() % get_window_title())
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_mdi_example