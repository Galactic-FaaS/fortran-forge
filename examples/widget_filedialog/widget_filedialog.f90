!> @brief File Dialog Widget Example
!> @details Demonstrates file dialog widgets for file selection
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_filedialog_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_file_dialog) :: file_dialog
    type(forge_button) :: open_button, save_button, dir_button
    type(forge_label) :: status_label, result_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== File Dialog Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating file dialog widgets"
    print '(A)', "  - Open file dialogs with filters"
    print '(A)', "  - Save file dialogs"
    print '(A)', "  - Directory selection dialogs"
    print '(A)', "  - Multiple file selection"
    print '(A)', "  - File type filters"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - dialogs won't actually appear"
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
        call builder%set_title("File Dialog Widget Example")
        call builder%set_size(500, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create buttons to trigger different file dialogs
    print '(A)', "Creating file dialog trigger buttons..."

    call open_button%set_label("Open File")
    call open_button%set_name("button_open")

    call save_button%set_label("Save File")
    call save_button%set_name("button_save")

    call dir_button%set_label("Select Directory")
    call dir_button%set_name("button_dir")

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Click buttons above to open different file dialogs")

    call result_label%set_name("result_label")
    call result_label%set_text("Selected files will appear here")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with file dialog buttons..."
    call window%show()

    ! Simulate showing different file dialogs
    print '(A)', ""
    print '(A)', "Simulating file dialog displays:"

    ! Open single file dialog
    print '(A)', ""
    print '(A)', "Showing Open File Dialog:"
    call file_dialog%set_accept_mode(0)  ! Open mode
    call file_dialog%set_file_mode(0)    ! Single file
    call file_dialog%set_window_title("Open Document")
    call file_dialog%set_directory("/home/user/documents")
    call file_dialog%set_name_filter("Text files (*.txt);;All files (*)")
    call file_dialog%set_default_suffix("txt")
    print '(A,A)', "  Title: ", trim(file_dialog%get_window_title())
    print '(A,A)', "  Directory: ", trim(file_dialog%get_directory())
    print '(A,A)', "  Filter: ", trim(file_dialog%get_name_filter())
    print '(A)', "  Mode: Open single file"

    ! Simulate file selection
    print '(A)', "  User selected: /home/user/documents/readme.txt"

    ! Open multiple files dialog
    print '(A)', ""
    print '(A)', "Showing Open Multiple Files Dialog:"
    call file_dialog%set_file_mode(1)    ! Multiple files
    call file_dialog%set_window_title("Open Multiple Documents")
    call file_dialog%set_name_filter("Text files (*.txt);;Image files (*.png *.jpg);;All files (*)")
    print '(A,A)', "  Title: ", trim(file_dialog%get_window_title())
    print '(A,A)', "  Filter: ", trim(file_dialog%get_name_filter())
    print '(A)', "  Mode: Open multiple files"

    ! Simulate multiple file selection
    print '(A)', "  User selected:"
    print '(A)', "    /home/user/documents/chapter1.txt"
    print '(A)', "    /home/user/documents/chapter2.txt"
    print '(A)', "    /home/user/images/logo.png"

    ! Save file dialog
    print '(A)', ""
    print '(A)', "Showing Save File Dialog:"
    call file_dialog%set_accept_mode(1)  ! Save mode
    call file_dialog%set_file_mode(0)    ! Single file
    call file_dialog%set_window_title("Save Document")
    call file_dialog%set_name_filter("Text files (*.txt);;PDF files (*.pdf);;All files (*)")
    call file_dialog%set_default_suffix("txt")
    print '(A,A)', "  Title: ", trim(file_dialog%get_window_title())
    print '(A,A)', "  Filter: ", trim(file_dialog%get_name_filter())
    print '(A)', "  Mode: Save file"

    ! Simulate save file selection
    print '(A)', "  User selected: /home/user/documents/new_document.txt"

    ! Directory selection dialog
    print '(A)', ""
    print '(A)', "Showing Directory Selection Dialog:"
    call file_dialog%set_file_mode(2)    ! Directory
    call file_dialog%set_window_title("Select Directory")
    call file_dialog%set_options(1)      ! Show directories only
    print '(A,A)', "  Title: ", trim(file_dialog%get_window_title())
    print '(A)', "  Mode: Select directory"

    ! Simulate directory selection
    print '(A)', "  User selected: /home/user/projects"

    ! Update result display
    call update_results()

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

    subroutine update_results()
        character(len:200) :: result_text

        write(result_text, '(A)') &
            "Last selection: /home/user/documents/readme.txt"
        call result_label%set_text(trim(result_text))
        print '(A,A)', "  Result: ", trim(result_text)
    end subroutine update_results

end program widget_filedialog_example