!> @brief Advanced Text Editor Example
!> @details Demonstrates a full-featured text editor application
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program advanced_text_editor_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_menu_bar) :: menubar
    type(forge_text_view) :: editor
    type(forge_status_bar) :: statusbar
    type(forge_label) :: status_label, cursor_label, file_label
    type(forge_file_dialog) :: file_dialog
    type(forge_message_box) :: msgbox
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    character(len=:), allocatable :: current_file
    logical :: modified = .false.

    print '(A)', "=== Advanced Text Editor Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Full-featured text editor with menu bar"
    print '(A)', "  - File operations (open, save, save as)"
    print '(A)', "  - Edit operations (cut, copy, paste, undo, redo)"
    print '(A)', "  - Find and replace functionality"
    print '(A)', "  - Status bar with cursor position"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - file operations are simulated"
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
        call builder%set_title("ForGE Text Editor - Untitled")
        call builder%set_size(800, 600)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create menu bar
    call create_menu_bar()

    ! Create text editor
    print '(A)', "Creating text editor..."
    call editor%set_name("text_editor")
    call editor%set_editable(.true.)
    call editor%set_word_wrap(.true.)
    call editor%set_font_family("Consolas")
    call editor%set_font_size(10)
    call editor%on_text_changed(on_text_changed)
    call editor%on_cursor_position_changed(on_cursor_changed)

    ! Create status bar
    call statusbar%set_name("status_bar")

    ! Status bar widgets
    call status_label%set_text("Ready")
    call statusbar%add_widget(status_label)

    call cursor_label%set_text("Ln 1, Col 1")
    call statusbar%add_widget(cursor_label)

    call file_label%set_text("Untitled")
    call statusbar%add_widget(file_label)

    ! Set menu bar and status bar on window
    call window%set_menu_bar(menubar)
    call window%set_status_bar(statusbar)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing advanced text editor..."
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

contains

    !> @brief Create the menu bar with all menus
    subroutine create_menu_bar()
        type(forge_menu) :: file_menu, edit_menu, view_menu, help_menu
        type(forge_action) :: new_action, open_action, save_action, save_as_action, exit_action
        type(forge_action) :: undo_action, redo_action, cut_action, copy_action, paste_action
        type(forge_action) :: find_action, replace_action
        type(forge_action) :: about_action

        ! File menu
        call file_menu%set_title("&File")

        call new_action%set_text("&New")
        call new_action%set_shortcut("Ctrl+N")
        call new_action%on_triggered(on_new_file)
        call file_menu%add_action(new_action)

        call open_action%set_text("&Open...")
        call open_action%set_shortcut("Ctrl+O")
        call open_action%on_triggered(on_open_file)
        call file_menu%add_action(open_action)

        call file_menu%add_separator()

        call save_action%set_text("&Save")
        call save_action%set_shortcut("Ctrl+S")
        call save_action%on_triggered(on_save_file)
        call file_menu%add_action(save_action)

        call save_as_action%set_text("Save &As...")
        call save_as_action%set_shortcut("Ctrl+Shift+S")
        call save_as_action%on_triggered(on_save_file_as)
        call file_menu%add_action(save_as_action)

        call file_menu%add_separator()

        call exit_action%set_text("E&xit")
        call exit_action%set_shortcut("Ctrl+Q")
        call exit_action%on_triggered(on_exit)
        call file_menu%add_action(exit_action)

        ! Edit menu
        call edit_menu%set_title("&Edit")

        call undo_action%set_text("&Undo")
        call undo_action%set_shortcut("Ctrl+Z")
        call undo_action%on_triggered(on_undo)
        call edit_menu%add_action(undo_action)

        call redo_action%set_text("&Redo")
        call redo_action%set_shortcut("Ctrl+Y")
        call redo_action%on_triggered(on_redo)
        call edit_menu%add_action(redo_action)

        call edit_menu%add_separator()

        call cut_action%set_text("Cu&t")
        call cut_action%set_shortcut("Ctrl+X")
        call cut_action%on_triggered(on_cut)
        call edit_menu%add_action(cut_action)

        call copy_action%set_text("&Copy")
        call copy_action%set_shortcut("Ctrl+C")
        call copy_action%on_triggered(on_copy)
        call edit_menu%add_action(copy_action)

        call paste_action%set_text("&Paste")
        call paste_action%set_shortcut("Ctrl+V")
        call paste_action%on_triggered(on_paste)
        call edit_menu%add_action(paste_action)

        call edit_menu%add_separator()

        call find_action%set_text("&Find...")
        call find_action%set_shortcut("Ctrl+F")
        call find_action%on_triggered(on_find)
        call edit_menu%add_action(find_action)

        call replace_action%set_text("&Replace...")
        call replace_action%set_shortcut("Ctrl+H")
        call replace_action%on_triggered(on_replace)
        call edit_menu%add_action(replace_action)

        ! Help menu
        call help_menu%set_title("&Help")

        call about_action%set_text("&About")
        call about_action%on_triggered(on_about)
        call help_menu%add_action(about_action)

        ! Add menus to menu bar
        call menubar%add_menu(file_menu)
        call menubar%add_menu(edit_menu)
        call menubar%add_menu(help_menu)
    end subroutine create_menu_bar

    !> @brief New file operation
    subroutine on_new_file()
        if (modified) then
            ! Ask to save changes (simplified)
            call msgbox%set_window_title("Save Changes")
            call msgbox%set_text("Do you want to save changes to the current file?")
            call msgbox%set_standard_buttons(6)  ! Yes | No
            ! In real implementation, check user response
        end if

        call editor%set_text("")
        current_file = ""
        modified = .false.
        call window%set_title("ForGE Text Editor - Untitled")
        call file_label%set_text("Untitled")
        call status_label%set_text("New file created")

        print '(A)', "  → New file created"
    end subroutine on_new_file

    !> @brief Open file operation
    subroutine on_open_file()
        call file_dialog%set_accept_mode(0)  ! Open
        call file_dialog%set_file_mode(0)    ! Single file
        call file_dialog%set_window_title("Open File")
        call file_dialog%set_name_filter("Text files (*.txt);;All files (*)")

        ! Simulate file selection
        current_file = "example.txt"
        call editor%set_text("This is sample text content.\nIt demonstrates the text editor functionality.\n\nYou can edit this text and save it to a file.")
        modified = .false.
        call window%set_title("ForGE Text Editor - " // current_file)
        call file_label%set_text(current_file)
        call status_label%set_text("File opened: " // current_file)

        print '(A,A)', "  → File opened: ", current_file
    end subroutine on_open_file

    !> @brief Save file operation
    subroutine on_save_file()
        if (.not. allocated(current_file) .or. len_trim(current_file) == 0) then
            call on_save_file_as()
            return
        end if

        ! Simulate saving file
        call status_label%set_text("File saved: " // current_file)
        modified = .false.

        print '(A,A)', "  → File saved: ", current_file
    end subroutine on_save_file

    !> @brief Save file as operation
    subroutine on_save_file_as()
        call file_dialog%set_accept_mode(1)  ! Save
        call file_dialog%set_file_mode(0)    ! Single file
        call file_dialog%set_window_title("Save File As")
        call file_dialog%set_name_filter("Text files (*.txt);;All files (*)")

        ! Simulate file selection
        current_file = "my_document.txt"
        call on_save_file()
    end subroutine on_save_file_as

    !> @brief Exit application
    subroutine on_exit()
        if (modified) then
            ! Ask to save changes (simplified)
            call msgbox%set_window_title("Save Changes")
            call msgbox%set_text("Do you want to save changes before exiting?")
            call msgbox%set_standard_buttons(6)  ! Yes | No
        end if

        print '(A)', "  → Application exit requested"
        ! In real implementation, this would close the application
    end subroutine on_exit

    !> @brief Undo operation
    subroutine on_undo()
        call editor%undo()
        call status_label%set_text("Undo performed")
        print '(A)', "  → Undo performed"
    end subroutine on_undo

    !> @brief Redo operation
    subroutine on_redo()
        call editor%redo()
        call status_label%set_text("Redo performed")
        print '(A)', "  → Redo performed"
    end subroutine on_redo

    !> @brief Cut operation
    subroutine on_cut()
        call editor%cut()
        call status_label%set_text("Text cut to clipboard")
        print '(A)', "  → Text cut"
    end subroutine on_cut

    !> @brief Copy operation
    subroutine on_copy()
        call editor%copy()
        call status_label%set_text("Text copied to clipboard")
        print '(A)', "  → Text copied"
    end subroutine on_copy

    !> @brief Paste operation
    subroutine on_paste()
        call editor%paste()
        call status_label%set_text("Text pasted from clipboard")
        print '(A)', "  → Text pasted"
    end subroutine on_paste

    !> @brief Find operation
    subroutine on_find()
        call status_label%set_text("Find dialog would open")
        print '(A)', "  → Find dialog requested"
    end subroutine on_find

    !> @brief Replace operation
    subroutine on_replace()
        call status_label%set_text("Replace dialog would open")
        print '(A)', "  → Replace dialog requested"
    end subroutine on_replace

    !> @brief About operation
    subroutine on_about()
        call msgbox%set_window_title("About ForGE Text Editor")
        call msgbox%set_text("ForGE Text Editor\nVersion 1.0\n\nA demonstration of advanced text editing capabilities.")
        call msgbox%set_standard_buttons(1)  ! OK
        print '(A)', "  → About dialog shown"
    end subroutine on_about

    !> @brief Text changed handler
    subroutine on_text_changed()
        modified = .true.
        call status_label%set_text("Text modified")
    end subroutine on_text_changed

    !> @brief Cursor position changed handler
    subroutine on_cursor_changed()
        integer :: line, column
        character(len=50) :: pos_text

        ! Get cursor position (simplified)
        line = 1
        column = 1

        write(pos_text, '("Ln ", I0, ", Col ", I0)') line, column
        call cursor_label%set_text(trim(pos_text))
    end subroutine on_cursor_changed

end program advanced_text_editor_example