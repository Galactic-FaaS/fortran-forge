!> @brief List View Widget Example
!> @details Demonstrates list view widgets for displaying item lists
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_listview_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_list_view) :: listview
    type(forge_label) :: status_label
    type(forge_button) :: add_button, remove_button, clear_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i

    print '(A)', "=== List View Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating list view widgets"
    print '(A)', "  - Adding and removing list items"
    print '(A)', "  - Selection modes (single, multi, extended)"
    print '(A)', "  - Item icons and text"
    print '(A)', "  - Sorting and filtering"
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
        call builder%set_title("List View Widget Example")
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

    ! Create list view
    print '(A)', "Creating list view..."
    call listview%set_name("main_list")
    call listview%set_selection_mode(2)  ! Extended selection
    call listview%set_sorting_enabled(.true.)

    ! Add initial items
    print '(A)', "Adding initial items..."
    call listview%add_item("Apple")
    call listview%add_item("Banana")
    call listview%add_item("Cherry")
    call listview%add_item("Date")
    call listview%add_item("Elderberry")
    call listview%add_item("Fig")
    call listview%add_item("Grape")
    call listview%add_item("Honeydew")

    ! Create control buttons
    call add_button%set_label("Add Item")
    call add_button%set_name("button_add")

    call remove_button%set_label("Remove Selected")
    call remove_button%set_name("button_remove")

    call clear_button%set_label("Clear All")
    call clear_button%set_name("button_clear")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("List view with 8 items - select items to see details")

    print '(A,I0,A)', "  List contains ", listview%get_item_count(), " items"
    print '(A,I0)', "  Selection mode: ", listview%get_selection_mode()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with list view..."
    call window%show()

    ! Simulate interactions
    print '(A)', ""
    print '(A)', "Simulating user interactions:"

    ! Select some items
    call listview%set_current_row(1)  ! Select Banana
    print '(A,A)', "  Selected item: ", trim(listview%get_current_item_text())

    call listview%set_current_row(3)  ! Select Date
    print '(A,A)', "  Selected item: ", trim(listview%get_current_item_text())

    ! Add new item
    call listview%add_item("Kiwi")
    print '(A,I0,A)', "  Added item, now ", listview%get_item_count(), " items"

    ! Remove item
    call listview%remove_item(2)  ! Remove Cherry
    print '(A,I0,A)', "  Removed item, now ", listview%get_item_count(), " items"

    ! Clear and repopulate
    call listview%clear()
    print '(A)', "  Cleared list"

    do i = 1, 5
        call listview%add_item("Item " // char(48+i))
    end do
    print '(A,I0,A)', "  Repopulated with ", listview%get_item_count(), " new items"

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

        write(status_text, '(A,I0,A,I0,A)') &
            "List has ", listview%get_item_count(), &
            " items, current row: ", listview%get_current_row(), &
            ", selected: ", listview%get_selected_count()
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_listview_example