!> @brief Table Widget Example
!> @details Demonstrates table widgets for displaying tabular data
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_tablewidget_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_table_widget) :: table
    type(forge_label) :: status_label
    type(forge_button) :: add_row_button, remove_row_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i

    print '(A)', "=== Table Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating table widgets"
    print '(A)', "  - Setting up columns and rows"
    print '(A)', "  - Adding and editing table data"
    print '(A)', "  - Table headers and sorting"
    print '(A)', "  - Cell selection and editing"
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
        call builder%set_title("Table Widget Example")
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

    ! Create table widget
    print '(A)', "Creating table widget..."
    call table%set_name("data_table")
    call table%set_row_count(5)
    call table%set_column_count(4)
    call table%set_sorting_enabled(.true.)
    call table%set_alternating_row_colors(.true.)

    ! Set column headers
    call table%set_horizontal_header_item(1, "Name")
    call table%set_horizontal_header_item(2, "Age")
    call table%set_horizontal_header_item(3, "City")
    call table%set_horizontal_header_item(4, "Salary")

    ! Add sample data
    print '(A)', "Adding sample data..."
    call table%set_item_text(1, 1, "John Doe")
    call table%set_item_text(1, 2, "30")
    call table%set_item_text(1, 3, "New York")
    call table%set_item_text(1, 4, "$75,000")

    call table%set_item_text(2, 1, "Jane Smith")
    call table%set_item_text(2, 2, "25")
    call table%set_item_text(2, 3, "Los Angeles")
    call table%set_item_text(2, 4, "$68,000")

    call table%set_item_text(3, 1, "Bob Johnson")
    call table%set_item_text(3, 2, "45")
    call table%set_item_text(3, 3, "Chicago")
    call table%set_item_text(3, 4, "$92,000")

    call table%set_item_text(4, 1, "Alice Brown")
    call table%set_item_text(4, 2, "35")
    call table%set_item_text(4, 3, "Houston")
    call table%set_item_text(4, 4, "$82,000")

    call table%set_item_text(5, 1, "Charlie Wilson")
    call table%set_item_text(5, 2, "28")
    call table%set_item_text(5, 3, "Phoenix")
    call table%set_item_text(5, 4, "$71,000")

    ! Create control buttons
    call add_row_button%set_label("Add Row")
    call add_row_button%set_name("button_add_row")

    call remove_row_button%set_label("Remove Selected Row")
    call remove_row_button%set_name("button_remove_row")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Table with employee data - click cells to edit")

    print '(A,I0,A,I0)', "  Table size: ", table%get_row_count(), " rows x ", table%get_column_count(), " columns"
    print '(A,I0)', "  Selected items: ", table%get_selected_items_count()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with table..."
    call window%show()

    ! Simulate interactions
    print '(A)', ""
    print '(A)', "Simulating user interactions:"

    ! Select a cell
    call table%set_current_cell(2, 3)  ! Row 2, Column 3 (Los Angeles)
    print '(A,A)', "  Selected cell content: ", trim(table%get_current_item_text())

    ! Edit a cell
    call table%set_item_text(3, 4, "$95,000")  ! Update Bob's salary
    print '(A)', "  Updated Bob's salary to $95,000"

    ! Add a new row
    call table%insert_row(6)
    call table%set_item_text(6, 1, "Diana Prince")
    call table%set_item_text(6, 2, "32")
    call table%set_item_text(6, 3, "Seattle")
    call table%set_item_text(6, 4, "$88,000")
    print '(A,I0,A)', "  Added new row, now ", table%get_row_count(), " rows"

    ! Remove a row
    call table%remove_row(4)  ! Remove Alice Brown
    print '(A,I0,A)', "  Removed row, now ", table%get_row_count(), " rows"

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

        write(status_text, '(A,I0,A,I0,A,I0,A)') &
            "Table: ", table%get_row_count(), &
            " rows, ", table%get_column_count(), &
            " columns, ", table%get_selected_items_count(), " selected"
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program widget_tablewidget_example