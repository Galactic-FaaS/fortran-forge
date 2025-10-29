!> @brief CRUD Operations Example
!> @details Demonstrates Create, Read, Update, Delete operations on database
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program database_crud_operations_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_database) :: db
    type(forge_sql_query) :: query
    type(forge_label) :: status_label, record_count_label
    type(forge_button) :: create_button, read_button, update_button, delete_button
    type(forge_entry) :: name_entry, email_entry, id_entry
    type(forge_text_view) :: records_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: connected = .false.
    integer :: record_count = 0

    print '(A)', "=== CRUD Operations Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Create: Inserting new records"
    print '(A)', "  - Read: Retrieving and displaying records"
    print '(A)', "  - Update: Modifying existing records"
    print '(A)', "  - Delete: Removing records"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - database operations are simulated"
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
        call builder%set_title("CRUD Operations Example")
        call builder%set_size(600, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create database connection
    print '(A)', "Setting up database..."
    call db%set_driver("QSQLITE")
    call db%set_database_name(":memory:")

    ! Connect to database
    call db%open(status)
    if (status%is_error()) then
        print '(A)', "Failed to connect to database"
        call status%print()
        stop 1
    end if
    connected = .true.

    ! Create table
    call create_table()

    ! Create UI elements
    print '(A)', "Creating CRUD operations interface..."

    ! Input fields
    call name_entry%set_placeholder_text("Enter name")
    call name_entry%set_name("name_entry")

    call email_entry%set_placeholder_text("Enter email")
    call email_entry%set_name("email_entry")

    call id_entry%set_placeholder_text("Record ID")
    call id_entry%set_name("id_entry")

    ! CRUD operation buttons
    call create_button%set_label("Create Record")
    call create_button%set_name("create_button")
    call create_button%on_click(on_create_clicked)

    call read_button%set_label("Read Records")
    call read_button%set_name("read_button")
    call read_button%on_click(on_read_clicked)

    call update_button%set_label("Update Record")
    call update_button%set_name("update_button")
    call update_button%on_click(on_update_clicked)

    call delete_button%set_label("Delete Record")
    call delete_button%set_name("delete_button")
    call delete_button%on_click(on_delete_clicked)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("Database ready - perform CRUD operations")

    call record_count_label%set_name("record_count_label")
    call record_count_label%set_text("Records: 0")

    ! Records display
    call records_view%set_name("records_view")
    call records_view%set_editable(.false.)
    call records_view%set_text("Records will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing CRUD operations interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (connected) then
        call db%close()
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Create the database table
    subroutine create_table()
        character(len=:), allocatable :: sql

        sql = "CREATE TABLE contacts (" // &
              "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
              "name TEXT NOT NULL, " // &
              "email TEXT UNIQUE, " // &
              "created_at DATETIME DEFAULT CURRENT_TIMESTAMP)"

        call query%prepare(sql)
        call query%execute()

        print '(A)', "  → Created contacts table"
    end subroutine create_table

    !> @brief Create a new record
    subroutine create_record()
        character(len=:), allocatable :: name, email

        name = trim(name_entry%get_text())
        email = trim(email_entry%get_text())

        if (len_trim(name) == 0 .or. len_trim(email) == 0) then
            call status_label%set_text("Error: Name and email are required")
            return
        end if

        print '(A,A,A,A,A)', "  Creating record: ", trim(name), " <", trim(email), ">"

        ! Insert record
        call query%prepare("INSERT INTO contacts (name, email) VALUES (?, ?)")
        call query%add_bind_value(name)
        call query%add_bind_value(email)
        call query%execute()

        record_count = record_count + 1

        call status_label%set_text("Record created successfully")
        call update_record_count()

        ! Clear input fields
        call name_entry%set_text("")
        call email_entry%set_text("")

        print '(A)', "  → Record created"
    end subroutine create_record

    !> @brief Read and display all records
    subroutine read_records()
        character(len=500) :: display_text

        print '(A)', "  Reading all records..."

        call query%prepare("SELECT id, name, email, created_at FROM contacts ORDER BY id")
        call query%execute()

        ! Build display text (simulated)
        display_text = "Contacts:\n" // &
                      "ID | Name          | Email                | Created\n" // &
                      "---+---------------+----------------------+---------\n"

        if (record_count >= 1) then
            display_text = trim(display_text) // "1  | John Doe      | john@example.com     | 2024-01-01\n"
        end if
        if (record_count >= 2) then
            display_text = trim(display_text) // "2  | Jane Smith    | jane@example.com     | 2024-01-02\n"
        end if
        if (record_count >= 3) then
            display_text = trim(display_text) // "3  | Bob Wilson    | bob@example.com      | 2024-01-03\n"
        end if

        call records_view%set_text(trim(display_text))
        call status_label%set_text("Records retrieved successfully")

        print '(A,I0,A)', "  → Retrieved ", record_count, " records"
    end subroutine read_records

    !> @brief Update an existing record
    subroutine update_record()
        character(len=:), allocatable :: id_str, name, email
        integer :: record_id

        id_str = trim(id_entry%get_text())
        name = trim(name_entry%get_text())
        email = trim(email_entry%get_text())

        if (len_trim(id_str) == 0) then
            call status_label%set_text("Error: Record ID is required for update")
            return
        end if

        read(id_str, *, iostat=ios) record_id
        if (ios /= 0 .or. record_id < 1) then
            call status_label%set_text("Error: Invalid record ID")
            return
        end if

        print '(A,I0,A)', "  Updating record ID: ", record_id

        ! Update record
        if (len_trim(name) > 0 .and. len_trim(email) > 0) then
            call query%prepare("UPDATE contacts SET name = ?, email = ? WHERE id = ?")
            call query%add_bind_value(name)
            call query%add_bind_value(email)
            call query%add_bind_value(record_id)
        else if (len_trim(name) > 0) then
            call query%prepare("UPDATE contacts SET name = ? WHERE id = ?")
            call query%add_bind_value(name)
            call query%add_bind_value(record_id)
        else if (len_trim(email) > 0) then
            call query%prepare("UPDATE contacts SET email = ? WHERE id = ?")
            call query%add_bind_value(email)
            call query%add_bind_value(record_id)
        else
            call status_label%set_text("Error: Name or email required for update")
            return
        end if

        call query%execute()

        call status_label%set_text("Record updated successfully")

        ! Clear input fields
        call id_entry%set_text("")
        call name_entry%set_text("")
        call email_entry%set_text("")

        print '(A,I0,A)', "  → Record ", record_id, " updated"
    end subroutine update_record

    !> @brief Delete a record
    subroutine delete_record()
        character(len=:), allocatable :: id_str
        integer :: record_id

        id_str = trim(id_entry%get_text())

        if (len_trim(id_str) == 0) then
            call status_label%set_text("Error: Record ID is required for deletion")
            return
        end if

        read(id_str, *, iostat=ios) record_id
        if (ios /= 0 .or. record_id < 1) then
            call status_label%set_text("Error: Invalid record ID")
            return
        end if

        print '(A,I0,A)', "  Deleting record ID: ", record_id

        ! Delete record
        call query%prepare("DELETE FROM contacts WHERE id = ?")
        call query%add_bind_value(record_id)
        call query%execute()

        if (record_count > 0) record_count = record_count - 1

        call status_label%set_text("Record deleted successfully")
        call update_record_count()

        ! Clear input field
        call id_entry%set_text("")

        print '(A,I0,A)', "  → Record ", record_id, " deleted"
    end subroutine delete_record

    !> @brief Update the record count display
    subroutine update_record_count()
        character(len=50) :: count_text

        write(count_text, '("Records: ", I0)') record_count
        call record_count_label%set_text(trim(count_text))
    end subroutine update_record_count

    !> @brief Handler for create button click
    subroutine on_create_clicked(event)
        type(forge_event), intent(in) :: event
        call create_record()
    end subroutine on_create_clicked

    !> @brief Handler for read button click
    subroutine on_read_clicked(event)
        type(forge_event), intent(in) :: event
        call read_records()
    end subroutine on_read_clicked

    !> @brief Handler for update button click
    subroutine on_update_clicked(event)
        type(forge_event), intent(in) :: event
        call update_record()
    end subroutine on_update_clicked

    !> @brief Handler for delete button click
    subroutine on_delete_clicked(event)
        type(forge_event), intent(in) :: event
        call delete_record()
    end subroutine on_delete_clicked

end program database_crud_operations_example