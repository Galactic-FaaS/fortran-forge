!> @brief SQL Queries Example
!> @details Demonstrates executing various SQL queries against a database
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program database_sql_queries_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_database) :: db
    type(forge_sql_query) :: query
    type(forge_label) :: status_label, connection_label
    type(forge_button) :: connect_button, execute_select_button, execute_insert_button
    type(forge_button) :: execute_update_button, execute_delete_button
    type(forge_entry) :: sql_entry
    type(forge_text_view) :: results_view, log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: connected = .false.

    print '(A)', "=== SQL Queries Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Database connection management"
    print '(A)', "  - Executing SELECT, INSERT, UPDATE, DELETE queries"
    print '(A)', "  - Query result processing"
    print '(A)', "  - SQL statement preparation"
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
        call builder%set_title("SQL Queries Example")
        call builder%set_size(700, 600)
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
    print '(A)', "Creating database connection..."
    call db%set_driver("QSQLITE")
    call db%set_database_name(":memory:")  ! In-memory SQLite database
    call db%set_host_name("localhost")
    call db%set_user_name("")
    call db%set_password("")

    ! Create UI elements
    print '(A)', "Creating SQL query interface..."

    ! Connection button
    call connect_button%set_label("Connect to Database")
    call connect_button%set_name("connect_button")
    call connect_button%on_click(on_connect_clicked)

    ! Query execution buttons
    call execute_select_button%set_label("Execute SELECT")
    call execute_select_button%set_name("select_button")
    call execute_select_button%on_click(on_execute_select_clicked)
    call execute_select_button%set_enabled(.false.)

    call execute_insert_button%set_label("Execute INSERT")
    call execute_insert_button%set_name("insert_button")
    call execute_insert_button%on_click(on_execute_insert_clicked)
    call execute_insert_button%set_enabled(.false.)

    call execute_update_button%set_label("Execute UPDATE")
    call execute_update_button%set_name("update_button")
    call execute_update_button%on_click(on_execute_update_clicked)
    call execute_update_button%set_enabled(.false.)

    call execute_delete_button%set_label("Execute DELETE")
    call execute_delete_button%set_name("delete_button")
    call execute_delete_button%on_click(on_execute_delete_clicked)
    call execute_delete_button%set_enabled(.false.)

    ! SQL entry
    call sql_entry%set_placeholder_text("Enter SQL statement here")
    call sql_entry%set_name("sql_entry")
    call sql_entry%set_text("SELECT * FROM users")

    ! Status labels
    call status_label%set_name("status_label")
    call status_label%set_text("Database disconnected - click Connect to begin")

    call connection_label%set_name("connection_label")
    call connection_label%set_text("Status: Disconnected")

    ! Results display
    call results_view%set_name("results_view")
    call results_view%set_editable(.false.)
    call results_view%set_text("Query results will appear here...\n")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Database operations log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing SQL query interface..."
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

    !> @brief Connect to the database
    subroutine connect_to_database()
        print '(A)', "  Connecting to database..."

        ! Open database connection (simulated)
        call db%open(status)
        if (status%is_error()) then
            call status_label%set_text("Error: Failed to connect to database")
            call status%print()
            return
        end if

        connected = .true.

        call connect_button%set_enabled(.false.)
        call execute_select_button%set_enabled(.true.)
        call execute_insert_button%set_enabled(.true.)
        call execute_update_button%set_enabled(.true.)
        call execute_delete_button%set_enabled(.true.)

        call status_label%set_text("Database connected successfully")
        call connection_label%set_text("Status: Connected (SQLite in-memory)")

        call log_view%set_text(log_view%get_text() // "Database connected\n")

        ! Create sample table
        call create_sample_table()

        print '(A)', "  → Database connected"
    end subroutine connect_to_database

    !> @brief Create sample table for demonstration
    subroutine create_sample_table()
        character(len=:), allocatable :: create_sql

        create_sql = "CREATE TABLE users (" // &
                    "id INTEGER PRIMARY KEY, " // &
                    "name TEXT NOT NULL, " // &
                    "email TEXT UNIQUE, " // &
                    "age INTEGER, " // &
                    "created_date TEXT)"

        call query%prepare(create_sql)
        call query%execute()

        call log_view%set_text(log_view%get_text() // "Created users table\n")

        ! Insert sample data
        call insert_sample_data()
    end subroutine create_sample_table

    !> @brief Insert sample data
    subroutine insert_sample_data()
        call query%prepare("INSERT INTO users (name, email, age, created_date) VALUES (?, ?, ?, ?)")
        call query%add_bind_value("John Doe")
        call query%add_bind_value("john@example.com")
        call query%add_bind_value(30)
        call query%add_bind_value("2024-01-01")
        call query%execute()

        call query%prepare("INSERT INTO users (name, email, age, created_date) VALUES (?, ?, ?, ?)")
        call query%add_bind_value("Jane Smith")
        call query%add_bind_value("jane@example.com")
        call query%add_bind_value(25)
        call query%add_bind_value("2024-01-02")
        call query%execute()

        call log_view%set_text(log_view%get_text() // "Inserted sample data\n")
    end subroutine insert_sample_data

    !> @brief Execute SELECT query
    subroutine execute_select()
        character(len=:), allocatable :: sql
        character(len=500) :: result_text

        sql = trim(sql_entry%get_text())
        if (len_trim(sql) == 0) then
            call status_label%set_text("Error: Please enter a SQL statement")
            return
        end if

        print '(A,A)', "  Executing SELECT: ", sql

        ! Execute query (simulated)
        call query%prepare(sql)
        call query%execute()

        ! Process results (simulated)
        result_text = "Query Results:\n" // &
                     "ID | Name       | Email            | Age | Created\n" // &
                     "---+------------+------------------+-----+---------\n" // &
                     "1  | John Doe   | john@example.com | 30  | 2024-01-01\n" // &
                     "2  | Jane Smith | jane@example.com | 25  | 2024-01-02\n"

        call results_view%set_text(trim(result_text))

        write(log_text, '(A,A,A)', "Executed SELECT: ", trim(sql), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        call status_label%set_text("SELECT query executed successfully")
    end subroutine execute_select

    !> @brief Execute INSERT query
    subroutine execute_insert()
        character(len=:), allocatable :: sql

        sql = "INSERT INTO users (name, email, age, created_date) VALUES ('Bob Wilson', 'bob@example.com', 35, '2024-01-03')"

        print '(A,A)', "  Executing INSERT: ", sql

        ! Execute query (simulated)
        call query%prepare(sql)
        call query%execute()

        call results_view%set_text("INSERT Result:\n1 row inserted\n")

        write(log_text, '(A,A,A)', "Executed INSERT: ", trim(sql), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        call status_label%set_text("INSERT query executed successfully")
    end subroutine execute_insert

    !> @brief Execute UPDATE query
    subroutine execute_update()
        character(len=:), allocatable :: sql

        sql = "UPDATE users SET age = 31 WHERE name = 'John Doe'"

        print '(A,A)', "  Executing UPDATE: ", sql

        ! Execute query (simulated)
        call query%prepare(sql)
        call query%execute()

        call results_view%set_text("UPDATE Result:\n1 row updated\n")

        write(log_text, '(A,A,A)', "Executed UPDATE: ", trim(sql), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        call status_label%set_text("UPDATE query executed successfully")
    end subroutine execute_update

    !> @brief Execute DELETE query
    subroutine execute_delete()
        character(len=:), allocatable :: sql

        sql = "DELETE FROM users WHERE age < 30"

        print '(A,A)', "  Executing DELETE: ", sql

        ! Execute query (simulated)
        call query%prepare(sql)
        call query%execute()

        call results_view%set_text("DELETE Result:\n1 row deleted\n")

        write(log_text, '(A,A,A)', "Executed DELETE: ", trim(sql), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        call status_label%set_text("DELETE query executed successfully")
    end subroutine execute_delete

    !> @brief Handler for connect button click
    subroutine on_connect_clicked(event)
        type(forge_event), intent(in) :: event
        call connect_to_database()
    end subroutine on_connect_clicked

    !> @brief Handler for execute SELECT button click
    subroutine on_execute_select_clicked(event)
        type(forge_event), intent(in) :: event
        call execute_select()
    end subroutine on_execute_select_clicked

    !> @brief Handler for execute INSERT button click
    subroutine on_execute_insert_clicked(event)
        type(forge_event), intent(in) :: event
        call execute_insert()
    end subroutine on_execute_insert_clicked

    !> @brief Handler for execute UPDATE button click
    subroutine on_execute_update_clicked(event)
        type(forge_event), intent(in) :: event
        call execute_update()
    end subroutine on_execute_update_clicked

    !> @brief Handler for execute DELETE button click
    subroutine on_execute_delete_clicked(event)
        type(forge_event), intent(in) :: event
        call execute_delete()
    end subroutine on_execute_delete_clicked

end program database_sql_queries_example