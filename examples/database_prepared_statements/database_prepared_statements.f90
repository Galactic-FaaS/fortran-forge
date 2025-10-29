!> @brief Prepared statements example demonstrating QSqlQuery prepared statement usage
!> @details Shows how to use prepared statements for efficient query execution
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_prepared_statements_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    type(sql_query) :: query
    logical :: success
    type(sql_error) :: error
    integer :: i

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE Prepared Statements Example"
    write(*,*) "================================"

    ! Connect to SQLite database
    db = sql_database_add_database("QSQLITE", "prepared_example")
    call sql_database_set_database_name(db, ":memory:")

    success = sql_database_open(db)
    if (.not. success) then
        error = sql_database_last_error(db)
        write(*,*) "Failed to open database"
        stop
    end if

    ! Create table
    query = sql_query_new(db)
    success = sql_query_exec(query, "CREATE TABLE users (" // &
                           "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
                           "username TEXT NOT NULL UNIQUE, " // &
                           "email TEXT NOT NULL, " // &
                           "created_date TEXT)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to create table"
        call sql_database_close(db)
        stop
    end if

    write(*,*) "1. Table created successfully"

    ! Prepare INSERT statement
    success = sql_query_prepare(query, "INSERT INTO users (username, email, created_date) VALUES (?, ?, ?)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to prepare INSERT statement"
        call sql_database_close(db)
        stop
    end if

    write(*,*) "2. INSERT statement prepared"

    ! Insert multiple records using prepared statement
    write(*,*) "3. Inserting records:"

    ! Note: In a full implementation, you would bind actual values
    ! For demonstration, we'll show the concept

    ! Example data to insert
    type :: user_data
        character(len=50) :: username
        character(len=100) :: email
        character(len=20) :: created_date
    end type user_data

    type(user_data), dimension(3) :: users = [ &
        user_data("john_doe", "john@example.com", "2025-01-15"), &
        user_data("jane_smith", "jane@example.com", "2025-01-16"), &
        user_data("bob_johnson", "bob@example.com", "2025-01-17") &
    ]

    do i = 1, size(users)
        ! In a real implementation:
        ! call sql_query_bind_value(query, ":username", users(i)%username, SQL_TYPE_STRING)
        ! call sql_query_bind_value(query, ":email", users(i)%email, SQL_TYPE_STRING)
        ! call sql_query_bind_value(query, ":created_date", users(i)%created_date, SQL_TYPE_STRING)
        ! success = sql_query_exec_batch(query)

        write(*,*) "   Inserting user:", trim(users(i)%username)
    end do

    write(*,*) "   (Actual binding not fully implemented in this example)"

    ! Prepare SELECT statement
    success = sql_query_prepare(query, "SELECT id, username, email FROM users WHERE username = ?")

    if (success) then
        write(*,*) "4. SELECT statement prepared"

        ! In a real implementation:
        ! call sql_query_bind_value(query, ":username", "john_doe", SQL_TYPE_STRING)
        ! success = sql_query_exec(query)
        ! if (success) then
        !     do while (sql_query_next(query))
        !         ! Extract and display results
        !     end do
        ! end if

        write(*,*) "   (Query execution not fully implemented)"
    else
        error = sql_query_last_error(query)
        write(*,*) "Failed to prepare SELECT statement"
    end if

    ! Prepare UPDATE statement
    success = sql_query_prepare(query, "UPDATE users SET email = ? WHERE username = ?")

    if (success) then
        write(*,*) "5. UPDATE statement prepared"

        ! In a real implementation:
        ! call sql_query_bind_value(query, ":email", "john.doe@example.com", SQL_TYPE_STRING)
        ! call sql_query_bind_value(query, ":username", "john_doe", SQL_TYPE_STRING)
        ! success = sql_query_exec(query)

        write(*,*) "   (Update execution not fully implemented)"
    else
        error = sql_query_last_error(query)
        write(*,*) "Failed to prepare UPDATE statement"
    end if

    ! Prepare DELETE statement
    success = sql_query_prepare(query, "DELETE FROM users WHERE id = ?")

    if (success) then
        write(*,*) "6. DELETE statement prepared"

        ! In a real implementation:
        ! call sql_query_bind_value(query, ":id", 2, SQL_TYPE_INT)
        ! success = sql_query_exec(query)

        write(*,*) "   (Delete execution not fully implemented)"
    else
        error = sql_query_last_error(query)
        write(*,*) "Failed to prepare DELETE statement"
    end if

    ! Batch operations example
    write(*,*) "7. Batch operations concept:"
    write(*,*) "   - Prepared statements allow efficient batch inserts/updates"
    write(*,*) "   - Reduces parsing overhead for repeated queries"
    write(*,*) "   - Provides protection against SQL injection"
    write(*,*) "   - Supports parameter binding for different data types"

    ! Clean up
    call sql_database_close(db)
    call sql_database_remove_database("prepared_example")

    write(*,*) "Prepared statements example completed."
    write(*,*) "Note: Full parameter binding requires additional type handling implementation"

end program database_prepared_statements_example