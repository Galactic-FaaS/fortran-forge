!> @brief Database error handling example demonstrating QSqlError usage
!> @details Shows how to handle and report database errors
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_error_handling_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    type(sql_query) :: query
    logical :: success
    type(sql_error) :: error
    integer :: error_type, error_number

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE Database Error Handling Example"
    write(*,*) "====================================="

    ! Example 1: Connection error
    write(*,*) "1. Testing connection errors:"

    ! Try to connect to non-existent database
    db = sql_database_add_database("QMYSQL", "error_test")
    call sql_database_set_host_name(db, "nonexistent.host")
    call sql_database_set_database_name(db, "nonexistent_db")
    call sql_database_set_user_name(db, "invalid_user")
    call sql_database_set_password(db, "invalid_password")

    success = sql_database_open(db)
    if (.not. success) then
        error = sql_database_last_error(db)
        error_type = sql_error_type(error)
        error_number = sql_error_number(error)
        write(*,*) "   Connection failed as expected:"
        write(*,*) "   - Error type:", error_type
        write(*,*) "   - Error number:", error_number
        write(*,*) "   - Error is valid:", sql_error_is_valid(error)
    else
        write(*,*) "   Unexpected: connection succeeded"
        call sql_database_close(db)
    end if

    ! Example 2: SQLite connection (should work)
    write(*,*) "2. Testing successful SQLite connection:"
    call sql_database_remove_database("error_test")

    db = sql_database_add_database("QSQLITE", "sqlite_test")
    call sql_database_set_database_name(db, ":memory:")

    success = sql_database_open(db)
    if (success) then
        write(*,*) "   SQLite connection successful"
    else
        error = sql_database_last_error(db)
        write(*,*) "   SQLite connection failed unexpectedly"
    end if

    ! Example 3: SQL syntax errors
    write(*,*) "3. Testing SQL syntax errors:"

    query = sql_query_new(db)

    ! Invalid SQL
    success = sql_query_exec(query, "INVALID SQL STATEMENT")
    if (.not. success) then
        error = sql_query_last_error(query)
        error_type = sql_error_type(error)
        error_number = sql_error_number(error)
        write(*,*) "   SQL syntax error detected:"
        write(*,*) "   - Error type:", error_type
        write(*,*) "   - Error number:", error_number
    else
        write(*,*) "   Unexpected: invalid SQL executed successfully"
    end if

    ! Example 4: Constraint violations
    write(*,*) "4. Testing constraint violations:"

    ! Create table with constraints
    success = sql_query_exec(query, "CREATE TABLE test_table (" // &
                           "id INTEGER PRIMARY KEY, " // &
                           "name TEXT NOT NULL UNIQUE, " // &
                           "age INTEGER CHECK(age > 0))")

    if (success) then
        write(*,*) "   Table created successfully"

        ! Try to insert NULL into NOT NULL column
        success = sql_query_exec(query, "INSERT INTO test_table (id, name) VALUES (1, NULL)")
        if (.not. success) then
            error = sql_query_last_error(query)
            write(*,*) "   NOT NULL constraint violation detected"
        end if

        ! Try to insert duplicate unique value
        success = sql_query_exec(query, "INSERT INTO test_table (id, name, age) VALUES (2, 'test', 25)")
        success = sql_query_exec(query, "INSERT INTO test_table (id, name, age) VALUES (3, 'test', 30)")
        if (.not. success) then
            error = sql_query_last_error(query)
            write(*,*) "   UNIQUE constraint violation detected"
        end if

        ! Try to insert invalid age
        success = sql_query_exec(query, "INSERT INTO test_table (id, name, age) VALUES (4, 'invalid', -5)")
        if (.not. success) then
            error = sql_query_last_error(query)
            write(*,*) "   CHECK constraint violation detected"
        end if

    else
        error = sql_query_last_error(query)
        write(*,*) "   Failed to create table"
    end if

    ! Example 5: Prepared statement errors
    write(*,*) "5. Testing prepared statement errors:"

    ! Try to prepare invalid SQL
    success = sql_query_prepare(query, "SELECT * FROM nonexistent_table WHERE id = ?")
    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "   Prepared statement error detected"
    end if

    ! Example 6: Transaction errors
    write(*,*) "6. Testing transaction errors:"

    success = sql_database_transaction(db)
    if (success) then
        write(*,*) "   Transaction started"

        ! Execute invalid operation
        success = sql_query_exec(query, "INVALID OPERATION")
        if (.not. success) then
            write(*,*) "   Operation failed within transaction"
        end if

        ! Try to commit (should work, but operation failed)
        success = sql_database_commit(db)
        if (.not. success) then
            write(*,*) "   Transaction commit failed"
        end if
    end if

    ! Example 7: Error recovery
    write(*,*) "7. Error recovery patterns:"

    ! Pattern 1: Check operation result and handle error
    success = sql_query_exec(query, "SELECT * FROM test_table")
    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "   Query failed, attempting recovery..."

        ! Try alternative approach or log error
        write(*,*) "   Error logged, continuing..."
    else
        write(*,*) "   Query succeeded"
    end if

    ! Pattern 2: Transaction rollback on error
    success = sql_database_transaction(db)
    if (success) then
        success = sql_query_exec(query, "INSERT INTO test_table (id, name, age) VALUES (5, 'recovery', 35)")
        if (.not. success) then
            write(*,*) "   Operation failed, rolling back transaction"
            call sql_database_rollback(db)
        else
            success = sql_database_commit(db)
            if (success) then
                write(*,*) "   Transaction committed"
            end if
        end if
    end if

    ! Clean up
    call sql_database_close(db)
    call sql_database_remove_database("sqlite_test")

    write(*,*) "Database error handling example completed."
    write(*,*) "Note: Full error text extraction requires additional string handling implementation"

end program database_error_handling_example