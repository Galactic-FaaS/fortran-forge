!> @brief SQL queries example demonstrating QSqlQuery usage
!> @details Shows how to execute various SQL queries
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_sql_queries_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    type(sql_query) :: query
    logical :: success
    integer :: row_count
    type(sql_error) :: error

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE SQL Queries Example"
    write(*,*) "========================"

    ! Connect to SQLite database
    db = sql_database_add_database("QSQLITE", "queries_example")
    call sql_database_set_database_name(db, ":memory:")

    success = sql_database_open(db)
    if (.not. success) then
        error = sql_database_last_error(db)
        write(*,*) "Failed to open database"
        stop
    end if

    ! Create table
    query = sql_query_new(db)
    success = sql_query_exec(query, "CREATE TABLE employees (" // &
                           "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
                           "name TEXT NOT NULL, " // &
                           "department TEXT, " // &
                           "salary REAL)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to create table"
        call sql_database_close(db)
        stop
    end if

    write(*,*) "1. Table created successfully"

    ! Insert data
    success = sql_query_exec(query, "INSERT INTO employees (name, department, salary) VALUES " // &
                           "('John Doe', 'Engineering', 75000.0), " // &
                           "('Jane Smith', 'Marketing', 65000.0), " // &
                           "('Bob Johnson', 'Sales', 55000.0)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to insert data"
    else
        write(*,*) "2. Data inserted successfully"
    end if

    ! Select all data
    success = sql_query_exec(query, "SELECT id, name, department, salary FROM employees")

    if (success) then
        write(*,*) "3. Query results:"
        write(*,*) "   ID | Name          | Department  | Salary"
        write(*,*) "   ---|---------------|-------------|--------"

        row_count = 0
        do while (sql_query_next(query))
            row_count = row_count + 1
            ! Note: In a real implementation, you would extract field values
            write(*,*) "   ", row_count, " | (data extraction not fully implemented)"
        end do

        write(*,*) "   Found", row_count, "rows"
    else
        error = sql_query_last_error(query)
        write(*,*) "Query failed"
    end if

    ! Prepared statement example
    success = sql_query_prepare(query, "INSERT INTO employees (name, department, salary) VALUES (?, ?, ?)")

    if (success) then
        ! Note: In a real implementation, you would bind values here
        write(*,*) "4. Prepared statement created (binding not fully implemented)"
    else
        error = sql_query_last_error(query)
        write(*,*) "Failed to prepare statement"
    end if

    ! Transaction example
    success = sql_database_transaction(db)
    if (success) then
        write(*,*) "5. Transaction started"

        ! Execute some operations within transaction
        success = sql_query_exec(query, "UPDATE employees SET salary = salary * 1.1 WHERE department = 'Engineering'")

        if (success) then
            success = sql_database_commit(db)
            if (success) then
                write(*,*) "   Transaction committed successfully"
            else
                write(*,*) "   Transaction commit failed"
                call sql_database_rollback(db)
            end if
        else
            write(*,*) "   Update failed, rolling back"
            call sql_database_rollback(db)
        end if
    else
        write(*,*) "Failed to start transaction"
    end if

    ! Clean up
    call sql_database_close(db)
    call sql_database_remove_database("queries_example")

    write(*,*) "SQL queries example completed."

end program database_sql_queries_example