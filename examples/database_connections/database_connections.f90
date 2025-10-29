!> @brief Database connection example demonstrating QSqlDatabase usage
!> @details Shows how to connect to different database types
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_connections_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    logical :: success
    type(sql_error) :: error

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE Database Connections Example"
    write(*,*) "=================================="

    ! Example 1: SQLite connection
    write(*,*) "1. SQLite Connection:"
    db = sql_database_add_database("QSQLITE", "sqlite_connection")
    call sql_database_set_database_name(db, "example.db")

    success = sql_database_open(db)
    if (success) then
        write(*,*) "   SQLite connection opened successfully"
        call sql_database_close(db)
        write(*,*) "   SQLite connection closed"
    else
        error = sql_database_last_error(db)
        write(*,*) "   SQLite connection failed"
    end if

    ! Example 2: MySQL connection (if available)
    write(*,*) "2. MySQL Connection:"
    if (sql_database_is_driver_available("QMYSQL")) then
        db = sql_database_add_database("QMYSQL", "mysql_connection")
        call sql_database_set_host_name(db, "localhost")
        call sql_database_set_database_name(db, "testdb")
        call sql_database_set_user_name(db, "user")
        call sql_database_set_password(db, "password")
        call sql_database_set_port(db, 3306)

        success = sql_database_open(db)
        if (success) then
            write(*,*) "   MySQL connection opened successfully"
            call sql_database_close(db)
            write(*,*) "   MySQL connection closed"
        else
            error = sql_database_last_error(db)
            write(*,*) "   MySQL connection failed"
        end if
    else
        write(*,*) "   MySQL driver not available"
    end if

    ! Example 3: PostgreSQL connection (if available)
    write(*,*) "3. PostgreSQL Connection:"
    if (sql_database_is_driver_available("QPSQL")) then
        db = sql_database_add_database("QPSQL", "pgsql_connection")
        call sql_database_set_host_name(db, "localhost")
        call sql_database_set_database_name(db, "testdb")
        call sql_database_set_user_name(db, "user")
        call sql_database_set_password(db, "password")
        call sql_database_set_port(db, 5432)

        success = sql_database_open(db)
        if (success) then
            write(*,*) "   PostgreSQL connection opened successfully"
            call sql_database_close(db)
            write(*,*) "   PostgreSQL connection closed"
        else
            error = sql_database_last_error(db)
            write(*,*) "   PostgreSQL connection failed"
        end if
    else
        write(*,*) "   PostgreSQL driver not available"
    end if

    ! Example 4: ODBC connection (if available)
    write(*,*) "4. ODBC Connection:"
    if (sql_database_is_driver_available("QODBC")) then
        db = sql_database_add_database("QODBC", "odbc_connection")
        call sql_database_set_database_name(db, "DSN=myodbc")

        success = sql_database_open(db)
        if (success) then
            write(*,*) "   ODBC connection opened successfully"
            call sql_database_close(db)
            write(*,*) "   ODBC connection closed"
        else
            error = sql_database_last_error(db)
            write(*,*) "   ODBC connection failed"
        end if
    else
        write(*,*) "   ODBC driver not available"
    end if

    ! List available drivers
    write(*,*) "5. Available Database Drivers:"
    ! Note: This would require implementing string list handling
    write(*,*) "   Check Qt documentation for available drivers"

    ! Clean up
    call sql_database_remove_database("sqlite_connection")
    call sql_database_remove_database("mysql_connection")
    call sql_database_remove_database("pgsql_connection")
    call sql_database_remove_database("odbc_connection")

    write(*,*) "Database connections example completed."

end program database_connections_example