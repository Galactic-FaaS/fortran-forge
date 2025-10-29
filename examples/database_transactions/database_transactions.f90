!> @brief Database transactions example demonstrating transaction management
!> @details Shows how to use transactions for data consistency
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_transactions_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    type(sql_query) :: query
    logical :: success
    type(sql_error) :: error

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE Database Transactions Example"
    write(*,*) "==================================="

    ! Connect to SQLite database
    db = sql_database_add_database("QSQLITE", "transactions_example")
    call sql_database_set_database_name(db, ":memory:")

    success = sql_database_open(db)
    if (.not. success) then
        error = sql_database_last_error(db)
        write(*,*) "Failed to open database"
        stop
    end if

    ! Create tables
    query = sql_query_new(db)
    success = sql_query_exec(query, "CREATE TABLE accounts (" // &
                           "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
                           "name TEXT NOT NULL, " // &
                           "balance REAL NOT NULL DEFAULT 0.0)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to create accounts table"
        call sql_database_close(db)
        stop
    end if

    success = sql_query_exec(query, "CREATE TABLE transfers (" // &
                           "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
                           "from_account INTEGER, " // &
                           "to_account INTEGER, " // &
                           "amount REAL, " // &
                           "timestamp TEXT, " // &
                           "FOREIGN KEY (from_account) REFERENCES accounts(id), " // &
                           "FOREIGN KEY (to_account) REFERENCES accounts(id))")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to create transfers table"
        call sql_database_close(db)
        stop
    end if

    write(*,*) "1. Tables created successfully"

    ! Insert initial account data
    success = sql_query_exec(query, "INSERT INTO accounts (name, balance) VALUES " // &
                           "('Alice', 1000.0), " // &
                           "('Bob', 500.0), " // &
                           "('Charlie', 750.0)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to insert initial data"
    else
        write(*,*) "2. Initial account data inserted"
    end if

    ! Example 1: Successful transaction
    write(*,*) "3. Performing successful transfer (Alice -> Bob, $200):"

    success = sql_database_transaction(db)
    if (success) then
        write(*,*) "   Transaction started"

        ! Debit Alice
        success = sql_query_exec(query, "UPDATE accounts SET balance = balance - 200.0 WHERE name = 'Alice'")
        if (.not. success) then
            write(*,*) "   Failed to debit Alice"
            call sql_database_rollback(db)
            goto 100
        end if

        ! Credit Bob
        success = sql_query_exec(query, "UPDATE accounts SET balance = balance + 200.0 WHERE name = 'Bob'")
        if (.not. success) then
            write(*,*) "   Failed to credit Bob"
            call sql_database_rollback(db)
            goto 100
        end if

        ! Record transfer
        success = sql_query_exec(query, "INSERT INTO transfers (from_account, to_account, amount, timestamp) " // &
                               "SELECT a1.id, a2.id, 200.0, datetime('now') " // &
                               "FROM accounts a1, accounts a2 " // &
                               "WHERE a1.name = 'Alice' AND a2.name = 'Bob'")

        if (.not. success) then
            write(*,*) "   Failed to record transfer"
            call sql_database_rollback(db)
            goto 100
        end if

        ! Commit transaction
        success = sql_database_commit(db)
        if (success) then
            write(*,*) "   Transaction committed successfully"
        else
            write(*,*) "   Transaction commit failed"
        end if

    else
        write(*,*) "   Failed to start transaction"
    end if

    100 continue

    ! Example 2: Failed transaction (insufficient funds)
    write(*,*) "4. Attempting failed transfer (Charlie -> Alice, $1000 - insufficient funds):"

    success = sql_database_transaction(db)
    if (success) then
        write(*,*) "   Transaction started"

        ! Check balance first
        success = sql_query_exec(query, "SELECT balance FROM accounts WHERE name = 'Charlie'")
        if (success .and. sql_query_next(query)) then
            ! In a real implementation, extract balance value
            write(*,*) "   Charlie's balance: (extraction not implemented)"
        end if

        ! Attempt debit (this would fail in a real scenario with insufficient funds check)
        success = sql_query_exec(query, "UPDATE accounts SET balance = balance - 1000.0 WHERE name = 'Charlie'")
        if (.not. success) then
            write(*,*) "   Insufficient funds - transaction rolled back"
            call sql_database_rollback(db)
            goto 200
        end if

        ! Credit Alice
        success = sql_query_exec(query, "UPDATE accounts SET balance = balance + 1000.0 WHERE name = 'Alice'")
        if (.not. success) then
            write(*,*) "   Failed to credit Alice"
            call sql_database_rollback(db)
            goto 200
        end if

        ! This point should not be reached in the failed case
        success = sql_database_commit(db)
        write(*,*) "   Transaction committed"

    else
        write(*,*) "   Failed to start transaction"
    end if

    200 continue

    ! Display final balances
    write(*,*) "5. Final account balances:"
    success = sql_query_exec(query, "SELECT name, balance FROM accounts ORDER BY name")

    if (success) then
        write(*,*) "   Name     | Balance"
        write(*,*) "   ----------|---------"

        do while (sql_query_next(query))
            ! In a real implementation, extract name and balance
            write(*,*) "   (data extraction not implemented)"
        end do
    end if

    ! Transaction isolation levels (concept demonstration)
    write(*,*) "6. Transaction isolation levels:"
    write(*,*) "   - READ UNCOMMITTED: Allows dirty reads"
    write(*,*) "   - READ COMMITTED: Prevents dirty reads"
    write(*,*) "   - REPEATABLE READ: Prevents non-repeatable reads"
    write(*,*) "   - SERIALIZABLE: Highest isolation level"

    ! Set transaction mode example
    call sql_database_set_transaction_mode(db, TRANSACTION_MODE_READ_WRITE)
    write(*,*) "7. Transaction mode set to READ_WRITE"

    ! Clean up
    call sql_database_close(db)
    call sql_database_remove_database("transactions_example")

    write(*,*) "Database transactions example completed."
    write(*,*) "Note: Full transaction logic requires balance checking and value extraction"

end program database_transactions_example