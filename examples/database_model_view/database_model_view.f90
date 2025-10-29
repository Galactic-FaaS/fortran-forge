!> @brief Database model/view example demonstrating QSqlTableModel usage
!> @details Shows how to integrate database tables with Qt's model/view framework
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program database_model_view_example
    use iso_c_binding
    use forge
    use forge_sql
    implicit none

    type(sql_database) :: db
    type(sql_table_model) :: model
    type(sql_query) :: query
    logical :: success
    type(sql_error) :: error
    type(qt_tableview) :: view
    type(qt_mainwindow) :: window

    ! Initialize ForGE application
    call forge_init()

    write(*,*) "ForGE Database Model/View Example"
    write(*,*) "================================="

    ! Connect to SQLite database
    db = sql_database_add_database("QSQLITE", "model_view_example")
    call sql_database_set_database_name(db, ":memory:")

    success = sql_database_open(db)
    if (.not. success) then
        error = sql_database_last_error(db)
        write(*,*) "Failed to open database"
        stop
    end if

    ! Create and populate table
    query = sql_query_new(db)
    success = sql_query_exec(query, "CREATE TABLE products (" // &
                           "id INTEGER PRIMARY KEY AUTOINCREMENT, " // &
                           "name TEXT NOT NULL, " // &
                           "category TEXT, " // &
                           "price REAL, " // &
                           "quantity INTEGER)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to create table"
        call sql_database_close(db)
        stop
    end if

    ! Insert sample data
    success = sql_query_exec(query, "INSERT INTO products (name, category, price, quantity) VALUES " // &
                           "('Laptop', 'Electronics', 999.99, 10), " // &
                           "('Mouse', 'Electronics', 29.99, 50), " // &
                           "('Book', 'Education', 19.99, 100), " // &
                           "('Chair', 'Furniture', 149.99, 25)")

    if (.not. success) then
        error = sql_query_last_error(query)
        write(*,*) "Failed to insert data"
    end if

    ! Create table model
    model = sql_table_model_new(c_null_ptr, db)
    call sql_table_model_set_table(model, "products")

    ! Set edit strategy (edit on manual submit)
    call sql_table_model_set_edit_strategy(model, EDIT_STRATEGY_ON_MANUAL_SUBMIT)

    ! Select data
    success = sql_table_model_select(model)
    if (.not. success) then
        error = sql_table_model_last_error(model)
        write(*,*) "Failed to select data"
        call sql_database_close(db)
        stop
    end if

    write(*,*) "1. Table model created and populated"

    ! Set headers
    ! Note: Header setting would be implemented in full version

    ! Create table view
    view%qobj%ptr = qt_tableview_new(c_null_ptr)

    ! Set model to view
    call qt_abstractitemview_set_model(view%qobj, model%qobj)

    write(*,*) "2. Table view created and model set"

    ! Create main window
    window%qobj%ptr = qt_mainwindow_new(c_null_ptr)
    call qt_widget_set_window_title(window%qobj, "Database Model/View Example"//c_null_char)
    call qt_mainwindow_set_central_widget(window%qobj, view%qobj%ptr)

    ! Show window
    call qt_widget_show(window%qobj)

    write(*,*) "3. Main window displayed"

    ! Demonstrate model operations
    write(*,*) "4. Model operations:"

    ! Get row count
    ! Note: Row count retrieval would be implemented in full version
    write(*,*) "   - Row count: (not implemented)"

    ! Get column count
    write(*,*) "   - Column count: (not implemented)"

    ! Filter example
    call sql_table_model_set_filter(model, "category = 'Electronics'")
    success = sql_table_model_select(model)
    if (success) then
        write(*,*) "   - Filter applied: category = 'Electronics'"
    end if

    ! Sort example
    call sql_table_model_set_sort(model, 1, 0) ! Sort by name, ascending
    success = sql_table_model_select(model)
    if (success) then
        write(*,*) "   - Sort applied: by name, ascending"
    end if

    ! Submit changes (if any)
    success = sql_table_model_submit_all(model)
    if (success) then
        write(*,*) "   - Changes submitted successfully"
    else
        write(*,*) "   - Submit failed"
    end if

    ! Clean up
    call sql_database_close(db)
    call sql_database_remove_database("model_view_example")

    write(*,*) "Database model/view example completed."
    write(*,*) "Note: Full GUI interaction requires Qt event loop"

end program database_model_view_example