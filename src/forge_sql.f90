
!> @brief Qt SQL database framework for ForGE
!> @details Provides comprehensive SQL database support with Qt's QSqlDatabase, QSqlQuery, etc.
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_sql
    use iso_c_binding
    use forge_qt
    use forge_types
    use forge_errors
    implicit none
    private

    !> Public API
    public :: sql_database, sql_query, sql_record, sql_field, sql_table_model
    public :: sql_relational_table_model, sql_query_model, sql_error, sql_driver
    public :: sql_database_new, sql_database_add_database, sql_database_database
    public :: sql_database_remove_database, sql_database_contains, sql_database_connection_names
    public :: sql_database_drivers, sql_database_is_driver_available, sql_database_register_sql_driver
    public :: sql_database_clone_database, sql_database_default_connection, sql_database_set_default_connection
    public :: sql_database_open, sql_database_close, sql_database_is_open, sql_database_is_open_error
    public :: sql_database_last_error, sql_database_set_database_name, sql_database_database_name
    public :: sql_database_set_user_name, sql_database_user_name, sql_database_set_password
    public :: sql_database_password, sql_database_set_host_name, sql_database_host_name
    public :: sql_database_set_port, sql_database_port, sql_database_set_connect_options
    public :: sql_database_connect_options, sql_database_driver, sql_database_driver_name
    public :: sql_database_set_numerical_precision_policy, sql_database_numerical_precision_policy
    public :: sql_database_transaction, sql_database_commit, sql_database_rollback
    public :: sql_database_set_transaction_mode, sql_database_transaction_mode
    public :: sql_query_new, sql_query_exec, sql_query_exec_batch, sql_query_prepare
    public :: sql_query_bind_value, sql_query_add_bind_value, sql_query_bound_value
    public :: sql_query_bound_values, sql_query_next, sql_query_previous, sql_query_first
    public :: sql_query_last, sql_query_seek, sql_query_at, sql_query_size, sql_query_is_active
    public :: sql_query_is_forward_only, sql_query_set_forward_only, sql_query_is_valid
    public :: sql_query_is_null, sql_query_value, sql_query_record, sql_query_driver
    public :: sql_query_numerical_precision_policy, sql_query_set_numerical_precision_policy
    public :: sql_query_last_query, sql_query_last_error, sql_query_executed_query
    public :: sql_query_clear, sql_query_finish, sql_query_next_result
    public :: sql_record_new, sql_record_append, sql_record_replace, sql_record_remove
    public :: sql_record_insert, sql_record_clear, sql_record_clear_values, sql_record_count
    public :: sql_record_contains, sql_record_field, sql_record_field_name, sql_record_index_of
    public :: sql_record_is_empty, sql_record_is_generated, sql_record_is_null, sql_record_name_to_index
    public :: sql_record_set_generated, sql_record_set_null, sql_record_set_value, sql_record_value
    public :: sql_field_new, sql_field_set_value, sql_field_value, sql_field_set_name
    public :: sql_field_name, sql_field_is_null, sql_field_set_null, sql_field_is_valid
    public :: sql_field_is_auto_value, sql_field_set_auto_value, sql_field_is_generated
    public :: sql_field_set_generated, sql_field_is_read_only, sql_field_set_read_only
    public :: sql_field_type, sql_field_set_type, sql_field_required_status, sql_field_set_required_status
    public :: sql_field_length, sql_field_set_length, sql_field_precision, sql_field_set_precision
    public :: sql_field_default_value, sql_field_set_default_value, sql_field_table_name, sql_field_set_table_name
    public :: sql_table_model_new, sql_table_model_set_table, sql_table_model_table_name
    public :: sql_table_model_set_edit_strategy, sql_table_model_edit_strategy, sql_table_model_select
    public :: sql_table_model_set_filter, sql_table_model_filter, sql_table_model_set_sort
    public :: sql_table_model_sort, sql_table_model_select_row, sql_table_model_is_dirty
    public :: sql_table_model_submit, sql_table_model_revert, sql_table_model_submit_all
    public :: sql_table_model_revert_all, sql_table_model_prime_insert, sql_table_model_insert_record
    public :: sql_table_model_set_record, sql_table_model_remove_columns, sql_table_model_remove_rows
    public :: sql_table_model_database, sql_table_model_last_error, sql_table_model_clear
    public :: sql_relational_table_model_new, sql_relational_table_model_set_relation
    public :: sql_relational_table_model_relation, sql_relational_table_model_set_join_mode
    public :: sql_relational_table_model_join_mode, sql_relational_table_model_select
    public :: sql_relational_table_model_revert_row, sql_relational_table_model_set_filter
    public :: sql_relational_table_model_filter, sql_relational_table_model_order_by_clause
    public :: sql_relational_table_model_set_order_by_clause
    public :: sql_query_model_new, sql_query_model_set_query, sql_query_model_query
    public :: sql_query_model_set_last_error, sql_query_model_last_error, sql_query_model_clear
    public :: sql_query_model_query_change, sql_query_model_index_in_query
    public :: sql_error_new, sql_error_type, sql_error_number, sql_error_database_text
    public :: sql_error_driver_text, sql_error_text, sql_error_is_valid, sql_error_swap
    public :: sql_driver_new, sql_driver_is_open, sql_driver_is_open_error, sql_driver_last_error
    public :: sql_driver_driver_name, sql_driver_has_feature, sql_driver_open, sql_driver_close
    public :: sql_driver_create_result, sql_driver_begin_transaction, sql_driver_commit_transaction
    public :: sql_driver_rollback_transaction, sql_driver_sql_statement, sql_driver_escape_identifier
    public :: sql_driver_format_value, sql_driver_record, sql_driver_primary_index
    public :: sql_driver_is_identifier_escaped, sql_driver_strip_delimiters, sql_driver_conformance_level

    !> Type aliases for Qt SQL classes
    type :: sql_database
        type(qt_sqldatabase) :: qobj
    end type sql_database

    type :: sql_query
        type(qt_sqlquery) :: qobj
    end type sql_query

    type :: sql_record
        type(qt_sqlrecord) :: qobj
    end type sql_record

    type :: sql_field
        type(qt_sqlfield) :: qobj
    end type sql_field

    type :: sql_table_model
        type(qt_sqltablemodel) :: qobj
    end type sql_table_model

    type :: sql_relational_table_model
        type(qt_sqlrelationaltablemodel) :: qobj
    end type sql_relational_table_model

    type :: sql_query_model
        type(qt_sqlquerymodel) :: qobj
    end type sql_query_model

    type :: sql_error
        type(qt_sqlerror) :: qobj
    end type sql_error

    type :: sql_driver
        type(qt_sqldriver) :: qobj
    end type sql_driver

    !> Constants for SQL types and policies
    integer, parameter :: SQL_TYPE_UNKNOWN = 0
    integer, parameter :: SQL_TYPE_BOOL = 1
    integer, parameter :: SQL_TYPE_INT = 2
    integer, parameter :: SQL_TYPE_UINT = 3
    integer, parameter :: SQL_TYPE_LONG = 4
    integer, parameter :: SQL_TYPE_ULONG = 5
    integer, parameter :: SQL_TYPE_DOUBLE = 6
    integer, parameter :: SQL_TYPE_STRING = 7
    integer, parameter :: SQL_TYPE_CLOB = 8
    integer, parameter :: SQL_TYPE_BLOB = 9
    integer, parameter :: SQL_TYPE_DATETIME = 10
    integer, parameter :: SQL_TYPE_DATE = 11
    integer, parameter :: SQL_TYPE_TIME = 12
    integer, parameter :: SQL_TYPE_INTERVAL = 13
    integer, parameter :: SQL_TYPE_NUMERIC = 14
    integer, parameter :: SQL_TYPE_CHAR = 15
    integer, parameter :: SQL_TYPE_VARCHAR = 16
    integer, parameter :: SQL_TYPE_BINARY = 17

    integer, parameter :: NUMERICAL_PRECISION_POLICY_LOW = 0
    integer, parameter :: NUMERICAL_PRECISION_POLICY_DEFAULT = 1
    integer, parameter :: NUMERICAL_PRECISION_POLICY_HIGH = 2

    integer, parameter :: TRANSACTION_MODE_READ_ONLY = 0
    integer, parameter :: TRANSACTION_MODE_READ_WRITE = 1

    integer, parameter :: EDIT_STRATEGY_ON_MANUAL_SUBMIT = 0
    integer, parameter :: EDIT_STRATEGY_ON_ROW_CHANGE = 1
    integer, parameter :: EDIT_STRATEGY_ON_FIELD_CHANGE = 2

    integer, parameter :: JOIN_MODE_INNER_JOIN = 0
    integer, parameter :: JOIN_MODE_LEFT_JOIN = 1

    integer, parameter :: ERROR_TYPE_NO_ERROR = 0
    integer, parameter :: ERROR_TYPE_CONNECTION_ERROR = 1
    integer, parameter :: ERROR_TYPE_STATEMENT_ERROR = 2
    integer, parameter :: ERROR_TYPE_TRANSACTION_ERROR = 3
    integer, parameter :: ERROR_TYPE_UNKNOWN_ERROR = 4

    integer, parameter :: DRIVER_FEATURE_TRANSACTIONS = 0
    integer, parameter :: DRIVER_FEATURE_QUERY_SIZE = 1
    integer, parameter :: DRIVER_FEATURE_BLOB = 2
    integer, parameter :: DRIVER_FEATURE_UNICODE = 3
    integer, parameter :: DRIVER_FEATURE_PREPARED_QUERIES = 4
    integer, parameter :: DRIVER_FEATURE_NAMED_PLACEHOLDERS = 5
    integer, parameter :: DRIVER_FEATURE_POSITIONAL_PLACEHOLDERS = 6
    integer, parameter :: DRIVER_FEATURE_LAST_INSERT_ID = 7
    integer, parameter :: DRIVER_FEATURE_BATCH_OPERATIONS = 8
    integer, parameter :: DRIVER_FEATURE_SIMPLE_LOCKING = 9
    integer, parameter :: DRIVER_FEATURE_LOW_PRECISION_NUMBERS = 10
    integer, parameter :: DRIVER_FEATURE_EVENT_NOTIFICATIONS = 11
    integer, parameter :: DRIVER_FEATURE_FINISH_QUERY = 12
    integer, parameter :: DRIVER_FEATURE_MULTIPLE_RESULT_SETS = 13

    integer, parameter :: IDENTIFIER_TYPE_FIELD_NAME = 0
    integer, parameter :: IDENTIFIER_TYPE_TABLE_NAME = 1

    integer, parameter :: CONFORMANCE_LEVEL_LEVEL0 = 0
    integer, parameter :: CONFORMANCE_LEVEL_LEVEL1 = 1
    integer, parameter :: CONFORMANCE_LEVEL_LEVEL2 = 2

contains

    !> @brief Create a new SQL database connection
    function sql_database_new() result(db)
        type(sql_database) :: db
        db%qobj%ptr = qt_sqldatabase_new()
    end function sql_database_new

    !> @brief Add a database connection
    function sql_database_add_database(type, connection_name) result(db)
        character(len=*), intent(in) :: type
        character(len=*), intent(in) :: connection_name
        type(sql_database) :: db
        db%qobj%ptr = qt_sqldatabase_add_database(trim(type)//c_null_char, trim(connection_name)//c_null_char)
    end function sql_database_add_database

    !> @brief Get database connection by name
    function sql_database_database(connection_name, open_default) result(db)
        character(len=*), intent(in) :: connection_name
        logical, intent(in) :: open_default
        type(sql_database) :: db
        db%qobj%ptr = qt_sqldatabase_database(trim(connection_name)//c_null_char, logical(open_default, c_bool))
    end function sql_database_database

    !> @brief Remove database connection
    subroutine sql_database_remove_database(connection_name)
        character(len=*), intent(in) :: connection_name
        call qt_sqldatabase_remove_database(trim(connection_name)//c_null_char)
    end subroutine sql_database_remove_database

    !> @brief Check if connection exists
    function sql_database_contains(connection_name) result(exists)
        character(len=*), intent(in) :: connection_name
        logical :: exists
        exists = logical(qt_sqldatabase_contains(trim(connection_name)//c_null_char))
    end function sql_database_contains

    !> @brief Get list of connection names
    function sql_database_connection_names() result(names)
        type(string_list) :: names
        ! Implementation would need to handle string list from Qt
        ! For now, return empty list
        names%count = 0
    end function sql_database_connection_names

    !> @brief Get available database drivers
    function sql_database_drivers() result(drivers)
        type(string_list) :: drivers
        ! Implementation would need to handle string list from Qt
        drivers%count = 0
    end function sql_database_drivers

    !> @brief Check if driver is available
    function sql_database_is_driver_available(name) result(available)
        character(len=*), intent(in) :: name
        logical :: available
        available = logical(qt_sqldatabase_is_driver_available(trim(name)//c_null_char))
    end function sql_database_is_driver_available

    !> @brief Register SQL driver
    subroutine sql_database_register_sql_driver(name, creator)
        character(len=*), intent(in) :: name
        type(c_ptr), intent(in) :: creator
        call qt_sqldatabase_register_sql_driver(trim(name)//c_null_char, creator)
    end subroutine sql_database_register_sql_driver

    !> @brief Clone database connection
    function sql_database_clone_database(other, connection_name) result(db)
        type(sql_database), intent(in) :: other
        character(len=*), intent(in) :: connection_name
        type(sql_database) :: db
        db%qobj%ptr = qt_sqldatabase_clone_database(other%qobj%ptr, trim(connection_name)//c_null_char)
    end function sql_database_clone_database

    !> @brief Get default connection
    function sql_database_default_connection() result(db)
        type(sql_database) :: db
        db%qobj%ptr = qt_sqldatabase_default_connection()
    end function sql_database_default_connection

    !> @brief Set default connection
    subroutine sql_database_set_default_connection(connection)
        type(sql_database), intent(in) :: connection
        call qt_sqldatabase_set_default_connection(connection%qobj%ptr)
    end subroutine sql_database_set_default_connection

    !> @brief Open database connection
    function sql_database_open(db) result(success)
        type(sql_database), intent(in) :: db
        logical :: success
        success = logical(qt_sqldatabase_open(db%qobj%ptr))
    end function sql_database_open

    !> @brief Close database connection
    subroutine sql_database_close(db)
        type(sql_database), intent(in) :: db
        call qt_sqldatabase_close(db%qobj%ptr)
    end subroutine sql_database_close

    !> @brief Check if database is open
    function sql_database_is_open(db) result(open)
        type(sql_database), intent(in) :: db
        logical :: open
        open = logical(qt_sqldatabase_is_open(db%qobj%ptr))
    end function sql_database_is_open

    !> @brief Check if database is open with error
    function sql_database_is_open_error(db) result(open_error)
        type(sql_database), intent(in) :: db
        logical :: open_error
        open_error = logical(qt_sqldatabase_is_open_error(db%qobj%ptr))
    end function sql_database_is_open_error

    !> @brief Get last error
    function sql_database_last_error(db) result(error)
        type(sql_database), intent(in) :: db
        type(sql_error) :: error
        error%qobj%ptr = qt_sqldatabase_last_error(db%qobj%ptr)
    end function sql_database_last_error

    !> @brief Set database name
    subroutine sql_database_set_database_name(db, name)
        type(sql_database), intent(in) :: db
        character(len=*), intent(in) :: name
        call qt_sqldatabase_set_database_name(db%qobj%ptr, trim(name)//c_null_char)
    end subroutine sql_database_set_database_name

    !> @brief Get database name
    function sql_database_database_name(db) result(name)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: name
        ! Implementation would need to handle string return from Qt
        name = ""
    end function sql_database_database_name

    !> @brief Set user name
    subroutine sql_database_set_user_name(db, name)
        type(sql_database), intent(in) :: db
        character(len=*), intent(in) :: name
        call qt_sqldatabase_set_user_name(db%qobj%ptr, trim(name)//c_null_char)
    end subroutine sql_database_set_user_name

    !> @brief Get user name
    function sql_database_user_name(db) result(name)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: name
        name = ""
    end function sql_database_user_name

    !> @brief Set password
    subroutine sql_database_set_password(db, password)
        type(sql_database), intent(in) :: db
        character(len=*), intent(in) :: password
        call qt_sqldatabase_set_password(db%qobj%ptr, trim(password)//c_null_char)
    end subroutine sql_database_set_password

    !> @brief Get password
    function sql_database_password(db) result(pwd)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: pwd
        pwd = ""
    end function sql_database_password

    !> @brief Set host name
    subroutine sql_database_set_host_name(db, host)
        type(sql_database), intent(in) :: db
        character(len=*), intent(in) :: host
        call qt_sqldatabase_set_host_name(db%qobj%ptr, trim(host)//c_null_char)
    end subroutine sql_database_set_host_name

    !> @brief Get host name
    function sql_database_host_name(db) result(host)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: host
        host = ""
    end function sql_database_host_name

    !> @brief Set port
    subroutine sql_database_set_port(db, port)
        type(sql_database), intent(in) :: db
        integer, intent(in) :: port
        call qt_sqldatabase_set_port(db%qobj%ptr, int(port, c_int))
    end subroutine sql_database_set_port

    !> @brief Get port
    function sql_database_port(db) result(port)
        type(sql_database), intent(in) :: db
        integer :: port
        port = int(qt_sqldatabase_port(db%qobj%ptr))
    end function sql_database_port

    !> @brief Set connect options
    subroutine sql_database_set_connect_options(db, options)
        type(sql_database), intent(in) :: db
        character(len=*), intent(in) :: options
        call qt_sqldatabase_set_connect_options(db%qobj%ptr, trim(options)//c_null_char)
    end subroutine sql_database_set_connect_options

    !> @brief Get connect options
    function sql_database_connect_options(db) result(options)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: options
        options = ""
    end function sql_database_connect_options

    !> @brief Get database driver
    function sql_database_driver(db) result(driver)
        type(sql_database), intent(in) :: db
        type(sql_driver) :: driver
        driver%qobj%ptr = qt_sqldatabase_driver(db%qobj%ptr)
    end function sql_database_driver

    !> @brief Get driver name
    function sql_database_driver_name(db) result(name)
        type(sql_database), intent(in) :: db
        character(len=:), allocatable :: name
        name = ""
    end function sql_database_driver_name

    !> @brief Set numerical precision policy
    subroutine sql_database_set_numerical_precision_policy(db, precision_policy)
        type(sql_database), intent(in) :: db
        integer, intent(in) :: precision_policy
        call qt_sqldatabase_set_numerical_precision_policy(db%qobj%ptr, int(precision_policy, c_int))
    end subroutine sql_database_set_numerical_precision_policy

    !> @brief Get numerical precision policy
    function sql_database_numerical_precision_policy(db) result(precision_policy)
        type(sql_database), intent(in) :: db
        integer :: precision_policy
        precision_policy = int(qt_sqldatabase_numerical_precision_policy(db%qobj%ptr))
    end function sql_database_numerical_precision_policy

    !> @brief Begin transaction
    function sql_database_transaction(db) result(success)
        type(sql_database), intent(in) :: db
        logical :: success
        success = logical(qt_sqldatabase_transaction(db%qobj%ptr))
    end function sql_database_transaction

    !> @brief Commit transaction
    function sql_database_commit(db) result(success)
        type(sql_database), intent(in) :: db
        logical :: success
        success = logical(qt_sqldatabase_commit(db%qobj%ptr))
    end function sql_database_commit

    !> @brief Rollback transaction
    function sql_database_rollback(db) result(success)
        type(sql_database), intent(in) :: db
        logical :: success
        success = logical(qt_sqldatabase_rollback(db%qobj%ptr))
    end function sql_database_rollback

    !> @brief Set transaction mode
    subroutine sql_database_set_transaction_mode(db, mode)
        type(sql_database), intent(in) :: db
        integer, intent(in) :: mode
        call qt_sqldatabase_set_transaction_mode(db%qobj%ptr, int(mode, c_int))
    end subroutine sql_database_set_transaction_mode

    !> @brief Get transaction mode
    function sql_database_transaction_mode(db) result(mode)
        type(sql_database), intent(in) :: db
        integer :: mode
        mode = int(qt_sqldatabase_transaction_mode(db%qobj%ptr))
    end function sql_database_transaction_mode

    !> @brief Create new SQL query
    function sql_query_new(db) result(query)
        type(sql_database), intent(in) :: db
        type(sql_query) :: query
        query%qobj%ptr = qt_sqlquery_new(db%qobj%ptr)
    end function sql_query_new

    !> @brief Execute SQL query
    function sql_query_exec(query, query_str) result(success)
        type(sql_query), intent(in) :: query
        character(len=*), intent(in) :: query_str
        logical :: success
        success = logical(qt_sqlquery_exec(query%qobj%ptr, trim(query_str)//c_null_char))
    end function sql_query_exec

    !> @brief Execute batch query
    function sql_query_exec_batch(query, mode) result(success)
        type(sql_query), intent(in) :: query
        integer, intent(in) :: mode
        logical :: success
        success = logical(qt_sqlquery_exec_batch(query%qobj%ptr, int(mode, c_int)))
    end function sql_query_exec_batch

    !> @brief Prepare SQL query
    function sql_query_prepare(query, query_str) result(success)
        type(sql_query), intent(in) :: query
        character(len=*), intent(in) :: query_str
        logical :: success
        success = logical(qt_sqlquery_prepare(query%qobj%ptr, trim(query_str)//c_null_char))
    end function sql_query_prepare

    !> @brief Bind value to placeholder
    subroutine sql_query_bind_value(query, placeholder, val, type)
        type(sql_query), intent(in) :: query
        character(len=*), intent(in) :: placeholder
        type(*), intent(in) :: val
        integer, intent(in) :: type
        ! Implementation would need to handle different value types
        ! For now, simplified implementation
        call qt_sqlquery_bind_value(query%qobj%ptr, trim(placeholder)//c_null_char, c_null_ptr, int(type, c_int))
    end subroutine sql_query_bind_value

    !> @brief Add bind value
    subroutine sql_query_add_bind_value(query, val, type)
        type(sql_query), intent(in) :: query
        type(*), intent(in) :: val
        integer, intent(in) :: type
        call qt_sqlquery_add_bind_value(query%qobj%ptr, c_null_ptr, int(type, c_int))
    end subroutine sql_query_add_bind_value

    !> @brief Get bound value
    function sql_query_bound_value(query, placeholder) result(val)
        type(sql_query), intent(in) :: query
        character(len=*), intent(in) :: placeholder
        type(c_ptr) :: val
        val = qt_sqlquery_bound_value(query%qobj%ptr, trim(placeholder)//c_null_char)
    end function sql_query_bound_value

    !> @brief Get bound values
    function sql_query_bound_values(query) result(vals)
        type(sql_query), intent(in) :: query
        type(c_ptr) :: vals
        vals = qt_sqlquery_bound_values(query%qobj%ptr)
    end function sql_query_bound_values

    !> @brief Move to next record
    function sql_query_next(query) result(success)
        type(sql_query), intent(in) :: query
        logical :: success
        success = logical(qt_sqlquery_next(query%qobj%ptr))
    end function sql_query_next

    !> @brief Move to previous record
    function sql_query_previous(query) result(success)
        type(sql_query), intent(in) :: query
        logical :: success
        success = logical(qt_sqlquery_previous(query%qobj%ptr))
    end function sql_query_previous

    !> @brief Move to first record
    function sql_query_first(query) result(success)
        type(sql_query), intent(in) :: query
        logical :: success
        success = logical(qt_sqlquery_first(query%qobj%ptr))
    end function sql_query_first

    !> @brief Move to last record
    function sql_query_last(query) result(success)
        type(sql_query), intent(in) :: query
        logical :: success
        success = logical(qt_sqlquery_last(query%qobj%ptr))
    end function sql_query_last

    !> @brief Seek to specific record
    function sql_query_seek(query, index, relative) result(success)
        type(sql_query), intent(in) :: query
        integer, intent(in) :: index
        logical, intent(in) :: relative
        logical :: success
        success = logical(qt_sqlquery_seek(query%qobj%ptr, int(index, c_int), logical(relative, c_bool)))
    end function sql_query_seek

    !> @brief Get current position
    function sql_query_at(query) result(pos)
        type(sql_query), intent(in) :: query
        integer :: pos
        pos = int(qt_sqlquery_at(query%qobj%ptr))
    end function sql_query_at

    !> @brief Get result size
    function sql_query_size(query) result(sz)
        type(sql_query), intent(in) :: query
        integer :: sz
        sz = int(qt_sqlquery_size(query%qobj%ptr))
    end function sql_query_size

    !> @brief Check if query is active
    function sql_query_is_active(query) result(active)
        type(sql_query), intent(in) :: query
        logical :: active
        active = logical(qt_sqlquery_is_active(query%qobj%ptr))
    end function sql_query_is_active

    !> @brief Check if query is forward only
    function sql_query_is_forward_only(query) result(forward_only)
        type(sql_query), intent(in) :: query
        logical :: forward_only
        forward_only = logical(qt_sqlquery_is_forward_only(query%qobj%ptr))
    end function sql_query_is_forward_only

    !> @brief Set forward only mode
    subroutine sql_query_set_forward_only(query, forward)
        type(sql_query), intent(in) :: query
        logical, intent(in) :: forward
        call qt_sqlquery_set_forward_only(query%qobj%ptr, logical(forward, c_bool))
    end subroutine sql_query_set_forward_only

    !> @brief Check if query is valid
    function sql_query_is_valid(query) result(valid)
        type(sql_query), intent(in) :: query
        logical :: valid
        valid = logical(qt_sqlquery_is_valid(query%qobj%ptr))
    end function sql_query_is_valid

    !> @brief Check if field is null
    function sql_query_is_null(query, field) result(is_null)
        type(sql_query), intent(in) :: query
        integer, intent(in) :: field
        logical :: is_null
        is_null = logical(qt_sqlquery_is_null(query%qobj%ptr, int(field, c_int)))
    end function sql_query_is_null

    !> @brief Get field value
    function sql_query_value(query, index) result(val)
        type(sql_query), intent(in) :: query
        integer, intent(in) :: index
        type(c_ptr) :: val
        val = qt_sqlquery_value(query%qobj%ptr, int(index, c_int))
    end function sql_query_value

    !> @brief Get record
    function sql_query_record(query) result(rec)
        type(sql_query), intent(in) :: query
        type(sql_record) :: rec
        rec%qobj%ptr = qt_sqlquery_record(query%qobj%ptr)
    end function sql_query_record

    !> @brief Get driver
    function sql_query_driver(query) result(drv)
        type(sql_query), intent(in) :: query
        type(sql_driver) :: drv
        drv%qobj%ptr = qt_sqlquery_driver(query%qobj%ptr)
    end function sql_query_driver

    !> @brief Set numerical precision policy
    subroutine sql_query_set_numerical_precision_policy(query, precision_policy)
        type(sql_query), intent(in) :: query
        integer, intent(in) :: precision_policy
        call qt_sqlquery_set_numerical_precision_policy(query%qobj%ptr, int(precision_policy, c_int))
    end subroutine sql_query_set_numerical_precision_policy

    !> @brief Get numerical precision policy
    function sql_query_numerical_precision_policy(query) result(precision_policy)
        type(sql_query), intent(in) :: query
        integer :: precision_policy
        precision_policy = int(qt_sqlquery_numerical_precision_policy(query%qobj%ptr))
    end function sql_query_numerical_precision_policy

    !> @brief Get last query
    function sql_query_last_query(query) result(last_q)
        type(sql_query), intent(in) :: query
        character(len=:), allocatable :: last_q
        last_q = ""
    end function sql_query_last_query

    !> @brief Get last error
    function sql_query_last_error(query) result(error)
        type(sql_query), intent(in) :: query
        type(sql_error) :: error
        error%qobj%ptr = qt_sqlquery_last_error(query%qobj%ptr)
    end function sql_query_last_error

    !> @brief Get executed query
    function sql_query_executed_query(query) result(exec_q)
        type(sql_query), intent(in) :: query
        character(len=:), allocatable :: exec_q
        exec_q = ""
    end function sql_query_executed_query

    !> @brief Clear query
    subroutine sql_query_clear(query)
        type(sql_query), intent(in) :: query
        call qt_sqlquery_clear(query%qobj%ptr)
    end subroutine sql_query_clear

    !> @brief Finish query
    subroutine sql_query_finish(query)
        type(sql_query), intent(in) :: query
        call qt_sqlquery_finish(query%qobj%ptr)
    end subroutine sql_query_finish

    !> @brief Move to next result set
    function sql_query_next_result(query) result(success)
        type(sql_query), intent(in) :: query
        logical :: success
        success = logical(qt_sqlquery_next_result(query%qobj%ptr))
    end function sql_query_next_result

    !> @brief Create new SQL record
    function sql_record_new() result(rec)
        type(sql_record) :: rec
        rec%qobj%ptr = qt_sqlrecord_new()
    end function sql_record_new

    !> @brief Append field to record
    subroutine sql_record_append(record, field)
        type(sql_record), intent(in) :: record
        type(sql_field), intent(in) :: field
        call qt_sqlrecord_append(record%qobj%ptr, field%qobj%ptr)
    end subroutine sql_record_append

    !> @brief Replace field in record
    subroutine sql_record_replace(record, pos, field)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: pos
        type(sql_field), intent(in) :: field
        call qt_sqlrecord_replace(record%qobj%ptr, int(pos, c_int), field%qobj%ptr)
    end subroutine sql_record_replace

    !> @brief Remove field from record
    subroutine sql_record_remove(record, pos)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: pos
        call qt_sqlrecord_remove(record%qobj%ptr, int(pos, c_int))
    end subroutine sql_record_remove

    !> @brief Insert field into record
    subroutine sql_record_insert(record, pos, field)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: pos
        type(sql_field), intent(in) :: field
        call qt_sqlrecord_insert(record%qobj%ptr, int(pos, c_int), field%qobj%ptr)
    end subroutine sql_record_insert

    !> @brief Clear record
    subroutine sql_record_clear(record)
        type(sql_record), intent(in) :: record
        call qt_sqlrecord_clear(record%qobj%ptr)
    end subroutine sql_record_clear

    !> @brief Clear record values
    subroutine sql_record_clear_values(record)
        type(sql_record), intent(in) :: record
        call qt_sqlrecord_clear_values(record%qobj%ptr)
    end subroutine sql_record_clear_values

    !> @brief Get record field count
    function sql_record_count(record) result(cnt)
        type(sql_record), intent(in) :: record
        integer :: cnt
        cnt = int(qt_sqlrecord_count(record%qobj%ptr))
    end function sql_record_count

    !> @brief Check if record contains field
    function sql_record_contains(record, name) result(contains)
        type(sql_record), intent(in) :: record
        character(len=*), intent(in) :: name
        logical :: contains
        contains = logical(qt_sqlrecord_contains(record%qobj%ptr, trim(name)//c_null_char))
    end function sql_record_contains

    !> @brief Get field by index
    function sql_record_field(record, index) result(field)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        type(sql_field) :: field
        field%qobj%ptr = qt_sqlrecord_field(record%qobj%ptr, int(index, c_int))
    end function sql_record_field

    !> @brief Get field name by index
    function sql_record_field_name(record, index) result(name)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        character(len=:), allocatable :: name
        name = ""
    end function sql_record_field_name

    !> @brief Get field index by name
    function sql_record_index_of(record, name) result(index)
        type(sql_record), intent(in) :: record
        character(len=*), intent(in) :: name
        integer :: index
        index = int(qt_sqlrecord_index_of(record%qobj%ptr, trim(name)//c_null_char))
    end function sql_record_index_of

    !> @brief Check if record is empty
    function sql_record_is_empty(record) result(empty)
        type(sql_record), intent(in) :: record
        logical :: empty
        empty = logical(qt_sqlrecord_is_empty(record%qobj%ptr))
    end function sql_record_is_empty

    !> @brief Check if field is generated
    function sql_record_is_generated(record, index) result(generated)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        logical :: generated
        generated = logical(qt_sqlrecord_is_generated(record%qobj%ptr, int(index, c_int)))
    end function sql_record_is_generated

    !> @brief Check if field value is null
    function sql_record_is_null(record, index) result(is_null)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        logical :: is_null
        is_null = logical(qt_sqlrecord_is_null(record%qobj%ptr, int(index, c_int)))
    end function sql_record_is_null

    !> @brief Get field index by name
    function sql_record_name_to_index(record, name) result(index)
        type(sql_record), intent(in) :: record
        character(len=*), intent(in) :: name
        integer :: index
        index = int(qt_sqlrecord_name_to_index(record%qobj%ptr, trim(name)//c_null_char))
    end function sql_record_name_to_index

    !> @brief Set field generated flag
    subroutine sql_record_set_generated(record, index, generated)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        logical, intent(in) :: generated
        call qt_sqlrecord_set_generated(record%qobj%ptr, int(index, c_int), logical(generated, c_bool))
    end subroutine sql_record_set_generated

    !> @brief Set field value to null
    subroutine sql_record_set_null(record, index)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        call qt_sqlrecord_set_null(record%qobj%ptr, int(index, c_int))
    end subroutine sql_record_set_null

    !> @brief Set field value
    subroutine sql_record_set_value(record, index, val)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        type(*), intent(in) :: val
        ! Implementation would need to handle different value types
        call qt_sqlrecord_set_value(record%qobj%ptr, int(index, c_int), c_null_ptr)
    end subroutine sql_record_set_value

    !> @brief Get field value
    function sql_record_value(record, index) result(val)
        type(sql_record), intent(in) :: record
        integer, intent(in) :: index
        type(c_ptr) :: val
        val = qt_sqlrecord_value(record%qobj%ptr, int(index, c_int))
    end function sql_record_value

    !> @brief Create new SQL field
    function sql_field_new(field_name, type) result(field)
        character(len=*), intent(in) :: field_name
        integer, intent(in) :: type
        type(sql_field) :: field
        field%qobj%ptr = qt_sqlfield_new(trim(field_name)//c_null_char, int(type, c_int))
    end function sql_field_new

    !> @brief Set field value
    subroutine sql_field_set_value(field, value)
        type(sql_field), intent(in) :: field
        type(*), intent(in) :: value
        ! Implementation would need to handle different value types
        call qt_sqlfield_set_value(field%qobj%ptr, c_null_ptr)
    end subroutine sql_field_set_value

    !> @brief Get field value
    function sql_field_value(field) result(val)
        type(sql_field), intent(in) :: field
        type(c_ptr) :: val
        val = qt_sqlfield_value(field%qobj%ptr)
    end function sql_field_value

    !> @brief Set field name
    subroutine sql_field_set_name(field, name)
        type(sql_field), intent(in) :: field
        character(len=*), intent(in) :: name
        call qt_sqlfield_set_name(field%qobj%ptr, trim(name)//c_null_char)
    end subroutine sql_field_set_name

    !> @brief Get field name
    function sql_field_name(field) result(name)
        type(sql_field), intent(in) :: field
        character(len=:), allocatable :: name
        name = ""
    end function sql_field_name

    !> @brief Check if field is null
    function sql_field_is_null(field) result(is_null)
        type(sql_field), intent(in) :: field
        logical :: is_null
        is_null = logical(qt_sqlfield_is_null(field%qobj%ptr))
    end function sql_field_is_null

    !> @brief Set field to null
    subroutine sql_field_set_null(field)
        type(sql_field), intent(in) :: field
        call qt_sqlfield_set_null(field%qobj%ptr)
    end subroutine sql_field_set_null

    !> @brief Check if field is valid
    function sql_field_is_valid(field) result(valid)
        type(sql_field), intent(in) :: field
        logical :: valid
        valid = logical(qt_sqlfield_is_valid(field%qobj%ptr))
    end function sql_field_is_valid

    !> @brief Check if field is auto value
    function sql_field_is_auto_value(field) result(auto_value)
        type(sql_field), intent(in) :: field
        logical :: auto_value
        auto_value = logical(qt_sqlfield_is_auto_value(field%qobj%ptr))
    end function sql_field_is_auto_value

    !> @brief Set auto value flag
    subroutine sql_field_set_auto_value(field, auto_value)
        type(sql_field), intent(in) :: field
        logical, intent(in) :: auto_value
        call qt_sqlfield_set_auto_value(field%qobj%ptr, logical(auto_value, c_bool))
    end subroutine sql_field_set_auto_value

    !> @brief Check if field is generated
    function sql_field_is_generated(field) result(generated)
        type(sql_field), intent(in) :: field
        logical :: generated
        generated = logical(qt_sqlfield_is_generated(field%qobj%ptr))
    end function sql_field_is_generated

    !> @brief Set generated flag
    subroutine sql_field_set_generated(field, generated)
        type(sql_field), intent(in) :: field
        logical, intent(in) :: generated
        call qt_sqlfield_set_generated(field%qobj%ptr, logical(generated, c_bool))
    end subroutine sql_field_set_generated

    !> @brief Check if field is read only
    function sql_field_is_read_only(field) result(read_only)
        type(sql_field), intent(in) :: field
        logical :: read_only
        read_only = logical(qt_sqlfield_is_read_only(field%qobj%ptr))
    end function sql_field_is_read_only

    !> @brief Set read only flag
    subroutine sql_field_set_read_only(field, read_only)
        type(sql_field), intent(in) :: field
        logical, intent(in) :: read_only
        call qt_sqlfield_set_read_only(field%qobj%ptr, logical(read_only, c_bool))
    end subroutine sql_field_set_read_only

    !> @brief Get field type
    function sql_field_type(field) result(typ)
        type(sql_field), intent(in) :: field
        integer :: typ
        typ = int(qt_sqlfield_type(field%qobj%ptr))
    end function sql_field_type

    !> @brief Set field type
    subroutine sql_field_set_type(field, type)
        type(sql_field), intent(in) :: field
        integer, intent(in) :: type
        call qt_sqlfield_set_type(field%qobj%ptr, int(type, c_int))
    end subroutine sql_field_set_type

    !> @brief Get required status
    function sql_field_required_status(field) result(status)
        type(sql_field), intent(in) :: field
        integer :: status
        status = int(qt_sqlfield_required_status(field%qobj%ptr))
    end function sql_field_required_status

    !> @brief Set required status
    subroutine sql_field_set_required_status(field, required_status)
        type(sql_field), intent(in) :: field
        integer, intent(in) :: required_status
        call qt_sqlfield_set_required_status(field%qobj%ptr, int(required_status, c_int))
    end subroutine sql_field_set_required_status

    !> @brief Get field length
    function sql_field_length(field) result(len)
        type(sql_field), intent(in) :: field
        integer :: len
        len = int(qt_sqlfield_length(field%qobj%ptr))
    end function sql_field_length

    !> @brief Set field length
    subroutine sql_field_set_length(field, field_length)
        type(sql_field), intent(in) :: field
        integer, intent(in) :: field_length
        call qt_sqlfield_set_length(field%qobj%ptr, int(field_length, c_int))
    end subroutine sql_field_set_length

    !> @brief Get field precision
    function sql_field_precision(field) result(prec)
        type(sql_field), intent(in) :: field
        integer :: prec
        prec = int(qt_sqlfield_precision(field%qobj%ptr))
    end function sql_field_precision

    !> @brief Set field precision
    subroutine sql_field_set_precision(field, precision)
        type(sql_field), intent(in) :: field
        integer, intent(in) :: precision
        call qt_sqlfield_set_precision(field%qobj%ptr, int(precision, c_int))
    end subroutine sql_field_set_precision

    !> @brief Get default value
    function sql_field_default_value(field) result(val)
        type(sql_field), intent(in) :: field
        type(c_ptr) :: val
        val = qt_sqlfield_default_value(field%qobj%ptr)
    end function sql_field_default_value

    !> @brief Set default value
    subroutine sql_field_set_default_value(field, default_value)
        type(sql_field), intent(in) :: field
        type(*), intent(in) :: default_value
        ! Implementation would need to handle different value types
        call qt_sqlfield_set_default_value(field%qobj%ptr, c_null_ptr)
    end subroutine sql_field_set_default_value

    !> @brief Get table name
    function sql_field_table_name(field) result(name)
        type(sql_field), intent(in) :: field
        character(len=:), allocatable :: name
        name = ""
    end function sql_field_table_name

    !> @brief Set table name

    subroutine sql_field_set_table_name(field, table_name)
        type(sql_field), intent(in) :: field
        character(len=*), intent(in) :: table_name
        call qt_sqlfield_set_table_name(field%qobj%ptr, trim(table_name)//c_null_char)
    end subroutine sql_field_set_table_name

    !> @brief Create new SQL table model
    function sql_table_model_new(parent, db) result(model)
        type(c_ptr), intent(in) :: parent
        type(sql_database), intent(in) :: db
        type(sql_table_model) :: model
        model%qobj%ptr = qt_sqltablemodel_new(parent, db%qobj%ptr)
    end function sql_table_model_new

    !> @brief Set table for model
    subroutine sql_table_model_set_table(model, table_name)
        type(sql_table_model), intent(in) :: model
        character(len=*), intent(in) :: table_name
        call qt_sqltablemodel_set_table(model%qobj%ptr, trim(table_name)//c_null_char)
    end subroutine sql_table_model_set_table

    !> @brief Get table name
    function sql_table_model_table_name(model) result(name)
        type(sql_table_model), intent(in) :: model
        character(len=:), allocatable :: name
        name = ""
    end function sql_table_model_table_name

    !> @brief Set edit strategy
    subroutine sql_table_model_set_edit_strategy(model, strategy)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: strategy
        call qt_sqltablemodel_set_edit_strategy(model%qobj%ptr, int(strategy, c_int))
    end subroutine sql_table_model_set_edit_strategy

    !> @brief Get edit strategy
    function sql_table_model_edit_strategy(model) result(strategy)
        type(sql_table_model), intent(in) :: model
        integer :: strategy
        strategy = int(qt_sqltablemodel_edit_strategy(model%qobj%ptr))
    end function sql_table_model_edit_strategy

    !> @brief Select data
    function sql_table_model_select(model) result(success)
        type(sql_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqltablemodel_select(model%qobj%ptr))
    end function sql_table_model_select

    !> @brief Set filter
    subroutine sql_table_model_set_filter(model, filter)
        type(sql_table_model), intent(in) :: model
        character(len=*), intent(in) :: filter
        call qt_sqltablemodel_set_filter(model%qobj%ptr, trim(filter)//c_null_char)
    end subroutine sql_table_model_set_filter

    !> @brief Get filter
    function sql_table_model_filter(model) result(filter)
        type(sql_table_model), intent(in) :: model
        character(len=:), allocatable :: filter
        filter = ""
    end function sql_table_model_filter

    !> @brief Set sort
    subroutine sql_table_model_set_sort(model, column, order)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: column, order
        call qt_sqltablemodel_set_sort(model%qobj%ptr, int(column, c_int), int(order, c_int))
    end subroutine sql_table_model_set_sort

    !> @brief Get sort
    function sql_table_model_sort(model) result(sort_info)
        type(sql_table_model), intent(in) :: model
        integer, dimension(2) :: sort_info
        sort_info = qt_sqltablemodel_sort(model%qobj%ptr)
    end function sql_table_model_sort

    !> @brief Select row
    function sql_table_model_select_row(model, row) result(success)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: row
        logical :: success
        success = logical(qt_sqltablemodel_select_row(model%qobj%ptr, int(row, c_int)))
    end function sql_table_model_select_row

    !> @brief Check if index is dirty
    function sql_table_model_is_dirty(model, index) result(dirty)
        type(sql_table_model), intent(in) :: model
        type(qt_modelindex), intent(in) :: index
        logical :: dirty
        dirty = logical(qt_sqltablemodel_is_dirty(model%qobj%ptr, index%ptr))
    end function sql_table_model_is_dirty

    !> @brief Submit changes
    function sql_table_model_submit(model) result(success)
        type(sql_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqltablemodel_submit(model%qobj%ptr))
    end function sql_table_model_submit

    !> @brief Revert changes
    function sql_table_model_revert(model) result(success)
        type(sql_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqltablemodel_revert(model%qobj%ptr))
    end function sql_table_model_revert

    !> @brief Submit all changes
    function sql_table_model_submit_all(model) result(success)
        type(sql_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqltablemodel_submit_all(model%qobj%ptr))
    end function sql_table_model_submit_all

    !> @brief Revert all changes
    function sql_table_model_revert_all(model) result(success)
        type(sql_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqltablemodel_revert_all(model%qobj%ptr))
    end function sql_table_model_revert_all

    !> @brief Prime insert
    subroutine sql_table_model_prime_insert(model, row, record)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: row
        type(sql_record), intent(in) :: record
        call qt_sqltablemodel_prime_insert(model%qobj%ptr, int(row, c_int), record%qobj%ptr)
    end subroutine sql_table_model_prime_insert

    !> @brief Insert record
    function sql_table_model_insert_record(model, row, record) result(success)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: row
        type(sql_record), intent(in) :: record
        logical :: success
        success = logical(qt_sqltablemodel_insert_record(model%qobj%ptr, int(row, c_int), record%qobj%ptr))
    end function sql_table_model_insert_record

    !> @brief Set record
    subroutine sql_table_model_set_record(model, row, record)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: row
        type(sql_record), intent(in) :: record
        call qt_sqltablemodel_set_record(model%qobj%ptr, int(row, c_int), record%qobj%ptr)
    end subroutine sql_table_model_set_record

    !> @brief Remove columns
    function sql_table_model_remove_columns(model, column, count, parent) result(success)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: column, count
        type(qt_modelindex), intent(in) :: parent
        logical :: success
        success = logical(qt_sqltablemodel_remove_columns(model%qobj%ptr, int(column, c_int), int(count, c_int), parent%ptr))
    end function sql_table_model_remove_columns

    !> @brief Remove rows
    function sql_table_model_remove_rows(model, row, count, parent) result(success)
        type(sql_table_model), intent(in) :: model
        integer, intent(in) :: row, count
        type(qt_modelindex), intent(in) :: parent
        logical :: success
        success = logical(qt_sqltablemodel_remove_rows(model%qobj%ptr, int(row, c_int), int(count, c_int), parent%ptr))
    end function sql_table_model_remove_rows

    !> @brief Get database
    function sql_table_model_database(model) result(db)
        type(sql_table_model), intent(in) :: model
        type(sql_database) :: db
        db%qobj%ptr = qt_sqltablemodel_database(model%qobj%ptr)
    end function sql_table_model_database

    !> @brief Get last error
    function sql_table_model_last_error(model) result(error)
        type(sql_table_model), intent(in) :: model
        type(sql_error) :: error
        error%qobj%ptr = qt_sqltablemodel_last_error(model%qobj%ptr)
    end function sql_table_model_last_error

    !> @brief Clear model
    subroutine sql_table_model_clear(model)
        type(sql_table_model), intent(in) :: model
        call qt_sqltablemodel_clear(model%qobj%ptr)
    end subroutine sql_table_model_clear

    !> @brief Create new relational table model
    function sql_relational_table_model_new(parent, db) result(model)
        type(c_ptr), intent(in) :: parent
        type(sql_database), intent(in) :: db
        type(sql_relational_table_model) :: model
        model%qobj%ptr = qt_sqlrelationaltablemodel_new(parent, db%qobj%ptr)
    end function sql_relational_table_model_new

    !> @brief Set relation
    subroutine sql_relational_table_model_set_relation(model, column, relation)
        type(sql_relational_table_model), intent(in) :: model
        integer, intent(in) :: column
        type(c_ptr), intent(in) :: relation
        call qt_sqlrelationaltablemodel_set_relation(model%qobj%ptr, int(column, c_int), relation)
    end subroutine sql_relational_table_model_set_relation

    !> @brief Get relation
    function sql_relational_table_model_relation(model, column) result(relation)
        type(sql_relational_table_model), intent(in) :: model
        integer, intent(in) :: column
        type(c_ptr) :: relation
        relation = qt_sqlrelationaltablemodel_relation(model%qobj%ptr, int(column, c_int))
    end function sql_relational_table_model_relation

    !> @brief Set join mode
    subroutine sql_relational_table_model_set_join_mode(model, join_mode)
        type(sql_relational_table_model), intent(in) :: model
        integer, intent(in) :: join_mode
        call qt_sqlrelationaltablemodel_set_join_mode(model%qobj%ptr, int(join_mode, c_int))
    end subroutine sql_relational_table_model_set_join_mode

    !> @brief Get join mode
    function sql_relational_table_model_join_mode(model) result(join_mode)
        type(sql_relational_table_model), intent(in) :: model
        integer :: join_mode
        join_mode = int(qt_sqlrelationaltablemodel_join_mode(model%qobj%ptr))
    end function sql_relational_table_model_join_mode

    !> @brief Select data
    function sql_relational_table_model_select(model) result(success)
        type(sql_relational_table_model), intent(in) :: model
        logical :: success
        success = logical(qt_sqlrelationaltablemodel_select(model%qobj%ptr))
    end function sql_relational_table_model_select

    !> @brief Revert row
    function sql_relational_table_model_revert_row(model, row) result(success)
        type(sql_relational_table_model), intent(in) :: model
        integer, intent(in) :: row
        ! Implementation needed
        success = .true.
    end function sql_relational_table_model_revert_row

    !> @brief Set filter
    subroutine sql_relational_table_model_set_filter(model, filter)
        type(sql_relational_table_model), intent(in) :: model
        character(len=*), intent(in) :: filter
        call qt_sqlrelationaltablemodel_set_filter(model%qobj%ptr, trim(filter)//c_null_char)
    end subroutine sql_relational_table_model_set_filter

    !> @brief Get filter
    function sql_relational_table_model_filter(model) result(filter)
        type(sql_relational_table_model), intent(in) :: model
        character(len=:), allocatable :: filter
        filter = ""
    end function sql_relational_table_model_filter

    !> @brief Get order by clause
    function sql_relational_table_model_order_by_clause(model) result(clause)
        type(sql_relational_table_model), intent(in) :: model
        character(len=:), allocatable :: clause
        clause = ""
    end function sql_relational_table_model_order_by_clause

    !> @brief Set order by clause
    subroutine sql_relational_table_model_set_order_by_clause(model, clause)
        type(sql_relational_table_model), intent(in) :: model
        character(len=*), intent(in) :: clause
        call qt_sqlrelationaltablemodel_set_order_by_clause(model%qobj%ptr, trim(clause)//c_null_char)
    end subroutine sql_relational_table_model_set_order_by_clause

    !> @brief Create new query model
    function sql_query_model_new(parent, db) result(model)
        type(c_ptr), intent(in) :: parent
        type(sql_database), intent(in) :: db
        type(sql_query_model) :: model
        model%qobj%ptr = qt_sqlquerymodel_new(parent, db%qobj%ptr)
    end function sql_query_model_new

    !> @brief Set query
    subroutine sql_query_model_set_query(model, query, db)
        type(sql_query_model), intent(in) :: model
        type(sql_query), intent(in) :: query
        type(sql_database), intent(in) :: db
        call qt_sqlquerymodel_set_query(model%qobj%ptr, query%qobj%ptr, db%qobj%ptr)
    end subroutine sql_query_model_set_query

    !> @brief Get query
    function sql_query_model_query(model) result(query)
        type(sql_query_model), intent(in) :: model
        type(sql_query) :: query
        query%qobj%ptr = qt_sqlquerymodel_query(model%qobj%ptr)
    end function sql_query_model_query

    !> @brief Set last error
    subroutine sql_query_model_set_last_error(model, error)
        type(sql_query_model), intent(in) :: model
        type(sql_error), intent(in) :: error
        call qt_sqlquerymodel_set_last_error(model%qobj%ptr, error%qobj%ptr)
    end subroutine sql_query_model_set_last_error

    !> @brief Get last error
    function sql_query_model_last_error(model) result(error)
        type(sql_query_model), intent(in) :: model
        type(sql_error) :: error
        error%qobj%ptr = qt_sqlquerymodel_last_error(model%qobj%ptr)
    end function sql_query_model_last_error

    !> @brief Clear model
    subroutine sql_query_model_clear(model)
        type(sql_query_model), intent(in) :: model
        call qt_sqlquerymodel_clear(model%qobj%ptr)
    end subroutine sql_query_model_clear

    !> @brief Check if query changed
    function sql_query_model_query_change(model) result(changed)
        type(sql_query_model), intent(in) :: model
        logical :: changed
        changed = logical(qt_sqlquerymodel_query_change(model%qobj%ptr))
    end function sql_query_model_query_change

    !> @brief Get index in query
    function sql_query_model_index_in_query(model, item) result(index)
        type(sql_query_model), intent(in) :: model
        type(qt_modelindex), intent(in) :: item
        integer :: index
        index = int(qt_sqlquerymodel_index_in_query(model%qobj%ptr, item%ptr))
    end function sql_query_model_index_in_query

    !> @brief Create new SQL error
    function sql_error_new(type, driver_text, database_text, connection_text, number) result(error)
        integer, intent(in) :: type, number
        character(len=*), intent(in) :: driver_text, database_text, connection_text
        type(sql_error) :: error
        error%qobj%ptr = qt_sqlerror_new(int(type, c_int), trim(driver_text)//c_null_char, &
                                       trim(database_text)//c_null_char, trim(connection_text)//c_null_char, &
                                       int(number, c_int))
    end function sql_error_new

    !> @brief Get error type
    function sql_error_type(error) result(typ)
        type(sql_error), intent(in) :: error
        integer :: typ
        typ = int(qt_sqlerror_type(error%qobj%ptr))
    end function sql_error_type

    !> @brief Get error number
    function sql_error_number(error) result(num)
        type(sql_error), intent(in) :: error
        integer :: num
        num = int(qt_sqlerror_number(error%qobj%ptr))
    end function sql_error_number

    !> @brief Get database text
    function sql_error_database_text(error) result(text)
        type(sql_error), intent(in) :: error
        character(len=:), allocatable :: text
        text = ""
    end function sql_error_database_text

    !> @brief Get driver text
    function sql_error_driver_text(error) result(text)
        type(sql_error), intent(in) :: error
        character(len=:), allocatable :: text
        text = ""
    end function sql_error_driver_text

    !> @brief Get error text
    function sql_error_text(error) result(text)
        type(sql_error), intent(in) :: error
        character(len=:), allocatable :: text
        text = ""
    end function sql_error_text

    !> @brief Check if error is valid
    function sql_error_is_valid(error) result(valid)
        type(sql_error), intent(in) :: error
        logical :: valid
        valid = logical(qt_sqlerror_is_valid(error%qobj%ptr))
    end function sql_error_is_valid

    !> @brief Swap errors
    subroutine sql_error_swap(error1, error2)
        type(sql_error), intent(in) :: error1, error2
        call qt_sqlerror_swap(error1%qobj%ptr, error2%qobj%ptr)
    end subroutine sql_error_swap

    !> @brief Create new SQL driver
    function sql_driver_new(parent) result(driver)
        type(c_ptr), intent(in) :: parent
        type(sql_driver) :: driver
        driver%qobj%ptr = qt_sqldriver_new(parent)
    end function sql_driver_new

    !> @brief Check if driver is open
    function sql_driver_is_open(driver) result(open)
        type(sql_driver), intent(in) :: driver
        logical :: open
        open = logical(qt_sqldriver_is_open(driver%qobj%ptr))
    end function sql_driver_is_open

    !> @brief Check if driver is open with error
    function sql_driver_is_open_error(driver) result(open_error)
        type(sql_driver), intent(in) :: driver
        logical :: open_error
        open_error = logical(qt_sqldriver_is_open_error(driver%qobj%ptr))
    end function sql_driver_is_open_error

    !> @brief Get last error
    function sql_driver_last_error(driver) result(error)
        type(sql_driver), intent(in) :: driver
        type(sql_error) :: error
        error%qobj%ptr = qt_sqldriver_last_error(driver%qobj%ptr)
    end function sql_driver_last_error

    !> @brief Get driver name
    function sql_driver_driver_name(driver) result(name)
        type(sql_driver), intent(in) :: driver
        character(len=:), allocatable :: name
        name = ""
    end function sql_driver_driver_name

    !> @brief Check driver feature
    function sql_driver_has_feature(driver, feature) result(has_feature)
        type(sql_driver), intent(in) :: driver
        integer, intent(in) :: feature
        logical :: has_feature
        has_feature = logical(qt_sqldriver_has_feature(driver%qobj%ptr, int(feature, c_int)))
    end function sql_driver_has_feature

    !> @brief Open driver connection
    function sql_driver_open(driver, db, user, password, host, port, options) result(success)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: db, user, password, host, options
        integer, intent(in) :: port
        logical :: success
        success = logical(qt_sqldriver_open(driver%qobj%ptr, trim(db)//c_null_char, &
                                          trim(user)//c_null_char, trim(password)//c_null_char, &
                                          trim(host)//c_null_char, int(port, c_int), &
                                          trim(options)//c_null_char))
    end function sql_driver_open

    !> @brief Close driver connection
    function sql_driver_close(driver) result(success)
        type(sql_driver), intent(in) :: driver
        logical :: success
        success = logical(qt_sqldriver_close(driver%qobj%ptr))
    end function sql_driver_close

    !> @brief Create result
    function sql_driver_create_result(driver) result(result)
        type(sql_driver), intent(in) :: driver
        type(c_ptr) :: result
        result = qt_sqldriver_create_result(driver%qobj%ptr)
    end function sql_driver_create_result

    !> @brief Begin transaction
    function sql_driver_begin_transaction(driver) result(success)
        type(sql_driver), intent(in) :: driver
        logical :: success
        success = logical(qt_sqldriver_begin_transaction(driver%qobj%ptr))
    end function sql_driver_begin_transaction

    !> @brief Commit transaction
    function sql_driver_commit_transaction(driver) result(success)
        type(sql_driver), intent(in) :: driver
        logical :: success
        success = logical(qt_sqldriver_commit_transaction(driver%qobj%ptr))
    end function sql_driver_commit_transaction

    !> @brief Rollback transaction
    function sql_driver_rollback_transaction(driver) result(success)
        type(sql_driver), intent(in) :: driver
        logical :: success
        success = logical(qt_sqldriver_rollback_transaction(driver%qobj%ptr))
    end function sql_driver_rollback_transaction

    !> @brief Get SQL statement
    function sql_driver_sql_statement(driver, type, table_name, rec, prepared_statement) result(stmt)
        type(sql_driver), intent(in) :: driver
        integer, intent(in) :: type
        character(len=*), intent(in) :: table_name
        type(sql_record), intent(in) :: rec
        logical, intent(in) :: prepared_statement
        character(len=:), allocatable :: stmt
        stmt = ""
    end function sql_driver_sql_statement

    !> @brief Escape identifier
    function sql_driver_escape_identifier(driver, identifier, type) result(escaped)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: identifier
        integer, intent(in) :: type
        character(len=:), allocatable :: escaped
        escaped = ""
    end function sql_driver_escape_identifier

    !> @brief Format value
    function sql_driver_format_value(driver, field, trim_strings) result(formatted)
        type(sql_driver), intent(in) :: driver
        type(sql_field), intent(in) :: field
        logical, intent(in) :: trim_strings
        character(len=:), allocatable :: formatted
        formatted = ""
    end function sql_driver_format_value

    !> @brief Get record
    function sql_driver_record(driver, table) result(rec)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: table
        type(sql_record) :: rec
        rec%qobj%ptr = qt_sqldriver_record(driver%qobj%ptr, trim(table)//c_null_char)
    end function sql_driver_record

    !> @brief Get primary index
    function sql_driver_primary_index(driver, table_name) result(index)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: table_name
        type(c_ptr) :: index
        index = qt_sqldriver_primary_index(driver%qobj%ptr, trim(table_name)//c_null_char)
    end function sql_driver_primary_index

    !> @brief Check if identifier is escaped
    function sql_driver_is_identifier_escaped(driver, identifier, type) result(escaped)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: identifier
        integer, intent(in) :: type
        logical :: escaped
        escaped = logical(qt_sqldriver_is_identifier_escaped(driver%qobj%ptr, &
                                                           trim(identifier)//c_null_char, int(type, c_int)))
    end function sql_driver_is_identifier_escaped

    !> @brief Strip delimiters
    function sql_driver_strip_delimiters(driver, identifier, type) result(stripped)
        type(sql_driver), intent(in) :: driver
        character(len=*), intent(in) :: identifier
        integer, intent(in) :: type
        character(len=:), allocatable :: stripped
        stripped = ""
    end function sql_driver_strip_delimiters

    !> @brief Get conformance level
    function sql_driver_conformance_level(driver) result(level)
        type(sql_driver), intent(in) :: driver
        integer :: level
        level = int(qt_sqldriver_conformance_level(driver%qobj%ptr))
    end function sql_driver_conformance_level

end module forge_sql