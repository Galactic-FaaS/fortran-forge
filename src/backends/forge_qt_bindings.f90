
!> @brief C bindings for Qt6 library
!> @details Provides Fortran interfaces to Qt6 C API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_bindings
    use iso_c_binding
    implicit none
    private

    public :: qapplication, qwidget, qpushbutton, qlabel, qlineedit, qtextedit
    public :: qmainwindow, qapplication_new, qapplication_exec, qapplication_quit
    public :: qwidget_new, qwidget_show, qwidget_hide, qwidget_close
    public :: qwidget_set_window_title, qwidget_resize, qwidget_set_parent
    public :: qpushbutton_new, qlabel_new, qlineedit_new, qtextedit_new
    public :: qmainwindow_new, qmainwindow_set_central_widget
    public :: qobject_connect, qcoreapplication_process_events
    public :: qabstractitemmodel, qabstractitemview, qlistview, qtableview, qtreeview
    public :: qitemdelegate, qmodelindex, qitemselectionmodel
    public :: qabstractlistmodel, qabstracttablemodel, qabstracttreemodel
    public :: qmodelindex_new, qmodelindex_row, qmodelindex_column, qmodelindex_parent
    public :: qmodelindex_internal_pointer, qmodelindex_is_valid
    public :: qabstractitemmodel_index, qabstractitemmodel_parent, qabstractitemmodel_data
    public :: qabstractitemmodel_set_data, qabstractitemmodel_row_count, qabstractitemmodel_column_count
    public :: qabstractitemmodel_flags, qabstractitemmodel_header_data, qabstractitemmodel_set_header_data
    public :: qabstractitemmodel_insert_rows, qabstractitemmodel_remove_rows
    public :: qabstractitemmodel_insert_columns, qabstractitemmodel_remove_columns
    public :: qabstractitemview_set_model, qabstractitemview_model, qabstractitemview_set_root_index
    public :: qabstractitemview_root_index, qabstractitemview_set_selection_model
    public :: qabstractitemview_selection_model, qabstractitemview_current_index
    public :: qabstractitemview_set_current_index, qabstractitemview_scroll_to
    public :: qlistview_new, qtableview_new, qtreeview_new
    public :: qitemdelegate_new, qitemdelegate_paint, qitemdelegate_size_hint
    public :: qitemdelegate_create_editor, qitemdelegate_set_editor_data, qitemdelegate_set_model_data
    public :: qitemselectionmodel_new, qitemselectionmodel_current_index, qitemselectionmodel_selection
    public :: qitemselectionmodel_set_current_index, qitemselectionmodel_select
    public :: qitemselectionmodel_clear, qitemselectionmodel_clear_selection
    public :: qitemselectionmodel_has_selection, qitemselectionmodel_selected_indexes
    public :: qitemselectionmodel_selected_rows, qitemselectionmodel_selected_columns
    public :: qabstractlistmodel_new, qabstracttablemodel_new, qabstracttreemodel_new
    ! Multimedia bindings
    public :: qaudioformat, qaudioinput, qaudiooutput, qvideowidget, qgraphicsvideoitem
    public :: qvideoformat, qcamera, qcameraviewfinder, qcameraimagecapture
    public :: qmediaplayer, qmediacontent, qmediametadata, qmediarecorder
    public :: qaudiorecorder, qvideorecorder
    public :: qaudioformat_new, qaudioformat_set_sample_rate, qaudioformat_set_channel_count
    public :: qaudioformat_set_sample_size, qaudioformat_set_codec, qaudioformat_sample_rate
    public :: qaudioformat_channel_count, qaudioformat_sample_size, qaudioformat_codec
    public :: qaudioinput_new, qaudioinput_start, qaudioinput_stop, qaudioinput_buffer_size
    public :: qaudioinput_set_buffer_size, qaudioinput_format, qaudioinput_set_format
    public :: qaudiooutput_new, qaudiooutput_start, qaudiooutput_stop, qaudiooutput_buffer_size
    public :: qaudiooutput_set_buffer_size, qaudiooutput_format, qaudiooutput_set_format
    public :: qvideowidget_new, qgraphicsvideoitem_new, qvideoformat_new
    public :: qvideoformat_set_resolution, qvideoformat_resolution, qvideoformat_frame_rate
    public :: qvideoformat_set_frame_rate, qvideoformat_pixel_format, qvideoformat_set_pixel_format
    public :: qcamera_new, qcameraviewfinder_new, qcameraimagecapture_new
    public :: qcamera_set_viewfinder, qcamera_start, qcamera_stop, qcamera_state
    public :: qcamera_set_capture_mode, qcamera_capture_mode, qcameraviewfinder_set_camera
    public :: qcameraimagecapture_set_camera, qcameraimagecapture_capture, qcameraimagecapture_image_captured
    public :: qmediaplayer_new, qmediaplayer_set_media, qmediaplayer_play, qmediaplayer_pause
    public :: qmediaplayer_stop, qmediaplayer_set_volume, qmediaplayer_volume, qmediaplayer_position
    public :: qmediaplayer_set_position, qmediaplayer_duration, qmediaplayer_state, qmediaplayer_error
    public :: qmediaplayer_set_video_output, qmediacontent_new, qmediametadata_title
    public :: qmediametadata_artist, qmediametadata_album, qmediametadata_duration
    public :: qmediarecorder_new, qmediarecorder_set_output_location, qmediarecorder_record
    public :: qmediarecorder_stop, qmediarecorder_state, qmediarecorder_error
    public :: qmediarecorder_set_audio_input, qmediarecorder_set_video_input
    public :: qaudiorecorder_new, qvideorecorder_new
    ! SQL Database bindings
    public :: qsqldatabase, qsqlquery, qsqlrecord, qsqlfield, qsqltablemodel
    public :: qsqlrelationaltablemodel, qsqlquerymodel, qsqlerror, qsqldriver
    public :: qsqldatabase_new, qsqldatabase_add_database, qsqldatabase_database
    public :: qsqldatabase_remove_database, qsqldatabase_contains, qsqldatabase_connection_names
    public :: qsqldatabase_drivers, qsqldatabase_is_driver_available, qsqldatabase_register_sql_driver
    public :: qsqldatabase_clone_database, qsqldatabase_default_connection, qsqldatabase_set_default_connection
    public :: qsqldatabase_open, qsqldatabase_close, qsqldatabase_is_open, qsqldatabase_is_open_error
    public :: qsqldatabase_last_error, qsqldatabase_set_database_name, qsqldatabase_database_name
    public :: qsqldatabase_set_user_name, qsqldatabase_user_name, qsqldatabase_set_password
    public :: qsqldatabase_password, qsqldatabase_set_host_name, qsqldatabase_host_name
    public :: qsqldatabase_set_port, qsqldatabase_port, qsqldatabase_set_connect_options
    public :: qsqldatabase_connect_options, qsqldatabase_driver, qsqldatabase_driver_name
    public :: qsqldatabase_set_numerical_precision_policy, qsqldatabase_numerical_precision_policy
    public :: qsqldatabase_transaction, qsqldatabase_commit, qsqldatabase_rollback
    public :: qsqldatabase_set_transaction_mode, qsqldatabase_transaction_mode
    public :: qsqlquery_new, qsqlquery_exec, qsqlquery_exec_batch, qsqlquery_prepare
    public :: qsqlquery_bind_value, qsqlquery_add_bind_value, qsqlquery_bound_value
    public :: qsqlquery_bound_values, qsqlquery_next, qsqlquery_previous, qsqlquery_first
    public :: qsqlquery_last, qsqlquery_seek, qsqlquery_at, qsqlquery_size, qsqlquery_is_active
    public :: qsqlquery_is_forward_only, qsqlquery_set_forward_only, qsqlquery_is_valid
    public :: qsqlquery_is_null, qsqlquery_value, qsqlquery_record, qsqlquery_driver
    public :: qsqlquery_numerical_precision_policy, qsqlquery_set_numerical_precision_policy
    public :: qsqlquery_last_query, qsqlquery_last_error, qsqlquery_executed_query
    public :: qsqlquery_clear, qsqlquery_finish, qsqlquery_next_result
    public :: qsqlrecord_new, qsqlrecord_append, qsqlrecord_replace, qsqlrecord_remove
    public :: qsqlrecord_insert, qsqlrecord_clear, qsqlrecord_clear_values, qsqlrecord_count
    public :: qsqlrecord_contains, qsqlrecord_field, qsqlrecord_field_name, qsqlrecord_index_of
    public :: qsqlrecord_is_empty, qsqlrecord_is_generated, qsqlrecord_is_null, qsqlrecord_name_to_index
    public :: qsqlrecord_set_generated, qsqlrecord_set_null, qsqlrecord_set_value, qsqlrecord_value
    public :: qsqlfield_new, qsqlfield_set_value, qsqlfield_value, qsqlfield_set_name
    public :: qsqlfield_name, qsqlfield_is_null, qsqlfield_set_null, qsqlfield_is_valid
    public :: qsqlfield_is_auto_value, qsqlfield_set_auto_value, qsqlfield_is_generated
    public :: qsqlfield_set_generated, qsqlfield_is_read_only, qsqlfield_set_read_only
    public :: qsqlfield_type, qsqlfield_set_type, qsqlfield_required_status, qsqlfield_set_required_status
    public :: qsqlfield_length, qsqlfield_set_length, qsqlfield_precision, qsqlfield_set_precision
    public :: qsqlfield_default_value, qsqlfield_set_default_value, qsqlfield_table_name, qsqlfield_set_table_name
    public :: qsqltablemodel_new, qsqltablemodel_set_table, qsqltablemodel_table_name
    public :: qsqltablemodel_set_edit_strategy, qsqltablemodel_edit_strategy, qsqltablemodel_select
    public :: qsqltablemodel_set_filter, qsqltablemodel_filter, qsqltablemodel_set_sort
    public :: qsqltablemodel_sort, qsqltablemodel_select_row, qsqltablemodel_is_dirty
    public :: qsqltablemodel_submit, qsqltablemodel_revert, qsqltablemodel_submit_all
    public :: qsqltablemodel_revert_all, qsqltablemodel_prime_insert, qsqltablemodel_insert_record
    public :: qsqltablemodel_set_record, qsqltablemodel_remove_columns, qsqltablemodel_remove_rows
    public :: qsqltablemodel_database, qsqltablemodel_last_error, qsqltablemodel_clear
    public :: qsqlrelationaltablemodel_new, qsqlrelationaltablemodel_set_relation
    public :: qsqlrelationaltablemodel_relation, qsqlrelationaltablemodel_set_join_mode
    public :: qsqlrelationaltablemodel_join_mode, qsqlrelationaltablemodel_select
    public :: qsqlrelationaltablemodel_revert_row, qsqlrelationaltablemodel_set_filter
    public :: qsqlrelationaltablemodel_filter, qsqlrelationaltablemodel_order_by_clause
    public :: qsqlrelationaltablemodel_set_order_by_clause
    public :: qsqlquerymodel_new, qsqlquerymodel_set_query, qsqlquerymodel_query
    public :: qsqlquerymodel_set_last_error, qsqlquerymodel_last_error, qsqlquerymodel_clear
    public :: qsqlquerymodel_query_change, qsqlquerymodel_index_in_query
    public :: qsqlerror_new, qsqlerror_type, qsqlerror_number, qsqlerror_database_text
    public :: qsqlerror_driver_text, qsqlerror_text, qsqlerror_is_valid, qsqlerror_swap
    public :: qsqldriver_new, qsqldriver_is_open, qsqldriver_is_open_error, qsqldriver_last_error
    public :: qsqldriver_driver_name, qsqldriver_has_feature, qsqldriver_open, qsqldriver_close
    public :: qsqldriver_create_result, qsqldriver_begin_transaction, qsqldriver_commit_transaction
    public :: qsqldriver_rollback_transaction, qsqldriver_sql_statement, qsqldriver_escape_identifier
    public :: qsqldriver_format_value, qsqldriver_record, qsqldriver_primary_index
    public :: qsqldriver_is_identifier_escaped, qsqldriver_strip_delimiters, qsqldriver_conformance_level

    !> Opaque Qt handles
    type :: qapplication
        type(c_ptr) :: ptr = c_null_ptr
    end type qapplication

    type :: qwidget
        type(c_ptr) :: ptr = c_null_ptr
    end type qwidget

    type :: qpushbutton
        type(c_ptr) :: ptr = c_null_ptr
    end type qpushbutton

    type :: qlabel
        type(c_ptr) :: ptr = c_null_ptr
    end type qlabel

    type :: qlineedit
        type(c_ptr) :: ptr = c_null_ptr
    end type qlineedit

    type :: qtextedit
        type(c_ptr) :: ptr = c_null_ptr
    end type qtextedit

    type :: qmainwindow
        type(c_ptr) :: ptr = c_null_ptr
    end type qmainwindow

    !> MVC Framework Types
    type :: qabstractitemmodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qabstractitemmodel

    type :: qabstractitemview
        type(c_ptr) :: ptr = c_null_ptr
    end type qabstractitemview

    type :: qlistview
        type(c_ptr) :: ptr = c_null_ptr
    end type qlistview

    type :: qtableview
        type(c_ptr) :: ptr = c_null_ptr
    end type qtableview

    type :: qtreeview
        type(c_ptr) :: ptr = c_null_ptr
    end type qtreeview

    type :: qitemdelegate
        type(c_ptr) :: ptr = c_null_ptr
    end type qitemdelegate

    type :: qmodelindex
        type(c_ptr) :: ptr = c_null_ptr
    end type qmodelindex

    type :: qitemselectionmodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qitemselectionmodel

    type :: qabstractlistmodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qabstractlistmodel

    type :: qabstracttablemodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qabstracttablemodel

    type :: qabstracttreemodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qabstracttreemodel

    !> Multimedia Types
    type :: qaudioformat
        type(c_ptr) :: ptr = c_null_ptr
    end type qaudioformat

    type :: qaudioinput
        type(c_ptr) :: ptr = c_null_ptr
    end type qaudioinput

    type :: qaudiooutput
        type(c_ptr) :: ptr = c_null_ptr
    end type qaudiooutput

    type :: qvideowidget
        type(c_ptr) :: ptr = c_null_ptr
    end type qvideowidget

    type :: qgraphicsvideoitem
        type(c_ptr) :: ptr = c_null_ptr
    end type qgraphicsvideoitem

    type :: qvideoformat
        type(c_ptr) :: ptr = c_null_ptr
    end type qvideoformat

    type :: qcamera
        type(c_ptr) :: ptr = c_null_ptr
    end type qcamera

    type :: qcameraviewfinder
        type(c_ptr) :: ptr = c_null_ptr
    end type qcameraviewfinder

    type :: qcameraimagecapture
        type(c_ptr) :: ptr = c_null_ptr
    end type qcameraimagecapture

    type :: qmediaplayer
        type(c_ptr) :: ptr = c_null_ptr
    end type qmediaplayer

    type :: qmediacontent
        type(c_ptr) :: ptr = c_null_ptr
    end type qmediacontent

    type :: qmediametadata
        type(c_ptr) :: ptr = c_null_ptr
    end type qmediametadata

    type :: qmediarecorder
        type(c_ptr) :: ptr = c_null_ptr
    end type qmediarecorder

    type :: qaudiorecorder
        type(c_ptr) :: ptr = c_null_ptr
    end type qaudiorecorder

    type :: qvideorecorder
        type(c_ptr) :: ptr = c_null_ptr
    end type qvideorecorder

    !> SQL Database Types
    type :: qsqldatabase
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqldatabase

    type :: qsqlquery
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlquery

    type :: qsqlrecord
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlrecord

    type :: qsqlfield
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlfield

    type :: qsqltablemodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqltablemodel

    type :: qsqlrelationaltablemodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlrelationaltablemodel

    type :: qsqlquerymodel
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlquerymodel

    type :: qsqlerror
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqlerror

    type :: qsqldriver
        type(c_ptr) :: ptr = c_null_ptr
    end type qsqldriver

    !> Qt6 C API bindings (simplified wrapper functions)
    interface
        !> Create QApplication
        function qapplication_new(argc, argv) bind(c, name="qapplication_new")
            import :: c_ptr, c_int
            integer(c_int), value :: argc
            type(c_ptr), value :: argv
            type(c_ptr) :: qapplication_new
        end function qapplication_new

        !> Execute QApplication event loop
        function qapplication_exec() bind(c, name="qapplication_exec")
            import :: c_int
            integer(c_int) :: qapplication_exec
        end function qapplication_exec

        !> Quit QApplication
        subroutine qapplication_quit() bind(c, name="qapplication_quit")
        end subroutine qapplication_quit

        !> Create QWidget
        function qwidget_new(parent) bind(c, name="qwidget_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qwidget_new
        end function qwidget_new

        !> Show QWidget
        subroutine qwidget_show(widget) bind(c, name="qwidget_show")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_show

        !> Hide QWidget
        subroutine qwidget_hide(widget) bind(c, name="qwidget_hide")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_hide

        !> Close QWidget
        subroutine qwidget_close(widget) bind(c, name="qwidget_close")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_close

        !> Set QWidget window title
        subroutine qwidget_set_window_title(widget, title) bind(c, name="qwidget_set_window_title")
            import :: c_ptr, c_char
            type(c_ptr), value :: widget
            character(kind=c_char), dimension(*) :: title
        end subroutine qwidget_set_window_title

        !> Resize QWidget
        subroutine qwidget_resize(widget, width, height) bind(c, name="qwidget_resize")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: width
            integer(c_int), value :: height
        end subroutine qwidget_resize

        !> Set QWidget parent
        subroutine qwidget_set_parent(widget, parent) bind(c, name="qwidget_set_parent")
            import :: c_ptr
            type(c_ptr), value :: widget
            type(c_ptr), value :: parent
        end subroutine qwidget_set_parent

        !> Create QPushButton
        function qpushbutton_new(text, parent) bind(c, name="qpushbutton_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: text
            type(c_ptr), value :: parent
            type(c_ptr) :: qpushbutton_new
        end function qpushbutton_new

        !> Create QLabel
        function qlabel_new(text, parent) bind(c, name="qlabel_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: text
            type(c_ptr), value :: parent
            type(c_ptr) :: qlabel_new
        end function qlabel_new

        !> Create QLineEdit
        function qlineedit_new(parent) bind(c, name="qlineedit_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qlineedit_new
        end function qlineedit_new

        !> Create QTextEdit
        function qtextedit_new(parent) bind(c, name="qtextedit_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qtextedit_new
        end function qtextedit_new

        !> Create QMainWindow
        function qmainwindow_new(parent) bind(c, name="qmainwindow_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qmainwindow_new
        end function qmainwindow_new

        !> Set QMainWindow central widget
        subroutine qmainwindow_set_central_widget(window, widget) bind(c, name="qmainwindow_set_central_widget")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr), value :: widget
        end subroutine qmainwindow_set_central_widget

        !> Connect QObject signal to slot
        subroutine qobject_connect(sender, signal, receiver, slot) bind(c, name="qobject_connect")
            import :: c_ptr, c_char
            type(c_ptr), value :: sender
            character(kind=c_char), dimension(*) :: signal
            type(c_ptr), value :: receiver
            character(kind=c_char), dimension(*) :: slot
        end subroutine qobject_connect

        !> Process pending events (non-blocking)
        subroutine qcoreapplication_process_events() bind(c, name="qcoreapplication_process_events")
        end subroutine qcoreapplication_process_events

        !> Delete QObject
        subroutine qobject_delete(obj) bind(c, name="qobject_delete")
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine qobject_delete

        !> MVC Framework Bindings

        !> QModelIndex
        function qmodelindex_new() bind(c, name="qmodelindex_new")
            import :: c_ptr
            type(c_ptr) :: qmodelindex_new
        end function qmodelindex_new

        function qmodelindex_row(index) bind(c, name="qmodelindex_row")
            import :: c_ptr, c_int
            type(c_ptr), value :: index
            integer(c_int) :: qmodelindex_row
        end function qmodelindex_row

        function qmodelindex_column(index) bind(c, name="qmodelindex_column")
            import :: c_ptr, c_int
            type(c_ptr), value :: index
            integer(c_int) :: qmodelindex_column
        end function qmodelindex_column

        function qmodelindex_parent(index) bind(c, name="qmodelindex_parent")
            import :: c_ptr
            type(c_ptr), value :: index
            type(c_ptr) :: qmodelindex_parent
        end function qmodelindex_parent

        function qmodelindex_internal_pointer(index) bind(c, name="qmodelindex_internal_pointer")
            import :: c_ptr
            type(c_ptr), value :: index
            type(c_ptr) :: qmodelindex_internal_pointer
        end function qmodelindex_internal_pointer

        function qmodelindex_is_valid(index) bind(c, name="qmodelindex_is_valid")
            import :: c_ptr, c_bool
            type(c_ptr), value :: index
            logical(c_bool) :: qmodelindex_is_valid
        end function qmodelindex_is_valid

        !> QAbstractItemModel
        function qabstractitemmodel_index(model, row, column, parent) bind(c, name="qabstractitemmodel_index")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: row, column
            type(c_ptr), value :: parent
            type(c_ptr) :: qabstractitemmodel_index
        end function qabstractitemmodel_index

        function qabstractitemmodel_parent(model, child) bind(c, name="qabstractitemmodel_parent")
            import :: c_ptr
            type(c_ptr), value :: model
            type(c_ptr), value :: child
            type(c_ptr) :: qabstractitemmodel_parent
        end function qabstractitemmodel_parent

        function qabstractitemmodel_data(model, index, role) bind(c, name="qabstractitemmodel_data")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: model
            type(c_ptr), value :: index
            integer(c_int), value :: role
            character(kind=c_char), dimension(*) :: qabstractitemmodel_data
        end function qabstractitemmodel_data

        function qabstractitemmodel_set_data(model, index, value, role) bind(c, name="qabstractitemmodel_set_data")
            import :: c_ptr, c_int, c_char, c_bool
            type(c_ptr), value :: model
            type(c_ptr), value :: index
            character(kind=c_char), dimension(*) :: value
            integer(c_int), value :: role
            logical(c_bool) :: qabstractitemmodel_set_data
        end function qabstractitemmodel_set_data

        function qabstractitemmodel_row_count(model, parent) bind(c, name="qabstractitemmodel_row_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            type(c_ptr), value :: parent
            integer(c_int) :: qabstractitemmodel_row_count
        end function qabstractitemmodel_row_count

        function qabstractitemmodel_column_count(model, parent) bind(c, name="qabstractitemmodel_column_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            type(c_ptr), value :: parent
            integer(c_int) :: qabstractitemmodel_column_count
        end function qabstractitemmodel_column_count

        function qabstractitemmodel_flags(model, index) bind(c, name="qabstractitemmodel_flags")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            type(c_ptr), value :: index
            integer(c_int) :: qabstractitemmodel_flags
        end function qabstractitemmodel_flags

        function qabstractitemmodel_header_data(model, section, orientation, role) bind(c, name="qabstractitemmodel_header_data")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: model
            integer(c_int), value :: section, orientation, role
            character(kind=c_char), dimension(*) :: qabstractitemmodel_header_data
        end function qabstractitemmodel_header_data

        function qabstractitemmodel_set_header_data(model, section, orientation, value, role) bind(c, name="qabstractitemmodel_set_header_data")
            import :: c_ptr, c_int, c_char, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: section, orientation
            character(kind=c_char), dimension(*) :: value
            integer(c_int), value :: role
            logical(c_bool) :: qabstractitemmodel_set_header_data
        end function qabstractitemmodel_set_header_data

        function qabstractitemmodel_insert_rows(model, row, count, parent) bind(c, name="qabstractitemmodel_insert_rows")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: row, count
            type(c_ptr), value :: parent
            logical(c_bool) :: qabstractitemmodel_insert_rows
        end function qabstractitemmodel_insert_rows

        function qabstractitemmodel_remove_rows(model, row, count, parent) bind(c, name="qabstractitemmodel_remove_rows")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: row, count
            type(c_ptr), value :: parent
            logical(c_bool) :: qabstractitemmodel_remove_rows
        end function qabstractitemmodel_remove_rows

        function qabstractitemmodel_insert_columns(model, column, count, parent) bind(c, name="qabstractitemmodel_insert_columns")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: column, count
            type(c_ptr), value :: parent
            logical(c_bool) :: qabstractitemmodel_insert_columns
        end function qabstractitemmodel_insert_columns

        function qabstractitemmodel_remove_columns(model, column, count, parent) bind(c, name="qabstractitemmodel_remove_columns")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: column, count
            type(c_ptr), value :: parent
            logical(c_bool) :: qabstractitemmodel_remove_columns
        end function qabstractitemmodel_remove_columns

        !> QAbstractItemView
        subroutine qabstractitemview_set_model(view, model) bind(c, name="qabstractitemview_set_model")
            import :: c_ptr
            type(c_ptr), value :: view, model
        end subroutine qabstractitemview_set_model

        function qabstractitemview_model(view) bind(c, name="qabstractitemview_model")
            import :: c_ptr
            type(c_ptr), value :: view
            type(c_ptr) :: qabstractitemview_model
        end function qabstractitemview_model

        subroutine qabstractitemview_set_root_index(view, index) bind(c, name="qabstractitemview_set_root_index")
            import :: c_ptr
            type(c_ptr), value :: view, index
        end subroutine qabstractitemview_set_root_index

        function qabstractitemview_root_index(view) bind(c, name="qabstractitemview_root_index")
            import :: c_ptr
            type(c_ptr), value :: view
            type(c_ptr) :: qabstractitemview_root_index
        end function qabstractitemview_root_index

        subroutine qabstractitemview_set_selection_model(view, selection_model) bind(c, name="qabstractitemview_set_selection_model")
            import :: c_ptr
            type(c_ptr), value :: view, selection_model
        end subroutine qabstractitemview_set_selection_model

        function qabstractitemview_selection_model(view) bind(c, name="qabstractitemview_selection_model")
            import :: c_ptr
            type(c_ptr), value :: view
            type(c_ptr) :: qabstractitemview_selection_model
        end function qabstractitemview_selection_model

        function qabstractitemview_current_index(view) bind(c, name="qabstractitemview_current_index")
            import :: c_ptr
            type(c_ptr), value :: view
            type(c_ptr) :: qabstractitemview_current_index
        end function qabstractitemview_current_index

        subroutine qabstractitemview_set_current_index(view, index) bind(c, name="qabstractitemview_set_current_index")
            import :: c_ptr
            type(c_ptr), value :: view, index
        end subroutine qabstractitemview_set_current_index

        subroutine qabstractitemview_scroll_to(view, index, hint) bind(c, name="qabstractitemview_scroll_to")
            import :: c_ptr, c_int
            type(c_ptr), value :: view, index
            integer(c_int), value :: hint
        end subroutine qabstractitemview_scroll_to

        !> QListView
        function qlistview_new(parent) bind(c, name="qlistview_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qlistview_new
        end function qlistview_new

        !> QTableView
        function qtableview_new(parent) bind(c, name="qtableview_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qtableview_new
        end function qtableview_new

        !> QTreeView
        function qtreeview_new(parent) bind(c, name="qtreeview_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qtreeview_new
        end function qtreeview_new

        !> QItemDelegate
        function qitemdelegate_new(parent) bind(c, name="qitemdelegate_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qitemdelegate_new
        end function qitemdelegate_new

        subroutine qitemdelegate_paint(delegate, painter, option, index) bind(c, name="qitemdelegate_paint")
            import :: c_ptr
            type(c_ptr), value :: delegate, painter, option, index
        end subroutine qitemdelegate_paint

        function qitemdelegate_size_hint(delegate, option, index) bind(c, name="qitemdelegate_size_hint")
            import :: c_ptr
            type(c_ptr), value :: delegate, option, index
            type(c_ptr) :: qitemdelegate_size_hint
        end function qitemdelegate_size_hint

        function qitemdelegate_create_editor(delegate, parent, option, index) bind(c, name="qitemdelegate_create_editor")
            import :: c_ptr
            type(c_ptr), value :: delegate, parent, option, index
            type(c_ptr) :: qitemdelegate_create_editor
        end function qitemdelegate_create_editor

        subroutine qitemdelegate_set_editor_data(delegate, editor, index) bind(c, name="qitemdelegate_set_editor_data")
            import :: c_ptr
            type(c_ptr), value :: delegate, editor, index
        end subroutine qitemdelegate_set_editor_data

        subroutine qitemdelegate_set_model_data(delegate, editor, model, index) bind(c, name="qitemdelegate_set_model_data")
            import :: c_ptr
            type(c_ptr), value :: delegate, editor, model, index
        end subroutine qitemdelegate_set_model_data

        !> QItemSelectionModel
        function qitemselectionmodel_new(model, parent) bind(c, name="qitemselectionmodel_new")
            import :: c_ptr
            type(c_ptr), value :: model, parent
            type(c_ptr) :: qitemselectionmodel_new
        end function qitemselectionmodel_new

        function qitemselectionmodel_current_index(selection_model) bind(c, name="qitemselectionmodel_current_index")
            import :: c_ptr
            type(c_ptr), value :: selection_model
            type(c_ptr) :: qitemselectionmodel_current_index
        end function qitemselectionmodel_current_index

        function qitemselectionmodel_selection(selection_model) bind(c, name="qitemselectionmodel_selection")
            import :: c_ptr
            type(c_ptr), value :: selection_model
            type(c_ptr) :: qitemselectionmodel_selection
        end function qitemselectionmodel_selection

        subroutine qitemselectionmodel_set_current_index(selection_model, index, command) bind(c, name="qitemselectionmodel_set_current_index")
            import :: c_ptr, c_int
            type(c_ptr), value :: selection_model, index
            integer(c_int), value :: command
        end subroutine qitemselectionmodel_set_current_index

        subroutine qitemselectionmodel_select(selection_model, index, command) bind(c, name="qitemselectionmodel_select")
            import :: c_ptr, c_int
            type(c_ptr), value :: selection_model, index
            integer(c_int), value :: command
        end subroutine qitemselectionmodel_select

        subroutine qitemselectionmodel_clear(selection_model) bind(c, name="qitemselectionmodel_clear")
            import :: c_ptr
            type(c_ptr), value :: selection_model
        end subroutine qitemselectionmodel_clear

        subroutine qitemselectionmodel_clear_selection(selection_model) bind(c, name="qitemselectionmodel_clear_selection")
            import :: c_ptr
            type(c_ptr), value :: selection_model
        end subroutine qitemselectionmodel_clear_selection

        function qitemselectionmodel_has_selection(selection_model) bind(c, name="qitemselectionmodel_has_selection")
            import :: c_ptr, c_bool
            type(c_ptr), value :: selection_model
            logical(c_bool) :: qitemselectionmodel_has_selection
        end function qitemselectionmodel_has_selection

        function qitemselectionmodel_selected_indexes(selection_model) bind(c, name="qitemselectionmodel_selected_indexes")
            import :: c_ptr
            type(c_ptr), value :: selection_model
            type(c_ptr) :: qitemselectionmodel_selected_indexes
        end function qitemselectionmodel_selected_indexes

        function qitemselectionmodel_selected_rows(selection_model, column) bind(c, name="qitemselectionmodel_selected_rows")
            import :: c_ptr, c_int
            type(c_ptr), value :: selection_model
            integer(c_int), value :: column
            type(c_ptr) :: qitemselectionmodel_selected_rows
        end function qitemselectionmodel_selected_rows

        function qitemselectionmodel_selected_columns(selection_model, row) bind(c, name="qitemselectionmodel_selected_columns")
            import :: c_ptr, c_int
            type(c_ptr), value :: selection_model
            integer(c_int), value :: row
            type(c_ptr) :: qitemselectionmodel_selected_columns
        end function qitemselectionmodel_selected_columns

        !> Concrete Model Classes
        function qabstractlistmodel_new(parent) bind(c, name="qabstractlistmodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qabstractlistmodel_new
        end function qabstractlistmodel_new

        function qabstracttablemodel_new(parent) bind(c, name="qabstracttablemodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qabstracttablemodel_new
        end function qabstracttablemodel_new

        function qabstracttreemodel_new(parent) bind(c, name="qabstracttreemodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qabstracttreemodel_new
        end function qabstracttreemodel_new

        !> Multimedia Bindings

        !> QAudioFormat
        function qaudioformat_new() bind(c, name="qaudioformat_new")
            import :: c_ptr
            type(c_ptr) :: qaudioformat_new
        end function qaudioformat_new

        subroutine qaudioformat_set_sample_rate(format, sample_rate) bind(c, name="qaudioformat_set_sample_rate")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), value :: sample_rate
        end subroutine qaudioformat_set_sample_rate

        subroutine qaudioformat_set_channel_count(format, channel_count) bind(c, name="qaudioformat_set_channel_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), value :: channel_count
        end subroutine qaudioformat_set_channel_count

        subroutine qaudioformat_set_sample_size(format, sample_size) bind(c, name="qaudioformat_set_sample_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), value :: sample_size
        end subroutine qaudioformat_set_sample_size

        subroutine qaudioformat_set_codec(format, codec) bind(c, name="qaudioformat_set_codec")
            import :: c_ptr, c_char
            type(c_ptr), value :: format
            character(kind=c_char), dimension(*) :: codec
        end subroutine qaudioformat_set_codec

        function qaudioformat_sample_rate(format) bind(c, name="qaudioformat_sample_rate")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int) :: qaudioformat_sample_rate
        end function qaudioformat_sample_rate

        function qaudioformat_channel_count(format) bind(c, name="qaudioformat_channel_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int) :: qaudioformat_channel_count
        end function qaudioformat_channel_count

        function qaudioformat_sample_size(format) bind(c, name="qaudioformat_sample_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int) :: qaudioformat_sample_size
        end function qaudioformat_sample_size

        function qaudioformat_codec(format) bind(c, name="qaudioformat_codec")
            import :: c_ptr, c_char
            type(c_ptr), value :: format
            character(kind=c_char), dimension(*) :: qaudioformat_codec
        end function qaudioformat_codec

        !> QAudioInput
        function qaudioinput_new(format, parent) bind(c, name="qaudioinput_new")
            import :: c_ptr
            type(c_ptr), value :: format, parent
            type(c_ptr) :: qaudioinput_new
        end function qaudioinput_new

        subroutine qaudioinput_start(input, device) bind(c, name="qaudioinput_start")
            import :: c_ptr
            type(c_ptr), value :: input, device
        end subroutine qaudioinput_start

        subroutine qaudioinput_stop(input) bind(c, name="qaudioinput_stop")
            import :: c_ptr
            type(c_ptr), value :: input
        end subroutine qaudioinput_stop

        function qaudioinput_buffer_size(input) bind(c, name="qaudioinput_buffer_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: input
            integer(c_int) :: qaudioinput_buffer_size
        end function qaudioinput_buffer_size

        subroutine qaudioinput_set_buffer_size(input, buffer_size) bind(c, name="qaudioinput_set_buffer_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: input
            integer(c_int), value :: buffer_size
        end subroutine qaudioinput_set_buffer_size

        function qaudioinput_format(input) bind(c, name="qaudioinput_format")
            import :: c_ptr
            type(c_ptr), value :: input
            type(c_ptr) :: qaudioinput_format
        end function qaudioinput_format

        subroutine qaudioinput_set_format(input, format) bind(c, name="qaudioinput_set_format")
            import :: c_ptr
            type(c_ptr), value :: input, format
        end subroutine qaudioinput_set_format

        !> QAudioOutput
        function qaudiooutput_new(format, parent) bind(c, name="qaudiooutput_new")
            import :: c_ptr
            type(c_ptr), value :: format, parent
            type(c_ptr) :: qaudiooutput_new
        end function qaudiooutput_new

        subroutine qaudiooutput_start(output, device) bind(c, name="qaudiooutput_start")
            import :: c_ptr
            type(c_ptr), value :: output, device
        end subroutine qaudiooutput_start

        subroutine qaudiooutput_stop(output) bind(c, name="qaudiooutput_stop")
            import :: c_ptr
            type(c_ptr), value :: output
        end subroutine qaudiooutput_stop

        function qaudiooutput_buffer_size(output) bind(c, name="qaudiooutput_buffer_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: output
            integer(c_int) :: qaudiooutput_buffer_size
        end function qaudiooutput_buffer_size

        subroutine qaudiooutput_set_buffer_size(output, buffer_size) bind(c, name="qaudiooutput_set_buffer_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: output
            integer(c_int), value :: buffer_size
        end subroutine qaudiooutput_set_buffer_size

        function qaudiooutput_format(output) bind(c, name="qaudiooutput_format")
            import :: c_ptr
            type(c_ptr), value :: output
            type(c_ptr) :: qaudiooutput_format
        end function qaudiooutput_format

        subroutine qaudiooutput_set_format(output, format) bind(c, name="qaudiooutput_set_format")
            import :: c_ptr
            type(c_ptr), value :: output, format
        end subroutine qaudiooutput_set_format

        !> QVideoWidget
        function qvideowidget_new(parent) bind(c, name="qvideowidget_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qvideowidget_new
        end function qvideowidget_new

        !> QGraphicsVideoItem
        function qgraphicsvideoitem_new(parent) bind(c, name="qgraphicsvideoitem_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qgraphicsvideoitem_new
        end function qgraphicsvideoitem_new

        !> QVideoFormat
        function qvideoformat_new() bind(c, name="qvideoformat_new")
            import :: c_ptr
            type(c_ptr) :: qvideoformat_new
        end function qvideoformat_new

        subroutine qvideoformat_set_resolution(format, width, height) bind(c, name="qvideoformat_set_resolution")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), value :: width, height
        end subroutine qvideoformat_set_resolution

        function qvideoformat_resolution(format) bind(c, name="qvideoformat_resolution")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), dimension(2) :: qvideoformat_resolution
        end function qvideoformat_resolution

        function qvideoformat_frame_rate(format) bind(c, name="qvideoformat_frame_rate")
            import :: c_ptr, c_double
            type(c_ptr), value :: format
            real(c_double) :: qvideoformat_frame_rate
        end function qvideoformat_frame_rate

        subroutine qvideoformat_set_frame_rate(format, frame_rate) bind(c,
        subroutine qvideoformat_set_frame_rate(format, frame_rate) bind(c, name="qvideoformat_set_frame_rate")
            import :: c_ptr, c_double
            type(c_ptr), value :: format
            real(c_double), value :: frame_rate
        end subroutine qvideoformat_set_frame_rate

        function qvideoformat_pixel_format(format) bind(c, name="qvideoformat_pixel_format")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int) :: qvideoformat_pixel_format
        end function qvideoformat_pixel_format

        subroutine qvideoformat_set_pixel_format(format, pixel_format) bind(c, name="qvideoformat_set_pixel_format")
            import :: c_ptr, c_int
            type(c_ptr), value :: format
            integer(c_int), value :: pixel_format
        end subroutine qvideoformat_set_pixel_format

        !> QCamera
        function qcamera_new(parent) bind(c, name="qcamera_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qcamera_new
        end function qcamera_new

        subroutine qcamera_set_viewfinder(camera, viewfinder) bind(c, name="qcamera_set_viewfinder")
            import :: c_ptr
            type(c_ptr), value :: camera, viewfinder
        end subroutine qcamera_set_viewfinder

        subroutine qcamera_start(camera) bind(c, name="qcamera_start")
            import :: c_ptr
            type(c_ptr), value :: camera
        end subroutine qcamera_start

        subroutine qcamera_stop(camera) bind(c, name="qcamera_stop")
            import :: c_ptr
            type(c_ptr), value :: camera
        end subroutine qcamera_stop

        function qcamera_state(camera) bind(c, name="qcamera_state")
            import :: c_ptr, c_int
            type(c_ptr), value :: camera
            integer(c_int) :: qcamera_state
        end function qcamera_state

        subroutine qcamera_set_capture_mode(camera, mode) bind(c, name="qcamera_set_capture_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: camera
            integer(c_int), value :: mode
        end subroutine qcamera_set_capture_mode

        function qcamera_capture_mode(camera) bind(c, name="qcamera_capture_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: camera
            integer(c_int) :: qcamera_capture_mode
        end function qcamera_capture_mode

        !> QCameraViewfinder
        function qcameraviewfinder_new(parent) bind(c, name="qcameraviewfinder_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qcameraviewfinder_new
        end function qcameraviewfinder_new

        subroutine qcameraviewfinder_set_camera(viewfinder, camera) bind(c, name="qcameraviewfinder_set_camera")
            import :: c_ptr
            type(c_ptr), value :: viewfinder, camera
        end subroutine qcameraviewfinder_set_camera

        !> QCameraImageCapture
        function qcameraimagecapture_new(camera, parent) bind(c, name="qcameraimagecapture_new")
            import :: c_ptr
            type(c_ptr), value :: camera, parent
            type(c_ptr) :: qcameraimagecapture_new
        end function qcameraimagecapture_new

        subroutine qcameraimagecapture_set_camera(capture, camera) bind(c, name="qcameraimagecapture_set_camera")
            import :: c_ptr
            type(c_ptr), value :: capture, camera
        end subroutine qcameraimagecapture_set_camera

        subroutine qcameraimagecapture_capture(capture, location) bind(c, name="qcameraimagecapture_capture")
            import :: c_ptr, c_char
            type(c_ptr), value :: capture
            character(kind=c_char), dimension(*) :: location
        end subroutine qcameraimagecapture_capture

        function qcameraimagecapture_image_captured(capture) bind(c, name="qcameraimagecapture_image_captured")
            import :: c_ptr, c_bool
            type(c_ptr), value :: capture
            logical(c_bool) :: qcameraimagecapture_image_captured
        end function qcameraimagecapture_image_captured

        !> QMediaPlayer
        function qmediaplayer_new(parent) bind(c, name="qmediaplayer_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qmediaplayer_new
        end function qmediaplayer_new

        subroutine qmediaplayer_set_media(player, media) bind(c, name="qmediaplayer_set_media")
            import :: c_ptr
            type(c_ptr), value :: player, media
        end subroutine qmediaplayer_set_media

        subroutine qmediaplayer_play(player) bind(c, name="qmediaplayer_play")
            import :: c_ptr
            type(c_ptr), value :: player
        end subroutine qmediaplayer_play

        subroutine qmediaplayer_pause(player) bind(c, name="qmediaplayer_pause")
            import :: c_ptr
            type(c_ptr), value :: player
        end subroutine qmediaplayer_pause

        subroutine qmediaplayer_stop(player) bind(c, name="qmediaplayer_stop")
            import :: c_ptr
            type(c_ptr), value :: player
        end subroutine qmediaplayer_stop

        subroutine qmediaplayer_set_volume(player, volume) bind(c, name="qmediaplayer_set_volume")
            import :: c_ptr, c_int
            type(c_ptr), value :: player
            integer(c_int), value :: volume
        end subroutine qmediaplayer_set_volume

        function qmediaplayer_volume(player) bind(c, name="qmediaplayer_volume")
            import :: c_ptr, c_int
            type(c_ptr), value :: player
            integer(c_int) :: qmediaplayer_volume
        end function qmediaplayer_volume

        function qmediaplayer_position(player) bind(c, name="qmediaplayer_position")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: player
            integer(c_long_long) :: qmediaplayer_position
        end function qmediaplayer_position

        subroutine qmediaplayer_set_position(player, position) bind(c, name="qmediaplayer_set_position")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: player
            integer(c_long_long), value :: position
        end subroutine qmediaplayer_set_position

        function qmediaplayer_duration(player) bind(c, name="qmediaplayer_duration")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: player
            integer(c_long_long) :: qmediaplayer_duration
        end function qmediaplayer_duration

        function qmediaplayer_state(player) bind(c, name="qmediaplayer_state")
            import :: c_ptr, c_int
            type(c_ptr), value :: player
            integer(c_int) :: qmediaplayer_state
        end function qmediaplayer_state

        function qmediaplayer_error(player) bind(c, name="qmediaplayer_error")
        function qsqltablemodel_select(model) bind(c, name="qsqltablemodel_select")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqltablemodel_select
        end function qsqltablemodel_select

        subroutine qsqltablemodel_set_filter(model, filter) bind(c, name="qsqltablemodel_set_filter")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: filter
        end subroutine qsqltablemodel_set_filter

        function qsqltablemodel_filter(model) bind(c, name="qsqltablemodel_filter")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: qsqltablemodel_filter
        end function qsqltablemodel_filter

        subroutine qsqltablemodel_set_sort(model, column, order) bind(c, name="qsqltablemodel_set_sort")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: column, order
        end subroutine qsqltablemodel_set_sort

        function qsqltablemodel_sort(model) bind(c, name="qsqltablemodel_sort")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), dimension(2) :: qsqltablemodel_sort
        end function qsqltablemodel_sort

        function qsqltablemodel_select_row(model, row) bind(c, name="qsqltablemodel_select_row")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model
            integer(c_int), value :: row
            logical(c_bool) :: qsqltablemodel_select_row
        end function qsqltablemodel_select_row

        function qsqltablemodel_is_dirty(model, index) bind(c, name="qsqltablemodel_is_dirty")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model, index
            logical(c_bool) :: qsqltablemodel_is_dirty
        end function qsqltablemodel_is_dirty

        function qsqltablemodel_submit(model) bind(c, name="qsqltablemodel_submit")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqltablemodel_submit
        end function qsqltablemodel_submit

        function qsqltablemodel_revert(model) bind(c, name="qsqltablemodel_revert")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqltablemodel_revert
        end function qsqltablemodel_revert

        function qsqltablemodel_submit_all(model) bind(c, name="qsqltablemodel_submit_all")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqltablemodel_submit_all
        end function qsqltablemodel_submit_all

        function qsqltablemodel_revert_all(model) bind(c, name="qsqltablemodel_revert_all")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqltablemodel_revert_all
        end function qsqltablemodel_revert_all

        function qsqltablemodel_prime_insert(model, row, record) bind(c, name="qsqltablemodel_prime_insert")
            import :: c_ptr
            type(c_ptr), value :: model, record
            integer(c_int), value :: row
        end function qsqltablemodel_prime_insert

        function qsqltablemodel_insert_record(model, row, record) bind(c, name="qsqltablemodel_insert_record")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model, record
            integer(c_int), value :: row
            logical(c_bool) :: qsqltablemodel_insert_record
        end function qsqltablemodel_insert_record

        subroutine qsqltablemodel_set_record(model, row, record) bind(c, name="qsqltablemodel_set_record")
            import :: c_ptr, c_int
            type(c_ptr), value :: model, record
            integer(c_int), value :: row
        end subroutine qsqltablemodel_set_record

        function qsqltablemodel_remove_columns(model, column, count, parent) bind(c, name="qsqltablemodel_remove_columns")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model, parent
            integer(c_int), value :: column, count
            logical(c_bool) :: qsqltablemodel_remove_columns
        end function qsqltablemodel_remove_columns

        function qsqltablemodel_remove_rows(model, row, count, parent) bind(c, name="qsqltablemodel_remove_rows")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: model, parent
            integer(c_int), value :: row, count
            logical(c_bool) :: qsqltablemodel_remove_rows
        end function qsqltablemodel_remove_rows

        function qsqltablemodel_database(model) bind(c, name="qsqltablemodel_database")
            import :: c_ptr
            type(c_ptr), value :: model
            type(c_ptr) :: qsqltablemodel_database
        end function qsqltablemodel_database

        function qsqltablemodel_last_error(model) bind(c, name="qsqltablemodel_last_error")
            import :: c_ptr
            type(c_ptr), value :: model
            type(c_ptr) :: qsqltablemodel_last_error
        end function qsqltablemodel_last_error

        subroutine qsqltablemodel_clear(model) bind(c, name="qsqltablemodel_clear")
            import :: c_ptr
            type(c_ptr), value :: model
        end subroutine qsqltablemodel_clear

        !> QSqlRelationalTableModel
        function qsqlrelationaltablemodel_new(parent, db) bind(c, name="qsqlrelationaltablemodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent, db
            type(c_ptr) :: qsqlrelationaltablemodel_new
        end function qsqlrelationaltablemodel_new

        subroutine qsqlrelationaltablemodel_set_relation(model, column, relation) bind(c, name="qsqlrelationaltablemodel_set_relation")
            import :: c_ptr, c_int
            type(c_ptr), value :: model, relation
            integer(c_int), value :: column
        end subroutine qsqlrelationaltablemodel_set_relation

        function qsqlrelationaltablemodel_relation(model, column) bind(c, name="qsqlrelationaltablemodel_relation")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: column
            type(c_ptr) :: qsqlrelationaltablemodel_relation
        end function qsqlrelationaltablemodel_relation

        subroutine qsqlrelationaltablemodel_set_join_mode(model, join_mode) bind(c, name="qsqlrelationaltablemodel_set_join_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: join_mode
        end subroutine qsqlrelationaltablemodel_set_join_mode

        function qsqlrelationaltablemodel_join_mode(model) bind(c, name="qsqlrelationaltablemodel_join_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int) :: qsqlrelationaltablemodel_join_mode
        end function qsqlrelationaltablemodel_join_mode

        function qsqlrelationaltablemodel_select(model) bind(c, name="qsqlrelationaltablemodel_select")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqlrelationaltablemodel_select
        end function qsqlrelationaltablemodel_select

        function qsqlrelationaltablemodel_revert_row(model, row) bind(c, name="qsqlrelationaltablemodel_revert_row")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: row
        end function qsqlrelationaltablemodel_revert_row

        subroutine qsqlrelationaltablemodel_set_filter(model, filter) bind(c, name="qsqlrelationaltablemodel_set_filter")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: filter
        end subroutine qsqlrelationaltablemodel_set_filter

        function qsqlrelationaltablemodel_filter(model) bind(c, name="qsqlrelationaltablemodel_filter")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: qsqlrelationaltablemodel_filter
        end function qsqlrelationaltablemodel_filter

        function qsqlrelationaltablemodel_order_by_clause(model) bind(c, name="qsqlrelationaltablemodel_order_by_clause")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: qsqlrelationaltablemodel_order_by_clause
        end function qsqlrelationaltablemodel_order_by_clause

        subroutine qsqlrelationaltablemodel_set_order_by_clause(model, clause) bind(c, name="qsqlrelationaltablemodel_set_order_by_clause")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: clause
        end subroutine qsqlrelationaltablemodel_set_order_by_clause

        !> QSqlQueryModel
        function qsqlquerymodel_new(parent, db) bind(c, name="qsqlquerymodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent, db
            type(c_ptr) :: qsqlquerymodel_new
        end function qsqlquerymodel_new

        subroutine qsqlquerymodel_set_query(model, query, db) bind(c, name="qsqlquerymodel_set_query")
            import :: c_ptr
            type(c_ptr), value :: model, query, db
        end subroutine qsqlquerymodel_set_query

        function qsqlquerymodel_query(model) bind(c, name="qsqlquerymodel_query")
            import :: c_ptr
            type(c_ptr), value :: model
            type(c_ptr) :: qsqlquerymodel_query
        end function qsqlquerymodel_query

        subroutine qsqlquerymodel_set_last_error(model, error) bind(c, name="qsqlquerymodel_set_last_error")
            import :: c_ptr
            type(c_ptr), value :: model, error
        end subroutine qsqlquerymodel_set_last_error

        function qsqlquerymodel_last_error(model) bind(c, name="qsqlquerymodel_last_error")
            import :: c_ptr
            type(c_ptr), value :: model
            type(c_ptr) :: qsqlquerymodel_last_error
        end function qsqlquerymodel_last_error

        subroutine qsqlquerymodel_clear(model) bind(c, name="qsqlquerymodel_clear")
            import :: c_ptr
            type(c_ptr), value :: model
        end subroutine qsqlquerymodel_clear

        function qsqlquerymodel_query_change(model) bind(c, name="qsqlquerymodel_query_change")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: qsqlquerymodel_query_change
        end function qsqlquerymodel_query_change

        function qsqlquerymodel_index_in_query(model, item) bind(c, name="qsqlquerymodel_index_in_query")
            import :: c_ptr, c_int
            type(c_ptr), value :: model, item
            integer(c_int) :: qsqlquerymodel_index_in_query
        end function qsqlquerymodel_index_in_query

        !> QSqlError
        function qsqlerror_new(type, driver_text, database_text, connection_text, number) bind(c, name="qsqlerror_new")
            import :: c_ptr, c_int, c_char
            integer(c_int), value :: type, number
            character(kind=c_char), dimension(*) :: driver_text, database_text, connection_text
            type(c_ptr) :: qsqlerror_new
        end function qsqlerror_new

        function qsqlerror_type(error) bind(c, name="qsqlerror_type")
            import :: c_ptr, c_int
            type(c_ptr), value :: error
            integer(c_int) :: qsqlerror_type
        end function qsqlerror_type

        function qsqlerror_number(error) bind(c, name="qsqlerror_number")
            import :: c_ptr, c_int
            type(c_ptr), value :: error
            integer(c_int) :: qsqlerror_number
        end function qsqlerror_number

        function qsqlerror_database_text(error) bind(c, name="qsqlerror_database_text")
            import :: c_ptr, c_char
            type(c_ptr), value :: error
            character(kind=c_char), dimension(*) :: qsqlerror_database_text
        end function qsqlerror_database_text

        function qsqlerror_driver_text(error) bind(c, name="qsqlerror_driver_text")
            import :: c_ptr, c_char
            type(c_ptr), value :: error
            character(kind=c_char), dimension(*) :: qsqlerror_driver_text
        end function qsqlerror_driver_text

        function qsqlerror_text(error) bind(c, name="qsqlerror_text")
            import :: c_ptr, c_char
            type(c_ptr), value :: error
            character(kind=c_char), dimension(*) :: qsqlerror_text
        end function qsqlerror_text

        function qsqlerror_is_valid(error) bind(c, name="qsqlerror_is_valid")
            import :: c_ptr, c_bool
            type(c_ptr), value :: error
            logical(c_bool) :: qsqlerror_is_valid
        end function qsqlerror_is_valid

        subroutine qsqlerror_swap(error1, error2) bind(c, name="qsqlerror_swap")
            import :: c_ptr
            type(c_ptr), value :: error1, error2
        end subroutine qsqlerror_swap

        !> QSqlDriver
        function qsqldriver_new(parent) bind(c, name="qsqldriver_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qsqldriver_new
        end function qsqldriver_new

        function qsqldriver_is_open(driver) bind(c, name="qsqldriver_is_open")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_is_open
        end function qsqldriver_is_open

        function qsqldriver_is_open_error(driver) bind(c, name="qsqldriver_is_open_error")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_is_open_error
        end function qsqldriver_is_open_error

        function qsqldriver_last_error(driver) bind(c, name="qsqldriver_last_error")
            import :: c_ptr
            type(c_ptr), value :: driver
            type(c_ptr) :: qsqldriver_last_error
        end function qsqldriver_last_error

        function qsqldriver_driver_name(driver) bind(c, name="qsqldriver_driver_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: qsqldriver_driver_name
        end function qsqldriver_driver_name

        function qsqldriver_has_feature(driver, feature) bind(c, name="qsqldriver_has_feature")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: driver
            integer(c_int), value :: feature
            logical(c_bool) :: qsqldriver_has_feature
        end function qsqldriver_has_feature

        function qsqldriver_open(driver, db, user, password, host, port, options) bind(c, name="qsqldriver_open")
            import :: c_ptr, c_char, c_int, c_bool
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: db, user, password, host, options
            integer(c_int), value :: port
            logical(c_bool) :: qsqldriver_open
        end function qsqldriver_open

        function qsqldriver_close(driver) bind(c, name="qsqldriver_close")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_close
        end function qsqldriver_close

        function qsqldriver_create_result(driver) bind(c, name="qsqldriver_create_result")
            import :: c_ptr
            type(c_ptr), value :: driver
            type(c_ptr) :: qsqldriver_create_result
        end function qsqldriver_create_result

        function qsqldriver_begin_transaction(driver) bind(c, name="qsqldriver_begin_transaction")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_begin_transaction
        end function qsqldriver_begin_transaction

        function qsqldriver_commit_transaction(driver) bind(c, name="qsqldriver_commit_transaction")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_commit_transaction
        end function qsqldriver_commit_transaction

        function qsqldriver_rollback_transaction(driver) bind(c, name="qsqldriver_rollback_transaction")
            import :: c_ptr, c_bool
            type(c_ptr), value :: driver
            logical(c_bool) :: qsqldriver_rollback_transaction
        end function qsqldriver_rollback_transaction

        function qsqldriver_sql_statement(driver, type, table_name, rec, prepared_statement) bind(c, name="qsqldriver_sql_statement")
            import :: c_ptr, c_int, c_char, c_bool
            type(c_ptr), value :: driver, rec
            integer(c_int), value :: type
            character(kind=c_char), dimension(*) :: table_name
            logical(c_bool), value :: prepared_statement
            character(kind=c_char), dimension(*) :: qsqldriver_sql_statement
        end function qsqldriver_sql_statement

        function qsqldriver_escape_identifier(driver, identifier, type) bind(c, name="qsqldriver_escape_identifier")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: identifier
            integer(c_int), value :: type
            character(kind=c_char), dimension(*) :: qsqldriver_escape_identifier
        end function qsqldriver_escape_identifier

        function qsqldriver_format_value(driver, field, trim_strings) bind(c, name="qsqldriver_format_value")
            import :: c_ptr, c_bool, c_char
            type(c_ptr), value :: field
            logical(c_bool), value :: trim_strings
            character(kind=c_char), dimension(*) :: qsqldriver_format_value
        end function qsqldriver_format_value

        function qsqldriver_record(driver, table) bind(c, name="qsqldriver_record")
            import :: c_ptr, c_char
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: table
            type(c_ptr) :: qsqldriver_record
        end function qsqldriver_record

        function qsqldriver_primary_index(driver, table_name) bind(c, name="qsqldriver_primary_index")
            import :: c_ptr, c_char
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: table_name
            type(c_ptr) :: qsqldriver_primary_index
        end function qsqldriver_primary_index

        function qsqldriver_is_identifier_escaped(driver, identifier, type) bind(c, name="qsqldriver_is_identifier_escaped")
            import :: c_ptr, c_char, c_int, c_bool
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: identifier
            integer(c_int), value :: type
            logical(c_bool) :: qsqldriver_is_identifier_escaped
        end function qsqldriver_is_identifier_escaped

        function qsqldriver_strip_delimiters(driver, identifier, type) bind(c, name="qsqldriver_strip_delimiters")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: driver
            character(kind=c_char), dimension(*) :: identifier
            integer(c_int), value :: type
            character(kind=c_char), dimension(*) :: qsqldriver_strip_delimiters
        end function qsqldriver_strip_delimiters

        function qsqldriver_conformance_level(driver) bind(c, name="qsqldriver_conformance_level")
            import :: c_ptr, c_int
            type(c_ptr), value :: driver
            integer(c_int) :: qsqldriver_conformance_level
        end function qsqldriver_conformance_level

    end interface

end module forge_qt_bindings
            import :: c_ptr, c_int
            type(c_ptr), value :: player
            integer(c_int) :: qmediaplayer_error
        end function qmediaplayer_error

        subroutine qmediaplayer_set_video_output(player, output) bind(c, name="qmediaplayer_set_video_output")
            import :: c_ptr
            type(c_ptr), value :: player, output
        end subroutine qmediaplayer_set_video_output

        !> QMediaContent
        function qmediacontent_new(url) bind(c, name="qmediacontent_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: url
            type(c_ptr) :: qmediacontent_new
        end function qmediacontent_new

        !> QMediaMetaData
        function qmediametadata_title(metadata) bind(c, name="qmediametadata_title")
            import :: c_ptr, c_char
            type(c_ptr), value :: metadata
            character(kind=c_char), dimension(*) :: qmediametadata_title
        end function qmediametadata_title

        function qmediametadata_artist(metadata) bind(c, name="qmediametadata_artist")
            import :: c_ptr, c_char
            type(c_ptr), value :: metadata
            character(kind=c_char), dimension(*) :: qmediametadata_artist
        end function qmediametadata_artist

        function qmediametadata_album(metadata) bind(c, name="qmediametadata_album")
            import :: c_ptr, c_char
            type(c_ptr), value :: metadata
            character(kind=c_char), dimension(*) :: qmediametadata_album
        end function qmediametadata_album

        function qmediametadata_duration(metadata) bind(c, name="qmediametadata_duration")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: metadata
            integer(c_long_long) :: qmediametadata_duration
        end function qmediametadata_duration

        !> QMediaRecorder
        function qmediarecorder_new(media_object, parent) bind(c, name="qmediarecorder_new")
            import :: c_ptr
            type(c_ptr), value :: media_object, parent
            type(c_ptr) :: qmediarecorder_new
        end function qmediarecorder_new

        subroutine qmediarecorder_set_output_location(recorder, location) bind(c, name="qmediarecorder_set_output_location")
            import :: c_ptr, c_char
            type(c_ptr), value :: recorder
            character(kind=c_char), dimension(*) :: location
        end subroutine qmediarecorder_set_output_location

        subroutine qmediarecorder_record(recorder) bind(c, name="qmediarecorder_record")
            import :: c_ptr
            type(c_ptr), value :: recorder
        end subroutine qmediarecorder_record

        subroutine qmediarecorder_stop(recorder) bind(c, name="qmediarecorder_stop")
            import :: c_ptr
            type(c_ptr), value :: recorder
        end subroutine qmediarecorder_stop

        function qmediarecorder_state(recorder) bind(c, name="qmediarecorder_state")
            import :: c_ptr, c_int
            type(c_ptr), value :: recorder
            integer(c_int) :: qmediarecorder_state
        end function qmediarecorder_state

        function qmediarecorder_error(recorder) bind(c, name="qmediarecorder_error")
            import :: c_ptr, c_int
            type(c_ptr), value :: recorder
            integer(c_int) :: qmediarecorder_error
        end function qmediarecorder_error

        subroutine qmediarecorder_set_audio_input(recorder, input) bind(c, name="qmediarecorder_set_audio_input")
            import :: c_ptr
            type(c_ptr), value :: recorder, input
        end subroutine qmediarecorder_set_audio_input

        subroutine qmediarecorder_set_video_input(recorder, input) bind(c, name="qmediarecorder_set_video_input")
            import :: c_ptr
            type(c_ptr), value :: recorder, input
        end subroutine qmediarecorder_set_video_input

        !> QAudioRecorder
        function qaudiorecorder_new(parent) bind(c, name="qaudiorecorder_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qaudiorecorder_new
        end function qaudiorecorder_new

        !> QVideoRecorder
        function qvideorecorder_new(parent) bind(c, name="qvideorecorder_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qvideorecorder_new
        end function qvideorecorder_new

        !> SQL Database Bindings

        !> QSqlDatabase
        function qsqldatabase_new() bind(c, name="qsqldatabase_new")
            import :: c_ptr
            type(c_ptr) :: qsqldatabase_new
        end function qsqldatabase_new

        function qsqldatabase_add_database(type, connection_name) bind(c, name="qsqldatabase_add_database")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: type
            character(kind=c_char), dimension(*) :: connection_name
            type(c_ptr) :: qsqldatabase_add_database
        end function qsqldatabase_add_database

        function qsqldatabase_database(connection_name, open_default) bind(c, name="qsqldatabase_database")
            import :: c_ptr, c_char, c_bool
            character(kind=c_char), dimension(*) :: connection_name
            logical(c_bool), value :: open_default
            type(c_ptr) :: qsqldatabase_database
        end function qsqldatabase_database

        subroutine qsqldatabase_remove_database(connection_name) bind(c, name="qsqldatabase_remove_database")
            import :: c_char
            character(kind=c_char), dimension(*) :: connection_name
        end subroutine qsqldatabase_remove_database

        function qsqldatabase_contains(connection_name) bind(c, name="qsqldatabase_contains")
            import :: c_bool, c_char
            character(kind=c_char), dimension(*) :: connection_name
            logical(c_bool) :: qsqldatabase_contains
        end function qsqldatabase_contains

        function qsqldatabase_connection_names() bind(c, name="qsqldatabase_connection_names")
            import :: c_ptr
            type(c_ptr) :: qsqldatabase_connection_names
        end function qsqldatabase_connection_names

        function qsqldatabase_drivers() bind(c, name="qsqldatabase_drivers")
            import :: c_ptr
            type(c_ptr) :: qsqldatabase_drivers
        end function qsqldatabase_drivers

        function qsqldatabase_is_driver_available(name) bind(c, name="qsqldatabase_is_driver_available")
            import :: c_bool, c_char
            character(kind=c_char), dimension(*) :: name
            logical(c_bool) :: qsqldatabase_is_driver_available
        end function qsqldatabase_is_driver_available

        subroutine qsqldatabase_register_sql_driver(name, creator) bind(c, name="qsqldatabase_register_sql_driver")
            import :: c_char, c_ptr
            character(kind=c_char), dimension(*) :: name
            type(c_ptr), value :: creator
        end subroutine qsqldatabase_register_sql_driver

        function qsqldatabase_clone_database(other, connection_name) bind(c, name="qsqldatabase_clone_database")
            import :: c_ptr, c_char
            type(c_ptr), value :: other
            character(kind=c_char), dimension(*) :: connection_name
            type(c_ptr) :: qsqldatabase_clone_database
        end function qsqldatabase_clone_database

        function qsqldatabase_default_connection() bind(c, name="qsqldatabase_default_connection")
            import :: c_ptr
            type(c_ptr) :: qsqldatabase_default_connection
        end function qsqldatabase_default_connection

        subroutine qsqldatabase_set_default_connection(connection) bind(c, name="qsqldatabase_set_default_connection")
            import :: c_ptr
            type(c_ptr), value :: connection
        end subroutine qsqldatabase_set_default_connection

        function qsqldatabase_open(db) bind(c, name="qsqldatabase_open")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_open
        end function qsqldatabase_open

        subroutine qsqldatabase_close(db) bind(c, name="qsqldatabase_close")
            import :: c_ptr
            type(c_ptr), value :: db
        end subroutine qsqldatabase_close

        function qsqldatabase_is_open(db) bind(c, name="qsqldatabase_is_open")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_is_open
        end function qsqldatabase_is_open

        function qsqldatabase_is_open_error(db) bind(c, name="qsqldatabase_is_open_error")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_is_open_error
        end function qsqldatabase_is_open_error

        function qsqldatabase_last_error(db) bind(c, name="qsqldatabase_last_error")
            import :: c_ptr
            type(c_ptr), value :: db
            type(c_ptr) :: qsqldatabase_last_error
        end function qsqldatabase_last_error

        subroutine qsqldatabase_set_database_name(db, name) bind(c, name="qsqldatabase_set_database_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: name
        end subroutine qsqldatabase_set_database_name

        function qsqldatabase_database_name(db) bind(c, name="qsqldatabase_database_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_database_name
        end function qsqldatabase_database_name

        subroutine qsqldatabase_set_user_name(db, name) bind(c, name="qsqldatabase_set_user_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: name
        end subroutine qsqldatabase_set_user_name

        function qsqldatabase_user_name(db) bind(c, name="qsqldatabase_user_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_user_name
        end function qsqldatabase_user_name

        subroutine qsqldatabase_set_password(db, password) bind(c, name="qsqldatabase_set_password")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: password
        end subroutine qsqldatabase_set_password

        function qsqldatabase_password(db) bind(c, name="qsqldatabase_password")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_password
        end function qsqldatabase_password

        subroutine qsqldatabase_set_host_name(db, host) bind(c, name="qsqldatabase_set_host_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: host
        end subroutine qsqldatabase_set_host_name

        function qsqldatabase_host_name(db) bind(c, name="qsqldatabase_host_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_host_name
        end function qsqldatabase_host_name

        subroutine qsqldatabase_set_port(db, port) bind(c, name="qsqldatabase_set_port")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int), value :: port
        end subroutine qsqldatabase_set_port

        function qsqldatabase_port(db) bind(c, name="qsqldatabase_port")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int) :: qsqldatabase_port
        end function qsqldatabase_port

        subroutine qsqldatabase_set_connect_options(db, options) bind(c, name="qsqldatabase_set_connect_options")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: options
        end subroutine qsqldatabase_set_connect_options

        function qsqldatabase_connect_options(db) bind(c, name="qsqldatabase_connect_options")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_connect_options
        end function qsqldatabase_connect_options

        function qsqldatabase_driver(db) bind(c, name="qsqldatabase_driver")
            import :: c_ptr
            type(c_ptr), value :: db
            type(c_ptr) :: qsqldatabase_driver
        end function qsqldatabase_driver

        function qsqldatabase_driver_name(db) bind(c, name="qsqldatabase_driver_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: db
            character(kind=c_char), dimension(*) :: qsqldatabase_driver_name
        end function qsqldatabase_driver_name

        subroutine qsqldatabase_set_numerical_precision_policy(db, precision_policy) bind(c, name="qsqldatabase_set_numerical_precision_policy")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int), value :: precision_policy
        end subroutine qsqldatabase_set_numerical_precision_policy

        function qsqldatabase_numerical_precision_policy(db) bind(c, name="qsqldatabase_numerical_precision_policy")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int) :: qsqldatabase_numerical_precision_policy
        end function qsqldatabase_numerical_precision_policy

        function qsqldatabase_transaction(db) bind(c, name="qsqldatabase_transaction")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_transaction
        end function qsqldatabase_transaction

        function qsqldatabase_commit(db) bind(c, name="qsqldatabase_commit")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_commit
        end function qsqldatabase_commit

        function qsqldatabase_rollback(db) bind(c, name="qsqldatabase_rollback")
            import :: c_ptr, c_bool
            type(c_ptr), value :: db
            logical(c_bool) :: qsqldatabase_rollback
        end function qsqldatabase_rollback

        subroutine qsqldatabase_set_transaction_mode(db, mode) bind(c, name="qsqldatabase_set_transaction_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int), value :: mode
        end subroutine qsqldatabase_set_transaction_mode

        function qsqldatabase_transaction_mode(db) bind(c, name="qsqldatabase_transaction_mode")
            import :: c_ptr, c_int
            type(c_ptr), value :: db
            integer(c_int) :: qsqldatabase_transaction_mode
        end function qsqldatabase_transaction_mode

        !> QSqlQuery
        function qsqlquery_new(db) bind(c, name="qsqlquery_new")
            import :: c_ptr
            type(c_ptr), value :: db
            type(c_ptr) :: qsqlquery_new
        end function qsqlquery_new

        function qsqlquery_exec(query, query_str) bind(c, name="qsqlquery_exec")
            import :: c_ptr, c_char, c_bool
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: query_str
            logical(c_bool) :: qsqlquery_exec
        end function qsqlquery_exec

        function qsqlquery_exec_batch(query, mode) bind(c, name="qsqlquery_exec_batch")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: query
            integer(c_int), value :: mode
            logical(c_bool) :: qsqlquery_exec_batch
        end function qsqlquery_exec_batch

        function qsqlquery_prepare(query, query_str) bind(c, name="qsqlquery_prepare")
            import :: c_ptr, c_char, c_bool
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: query_str
            logical(c_bool) :: qsqlquery_prepare
        end function qsqlquery_prepare

        subroutine qsqlquery_bind_value(query, placeholder, val, type) bind(c, name="qsqlquery_bind_value")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: placeholder
            type(c_ptr), value :: val
            integer(c_int), value :: type
        end subroutine qsqlquery_bind_value

        subroutine qsqlquery_add_bind_value(query, val, type) bind(c, name="qsqlquery_add_bind_value")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            type(c_ptr), value :: val
            integer(c_int), value :: type
        end subroutine qsqlquery_add_bind_value

        function qsqlquery_bound_value(query, placeholder) bind(c, name="qsqlquery_bound_value")
            import :: c_ptr, c_char
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: placeholder
            type(c_ptr) :: qsqlquery_bound_value
        end function qsqlquery_bound_value

        function qsqlquery_bound_values(query) bind(c, name="qsqlquery_bound_values")
            import :: c_ptr
            type(c_ptr), value :: query
            type(c_ptr) :: qsqlquery_bound_values
        end function qsqlquery_bound_values

        function qsqlquery_next(query) bind(c, name="qsqlquery_next")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_next
        end function qsqlquery_next

        function qsqlquery_previous(query) bind(c, name="qsqlquery_previous")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_previous
        end function qsqlquery_previous

        function qsqlquery_first(query) bind(c, name="qsqlquery_first")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_first
        end function qsqlquery_first

        function qsqlquery_last(query) bind(c, name="qsqlquery_last")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_last
        end function qsqlquery_last

        function qsqlquery_seek(query, index, relative) bind(c, name="qsqlquery_seek")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: query
            integer(c_int), value :: index
            logical(c_bool), value :: relative
            logical(c_bool) :: qsqlquery_seek
        end function qsqlquery_seek

        function qsqlquery_at(query) bind(c, name="qsqlquery_at")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            integer(c_int) :: qsqlquery_at
        end function qsqlquery_at

        function qsqlquery_size(query) bind(c, name="qsqlquery_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            integer(c_int) :: qsqlquery_size
        end function qsqlquery_size

        function qsqlquery_is_active(query) bind(c, name="qsqlquery_is_active")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_is_active
        end function qsqlquery_is_active

        function qsqlquery_is_forward_only(query) bind(c, name="qsqlquery_is_forward_only")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_is_forward_only
        end function qsqlquery_is_forward_only

        subroutine qsqlquery_set_forward_only(query, forward) bind(c, name="qsqlquery_set_forward_only")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool), value :: forward
        end subroutine qsqlquery_set_forward_only

        function qsqlquery_is_valid(query) bind(c, name="qsqlquery_is_valid")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_is_valid
        end function qsqlquery_is_valid

        function qsqlquery_is_null(query, field) bind(c, name="qsqlquery_is_null")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: query
            integer(c_int), value :: field
            logical(c_bool) :: qsqlquery_is_null
        end function qsqlquery_is_null

        function qsqlquery_value(query, index) bind(c, name="qsqlquery_value")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            integer(c_int), value :: index
            type(c_ptr) :: qsqlquery_value
        end function qsqlquery_value

        function qsqlquery_record(query) bind(c, name="qsqlquery_record")
            import :: c_ptr
            type(c_ptr), value :: query
            type(c_ptr) :: qsqlquery_record
        end function qsqlquery_record

        function qsqlquery_driver(query) bind(c, name="qsqlquery_driver")
            import :: c_ptr
            type(c_ptr), value :: query
            type(c_ptr) :: qsqlquery_driver
        end function qsqlquery_driver

        function qsqlquery_numerical_precision_policy(query) bind(c, name="qsqlquery_numerical_precision_policy")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            integer(c_int) :: qsqlquery_numerical_precision_policy
        end function qsqlquery_numerical_precision_policy

        subroutine qsqlquery_set_numerical_precision_policy(query, precision_policy) bind(c, name="qsqlquery_set_numerical_precision_policy")
            import :: c_ptr, c_int
            type(c_ptr), value :: query
            integer(c_int), value :: precision_policy
        end subroutine qsqlquery_set_numerical_precision_policy

        function qsqlquery_last_query(query) bind(c, name="qsqlquery_last_query")
            import :: c_ptr, c_char
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: qsqlquery_last_query
        end function qsqlquery_last_query

        function qsqlquery_last_error(query) bind(c, name="qsqlquery_last_error")
            import :: c_ptr
            type(c_ptr), value :: query
            type(c_ptr) :: qsqlquery_last_error
        end function qsqlquery_last_error

        function qsqlquery_executed_query(query) bind(c, name="qsqlquery_executed_query")
            import :: c_ptr, c_char
            type(c_ptr), value :: query
            character(kind=c_char), dimension(*) :: qsqlquery_executed_query
        end function qsqlquery_executed_query

        subroutine qsqlquery_clear(query) bind(c, name="qsqlquery_clear")
            import :: c_ptr
            type(c_ptr), value :: query
        end subroutine qsqlquery_clear

        subroutine qsqlquery_finish(query) bind(c, name="qsqlquery_finish")
            import :: c_ptr
            type(c_ptr), value :: query
        end subroutine qsqlquery_finish

        function qsqlquery_next_result(query) bind(c, name="qsqlquery_next_result")
            import :: c_ptr, c_bool
            type(c_ptr), value :: query
            logical(c_bool) :: qsqlquery_next_result
        end function qsqlquery_next_result

        !> QSqlRecord
        function qsqlrecord_new() bind(c, name="qsqlrecord_new")
            import :: c_ptr
            type(c_ptr) :: qsqlrecord_new
        end function qsqlrecord_new

        subroutine qsqlrecord_append(record, field) bind(c, name="qsqlrecord_append")
            import :: c_ptr
            type(c_ptr), value :: record, field
        end subroutine qsqlrecord_append

        subroutine qsqlrecord_replace(record, pos, field) bind(c, name="qsqlrecord_replace")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: pos
            type(c_ptr), value :: field
        end subroutine qsqlrecord_replace

        subroutine qsqlrecord_remove(record, pos) bind(c, name="qsqlrecord_remove")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: pos
        end subroutine qsqlrecord_remove

        subroutine qsqlrecord_insert(record, pos, field) bind(c, name="qsqlrecord_insert")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: pos
            type(c_ptr), value :: field
        end subroutine qsqlrecord_insert

        subroutine qsqlrecord_clear(record) bind(c, name="qsqlrecord_clear")
            import :: c_ptr
            type(c_ptr), value :: record
        end subroutine qsqlrecord_clear

        subroutine qsqlrecord_clear_values(record) bind(c, name="qsqlrecord_clear_values")
            import :: c_ptr
            type(c_ptr), value :: record
        end subroutine qsqlrecord_clear_values

        function qsqlrecord_count(record) bind(c, name="qsqlrecord_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int) :: qsqlrecord_count
        end function qsqlrecord_count

        function qsqlrecord_contains(record, name) bind(c, name="qsqlrecord_contains")
            import :: c_ptr, c_char, c_bool
            type(c_ptr), value :: record
            character(kind=c_char), dimension(*) :: name
            logical(c_bool) :: qsqlrecord_contains
        end function qsqlrecord_contains

        function qsqlrecord_field(record, index) bind(c, name="qsqlrecord_field")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: index
            type(c_ptr) :: qsqlrecord_field
        end function qsqlrecord_field

        function qsqlrecord_field_name(record, index) bind(c, name="qsqlrecord_field_name")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: record
            integer(c_int), value :: index
            character(kind=c_char), dimension(*) :: qsqlrecord_field_name
        end function qsqlrecord_field_name

        function qsqlrecord_index_of(record, name) bind(c, name="qsqlrecord_index_of")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: record
            character(kind=c_char), dimension(*) :: name
            integer(c_int) :: qsqlrecord_index_of
        end function qsqlrecord_index_of

        function qsqlrecord_is_empty(record) bind(c, name="qsqlrecord_is_empty")
            import :: c_ptr, c_bool
            type(c_ptr), value :: record
            logical(c_bool) :: qsqlrecord_is_empty
        end function qsqlrecord_is_empty

        function qsqlrecord_is_generated(record, index) bind(c, name="qsqlrecord_is_generated")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: record
            integer(c_int), value :: index
            logical(c_bool) :: qsqlrecord_is_generated
        end function qsqlrecord_is_generated

        function qsqlrecord_is_null(record, index) bind(c, name="qsqlrecord_is_null")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: record
            integer(c_int), value :: index
            logical(c_bool) :: qsqlrecord_is_null
        end function qsqlrecord_is_null

        function qsqlrecord_name_to_index(record, name) bind(c, name="qsqlrecord_name_to_index")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: record
            character(kind=c_char), dimension(*) :: name
            integer(c_int) :: qsqlrecord_name_to_index
        end function qsqlrecord_name_to_index

        subroutine qsqlrecord_set_generated(record, index, generated) bind(c, name="qsqlrecord_set_generated")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: record
            integer(c_int), value :: index
            logical(c_bool), value :: generated
        end subroutine qsqlrecord_set_generated

        subroutine qsqlrecord_set_null(record, index) bind(c, name="qsqlrecord_set_null")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: index
        end subroutine qsqlrecord_set_null

        subroutine qsqlrecord_set_value(record, index, val) bind(c, name="qsqlrecord_set_value")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: index
            type(c_ptr), value :: val
        end subroutine qsqlrecord_set_value

        function qsqlrecord_value(record, index) bind(c, name="qsqlrecord_value")
            import :: c_ptr, c_int
            type(c_ptr), value :: record
            integer(c_int), value :: index
            type(c_ptr) :: qsqlrecord_value
        end function qsqlrecord_value

        !> QSqlField
        function qsqlfield_new(field_name, type) bind(c, name="qsqlfield_new")
            import :: c_ptr, c_char, c_int
            character(kind=c_char), dimension(*) :: field_name
            integer(c_int), value :: type
            type(c_ptr) :: qsqlfield_new
        end function qsqlfield_new

        subroutine qsqlfield_set_value(field, value) bind(c, name="qsqlfield_set_value")
            import :: c_ptr
            type(c_ptr), value :: field, value
        end subroutine qsqlfield_set_value

        function qsqlfield_value(field) bind(c, name="qsqlfield_value")
            import :: c_ptr
            type(c_ptr), value :: field
            type(c_ptr) :: qsqlfield_value
        end function qsqlfield_value

        subroutine qsqlfield_set_name(field, name) bind(c, name="qsqlfield_set_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: field
            character(kind=c_char), dimension(*) :: name
        end subroutine qsqlfield_set_name

        function qsqlfield_name(field) bind(c, name="qsqlfield_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: field
            character(kind=c_char), dimension(*) :: qsqlfield_name
        end function qsqlfield_name

        function qsqlfield_is_null(field) bind(c, name="qsqlfield_is_null")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool) :: qsqlfield_is_null
        end function qsqlfield_is_null

        subroutine qsqlfield_set_null(field) bind(c, name="qsqlfield_set_null")
            import :: c_ptr
            type(c_ptr), value :: field
        end subroutine qsqlfield_set_null

        function qsqlfield_is_valid(field) bind(c, name="qsqlfield_is_valid")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool) :: qsqlfield_is_valid
        end function qsqlfield_is_valid

        function qsqlfield_is_auto_value(field) bind(c, name="qsqlfield_is_auto_value")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool) :: qsqlfield_is_auto_value
        end function qsqlfield_is_auto_value

        subroutine qsqlfield_set_auto_value(field, auto_value) bind(c, name="qsqlfield_set_auto_value")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool), value :: auto_value
        end subroutine qsqlfield_set_auto_value

        function qsqlfield_is_generated(field) bind(c, name="qsqlfield_is_generated")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool) :: qsqlfield_is_generated
        end function qsqlfield_is_generated

        subroutine qsqlfield_set_generated(field, generated) bind(c, name="qsqlfield_set_generated")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool), value :: generated
        end subroutine qsqlfield_set_generated

        function qsqlfield_is_read_only(field) bind(c, name="qsqlfield_is_read_only")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool) :: qsqlfield_is_read_only
        end function qsqlfield_is_read_only

        subroutine qsqlfield_set_read_only(field, read_only) bind(c, name="qsqlfield_set_read_only")
            import :: c_ptr, c_bool
            type(c_ptr), value :: field
            logical(c_bool), value :: read_only
        end subroutine qsqlfield_set_read_only

        function qsqlfield_type(field) bind(c, name="qsqlfield_type")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int) :: qsqlfield_type
        end function qsqlfield_type

        subroutine qsqlfield_set_type(field, type) bind(c, name="qsqlfield_set_type")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int), value :: type
        end subroutine qsqlfield_set_type

        function qsqlfield_required_status(field) bind(c, name="qsqlfield_required_status")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int) :: qsqlfield_required_status
        end function qsqlfield_required_status

        subroutine qsqlfield_set_required_status(field, required_status) bind(c, name="qsqlfield_set_required_status")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int), value :: required_status
        end subroutine qsqlfield_set_required_status

        function qsqlfield_length(field) bind(c, name="qsqlfield_length")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int) :: qsqlfield_length
        end function qsqlfield_length

        subroutine qsqlfield_set_length(field, field_length) bind(c, name="qsqlfield_set_length")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int), value :: field_length
        end subroutine qsqlfield_set_length

        function qsqlfield_precision(field) bind(c, name="qsqlfield_precision")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int) :: qsqlfield_precision
        end function qsqlfield_precision

        subroutine qsqlfield_set_precision(field, precision) bind(c, name="qsqlfield_set_precision")
            import :: c_ptr, c_int
            type(c_ptr), value :: field
            integer(c_int), value :: precision
        end subroutine qsqlfield_set_precision

        function qsqlfield_default_value(field) bind(c, name="qsqlfield_default_value")
            import :: c_ptr
            type(c_ptr), value :: field
            type(c_ptr) :: qsqlfield_default_value
        end function qsqlfield_default_value

        subroutine qsqlfield_set_default_value(field, default_value) bind(c, name="qsqlfield_set_default_value")
            import :: c_ptr
            type(c_ptr), value :: field, default_value
        end subroutine qsqlfield_set_default_value

        function qsqlfield_table_name(field) bind(c, name="qsqlfield_table_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: field
            character(kind=c_char), dimension(*) :: qsqlfield_table_name
        end function qsqlfield_table_name

        subroutine qsqlfield_set_table_name(field, table_name) bind(c, name="qsqlfield_set_table_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: field
            character(kind=c_char), dimension(*) :: table_name
        end subroutine qsqlfield_set_table_name

        !> QSqlTableModel
        function qsqltablemodel_new(parent, db) bind(c, name="qsqltablemodel_new")
            import :: c_ptr
            type(c_ptr), value :: parent, db
            type(c_ptr) :: qsqltablemodel_new
        end function qsqltablemodel_new

        subroutine qsqltablemodel_set_table(model, table_name) bind(c, name="qsqltablemodel_set_table")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: table_name
        end subroutine qsqltablemodel_set_table

        function qsqltablemodel_table_name(model) bind(c, name="qsqltablemodel_table_name")
            import :: c_ptr, c_char
            type(c_ptr), value :: model
            character(kind=c_char), dimension(*) :: qsqltablemodel_table_name
        end function qsqltablemodel_table_name

        subroutine qsqltablemodel_set_edit_strategy(model, strategy) bind(c, name="qsqltablemodel_set_edit_strategy")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int), value :: strategy
        end subroutine qsqltablemodel_set_edit_strategy

        function qsqltablemodel_edit_strategy(model) bind(c, name="qsqltablemodel_edit_strategy")
            import :: c_ptr, c_int
            type(c_ptr), value :: model
            integer(c_int) :: qsqltablemodel_edit_strategy
        end function qsqltablemodel_edit_strategy

        function qsqltablemodel_select(model) bind(c, name="qsqltablemodel_select")
            import :: c_ptr, c_bool
            type(c_ptr), value :: model
            logical(c_bool) :: q