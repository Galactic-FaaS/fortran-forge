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

    end interface

end module forge_qt_bindings