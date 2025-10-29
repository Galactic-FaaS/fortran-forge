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
    end interface

end module forge_qt_bindings