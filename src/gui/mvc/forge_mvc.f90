!> @brief Qt MVC Framework Implementation
!> @details Comprehensive Model-View-Controller implementation for ForGE
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_mvc
    use iso_c_binding
    use forge_types
    use forge_signals
    use forge_qt_bindings
    implicit none
    private

    public :: QAbstractItemModel, QModelIndex, Qt_ItemFlags
    public :: QAbstractItemView, QListView, QTableView, QTreeView
    public :: QItemDelegate, QItemSelectionModel
    public :: QAbstractListModel, QAbstractTableModel, QAbstractTreeModel
    public :: Qt_DisplayRole, Qt_EditRole, Qt_ToolTipRole, Qt_StatusTipRole
    public :: Qt_UserRole, Qt_Horizontal, Qt_Vertical
    public :: SelectionMode, SingleSelection, MultiSelection, ExtendedSelection
    public :: SelectionBehavior, SelectItems, SelectRows, SelectColumns

    !> Data roles for model data access
    integer, parameter, public :: Qt_DisplayRole = 0
    integer, parameter, public :: Qt_EditRole = 2
    integer, parameter, public :: Qt_ToolTipRole = 3
    integer, parameter, public :: Qt_StatusTipRole = 4
    integer, parameter, public :: Qt_UserRole = 32

    !> Orientation constants
    integer, parameter, public :: Qt_Horizontal = 1
    integer, parameter, public :: Qt_Vertical = 2

    !> Selection modes
    integer, parameter, public :: NoSelection = 0
    integer, parameter, public :: SingleSelection = 1
    integer, parameter, public :: MultiSelection = 2
    integer, parameter, public :: ExtendedSelection = 3

    !> Selection behavior
    integer, parameter, public :: SelectItems = 0
    integer, parameter, public :: SelectRows = 1
    integer, parameter, public :: SelectColumns = 2

    type :: SelectionMode
        integer :: value = SingleSelection
    end type SelectionMode

    type :: SelectionBehavior
        integer :: value = SelectItems
    end type SelectionBehavior

    !> @brief Item flags for model items
    integer, parameter :: Qt_NoItemFlags = 0
    integer, parameter :: Qt_ItemIsSelectable = 1
    integer, parameter :: Qt_ItemIsEditable = 2
    integer, parameter :: Qt_ItemIsDragEnabled = 4
    integer, parameter :: Qt_ItemIsDropEnabled = 8
    integer, parameter :: Qt_ItemIsUserCheckable = 16
    integer, parameter :: Qt_ItemIsEnabled = 32
    integer, parameter :: Qt_ItemIsTristate = 64

    type :: Qt_ItemFlags
        integer :: value = Qt_NoItemFlags
    contains
        procedure :: set_flag => qt_itemflags_set_flag
        procedure :: test_flag => qt_itemflags_test_flag
        procedure :: clear_flag => qt_itemflags_clear_flag
    end type Qt_ItemFlags

    !> @brief Model index for identifying items in models
    type :: QModelIndex
        private
        type(qmodelindex) :: qt_index
    contains
        procedure :: row => qmodelindex_row
        procedure :: column => qmodelindex_column
        procedure :: parent => qmodelindex_parent
        procedure :: internal_pointer => qmodelindex_internal_pointer
        procedure :: is_valid => qmodelindex_is_valid
        procedure :: data => qmodelindex_data
        procedure :: set_data => qmodelindex_set_data
        procedure :: flags => qmodelindex_flags
        procedure :: sibling => qmodelindex_sibling
    end type QModelIndex

    !> @brief Abstract base class for all item models
    type, abstract :: QAbstractItemModel
        private
        type(qabstractitemmodel) :: qt_model
        type(signal_model_changed) :: data_changed
        type(signal_model_changed) :: header_data_changed
        type(signal_model_reset) :: model_reset
        type(signal_rows_changed) :: rows_inserted
        type(signal_rows_changed) :: rows_removed
        type(signal_columns_changed) :: columns_inserted
        type(signal_columns_changed) :: columns_removed
        type(signal_layout_changed) :: layout_changed
    contains
        procedure :: get_qt_model => qabstractitemmodel_get_qt_model
        procedure :: set_qt_model => qabstractitemmodel_set_qt_model
        procedure(model_index_interface), deferred :: index
        procedure(model_parent_interface), deferred :: parent
        procedure(model_row_count_interface), deferred :: row_count
        procedure(model_column_count_interface), deferred :: column_count
        procedure(model_data_interface), deferred :: data
        procedure(model_set_data_interface), deferred :: set_data
        procedure(model_flags_interface), deferred :: flags
        procedure(model_header_data_interface), deferred :: header_data
        procedure(model_set_header_data_interface), deferred :: set_header_data
        procedure(model_insert_rows_interface), deferred :: insert_rows
        procedure(model_remove_rows_interface), deferred :: remove_rows
        procedure(model_insert_columns_interface), deferred :: insert_columns
        procedure(model_remove_columns_interface), deferred :: remove_columns
        procedure :: begin_insert_rows => qabstractitemmodel_begin_insert_rows
        procedure :: end_insert_rows => qabstractitemmodel_end_insert_rows
        procedure :: begin_remove_rows => qabstractitemmodel_begin_remove_rows
        procedure :: end_remove_rows => qabstractitemmodel_end_remove_rows
        procedure :: begin_insert_columns => qabstractitemmodel_begin_insert_columns
        procedure :: end_insert_columns => qabstractitemmodel_end_insert_columns
        procedure :: begin_remove_columns => qabstractitemmodel_begin_remove_columns
        procedure :: end_remove_columns => qabstractitemmodel_end_remove_columns
        procedure :: emit_data_changed => qabstractitemmodel_emit_data_changed
        procedure :: emit_header_data_changed => qabstractitemmodel_emit_header_data_changed
        procedure :: emit_layout_changed => qabstractitemmodel_emit_layout_changed
        procedure :: emit_model_reset => qabstractitemmodel_emit_model_reset
    end type QAbstractItemModel

    !> @brief Abstract base class for item views
    type, abstract :: QAbstractItemView
        private
        type(qabstractitemview) :: qt_view
        type(QAbstractItemModel), pointer :: model => null()
        type(QItemSelectionModel), pointer :: selection_model => null()
        type(QModelIndex) :: root_index
        type(SelectionMode) :: selection_mode
        type(SelectionBehavior) :: selection_behavior
        logical :: alternating_row_colors = .false.
        type(signal_view) :: clicked
        type(signal_view) :: double_clicked
        type(signal_view) :: activated
        type(signal_view) :: entered
        type(signal_view) :: pressed
        type(signal_view) :: viewport_entered
    contains
        procedure :: get_qt_view => qabstractitemview_get_qt_view
        procedure :: set_qt_view => qabstractitemview_set_qt_view
        procedure :: set_model => qabstractitemview_set_model
        procedure :: get_model => qabstractitemview_get_model
        procedure :: set_root_index => qabstractitemview_set_root_index
        procedure :: get_root_index => qabstractitemview_get_root_index
        procedure :: set_selection_model => qabstractitemview_set_selection_model
        procedure :: get_selection_model => qabstractitemview_get_selection_model
        procedure :: set_selection_mode => qabstractitemview_set_selection_mode
        procedure :: get_selection_mode => qabstractitemview_get_selection_mode
        procedure :: set_selection_behavior => qabstractitemview_set_selection_behavior
        procedure :: get_selection_behavior => qabstractitemview_get_selection_behavior
        procedure :: set_alternating_row_colors => qabstractitemview_set_alternating_row_colors
        procedure :: get_alternating_row_colors => qabstractitemview_get_alternating_row_colors
        procedure :: current_index => qabstractitemview_current_index
        procedure :: set_current_index => qabstractitemview_set_current_index
        procedure :: scroll_to => qabstractitemview_scroll_to
        procedure :: scroll_to_top => qabstractitemview_scroll_to_top
        procedure :: scroll_to_bottom => qabstractitemview_scroll_to_bottom
        procedure :: update => qabstractitemview_update
        procedure :: repaint => qabstractitemview_repaint
        procedure :: set_item_delegate => qabstractitemview_set_item_delegate
        procedure :: item_delegate => qabstractitemview_item_delegate
    end type QAbstractItemView

    !> @brief List view widget
    type, extends(QAbstractItemView) :: QListView
        private
        type(qlistview) :: qt_listview
        integer :: view_mode = 0  ! ListMode
        logical :: word_wrap = .false.
        logical :: uniform_item_sizes = .true.
    contains
        procedure :: set_view_mode => qlistview_set_view_mode
        procedure :: get_view_mode => qlistview_get_view_mode
        procedure :: set_word_wrap => qlistview_set_word_wrap
        procedure :: get_word_wrap => qlistview_get_word_wrap
        procedure :: set_uniform_item_sizes => qlistview_set_uniform_item_sizes
        procedure :: get_uniform_item_sizes => qlistview_get_uniform_item_sizes
        procedure :: init => qlistview_init
    end type QListView

    !> @brief Table view widget
    type, extends(QAbstractItemView) :: QTableView
        private
        type(qtableview) :: qt_tableview
        logical :: show_grid = .true.
        logical :: sorting_enabled = .false.
        integer :: grid_style = 0
        integer :: selection_behavior = SelectItems
        integer :: selection_mode = SingleSelection
    contains
        procedure :: set_show_grid => qtableview_set_show_grid
        procedure :: show_grid => qtableview_show_grid
        procedure :: set_grid_style => qtableview_set_grid_style
        procedure :: grid_style => qtableview_grid_style
        procedure :: set_sorting_enabled => qtableview_set_sorting_enabled
        procedure :: is_sorting_enabled => qtableview_is_sorting_enabled
        procedure :: set_horizontal_header => qtableview_set_horizontal_header
        procedure :: horizontal_header => qtableview_horizontal_header
        procedure :: set_vertical_header => qtableview_set_vertical_header
        procedure :: vertical_header => qtableview_vertical_header
        procedure :: set_column_width => qtableview_set_column_width
        procedure :: column_width => qtableview_column_width
        procedure :: set_row_height => qtableview_set_row_height
        procedure :: row_height => qtableview_row_height
        procedure :: resize_column_to_contents => qtableview_resize_column_to_contents
        procedure :: resize_columns_to_contents => qtableview_resize_columns_to_contents
        procedure :: resize_row_to_contents => qtableview_resize_row_to_contents
        procedure :: resize_rows_to_contents => qtableview_resize_rows_to_contents
        procedure :: init => qtableview_init
    end type QTableView

    !> @brief Tree view widget
    type, extends(QAbstractItemView) :: QTreeView
        private
        type(qtreeview) :: qt_treeview
        logical :: root_is_decorated = .true.
        logical :: items_expandable = .true.
        logical :: sorting_enabled = .false.
        logical :: header_hidden = .false.
        logical :: all_columns_show_focus = .false.
        integer :: indentation = 20
    contains
        procedure :: set_root_is_decorated => qtreeview_set_root_is_decorated
        procedure :: root_is_decorated => qtreeview_root_is_decorated
        procedure :: set_items_expandable => qtreeview_set_items_expandable
        procedure :: items_expandable => qtreeview_items_expandable
        procedure :: set_sorting_enabled => qtreeview_set_sorting_enabled
        procedure :: is_sorting_enabled => qtreeview_is_sorting_enabled
        procedure :: set_header_hidden => qtreeview_set_header_hidden
        procedure :: is_header_hidden => qtreeview_is_header_hidden
        procedure :: set_all_columns_show_focus => qtreeview_set_all_columns_show_focus
        procedure :: all_columns_show_focus => qtreeview_all_columns_show_focus
        procedure :: set_indentation => qtreeview_set_indentation
        procedure :: indentation => qtreeview_indentation
        procedure :: expand => qtreeview_expand
        procedure :: collapse => qtreeview_collapse
        procedure :: expand_all => qtreeview_expand_all
        procedure :: collapse_all => qtreeview_collapse_all
        procedure :: expand_to_depth => qtreeview_expand_to_depth
        procedure :: resize_column_to_contents => qtreeview_resize_column_to_contents
        procedure :: set_header => qtreeview_set_header
        procedure :: header => qtreeview_header
        procedure :: init => qtreeview_init
    end type QTreeView

    !> @brief Item delegate for custom rendering and editing
    type :: QItemDelegate
        private
        type(qitemdelegate) :: qt_delegate
        logical :: has_clipping = .true.
    contains
        procedure :: get_qt_delegate => qitemdelegate_get_qt_delegate
        procedure :: set_qt_delegate => qitemdelegate_set_qt_delegate
        procedure :: paint => qitemdelegate_paint
        procedure :: size_hint => qitemdelegate_size_hint
        procedure :: create_editor => qitemdelegate_create_editor
        procedure :: set_editor_data => qitemdelegate_set_editor_data
        procedure :: set_model_data => qitemdelegate_set_model_data
        procedure :: update_editor_geometry => qitemdelegate_update_editor_geometry
        procedure :: set_has_clipping => qitemdelegate_set_has_clipping
        procedure :: has_clipping => qitemdelegate_has_clipping
        procedure :: init => qitemdelegate_init
    end type QItemDelegate

    !> @brief Selection model for managing item selection
    type :: QItemSelectionModel
        private
        type(qitemselectionmodel) :: qt_selection_model
        type(QAbstractItemModel), pointer :: model => null()
        type(signal_selection) :: current_changed
        type(signal_selection) :: current_row_changed
        type(signal_selection) :: current_column_changed
        type(signal_selection) :: selection_changed
        type(signal_selection) :: model_changed
    contains
        procedure :: get_qt_selection_model => qitemselectionmodel_get_qt_selection_model
        procedure :: set_qt_selection_model => qitemselectionmodel_set_qt_selection_model
        procedure :: current_index => qitemselectionmodel_current_index
        procedure :: selection => qitemselectionmodel_selection
        procedure :: selected_indexes => qitemselectionmodel_selected_indexes
        procedure :: selected_rows => qitemselectionmodel_selected_rows
        procedure :: selected_columns => qitemselectionmodel_selected_columns
        procedure :: has_selection => qitemselectionmodel_has_selection
        procedure :: is_selected => qitemselectionmodel_is_selected
        procedure :: is_row_selected => qitemselectionmodel_is_row_selected
        procedure :: is_column_selected => qitemselectionmodel_is_column_selected
        procedure :: set_current_index => qitemselectionmodel_set_current_index
        procedure :: select => qitemselectionmodel_select
        procedure :: clear => qitemselectionmodel_clear
        procedure :: clear_selection => qitemselectionmodel_clear_selection
        procedure :: reset => qitemselectionmodel_reset
        procedure :: emit_current_changed => qitemselectionmodel_emit_current_changed
        procedure :: emit_selection_changed => qitemselectionmodel_emit_selection_changed
        procedure :: init => qitemselectionmodel_init
    end type QItemSelectionModel

    !> @brief Abstract list model base class
    type, extends(QAbstractItemModel) :: QAbstractListModel
        private
        type(qabstractlistmodel) :: qt_listmodel
    contains
        procedure :: index => qabstractlistmodel_index
        procedure :: parent => qabstractlistmodel_parent
        procedure :: row_count => qabstractlistmodel_row_count
        procedure :: column_count => qabstractlistmodel_column_count
        procedure :: init => qabstractlistmodel_init
    end type QAbstractListModel

    !> @brief Abstract table model base class
    type, extends(QAbstractItemModel) :: QAbstractTableModel
        private
        type(qabstracttablemodel) :: qt_tablemodel
    contains
        procedure :: index => qabstracttablemodel_index
        procedure :: parent => qabstracttablemodel_parent
        procedure :: row_count => qabstracttablemodel_row_count
        procedure :: column_count => qabstracttablemodel_column_count
        procedure :: init => qabstracttablemodel_init
    end type QAbstractTableModel

    !> @brief Abstract tree model base class
    type, extends(QAbstractItemModel) :: QAbstractTreeModel
        private
        type(qabstracttreemodel) :: qt_treemodel
    contains
        procedure :: index => qabstracttreemodel_index
        procedure :: parent => qabstracttreemodel_parent
        procedure :: row_count => qabstracttreemodel_row_count
        procedure :: column_count => qabstracttreemodel_column_count
        procedure :: init => qabstracttreemodel_init
    end type QAbstractTreeModel

    !> Signal types for MVC framework
    type :: signal_model_changed
        private
        type(QModelIndex) :: top_left
        type(QModelIndex) :: bottom_right
        integer, dimension(:), allocatable :: roles
    contains
        procedure :: emit => signal_model_changed_emit
    end type signal_model_changed

    type :: signal_model_reset
    contains
        procedure :: emit => signal_model_reset_emit
    end type signal_model_reset

    type :: signal_rows_changed
        private
        type(QModelIndex) :: parent
        integer :: first
        integer :: last
    contains
        procedure :: emit => signal_rows_changed_emit
    end type signal_rows_changed

    type :: signal_columns_changed
        private
        type(QModelIndex) :: parent
        integer :: first
        integer :: last
    contains
        procedure :: emit => signal_columns_changed_emit
    end type signal_columns_changed

    type :: signal_layout_changed
    contains
        procedure :: emit => signal_layout_changed_emit
    end type signal_layout_changed

    type :: signal_view
        private
        type(QModelIndex) :: index
    contains
        procedure :: emit => signal_view_emit
    end type signal_view

    type :: signal_selection
        private
        type(QModelIndex) :: current
        type(QModelIndex) :: previous
    contains
        procedure :: emit => signal_selection_emit
    end type signal_selection

    !> Abstract interfaces
    abstract interface
        function model_index_interface(this, row, column, parent) result(index)
            import :: QAbstractItemModel, QModelIndex, c_int
            class(QAbstractItemModel), intent(in) :: this
            integer(c_int), intent(in) :: row, column
            type(QModelIndex), intent(in) :: parent
            type(QModelIndex) :: index
        end function model_index_interface

        function model_parent_interface(this, child) result(parent)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: child
            type(QModelIndex) :: parent
        end function model_parent_interface

        function model_row_count_interface(this, parent) result(count)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: parent
            integer :: count
        end function model_row_count_interface

        function model_column_count_interface(this, parent) result(count)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: parent
            integer :: count
        end function model_column_count_interface

        function model_data_interface(this, index, role) result(data)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            integer, intent(in) :: role
            character(len=:), allocatable :: data
        end function model_data_interface

        function model_set_data_interface(this, index, value, role) result(success)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            character(len=*), intent(in) :: value
            integer, intent(in) :: role
            logical :: success
        end function model_set_data_interface

        function model_flags_interface(this, index) result(flags)
            import :: QAbstractItemModel, QModelIndex, Qt_ItemFlags
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            type(Qt_ItemFlags) :: flags
        end function model_flags_interface

        function model_header_data_interface(this, section, orientation, role) result(data)
            import :: QAbstractItemModel
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: section, orientation, role
            character(len=:), allocatable :: data
        end function model_header_data_interface

        function model_set_header_data_interface(this, section, orientation, value, role) result(success)
            import :: QAbstractItemModel
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: section, orientation
            character(len=*), intent(in) :: value
            integer, intent(in) :: role
            logical :: success
        end function model_set_header_data_interface

        function model_insert_rows_interface(this, row, count, parent) result(success)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: row, count
            type(QModelIndex), intent(in) :: parent
            logical :: success
        end function model_insert_rows_interface

        function model_remove_rows_interface(this, row, count, parent) result(success)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: row, count
            type(QModelIndex), intent(in) :: parent
            logical :: success
        end function model_remove_rows_interface

        function model_insert_columns_interface(this, column, count, parent) result(success)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: column, count
            type(QModelIndex), intent(in) :: parent
            logical :: success
        end function model_insert_columns_interface

        function model_remove_columns_interface(this, column, count, parent) result(success)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            integer, intent(in) :: column, count
            type(QModelIndex), intent(in) :: parent
            logical :: success
        end function model_remove_columns_interface
    end interface

contains

    ! ========== Qt_ItemFlags Implementation ==========

    subroutine qt_itemflags_set_flag(this, flag)
        class(Qt_ItemFlags), intent(inout) :: this
        integer, intent(in) :: flag
        this%value = ior(this%value, flag)
    end subroutine qt_itemflags_set_flag

    function qt_itemflags_test_flag(this, flag) result(test)
        class(Qt_ItemFlags), intent(in) :: this
        integer, intent(in) :: flag
        logical :: test
        test = (iand(this%value, flag) /= 0)
    end function qt_itemflags_test_flag

    subroutine qt_itemflags_clear_flag(this, flag)
        class(Qt_ItemFlags), intent(inout) :: this
        integer, intent(in) :: flag
        this%value = iand(this%value, not(flag))
    end subroutine qt_itemflags_clear_flag

    ! ========== QModelIndex Implementation ==========

    function qmodelindex_row(this) result(row)
        class(QModelIndex), intent(in) :: this
        integer :: row
        row = qmodelindex_row(this%qt_index%ptr)
    end function qmodelindex_row

    function qmodelindex_column(this) result(column)
        class(QModelIndex), intent(in) :: this
        integer :: column
        column = qmodelindex_column(this%qt_index%ptr)
    end function qmodelindex_column

    function qmodelindex_parent(this) result(parent)
        class(QModelIndex), intent(in) :: this
        type(QModelIndex) :: parent
        parent%qt_index%ptr = qmodelindex_parent(this%qt_index%ptr)
    end function qmodelindex_parent

    function qmodelindex_internal_pointer(this) result(ptr)
        class(QModelIndex), intent(in) :: this
        type(c_ptr) :: ptr
        ptr = qmodelindex_internal_pointer(this%qt_index%ptr)
    end function qmodelindex_internal_pointer

    function qmodelindex_is_valid(this) result(valid)
        class(QModelIndex), intent(in) :: this
        logical :: valid
        valid = qmodelindex_is_valid(this%qt_index%ptr)
    end function qmodelindex_is_valid

    function qmodelindex_data(this, role) result(data)
        class(QModelIndex), intent(in) :: this
        integer, intent(in) :: role
        character(len=:), allocatable :: data
        character(len=FORGE_MAX_STRING_LEN) :: buffer
        buffer = qabstractitemmodel_data(c_null_ptr, this%qt_index%ptr, role)
        data = trim(buffer)
    end function qmodelindex_data

    function qmodelindex_set_data(this, value, role) result(success)
        class(QModelIndex), intent(in) :: this
        character(len=*), intent(in) :: value
        integer, intent(in) :: role
        logical :: success
        success = qabstractitemmodel_set_data(c_null_ptr, this%qt_index%ptr, &
            value // c_null_char, role)
    end function qmodelindex_set_data

    function qmodelindex_flags(this) result(flags)
        class(QModelIndex), intent(in) :: this
        type(Qt_ItemFlags) :: flags
        flags%value = qabstractitemmodel_flags(c_null_ptr, this%qt_index%ptr)
    end function qmodelindex_flags

    function qmodelindex_sibling(this, row, column) result(sibling)
        class(QModelIndex), intent(in) :: this
        integer, intent(in) :: row, column
        type(QModelIndex) :: sibling
        ! Create sibling index
        sibling%qt_index%ptr = qabstractitemmodel_index(c_null_ptr, row, column, &
            this%qt_index%ptr)
    end function qmodelindex_sibling

    ! ========== QAbstractItemModel Implementation ==========

    function qabstractitemmodel_get_qt_model(this) result(qt_model)
        class(QAbstractItemModel), intent(in) :: this
        type(qabstractitemmodel) :: qt_model
        qt_model = this%qt_model
    end function qabstractitemmodel_get_qt_model

    subroutine qabstractitemmodel_set_qt_model(this, qt_model)
        class(QAbstractItemModel), intent(inout) :: this
        type(qabstractitemmodel), intent(in) :: qt_model
        this%qt_model = qt_model
    end subroutine qabstractitemmodel_set_qt_model

    subroutine qabstractitemmodel_begin_insert_rows(this, parent, first, last)
        class(QAbstractItemModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: parent
        integer, intent(in) :: first, last
        ! Notify Qt about row insertion
    end subroutine qabstractitemmodel_begin_insert_rows

    subroutine qabstractitemmodel_end_insert_rows(this)
        class(QAbstractItemModel), intent(inout) :: this
        ! End row insertion notification
    end subroutine qabstractitemmodel_end_insert_rows

    subroutine qabstractitemmodel_begin_remove_rows(this, parent, first, last)
        class(QAbstractItemModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: parent
        integer, intent(in) :: first, last
        ! Notify Qt about row removal
    end subroutine qabstractitemmodel_begin_remove_rows

    subroutine qabstractitemmodel_end_remove_rows(this)
        class(QAbstractItemModel), intent(inout) :: this
        ! End row removal notification
    end subroutine qabstractitemmodel_end_remove_rows

    subroutine qabstractitemmodel_begin_insert_columns(this, parent, first, last)
        class(QAbstractItemModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: parent
        integer, intent(in) :: first, last
        ! Notify Qt about column insertion
    end subroutine qabstractitemmodel_begin_insert_columns

    subroutine qabstractitemmodel_end_insert_columns(this)
        class(QAbstractItemModel), intent(inout) :: this
        ! End column insertion notification
    end subroutine qabstractitemmodel_end_insert_columns

    subroutine qabstractitemmodel_begin_remove_columns(this, parent, first, last)
        class(QAbstractItemModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: parent
        integer, intent(in) :: first, last
        ! Notify Qt about column removal
    end subroutine qabstractitemmodel_begin_remove_columns

    subroutine qabstractitemmodel_end_remove_columns(this)
        class(QAbstractItemModel), intent(inout) :: this
        ! End column removal notification
    end subroutine qabstractitemmodel_end_remove_columns

    subroutine qabstractitemmodel_emit_data_changed(this, top_left, bottom_right, roles)
        class(QAbstractItemModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: top_left, bottom_right
        integer, dimension(:), intent(in), optional :: roles
        call this%data_changed%emit(top_left, bottom_right, roles)
    end subroutine qabstractitemmodel_emit_data_changed

    subroutine qabstractitemmodel_emit_header_data_changed(this, orientation, first, last)
        class(QAbstractItemModel), intent(inout) :: this
        integer, intent(in) :: orientation, first, last
        call this%header_data_changed%emit(orientation, first, last)
    end subroutine qabstractitemmodel_emit_header_data_changed

    subroutine qabstractitemmodel_emit_layout_changed(this)
        class(QAbstractItemModel), intent(inout) :: this
        call this%layout_changed%emit()
    end subroutine qabstractitemmodel_emit_layout_changed

    subroutine qabstractitemmodel_emit_model_reset(this)
        class(QAbstractItemModel), intent(inout) :: this
        call this%model_reset%emit()
    end subroutine qabstractitemmodel_emit_model_reset

    ! ========== QAbstractItemView Implementation ==========

    function qabstractitemview_get_qt_view(this) result(qt_view)
        class(QAbstractItemView), intent(in) :: this
        type(qabstractitemview) :: qt_view
        qt_view = this%qt_view
    end function qabstractitemview_get_qt_view

    subroutine qabstractitemview_set_qt_view(this, qt_view)
        class(QAbstractItemView), intent(inout) :: this
        type(qabstractitemview), intent(in) :: qt_view
        this%qt_view = qt_view
    end subroutine qabstractitemview_set_qt_view

    subroutine qabstractitemview_set_model(this, model)
        class(QAbstractItemView), intent(inout) :: this
        type(QAbstractItemModel), target, intent(in) :: model
        this%model => model
        call qabstractitemview_set_model(this%qt_view%ptr, model%get_qt_model()%ptr)
    end subroutine qabstractitemview_set_model

    function qabstractitemview_get_model(this) result(model)
        class(QAbstractItemView), intent(in) :: this
        type(QAbstractItemModel), pointer :: model
        model => this%model
    end function qabstractitemview_get_model

    subroutine qabstractitemview_set_root_index(this, index)
        class(QAbstractItemView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        this%root_index = index
        call qabstractitemview_set_root_index(this%qt_view%ptr, index%qt_index%ptr)
    end subroutine qabstractitemview_set_root_index

    function qabstractitemview_get_root_index(this) result(index)
        class(QAbstractItemView), intent(in) :: this
        type(QModelIndex) :: index
        index%qt_index%ptr = qabstractitemview_root_index(this%qt_view%ptr)
    end function qabstractitemview_get_root_index

    subroutine qabstractitemview_set_selection_model(this, selection_model)
        class(QAbstractItemView), intent(inout) :: this
        type(QItemSelectionModel), target, intent(in) :: selection_model
        this%selection_model => selection_model
        call qabstractitemview_set_selection_model(this%qt_view%ptr, &
            selection_model%get_qt_selection_model()%ptr)
    end subroutine qabstractitemview_set_selection_model

    function qabstractitemview_get_selection_model(this) result(selection_model)
        class(QAbstractItemView), intent(in) :: this
        type(QItemSelectionModel), pointer :: selection_model
        selection_model => this%selection_model
    end function qabstractitemview_get_selection_model

    subroutine qabstractitemview_set_selection_mode(this, mode)
        class(QAbstractItemView), intent(inout) :: this
        integer, intent(in) :: mode
        this%selection_mode%value = mode
    end subroutine qabstractitemview_set_selection_mode

    function qabstractitemview_get_selection_mode(this) result(mode)
        class(QAbstractItemView), intent(in) :: this
        integer :: mode
        mode = this%selection_mode%value
    end function qabstractitemview_get_selection_mode

    subroutine qabstractitemview_set_selection_behavior(this, behavior)
        class(QAbstractItemView), intent(inout) :: this
        integer, intent(in) :: behavior
        this%selection_behavior%value = behavior
    end subroutine qabstractitemview_set_selection_behavior

    function qabstractitemview_get_selection_behavior(this) result(behavior)
        class(QAbstractItemView), intent(in) :: this
        integer :: behavior
        behavior = this%selection_behavior%value
    end function qabstractitemview_get_selection_behavior

    subroutine qabstractitemview_set_alternating_row_colors(this, enable)
        class(QAbstractItemView), intent(inout) :: this
        logical, intent(in) :: enable
        this%alternating_row_colors = enable
    end subroutine qabstractitemview_set_alternating_row_colors

    function qabstractitemview_get_alternating_row_colors(this) result(enable)
        class(QAbstractItemView), intent(in) :: this
        logical :: enable
        enable = this%alternating_row_colors
    end function qabstractitemview_get_alternating_row_colors

    function qabstractitemview_current_index(this) result(index)
        class(QAbstractItemView), intent(in) :: this
        type(QModelIndex) :: index
        index%qt_index%ptr = qabstractitemview_current_index(this%qt_view%ptr)
    end function qabstractitemview_current_index

    subroutine qabstractitemview_set_current_index(this, index)
        class(QAbstractItemView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        call qabstractitemview_set_current_index(this%qt_view%ptr, index%qt_index%ptr)
    end subroutine qabstractitemview_set_current_index

    subroutine qabstractitemview_scroll_to(this, index, hint)
        class(QAbstractItemView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        integer, intent(in), optional :: hint
        integer :: scroll_hint
        if (present(hint)) then
            scroll_hint = hint
        else
            scroll_hint = 0  ! EnsureVisible
        end if
        call qabstractitemview_scroll_to(this%qt_view%ptr, index%qt_index%ptr, scroll_hint)
    end subroutine qabstractitemview_scroll_to

    subroutine qabstractitemview_scroll_to_top(this)
        class(QAbstractItemView), intent(inout) :: this
        ! Scroll to top implementation
    end subroutine qabstractitemview_scroll_to_top

    subroutine qabstractitemview_scroll_to_bottom(this)
        class(QAbstractItemView), intent(inout) :: this
        ! Scroll to bottom implementation
    end subroutine qabstractitemview_scroll_to_bottom

    subroutine qabstractitemview_update(this, index)
        class(QAbstractItemView), intent(inout) :: this
        type(QModelIndex), intent(in), optional :: index
        ! Update view implementation
    end subroutine qabstractitemview_update

    subroutine qabstractitemview_repaint(this, index)
        class(QAbstractItemView), intent(inout) :: this
        type(QModelIndex), intent(in), optional :: index
        ! Repaint view implementation
    end subroutine qabstractitemview_repaint

    subroutine qabstractitemview_set_item_delegate(this, delegate)
        class(QAbstractItemView), intent(inout) :: this
        type(QItemDelegate), intent(in) :: delegate
        ! Set item delegate implementation
    end subroutine qabstractitemview_set_item_delegate

    function qabstractitemview_item_delegate(this) result(delegate)
        class(QAbstractItemView), intent(in) :: this
        type(QItemDelegate) :: delegate
        ! Get item delegate implementation
    end function qabstractitemview_item_delegate

    ! ========== QListView Implementation ==========

    subroutine qlistview_init(this, parent)
        class(QListView), intent(inout) :: this
        type(c_ptr), intent(in), optional :: parent
        type(c_ptr) :: parent_ptr
        if (present(parent)) then
            parent_ptr = parent
        else
            parent_ptr = c_null_ptr
        end if
        this%qt_listview%ptr = qlistview_new(parent_ptr)
        call this%set_qt_view(this%qt_listview)
    end subroutine qlistview_init

    subroutine qlistview_set_view_mode(this, mode)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: mode
        this%view_mode = mode
    end subroutine qlistview_set_view_mode

    function qlistview_get_view_mode(this) result(mode)
        class(QListView), intent(in) :: this
        integer :: mode
        mode = this%view_mode
    end function qlistview_get_view_mode

    subroutine qlistview_set_word_wrap(this, wrap)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: wrap
        this%word_wrap = wrap
    end subroutine qlistview_set_word_wrap

    function qlistview_get_word_wrap(this) result(wrap)
        class(QListView), intent(in) :: this
        logical :: wrap
        wrap = this%word_wrap
    end function qlistview_get_word_wrap

    subroutine qlistview_set_uniform_item_sizes(this, uniform)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: uniform
        this%uniform_item_sizes = uniform
    end subroutine qlistview_set_uniform_item_sizes

    function qlistview_get_uniform_item_sizes(this) result(uniform)
        class(QListView), intent(in) :: this
        logical :: uniform
        uniform = this%uniform_item_sizes
    end function qlistview_get_uniform_item_sizes

    ! ========== QTableView Implementation ==========

    subroutine qtableview_init(this, parent)
        class(QTableView), intent(inout) :: this
        type(c_ptr), intent(in), optional :: parent
        type(c_ptr) :: parent_ptr
        if (present(parent)) then
            parent_ptr = parent
        else
            parent_ptr = c_null_ptr
        end if
        this%qt_tableview%ptr = qtableview_new(parent_ptr)
        call this%set_qt_view(this%qt_tableview)
    end subroutine qtableview_init

    subroutine qtableview_set_show_grid(this, show)
        class(QTableView), intent(inout) :: this
        logical, intent(in) :: show
        this%show_grid = show
    end subroutine qtableview_set_show_grid

    function qtableview_show_grid(this) result(show)
        class(QTableView), intent(in) :: this
        logical :: show
        show = this%show_grid
    end function qtableview_show_grid

    subroutine qtableview_set_grid_style(this, style)
        class(QTableView), intent(inout) :: this
        integer, intent(in) :: style
        this%grid_style = style
    end subroutine qtableview_set_grid_style

    function qtableview_grid_style(this) result(style)
        class(QTableView), intent(in) :: this
        integer :: style
        style = this%grid_style
    end function qtableview_grid_style

    subroutine qtableview_set_sorting_enabled(this, enable)
        class(QTableView), intent(inout) :: this
        logical, intent(in) :: enable
        this%sorting_enabled = enable
    end subroutine qtableview_set_sorting_enabled

    function qtableview_is_sorting_enabled(this) result(enabled)
        class(QTableView), intent(in) :: this
        logical :: enabled
        enabled = this%sorting_enabled
    end function qtableview_is_sorting_enabled

    subroutine qtableview_set_horizontal_header(this, header)
        class(QTableView), intent(inout) :: this
        type(c_ptr), intent(in) :: header
        ! Set horizontal header implementation
    end subroutine qtableview_set_horizontal_header

    function qtableview_horizontal_header(this) result(header)
        class(QTableView), intent(in) :: this
        type(c_ptr) :: header
        ! Get horizontal header implementation
    end function qtableview_horizontal_header

    subroutine qtableview_set_vertical_header(this, header)
        class(QTableView), intent(inout) :: this
        type(c_ptr), intent(in) :: header
        ! Set vertical header implementation
    end subroutine qtableview_set_vertical_header

    function qtableview_vertical_header(this) result(header)
        class(QTableView), intent(in) :: this
        type(c_ptr) :: header
        ! Get vertical header implementation
    end function qtableview_vertical_header

    subroutine qtableview_set_column_width(this, column, width)
        class(QTableView), intent(inout) :: this
        integer, intent(in) :: column, width
        ! Set column width implementation
    end subroutine qtableview_set_column_width

    function qtableview_column_width(this, column) result(width)
        class(QTableView), intent(in) :: this
        integer, intent(in) :: column
        integer :: width
        ! Get column width implementation
    end function qtableview_column_width

    subroutine qtableview_set_row_height(this, row, height)
        class(QTableView), intent(inout) :: this
        integer, intent(in) :: row, height
        ! Set row height implementation
    end subroutine qtableview_set_row_height

    function qtableview_row_height(this, row) result(height)
        class(QTableView), intent(in) :: this
        integer, intent(in) :: row
        integer :: height
        ! Get row height implementation
    end function qtableview_row_height

    subroutine qtableview_resize_column_to_contents(this, column)
        class(QTableView), intent(inout) :: this
        integer, intent(in) :: column
        ! Resize column to contents implementation
    end subroutine qtableview_resize_column_to_contents

    subroutine qtableview_resize_columns_to_contents(this)
        class(QTableView), intent(inout) :: this
        ! Resize all columns to contents implementation
    end subroutine qtableview_resize_columns_to_contents

    subroutine qtableview_resize_row_to_contents(this, row)
        class(QTableView), intent(inout) :: this
        integer, intent(in) :: row
        ! Resize row to contents implementation
    end subroutine qtableview_resize_row_to_contents

    subroutine qtableview_resize_rows_to_contents(this)
        class(QTableView), intent(inout) :: this
        ! Resize all rows to contents implementation
    end subroutine qtableview_resize_rows_to_contents

    ! ========== QTreeView Implementation ==========

    subroutine qtreeview_init(this, parent)
        class(QTreeView), intent(inout) :: this
        type(c_ptr), intent(in), optional :: parent
        type(c_ptr) :: parent_ptr
        if (present(parent)) then
            parent_ptr = parent
        else
            parent_ptr = c_null_ptr
        end if
        this%qt_treeview%ptr = qtreeview_new(parent_ptr)
        call this%set_qt_view(this%qt_treeview)
    end subroutine qtreeview_init

    subroutine qtreeview_set_root_is_decorated(this, show)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: show
        this%root_is_decorated = show
    end subroutine qtreeview_set_root_is_decorated

    function qtreeview_root_is_decorated(this) result(show)
        class(QTreeView), intent(in) :: this
        logical :: show
        show = this%root_is_decorated
    end function qtreeview_root_is_decorated

    subroutine qtreeview_set_items_expandable(this, expandable)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: expandable
        this%items_expandable = expandable
    end subroutine qtreeview_set_items_expandable

    function qtreeview_items_expandable(this) result(expandable)
        class(QTreeView), intent(in) :: this
        logical :: expandable
        expandable = this%items_expandable
    end function qtreeview_items_expandable

    subroutine qtreeview_set_sorting_enabled(this, enable)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: enable
        this%sorting_enabled = enable
    end subroutine qtreeview_set_sorting_enabled

    function qtreeview_is_sorting_enabled(this) result(enabled)
        class(QTreeView), intent(in) :: this
        logical :: enabled
        enabled = this%sorting_enabled
    end function qtreeview_is_sorting_enabled

    subroutine qtreeview_set_header_hidden(this, hide)
        class(QTreeView), intent(inout