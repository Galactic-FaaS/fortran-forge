!> @brief ListView and TreeView widgets
!> @details Model-View implementation for lists and trees
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_listview
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    use forge_string_utils
    use forge_containers
    implicit none
    private

    public :: QListView, QListWidget, QTreeView, QTreeWidget
    public :: ViewMode, ListMode, IconMode
    public :: SelectionMode, SingleSelection, MultiSelection, ExtendedSelection
    public :: SelectionBehavior, SelectItems, SelectRows

    !> View mode constants
    integer, parameter :: ListMode = 0
    integer, parameter :: IconMode = 1

    type :: ViewMode
        integer :: value = ListMode
    end type ViewMode

    !> Selection mode constants
    integer, parameter :: NoSelection = 0
    integer, parameter :: SingleSelection = 1
    integer, parameter :: MultiSelection = 2
    integer, parameter :: ExtendedSelection = 3
    integer, parameter :: ContiguousSelection = 4

    type :: SelectionMode
        integer :: value = SingleSelection
    end type SelectionMode

    !> Selection behavior constants
    integer, parameter :: SelectItems = 0
    integer, parameter :: SelectRows = 1
    integer, parameter :: SelectColumns = 2

    type :: SelectionBehavior
        integer :: value = SelectItems
    end type SelectionBehavior

    !> @brief Abstract item model base class
    type, abstract :: QAbstractItemModel
        integer :: row_count = 0
        integer :: column_count = 1
    contains
        procedure(model_index_interface), deferred :: index
        procedure(model_parent_interface), deferred :: parent
        procedure(model_data_interface), deferred :: data
        procedure(model_set_data_interface), deferred :: set_data
        procedure(model_row_count_interface), deferred :: row_count
        procedure(model_column_count_interface), deferred :: column_count
        procedure(model_flags_interface), deferred :: flags
    end type QAbstractItemModel

    !> Model index
    type :: QModelIndex
        integer :: row = -1
        integer :: column = -1
        type(c_ptr) :: internal_ptr = c_null_ptr
    end type QModelIndex

    !> @brief Item flags
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
    end type Qt_ItemFlags

    !> @brief List view widget (Model-View)
    type, extends(forge_widget) :: QListView
        private
        type(c_ptr) :: model_ptr = c_null_ptr
        type(QModelIndex) :: root_index
        type(ViewMode) :: view_mode
        type(SelectionMode) :: selection_mode
        type(SelectionBehavior) :: selection_behavior
        logical :: alternating_row_colors = .false.
        logical :: word_wrap = .false.
        logical :: uniform_item_sizes = .true.
        type(signal_int) :: clicked
        type(signal_int) :: double_clicked
        type(signal_int) :: activated
        type(signal_int) :: current_changed
        type(signal_int) :: selection_changed
    contains
        procedure :: set_model => listview_set_model
        procedure :: get_model => listview_get_model
        procedure :: set_root_index => listview_set_root_index
        procedure :: set_view_mode => listview_set_view_mode
        procedure :: set_selection_mode => listview_set_selection_mode
        procedure :: set_selection_behavior => listview_set_selection_behavior
        procedure :: set_alternating_row_colors => listview_set_alternating
        procedure :: set_word_wrap => listview_set_word_wrap
        procedure :: set_uniform_item_sizes => listview_set_uniform
        procedure :: current_index => listview_current_index
        procedure :: selection_model => listview_selection_model
        procedure :: scroll_to => listview_scroll_to
        procedure :: scroll_to_bottom => listview_scroll_to_bottom
        procedure :: scroll_to_top => listview_scroll_to_top
    end type QListView

    !> @brief Tree view widget
    type, extends(QListView) :: QTreeView
        private
        logical :: root_is_decorated = .true.
        logical :: items_expandable = .true.
        logical :: sorting_enabled = .false.
        logical :: header_hidden = .false.
        logical :: all_columns_show_focus = .false.
        type(signal_int) :: expanded
        type(signal_int) :: collapsed
    contains
        procedure :: set_root_is_decorated => treeview_set_root_decorated
        procedure :: set_items_expandable => treeview_set_items_expandable
        procedure :: set_sorting_enabled => treeview_set_sorting_enabled
        procedure :: set_header_hidden => treeview_set_header_hidden
        procedure :: expand => treeview_expand
        procedure :: collapse => treeview_collapse
        procedure :: expand_all => treeview_expand_all
        procedure :: collapse_all => treeview_collapse_all
        procedure :: resize_column_to_contents => treeview_resize_column
    end type QTreeView

    !> @brief List widget (convenience, no model needed)
    type, extends(QListView) :: QListWidget
        private
        type(QList_string) :: items
        type(QList_string) :: tooltips
        type(QList_string) :: icons
        logical :: sorting_enabled = .false.
        type(signal_int) :: item_clicked
        type(signal_int) :: item_double_clicked
    contains
        procedure :: add_item => listwidget_add_item
        procedure :: add_items => listwidget_add_items
        procedure :: insert_item => listwidget_insert_item
        procedure :: take_item => listwidget_take_item
        procedure :: remove_item_widget => listwidget_remove_item_widget
        procedure :: set_item_text => listwidget_set_item_text
        procedure :: get_item_text => listwidget_get_item_text
        procedure :: set_item_tooltip => listwidget_set_item_tooltip
        procedure :: set_item_icon => listwidget_set_item_icon
        procedure :: count => listwidget_count
        procedure :: clear => listwidget_clear
        procedure :: set_sorting_enabled => listwidget_set_sorting
        procedure :: sort_items => listwidget_sort_items
    end type QListWidget

    !> @brief Tree widget (hierarchical)
    type, extends(QTreeView) :: QTreeWidget
        private
        type(QList_string) :: headers
        type(tree_item), pointer :: root_item => null()
    contains
        procedure :: set_header_labels => treewidget_set_header_labels
        procedure :: set_header_label => treewidget_set_header_label
        procedure :: header_item => treewidget_header_item
        procedure :: top_level_item => treewidget_top_level_item
        procedure :: add_top_level_item => treewidget_add_top_level_item
        procedure :: take_top_level_item => treewidget_take_top_level_item
        procedure :: top_level_item_count => treewidget_top_level_item_count
    end type QTreeWidget

    !> @brief Tree item for hierarchical data
    type :: tree_item
        private
        type(tree_item), pointer :: parent => null()
        type(tree_item), pointer :: children(:) => null()
        integer :: child_count = 0
        type(QList_string) :: data  ! Column data
        type(tree_item), pointer :: next_sibling => null()
        type(tree_item), pointer :: prev_sibling => null()
    contains
        procedure :: add_child => treeitem_add_child
        procedure :: remove_child => treeitem_remove_child
        procedure :: set_text => treeitem_set_text
        procedure :: get_text => treeitem_get_text
        procedure :: set_data => treeitem_set_data
        procedure :: get_data => treeitem_get_data
        procedure :: child => treeitem_child
        procedure :: child_count => treeitem_child_count
        procedure :: parent => treeitem_parent
        procedure :: row => treeitem_row
    end type tree_item

    !> Abstract interfaces for model operations
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

        function model_data_interface(this, index, role) result(data)
            import :: QAbstractItemModel, QModelIndex, c_int
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            integer(c_int), intent(in) :: role
            character(len=:), allocatable :: data
        end function model_data_interface

        function model_set_data_interface(this, index, value, role) result(success)
            import :: QAbstractItemModel, QModelIndex, c_int
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            character(len=*), intent(in) :: value
            integer(c_int), intent(in) :: role
            logical :: success
        end function model_set_data_interface

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

        function model_flags_interface(this, index) result(flags)
            import :: QAbstractItemModel, QModelIndex
            class(QAbstractItemModel), intent(in) :: this
            type(QModelIndex), intent(in) :: index
            type(Qt_ItemFlags) :: flags
        end function model_flags_interface

    end interface

contains

    ! ========== QListView Implementation ==========

    subroutine listview_set_model(this, model)
        class(QListView), intent(inout) :: this
        type(c_ptr), intent(in) :: model
        this%model_ptr = model
    end subroutine listview_set_model

    function listview_get_model(this) result(model)
        class(QListView), intent(in) :: this
        type(c_ptr) :: model
        model = this%model_ptr
    end function listview_get_model

    subroutine listview_set_root_index(this, index)
        class(QListView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        this%root_index = index
    end subroutine listview_set_root_index

    subroutine listview_set_view_mode(this, mode)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: mode
        this%view_mode%value = mode
    end subroutine listview_set_view_mode

    subroutine listview_set_selection_mode(this, mode)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: mode
        this%selection_mode%value = mode
    end subroutine listview_set_selection_mode

    subroutine listview_set_selection_behavior(this, behavior)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: behavior
        this%selection_behavior%value = behavior
    end subroutine listview_set_selection_behavior

    subroutine listview_set_alternating(this, alternating)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: alternating
        this%alternating_row_colors = alternating
    end subroutine listview_set_alternating

    subroutine listview_set_word_wrap(this, wrap)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: wrap
        this%word_wrap = wrap
    end subroutine listview_set_word_wrap

    subroutine listview_set_uniform(this, uniform)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: uniform
        this%uniform_item_sizes = uniform
    end subroutine listview_set_uniform

    function listview_current_index(this) result(index)
        class(QListView), intent(in) :: this
        type(QModelIndex) :: index
        ! Would return current selection
    end function listview_current_index

    function listview_selection_model(this) result(model)
        class(QListView), intent(in) :: this
        type(c_ptr) :: model
        ! Would return selection model
    end function listview_selection_model

    subroutine listview_scroll_to(this, index, hint)
        class(QListView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        integer, intent(in), optional :: hint
        ! Scroll to ensure item is visible
    end subroutine listview_scroll_to

    subroutine listview_scroll_to_bottom(this)
        class(QListView), intent(inout) :: this
        ! Scroll to bottom
    end subroutine listview_scroll_to_bottom

    subroutine listview_scroll_to_top(this)
        class(QListView), intent(inout) :: this
        ! Scroll to top
    end subroutine listview_scroll_to_top

    ! ========== QListWidget Implementation ==========

    subroutine listwidget_add_item(this, text, tooltip, icon)
        class(QListWidget), intent(inout) :: this
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: tooltip, icon

        call this%items%append(text)
        if (present(tooltip)) call this%tooltips%append(tooltip)
        if (present(icon)) call this%icons%append(icon)
    end subroutine listwidget_add_item

    subroutine listwidget_add_items(this, texts)
        class(QListWidget), intent(inout) :: this
        character(len=*), intent(in) :: texts(:)
        integer :: i

        do i = 1, size(texts)
            call this%add_item(texts(i))
        end do
    end subroutine listwidget_add_items

    subroutine listwidget_insert_item(this, row, text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: text

        ! Insert at position
        call this%items%insert(row, text)
    end subroutine listwidget_insert_item

    function listwidget_take_item(this, row) result(text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=:), allocatable :: text

        if (row >= 0 .and. row < this%items%size()) then
            text = this%items%at(row)
            call this%items%remove(row)
        else
            allocate(character(len=0) :: text)
        end if
    end function listwidget_take_item

    subroutine listwidget_remove_item_widget(this, row)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row

        if (row >= 0 .and. row < this%items%size()) then
            call this%items%remove(row)
        end if
    end subroutine listwidget_remove_item_widget

    subroutine listwidget_set_item_text(this, row, text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: text

        if (row >= 0 .and. row < this%items%size()) then
            call this%items%set(row, text)
        end if
    end subroutine listwidget_set_item_text

    function listwidget_get_item_text(this, row) result(text)
        class(QListWidget), intent(in) :: this
        integer, intent(in) :: row
        character(len=:), allocatable :: text

        if (row >= 0 .and. row < this%items%size()) then
            text = this%items%at(row)
        else
            allocate(character(len=0) :: text)
        end if
    end function listwidget_get_item_text

    subroutine listwidget_set_item_tooltip(this, row, tooltip)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: tooltip

        if (row >= 0 .and. row < this%tooltips%size()) then
            call this%tooltips%set(row, tooltip)
        end if
    end subroutine listwidget_set_item_tooltip

    subroutine listwidget_set_item_icon(this, row, icon)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: icon

        if (row >= 0 .and. row < this%icons%size()) then
            call this%icons%set(row, icon)
        end if
    end subroutine listwidget_set_item_icon

    function listwidget_count(this) result(cnt)
        class(QListWidget), intent(in) :: this
        integer :: cnt
        cnt = this%items%size()
    end function listwidget_count

    subroutine listwidget_clear(this)
        class(QListWidget), intent(inout) :: this
        call this%items%clear()
        call this%tooltips%clear()
        call this%icons%clear()
    end subroutine listwidget_clear

    subroutine listwidget_set_sorting(this, enabled)
        class(QListWidget), intent(inout) :: this
        logical, intent(in) :: enabled
        this%sorting_enabled = enabled
        if (enabled) call this%sort_items()
    end subroutine listwidget_set_sorting

    subroutine listwidget_sort_items(this)
        class(QListWidget), intent(inout) :: this
        ! Simple bubble sort for demonstration
        integer :: i, j
        character(len=:), allocatable :: temp

        do i = 1, this%items%size() - 1
            do j = i + 1, this%items%size()
                if (this%items%at(i) > this%items%at(j)) then
                    temp = this%items%at(i)
                    call this%items%set(i, this%items%at(j))
                    call this%items%set(j, temp)
                end if
            end do
        end do
    end subroutine listwidget_sort_items

    ! ========== QTreeView Implementation ==========

    subroutine treeview_set_root_decorated(this, show)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: show
        this%root_is_decorated = show
    end subroutine treeview_set_root_decorated

    subroutine treeview_set_items_expandable(this, expandable)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: expandable
        this%items_expandable = expandable
    end subroutine treeview_set_items_expandable

    subroutine treeview_set_sorting_enabled(this, enable)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: enable
        this%sorting_enabled = enable
    end subroutine treeview_set_sorting_enabled

    subroutine treeview_set_header_hidden(this, hide)
        class(QTreeView), intent(inout) :: this
        logical, intent(in) :: hide
        this%header_hidden = hide
    end subroutine treeview_set_header_hidden

    subroutine treeview_expand(this, index)
        class(QTreeView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        call this%expanded%emit(index%row)
    end subroutine treeview_expand

    subroutine treeview_collapse(this, index)
        class(QTreeView), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        call this%collapsed%emit(index%row)
    end subroutine treeview_collapse

    subroutine treeview_expand_all(this)
        class(QTreeView), intent(inout) :: this
        ! Expand all items
    end subroutine treeview_expand_all

    subroutine treeview_collapse_all(this)
        class(QTreeView), intent(inout) :: this
        ! Collapse all items
    end subroutine treeview_collapse_all

    subroutine treeview_resize_column(this, column, width)
        class(QTreeView), intent(inout) :: this
        integer, intent(in) :: column
        integer, intent(in), optional :: width
        ! Resize column to contents or specified width
    end subroutine treeview_resize_column

    ! ========== QTreeWidget Implementation ==========

    subroutine treewidget_set_header_labels(this, labels)
        class(QTreeWidget), intent(inout) :: this
        character(len=*), intent(in) :: labels(:)
        integer :: i

        call this%headers%clear()
        do i = 1, size(labels)
            call this%headers%append(labels(i))
        end do
    end subroutine treewidget_set_header_labels

    subroutine treewidget_set_header_label(this, column, label)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: column
        character(len=*), intent(in) :: label

        if (column >= 0 .and. column < this%headers%size()) then
            call this%headers%set(column, label)
        end if
    end subroutine treewidget_set_header_label

    function treewidget_header_item(this) result(item)
        class(QTreeWidget), intent(in) :: this
        type(tree_item) :: item
        ! Return header item
    end function treewidget_header_item

    function treewidget_top_level_item(this, index) result(item)
        class(QTreeWidget), intent(in) :: this
        integer, intent(in) :: index
        type(tree_item) :: item
        ! Return top-level item at index
    end function treewidget_top_level_item

    subroutine treewidget_add_top_level_item(this, item)
        class(QTreeWidget), intent(inout) :: this
        type(tree_item), intent(in) :: item
        ! Add item as top-level
    end subroutine treewidget_add_top_level_item

    function treewidget_take_top_level_item(this, index) result(item)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: index
        type(tree_item) :: item
        ! Remove and return top-level item
    end function treewidget_take_top_level_item

    function treewidget_top_level_item_count(this) result(count)
        class(QTreeWidget), intent(in) :: this
        integer :: count
        count = 0  ! Would count children of root
    end function treewidget_top_level_item_count

    ! ========== Tree Item Implementation ==========

    function treeitem_add_child(this, child) result(index)
        class(tree_item), intent(inout) :: this
        type(tree_item), intent(in) :: child
        integer :: index

        if (.not. associated(this%children)) then
            allocate(this%children(10))
        else if (this%child_count >= size(this%children)) then
            ! Expand array (simplified)
        end if

        this%child_count = this%child_count + 1
        this%children(this%child_count) = child
        index = this%child_count
    end function treeitem_add_child

    subroutine treeitem_remove_child(this, index)
        class(tree_item), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i

        if (index > 0 .and. index <= this%child_count) then
            do i = index, this%child_count - 1
                this%children(i) = this%children(i+1)
            end do
            this%child_count = this%child_count - 1
        end if
    end subroutine treeitem_remove_child

    subroutine treeitem_set_text(this, column, text)
        class(tree_item), intent(inout) :: this
        integer, intent(in) :: column
        character(len=*), intent(in) :: text

        if (.not. allocated(this%data)) then
            allocate(this%data)
        end if

        call this%data%set(column, text)
    end subroutine treeitem_set_text

    function treeitem_get_text(this, column) result(text)
        class(tree_item), intent(in) :: this
        integer, intent(in) :: column
        character(len=:), allocatable :: text

        if (allocated(this%data)) then
            text = this%data%at(column)
        else
            allocate(character(len=0) :: text)
        end if
    end function treeitem_get_text

    subroutine treeitem_set_data(this, column, data)
        class(tree_item), intent(inout) :: this
        integer, intent(in) :: column
        character(len=*), intent(in) :: data

        if (.not. allocated(this%data)) then
            allocate(this%data)
        end if

        call this%data%set(column, data)
    end subroutine treeitem_set_data

    function treeitem_get_data(this, column) result(data)
        class(tree_item), intent(in) :: this
        integer, intent(in) :: column
        character(len=:), allocatable :: data

        if (allocated(this%data)) then
            data = this%data%at(column)
        else
            allocate(character(len=0) :: data)
        end if
    end function treeitem_get_data

    function treeitem_child(this, index) result(child)
        class(tree_item), intent(in) :: this
        integer, intent(in) :: index
        type(tree_item) :: child

        if (associated(this%children) .and. index > 0 .and. index <= this%child_count) then
            child = this%children(index)
        end if
    end function treeitem_child

    function treeitem_child_count(this) result(count)
        class(tree_item), intent(in) :: this
        integer :: count
        count = this%child_count
    end function treeitem_child_count

    function treeitem_parent(this) result(parent)
        class(tree_item), intent(in) :: this
        type(tree_item) :: parent

        if (associated(this%parent)) then
            parent = this%parent
        end if
    end function treeitem_parent

    function treeitem_row(this) result(row)
        class(tree_item), intent(in) :: this
        integer :: row
        type(tree_item) :: parent_item

        row = 0
        if (associated(this%parent)) then
            parent_item = this%parent
            ! Find our index in parent's children
            do row = 1, parent_item%child_count
                if (parent_item%child(row)%data%size() == this%data%size()) then
                    ! Simple comparison - would need proper equality
                    exit
                end if
            end do
        end if
    end function treeitem_row

end module forge_listview
