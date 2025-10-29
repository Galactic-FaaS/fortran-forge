!> @brief QTreeWidget implementation
!> @details Hierarchical tree view with expandable/collapsible items, columns, and editing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_treewidget
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QTreeWidget, QTreeWidgetItem

    !> @brief Tree widget item
    type :: QTreeWidgetItem
        private
        type(forge_string) :: text
        type(forge_string), allocatable :: column_texts(:)
        logical :: expanded = .false.
        logical :: selected = .false.
        logical :: checkable = .false.
        integer :: check_state = 0  ! 0=Unchecked, 1=Partially, 2=Checked
        type(QTreeWidgetItem), pointer :: parent => null()
        type(QTreeWidgetItem), pointer :: first_child => null()
        type(QTreeWidgetItem), pointer :: next_sibling => null()
        integer :: child_count = 0
        integer :: row = -1
        integer :: column_count = 1
    contains
        procedure :: set_text => tree_item_set_text
        procedure :: text => tree_item_text
        procedure :: set_data => tree_item_set_data
        procedure :: data => tree_item_data
        procedure :: set_expanded => tree_item_set_expanded
        procedure :: is_expanded => tree_item_is_expanded
        procedure :: set_selected => tree_item_set_selected
        procedure :: is_selected => tree_item_is_selected
        procedure :: set_check_state => tree_item_set_check_state
        procedure :: check_state => tree_item_check_state
        procedure :: add_child => tree_item_add_child
        procedure :: insert_child => tree_item_insert_child
        procedure :: remove_child => tree_item_remove_child
        procedure :: child => tree_item_child
        procedure :: child_count => tree_item_child_count
        procedure :: parent => tree_item_parent
        procedure :: row => tree_item_row
        procedure :: column_count => tree_item_column_count
        procedure :: set_column_count => tree_item_set_column_count
        final :: tree_item_finalize
    end type QTreeWidgetItem

    !> @brief Tree widget with hierarchical data display
    type, extends(forge_widget) :: QTreeWidget
        private
        type(QTreeWidgetItem), pointer :: root_item => null()
        type(forge_string), allocatable :: header_labels(:)
        integer :: column_count = 1
        logical :: root_is_decorated = .true.
        logical :: items_expandable = .true.
        logical :: sorting_enabled = .false.
        integer :: sort_column = 0
        logical :: sort_order_ascending = .true.
        type(signal_item) :: item_clicked
        type(signal_item) :: item_double_clicked
        type(signal_item) :: item_changed
        type(signal_item) :: item_expanded
        type(signal_item) :: item_collapsed
        type(signal_item) :: current_item_changed
    contains
        procedure :: set_header_labels => tree_set_header_labels
        procedure :: header_item => tree_header_item
        procedure :: set_column_count => tree_set_column_count
        procedure :: column_count => tree_column_count
        procedure :: set_root_is_decorated => tree_set_root_is_decorated
        procedure :: set_items_expandable => tree_set_items_expandable
        procedure :: add_top_level_item => tree_add_top_level_item
        procedure :: insert_top_level_item => tree_insert_top_level_item
        procedure :: take_top_level_item => tree_take_top_level_item
        procedure :: top_level_item => tree_top_level_item
        procedure :: top_level_item_count => tree_top_level_item_count
        procedure :: clear => tree_clear
        procedure :: current_item => tree_current_item
        procedure :: set_current_item => tree_set_current_item
        procedure :: selected_items => tree_selected_items
        procedure :: find_items => tree_find_items
        procedure :: sort_items => tree_sort_items
        procedure :: set_sorting_enabled => tree_set_sorting_enabled
        procedure :: is_sorting_enabled => tree_is_sorting_enabled
        procedure :: sort_column => tree_sort_column
        procedure :: set_sort_column => tree_set_sort_column
        procedure :: sort_order => tree_sort_order
        procedure :: set_sort_order => tree_set_sort_order
        procedure :: expand_item => tree_expand_item
        procedure :: collapse_item => tree_collapse_item
        procedure :: expand_all => tree_expand_all
        procedure :: collapse_all => tree_collapse_all
        procedure :: scroll_to_item => tree_scroll_to_item
        procedure :: item_at => tree_item_at
        procedure :: visual_item_rect => tree_visual_item_rect
        final :: tree_finalize
    end type QTreeWidget

    !> @brief Item signal type
    type :: signal_item
        private
        integer :: connection_count = 0
        type(slot_item_proc), dimension(:), allocatable :: slots
        type(signal_connection) :: connections(100)
        logical :: blocked = .false.
    contains
        procedure :: connect => signal_item_connect
        procedure :: disconnect => signal_item_disconnect
        procedure :: emit => signal_item_emit
        procedure :: is_connected => signal_item_is_connected
    end type signal_item

    !> @brief Item slot procedure pointer
    type :: slot_item_proc
        procedure(slot_item), pointer, nopass :: proc => null()
    end type slot_item_proc

contains

    ! ========== QTreeWidgetItem Implementation ==========

    subroutine tree_item_set_text(this, column, text)
        class(QTreeWidgetItem), intent(inout) :: this
        integer, intent(in) :: column
        character(len=*), intent(in) :: text

        if (column < 0) return
        if (column >= this%column_count) then
            call this%set_column_count(column + 1)
        end if

        if (.not. allocated(this%column_texts)) then
            allocate(this%column_texts(this%column_count))
        end if

        call this%column_texts(column + 1)%set(text)
    end subroutine tree_item_set_text

    function tree_item_text(this, column) result(text)
        class(QTreeWidgetItem), intent(in) :: this
        integer, intent(in) :: column
        character(len=:), allocatable :: text

        if (column >= 0 .and. column < this%column_count .and. allocated(this%column_texts)) then
            text = this%column_texts(column + 1)%get()
        else
            text = ""
        end if
    end function tree_item_text

    subroutine tree_item_set_data(this, column, role, value)
        class(QTreeWidgetItem), intent(inout) :: this
        integer, intent(in) :: column, role
        class(*), intent(in) :: value
        ! Data storage would be implemented with a more complex data structure
        ! For now, just store text data
        select type (value)
        type is (character(len=*))
            call this%set_text(column, value)
        end select
    end subroutine tree_item_set_data

    function tree_item_data(this, column, role) result(value)
        class(QTreeWidgetItem), intent(in) :: this
        integer, intent(in) :: column, role
        class(*), allocatable :: value

        if (column >= 0 .and. column < this%column_count) then
            allocate(character(len=len(this%text(column))) :: value)
            select type (value)
            type is (character(len=*))
                value = this%text(column)
            end select
        end if
    end function tree_item_data

    subroutine tree_item_set_expanded(this, expanded)
        class(QTreeWidgetItem), intent(inout) :: this
        logical, intent(in) :: expanded
        this%expanded = expanded
    end subroutine tree_item_set_expanded

    function tree_item_is_expanded(this) result(expanded)
        class(QTreeWidgetItem), intent(in) :: this
        logical :: expanded
        expanded = this%expanded
    end function tree_item_is_expanded

    subroutine tree_item_set_selected(this, selected)
        class(QTreeWidgetItem), intent(inout) :: this
        logical, intent(in) :: selected
        this%selected = selected
    end subroutine tree_item_set_selected

    function tree_item_is_selected(this) result(selected)
        class(QTreeWidgetItem), intent(in) :: this
        logical :: selected
        selected = this%selected
    end function tree_item_is_selected

    subroutine tree_item_set_check_state(this, column, state)
        class(QTreeWidgetItem), intent(inout) :: this
        integer, intent(in) :: column, state
        this%check_state = state
        this%checkable = .true.
    end subroutine tree_item_set_check_state

    function tree_item_check_state(this, column) result(state)
        class(QTreeWidgetItem), intent(in) :: this
        integer, intent(in) :: column
        integer :: state
        state = this%check_state
    end function tree_item_check_state

    subroutine tree_item_add_child(this, child)
        class(QTreeWidgetItem), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: child

        if (.not. associated(child)) return

        child%parent => this
        child%row = this%child_count

        if (.not. associated(this%first_child)) then
            this%first_child => child
        else
            ! Find last sibling
            block
                type(QTreeWidgetItem), pointer :: last_child
                last_child => this%first_child
                do while (associated(last_child%next_sibling))
                    last_child => last_child%next_sibling
                end do
                last_child%next_sibling => child
            end block
        end if

        this%child_count = this%child_count + 1
    end subroutine tree_item_add_child

    subroutine tree_item_insert_child(this, index, child)
        class(QTreeWidgetItem), intent(inout) :: this
        integer, intent(in) :: index
        type(QTreeWidgetItem), pointer, intent(in) :: child

        if (.not. associated(child) .or. index < 0) return

        child%parent => this
        child%row = index

        if (index == 0) then
            child%next_sibling => this%first_child
            this%first_child => child
        else
            ! Find insertion point
            block
                type(QTreeWidgetItem), pointer :: prev_child
                integer :: i
                prev_child => this%first_child
                do i = 1, index - 1
                    if (.not. associated(prev_child)) exit
                    prev_child => prev_child%next_sibling
                end do
                if (associated(prev_child)) then
                    child%next_sibling => prev_child%next_sibling
                    prev_child%next_sibling => child
                end if
            end block
        end if

        this%child_count = this%child_count + 1
    end subroutine tree_item_insert_child

    subroutine tree_item_remove_child(this, child)
        class(QTreeWidgetItem), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: child

        if (.not. associated(child) .or. .not. associated(child%parent)) return
        if (child%parent .ne. this) return

        ! Remove from linked list
        if (associated(this%first_child, child)) then
            this%first_child => child%next_sibling
        else
            block
                type(QTreeWidgetItem), pointer :: prev_child
                prev_child => this%first_child
                do while (associated(prev_child%next_sibling))
                    if (associated(prev_child%next_sibling, child)) then
                        prev_child%next_sibling => child%next_sibling
                        exit
                    end if
                    prev_child => prev_child%next_sibling
                end do
            end block
        end if

        child%parent => null()
        child%next_sibling => null()
        this%child_count = this%child_count - 1
    end subroutine tree_item_remove_child

    function tree_item_child(this, index) result(child)
        class(QTreeWidgetItem), intent(in) :: this
        integer, intent(in) :: index
        type(QTreeWidgetItem), pointer :: child

        child => null()
        if (index < 0 .or. index >= this%child_count) return

        child => this%first_child
        block
            integer :: i
            do i = 1, index
                if (.not. associated(child)) exit
                child => child%next_sibling
            end do
        end block
    end function tree_item_child

    function tree_item_child_count(this) result(count)
        class(QTreeWidgetItem), intent(in) :: this
        integer :: count
        count = this%child_count
    end function tree_item_child_count

    function tree_item_parent(this) result(parent)
        class(QTreeWidgetItem), intent(in) :: this
        type(QTreeWidgetItem), pointer :: parent
        parent => this%parent
    end function tree_item_parent

    function tree_item_row(this) result(row)
        class(QTreeWidgetItem), intent(in) :: this
        integer :: row
        row = this%row
    end function tree_item_row

    function tree_item_column_count(this) result(count)
        class(QTreeWidgetItem), intent(in) :: this
        integer :: count
        count = this%column_count
    end function tree_item_column_count

    subroutine tree_item_set_column_count(this, columns)
        class(QTreeWidgetItem), intent(inout) :: this
        integer, intent(in) :: columns

        if (columns <= 0) return

        if (allocated(this%column_texts)) then
            deallocate(this%column_texts)
        end if

        this%column_count = columns
        allocate(this%column_texts(columns))
    end subroutine tree_item_set_column_count

    subroutine tree_item_finalize(this)
        type(QTreeWidgetItem), intent(inout) :: this
        if (allocated(this%column_texts)) deallocate(this%column_texts)
    end subroutine tree_item_finalize

    ! ========== QTreeWidget Implementation ==========

    subroutine tree_set_header_labels(this, labels)
        class(QTreeWidget), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: labels

        if (allocated(this%header_labels)) deallocate(this%header_labels)
        allocate(this%header_labels(size(labels)))

        block
            integer :: i
            do i = 1, size(labels)
                call this%header_labels(i)%set(labels(i))
            end do
        end block

        this%column_count = size(labels)
    end subroutine tree_set_header_labels

    function tree_header_item(this) result(item)
        class(QTreeWidget), intent(in) :: this
        type(QTreeWidgetItem), pointer :: item

        item => null()
        ! Header item would be a special item - for now return null
    end function tree_header_item

    subroutine tree_set_column_count(this, columns)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: columns
        this%column_count = max(1, columns)
    end subroutine tree_set_column_count

    function tree_column_count(this) result(count)
        class(QTreeWidget), intent(in) :: this
        integer :: count
        count = this%column_count
    end function tree_column_count

    subroutine tree_set_root_is_decorated(this, decorated)
        class(QTreeWidget), intent(inout) :: this
        logical, intent(in) :: decorated
        this%root_is_decorated = decorated
    end subroutine tree_set_root_is_decorated

    subroutine tree_set_items_expandable(this, expandable)
        class(QTreeWidget), intent(inout) :: this
        logical, intent(in) :: expandable
        this%items_expandable = expandable
    end subroutine tree_set_items_expandable

    subroutine tree_add_top_level_item(this, item)
        class(QTreeWidget), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item

        if (.not. associated(this%root_item)) then
            allocate(this%root_item)
        end if

        call this%root_item%add_child(item)
    end subroutine tree_add_top_level_item

    subroutine tree_insert_top_level_item(this, index, item)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: index
        type(QTreeWidgetItem), pointer, intent(in) :: item

        if (.not. associated(this%root_item)) then
            allocate(this%root_item)
        end if

        call this%root_item%insert_child(index, item)
    end subroutine tree_insert_top_level_item

    function tree_take_top_level_item(this, index) result(item)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: index
        type(QTreeWidgetItem), pointer :: item

        item => null()
        if (associated(this%root_item)) then
            item => this%root_item%child(index)
            if (associated(item)) then
                call this%root_item%remove_child(item)
            end if
        end if
    end function tree_take_top_level_item

    function tree_top_level_item(this, index) result(item)
        class(QTreeWidget), intent(in) :: this
        integer, intent(in) :: index
        type(QTreeWidgetItem), pointer :: item

        item => null()
        if (associated(this%root_item)) then
            item => this%root_item%child(index)
        end if
    end function tree_top_level_item

    function tree_top_level_item_count(this) result(count)
        class(QTreeWidget), intent(in) :: this
        integer :: count

        count = 0
        if (associated(this%root_item)) then
            count = this%root_item%child_count()
        end if
    end function tree_top_level_item_count

    subroutine tree_clear(this)
        class(QTreeWidget), intent(inout) :: this
        if (associated(this%root_item)) then
            deallocate(this%root_item)
            this%root_item => null()
        end if
        if (allocated(this%header_labels)) deallocate(this%header_labels)
    end subroutine tree_clear

    function tree_current_item(this) result(item)
        class(QTreeWidget), intent(in) :: this
        type(QTreeWidgetItem), pointer :: item
        item => null()
        ! Would need to track current item
    end function tree_current_item

    subroutine tree_set_current_item(this, item)
        class(QTreeWidget), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        ! Would set current item and emit signal
    end subroutine tree_set_current_item

    function tree_selected_items(this) result(items)
        class(QTreeWidget), intent(in) :: this
        type(QTreeWidgetItem), dimension(:), pointer :: items
        items => null()
        ! Would need to collect all selected items
    end function tree_selected_items

    function tree_find_items(this, text, flags, column) result(items)
        class(QTreeWidget), intent(in) :: this
        character(len=*), intent(in) :: text
        integer, intent(in) :: flags, column
        type(QTreeWidgetItem), dimension(:), pointer :: items
        items => null()
        ! Would implement search functionality
    end function tree_find_items

    subroutine tree_sort_items(this, column, order)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: column, order
        this%sort_column = column
        this%sort_order_ascending = (order == 0)  ! 0=Ascending, 1=Descending
        ! Would implement sorting
    end subroutine tree_sort_items

    subroutine tree_set_sorting_enabled(this, enabled)
        class(QTreeWidget), intent(inout) :: this
        logical, intent(in) :: enabled
        this%sorting_enabled = enabled
    end subroutine tree_set_sorting_enabled

    function tree_is_sorting_enabled(this) result(enabled)
        class(QTreeWidget), intent(in) :: this
        logical :: enabled
        enabled = this%sorting_enabled
    end function tree_is_sorting_enabled

    function tree_sort_column(this) result(column)
        class(QTreeWidget), intent(in) :: this
        integer :: column
        column = this%sort_column
    end function tree_sort_column

    subroutine tree_set_sort_column(this, column)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: column
        this%sort_column = column
    end subroutine tree_set_sort_column

    function tree_sort_order(this) result(order)
        class(QTreeWidget), intent(in) :: this
        integer :: order
        if (this%sort_order_ascending) then
            order = 0  ! Ascending
        else
            order = 1  ! Descending
        end if
    end function tree_sort_order

    subroutine tree_set_sort_order(this, order)
        class(QTreeWidget), intent(inout) :: this
        integer, intent(in) :: order
        this%sort_order_ascending = (order == 0)
    end subroutine tree_set_sort_order

    subroutine tree_expand_item(this, item)
        class(QTreeWidget), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        if (associated(item)) then
            call item%set_expanded(.true.)
            call this%item_expanded%emit(item)
        end if
    end subroutine tree_expand_item

    subroutine tree_collapse_item(this, item)
        class(QTreeWidget), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        if (associated(item)) then
            call item%set_expanded(.false.)
            call this%item_collapsed%emit(item)
        end if
    end subroutine tree_collapse_item

    subroutine tree_expand_all(this)
        class(QTreeWidget), intent(inout) :: this
        ! Would recursively expand all items
    end subroutine tree_expand_all

    subroutine tree_collapse_all(this)
        class(QTreeWidget), intent(inout) :: this
        ! Would recursively collapse all items
    end subroutine tree_collapse_all

    subroutine tree_scroll_to_item(this, item, hint)
        class(QTreeWidget), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        integer, intent(in), optional :: hint
        ! Would scroll to make item visible
    end subroutine tree_scroll_to_item

    function tree_item_at(this, x, y) result(item)
        class(QTreeWidget), intent(in) :: this
        integer, intent(in) :: x, y
        type(QTreeWidgetItem), pointer :: item
        item => null()
        ! Would calculate which item is at the given position
    end function tree_item_at

    function tree_visual_item_rect(this, item) result(rect)
        class(QTreeWidget), intent(in) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        type(forge_rect) :: rect
        ! Would calculate the visual rectangle for the item
    end function tree_visual_item_rect

    subroutine tree_finalize(this)
        type(QTreeWidget), intent(inout) :: this
        call this%clear()
    end subroutine tree_finalize

    ! ========== signal_item Implementation ==========

    subroutine signal_item_connect(this, slot)
        class(signal_item), intent(inout) :: this
        procedure(slot_item) :: slot

        if (this%connection_count < size(this%slots)) then
            this%connection_count = this%connection_count + 1
            allocate(this%slots(this%connection_count)%proc, source=slot)
        end if
    end subroutine signal_item_connect

    subroutine signal_item_disconnect(this, slot)
        class(signal_item), intent(inout) :: this
        procedure(slot_item) :: slot
        integer :: i

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc, slot)) then
                this%slots(i)%proc => null()
                exit
            end if
        end do
    end subroutine signal_item_disconnect

    subroutine signal_item_emit(this, item)
        class(signal_item), intent(inout) :: this
        type(QTreeWidgetItem), pointer, intent(in) :: item
        integer :: i

        if (this%blocked) return

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc)) then
                call this%slots(i)%proc(item)
            end if
        end do
    end subroutine signal_item_emit

    function signal_item_is_connected(this) result(connected)
        class(signal_item), intent(in) :: this
        logical :: connected
        connected = this%connection_count > 0
    end function signal_item_is_connected

end module forge_treewidget