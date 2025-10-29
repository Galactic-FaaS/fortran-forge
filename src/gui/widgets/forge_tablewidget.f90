!> @brief QTableWidget implementation
!> @details Spreadsheet-like table with rows/columns, editing, sorting, and selection
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_tablewidget
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QTableWidget, QTableWidgetItem

    !> @brief Table widget item
    type :: QTableWidgetItem
        private
        type(forge_string) :: text
        type(forge_string) :: tool_tip
        type(forge_string) :: status_tip
        type(forge_string) :: whats_this
        integer :: type = 0  ! 0=Type, 1=UserType
        logical :: selected = .false.
        logical :: editable = .true.
        integer :: text_alignment = 0  ! Qt::Alignment
        integer :: flags = 63  ! Qt::ItemIsSelectable | Qt::ItemIsEditable | etc.
        class(*), allocatable :: data
    contains
        procedure :: set_text => table_item_set_text
        procedure :: text => table_item_text
        procedure :: set_tool_tip => table_item_set_tool_tip
        procedure :: tool_tip => table_item_tool_tip
        procedure :: set_status_tip => table_item_set_status_tip
        procedure :: status_tip => table_item_status_tip
        procedure :: set_whats_this => table_item_set_whats_this
        procedure :: whats_this => table_item_whats_this
        procedure :: set_selected => table_item_set_selected
        procedure :: is_selected => table_item_is_selected
        procedure :: set_editable => table_item_set_editable
        procedure :: is_editable => table_item_is_editable
        procedure :: set_text_alignment => table_item_set_text_alignment
        procedure :: text_alignment => table_item_text_alignment
        procedure :: set_flags => table_item_set_flags
        procedure :: flags => table_item_flags
        procedure :: set_data => table_item_set_data
        procedure :: data => table_item_data
        procedure :: set_background => table_item_set_background
        procedure :: background => table_item_background
        procedure :: set_foreground => table_item_set_foreground
        procedure :: foreground => table_item_foreground
        procedure :: set_icon => table_item_set_icon
        procedure :: icon => table_item_icon
        procedure :: set_size_hint => table_item_set_size_hint
        procedure :: size_hint => table_item_size_hint
        procedure :: set_font => table_item_set_font
        procedure :: font => table_item_font
        procedure :: set_check_state => table_item_set_check_state
        procedure :: check_state => table_item_check_state
        procedure :: type => table_item_type
        procedure :: clone => table_item_clone
        final :: table_item_finalize
    end type QTableWidgetItem

    !> @brief Table widget with spreadsheet functionality
    type, extends(forge_widget) :: QTableWidget
        private
        type(QTableWidgetItem), dimension(:,:), allocatable :: items
        type(forge_string), dimension(:), allocatable :: horizontal_header_labels
        type(forge_string), dimension(:), allocatable :: vertical_header_labels
        integer :: row_count = 0
        integer :: column_count = 0
        logical :: show_grid = .true.
        logical :: sorting_enabled = .false.
        integer :: sort_column = -1
        logical :: sort_order_ascending = .true.
        integer :: selection_behavior = 1  ! 0=SelectItems, 1=SelectRows, 2=SelectColumns
        integer :: selection_mode = 1       ! 0=NoSelection, 1=SingleSelection, 2=MultiSelection, 3=ExtendedSelection, 4=ContiguousSelection
        logical :: alternating_row_colors = .false.
        logical :: horizontal_header_visible = .true.
        logical :: vertical_header_visible = .true.
        type(signal_item) :: item_clicked
        type(signal_item) :: item_double_clicked
        type(signal_item) :: item_changed
        type(signal_item) :: item_entered
        type(signal_item) :: item_pressed
        type(signal_item) :: current_item_changed
        type(signal_cell) :: cell_clicked
        type(signal_cell) :: cell_double_clicked
        type(signal_cell) :: cell_changed
        type(signal_cell) :: cell_entered
        type(signal_cell) :: cell_pressed
    contains
        procedure :: set_row_count => table_set_row_count
        procedure :: row_count => table_row_count
        procedure :: set_column_count => table_set_column_count
        procedure :: column_count => table_column_count
        procedure :: set_item => table_set_item
        procedure :: item => table_item
        procedure :: take_item => table_take_item
        procedure :: set_horizontal_header_labels => table_set_horizontal_header_labels
        procedure :: set_vertical_header_labels => table_set_vertical_header_labels
        procedure :: horizontal_header_item => table_horizontal_header_item
        procedure :: vertical_header_item => table_vertical_header_item
        procedure :: set_cell_widget => table_set_cell_widget
        procedure :: cell_widget => table_cell_widget
        procedure :: remove_cell_widget => table_remove_cell_widget
        procedure :: is_item_selected => table_is_item_selected
        procedure :: set_item_selected => table_set_item_selected
        procedure :: selected_items => table_selected_items
        procedure :: selected_ranges => table_selected_ranges
        procedure :: clear => table_clear
        procedure :: clear_contents => table_clear_contents
        procedure :: insert_row => table_insert_row
        procedure :: insert_column => table_insert_column
        procedure :: remove_row => table_remove_row
        procedure :: remove_column => table_remove_column
        procedure :: scroll_to_item => table_scroll_to_item
        procedure :: scroll_to_cell => table_scroll_to_cell
        procedure :: set_show_grid => table_set_show_grid
        procedure :: show_grid => table_show_grid
        procedure :: set_grid_style => table_set_grid_style
        procedure :: grid_style => table_grid_style
        procedure :: set_selection_behavior => table_set_selection_behavior
        procedure :: selection_behavior => table_selection_behavior
        procedure :: set_selection_mode => table_set_selection_mode
        procedure :: selection_mode => table_selection_mode
        procedure :: set_current_item => table_set_current_item
        procedure :: current_item => table_current_item
        procedure :: set_current_cell => table_set_current_cell
        procedure :: current_row => table_current_row
        procedure :: current_column => table_current_column
        procedure :: sort_items => table_sort_items
        procedure :: set_sorting_enabled => table_set_sorting_enabled
        procedure :: is_sorting_enabled => table_is_sorting_enabled
        procedure :: sort_column => table_sort_column
        procedure :: set_sort_column => table_set_sort_column
        procedure :: sort_order => table_sort_order
        procedure :: set_sort_order => table_set_sort_order
        procedure :: edit_item => table_edit_item
        procedure :: open_persistent_editor => table_open_persistent_editor
        procedure :: close_persistent_editor => table_close_persistent_editor
        procedure :: is_persistent_editor_open => table_is_persistent_editor_open
        procedure :: item_at => table_item_at
        procedure :: visual_item_rect => table_visual_item_rect
        procedure :: find_items => table_find_items
        procedure :: set_row_height => table_set_row_height
        procedure :: row_height => table_row_height
        procedure :: set_column_width => table_set_column_width
        procedure :: column_width => table_column_width
        procedure :: resize_rows_to_contents => table_resize_rows_to_contents
        procedure :: resize_columns_to_contents => table_resize_columns_to_contents
        procedure :: set_alternating_row_colors => table_set_alternating_row_colors
        procedure :: alternating_row_colors => table_alternating_row_colors
        procedure :: set_horizontal_header_visible => table_set_horizontal_header_visible
        procedure :: horizontal_header_visible => table_horizontal_header_visible
        procedure :: set_vertical_header_visible => table_set_vertical_header_visible
        procedure :: vertical_header_visible => table_vertical_header_visible
        final :: table_finalize
    end type QTableWidget

    !> @brief Cell signal type (row, column)
    type :: signal_cell
        private
        integer :: connection_count = 0
        type(slot_cell_proc), dimension(:), allocatable :: slots
        type(signal_connection) :: connections(100)
        logical :: blocked = .false.
    contains
        procedure :: connect => signal_cell_connect
        procedure :: disconnect => signal_cell_disconnect
        procedure :: emit => signal_cell_emit
        procedure :: is_connected => signal_cell_is_connected
    end type signal_cell

    !> @brief Cell slot procedure pointer
    type :: slot_cell_proc
        procedure(slot_cell), pointer, nopass :: proc => null()
    end type slot_cell_proc

contains

    ! ========== QTableWidgetItem Implementation ==========

    subroutine table_item_set_text(this, text)
        class(QTableWidgetItem), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine table_item_set_text

    function table_item_text(this) result(text)
        class(QTableWidgetItem), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function table_item_text

    subroutine table_item_set_tool_tip(this, tool_tip)
        class(QTableWidgetItem), intent(inout) :: this
        character(len=*), intent(in) :: tool_tip
        call this%tool_tip%set(tool_tip)
    end subroutine table_item_set_tool_tip

    function table_item_tool_tip(this) result(tool_tip)
        class(QTableWidgetItem), intent(in) :: this
        character(len=:), allocatable :: tool_tip
        tool_tip = this%tool_tip%get()
    end function table_item_tool_tip

    subroutine table_item_set_status_tip(this, status_tip)
        class(QTableWidgetItem), intent(inout) :: this
        character(len=*), intent(in) :: status_tip
        call this%status_tip%set(status_tip)
    end subroutine table_item_set_status_tip

    function table_item_status_tip(this) result(status_tip)
        class(QTableWidgetItem), intent(in) :: this
        character(len=:), allocatable :: status_tip
        status_tip = this%status_tip%get()
    end function table_item_status_tip

    subroutine table_item_set_whats_this(this, whats_this)
        class(QTableWidgetItem), intent(inout) :: this
        character(len=*), intent(in) :: whats_this
        call this%whats_this%set(whats_this)
    end subroutine table_item_set_whats_this

    function table_item_whats_this(this) result(whats_this)
        class(QTableWidgetItem), intent(in) :: this
        character(len=:), allocatable :: whats_this
        whats_this = this%whats_this%get()
    end function table_item_whats_this

    subroutine table_item_set_selected(this, selected)
        class(QTableWidgetItem), intent(inout) :: this
        logical, intent(in) :: selected
        this%selected = selected
    end subroutine table_item_set_selected

    function table_item_is_selected(this) result(selected)
        class(QTableWidgetItem), intent(in) :: this
        logical :: selected
        selected = this%selected
    end function table_item_is_selected

    subroutine table_item_set_editable(this, editable)
        class(QTableWidgetItem), intent(inout) :: this
        logical, intent(in) :: editable
        this%editable = editable
    end subroutine table_item_set_editable

    function table_item_is_editable(this) result(editable)
        class(QTableWidgetItem), intent(in) :: this
        logical :: editable
        editable = this%editable
    end function table_item_is_editable

    subroutine table_item_set_text_alignment(this, alignment)
        class(QTableWidgetItem), intent(inout) :: this
        integer, intent(in) :: alignment
        this%text_alignment = alignment
    end subroutine table_item_set_text_alignment

    function table_item_text_alignment(this) result(alignment)
        class(QTableWidgetItem), intent(in) :: this
        integer :: alignment
        alignment = this%text_alignment
    end function table_item_text_alignment

    subroutine table_item_set_flags(this, flags)
        class(QTableWidgetItem), intent(inout) :: this
        integer, intent(in) :: flags
        this%flags = flags
    end subroutine table_item_set_flags

    function table_item_flags(this) result(flags)
        class(QTableWidgetItem), intent(in) :: this
        integer :: flags
        flags = this%flags
    end function table_item_flags

    subroutine table_item_set_data(this, role, value)
        class(QTableWidgetItem), intent(inout) :: this
        integer, intent(in) :: role
        class(*), intent(in) :: value
        if (allocated(this%data)) deallocate(this%data)
        allocate(this%data, source=value)
    end subroutine table_item_set_data

    function table_item_data(this, role) result(value)
        class(QTableWidgetItem), intent(in) :: this
        integer, intent(in) :: role
        class(*), allocatable :: value
        if (allocated(this%data)) then
            allocate(value, source=this%data)
        end if
    end function table_item_data

    subroutine table_item_set_background(this, brush)
        class(QTableWidgetItem), intent(inout) :: this
        class(*), intent(in) :: brush
        ! Would set background brush
    end subroutine table_item_set_background

    function table_item_background(this) result(brush)
        class(QTableWidgetItem), intent(in) :: this
        class(*), allocatable :: brush
        ! Would return background brush
    end function table_item_background

    subroutine table_item_set_foreground(this, brush)
        class(QTableWidgetItem), intent(inout) :: this
        class(*), intent(in) :: brush
        ! Would set foreground brush
    end subroutine table_item_set_foreground

    function table_item_foreground(this) result(brush)
        class(QTableWidgetItem), intent(in) :: this
        class(*), allocatable :: brush
        ! Would return foreground brush
    end function table_item_foreground

    subroutine table_item_set_icon(this, icon)
        class(QTableWidgetItem), intent(inout) :: this
        class(*), intent(in) :: icon
        ! Would set icon
    end subroutine table_item_set_icon

    function table_item_icon(this) result(icon)
        class(QTableWidgetItem), intent(in) :: this
        class(*), allocatable :: icon
        ! Would return icon
    end function table_item_icon

    subroutine table_item_set_size_hint(this, size)
        class(QTableWidgetItem), intent(inout) :: this
        type(forge_size), intent(in) :: size
        ! Would set size hint
    end subroutine table_item_set_size_hint

    function table_item_size_hint(this) result(size)
        class(QTableWidgetItem), intent(in) :: this
        type(forge_size) :: size
        ! Would return size hint
    end function table_item_size_hint

    subroutine table_item_set_font(this, font)
        class(QTableWidgetItem), intent(inout) :: this
        class(*), intent(in) :: font
        ! Would set font
    end subroutine table_item_set_font

    function table_item_font(this) result(font)
        class(QTableWidgetItem), intent(in) :: this
        class(*), allocatable :: font
        ! Would return font
    end function table_item_font

    subroutine table_item_set_check_state(this, state)
        class(QTableWidgetItem), intent(inout) :: this
        integer, intent(in) :: state
        ! Would set check state
    end subroutine table_item_set_check_state

    function table_item_check_state(this) result(state)
        class(QTableWidgetItem), intent(in) :: this
        integer :: state
        state = 0  ! Unchecked
    end function table_item_check_state

    function table_item_type(this) result(type_val)
        class(QTableWidgetItem), intent(in) :: this
        integer :: type_val
        type_val = this%type
    end function table_item_type

    function table_item_clone(this) result(clone)
        class(QTableWidgetItem), intent(in) :: this
        type(QTableWidgetItem) :: clone
        clone%text = this%text
        clone%tool_tip = this%tool_tip
        clone%status_tip = this%status_tip
        clone%whats_this = this%whats_this
        clone%type = this%type
        clone%selected = this%selected
        clone%editable = this%editable
        clone%text_alignment = this%text_alignment
        clone%flags = this%flags
        if (allocated(this%data)) then
            allocate(clone%data, source=this%data)
        end if
    end function table_item_clone

    subroutine table_item_finalize(this)
        type(QTableWidgetItem), intent(inout) :: this
        if (allocated(this%data)) deallocate(this%data)
    end subroutine table_item_finalize

    ! ========== QTableWidget Implementation ==========

    subroutine table_set_row_count(this, rows)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: rows

        if (rows < 0) return

        if (allocated(this%items)) then
            deallocate(this%items)
        end if

        this%row_count = rows
        if (this%column_count > 0 .and. rows > 0) then
            allocate(this%items(rows, this%column_count))
        end if
    end subroutine table_set_row_count

    function table_row_count(this) result(rows)
        class(QTableWidget), intent(in) :: this
        integer :: rows
        rows = this%row_count
    end function table_row_count

    subroutine table_set_column_count(this, columns)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: columns

        if (columns < 0) return

        if (allocated(this%items)) then
            deallocate(this%items)
        end if

        this%column_count = columns
        if (this%row_count > 0 .and. columns > 0) then
            allocate(this%items(this%row_count, columns))
        end if
    end subroutine table_set_column_count

    function table_column_count(this) result(columns)
        class(QTableWidget), intent(in) :: this
        integer :: columns
        columns = this%column_count
    end function table_column_count

    subroutine table_set_item(this, row, column, item)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        type(QTableWidgetItem), intent(in) :: item

        if (row < 0 .or. row >= this%row_count .or. &
            column < 0 .or. column >= this%column_count) return

        this%items(row + 1, column + 1) = item
    end subroutine table_set_item

    function table_item(this, row, column) result(item)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: row, column
        type(QTableWidgetItem), pointer :: item

        item => null()
        if (row >= 0 .and. row < this%row_count .and. &
            column >= 0 .and. column < this%column_count .and. &
            allocated(this%items)) then
            item => this%items(row + 1, column + 1)
        end if
    end function table_item

    function table_take_item(this, row, column) result(item)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        type(QTableWidgetItem), pointer :: item

        item => this%item(row, column)
        if (associated(item)) then
            ! Would need to nullify the item in the array
        end if
    end function table_take_item

    subroutine table_set_horizontal_header_labels(this, labels)
        class(QTableWidget), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: labels

        if (allocated(this%horizontal_header_labels)) deallocate(this%horizontal_header_labels)
        allocate(this%horizontal_header_labels(size(labels)))

        block
            integer :: i
            do i = 1, size(labels)
                call this%horizontal_header_labels(i)%set(labels(i))
            end do
        end block
    end subroutine table_set_horizontal_header_labels

    subroutine table_set_vertical_header_labels(this, labels)
        class(QTableWidget), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: labels

        if (allocated(this%vertical_header_labels)) deallocate(this%vertical_header_labels)
        allocate(this%vertical_header_labels(size(labels)))

        block
            integer :: i
            do i = 1, size(labels)
                call this%vertical_header_labels(i)%set(labels(i))
            end do
        end block
    end subroutine table_set_vertical_header_labels

    function table_horizontal_header_item(this, column) result(item)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: column
        type(QTableWidgetItem), pointer :: item
        item => null()
        ! Would return header item
    end function table_horizontal_header_item

    function table_vertical_header_item(this, row) result(item)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: row
        type(QTableWidgetItem), pointer :: item
        item => null()
        ! Would return header item
    end function table_vertical_header_item

    subroutine table_set_cell_widget(this, row, column, widget)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        class(forge_widget), pointer, intent(in) :: widget
        ! Would set widget in cell
    end subroutine table_set_cell_widget

    function table_cell_widget(this, row, column) result(widget)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: row, column
        class(forge_widget), pointer :: widget
        widget => null()
        ! Would return widget in cell
    end function table_cell_widget

    subroutine table_remove_cell_widget(this, row, column)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        ! Would remove widget from cell
    end subroutine table_remove_cell_widget

    function table_is_item_selected(this, item) result(selected)
        class(QTableWidget), intent(in) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        logical :: selected
        selected = .false.
        if (associated(item)) selected = item%is_selected()
    end function table_is_item_selected

    subroutine table_set_item_selected(this, item, selected)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        logical, intent(in) :: selected
        if (associated(item)) call item%set_selected(selected)
    end subroutine table_set_item_selected

    function table_selected_items(this) result(items)
        class(QTableWidget), intent(in) :: this
        type(QTableWidgetItem), dimension(:), pointer :: items
        items => null()
        ! Would collect selected items
    end function table_selected_items

    function table_selected_ranges(this) result(ranges)
        class(QTableWidget), intent(in) :: this
        class(*), dimension(:), pointer :: ranges
        ranges => null()
        ! Would return selected ranges
    end function table_selected_ranges

    subroutine table_clear(this)
        class(QTableWidget), intent(inout) :: this
        if (allocated(this%items)) deallocate(this%items)
        if (allocated(this%horizontal_header_labels)) deallocate(this%horizontal_header_labels)
        if (allocated(this%vertical_header_labels)) deallocate(this%vertical_header_labels)
        this%row_count = 0
        this%column_count = 0
    end subroutine table_clear

    subroutine table_clear_contents(this)
        class(QTableWidget), intent(inout) :: this
        if (allocated(this%items)) deallocate(this%items)
        if (this%row_count > 0 .and. this%column_count > 0) then
            allocate(this%items(this%row_count, this%column_count))
        end if
    end subroutine table_clear_contents

    subroutine table_insert_row(this, row)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row
        ! Would insert row at specified position
    end subroutine table_insert_row

    subroutine table_insert_column(this, column)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: column
        ! Would insert column at specified position
    end subroutine table_insert_column

    subroutine table_remove_row(this, row)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row
        ! Would remove row at specified position
    end subroutine table_remove_row

    subroutine table_remove_column(this, column)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: column
        ! Would remove column at specified position
    end subroutine table_remove_column

    subroutine table_scroll_to_item(this, item, hint)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        integer, intent(in), optional :: hint
        ! Would scroll to make item visible
    end subroutine table_scroll_to_item

    subroutine table_scroll_to_cell(this, row, column, hint)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        integer, intent(in), optional :: hint
        ! Would scroll to make cell visible
    end subroutine table_scroll_to_cell

    subroutine table_set_show_grid(this, show)
        class(QTableWidget), intent(inout) :: this
        logical, intent(in) :: show
        this%show_grid = show
    end subroutine table_set_show_grid

    function table_show_grid(this) result(show)
        class(QTableWidget), intent(in) :: this
        logical :: show
        show = this%show_grid
    end function table_show_grid

    subroutine table_set_grid_style(this, style)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: style
        ! Would set grid style
    end subroutine table_set_grid_style

    function table_grid_style(this) result(style)
        class(QTableWidget), intent(in) :: this
        integer :: style
        style = 0  ! SolidLine
    end function table_grid_style

    subroutine table_set_selection_behavior(this, behavior)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: behavior
        this%selection_behavior = behavior
    end subroutine table_set_selection_behavior

    function table_selection_behavior(this) result(behavior)
        class(QTableWidget), intent(in) :: this
        integer :: behavior
        behavior = this%selection_behavior
    end function table_selection_behavior

    subroutine table_set_selection_mode(this, mode)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: mode
        this%selection_mode = mode
    end subroutine table_set_selection_mode

    function table_selection_mode(this) result(mode)
        class(QTableWidget), intent(in) :: this
        integer :: mode
        mode = this%selection_mode
    end function table_selection_mode

    subroutine table_set_current_item(this, item)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        ! Would set current item
    end subroutine table_set_current_item

    function table_current_item(this) result(item)
        class(QTableWidget), intent(in) :: this
        type(QTableWidgetItem), pointer :: item
        item => null()
        ! Would return current item
    end function table_current_item

    subroutine table_set_current_cell(this, row, column)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, column
        ! Would set current cell
    end subroutine table_set_current_cell

    function table_current_row(this) result(row)
        class(QTableWidget), intent(in) :: this
        integer :: row
        row = -1
        ! Would return current row
    end function table_current_row

    function table_current_column(this) result(column)
        class(QTableWidget), intent(in) :: this
        integer :: column
        column = -1
        ! Would return current column
    end function table_current_column

    subroutine table_sort_items(this, column, order)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: column, order
        this%sort_column = column
        this%sort_order_ascending = (order == 0)
        ! Would implement sorting
    end subroutine table_sort_items

    subroutine table_set_sorting_enabled(this, enabled)
        class(QTableWidget), intent(inout) :: this
        logical, intent(in) :: enabled
        this%sorting_enabled = enabled
    end subroutine table_set_sorting_enabled

    function table_is_sorting_enabled(this) result(enabled)
        class(QTableWidget), intent(in) :: this
        logical :: enabled
        enabled = this%sorting_enabled
    end function table_is_sorting_enabled

    function table_sort_column(this) result(column)
        class(QTableWidget), intent(in) :: this
        integer :: column
        column = this%sort_column
    end function table_sort_column

    subroutine table_set_sort_column(this, column)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: column
        this%sort_column = column
    end subroutine table_set_sort_column

    function table_sort_order(this) result(order)
        class(QTableWidget), intent(in) :: this
        integer :: order
        if (this%sort_order_ascending) then
            order = 0  ! Ascending
        else
            order = 1  ! Descending
        end if
    end function table_sort_order

    subroutine table_set_sort_order(this, order)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: order
        this%sort_order_ascending = (order == 0)
    end subroutine table_set_sort_order

    subroutine table_edit_item(this, item)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        ! Would start editing the item
    end subroutine table_edit_item

    subroutine table_open_persistent_editor(this, item)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        ! Would open persistent editor for item
    end subroutine table_open_persistent_editor

    subroutine table_close_persistent_editor(this, item)
        class(QTableWidget), intent(inout) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        ! Would close persistent editor for item
    end subroutine table_close_persistent_editor

    function table_is_persistent_editor_open(this, item) result(open)
        class(QTableWidget), intent(in) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        logical :: open
        open = .false.
        ! Would check if persistent editor is open
    end function table_is_persistent_editor_open

    function table_item_at(this, point) result(item)
        class(QTableWidget), intent(in) :: this
        type(forge_position), intent(in) :: point
        type(QTableWidgetItem), pointer :: item
        item => null()
        ! Would return item at position
    end function table_item_at

    function table_visual_item_rect(this, item) result(rect)
        class(QTableWidget), intent(in) :: this
        type(QTableWidgetItem), pointer, intent(in) :: item
        type(forge_rect) :: rect
        ! Would return visual rectangle for item
    end function table_visual_item_rect

    function table_find_items(this, text, flags) result(items)
        class(QTableWidget), intent(in) :: this
        character(len=*), intent(in) :: text
        integer, intent(in) :: flags
        type(QTableWidgetItem), dimension(:), pointer :: items
        items => null()
        ! Would find items matching text
    end function table_find_items

    subroutine table_set_row_height(this, row, height)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: row, height
        ! Would set row height
    end subroutine table_set_row_height

    function table_row_height(this, row) result(height)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: row
        integer :: height
        height = 30  ! Default height
    end function table_row_height

    subroutine table_set_column_width(this, column, width)
        class(QTableWidget), intent(inout) :: this
        integer, intent(in) :: column, width
        ! Would set column width
    end subroutine table_set_column_width

    function table_column_width(this, column) result(width)
        class(QTableWidget), intent(in) :: this
        integer, intent(in) :: column
        integer :: width
        width = 100  ! Default width
    end function table_column_width

    subroutine table_resize_rows_to_contents(this)
        class(QTableWidget), intent(inout) :: this
        ! Would resize rows to contents
    end subroutine table_resize_rows_to_contents

    subroutine table_resize_columns_to_contents(this)
        class(QTableWidget), intent(inout) :: this
        ! Would resize columns to contents
    end subroutine table_resize_columns_to_contents

    subroutine table_set_alternating_row_colors(this, enable)
        class(QTableWidget), intent(inout) :: this
        logical, intent(in) :: enable
        this%alternating_row_colors = enable
    end subroutine table_set_alternating_row_colors

    function table_alternating_row_colors(this) result(enable)
        class(QTableWidget), intent(in) :: this
        logical :: enable
        enable = this%alternating_row_colors
    end function table_alternating_row_colors

    subroutine table_set_horizontal_header_visible(this, visible)
        class(QTableWidget), intent(inout) :: this
        logical, intent(in) :: visible
        this%horizontal_header_visible = visible
    end subroutine table_set_horizontal_header_visible

    function table_horizontal_header_visible(this) result(visible)
        class(QTableWidget), intent(in) :: this
        logical :: visible
        visible = this%horizontal_header_visible
    end function table_horizontal_header_visible

    subroutine table_set_vertical_header_visible(this, visible)
        class(QTableWidget), intent(inout) :: this
        logical, intent(in) :: visible
        this%vertical_header_visible = visible
    end subroutine table_set_vertical_header_visible

    function table_vertical_header_visible(this) result(visible)
        class(QTableWidget), intent(in) :: this
        logical :: visible
        visible = this%vertical_header_visible
    end function table_vertical_header_visible

    subroutine table_finalize(this)
        type(QTableWidget), intent(inout) :: this
        call this%clear()
    end subroutine table_finalize

    ! ========== signal_cell Implementation ==========

    subroutine signal_cell_connect(this, slot)
        class(signal_cell), intent(inout) :: this
        procedure(slot_cell) :: slot

        if (this%connection_count < size(this%slots)) then
            this%connection_count = this%connection_count + 1
            allocate(this%slots(this%connection_count)%proc, source=slot)
        end if
    end subroutine signal_cell_connect

    subroutine signal_cell_disconnect(this, slot)
        class(signal_cell), intent(inout) :: this
        procedure(slot_cell) :: slot
        integer :: i

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc, slot)) then
                this%slots(i)%proc => null()
                exit
            end if
        end do
    end subroutine signal_cell_disconnect

    subroutine signal_cell_emit(this, row, column)
        class(signal_cell), intent(inout) :: this
        integer, intent(in) :: row, column
        integer :: i

        if (this%blocked) return

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc)) then
                call this%slots(i)%proc(row, column)
            end if
        end do
    end subroutine signal_cell_emit

    function signal_cell_is_connected(this) result(connected)
        class(signal_cell), intent(in) :: this
        logical :: connected
        connected = this%connection_count > 0
    end function signal_cell_is_connected

end module forge_tablewidget