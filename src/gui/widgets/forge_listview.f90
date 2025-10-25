!> @brief ListView widget (item list display)
!> @details List view with Model-View support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_listview
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QListView, QListWidget, ViewMode, ListMode, IconMode

    !> View mode constants
    integer, parameter :: ListMode = 0
    integer, parameter :: IconMode = 1

    type :: ViewMode
        integer :: value = ListMode
    end type ViewMode

    !> @brief List view (Model-View)
    type, extends(forge_widget) :: QListView
        private
        type(c_ptr) :: model = c_null_ptr  ! Reference to AbstractItemModel
        integer :: current_index = -1
        type(ViewMode) :: view_mode
        logical :: alternating_row_colors = .false.
        type(signal_int) :: clicked
        type(signal_int) :: double_clicked
        type(signal_int) :: current_changed
    contains
        procedure :: set_model => listview_set_model
        procedure :: get_model => listview_get_model
        procedure :: set_current_index => listview_set_current
        procedure :: get_current_index => listview_get_current
        procedure :: set_view_mode => listview_set_view_mode
        procedure :: set_alternating_row_colors => listview_set_alternating
    end type QListView

    !> @brief List widget (convenience, no model needed)
    type, extends(QListView) :: QListWidget
        private
        ! Internal string list model
        character(len=:), allocatable :: items(:)
        integer :: item_count = 0
    contains
        procedure :: add_item => listwidget_add_item
        procedure :: insert_item => listwidget_insert_item
        procedure :: take_item => listwidget_take_item
        procedure :: get_item => listwidget_get_item
        procedure :: set_item_text => listwidget_set_item_text
        procedure :: count => listwidget_count
        procedure :: clear => listwidget_clear
    end type QListWidget

contains

    ! ========== QListView Implementation ==========

    subroutine listview_set_model(this, model)
        class(QListView), intent(inout) :: this
        type(c_ptr), intent(in) :: model
        this%model = model
    end subroutine listview_set_model

    function listview_get_model(this) result(model)
        class(QListView), intent(in) :: this
        type(c_ptr) :: model
        model = this%model
    end function listview_get_model

    subroutine listview_set_current(this, index)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: index
        integer :: old_index
        
        old_index = this%current_index
        this%current_index = index
        
        if (old_index /= index) then
            call this%current_changed%emit(index)
        end if
    end subroutine listview_set_current

    function listview_get_current(this) result(index)
        class(QListView), intent(in) :: this
        integer :: index
        index = this%current_index
    end function listview_get_current

    subroutine listview_set_view_mode(this, mode)
        class(QListView), intent(inout) :: this
        integer, intent(in) :: mode
        this%view_mode%value = mode
    end subroutine listview_set_view_mode

    subroutine listview_set_alternating(this, alternating)
        class(QListView), intent(inout) :: this
        logical, intent(in) :: alternating
        this%alternating_row_colors = alternating
    end subroutine listview_set_alternating

    ! ========== QListWidget Implementation ==========

    subroutine listwidget_add_item(this, text)
        class(QListWidget), intent(inout) :: this
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: temp(:)
        integer :: max_len, i
        
        if (.not. allocated(this%items)) then
            allocate(character(len=256) :: this%items(10))
            this%items = ""
        else if (this%item_count >= size(this%items)) then
            ! Get max length
            max_len = len(this%items)
            if (len(text) > max_len) max_len = len(text)
            
            allocate(character(len=max_len) :: temp(size(this%items) * 2))
            do i = 1, this%item_count
                temp(i) = this%items(i)
            end do
            call move_alloc(temp, this%items)
        end if
        
        this%item_count = this%item_count + 1
        this%items(this%item_count) = text
    end subroutine listwidget_add_item

    subroutine listwidget_insert_item(this, row, text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: text
        integer :: i
        
        call this%add_item(text)  ! Ensure space
        
        ! Shift items
        do i = this%item_count, row + 2, -1
            this%items(i) = this%items(i-1)
        end do
        
        this%items(row + 1) = text
    end subroutine listwidget_insert_item

    function listwidget_take_item(this, row) result(text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=:), allocatable :: text
        integer :: i
        
        if (row < 0 .or. row >= this%item_count) then
            allocate(character(len=0) :: text)
            return
        end if
        
        text = trim(this%items(row + 1))
        
        ! Remove item
        do i = row + 1, this%item_count - 1
            this%items(i) = this%items(i+1)
        end do
        this%item_count = this%item_count - 1
    end function listwidget_take_item

    function listwidget_get_item(this, row) result(text)
        class(QListWidget), intent(in) :: this
        integer, intent(in) :: row
        character(len=:), allocatable :: text
        
        if (row >= 0 .and. row < this%item_count) then
            text = trim(this%items(row + 1))
        else
            allocate(character(len=0) :: text)
        end if
    end function listwidget_get_item

    subroutine listwidget_set_item_text(this, row, text)
        class(QListWidget), intent(inout) :: this
        integer, intent(in) :: row
        character(len=*), intent(in) :: text
        
        if (row >= 0 .and. row < this%item_count) then
            this%items(row + 1) = text
        end if
    end subroutine listwidget_set_item_text

    function listwidget_count(this) result(cnt)
        class(QListWidget), intent(in) :: this
        integer :: cnt
        cnt = this%item_count
    end function listwidget_count

    subroutine listwidget_clear(this)
        class(QListWidget), intent(inout) :: this
        this%item_count = 0
    end subroutine listwidget_clear

end module forge_listview

