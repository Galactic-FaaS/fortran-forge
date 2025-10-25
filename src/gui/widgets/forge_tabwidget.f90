!> @brief TabWidget (tabbed interface)
!> @details Widget with multiple tabs/pages
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_tabwidget
    use forge_widgets
    use forge_types
    use forge_string_utils
    use forge_signals
    implicit none
    private

    public :: QTabWidget, TabPosition
    public :: North, South, East, West

    !> Tab position constants
    integer, parameter :: North = 0
    integer, parameter :: South = 1
    integer, parameter :: East = 2
    integer, parameter :: West = 3

    type :: TabPosition
        integer :: value = North
    end type TabPosition

    !> @brief Tab widget
    type, extends(forge_widget) :: QTabWidget
        private
        class(forge_widget), allocatable :: pages(:)
        type(QString), allocatable :: tab_texts(:)
        type(QString), allocatable :: tab_tooltips(:)
        integer :: tab_count = 0
        integer :: current_index = -1
        type(TabPosition) :: tab_position
        logical :: tabs_closable = .false.
        logical :: movable = .false.
        type(signal_int) :: current_changed
        type(signal_int) :: tab_close_requested
    contains
        procedure :: add_tab => tabwidget_add_tab
        procedure :: insert_tab => tabwidget_insert_tab
        procedure :: remove_tab => tabwidget_remove_tab
        procedure :: set_current_index => tabwidget_set_current_index
        procedure :: get_current_index => tabwidget_get_current_index
        procedure :: get_current_widget => tabwidget_get_current_widget
        procedure :: get_widget_at => tabwidget_get_widget_at
        procedure :: set_tab_text => tabwidget_set_tab_text
        procedure :: get_tab_text => tabwidget_get_tab_text
        procedure :: set_tab_tooltip => tabwidget_set_tab_tooltip
        procedure :: count => tabwidget_count
        procedure :: set_tabs_closable => tabwidget_set_closable
        procedure :: set_tab_position => tabwidget_set_position
    end type QTabWidget

contains

    function tabwidget_add_tab(this, widget, text) result(index)
        class(QTabWidget), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        character(len=*), intent(in) :: text
        integer :: index
        class(forge_widget), allocatable :: temp_pages(:)
        type(QString), allocatable :: temp_texts(:), temp_tooltips(:)
        
        if (.not. allocated(this%pages)) then
            allocate(this%pages(10))
            allocate(this%tab_texts(10))
            allocate(this%tab_tooltips(10))
        else if (this%tab_count >= size(this%pages)) then
            ! Expand arrays
            allocate(temp_pages(size(this%pages) * 2))
            allocate(temp_texts(size(this%tab_texts) * 2))
            allocate(temp_tooltips(size(this%tab_tooltips) * 2))
            temp_pages(1:this%tab_count) = this%pages(1:this%tab_count)
            temp_texts(1:this%tab_count) = this%tab_texts(1:this%tab_count)
            temp_tooltips(1:this%tab_count) = this%tab_tooltips(1:this%tab_count)
            call move_alloc(temp_pages, this%pages)
            call move_alloc(temp_texts, this%tab_texts)
            call move_alloc(temp_tooltips, this%tab_tooltips)
        end if
        
        this%tab_count = this%tab_count + 1
        this%pages(this%tab_count) = widget
        call this%tab_texts(this%tab_count)%set(text)
        
        if (this%current_index < 0) then
            this%current_index = 0
        end if
        
        index = this%tab_count - 1  ! 0-based index
    end function tabwidget_add_tab

    function tabwidget_insert_tab(this, index, widget, text) result(actual_index)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: index
        class(forge_widget), intent(in) :: widget
        character(len=*), intent(in) :: text
        integer :: actual_index
        
        ! TODO: Insert at specific position
        actual_index = this%add_tab(widget, text)
    end function tabwidget_insert_tab

    subroutine tabwidget_remove_tab(this, index)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i
        
        if (index < 0 .or. index >= this%tab_count) return
        
        ! Shift tabs
        do i = index + 1, this%tab_count - 1
            this%pages(i) = this%pages(i+1)
            this%tab_texts(i) = this%tab_texts(i+1)
            this%tab_tooltips(i) = this%tab_tooltips(i+1)
        end do
        
        this%tab_count = this%tab_count - 1
        
        if (this%current_index >= this%tab_count) then
            this%current_index = this%tab_count - 1
        end if
    end subroutine tabwidget_remove_tab

    subroutine tabwidget_set_current_index(this, index)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: index
        integer :: old_index
        
        if (index < 0 .or. index >= this%tab_count) return
        
        old_index = this%current_index
        this%current_index = index
        
        if (old_index /= index) then
            call this%current_changed%emit(index)
        end if
    end subroutine tabwidget_set_current_index

    function tabwidget_get_current_index(this) result(index)
        class(QTabWidget), intent(in) :: this
        integer :: index
        index = this%current_index
    end function tabwidget_get_current_index

    function tabwidget_get_current_widget(this) result(widget)
        class(QTabWidget), intent(in) :: this
        class(forge_widget), allocatable :: widget
        
        if (this%current_index >= 0 .and. this%current_index < this%tab_count) then
            allocate(widget, source=this%pages(this%current_index + 1))
        end if
    end function tabwidget_get_current_widget

    function tabwidget_get_widget_at(this, index) result(widget)
        class(QTabWidget), intent(in) :: this
        integer, intent(in) :: index
        class(forge_widget), allocatable :: widget
        
        if (index >= 0 .and. index < this%tab_count) then
            allocate(widget, source=this%pages(index + 1))
        end if
    end function tabwidget_get_widget_at

    subroutine tabwidget_set_tab_text(this, index, text)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: index
        character(len=*), intent(in) :: text
        
        if (index >= 0 .and. index < this%tab_count) then
            call this%tab_texts(index + 1)%set(text)
        end if
    end subroutine tabwidget_set_tab_text

    function tabwidget_get_tab_text(this, index) result(text)
        class(QTabWidget), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: text
        
        if (index >= 0 .and. index < this%tab_count) then
            text = this%tab_texts(index + 1)%get()
        else
            allocate(character(len=0) :: text)
        end if
    end function tabwidget_get_tab_text

    subroutine tabwidget_set_tab_tooltip(this, index, tooltip)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: index
        character(len=*), intent(in) :: tooltip
        
        if (index >= 0 .and. index < this%tab_count) then
            call this%tab_tooltips(index + 1)%set(tooltip)
        end if
    end subroutine tabwidget_set_tab_tooltip

    function tabwidget_count(this) result(cnt)
        class(QTabWidget), intent(in) :: this
        integer :: cnt
        cnt = this%tab_count
    end function tabwidget_count

    subroutine tabwidget_set_closable(this, closable)
        class(QTabWidget), intent(inout) :: this
        logical, intent(in) :: closable
        this%tabs_closable = closable
    end subroutine tabwidget_set_closable

    subroutine tabwidget_set_position(this, position)
        class(QTabWidget), intent(inout) :: this
        integer, intent(in) :: position
        this%tab_position%value = position
    end subroutine tabwidget_set_position

end module forge_tabwidget

