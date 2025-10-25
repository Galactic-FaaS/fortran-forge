!> @brief ComboBox widget (dropdown selector)
!> @details Dropdown list for selecting from options
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_combobox
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    use forge_string_utils
    implicit none
    private

    public :: QComboBox

    !> @brief ComboBox widget
    type, extends(forge_widget) :: QComboBox
        private
        type(QString), allocatable :: items(:)
        integer :: current_index = -1
        integer :: count = 0
        logical :: editable = .false.
        type(signal_int) :: current_index_changed
        type(signal_string) :: current_text_changed
        type(signal_void) :: activated
    contains
        procedure :: add_item => combobox_add_item
        procedure :: add_items => combobox_add_items
        procedure :: insert_item => combobox_insert_item
        procedure :: remove_item => combobox_remove_item
        procedure :: clear => combobox_clear
        procedure :: get_count => combobox_get_count
        procedure :: set_current_index => combobox_set_current_index
        procedure :: get_current_index => combobox_get_current_index
        procedure :: get_current_text => combobox_get_current_text
        procedure :: get_item_text => combobox_get_item_text
        procedure :: set_editable => combobox_set_editable
        procedure :: is_editable => combobox_is_editable
    end type QComboBox

contains

    subroutine combobox_add_item(this, text, user_data)
        class(QComboBox), intent(inout) :: this
        character(len=*), intent(in) :: text
        type(c_ptr), intent(in), optional :: user_data
        type(QString), allocatable :: temp(:)
        
        if (.not. allocated(this%items)) then
            allocate(this%items(10))
        else if (this%count >= size(this%items)) then
            ! Expand array
            allocate(temp(size(this%items) * 2))
            temp(1:this%count) = this%items(1:this%count)
            call move_alloc(temp, this%items)
        end if
        
        this%count = this%count + 1
        call this%items(this%count)%set(text)
        
        if (this%current_index < 0) then
            this%current_index = 0
        end if
    end subroutine combobox_add_item

    subroutine combobox_add_items(this, texts)
        class(QComboBox), intent(inout) :: this
        character(len=*), intent(in) :: texts(:)
        integer :: i
        
        do i = 1, size(texts)
            call this%add_item(texts(i))
        end do
    end subroutine combobox_add_items

    subroutine combobox_insert_item(this, index, text)
        class(QComboBox), intent(inout) :: this
        integer, intent(in) :: index
        character(len=*), intent(in) :: text
        integer :: i
        
        call this%add_item("")  ! Ensure space
        
        ! Shift items
        do i = this%count, index + 1, -1
            this%items(i) = this%items(i-1)
        end do
        
        call this%items(index)%set(text)
    end subroutine combobox_insert_item

    subroutine combobox_remove_item(this, index)
        class(QComboBox), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i
        
        if (index < 1 .or. index > this%count) return
        
        do i = index, this%count - 1
            this%items(i) = this%items(i+1)
        end do
        this%count = this%count - 1
        
        if (this%current_index >= this%count) then
            this%current_index = this%count - 1
        end if
    end subroutine combobox_remove_item

    subroutine combobox_clear(this)
        class(QComboBox), intent(inout) :: this
        
        this%count = 0
        this%current_index = -1
    end subroutine combobox_clear

    function combobox_get_count(this) result(cnt)
        class(QComboBox), intent(in) :: this
        integer :: cnt
        cnt = this%count
    end function combobox_get_count

    subroutine combobox_set_current_index(this, index)
        class(QComboBox), intent(inout) :: this
        integer, intent(in) :: index
        integer :: old_index
        
        if (index < 0 .or. index >= this%count) return
        
        old_index = this%current_index
        this%current_index = index
        
        if (old_index /= index) then
            call this%current_index_changed%emit(index)
            call this%current_text_changed%emit(this%get_current_text())
        end if
    end subroutine combobox_set_current_index

    function combobox_get_current_index(this) result(index)
        class(QComboBox), intent(in) :: this
        integer :: index
        index = this%current_index
    end function combobox_get_current_index

    function combobox_get_current_text(this) result(text)
        class(QComboBox), intent(in) :: this
        character(len=:), allocatable :: text
        
        if (this%current_index >= 0 .and. this%current_index < this%count) then
            text = this%items(this%current_index + 1)%get()
        else
            allocate(character(len=0) :: text)
        end if
    end function combobox_get_current_text

    function combobox_get_item_text(this, index) result(text)
        class(QComboBox), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: text
        
        if (index >= 0 .and. index < this%count) then
            text = this%items(index + 1)%get()
        else
            allocate(character(len=0) :: text)
        end if
    end function combobox_get_item_text

    subroutine combobox_set_editable(this, editable)
        class(QComboBox), intent(inout) :: this
        logical, intent(in) :: editable
        this%editable = editable
    end subroutine combobox_set_editable

    function combobox_is_editable(this) result(editable)
        class(QComboBox), intent(in) :: this
        logical :: editable
        editable = this%editable
    end function combobox_is_editable

end module forge_combobox

