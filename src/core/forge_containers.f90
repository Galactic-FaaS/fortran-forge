!> @brief Container types (List, Map, Set, etc.)
!> @details Qt-equivalent collection types for Fortran
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_containers
    use iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: QList_int, QList_real, QList_string
    public :: QMap_string_int, QStack_int, QQueue_int

    !> Initial capacity for dynamic arrays
    integer, parameter :: INITIAL_CAPACITY = 10

    !> @brief Dynamic list of integers (QList<int> equivalent)
    type :: QList_int
        private
        integer(int32), allocatable :: items(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: append => qlist_int_append
        procedure :: prepend => qlist_int_prepend
        procedure :: insert => qlist_int_insert
        procedure :: remove => qlist_int_remove
        procedure :: at => qlist_int_at
        procedure :: set => qlist_int_set
        procedure :: size => qlist_int_size
        procedure :: is_empty => qlist_int_is_empty
        procedure :: clear => qlist_int_clear
        procedure :: contains => qlist_int_contains
        procedure :: index_of => qlist_int_index_of
    end type QList_int

    !> @brief Dynamic list of reals
    type :: QList_real
        private
        real(real64), allocatable :: items(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: append => qlist_real_append
        procedure :: at => qlist_real_at
        procedure :: size => qlist_real_size
        procedure :: clear => qlist_real_clear
    end type QList_real

    !> @brief Dynamic list of strings
    type :: QList_string
        private
        character(len=:), allocatable :: items(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: append => qlist_string_append
        procedure :: at => qlist_string_at
        procedure :: size => qlist_string_size
        procedure :: clear => qlist_string_clear
    end type QList_string

    !> @brief Simple string-to-integer map
    type :: QMap_string_int
        private
        character(len=:), allocatable :: keys(:)
        integer(int32), allocatable :: values(:)
        integer :: count = 0
    contains
        procedure :: insert => qmap_string_int_insert
        procedure :: get => qmap_string_int_get
        procedure :: contains => qmap_string_int_contains
        procedure :: remove => qmap_string_int_remove
        procedure :: size => qmap_string_int_size
        procedure :: clear => qmap_string_int_clear
    end type QMap_string_int

    !> @brief Stack (LIFO) of integers
    type :: QStack_int
        private
        integer(int32), allocatable :: items(:)
        integer :: top = 0
    contains
        procedure :: push => qstack_int_push
        procedure :: pop => qstack_int_pop
        procedure :: peek => qstack_int_peek
        procedure :: is_empty => qstack_int_is_empty
        procedure :: size => qstack_int_size
    end type QStack_int

    !> @brief Queue (FIFO) of integers
    type :: QQueue_int
        private
        integer(int32), allocatable :: items(:)
        integer :: front = 1
        integer :: rear = 0
        integer :: count = 0
    contains
        procedure :: enqueue => qqueue_int_enqueue
        procedure :: dequeue => qqueue_int_dequeue
        procedure :: peek => qqueue_int_peek
        procedure :: is_empty => qqueue_int_is_empty
        procedure :: size => qqueue_int_size
    end type QQueue_int

contains

    ! ========== QList_int Implementation ==========

    subroutine qlist_int_append(this, item)
        class(QList_int), intent(inout) :: this
        integer(int32), intent(in) :: item
        integer(int32), allocatable :: temp(:)

        ! Ensure capacity
        if (.not. allocated(this%items)) then
            allocate(this%items(INITIAL_CAPACITY))
            this%capacity = INITIAL_CAPACITY
        else if (this%count >= this%capacity) then
            ! Double capacity
            allocate(temp(this%capacity * 2))
            temp(1:this%count) = this%items(1:this%count)
            call move_alloc(temp, this%items)
            this%capacity = this%capacity * 2
        end if

        this%count = this%count + 1
        this%items(this%count) = item
    end subroutine qlist_int_append

    subroutine qlist_int_prepend(this, item)
        class(QList_int), intent(inout) :: this
        integer(int32), intent(in) :: item
        integer :: i

        call this%insert(1, item)
    end subroutine qlist_int_prepend

    subroutine qlist_int_insert(this, index, item)
        class(QList_int), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item
        integer :: i

        call this%append(item)  ! Ensure space
        ! Shift items
        do i = this%count, index + 1, -1
            this%items(i) = this%items(i-1)
        end do
        this%items(index) = item
    end subroutine qlist_int_insert

    subroutine qlist_int_remove(this, index)
        class(QList_int), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i

        if (index < 1 .or. index > this%count) return

        do i = index, this%count - 1
            this%items(i) = this%items(i+1)
        end do
        this%count = this%count - 1
    end subroutine qlist_int_remove

    function qlist_int_at(this, index) result(item)
        class(QList_int), intent(in) :: this
        integer, intent(in) :: index
        integer(int32) :: item

        if (index >= 1 .and. index <= this%count) then
            item = this%items(index)
        else
            item = 0
        end if
    end function qlist_int_at

    subroutine qlist_int_set(this, index, item)
        class(QList_int), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item

        if (index >= 1 .and. index <= this%count) then
            this%items(index) = item
        end if
    end subroutine qlist_int_set

    function qlist_int_size(this) result(size_val)
        class(QList_int), intent(in) :: this
        integer :: size_val
        
        size_val = this%count
    end function qlist_int_size

    function qlist_int_is_empty(this) result(empty)
        class(QList_int), intent(in) :: this
        logical :: empty
        
        empty = (this%count == 0)
    end function qlist_int_is_empty

    subroutine qlist_int_clear(this)
        class(QList_int), intent(inout) :: this
        
        this%count = 0
    end subroutine qlist_int_clear

    function qlist_int_contains(this, item) result(found)
        class(QList_int), intent(in) :: this
        integer(int32), intent(in) :: item
        logical :: found
        integer :: i

        found = .false.
        do i = 1, this%count
            if (this%items(i) == item) then
                found = .true.
                return
            end if
        end do
    end function qlist_int_contains

    function qlist_int_index_of(this, item) result(index)
        class(QList_int), intent(in) :: this
        integer(int32), intent(in) :: item
        integer :: index
        integer :: i

        index = -1
        do i = 1, this%count
            if (this%items(i) == item) then
                index = i
                return
            end if
        end do
    end function qlist_int_index_of

    ! ========== QList_real Implementation (Simplified) ==========

    subroutine qlist_real_append(this, item)
        class(QList_real), intent(inout) :: this
        real(real64), intent(in) :: item
        real(real64), allocatable :: temp(:)

        if (.not. allocated(this%items)) then
            allocate(this%items(INITIAL_CAPACITY))
            this%capacity = INITIAL_CAPACITY
        else if (this%count >= this%capacity) then
            allocate(temp(this%capacity * 2))
            temp(1:this%count) = this%items(1:this%count)
            call move_alloc(temp, this%items)
            this%capacity = this%capacity * 2
        end if

        this%count = this%count + 1
        this%items(this%count) = item
    end subroutine qlist_real_append

    function qlist_real_at(this, index) result(item)
        class(QList_real), intent(in) :: this
        integer, intent(in) :: index
        real(real64) :: item

        if (index >= 1 .and. index <= this%count) then
            item = this%items(index)
        else
            item = 0.0_real64
        end if
    end function qlist_real_at

    function qlist_real_size(this) result(size_val)
        class(QList_real), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function qlist_real_size

    subroutine qlist_real_clear(this)
        class(QList_real), intent(inout) :: this
        this%count = 0
    end subroutine qlist_real_clear

    ! ========== QList_string Implementation (Simplified) ==========

    subroutine qlist_string_append(this, item)
        class(QList_string), intent(inout) :: this
        character(len=*), intent(in) :: item
        ! TODO: Proper implementation with dynamic string arrays
    end subroutine qlist_string_append

    function qlist_string_at(this, index) result(item)
        class(QList_string), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: item
        allocate(character(len=0) :: item)
    end function qlist_string_at

    function qlist_string_size(this) result(size_val)
        class(QList_string), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function qlist_string_size

    subroutine qlist_string_clear(this)
        class(QList_string), intent(inout) :: this
        this%count = 0
    end subroutine qlist_string_clear

    ! ========== QMap Implementation (Simplified) ==========

    subroutine qmap_string_int_insert(this, key, value)
        class(QMap_string_int), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        ! TODO: Proper hash map implementation
    end subroutine qmap_string_int_insert

    function qmap_string_int_get(this, key, found) result(value)
        class(QMap_string_int), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        value = 0
        if (present(found)) found = .false.
    end function qmap_string_int_get

    function qmap_string_int_contains(this, key) result(found)
        class(QMap_string_int), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: found
        found = .false.
    end function qmap_string_int_contains

    subroutine qmap_string_int_remove(this, key)
        class(QMap_string_int), intent(inout) :: this
        character(len=*), intent(in) :: key
    end subroutine qmap_string_int_remove

    function qmap_string_int_size(this) result(size_val)
        class(QMap_string_int), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function qmap_string_int_size

    subroutine qmap_string_int_clear(this)
        class(QMap_string_int), intent(inout) :: this
        this%count = 0
    end subroutine qmap_string_int_clear

    ! ========== QStack Implementation ==========

    subroutine qstack_int_push(this, item)
        class(QStack_int), intent(inout) :: this
        integer(int32), intent(in) :: item
        integer(int32), allocatable :: temp(:)

        if (.not. allocated(this%items)) then
            allocate(this%items(INITIAL_CAPACITY))
        else if (this%top >= size(this%items)) then
            allocate(temp(size(this%items) * 2))
            temp(1:this%top) = this%items(1:this%top)
            call move_alloc(temp, this%items)
        end if

        this%top = this%top + 1
        this%items(this%top) = item
    end subroutine qstack_int_push

    function qstack_int_pop(this) result(item)
        class(QStack_int), intent(inout) :: this
        integer(int32) :: item

        if (this%top > 0) then
            item = this%items(this%top)
            this%top = this%top - 1
        else
            item = 0
        end if
    end function qstack_int_pop

    function qstack_int_peek(this) result(item)
        class(QStack_int), intent(in) :: this
        integer(int32) :: item

        if (this%top > 0) then
            item = this%items(this%top)
        else
            item = 0
        end if
    end function qstack_int_peek

    function qstack_int_is_empty(this) result(empty)
        class(QStack_int), intent(in) :: this
        logical :: empty
        empty = (this%top == 0)
    end function qstack_int_is_empty

    function qstack_int_size(this) result(size_val)
        class(QStack_int), intent(in) :: this
        integer :: size_val
        size_val = this%top
    end function qstack_int_size

    ! ========== QQueue Implementation ==========

    subroutine qqueue_int_enqueue(this, item)
        class(QQueue_int), intent(inout) :: this
        integer(int32), intent(in) :: item
        integer(int32), allocatable :: temp(:)

        if (.not. allocated(this%items)) then
            allocate(this%items(INITIAL_CAPACITY))
        else if (this%count >= size(this%items)) then
            allocate(temp(size(this%items) * 2))
            temp(1:this%count) = this%items(1:this%count)
            call move_alloc(temp, this%items)
        end if

        this%rear = this%rear + 1
        this%items(this%rear) = item
        this%count = this%count + 1
    end subroutine qqueue_int_enqueue

    function qqueue_int_dequeue(this) result(item)
        class(QQueue_int), intent(inout) :: this
        integer(int32) :: item

        if (this%count > 0) then
            item = this%items(this%front)
            this%front = this%front + 1
            this%count = this%count - 1
        else
            item = 0
        end if
    end function qqueue_int_dequeue

    function qqueue_int_peek(this) result(item)
        class(QQueue_int), intent(in) :: this
        integer(int32) :: item

        if (this%count > 0) then
            item = this%items(this%front)
        else
            item = 0
        end if
    end function qqueue_int_peek

    function qqueue_int_is_empty(this) result(empty)
        class(QQueue_int), intent(in) :: this
        logical :: empty
        empty = (this%count == 0)
    end function qqueue_int_is_empty

    function qqueue_int_size(this) result(size_val)
        class(QQueue_int), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function qqueue_int_size

end module forge_containers

