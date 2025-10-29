!> @brief Qt-style container classes with implicit sharing
!> @details Comprehensive implementation of QVector, QLinkedList, QHash, QSet
!> with implicit sharing (copy-on-write), iterators, and Qt-compatible APIs
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_containers
    use iso_fortran_env, only: int32, int64, real64
    use forge_types, only: forge_string
    implicit none
    private

    !> @brief Base class for shared data with reference counting
    type :: QSharedData
        integer :: ref = 1
    contains
        procedure :: ref_ => shared_data_ref
        procedure :: deref => shared_data_deref
        procedure :: is_shared => shared_data_is_shared
    end type QSharedData

    !> @brief QVector data (dynamic array)
    type, extends(QSharedData) :: QVectorData
        integer(int32), allocatable :: items(:)
        integer :: count = 0
        integer :: capacity = 0
    end type QVectorData

    !> @brief QLinkedList node
    type :: QLinkedListNode
        integer(int32) :: value
        type(QLinkedListNode), pointer :: next => null()
        type(QLinkedListNode), pointer :: prev => null()
    end type QLinkedListNode

    !> @brief QLinkedList data
    type, extends(QSharedData) :: QLinkedListData
        type(QLinkedListNode), pointer :: first => null()
        type(QLinkedListNode), pointer :: last => null()
        integer :: count = 0
    end type QLinkedListData

    !> @brief QHash bucket entry
    type :: QHashNode
        character(len=:), allocatable :: key
        integer(int32) :: value
        type(QHashNode), pointer :: next => null()
    end type QHashNode

    !> @brief QHash data
    type, extends(QSharedData) :: QHashData
        type(QHashNode), pointer :: buckets(:) => null()
        integer :: count = 0
        integer :: bucket_count = 0
    end type QHashData

    !> @brief QSet data (uses hash table internally)
    type, extends(QSharedData) :: QSetData
        type(QHashNode), pointer :: buckets(:) => null()
        integer :: count = 0
        integer :: bucket_count = 0
    end type QSetData

    !> @brief QVector iterator
    type :: QVectorIterator
        type(QVector), pointer :: container => null()
        integer :: index = 0
    contains
        procedure :: has_next => qvector_iterator_has_next
        procedure :: next => qvector_iterator_next
        procedure :: value => qvector_iterator_value
    end type QVectorIterator

    !> @brief QLinkedList iterator
    type :: QLinkedListIterator
        type(QLinkedList), pointer :: container => null()
        type(QLinkedListNode), pointer :: current => null()
    contains
        procedure :: has_next => qlinkedlist_iterator_has_next
        procedure :: next => qlinkedlist_iterator_next
        procedure :: value => qlinkedlist_iterator_value
    end type QLinkedListIterator

    !> @brief QHash iterator
    type :: QHashIterator
        type(QHash), pointer :: container => null()
        integer :: bucket_index = 0
        type(QHashNode), pointer :: current => null()
    contains
        procedure :: has_next => qhash_iterator_has_next
        procedure :: next => qhash_iterator_next
        procedure :: key => qhash_iterator_key
        procedure :: value => qhash_iterator_value
    end type QHashIterator

    !> @brief QSet iterator
    type :: QSetIterator
        type(QSet), pointer :: container => null()
        integer :: bucket_index = 0
        type(QHashNode), pointer :: current => null()
    contains
        procedure :: has_next => qset_iterator_has_next
        procedure :: next => qset_iterator_next
        procedure :: value => qset_iterator_value
    end type QSetIterator

    !> @brief Dynamic array with implicit sharing and fast random access
    type :: QVector
        type(QVectorData), pointer :: d => null()
    contains
        procedure :: init => qvector_init
        procedure :: detach => qvector_detach
        procedure :: append => qvector_append
        procedure :: prepend => qvector_prepend
        procedure :: insert => qvector_insert
        procedure :: remove => qvector_remove
        procedure :: at => qvector_at
        procedure :: set => qvector_set
        procedure :: size => qvector_size
        procedure :: is_empty => qvector_is_empty
        procedure :: clear => qvector_clear
        procedure :: contains => qvector_contains
        procedure :: index_of => qvector_index_of
        procedure :: reserve => qvector_reserve
        procedure :: squeeze => qvector_squeeze
        procedure :: iterator => qvector_iterator
        final :: qvector_finalize
    end type QVector

    !> @brief Doubly-linked list with fast insertions/removals
    type :: QLinkedList
        type(QLinkedListData), pointer :: d => null()
    contains
        procedure :: init => qlinkedlist_init
        procedure :: detach => qlinkedlist_detach
        procedure :: append => qlinkedlist_append
        procedure :: prepend => qlinkedlist_prepend
        procedure :: insert => qlinkedlist_insert
        procedure :: remove => qlinkedlist_remove
        procedure :: at => qlinkedlist_at
        procedure :: set => qlinkedlist_set
        procedure :: size => qlinkedlist_size
        procedure :: is_empty => qlinkedlist_is_empty
        procedure :: clear => qlinkedlist_clear
        procedure :: contains => qlinkedlist_contains
        procedure :: index_of => qlinkedlist_index_of
        procedure :: iterator => qlinkedlist_iterator
        final :: qlinkedlist_finalize
    end type QLinkedList

    !> @brief Hash table with key-value pairs and fast lookups
    type :: QHash
        type(QHashData), pointer :: d => null()
    contains
        procedure :: init => qhash_init
        procedure :: detach => qhash_detach
        procedure :: insert => qhash_insert
        procedure :: get => qhash_get
        procedure :: contains => qhash_contains
        procedure :: remove => qhash_remove
        procedure :: size => qhash_size
        procedure :: is_empty => qhash_is_empty
        procedure :: clear => qhash_clear
        procedure :: keys => qhash_keys
        procedure :: values => qhash_values
        procedure :: iterator => qhash_iterator
        final :: qhash_finalize
    end type QHash

    !> @brief Unique value collection with fast membership testing
    type :: QSet
        type(QSetData), pointer :: d => null()
    contains
        procedure :: init => qset_init
        procedure :: detach => qset_detach
        procedure :: insert => qset_insert
        procedure :: contains => qset_contains
        procedure :: remove => qset_remove
        procedure :: size => qset_size
        procedure :: is_empty => qset_is_empty
        procedure :: clear => qset_clear
        procedure :: iterator => qset_iterator
        procedure :: unite => qset_unite
        procedure :: intersect => qset_intersect
        procedure :: subtract => qset_subtract
        final :: qset_finalize
    end type QSet

    ! Public interfaces
    public :: QSharedData, QVector, QLinkedList, QHash, QSet
    public :: QVectorIterator, QLinkedListIterator, QHashIterator, QSetIterator

    ! Constants
    integer, parameter :: INITIAL_CAPACITY = 10
    integer, parameter :: INITIAL_BUCKET_COUNT = 16

contains

    ! ========== QSharedData Implementation ==========

    subroutine shared_data_ref(this)
        class(QSharedData), intent(inout) :: this
        this%ref = this%ref + 1
    end subroutine shared_data_ref

    subroutine shared_data_deref(this)
        class(QSharedData), intent(inout) :: this
        this%ref = this%ref - 1
    end subroutine shared_data_deref

    function shared_data_is_shared(this) result(shared)
        class(QSharedData), intent(in) :: this
        logical :: shared
        shared = (this%ref > 1)
    end function shared_data_is_shared

    ! ========== QVector Implementation ==========

    subroutine qvector_init(this)
        class(QVector), intent(inout) :: this
        if (.not. associated(this%d)) then
            allocate(this%d)
            allocate(this%d%items(INITIAL_CAPACITY))
            this%d%capacity = INITIAL_CAPACITY
        end if
    end subroutine qvector_init

    subroutine qvector_detach(this)
        class(QVector), intent(inout) :: this
        type(QVectorData), pointer :: new_d

        if (.not. associated(this%d)) then
            call this%init()
            return
        end if

        if (this%d%ref > 1) then
            allocate(new_d)
            if (allocated(this%d%items)) then
                allocate(new_d%items(size(this%d%items)))
                new_d%items = this%d%items
            end if
            new_d%count = this%d%count
            new_d%capacity = this%d%capacity
            new_d%ref = 1
            call this%d%deref()
            this%d => new_d
        end if
    end subroutine qvector_detach

    subroutine qvector_append(this, item)
        class(QVector), intent(inout) :: this
        integer(int32), intent(in) :: item
        integer(int32), allocatable :: temp(:)

        call this%detach()
        if (.not. allocated(this%d%items)) then
            allocate(this%d%items(INITIAL_CAPACITY))
            this%d%capacity = INITIAL_CAPACITY
        else if (this%d%count >= this%d%capacity) then
            allocate(temp(this%d%capacity * 2))
            temp(1:this%d%count) = this%d%items(1:this%d%count)
            call move_alloc(temp, this%d%items)
            this%d%capacity = this%d%capacity * 2
        end if

        this%d%count = this%d%count + 1
        this%d%items(this%d%count) = item
    end subroutine qvector_append

    subroutine qvector_prepend(this, item)
        class(QVector), intent(inout) :: this
        integer(int32), intent(in) :: item
        call this%insert(1, item)
    end subroutine qvector_prepend

    subroutine qvector_insert(this, index, item)
        class(QVector), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item
        integer :: i

        if (index < 1 .or. index > this%d%count + 1) return

        call this%detach()
        call this%append(item)  ! Ensure space
        do i = this%d%count, index + 1, -1
            this%d%items(i) = this%d%items(i-1)
        end do
        this%d%items(index) = item
    end subroutine qvector_insert

    subroutine qvector_remove(this, index)
        class(QVector), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i

        if (index < 1 .or. index > this%d%count) return

        call this%detach()
        do i = index, this%d%count - 1
            this%d%items(i) = this%d%items(i+1)
        end do
        this%d%count = this%d%count - 1
    end subroutine qvector_remove

    function qvector_at(this, index) result(item)
        class(QVector), intent(in) :: this
        integer, intent(in) :: index
        integer(int32) :: item

        if (index >= 1 .and. index <= this%d%count) then
            item = this%d%items(index)
        else
            item = 0
        end if
    end function qvector_at

    subroutine qvector_set(this, index, item)
        class(QVector), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item

        if (index >= 1 .and. index <= this%d%count) then
            call this%detach()
            this%d%items(index) = item
        end if
    end subroutine qvector_set

    function qvector_size(this) result(size_val)
        class(QVector), intent(in) :: this
        integer :: size_val
        if (associated(this%d)) then
            size_val = this%d%count
        else
            size_val = 0
        end if
    end function qvector_size

    function qvector_is_empty(this) result(empty)
        class(QVector), intent(in) :: this
        logical :: empty
        empty = (this%size() == 0)
    end function qvector_is_empty

    subroutine qvector_clear(this)
        class(QVector), intent(inout) :: this
        call this%detach()
        this%d%count = 0
    end subroutine qvector_clear

    function qvector_contains(this, item) result(found)
        class(QVector), intent(in) :: this
        integer(int32), intent(in) :: item
        logical :: found
        integer :: i

        found = .false.
        do i = 1, this%d%count
            if (this%d%items(i) == item) then
                found = .true.
                return
            end if
        end do
    end function qvector_contains

    function qvector_index_of(this, item) result(index)
        class(QVector), intent(in) :: this
        integer(int32), intent(in) :: item
        integer :: index
        integer :: i

        index = -1
        do i = 1, this%d%count
            if (this%d%items(i) == item) then
                index = i
                return
            end if
        end do
    end function qvector_index_of

    subroutine qvector_reserve(this, size)
        class(QVector), intent(inout) :: this
        integer, intent(in) :: size
        integer(int32), allocatable :: temp(:)

        if (size <= this%d%capacity) return

        call this%detach()
        allocate(temp(size))
        if (allocated(this%d%items)) then
            temp(1:this%d%count) = this%d%items(1:this%d%count)
            deallocate(this%d%items)
        end if
        this%d%items = temp
        this%d%capacity = size
    end subroutine qvector_reserve

    subroutine qvector_squeeze(this)
        class(QVector), intent(inout) :: this
        integer(int32), allocatable :: temp(:)

        if (this%d%count == 0) then
            if (allocated(this%d%items)) deallocate(this%d%items)
            this%d%capacity = 0
            return
        end if

        if (this%d%count == this%d%capacity) return

        call this%detach()
        allocate(temp(this%d%count))
        temp = this%d%items(1:this%d%count)
        deallocate(this%d%items)
        this%d%items = temp
        this%d%capacity = this%d%count
    end subroutine qvector_squeeze

    function qvector_iterator(this) result(iter)
        class(QVector), intent(in) :: this
        type(QVectorIterator) :: iter
        iter%container => this
        iter%index = 0
    end function qvector_iterator

    subroutine qvector_finalize(this)
        class(QVector), intent(inout) :: this
        if (associated(this%d)) then
            call this%d%deref()
            if (this%d%ref == 0) then
                if (allocated(this%d%items)) deallocate(this%d%items)
                deallocate(this%d)
            end if
            this%d => null()
        end if
    end subroutine qvector_finalize

    ! ========== QVectorIterator Implementation ==========

    function qvector_iterator_has_next(this) result(has)
        class(QVectorIterator), intent(in) :: this
        logical :: has
        has = (this%index < this%container%size())
    end function qvector_iterator_has_next

    subroutine qvector_iterator_next(this)
        class(QVectorIterator), intent(inout) :: this
        this%index = this%index + 1
    end subroutine qvector_iterator_next

    function qvector_iterator_value(this) result(val)
        class(QVectorIterator), intent(in) :: this
        integer(int32) :: val
        val = this%container%at(this%index)
    end function qvector_iterator_value

    ! ========== QLinkedList Implementation ==========

    subroutine qlinkedlist_init(this)
        class(QLinkedList), intent(inout) :: this
        if (.not. associated(this%d)) then
            allocate(this%d)
        end if
    end subroutine qlinkedlist_init

    subroutine qlinkedlist_detach(this)
        class(QLinkedList), intent(inout) :: this
        type(QLinkedListData), pointer :: new_d
        type(QLinkedListNode), pointer :: node, new_node

        if (.not. associated(this%d)) then
            call this%init()
            return
        end if

        if (this%d%ref > 1) then
            allocate(new_d)
            new_d%count = this%d%count
            new_d%ref = 1

            ! Deep copy the list
            node => this%d%first
            do while (associated(node))
                allocate(new_node)
                new_node%value = node%value
                if (.not. associated(new_d%first)) then
                    new_d%first => new_node
                    new_d%last => new_node
                else
                    new_d%last%next => new_node
                    new_node%prev => new_d%last
                    new_d%last => new_node
                end if
                node => node%next
            end do

            call this%d%deref()
            this%d => new_d
        end if
    end subroutine qlinkedlist_detach

    subroutine qlinkedlist_append(this, item)
        class(QLinkedList), intent(inout) :: this
        integer(int32), intent(in) :: item
        type(QLinkedListNode), pointer :: node

        call this%detach()
        allocate(node)
        node%value = item

        if (.not. associated(this%d%first)) then
            this%d%first => node
            this%d%last => node
        else
            this%d%last%next => node
            node%prev => this%d%last
            this%d%last => node
        end if

        this%d%count = this%d%count + 1
    end subroutine qlinkedlist_append

    subroutine qlinkedlist_prepend(this, item)
        class(QLinkedList), intent(inout) :: this
        integer(int32), intent(in) :: item
        type(QLinkedListNode), pointer :: node

        call this%detach()
        allocate(node)
        node%value = item

        if (.not. associated(this%d%first)) then
            this%d%first => node
            this%d%last => node
        else
            node%next => this%d%first
            this%d%first%prev => node
            this%d%first => node
        end if

        this%d%count = this%d%count + 1
    end subroutine qlinkedlist_prepend

    subroutine qlinkedlist_insert(this, index, item)
        class(QLinkedList), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item
        type(QLinkedListNode), pointer :: node, current
        integer :: i

        if (index < 1 .or. index > this%d%count + 1) return

        call this%detach()

        if (index == 1) then
            call this%prepend(item)
            return
        end if

        if (index == this%d%count + 1) then
            call this%append(item)
            return
        end if

        allocate(node)
        node%value = item

        current => this%d%first
        do i = 1, index - 1
            current => current%next
        end do

        node%next => current
        node%prev => current%prev
        if (associated(current%prev)) current%prev%next => node
        current%prev => node

        this%d%count = this%d%count + 1
    end subroutine qlinkedlist_insert

    subroutine qlinkedlist_remove(this, index)
        class(QLinkedList), intent(inout) :: this
        integer, intent(in) :: index
        type(QLinkedListNode), pointer :: node
        integer :: i

        if (index < 1 .or. index > this%d%count) return

        call this%detach()

        node => this%d%first
        do i = 1, index - 1
            node => node%next
        end do

        if (associated(node%prev)) node%prev%next => node%next
        if (associated(node%next)) node%next%prev => node%prev

        if (associated(node, this%d%first)) this%d%first => node%next
        if (associated(node, this%d%last)) this%d%last => node%prev

        deallocate(node)
        this%d%count = this%d%count - 1
    end subroutine qlinkedlist_remove

    function qlinkedlist_at(this, index) result(item)
        class(QLinkedList), intent(in) :: this
        integer, intent(in) :: index
        integer(int32) :: item
        type(QLinkedListNode), pointer :: node
        integer :: i

        item = 0
        if (index < 1 .or. index > this%d%count) return

        node => this%d%first
        do i = 1, index - 1
            node => node%next
        end do
        item = node%value
    end function qlinkedlist_at

    subroutine qlinkedlist_set(this, index, item)
        class(QLinkedList), intent(inout) :: this
        integer, intent(in) :: index
        integer(int32), intent(in) :: item
        type(QLinkedListNode), pointer :: node
        integer :: i

        if (index < 1 .or. index > this%d%count) return

        call this%detach()

        node => this%d%first
        do i = 1, index - 1
            node => node%next
        end do
        node%value = item
    end subroutine qlinkedlist_set

    function qlinkedlist_size(this) result(size_val)
        class(QLinkedList), intent(in) :: this
        integer :: size_val
        if (associated(this%d)) then
            size_val = this%d%count
        else
            size_val = 0
        end if
    end function qlinkedlist_size

    function qlinkedlist_is_empty(this) result(empty)
        class(QLinkedList), intent(in) :: this
        logical :: empty
        empty = (this%size() == 0)
    end function qlinkedlist_is_empty

    subroutine qlinkedlist_clear(this)
        class(QLinkedList), intent(inout) :: this
        type(QLinkedListNode), pointer :: node, next_node

        call this%detach()

        node => this%d%first
        do while (associated(node))
            next_node => node%next
            deallocate(node)
            node => next_node
        end do

        this%d%first => null()
        this%d%last => null()
        this%d%count = 0
    end subroutine qlinkedlist_clear

    function qlinkedlist_contains(this, item) result(found)
        class(QLinkedList), intent(in) :: this
        integer(int32), intent(in) :: item
        logical :: found
        type(QLinkedListNode), pointer :: node

        found = .false.
        node => this%d%first
        do while (associated(node))
            if (node%value == item) then
                found = .true.
                return
            end if
            node => node%next
        end do
    end function qlinkedlist_contains

    function qlinkedlist_index_of(this, item) result(index)
        class(QLinkedList), intent(in) :: this
        integer(int32), intent(in) :: item
        integer :: index
        type(QLinkedListNode), pointer :: node
        integer :: i

        index = -1
        node => this%d%first
        i = 1
        do while (associated(node))
            if (node%value == item) then
                index = i
                return
            end if
            node => node%next
            i = i + 1
        end do
    end function qlinkedlist_index_of

    function qlinkedlist_iterator(this) result(iter)
        class(QLinkedList), intent(in) :: this
        type(QLinkedListIterator) :: iter
        iter%container => this
        iter%current => this%d%first
    end function qlinkedlist_iterator

    subroutine qlinkedlist_finalize(this)
        class(QLinkedList), intent(inout) :: this
        if (associated(this%d)) then
            call this%d%deref()
            if (this%d%ref == 0) then
                call this%clear()
                deallocate(this%d)
            end if
            this%d => null()
        end if
    end subroutine qlinkedlist_finalize

    ! ========== QLinkedListIterator Implementation ==========

    function qlinkedlist_iterator_has_next(this) result(has)
        class(QLinkedListIterator), intent(in) :: this
        logical :: has
        has = associated(this%current)
    end function qlinkedlist_iterator_has_next

    subroutine qlinkedlist_iterator_next(this)
        class(QLinkedListIterator), intent(inout) :: this
        if (associated(this%current)) then
            this%current => this%current%next
        end if
    end subroutine qlinkedlist_iterator_next

    function qlinkedlist_iterator_value(this) result(val)
        class(QLinkedListIterator), intent(in) :: this
        integer(int32) :: val
        if (associated(this%current)) then
            val = this%current%value
        else
            val = 0
        end if
    end function qlinkedlist_iterator_value

    ! ========== QHash Implementation ==========

    subroutine qhash_init(this)
        class(QHash), intent(inout) :: this
        if (.not. associated(this%d)) then
            allocate(this%d)
            allocate(this%d%buckets(INITIAL_BUCKET_COUNT))
            this%d%bucket_count = INITIAL_BUCKET_COUNT
        end if
    end subroutine qhash_init

    subroutine qhash_detach(this)
        class(QHash), intent(inout) :: this
        type(QHashData), pointer :: new_d
        type(QHashNode), pointer :: node, new_node
        integer :: i

        if (.not. associated(this%d)) then
            call this%init()
            return
        end if

        if (this%d%ref > 1) then
            allocate(new_d)
            allocate(new_d%buckets(this%d%bucket_count))
            new_d%bucket_count = this%d%bucket_count
            new_d%count = this%d%count
            new_d%ref = 1

            ! Deep copy the hash table
            do i = 1, this%d%bucket_count
                node => this%d%buckets(i)%next
                do while (associated(node))
                    allocate(new_node)
                    new_node%key = node%key
                    new_node%value = node%value
                    new_node%next => new_d%buckets(i)%next
                    new_d%buckets(i)%next => new_node
                    node => node%next
                end do
            end do

            call this%d%deref()
            this%d => new_d
        end if
    end subroutine qhash_detach

    function qhash_hash(key) result(hash_val)
        character(len=*), intent(in) :: key
        integer :: hash_val
        integer :: i

        hash_val = 0
        do i = 1, len(key)
            hash_val = hash_val + ichar(key(i:i))
        end do
    end function qhash_hash

    subroutine qhash_insert(this, key, value)
        class(QHash), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node

        call this%detach()

        hash_val = qhash_hash(key)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        ! Check if key exists
        node => this%d%buckets(bucket_index)%next
        do while (associated(node))
            if (node%key == key) then
                node%value = value
                return
            end if
            node => node%next
        end do

        ! Insert new node
        allocate(node)
        node%key = key
        node%value = value
        node%next => this%d%buckets(bucket_index)%next
        this%d%buckets(bucket_index)%next => node
        this%d%count = this%d%count + 1
    end subroutine qhash_insert

    function qhash_get(this, key, found) result(value)
        class(QHash), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node

        value = 0
        if (present(found)) found = .false.

        if (.not. associated(this%d)) return

        hash_val = qhash_hash(key)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        node => this%d%buckets(bucket_index)%next
        do while (associated(node))
            if (node%key == key) then
                value = node%value
                if (present(found)) found = .true.
                return
            end if
            node => node%next
        end do
    end function qhash_get

    function qhash_contains(this, key) result(found)
        class(QHash), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: found
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node

        found = .false.
        if (.not. associated(this%d)) return

        hash_val = qhash_hash(key)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        node => this%d%buckets(bucket_index)%next
        do while (associated(node))
            if (node%key == key) then
                found = .true.
                return
            end if
            node => node%next
        end do
    end function qhash_contains

    subroutine qhash_remove(this, key)
        class(QHash), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node, prev

        if (.not. associated(this%d)) return

        call this%detach()

        hash_val = qhash_hash(key)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        prev => this%d%buckets(bucket_index)
        node => prev%next
        do while (associated(node))
            if (node%key == key) then
                prev%next => node%next
                deallocate(node)
                this%d%count = this%d%count - 1
                return
            end if
            prev => node
            node => node%next
        end do
    end subroutine qhash_remove

    function qhash_size(this) result(size_val)
        class(QHash), intent(in) :: this
        integer :: size_val
        if (associated(this%d)) then
            size_val = this%d%count
        else
            size_val = 0
        end if
    end function qhash_size

    function qhash_is_empty(this) result(empty)
        class(QHash), intent(in) :: this
        logical :: empty
        empty = (this%size() == 0)
    end function qhash_is_empty

    subroutine qhash_clear(this)
        class(QHash), intent(inout) :: this
        type(QHashNode), pointer :: node, next_node
        integer :: i

        call this%detach()

        do i = 1, this%d%bucket_count
            node => this%d%buckets(i)%next
            do while (associated(node))
                next_node => node%next
                deallocate(node)
                node => next_node
            end do
            this%d%buckets(i)%next => null()
        end do

        this%d%count = 0
    end subroutine qhash_clear

    function qhash_keys(this) result(keys_list)
        class(QHash), intent(in) :: this
        type(QVector) :: keys_list
        type(QHashNode), pointer :: node
        integer :: i

        do i = 1, this%d%bucket_count
            node => this%d%buckets(i)%next
            do while (associated(node))
                ! Note: This is simplified - in real implementation would need string vector
                ! For now, just count
                node => node%next
            end do
        end do
    end function qhash_keys

    function qhash_values(this) result(values_list)
        class(QHash), intent(in) :: this
        type(QVector) :: values_list
        type(QHashNode), pointer :: node
        integer :: i

        call values_list%init()
        do i = 1, this%d%bucket_count
            node => this%d%buckets(i)%next
            do while (associated(node))
                call values_list%append(node%value)
                node => node%next
            end do
        end do
    end function qhash_values

    function qhash_iterator(this) result(iter)
        class(QHash), intent(in) :: this
        type(QHashIterator) :: iter
        iter%container => this
        iter%bucket_index = 1
        if (associated(this%d)) then
            iter%current => this%d%buckets(1)%next
        end if
    end function qhash_iterator

    subroutine qhash_finalize(this)
        class(QHash), intent(inout) :: this
        if (associated(this%d)) then
            call this%d%deref()
            if (this%d%ref == 0) then
                call this%clear()
                deallocate(this%d%buckets)
                deallocate(this%d)
            end if
            this%d => null()
        end if
    end subroutine qhash_finalize

    ! ========== QHashIterator Implementation ==========

    function qhash_iterator_has_next(this) result(has)
        class(QHashIterator), intent(in) :: this
        logical :: has
        has = associated(this%current)
    end function qhash_iterator_has_next

    subroutine qhash_iterator_next(this)
        class(QHashIterator), intent(inout) :: this
        if (associated(this%current)) then
            this%current => this%current%next
        end if

        ! Move to next bucket if needed
        do while (.not. associated(this%current) .and. this%bucket_index < this%container%d%bucket_count)
            this%bucket_index = this%bucket_index + 1
            this%current => this%container%d%buckets(this%bucket_index)%next
        end do
    end subroutine qhash_iterator_next

    function qhash_iterator_key(this) result(key)
        class(QHashIterator), intent(in) :: this
        character(len=:), allocatable :: key
        if (associated(this%current)) then
            key = this%current%key
        else
            key = ""
        end if
    end function qhash_iterator_key

    function qhash_iterator_value(this) result(val)
        class(QHashIterator), intent(in) :: this
        integer(int32) :: val
        if (associated(this%current)) then
            val = this%current%value
        else
            val = 0
        end if
    end function qhash_iterator_value

    ! ========== QSet Implementation ==========

    subroutine qset_init(this)
        class(QSet), intent(inout) :: this
        if (.not. associated(this%d)) then
            allocate(this%d)
            allocate(this%d%buckets(INITIAL_BUCKET_COUNT))
            this%d%bucket_count = INITIAL_BUCKET_COUNT
        end if
    end subroutine qset_init

    subroutine qset_detach(this)
        class(QSet), intent(inout) :: this
        type(QSetData), pointer :: new_d
        type(QHashNode), pointer :: node, new_node
        integer :: i

        if (.not. associated(this%d)) then
            call this%init()
            return
        end if

        if (this%d%ref > 1) then
            allocate(new_d)
            allocate(new_d%buckets(this%d%bucket_count))
            new_d%bucket_count = this%d%bucket_count
            new_d%count = this%d%count
            new_d%ref = 1

            ! Deep copy the hash table
            do i = 1, this%d%bucket_count
                node => this%d%buckets(i)%next
                do while (associated(node))
                    allocate(new_node)
                    new_node%key = node%key
                    new_node%value = node%value
                    new_node%next => new_d%buckets(i)%next
                    new_d%buckets(i)%next => new_node
                    node => node%next
                end do
            end do

            call this%d%deref()
            this%d => new_d
        end if
    end subroutine qset_detach

    function qset_hash(value) result(hash_val)
        integer(int32), intent(in) :: value
        integer :: hash_val
        hash_val = abs(value)
    end function qset_hash

    subroutine qset_insert(this, value)
        class(QSet), intent(inout) :: this
        integer(int32), intent(in) :: value
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node
        character(len=20) :: key_str

        call this%detach()

        write(key_str, *) value
        hash_val = qset_hash(value)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        ! Check if value exists
        node => this%d%buckets(bucket_index)%next
        do while (associated(node))
            if (node%value == value) then
                return
            end if
            node => node%next
        end do

        ! Insert new node
        allocate(node)
        node%key = trim(key_str)
        node%value = value
        node%next => this%d%buckets(bucket_index)%next
        this%d%buckets(bucket_index)%next => node
        this%d%count = this%d%count + 1
    end subroutine qset_insert

    function qset_contains(this, value) result(found)
        class(QSet), intent(in) :: this
        integer(int32), intent(in) :: value
        logical :: found
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node

        found = .false.
        if (.not. associated(this%d)) return

        hash_val = qset_hash(value)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        node => this%d%buckets(bucket_index)%next
        do while (associated(node))
            if (node%value == value) then
                found = .true.
                return
            end if
            node => node%next
        end do
    end function qset_contains

    subroutine qset_remove(this, value)
        class(QSet), intent(inout) :: this
        integer(int32), intent(in) :: value
        integer :: hash_val, bucket_index
        type(QHashNode), pointer :: node, prev

        if (.not. associated(this%d)) return

        call this%detach()

        hash_val = qset_hash(value)
        bucket_index = mod(hash_val, this%d%bucket_count) + 1

        prev => this%d%buckets(bucket_index)
        node => prev%next
        do while (associated(node))
            if (node%value == value) then
                prev%next => node%next
                deallocate(node)
                this%d%count = this%d%count - 1
                return
            end if
            prev => node
            node => node%next
        end do
    end subroutine qset_remove

    function qset_size(this) result(size_val)
        class(QSet), intent(in) :: this
        integer :: size_val
        if (associated(this%d)) then
            size_val = this%d%count
        else
            size_val = 0
        end if
    end function qset_size

    function qset_is_empty(this) result(empty)
    ! ========== Foreach Loop Support ==========

    !> @brief Foreach loop support for QVector
    !> @param container The QVector to iterate over
    !> @param iterator The iterator variable
    !> @param code The code block to execute for each element
    !> Usage: Q_FOREACH_VECTOR(vec, iter, print *, iter%value())
    ! Note: Fortran doesn't support macros like C++, so this is conceptual

    !> @brief Foreach loop support for QLinkedList
    !> @param container The QLinkedList to iterate over
    !> @param iterator The iterator variable
    !> @param code The code block to execute for each element
    ! Note: Fortran doesn't support macros like C++, so this is conceptual

    !> @brief Foreach loop support for QHash
    !> @param container The QHash to iterate over
    !> @param iterator The iterator variable
    !> @param code The code block to execute for each element
    ! Note: Fortran doesn't support macros like C++, so this is conceptual

    !> @brief Foreach loop support for QSet
    !> @param container The QSet to iterate over
    !> @param iterator The iterator variable
    !> @param code The code block to execute for each element
    ! Note: Fortran doesn't support macros like C++, so this is conceptual

    ! ========== Thread-Safe Variants ==========

    !> @brief Thread-safe QVector with mutex protection
    !> Note: In a real implementation, this would include mutex/lock
    !> For Fortran, we would need to use threading libraries like OpenMP or coarrays
    type :: QThreadSafeVector
        type(QVector) :: vector
        ! integer :: mutex ! Would be added with threading library
    contains
        procedure :: append => qthreadsafevector_append
        procedure :: at => qthreadsafevector_at
        procedure :: size => qthreadsafevector_size
    end type QThreadSafeVector

    !> @brief Thread-safe QHash with mutex protection
    !> Note: In a real implementation, this would include mutex/lock
    type :: QThreadSafeHash
        type(QHash) :: hash
        ! integer :: mutex ! Would be added with threading library
    contains
        procedure :: insert => qthreadsafehash_insert
        procedure :: get => qthreadsafehash_get
        procedure :: size => qthreadsafehash_size
    end type QThreadSafeHash

    ! ========== STL-Style Algorithms ==========

    !> @brief Find element in QVector
    function qfind_vector(container, value) result(iter)
        type(QVector), intent(in) :: container
        integer(int32), intent(in) :: value
        type(QVectorIterator) :: iter
        integer :: i

        iter = container%iterator()
        do i = 1, container%size()
            if (iter%value() == value) return
            call iter%next()
        end do
        ! Return end iterator if not found
        iter%index = container%size() + 1
    end function qfind_vector

    !> @brief Sort QVector (simple bubble sort)
    subroutine qsort_vector(container)
        type(QVector), intent(inout) :: container
        integer :: i, j, temp

        call container%detach()
        do i = 1, container%size() - 1
            do j = i + 1, container%size()
                if (container%at(i) > container%at(j)) then
                    temp = container%at(i)
                    call container%set(i, container%at(j))
                    call container%set(j, temp)
                end if
            end do
        end do
    end subroutine qsort_vector

    !> @brief Count occurrences in QVector
    function qcount_vector(container, value) result(count)
        type(QVector), intent(in) :: container
        integer(int32), intent(in) :: value
        integer :: count
        type(QVectorIterator) :: iter

        count = 0
        iter = container%iterator()
        do while (iter%has_next())
            if (iter%value() == value) count = count + 1
            call iter%next()
        end do
    end function qcount_vector

    !> @brief Transform QVector elements (simplified - no function pointers in Fortran)
    subroutine qtransform_vector(container, multiplier)
        type(QVector), intent(inout) :: container
        integer(int32), intent(in) :: multiplier
        type(QVectorIterator) :: iter
        integer :: i

        call container%detach()
        iter = container%iterator()
        i = 1
        do while (iter%has_next())
            call container%set(i, iter%value() * multiplier)
            call iter%next()
            i = i + 1
        end do
    end subroutine qtransform_vector

    ! ========== Thread-Safe Implementations ==========

    subroutine qthreadsafevector_append(this, item)
        class(QThreadSafeVector), intent(inout) :: this
        integer(int32), intent(in) :: item
        ! In real implementation: lock mutex
        call this%vector%append(item)
        ! unlock mutex
    end subroutine qthreadsafevector_append

    function qthreadsafevector_at(this, index) result(item)
        class(QThreadSafeVector), intent(in) :: this
        integer, intent(in) :: index
        integer(int32) :: item
        ! In real implementation: lock mutex
        item = this%vector%at(index)
        ! unlock mutex
    end function qthreadsafevector_at

    function qthreadsafevector_size(this) result(size_val)
        class(QThreadSafeVector), intent(in) :: this
        integer :: size_val
        ! In real implementation: lock mutex
        size_val = this%vector%size()
        ! unlock mutex
    end function qthreadsafevector_size

    subroutine qthreadsafehash_insert(this, key, value)
        class(QThreadSafeHash), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        ! In real implementation: lock mutex
        call this%hash%insert(key, value)
        ! unlock mutex
    end subroutine qthreadsafehash_insert

    function qthreadsafehash_get(this, key, found) result(value)
        class(QThreadSafeHash), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        ! In real implementation: lock mutex
        value = this%hash%get(key, found)
        ! unlock mutex
    end function qthreadsafehash_get

    function qthreadsafehash_size(this) result(size_val)
        class(QThreadSafeHash), intent(in) :: this
        integer :: size_val
        ! In real implementation: lock mutex
        size_val = this%hash%size()
        ! unlock mutex
    end function qthreadsafehash_size

    ! ========== Additional Utility Functions ==========

    !> @brief Create QVector from array
    function qvector_from_array(arr) result(vec)
        integer(int32), intent(in) :: arr(:)
        type(QVector) :: vec
        integer :: i

        call vec%init()
        do i = 1, size(arr)
            call vec%append(arr(i))
        end do
    end function qvector_from_array

    !> @brief Convert QVector to array
    function qvector_to_array(vec) result(arr)
        type(QVector), intent(in) :: vec
        integer(int32), allocatable :: arr(:)
        integer :: i

        allocate(arr(vec%size()))
        do i = 1, vec%size()
            arr(i) = vec%at(i)
        end do
    end function qvector_to_array

    !> @brief Create QSet from array
    function qset_from_array(arr) result(set)
        integer(int32), intent(in) :: arr(:)
        type(QSet) :: set
        integer :: i

        call set%init()
        do i = 1, size(arr)
            call set%insert(arr(i))
        end do
    end function qset_from_array

    !> @brief Check if two QVectors are equal
    function qvector_equals(a, b) result(equal)
        type(QVector), intent(in) :: a, b
        logical :: equal
        integer :: i

        equal = .false.
        if (a%size() /= b%size()) return

        do i = 1, a%size()
            if (a%at(i) /= b%at(i)) return
        end do
        equal = .true.
    end function qvector_equals

    !> @brief Check if two QSets are equal
    function qset_equals(a, b) result(equal)
        type(QSet), intent(in) :: a, b
        logical :: equal
        type(QSetIterator) :: iter

        equal = .false.
        if (a%size() /= b%size()) return

        iter = a%iterator()
        do while (iter%has_next())
            if (.not. b%contains(iter%value())) return
            call iter%next()
        end do
        equal = .true.
    end function qset_equals

end module forge_containers
        class(QSet), intent(in) :: this
        logical :: empty
        empty = (this%size() == 0)
    end function qset_is_empty

    subroutine qset_clear(this)
        class(QSet), intent(inout) :: this
        type(QHashNode), pointer :: node, next_node
        integer :: i

        call this%detach()

        do i = 1, this%d%bucket_count
            node => this%d%buckets(i)%next
            do while (associated(node))
                next_node => node%next
                deallocate(node)
                node => next_node
            end do
            this%d%buckets(i)%next => null()
        end do

        this%d%count = 0
    end subroutine qset_clear

    function qset_iterator(this) result(iter)
        class(QSet), intent(in) :: this
        type(QSetIterator) :: iter
        iter%container => this
        iter%bucket_index = 1
        if (associated(this%d)) then
            iter%current => this%d%buckets(1)%next
        end if
    end function qset_iterator

    subroutine qset_unite(this, other)
        class(QSet), intent(inout) :: this
        type(QSet), intent(in) :: other
        type(QSetIterator) :: iter

        iter = other%iterator()
        do while (iter%has_next())
            call this%insert(iter%value())
            call iter%next()
        end do
    end subroutine qset_unite

    subroutine qset_intersect(this, other)
        class(QSet), intent(inout) :: this
        type(QSet), intent(in) :: other
        type(QSet) :: temp_set
        type(QSetIterator) :: iter

        call temp_set%init()
        iter = this%iterator()
        do while (iter%has_next())
            if (other%contains(iter%value())) then
                call temp_set%insert(iter%value())
            end if
            call iter%next()
        end do

        this = temp_set
    end subroutine qset_intersect

    subroutine qset_subtract(this, other)
        class(QSet), intent(inout) :: this
        type(QSet), intent(in) :: other
        type(QSetIterator) :: iter

        iter = other%iterator()
        do while (iter%has_next())
            call this%remove(iter%value())
            call iter%next()
        end do
    end subroutine qset_subtract

    subroutine qset_finalize(this)
        class(QSet), intent(inout) :: this
        if (associated(this%d)) then
            call this%d%deref()
            if (this%d%ref == 0) then
                call this%clear()
                deallocate(this%d%buckets)
                deallocate(this%d)
            end if
            this%d => null()
        end if
    end subroutine qset_finalize

    ! ========== QSetIterator Implementation ==========

    function qset_iterator_has_next(this) result(has)
        class(QSetIterator), intent(in) :: this
        logical :: has
        has = associated(this%current)
    end function qset_iterator_has_next

    subroutine qset_iterator_next(this)
        class(QSetIterator), intent(inout) :: this
        if (associated(this%current)) then
            this%current => this%current%next
        end if

        ! Move to next bucket if needed
        do while (.not. associated(this%current) .and. this%bucket_index < this%container%d%bucket_count)
            this%bucket_index = this%bucket_index + 1
            this%current => this%container%d%buckets(this%bucket_index)%next
        end do
    end subroutine qset_iterator_next

    function qset_iterator_value(this) result(val)
        class(QSetIterator), intent(in) :: this
        integer(int32) :: val
        if (associated(this%current)) then
            val = this%current%value
        else
            val = 0
        end if
    end function qset_iterator_value

end module forge_containers