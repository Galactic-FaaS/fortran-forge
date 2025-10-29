!> @brief QObject-like base class implementation for ForGE
!> @details Implements Qt-style object model with parent-child relationships,
!> automatic memory management, dynamic properties, and meta-object system
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qobject
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: forge_qobject
    public :: forge_qobject_ptr
    public :: forge_property
    public :: forge_meta_object
    public :: forge_event
    public :: QObjectEvent
    public :: QObjectChildEvent
    public :: QObjectTimerEvent
    public :: QEvent_Type

    !> Event types (Qt-style)
    enum, bind(c)
        enumerator :: QEvent_None = 0
        enumerator :: QEvent_Timer = 1
        enumerator :: QEvent_MouseButtonPress = 2
        enumerator :: QEvent_MouseButtonRelease = 3
        enumerator :: QEvent_MouseButtonDblClick = 4
        enumerator :: QEvent_MouseMove = 5
        enumerator :: QEvent_KeyPress = 6
        enumerator :: QEvent_KeyRelease = 7
        enumerator :: QEvent_FocusIn = 8
        enumerator :: QEvent_FocusOut = 9
        enumerator :: QEvent_Enter = 10
        enumerator :: QEvent_Leave = 11
        enumerator :: QEvent_Paint = 12
        enumerator :: QEvent_Move = 13
        enumerator :: QEvent_Resize = 14
        enumerator :: QEvent_Show = 17
        enumerator :: QEvent_Hide = 18
        enumerator :: QEvent_Close = 19
        enumerator :: QEvent_ParentChange = 21
        enumerator :: QEvent_ChildAdded = 68
        enumerator :: QEvent_ChildRemoved = 69
        enumerator :: QEvent_User = 1000
    end enum

    !> @brief Event type enumeration
    type :: QEvent_Type
        integer :: value
    end type QEvent_Type

    !> @brief Base event class
    type :: forge_event
        private
        integer :: event_type = QEvent_None
        logical :: accepted = .true.
        logical :: spontaneous = .false.
    contains
        procedure :: type => event_type
        procedure :: set_type => event_set_type
        procedure :: is_accepted => event_is_accepted
        procedure :: set_accepted => event_set_accepted
        procedure :: is_spontaneous => event_is_spontaneous
        procedure :: set_spontaneous => event_set_spontaneous
        procedure :: clone => event_clone
        final :: event_finalize
    end type forge_event

    !> @brief QObject-specific event
    type, extends(forge_event) :: QObjectEvent
        private
        class(forge_qobject), pointer :: obj => null()
    contains
        procedure :: qobject => qobject_event_qobject
        procedure :: set_qobject => qobject_event_set_qobject
    end type QObjectEvent

    !> @brief Child event for parent-child notifications
    type, extends(QObjectEvent) :: QObjectChildEvent
        private
        logical :: added = .false.
    contains
        procedure :: child_added => child_event_added
        procedure :: set_child_added => child_event_set_added
    end type QObjectChildEvent

    !> @brief Timer event
    type, extends(forge_event) :: QObjectTimerEvent
        private
        integer :: timer_id = 0
    contains
        procedure :: timerId => timer_event_id
        procedure :: set_timer_id => timer_event_set_id
    end type QObjectTimerEvent

    !> @brief Dynamic property container
    type :: forge_property
        private
        character(len=:), allocatable :: name
        class(*), allocatable :: value
        logical :: is_valid = .false.
    contains
        procedure :: get_name => property_get_name
        procedure :: set_name => property_set_name
        procedure :: has_value => property_has_value
        procedure :: get_value_int => property_get_int
        procedure :: get_value_real => property_get_real
        procedure :: get_value_string => property_get_string
        procedure :: get_value_bool => property_get_bool
        procedure :: set_value_int => property_set_int
        procedure :: set_value_real => property_set_real
        procedure :: set_value_string => property_set_string
        procedure :: set_value_bool => property_set_bool
        procedure :: clear => property_clear
        final :: property_finalize
    end type forge_property

    !> @brief Meta-object information
    type :: forge_meta_object
        private
        character(len=:), allocatable :: class_name
        character(len=:), allocatable :: super_class_name
        integer :: property_count = 0
        integer :: method_count = 0
        integer :: signal_count = 0
        integer :: slot_count = 0
        type(forge_string), dimension(:), allocatable :: property_names
        type(forge_string), dimension(:), allocatable :: method_names
        type(forge_string), dimension(:), allocatable :: signal_names
        type(forge_string), dimension(:), allocatable :: slot_names
    contains
        procedure :: get_class_name => meta_get_class_name
        procedure :: set_class_name => meta_set_class_name
        procedure :: get_super_class_name => meta_get_super_class_name
        procedure :: set_super_class_name => meta_set_super_class_name
        procedure :: get_property_count => meta_get_property_count
        procedure :: get_method_count => meta_get_method_count
        procedure :: get_signal_count => meta_get_signal_count
        procedure :: get_slot_count => meta_get_slot_count
        procedure :: add_property => meta_add_property
        procedure :: add_method => meta_add_method
        procedure :: add_signal => meta_add_signal
        procedure :: add_slot => meta_add_slot
        procedure :: get_property_name => meta_get_property_name
        procedure :: get_method_name => meta_get_method_name
        procedure :: get_signal_name => meta_get_signal_name
        procedure :: get_slot_name => meta_get_slot_name
        final :: meta_finalize
    end type forge_meta_object

    !> @brief QObject pointer wrapper for safe referencing
    type :: forge_qobject_ptr
        private
        class(forge_qobject), pointer :: ptr => null()
    contains
        procedure :: assign => qobject_ptr_assign
        procedure :: get => qobject_ptr_get
        procedure :: is_valid => qobject_ptr_is_valid
        procedure :: clear => qobject_ptr_clear
        final :: qobject_ptr_finalize
    end type forge_qobject_ptr

    !> @brief QObject base class - Qt-style object model
    type :: forge_qobject
        private
        ! Parent-child relationships
        class(forge_qobject), pointer :: parent => null()
        type(forge_qobject_ptr), dimension(:), allocatable :: children

        ! Object identity
        integer(int64) :: object_id = 0
        type(forge_string) :: object_name

        ! Properties system
        type(forge_property), dimension(:), allocatable :: properties
        integer :: property_count = 0

        ! Meta-object system
        type(forge_meta_object) :: meta_object

        ! Thread affinity
        integer :: thread_id = 0

        ! Event handling
        logical :: event_enabled = .true.
        logical :: destroyed = .false.

        ! Signals for object lifecycle
        type(signal_void) :: destroyed_signal
        type(signal_string) :: object_name_changed_signal

    contains
        ! Constructor/destructor
        procedure :: init => qobject_init
        procedure :: cleanup => qobject_cleanup
        final :: qobject_finalize

        ! Parent-child relationships
        procedure :: set_parent => qobject_set_parent
        procedure :: get_parent => qobject_get_parent
        procedure :: add_child => qobject_add_child
        procedure :: remove_child => qobject_remove_child
        procedure :: get_children => qobject_get_children
        procedure :: find_child => qobject_find_child
        procedure :: child_count => qobject_child_count

        ! Object naming
        procedure :: set_object_name => qobject_set_object_name
        procedure :: get_object_name => qobject_get_object_name
        procedure :: get_object_id => qobject_get_object_id

        ! Properties system
        procedure :: set_property => qobject_set_property
        procedure :: get_property => qobject_get_property
        procedure :: has_property => qobject_has_property
        procedure :: remove_property => qobject_remove_property
        procedure :: get_property_names => qobject_get_property_names
        procedure :: property_count => qobject_property_count

        ! Meta-object system
        procedure :: metaObject => qobject_meta_object

        ! Thread affinity
        procedure :: thread => qobject_thread
        procedure :: move_to_thread => qobject_move_to_thread

        ! Event handling
        procedure :: event => qobject_event
        procedure :: eventFilter => qobject_event_filter
        procedure :: install_event_filter => qobject_install_event_filter
        procedure :: remove_event_filter => qobject_remove_event_filter
        procedure :: is_widget_type => qobject_is_widget_type
        procedure :: is_window_type => qobject_is_window_type

        ! Signals
        procedure :: destroyed => qobject_destroyed_signal
        procedure :: objectNameChanged => qobject_object_name_changed_signal

        ! Utility methods
        procedure :: dump_object_tree => qobject_dump_tree
        procedure :: inherits => qobject_inherits
        procedure :: is_widget => qobject_is_widget
        procedure :: is_window => qobject_is_window
        procedure :: set_object_id => qobject_set_object_id

        ! Protected methods (for subclasses)
        procedure :: child_event => qobject_child_event
        procedure :: timer_event => qobject_timer_event
        procedure :: custom_event => qobject_custom_event
    end type forge_qobject

    ! Global object ID counter
    integer(int64), private :: global_object_id = 0_int64

contains

    ! ========== forge_event Implementation ==========

    function event_type(this) result(type_val)
        class(forge_event), intent(in) :: this
        integer :: type_val
        type_val = this%event_type
    end function event_type

    subroutine event_set_type(this, type_val)
        class(forge_event), intent(inout) :: this
        integer, intent(in) :: type_val
        this%event_type = type_val
    end subroutine event_set_type

    function event_is_accepted(this) result(accepted)
        class(forge_event), intent(in) :: this
        logical :: accepted
        accepted = this%accepted
    end function event_is_accepted

    subroutine event_set_accepted(this, accepted)
        class(forge_event), intent(inout) :: this
        logical, intent(in) :: accepted
        this%accepted = accepted
    end subroutine event_set_accepted

    function event_is_spontaneous(this) result(spontaneous)
        class(forge_event), intent(in) :: this
        logical :: spontaneous
        spontaneous = this%spontaneous
    end function event_is_spontaneous

    subroutine event_set_spontaneous(this, spontaneous)
        class(forge_event), intent(inout) :: this
        logical, intent(in) :: spontaneous
        this%spontaneous = spontaneous
    end subroutine event_set_spontaneous

    function event_clone(this) result(cloned_event)
        class(forge_event), intent(in) :: this
        class(forge_event), allocatable :: cloned_event

        allocate(cloned_event, source=this)
    end function event_clone

    subroutine event_finalize(this)
        type(forge_event), intent(inout) :: this
        ! Cleanup if needed
    end subroutine event_finalize

    ! ========== QObjectEvent Implementation ==========

    function qobject_event_qobject(this) result(obj)
        class(QObjectEvent), intent(in) :: this
        class(forge_qobject), pointer :: obj
        obj => this%obj
    end function qobject_event_qobject

    subroutine qobject_event_set_qobject(this, obj)
        class(QObjectEvent), intent(inout) :: this
        class(forge_qobject), target :: obj
        this%obj => obj
    end subroutine qobject_event_set_qobject

    ! ========== QObjectChildEvent Implementation ==========

    function child_event_added(this) result(added)
        class(QObjectChildEvent), intent(in) :: this
        logical :: added
        added = this%added
    end function child_event_added

    subroutine child_event_set_added(this, added)
        class(QObjectChildEvent), intent(inout) :: this
        logical, intent(in) :: added
        this%added = added
    end subroutine child_event_set_added

    ! ========== QObjectTimerEvent Implementation ==========

    function timer_event_id(this) result(id)
        class(QObjectTimerEvent), intent(in) :: this
        integer :: id
        id = this%timer_id
    end function timer_event_id

    subroutine timer_event_set_id(this, id)
        class(QObjectTimerEvent), intent(inout) :: this
        integer, intent(in) :: id
        this%timer_id = id
    end subroutine timer_event_set_id

    ! ========== forge_property Implementation ==========

    function property_get_name(this) result(name)
        class(forge_property), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%name)) then
            name = this%name
        else
            name = ""
        end if
    end function property_get_name

    subroutine property_set_name(this, name)
        class(forge_property), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%name = name
    end subroutine property_set_name

    function property_has_value(this) result(has)
        class(forge_property), intent(in) :: this
        logical :: has
        has = allocated(this%value) .and. this%is_valid
    end function property_has_value

    function property_get_int(this, default_val) result(val)
        class(forge_property), intent(in) :: this
        integer, intent(in), optional :: default_val
        integer :: val

        if (this%has_value()) then
            select type (v => this%value)
            type is (integer)
                val = v
            class default
                if (present(default_val)) then
                    val = default_val
                else
                    val = 0
                end if
            end select
        else
            if (present(default_val)) then
                val = default_val
            else
                val = 0
            end if
        end if
    end function property_get_int

    function property_get_real(this, default_val) result(val)
        class(forge_property), intent(in) :: this
        real, intent(in), optional :: default_val
        real :: val

        if (this%has_value()) then
            select type (v => this%value)
            type is (real)
                val = v
            class default
                if (present(default_val)) then
                    val = default_val
                else
                    val = 0.0
                end if
            end select
        else
            if (present(default_val)) then
                val = default_val
            else
                val = 0.0
            end if
        end if
    end function property_get_real

    function property_get_string(this, default_val) result(val)
        class(forge_property), intent(in) :: this
        character(len=*), intent(in), optional :: default_val
        character(len=:), allocatable :: val

        if (this%has_value()) then
            select type (v => this%value)
            type is (character(len=*))
                val = v
            class default
                if (present(default_val)) then
                    val = default_val
                else
                    val = ""
                end if
            end select
        else
            if (present(default_val)) then
                val = default_val
            else
                val = ""
            end if
        end if
    end function property_get_string

    function property_get_bool(this, default_val) result(val)
        class(forge_property), intent(in) :: this
        logical, intent(in), optional :: default_val
        logical :: val

        if (this%has_value()) then
            select type (v => this%value)
            type is (logical)
                val = v
            class default
                if (present(default_val)) then
                    val = default_val
                else
                    val = .false.
                end if
            end select
        else
            if (present(default_val)) then
                val = default_val
            else
                val = .false.
            end if
        end if
    end function property_get_bool

    subroutine property_set_int(this, val)
        class(forge_property), intent(inout) :: this
        integer, intent(in) :: val
        if (allocated(this%value)) deallocate(this%value)
        allocate(this%value, source=val)
        this%is_valid = .true.
    end subroutine property_set_int

    subroutine property_set_real(this, val)
        class(forge_property), intent(inout) :: this
        real, intent(in) :: val
        if (allocated(this%value)) deallocate(this%value)
        allocate(this%value, source=val)
        this%is_valid = .true.
    end subroutine property_set_real

    subroutine property_set_string(this, val)
        class(forge_property), intent(inout) :: this
        character(len=*), intent(in) :: val
        if (allocated(this%value)) deallocate(this%value)
        allocate(this%value, source=val)
        this%is_valid = .true.
    end subroutine property_set_string

    subroutine property_set_bool(this, val)
        class(forge_property), intent(inout) :: this
        logical, intent(in) :: val
        if (allocated(this%value)) deallocate(this%value)
        allocate(this%value, source=val)
        this%is_valid = .true.
    end subroutine property_set_bool

    subroutine property_clear(this)
        class(forge_property), intent(inout) :: this
        if (allocated(this%value)) deallocate(this%value)
        this%is_valid = .false.
    end subroutine property_clear

    subroutine property_finalize(this)
        type(forge_property), intent(inout) :: this
        call this%clear()
    end subroutine property_finalize

    ! ========== forge_meta_object Implementation ==========

    function meta_get_class_name(this) result(name)
        class(forge_meta_object), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%class_name)) then
            name = this%class_name
        else
            name = ""
        end if
    end function meta_get_class_name

    subroutine meta_set_class_name(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%class_name = name
    end subroutine meta_set_class_name

    function meta_get_super_class_name(this) result(name)
        class(forge_meta_object), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%super_class_name)) then
            name = this%super_class_name
        else
            name = ""
        end if
    end function meta_get_super_class_name

    subroutine meta_set_super_class_name(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%super_class_name = name
    end subroutine meta_set_super_class_name

    function meta_get_property_count(this) result(count)
        class(forge_meta_object), intent(in) :: this
        integer :: count
        count = this%property_count
    end function meta_get_property_count

    function meta_get_method_count(this) result(count)
        class(forge_meta_object), intent(in) :: this
        integer :: count
        count = this%method_count
    end function meta_get_method_count

    function meta_get_signal_count(this) result(count)
        class(forge_meta_object), intent(in) :: this
        integer :: count
        count = this%signal_count
    end function meta_get_signal_count

    function meta_get_slot_count(this) result(count)
        class(forge_meta_object), intent(in) :: this
        integer :: count
        count = this%slot_count
    end function meta_get_slot_count

    subroutine meta_add_property(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%property_names)) then
            allocate(this%property_names(1))
        else
            allocate(temp(size(this%property_names) + 1))
            temp(1:size(this%property_names)) = this%property_names
            call move_alloc(temp, this%property_names)
        end if

        call this%property_names(this%property_count + 1)%set(name)
        this%property_count = this%property_count + 1
    end subroutine meta_add_property

    subroutine meta_add_method(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%method_names)) then
            allocate(this%method_names(1))
        else
            allocate(temp(size(this%method_names) + 1))
            temp(1:size(this%method_names)) = this%method_names
            call move_alloc(temp, this%method_names)
        end if

        call this%method_names(this%method_count + 1)%set(name)
        this%method_count = this%method_count + 1
    end subroutine meta_add_method

    subroutine meta_add_signal(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%signal_names)) then
            allocate(this%signal_names(1))
        else
            allocate(temp(size(this%signal_names) + 1))
            temp(1:size(this%signal_names)) = this%signal_names
            call move_alloc(temp, this%signal_names)
        end if

        call this%signal_names(this%signal_count + 1)%set(name)
        this%signal_count = this%signal_count + 1
    end subroutine meta_add_signal

    subroutine meta_add_slot(this, name)
        class(forge_meta_object), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%slot_names)) then
            allocate(this%slot_names(1))
        else
            allocate(temp(size(this%slot_names) + 1))
            temp(1:size(this%slot_names)) = this%slot_names
            call move_alloc(temp, this%slot_names)
        end if

        call this%slot_names(this%slot_count + 1)%set(name)
        this%slot_count = this%slot_count + 1
    end subroutine meta_add_slot

    function meta_get_property_name(this, index) result(name)
        class(forge_meta_object), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        if (index >= 1 .and. index <= this%property_count) then
            name = this%property_names(index)%get()
        else
            name = ""
        end if
    end function meta_get_property_name

    function meta_get_method_name(this, index) result(name)
        class(forge_meta_object), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        if (index >= 1 .and. index <= this%method_count) then
            name = this%method_names(index)%get()
        else
            name = ""
        end if
    end function meta_get_method_name

    function meta_get_signal_name(this, index) result(name)
        class(forge_meta_object), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        if (index >= 1 .and. index <= this%signal_count) then
            name = this%signal_names(index)%get()
        else
            name = ""
        end if
    end function meta_get_signal_name

    function meta_get_slot_name(this, index) result(name)
        class(forge_meta_object), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        if (index >= 1 .and. index <= this%slot_count) then
            name = this%slot_names(index)%get()
        else
            name = ""
        end if
    end function meta_get_slot_name

    subroutine meta_finalize(this)
        type(forge_meta_object), intent(inout) :: this
        if (allocated(this%property_names)) deallocate(this%property_names)
        if (allocated(this%method_names)) deallocate(this%method_names)
        if (allocated(this%signal_names)) deallocate(this%signal_names)
        if (allocated(this%slot_names)) deallocate(this%slot_names)
    end subroutine meta_finalize

    ! ========== forge_qobject_ptr Implementation ==========

    subroutine qobject_ptr_assign(this, obj)
        class(forge_qobject_ptr), intent(inout) :: this
        class(forge_qobject), target, intent(in) :: obj
        this%ptr => obj
    end subroutine qobject_ptr_assign

    function qobject_ptr_get(this) result(obj)
        class(forge_qobject_ptr), intent(in) :: this
        class(forge_qobject), pointer :: obj
        obj => this%ptr
    end function qobject_ptr_get

    function qobject_ptr_is_valid(this) result(valid)
        class(forge_qobject_ptr), intent(in) :: this
        logical :: valid
        valid = associated(this%ptr)
    end function qobject_ptr_is_valid

    subroutine qobject_ptr_clear(this)
        class(forge_qobject_ptr), intent(inout) :: this
        this%ptr => null()
    end subroutine qobject_ptr_clear

    subroutine qobject_ptr_finalize(this)
        type(forge_qobject_ptr), intent(inout) :: this
        call this%clear()
    end subroutine qobject_ptr_finalize

    ! ========== forge_qobject Implementation ==========

    subroutine qobject_init(this, parent)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), optional, target :: parent

        ! Assign unique object ID
        global_object_id = global_object_id + 1
        this%object_id = global_object_id

        ! Set parent if provided
        if (present(parent)) then
            call this%set_parent(parent)
        end if

        ! Initialize meta-object
        call this%meta_object%set_class_name("QObject")
        call this%meta_object%set_super_class_name("")

        ! Initialize properties array
        allocate(this%properties(10))  ! Start with capacity of 10

        ! Initialize children array
        allocate(this%children(10))  ! Start with capacity of 10

        ! Set default thread affinity (main thread)
        this%thread_id = 0
    end subroutine qobject_init

    subroutine qobject_cleanup(this)
        class(forge_qobject), intent(inout) :: this
        integer :: i

        ! Prevent double cleanup
        if (this%destroyed) return
        this%destroyed = .true.

        ! Emit destroyed signal before cleanup
        call this%destroyed_signal%emit()

        ! Remove from parent
        if (associated(this%parent)) then
            call this%parent%remove_child(this)
        end if

        ! Delete all children (this will recursively clean up)
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) then
                call this%children(i)%get()%cleanup()
            end if
        end do

        ! Clear children array
        if (allocated(this%children)) deallocate(this%children)

        ! Clear properties
        if (allocated(this%properties)) then
            do i = 1, this%property_count
                call this%properties(i)%clear()
            end do
            deallocate(this%properties)
        end if

        ! Disconnect all signals
        call this%destroyed_signal%disconnect_all()
        call this%object_name_changed_signal%disconnect_all()
    end subroutine qobject_cleanup

    subroutine qobject_finalize(this)
        type(forge_qobject), intent(inout) :: this
        call this%cleanup()
    end subroutine qobject_finalize

    subroutine qobject_set_parent(this, parent)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), target :: parent
        type(QObjectChildEvent) :: child_event

        ! Remove from current parent
        if (associated(this%parent)) then
            if (.not. associated(this%parent, parent)) then
                call this%parent%remove_child(this)
            end if
        end if

        ! Set new parent
        this%parent => parent

        ! Add to new parent's children
        call parent%add_child(this)

        ! Emit child event
        call child_event%set_type(QEvent_ChildAdded)
        call child_event%set_qobject(this)
        call child_event%set_child_added(.true.)
        call parent%event(child_event)
    end subroutine qobject_set_parent

    function qobject_get_parent(this) result(parent)
        class(forge_qobject), intent(in) :: this
        class(forge_qobject), pointer :: parent
        parent => this%parent
    end function qobject_get_parent

    subroutine qobject_add_child(this, child)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), target :: child
        type(forge_qobject_ptr), dimension(:), allocatable :: temp
        integer :: i, new_size

        ! Check if child is already in the list
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) then
                if (associated(this%children(i)%get(), child)) return
            end if
        end do

        ! Find first empty slot or resize array
        do i = 1, size(this%children)
            if (.not. this%children(i)%is_valid()) then
                call this%children(i)%assign(child)
                return
            end if
        end do

        ! Resize array
        new_size = size(this%children) * 2
        allocate(temp(new_size))
        temp(1:size(this%children)) = this%children
        call move_alloc(temp, this%children)

        ! Add child to new slot
        call this%children(size(this%children) - size(this%children)/2 + 1)%assign(child)
    end subroutine qobject_add_child

    subroutine qobject_remove_child(this, child)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), target :: child
        integer :: i
        type(QObjectChildEvent) :: child_event

        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) then
                if (associated(this%children(i)%get(), child)) then
                    ! Emit child removed event
                    call child_event%set_type(QEvent_ChildRemoved)
                    call child_event%set_qobject(child)
                    call child_event%set_child_added(.false.)
                    call this%event(child_event)

                    ! Clear the pointer
                    call this%children(i)%clear()
                    exit
                end if
            end if
        end do
    end subroutine qobject_remove_child

    function qobject_get_children(this) result(children_list)
        class(forge_qobject), intent(in) :: this
        type(forge_qobject_ptr), dimension(:), allocatable :: children_list
        integer :: count, i, j

        ! Count valid children
        count = 0
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) count = count + 1
        end do

        ! Allocate result array
        allocate(children_list(count))

        ! Fill result array
        j = 1
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) then
                children_list(j) = this%children(i)
                j = j + 1
            end if
        end do
    end function qobject_get_children

    function qobject_find_child(this, name) result(child)
        class(forge_qobject), intent(in) :: this
        character(len=*), intent(in) :: name
        class(forge_qobject), pointer :: child
        integer :: i

        child => null()
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) then
                if (this%children(i)%get()%get_object_name() == name) then
                    child => this%children(i)%get()
                    exit
                end if
            end if
        end do
    end function qobject_find_child

    function qobject_child_count(this) result(count)
        class(forge_qobject), intent(in) :: this
        integer :: count
        integer :: i

        count = 0
        do i = 1, size(this%children)
            if (this%children(i)%is_valid()) count = count + 1
        end do
    end function qobject_child_count

    subroutine qobject_set_object_name(this, name)
        class(forge_qobject), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: old_name

        old_name = this%get_object_name()
        call this%object_name%set(name)

        ! Emit signal if name changed
        if (old_name /= name) then
            call this%object_name_changed_signal%emit(name)
        end if
    end subroutine qobject_set_object_name

    function qobject_get_object_name(this) result(name)
        class(forge_qobject), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%object_name%get()
    end function qobject_get_object_name

    function qobject_get_object_id(this) result(id)
        class(forge_qobject), intent(in) :: this
        integer(int64) :: id
        id = this%object_id
    end function qobject_get_object_id

    subroutine qobject_set_object_id(this, id)
        class(forge_qobject), intent(inout) :: this
        integer(int64), intent(in) :: id
        this%object_id = id
    end subroutine qobject_set_object_id

    subroutine qobject_set_property(this, name, value)
        class(forge_qobject), intent(inout) :: this
        character(len=*), intent(in) :: name
        class(*), intent(in) :: value
        integer :: i, empty_slot
        type(forge_property), dimension(:), allocatable :: temp

        ! Find existing property or empty slot
        empty_slot = -1
        do i = 1, this%property_count
            if (this%properties(i)%get_name() == name) then
                ! Update existing property
                select type (value)
                type is (integer)
                    call this%properties(i)%set_value_int(value)
                type is (real)
                    call this%properties(i)%set_value_real(value)
                type is (character(len=*))
                    call this%properties(i)%set_value_string(value)
                type is (logical)
                    call this%properties(i)%set_value_bool(value)
                end select
                return
            end if
            if (.not. this%properties(i)%has_value() .and. empty_slot == -1) then
                empty_slot = i
            end if
        end do

        ! Use empty slot or add new property
        if (empty_slot /= -1) then
            call this%properties(empty_slot)%set_name(name)
            select type (value)
            type is (integer)
                call this%properties(empty_slot)%set_value_int(value)
            type is (real)
                call this%properties(empty_slot)%set_value_real(value)
            type is (character(len=*))
                call this%properties(empty_slot)%set_value_string(value)
            type is (logical)
                call this%properties(empty_slot)%set_value_bool(value)
            end select
        else
            ! Check if we need to resize properties array
            if (this%property_count >= size(this%properties)) then
                allocate(temp(size(this%properties) * 2))
                temp(1:size(this%properties)) = this%properties
                call move_alloc(temp, this%properties)
            end if

            this%property_count = this%property_count + 1
            call this%properties(this%property_count)%set_name(name)
            select type (value)
            type is (integer)
                call this%properties(this%property_count)%set_value_int(value)
            type is (real)
                call this%properties(this%property_count)%set_value_real(value)
            type is (character(len=*))
                call this%properties(this%property_count)%set_value_string(value)
            type is (logical)
                call this%properties(this%property_count)%set_value_bool(value)
            end select
        end if
    end subroutine qobject_set_property

    function qobject_get_property(this, name, default_val) result(value)
        class(forge_qobject), intent(in) :: this
        character(len=*), intent(in) :: name
        class(*), optional, intent(in) :: default_val
        class(*), allocatable :: value
        integer :: i

        do i = 1, this%property_count
            if (this%properties(i)%get_name() == name .and. this%properties(i)%has_value()) then
                select type (v => this%properties(i)%value)
                type is (integer)
                    allocate(value, source=v)
                type is (real)
                    allocate(value, source=v)
                type is (character(len=*))
                    allocate(value, source=v)
                type is (logical)
                    allocate(value, source=v)
                end select
                return
            end if
        end do

        ! Return default value if property not found
        if (present(default_val)) then
            allocate(value, source=default_val)
        else
            allocate(value, source=0)  ! Default to integer 0
        end if
    end function qobject_get_property

    function qobject_has_property(this, name) result(has)
        class(forge_qobject), intent(in) :: this
        character(len=*), intent(in) :: name
        logical :: has
        integer :: i

        has = .false.
        do i = 1, this%property_count
            if (this%properties(i)%get_name() == name .and. this%properties(i)%has_value()) then
                has = .true.
                exit
            end if
        end do
    end function qobject_has_property

    subroutine qobject_remove_property(this, name)
        class(forge_qobject), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer :: i

        do i = 1, this%property_count
            if (this%properties(i)%get_name() == name) then
                call this%properties(i)%clear()
                exit
            end if
        end do
    end subroutine qobject_remove_property

    function qobject_get_property_names(this) result(names)
        class(forge_qobject), intent(in) :: this
        type(forge_string), dimension(:), allocatable :: names
        integer :: count, i, j

        count = 0
        do i = 1, this%property_count
            if (this%properties(i)%has_value()) count = count + 1
        end do

        allocate(names(count))
        j = 1
        do i = 1, this%property_count
            if (this%properties(i)%has_value()) then
                names(j) = this%properties(i)%get_name()
                j = j + 1
            end if
        end do
    end function qobject_get_property_names

    function qobject_property_count(this) result(count)
        class(forge_qobject), intent(in) :: this
        integer :: count

        count = 0
        do i = 1, this%property_count
            if (this%properties(i)%has_value()) count = count + 1
        end do
    end function qobject_property_count

    function qobject_meta_object(this) result(meta)
        class(forge_qobject), intent(in) :: this
        type(forge_meta_object) :: meta
        meta = this%meta_object
    end function qobject_meta_object

    function qobject_thread(this) result(thread_id)
        class(forge_qobject), intent(in) :: this
        integer :: thread_id
        thread_id = this%thread_id
    end function qobject_thread

    subroutine qobject_move_to_thread(this, thread_id)
        class(forge_qobject), intent(inout) :: this
        integer, intent(in) :: thread_id
        this%thread_id = thread_id
        ! Note: In a real implementation, this would move the object to another thread
        ! For now, we just update the thread affinity
    end subroutine qobject_move_to_thread

    function qobject_event(this, event) result(handled)
        class(forge_qobject), intent(inout) :: this
        class(forge_event), intent(inout) :: event
        logical :: handled

        handled = .false.

        if (.not. this%event_enabled) return

        select case (event%type())
        case (QEvent_ChildAdded, QEvent_ChildRemoved)
            select type (event)
            type is (QObjectChildEvent)
                call this%child_event(event)
                handled = .true.
            end select
        case (QEvent_Timer)
            select type (event)
            type is (QObjectTimerEvent)
                call this%timer_event(event)
                handled = .true.
            end select
        case default
            call this%custom_event(event)
        end select
    end function qobject_event

    function qobject_event_filter(this, watched, event) result(filtered)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), intent(inout) :: watched
        class(forge_event), intent(inout) :: event
        logical :: filtered
        filtered = .false.  ! Default: don't filter
    end function qobject_event_filter

    subroutine qobject_install_event_filter(this, filter_obj)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), intent(in) :: filter_obj
        ! Note: Event filter installation would be implemented here
        ! For now, this is a placeholder
    end subroutine qobject_install_event_filter

    subroutine qobject_remove_event_filter(this, filter_obj)
        class(forge_qobject), intent(inout) :: this
        class(forge_qobject), intent(in) :: filter_obj
        ! Note: Event filter removal would be implemented here
        ! For now, this is a placeholder
    end subroutine qobject_remove_event_filter

    function qobject_is_widget_type(this) result(is_widget)
        class(forge_qobject), intent(in) :: this
        logical :: is_widget
        is_widget = .false.  ! Base QObject is not a widget
    end function qobject_is_widget_type

    function qobject_is_window_type(this) result(is_window)
        class(forge_qobject), intent(in) :: this
        logical :: is_window
        is_window = .false.  ! Base QObject is not a window
    end function qobject_is_window_type

    function qobject_destroyed_signal(this) result(sig)
        class(forge_qobject), intent(in) :: this
        type(signal_void) :: sig
        sig = this%destroyed_signal
    end function qobject_destroyed_signal

    function qobject_object_name_changed_signal(this) result(sig)
        class(forge_qobject), intent(in) :: this
        type(signal_string) :: sig
        sig = this%object_name_changed_signal
    end function qobject_object_name_changed_signal

    subroutine qobject_dump_tree(this, indent)
        class(forge_qobject), intent(in) :: this
        integer, intent(in), optional :: indent
        integer :: i, indent_level
        character(len=256) :: indent_str
        type(forge_qobject_ptr), dimension(:), allocatable :: children

        indent_level = 0
        if (present(indent)) indent_level = indent

        indent_str = repeat("  ", indent_level)
        write(*, '(A,A,A,I0,A,A)') indent_str, "QObject(id=", &
            this%get_object_name(), this%get_object_id(), &
            ", class=", this%meta_object%get_class_name()

        children = this%get_children()
        do i = 1, size(children)
            if (children(i)%is_valid()) then
                call children(i)%get()%dump_object_tree(indent_level + 1)
            end if
        end do
    end subroutine qobject_dump_tree

    function qobject_inherits(this, class_name) result(inherits)
        class(forge_qobject), intent(in) :: this
        character(len=*), intent(in) :: class_name
        logical :: inherits
        inherits = (this%meta_object%get_class_name() == class_name)
    end function qobject_inherits

    function qobject_is_widget(this) result(is_widget)
        class(forge_qobject), intent(in) :: this
        logical :: is_widget
        is_widget = this%is_widget_type()
    end function qobject_is_widget

    function qobject_is_window(this) result(is_window)
        class(forge_qobject), intent(in) :: this
        logical :: is_window
        is_window = this%is_window_type()
    end function qobject_is_window

    !
    ! Protected virtual methods for subclasses
    subroutine qobject_child_event(this, event)
        class(forge_qobject), intent(inout) :: this
        class(QObjectChildEvent), intent(inout) :: event
        ! Default implementation: do nothing
    end subroutine qobject_child_event

    subroutine qobject_timer_event(this, event)
        class(forge_qobject), intent(inout) :: this
        class(QObjectTimerEvent), intent(inout) :: event
        ! Default implementation: do nothing
    end subroutine qobject_timer_event

    subroutine qobject_custom_event(this, event)
        class(forge_qobject), intent(inout) :: this
        class(forge_event), intent(inout) :: event
        ! Default implementation: do nothing
    end subroutine qobject_custom_event

end module forge_qobject