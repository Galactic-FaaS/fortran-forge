!> @brief Signals and Slots system for ForGE Qt
!> @details Implements Qt-style signals and slots for event handling
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_signals
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: forge_signal, forge_slot, forge_connection
    public :: signal_void, signal_int, signal_string, signal_bool
    public :: signal_double, signal_qobject, signal_qvariant, signal_custom
    public :: signal_blocker, signal_spy, signal_connection
    public :: slot_void, slot_int, slot_string, slot_bool
    public :: slot_double, slot_qobject, slot_qvariant, slot_custom

    !> Maximum connections per signal
    integer, parameter :: MAX_CONNECTIONS = 100

    !> Signal compression threshold
    integer, parameter :: SIGNAL_COMPRESSION_THRESHOLD = 10

    !> @brief Connection ID type
    type :: forge_connection
        integer :: id = 0
        logical :: active = .false.
        logical :: auto_disconnect = .true.
        integer :: thread_id = 0
    end type forge_connection

    !> @brief Enhanced connection with metadata
    type :: signal_connection
        type(forge_connection) :: base
        character(len=:), allocatable :: signal_name
        character(len=:), allocatable :: slot_name
        integer :: connection_type = 0  ! 0=direct, 1=queued, 2=blocking
    end type signal_connection

    !> @brief Abstract slot interface (void - no arguments)
    abstract interface
        subroutine slot_void()
        end subroutine slot_void

        subroutine slot_int(value)
            import :: c_int
            integer(c_int), intent(in) :: value
        end subroutine slot_int

        subroutine slot_string(value)
            character(len=*), intent(in) :: value
        end subroutine slot_string

        subroutine slot_bool(value)
            logical, intent(in) :: value
        end subroutine slot_bool

        subroutine slot_double(value)
            import :: c_double
            real(c_double), intent(in) :: value
        end subroutine slot_double

        subroutine slot_qobject(obj)
            class(*), intent(in) :: obj
        end subroutine slot_qobject

        subroutine slot_qvariant(var)
            class(*), intent(in) :: var
        end subroutine slot_qvariant

        subroutine slot_custom(data)
            class(*), intent(in) :: data
        end subroutine slot_custom
    end interface

    !> @brief QVariant type for generic data
    type :: forge_qvariant
        private
        integer :: type_id = 0
        class(*), allocatable :: data
    contains
        procedure :: set_int => qvariant_set_int
        procedure :: set_double => qvariant_set_double
        procedure :: set_string => qvariant_set_string
        procedure :: set_bool => qvariant_set_bool
        procedure :: get_int => qvariant_get_int
        procedure :: get_double => qvariant_get_double
        procedure :: get_string => qvariant_get_string
        procedure :: get_bool => qvariant_get_bool
        procedure :: type => qvariant_type
        procedure :: is_valid => qvariant_is_valid
        final :: qvariant_finalize
    end type forge_qvariant

! ========== Additional Signal Types ==========

!> @brief Double signal (one double argument)
type :: signal_double
    private
    integer :: connection_count = 0
    type(slot_double_proc), dimension(:), allocatable :: slots
    type(signal_connection) :: connections(MAX_CONNECTIONS)
    logical :: blocked = .false.
    integer :: thread_id = 0
    integer :: emit_count = 0
    logical :: compression_enabled = .true.
contains
    procedure :: connect => signal_double_connect
    procedure :: disconnect => signal_double_disconnect
    procedure :: disconnect_all => signal_double_disconnect_all
    procedure :: emit => signal_double_emit
    procedure :: is_connected => signal_double_is_connected
    procedure :: block => signal_double_block
    procedure :: unblock => signal_double_unblock
    procedure :: is_blocked => signal_double_is_blocked
    procedure :: set_compression => signal_double_set_compression
    procedure :: move_to_thread => signal_double_move_to_thread
    procedure :: introspect => signal_double_introspect
end type signal_double

!> @brief Procedure pointer type for double slots
type :: slot_double_proc
    procedure(slot_double), pointer, nopass :: proc => null()
end type slot_double_proc

!> @brief QObject signal (one QObject argument)
type :: signal_qobject
    private
    integer :: connection_count = 0
    type(slot_qobject_proc), dimension(:), allocatable :: slots
    type(signal_connection) :: connections(MAX_CONNECTIONS)
    logical :: blocked = .false.
    integer :: thread_id = 0
    integer :: emit_count = 0
    logical :: compression_enabled = .true.
contains
    procedure :: connect => signal_qobject_connect
    procedure :: disconnect => signal_qobject_disconnect
    procedure :: disconnect_all => signal_qobject_disconnect_all
    procedure :: emit => signal_qobject_emit
    procedure :: is_connected => signal_qobject_is_connected
    procedure :: block => signal_qobject_block
    procedure :: unblock => signal_qobject_unblock
    procedure :: is_blocked => signal_qobject_is_blocked
    procedure :: set_compression => signal_qobject_set_compression
    procedure :: move_to_thread => signal_qobject_move_to_thread
    procedure :: introspect => signal_qobject_introspect
end type signal_qobject

!> @brief Procedure pointer type for QObject slots
type :: slot_qobject_proc
    procedure(slot_qobject), pointer, nopass :: proc => null()
end type slot_qobject_proc

!> @brief QVariant signal (one QVariant argument)
type :: signal_qvariant
    private
    integer :: connection_count = 0
    type(slot_qvariant_proc), dimension(:), allocatable :: slots
    type(signal_connection) :: connections(MAX_CONNECTIONS)
    logical :: blocked = .false.
    integer :: thread_id = 0
    integer :: emit_count = 0
    logical :: compression_enabled = .true.
contains
    procedure :: connect => signal_qvariant_connect
    procedure :: disconnect => signal_qvariant_disconnect
    procedure :: disconnect_all => signal_qvariant_disconnect_all
    procedure :: emit => signal_qvariant_emit
    procedure :: is_connected => signal_qvariant_is_connected
    procedure :: block => signal_qvariant_block
    procedure :: unblock => signal_qvariant_unblock
    procedure :: is_blocked => signal_qvariant_is_blocked
    procedure :: set_compression => signal_qvariant_set_compression
    procedure :: move_to_thread => signal_qvariant_move_to_thread
    procedure :: introspect => signal_qvariant_introspect
end type signal_qvariant

!> @brief Procedure pointer type for QVariant slots
type :: slot_qvariant_proc
    procedure(slot_qvariant), pointer, nopass :: proc => null()
end type slot_qvariant_proc

!> @brief Custom type signal (one custom class argument)
type :: signal_custom
    private
    integer :: connection_count = 0
    type(slot_custom_proc), dimension(:), allocatable :: slots
    type(signal_connection) :: connections(MAX_CONNECTIONS)
    logical :: blocked = .false.
    integer :: thread_id = 0
    integer :: emit_count = 0
    logical :: compression_enabled = .true.
contains
    procedure :: connect => signal_custom_connect
    procedure :: disconnect => signal_custom_disconnect
    procedure :: disconnect_all => signal_custom_disconnect_all
    procedure :: emit => signal_custom_emit
    procedure :: is_connected => signal_custom_is_connected
    procedure :: block => signal_custom_block
    procedure :: unblock => signal_custom_unblock
    procedure :: is_blocked => signal_custom_is_blocked
    procedure :: set_compression => signal_custom_set_compression
    procedure :: move_to_thread => signal_custom_move_to_thread
    procedure :: introspect => signal_custom_introspect
end type signal_custom

!> @brief Procedure pointer type for custom slots
type :: slot_custom_proc
    procedure(slot_custom), pointer, nopass :: proc => null()
end type slot_custom_proc

!> @brief Signal blocker for RAII-style blocking
type :: signal_blocker
    private
    class(*), pointer :: signal_ptr => null()
    logical :: was_blocked = .false.
contains
    procedure :: init => signal_blocker_init
    procedure :: cleanup => signal_blocker_cleanup
    final :: signal_blocker_finalize
end type signal_blocker

!> @brief Signal spy for debugging signal emissions
type :: signal_spy
    private
    character(len=:), allocatable :: signal_name
    integer :: emit_count = 0
    integer :: last_emit_time = 0
    logical :: enabled = .true.
contains
    procedure :: record_emit => signal_spy_record_emit
    procedure :: get_emit_count => signal_spy_get_emit_count
    procedure :: reset => signal_spy_reset
    procedure :: enable => signal_spy_enable
    procedure :: disable => signal_spy_disable
    procedure :: is_enabled => signal_spy_is_enabled
end type signal_spy

! Generic signal type (for polymorphism)
type :: forge_signal
    class(*), allocatable :: signal_impl
end type forge_signal

type :: forge_slot
    ! Placeholder for future slot abstraction
end type forge_slot

contains

! ========== forge_qvariant Implementation ==========

subroutine qvariant_set_int(this, val)
    class(forge_qvariant), intent(inout) :: this
    integer, intent(in) :: val
    if (allocated(this%data)) deallocate(this%data)
    allocate(this%data, source=val)
    this%type_id = 1
end subroutine qvariant_set_int

subroutine qvariant_set_double(this, val)
    class(forge_qvariant), intent(inout) :: this
    real(c_double), intent(in) :: val
    if (allocated(this%data)) deallocate(this%data)
    allocate(this%data, source=val)
    this%type_id = 2
end subroutine qvariant_set_double

subroutine qvariant_set_string(this, val)
    class(forge_qvariant), intent(inout) :: this
    character(len=*), intent(in) :: val
    if (allocated(this%data)) deallocate(this%data)
    allocate(this%data, source=val)
    this%type_id = 3
end subroutine qvariant_set_string

subroutine qvariant_set_bool(this, val)
    class(forge_qvariant), intent(inout) :: this
    logical, intent(in) :: val
    if (allocated(this%data)) deallocate(this%data)
    allocate(this%data, source=val)
    this%type_id = 4
end subroutine qvariant_set_bool

function qvariant_get_int(this, default_val) result(val)
    class(forge_qvariant), intent(in) :: this
    integer, intent(in), optional :: default_val
    integer :: val
    if (this%type_id == 1 .and. allocated(this%data)) then
        select type (v => this%data)
        type is (integer)
            val = v
        class default
            val = 0
        end select
    else
        if (present(default_val)) then
            val = default_val
        else
            val = 0
        end if
    end if
end function qvariant_get_int

function qvariant_get_double(this, default_val) result(val)
    class(forge_qvariant), intent(in) :: this
    real(c_double), intent(in), optional :: default_val
    real(c_double) :: val
    if (this%type_id == 2 .and. allocated(this%data)) then
        select type (v => this%data)
        type is (real(c_double))
            val = v
        class default
            val = 0.0_c_double
        end select
    else
        if (present(default_val)) then
            val = default_val
        else
            val = 0.0_c_double
        end if
    end if
end function qvariant_get_double

function qvariant_get_string(this, default_val) result(val)
    class(forge_qvariant), intent(in) :: this
    character(len=*), intent(in), optional :: default_val
    character(len=:), allocatable :: val
    if (this%type_id == 3 .and. allocated(this%data)) then
        select type (v => this%data)
        type is (character(len=*))
            val = v
        class default
            val = ""
        end select
    else
        if (present(default_val)) then
            val = default_val
        else
            val = ""
        end if
    end if
end function qvariant_get_string

function qvariant_get_bool(this, default_val) result(val)
    class(forge_qvariant), intent(in) :: this
    logical, intent(in), optional :: default_val
    logical :: val
    if (this%type_id == 4 .and. allocated(this%data)) then
        select type (v => this%data)
        type is (logical)
            val = v
        class default
            val = .false.
        end select
    else
        if (present(default_val)) then
            val = default_val
        else
            val = .false.
        end if
    end if
end function qvariant_get_bool

function qvariant_type(this) result(type_id)
    class(forge_qvariant), intent(in) :: this
    integer :: type_id
    type_id = this%type_id
end function qvariant_type

function qvariant_is_valid(this) result(valid)
    class(forge_qvariant), intent(in) :: this
    logical :: valid
    valid = allocated(this%data) .and. this%type_id > 0
end function qvariant_is_valid

subroutine qvariant_finalize(this)
    type(forge_qvariant), intent(inout) :: this
    if (allocated(this%data)) deallocate(this%data)
    this%type_id = 0
end subroutine qvariant_finalize

! ========== signal_blocker Implementation ==========

subroutine signal_blocker_init(this, signal)
    class(signal_blocker), intent(inout) :: this
    class(*), target :: signal

    this%signal_ptr => signal
    select type (sig => signal)
    type is (signal_void)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_int)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_string)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_bool)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_double)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_qobject)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_qvariant)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    type is (signal_custom)
        this%was_blocked = sig%blocked
        sig%blocked = .true.
    end select
end subroutine signal_blocker_init

subroutine signal_blocker_cleanup(this)
    class(signal_blocker), intent(inout) :: this
    if (associated(this%signal_ptr)) then
        select type (sig => this%signal_ptr)
        type is (signal_void)
            sig%blocked = this%was_blocked
        type is (signal_int)
            sig%blocked = this%was_blocked
        type is (signal_string)
            sig%blocked = this%was_blocked
        type is (signal_bool)
            sig%blocked = this%was_blocked
        type is (signal_double)
            sig%blocked = this%was_blocked
        type is (signal_qobject)
            sig%blocked = this%was_blocked
        type is (signal_qvariant)
            sig%blocked = this%was_blocked
        type is (signal_custom)
            sig%blocked = this%was_blocked
        end select
    end if
end subroutine signal_blocker_cleanup

subroutine signal_blocker_finalize(this)
    type(signal_blocker), intent(inout) :: this
    call this%cleanup()
end subroutine signal_blocker_finalize

! ========== signal_spy Implementation ==========

subroutine signal_spy_record_emit(this)
    class(signal_spy), intent(inout) :: this
    if (this%enabled) then
        this%emit_count = this%emit_count + 1
        ! In real implementation, would get current time
        this%last_emit_time = 1  ! Placeholder
    end if
end subroutine signal_spy_record_emit

function signal_spy_get_emit_count(this) result(count)
    class(signal_spy), intent(in) :: this
    integer :: count
    count = this%emit_count
end function signal_spy_get_emit_count

subroutine signal_spy_reset(this)
    class(signal_spy), intent(inout) :: this
    this%emit_count = 0
    this%last_emit_time = 0
end subroutine signal_spy_reset

subroutine signal_spy_enable(this)
    class(signal_spy), intent(inout) :: this
    this%enabled = .true.
end subroutine signal_spy_enable

subroutine signal_spy_disable(this)
    class(signal_spy), intent(inout) :: this
    this%enabled = .false.
end subroutine signal_spy_disable

function signal_spy_is_enabled(this) result(enabled)
    class(signal_spy), intent(in) :: this
    logical :: enabled
    enabled = this%enabled
end function signal_spy_is_enabled

! ========== Enhanced Signal Methods ==========

! Block signal emission
subroutine signal_void_block(this)
    class(signal_void), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_void_block

subroutine signal_void_unblock(this)
    class(signal_void), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_void_unblock

function signal_void_is_blocked(this) result(blocked)
    class(signal_void), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_void_is_blocked

subroutine signal_void_set_compression(this, enabled)
    class(signal_void), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_void_set_compression

subroutine signal_void_move_to_thread(this, thread_id)
    class(signal_void), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_void_move_to_thread

function signal_void_introspect(this) result(info)
    class(signal_void), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_void: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_void_introspect

! Similar implementations for other signal types
subroutine signal_int_block(this)
    class(signal_int), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_int_block

subroutine signal_int_unblock(this)
    class(signal_int), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_int_unblock

function signal_int_is_blocked(this) result(blocked)
    class(signal_int), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_int_is_blocked

subroutine signal_int_set_compression(this, enabled)
    class(signal_int), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_int_set_compression

subroutine signal_int_move_to_thread(this, thread_id)
    class(signal_int), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_int_move_to_thread

function signal_int_introspect(this) result(info)
    class(signal_int), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_int: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_int_introspect

subroutine signal_string_block(this)
    class(signal_string), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_string_block

subroutine signal_string_unblock(this)
    class(signal_string), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_string_unblock

function signal_string_is_blocked(this) result(blocked)
    class(signal_string), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_string_is_blocked

subroutine signal_string_set_compression(this, enabled)
    class(signal_string), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_string_set_compression

subroutine signal_string_move_to_thread(this, thread_id)
    class(signal_string), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_string_move_to_thread

function signal_string_introspect(this) result(info)
    class(signal_string), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_string: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_string_introspect

subroutine signal_bool_block(this)
    class(signal_bool), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_bool_block

subroutine signal_bool_unblock(this)
    class(signal_bool), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_bool_unblock

function signal_bool_is_blocked(this) result(blocked)
    class(signal_bool), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_bool_is_blocked

subroutine signal_bool_set_compression(this, enabled)
    class(signal_bool), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_bool_set_compression

subroutine signal_bool_move_to_thread(this, thread_id)
    class(signal_bool), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_bool_move_to_thread

function signal_bool_introspect(this) result(info)
    class(signal_bool), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_bool: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_bool_introspect

! Additional signal types implementations
subroutine signal_double_block(this)
    class(signal_double), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_double_block

subroutine signal_double_unblock(this)
    class(signal_double), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_double_unblock

function signal_double_is_blocked(this) result(blocked)
    class(signal_double), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_double_is_blocked

subroutine signal_double_set_compression(this, enabled)
    class(signal_double), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_double_set_compression

subroutine signal_double_move_to_thread(this, thread_id)
    class(signal_double), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_double_move_to_thread

function signal_double_introspect(this) result(info)
    class(signal_double), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_double: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_double_introspect

subroutine signal_qobject_block(this)
    class(signal_qobject), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_qobject_block

subroutine signal_qobject_unblock(this)
    class(signal_qobject), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_qobject_unblock

function signal_qobject_is_blocked(this) result(blocked)
    class(signal_qobject), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_qobject_is_blocked

subroutine signal_qobject_set_compression(this, enabled)
    class(signal_qobject), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_qobject_set_compression

subroutine signal_qobject_move_to_thread(this, thread_id)
    class(signal_qobject), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_qobject_move_to_thread

function signal_qobject_introspect(this) result(info)
    class(signal_qobject), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_qobject: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_qobject_introspect

subroutine signal_qvariant_block(this)
    class(signal_qvariant), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_qvariant_block

subroutine signal_qvariant_unblock(this)
    class(signal_qvariant), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_qvariant_unblock

function signal_qvariant_is_blocked(this) result(blocked)
    class(signal_qvariant), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_qvariant_is_blocked

subroutine signal_qvariant_set_compression(this, enabled)
    class(signal_qvariant), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_qvariant_set_compression

subroutine signal_qvariant_move_to_thread(this, thread_id)
    class(signal_qvariant), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_qvariant_move_to_thread

function signal_qvariant_introspect(this) result(info)
    class(signal_qvariant), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_qvariant: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_qvariant_introspect

subroutine signal_custom_block(this)
    class(signal_custom), intent(inout) :: this
    this%blocked = .true.
end subroutine signal_custom_block

subroutine signal_custom_unblock(this)
    class(signal_custom), intent(inout) :: this
    this%blocked = .false.
end subroutine signal_custom_unblock

function signal_custom_is_blocked(this) result(blocked)
    class(signal_custom), intent(in) :: this
    logical :: blocked
    blocked = this%blocked
end function signal_custom_is_blocked

subroutine signal_custom_set_compression(this, enabled)
    class(signal_custom), intent(inout) :: this
    logical, intent(in) :: enabled
    this%compression_enabled = enabled
end subroutine signal_custom_set_compression

subroutine signal_custom_move_to_thread(this, thread_id)
    class(signal_custom), intent(inout) :: this
    integer, intent(in) :: thread_id
    this%thread_id = thread_id
end subroutine signal_custom_move_to_thread

function signal_custom_introspect(this) result(info)
    class(signal_custom), intent(in) :: this
    character(len=:), allocatable :: info
    character(len=256) :: temp
    write(temp, '(A,I0,A,L1,A,I0)') "signal_custom: connections=", this%connection_count, &
        ", blocked=", this%blocked, ", thread=", this%thread_id
    info = trim(temp)
end function signal_custom_introspect

end module forge_signals