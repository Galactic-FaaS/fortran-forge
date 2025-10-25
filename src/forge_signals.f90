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
    public :: slot_void, slot_int, slot_string, slot_bool

    !> Maximum connections per signal
    integer, parameter :: MAX_CONNECTIONS = 100

    !> @brief Connection ID type
    type :: forge_connection
        integer :: id = 0
        logical :: active = .false.
    end type forge_connection

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
    end interface

    !> @brief Base signal class (void - no arguments)
    type :: signal_void
        private
        integer :: connection_count = 0
        procedure(slot_void), pointer :: slots(MAX_CONNECTIONS)
        type(forge_connection) :: connections(MAX_CONNECTIONS)
    contains
        procedure :: connect => signal_void_connect
        procedure :: disconnect => signal_void_disconnect
        procedure :: disconnect_all => signal_void_disconnect_all
        procedure :: emit => signal_void_emit
        procedure :: is_connected => signal_void_is_connected
    end type signal_void

    !> @brief Integer signal (one integer argument)
    type :: signal_int
        private
        integer :: connection_count = 0
        procedure(slot_int), pointer :: slots(MAX_CONNECTIONS)
        type(forge_connection) :: connections(MAX_CONNECTIONS)
    contains
        procedure :: connect => signal_int_connect
        procedure :: disconnect => signal_int_disconnect
        procedure :: disconnect_all => signal_int_disconnect_all
        procedure :: emit => signal_int_emit
        procedure :: is_connected => signal_int_is_connected
    end type signal_int

    !> @brief String signal (one string argument)
    type :: signal_string
        private
        integer :: connection_count = 0
        procedure(slot_string), pointer :: slots(MAX_CONNECTIONS)
        type(forge_connection) :: connections(MAX_CONNECTIONS)
    contains
        procedure :: connect => signal_string_connect
        procedure :: disconnect => signal_string_disconnect
        procedure :: disconnect_all => signal_string_disconnect_all
        procedure :: emit => signal_string_emit
        procedure :: is_connected => signal_string_is_connected
    end type signal_string

    !> @brief Boolean signal (one boolean argument)
    type :: signal_bool
        private
        integer :: connection_count = 0
        procedure(slot_bool), pointer :: slots(MAX_CONNECTIONS)
        type(forge_connection) :: connections(MAX_CONNECTIONS)
    contains
        procedure :: connect => signal_bool_connect
        procedure :: disconnect => signal_bool_disconnect
        procedure :: disconnect_all => signal_bool_disconnect_all
        procedure :: emit => signal_bool_emit
        procedure :: is_connected => signal_bool_is_connected
    end type signal_bool

    ! Generic signal type (for polymorphism)
    type :: forge_signal
        class(*), allocatable :: signal_impl
    end type forge_signal

    type :: forge_slot
        ! Placeholder for future slot abstraction
    end type forge_slot

contains

    ! ========== signal_void Implementation ==========

    !> @brief Connect a slot to void signal
    function signal_void_connect(this, slot) result(conn)
        class(signal_void), intent(inout) :: this
        procedure(slot_void) :: slot
        type(forge_connection) :: conn

        if (this%connection_count >= MAX_CONNECTIONS) then
            conn%id = -1
            conn%active = .false.
            return
        end if

        this%connection_count = this%connection_count + 1
        this%slots(this%connection_count) => slot
        this%connections(this%connection_count)%id = this%connection_count
        this%connections(this%connection_count)%active = .true.
        
        conn = this%connections(this%connection_count)
    end function signal_void_connect

    !> @brief Disconnect specific connection
    subroutine signal_void_disconnect(this, conn)
        class(signal_void), intent(inout) :: this
        type(forge_connection), intent(in) :: conn
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%id == conn%id) then
                this%connections(i)%active = .false.
                nullify(this%slots(i))
                exit
            end if
        end do
    end subroutine signal_void_disconnect

    !> @brief Disconnect all slots
    subroutine signal_void_disconnect_all(this)
        class(signal_void), intent(inout) :: this
        integer :: i

        do i = 1, this%connection_count
            this%connections(i)%active = .false.
            nullify(this%slots(i))
        end do
        this%connection_count = 0
    end subroutine signal_void_disconnect_all

    !> @brief Emit signal (call all connected slots)
    subroutine signal_void_emit(this)
        class(signal_void), intent(in) :: this
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%active .and. associated(this%slots(i))) then
                call this%slots(i)()
            end if
        end do
    end subroutine signal_void_emit

    !> @brief Check if signal has connections
    function signal_void_is_connected(this) result(connected)
        class(signal_void), intent(in) :: this
        logical :: connected
        integer :: i

        connected = .false.
        do i = 1, this%connection_count
            if (this%connections(i)%active) then
                connected = .true.
                return
            end if
        end do
    end function signal_void_is_connected

    ! ========== signal_int Implementation ==========

    function signal_int_connect(this, slot) result(conn)
        class(signal_int), intent(inout) :: this
        procedure(slot_int) :: slot
        type(forge_connection) :: conn

        if (this%connection_count >= MAX_CONNECTIONS) then
            conn%id = -1
            conn%active = .false.
            return
        end if

        this%connection_count = this%connection_count + 1
        this%slots(this%connection_count) => slot
        this%connections(this%connection_count)%id = this%connection_count
        this%connections(this%connection_count)%active = .true.
        
        conn = this%connections(this%connection_count)
    end function signal_int_connect

    subroutine signal_int_disconnect(this, conn)
        class(signal_int), intent(inout) :: this
        type(forge_connection), intent(in) :: conn
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%id == conn%id) then
                this%connections(i)%active = .false.
                nullify(this%slots(i))
                exit
            end if
        end do
    end subroutine signal_int_disconnect

    subroutine signal_int_disconnect_all(this)
        class(signal_int), intent(inout) :: this
        integer :: i

        do i = 1, this%connection_count
            this%connections(i)%active = .false.
            nullify(this%slots(i))
        end do
        this%connection_count = 0
    end subroutine signal_int_disconnect_all

    subroutine signal_int_emit(this, value)
        class(signal_int), intent(in) :: this
        integer(c_int), intent(in) :: value
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%active .and. associated(this%slots(i))) then
                call this%slots(i)(value)
            end if
        end do
    end subroutine signal_int_emit

    function signal_int_is_connected(this) result(connected)
        class(signal_int), intent(in) :: this
        logical :: connected
        integer :: i

        connected = .false.
        do i = 1, this%connection_count
            if (this%connections(i)%active) then
                connected = .true.
                return
            end if
        end do
    end function signal_int_is_connected

    ! ========== signal_string Implementation ==========

    function signal_string_connect(this, slot) result(conn)
        class(signal_string), intent(inout) :: this
        procedure(slot_string) :: slot
        type(forge_connection) :: conn

        if (this%connection_count >= MAX_CONNECTIONS) then
            conn%id = -1
            conn%active = .false.
            return
        end if

        this%connection_count = this%connection_count + 1
        this%slots(this%connection_count) => slot
        this%connections(this%connection_count)%id = this%connection_count
        this%connections(this%connection_count)%active = .true.
        
        conn = this%connections(this%connection_count)
    end function signal_string_connect

    subroutine signal_string_disconnect(this, conn)
        class(signal_string), intent(inout) :: this
        type(forge_connection), intent(in) :: conn
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%id == conn%id) then
                this%connections(i)%active = .false.
                nullify(this%slots(i))
                exit
            end if
        end do
    end subroutine signal_string_disconnect

    subroutine signal_string_disconnect_all(this)
        class(signal_string), intent(inout) :: this
        integer :: i

        do i = 1, this%connection_count
            this%connections(i)%active = .false.
            nullify(this%slots(i))
        end do
        this%connection_count = 0
    end subroutine signal_string_disconnect_all

    subroutine signal_string_emit(this, value)
        class(signal_string), intent(in) :: this
        character(len=*), intent(in) :: value
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%active .and. associated(this%slots(i))) then
                call this%slots(i)(value)
            end if
        end do
    end subroutine signal_string_emit

    function signal_string_is_connected(this) result(connected)
        class(signal_string), intent(in) :: this
        logical :: connected
        integer :: i

        connected = .false.
        do i = 1, this%connection_count
            if (this%connections(i)%active) then
                connected = .true.
                return
            end if
        end do
    end function signal_string_is_connected

    ! ========== signal_bool Implementation ==========

    function signal_bool_connect(this, slot) result(conn)
        class(signal_bool), intent(inout) :: this
        procedure(slot_bool) :: slot
        type(forge_connection) :: conn

        if (this%connection_count >= MAX_CONNECTIONS) then
            conn%id = -1
            conn%active = .false.
            return
        end if

        this%connection_count = this%connection_count + 1
        this%slots(this%connection_count) => slot
        this%connections(this%connection_count)%id = this%connection_count
        this%connections(this%connection_count)%active = .true.
        
        conn = this%connections(this%connection_count)
    end function signal_bool_connect

    subroutine signal_bool_disconnect(this, conn)
        class(signal_bool), intent(inout) :: this
        type(forge_connection), intent(in) :: conn
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%id == conn%id) then
                this%connections(i)%active = .false.
                nullify(this%slots(i))
                exit
            end if
        end do
    end subroutine signal_bool_disconnect

    subroutine signal_bool_disconnect_all(this)
        class(signal_bool), intent(inout) :: this
        integer :: i

        do i = 1, this%connection_count
            this%connections(i)%active = .false.
            nullify(this%slots(i))
        end do
        this%connection_count = 0
    end subroutine signal_bool_disconnect_all

    subroutine signal_bool_emit(this, value)
        class(signal_bool), intent(in) :: this
        logical, intent(in) :: value
        integer :: i

        do i = 1, this%connection_count
            if (this%connections(i)%active .and. associated(this%slots(i))) then
                call this%slots(i)(value)
            end if
        end do
    end subroutine signal_bool_emit

    function signal_bool_is_connected(this) result(connected)
        class(signal_bool), intent(in) :: this
        logical :: connected
        integer :: i

        connected = .false.
        do i = 1, this%connection_count
            if (this%connections(i)%active) then
                connected = .true.
                return
            end if
        end do
    end function signal_bool_is_connected

end module forge_signals

