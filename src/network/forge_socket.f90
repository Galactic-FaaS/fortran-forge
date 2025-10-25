!> @brief Socket abstraction for networking
!> @details TCP and UDP socket support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_socket
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    implicit none
    private

    public :: QTcpSocket, QUdpSocket, QHostAddress
    public :: SocketState, UnconnectedState, ConnectingState, ConnectedState
    public :: BoundState, ClosingState, ListeningState

    !> Socket states
    integer, parameter :: UnconnectedState = 0
    integer, parameter :: HostLookupState = 1
    integer, parameter :: ConnectingState = 2
    integer, parameter :: ConnectedState = 3
    integer, parameter :: BoundState = 4
    integer, parameter :: ClosingState = 5
    integer, parameter :: ListeningState = 6

    type :: SocketState
        integer :: value = UnconnectedState
    end type SocketState

    !> @brief Host address
    type :: QHostAddress
        private
        character(len=45) :: address = ""  ! IPv4 or IPv6
        integer :: port = 0
    contains
        procedure :: set_address => hostaddress_set
        procedure :: get_address => hostaddress_get
        procedure :: set_port => hostaddress_set_port
        procedure :: get_port => hostaddress_get_port
    end type QHostAddress

    !> @brief TCP socket
    type :: QTcpSocket
        private
        type(c_ptr) :: socket_handle = c_null_ptr
        type(SocketState) :: state
        type(QHostAddress) :: peer_address
        character(len=:), allocatable :: read_buffer
        integer :: buffer_size = 0
        logical :: winsock_initialized = .false.
        type(signal_void) :: connected
        type(signal_void) :: disconnected
        type(signal_void) :: ready_read
        type(signal_int) :: bytes_written
        ! error signal would need signal_string or signal_error
    contains
        procedure :: connect_to_host => tcpsocket_connect
        procedure :: disconnect_from_host => tcpsocket_disconnect
        procedure :: write_data => tcpsocket_write
        procedure :: read_data => tcpsocket_read
        procedure :: read_all => tcpsocket_read_all
        procedure :: bytes_available => tcpsocket_bytes_available
        procedure :: flush => tcpsocket_flush
        procedure :: close => tcpsocket_close
        procedure :: get_state => tcpsocket_get_state
        procedure :: peer_addr => tcpsocket_peer_address
        procedure :: wait_for_connected => tcpsocket_wait_connected
        procedure :: wait_for_ready_read => tcpsocket_wait_read
    end type QTcpSocket

    !> @brief UDP socket
    type :: QUdpSocket
        private
        type(c_ptr) :: socket_handle = c_null_ptr
        type(SocketState) :: state
        logical :: winsock_initialized = .false.
        type(signal_void) :: ready_read
    contains
        procedure :: bind => udpsocket_bind
        procedure :: write_datagram => udpsocket_write_datagram
        procedure :: read_datagram => udpsocket_read_datagram
        procedure :: has_pending_datagrams => udpsocket_has_pending
        procedure :: close => udpsocket_close
        procedure :: init_socket => udpsocket_init
    end type QUdpSocket

contains

    ! ========== QHostAddress Implementation ==========

    subroutine hostaddress_set(this, address, port)
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: address
        integer, intent(in), optional :: port
        
        this%address = address
        if (present(port)) this%port = port
    end subroutine hostaddress_set

    function hostaddress_get(this) result(address)
        class(QHostAddress), intent(in) :: this
        character(len=:), allocatable :: address
        address = trim(this%address)
    end function hostaddress_get

    subroutine hostaddress_set_port(this, port)
        class(QHostAddress), intent(inout) :: this
        integer, intent(in) :: port
        this%port = port
    end subroutine hostaddress_set_port

    function hostaddress_get_port(this) result(port)
        class(QHostAddress), intent(in) :: this
        integer :: port
        port = this%port
    end function hostaddress_get_port

    ! ========== QTcpSocket Implementation ==========

    subroutine tcpsocket_connect(this, host, port)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        logical :: success
        
        ! Initialize Winsock if not already done
        if (.not. this%winsock_initialized) then
            success = winsock_init()
            if (.not. success) then
                this%state%value = UnconnectedState
                return
            end if
            this%winsock_initialized = .true.
        end if
        
        ! Create socket
        this%socket_handle = create_tcp_socket()
        if (.not. c_associated(this%socket_handle)) then
            this%state%value = UnconnectedState
            return
        end if
        
        this%state%value = ConnectingState
        call this%peer_address%set(host, port)
        
        ! Connect to host
        success = socket_connect(this%socket_handle, host, port)
        
        if (success) then
            this%state%value = ConnectedState
            call this%connected%emit()
        else
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            this%state%value = UnconnectedState
        end if
    end subroutine tcpsocket_connect

    subroutine tcpsocket_disconnect(this)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        
        if (this%state%value == ConnectedState) then
            this%state%value = ClosingState
            
            if (c_associated(this%socket_handle)) then
                call socket_close(this%socket_handle)
                this%socket_handle = c_null_ptr
            end if
            
            call this%disconnected%emit()
            this%state%value = UnconnectedState
        end if
    end subroutine tcpsocket_disconnect

    function tcpsocket_write(this, data) result(bytes_written)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        character(len=*), intent(in) :: data
        integer :: bytes_written
        
        if (this%state%value /= ConnectedState) then
            bytes_written = -1
            return
        end if
        
        bytes_written = socket_send(this%socket_handle, data, len(data))
        
        if (bytes_written > 0) then
            call this%bytes_written%emit(bytes_written)
        end if
    end function tcpsocket_write

    function tcpsocket_read(this, max_size) result(data)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in) :: max_size
        character(len=:), allocatable :: data
        character(len=4096) :: buffer
        integer :: bytes_read, read_size
        
        if (this%state%value /= ConnectedState) then
            allocate(character(len=0) :: data)
            return
        end if
        
        read_size = min(max_size, 4096)
        bytes_read = socket_recv(this%socket_handle, buffer, read_size)
        
        if (bytes_read > 0) then
            data = buffer(1:bytes_read)
        else if (bytes_read == 0) then
            ! Connection closed
            call this%disconnect_from_host()
            allocate(character(len=0) :: data)
        else
            allocate(character(len=0) :: data)
        end if
    end function tcpsocket_read

    function tcpsocket_read_all(this) result(data)
        class(QTcpSocket), intent(inout) :: this
        character(len=:), allocatable :: data
        character(len=:), allocatable :: chunk
        character(len=65536) :: accumulated
        integer :: total_read, chunk_size
        
        total_read = 0
        
        ! Read all available data
        do
            chunk = this%read(4096)
            chunk_size = len(chunk)
            
            if (chunk_size == 0) exit
            if (total_read + chunk_size > size(accumulated)) exit
            
            accumulated(total_read+1:total_read+chunk_size) = chunk
            total_read = total_read + chunk_size
            
            ! Check if more data available
            if (this%bytes_available() == 0) exit
        end do
        
        data = accumulated(1:total_read)
    end function tcpsocket_read_all

    function tcpsocket_bytes_available(this) result(bytes)
        class(QTcpSocket), intent(in) :: this
        integer :: bytes
        
        ! For now, return size of internal buffer if we had one
        if (allocated(this%read_buffer)) then
            bytes = len(this%read_buffer)
        else
            bytes = 0
        end if
    end function tcpsocket_bytes_available

    subroutine tcpsocket_flush(this)
        class(QTcpSocket), intent(inout) :: this
        ! Winsock sends immediately by default (no buffering needed)
    end subroutine tcpsocket_flush

    subroutine tcpsocket_close(this)
        class(QTcpSocket), intent(inout) :: this
        call this%disconnect_from_host()
    end subroutine tcpsocket_close

    function tcpsocket_wait_connected(this, msecs) result(success)
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in), optional :: msecs
        logical :: success
        integer :: timeout, elapsed
        
        timeout = 30000  ! 30 seconds default
        if (present(msecs)) timeout = msecs
        
        elapsed = 0
        do while (elapsed < timeout .and. this%state%value == ConnectingState)
            ! In real implementation, use select() or WSAPoll
            ! For now, simple wait
            call sleep_ms(10)
            elapsed = elapsed + 10
        end do
        
        success = (this%state%value == ConnectedState)
    end function tcpsocket_wait_connected

    function tcpsocket_wait_read(this, msecs) result(has_data)
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in), optional :: msecs
        logical :: has_data
        
        ! In real implementation, use select() or WSAPoll
        has_data = (this%bytes_available() > 0)
    end function tcpsocket_wait_read

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        
        interface
            subroutine Sleep(dwMilliseconds) bind(C, name="Sleep")
                import :: c_int
                integer(c_int), value :: dwMilliseconds
            end subroutine Sleep
        end interface
        
        call Sleep(milliseconds)
    end subroutine sleep_ms

    function tcpsocket_get_state(this) result(state)
        class(QTcpSocket), intent(in) :: this
        type(SocketState) :: state
        state = this%state
    end function tcpsocket_get_state

    function tcpsocket_peer_address(this) result(address)
        class(QTcpSocket), intent(in) :: this
        type(QHostAddress) :: address
        address = this%peer_address
    end function tcpsocket_peer_address

    ! ========== QUdpSocket Implementation ==========

    subroutine udpsocket_init(this)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        logical :: success
        
        if (this%winsock_initialized) return
        
        success = winsock_init()
        if (success) then
            this%winsock_initialized = .true.
        end if
    end subroutine udpsocket_init

    subroutine udpsocket_bind(this, port, mode)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        integer, intent(in) :: port
        integer, intent(in), optional :: mode
        logical :: success
        
        call this%init_socket()
        
        ! Create UDP socket
        this%socket_handle = create_udp_socket()
        if (.not. c_associated(this%socket_handle)) then
            this%state%value = UnconnectedState
            return
        end if
        
        ! Bind to port
        success = socket_bind(this%socket_handle, port)
        
        if (success) then
            this%state%value = BoundState
        else
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            this%state%value = UnconnectedState
        end if
    end subroutine udpsocket_bind

    function udpsocket_write_datagram(this, data, host, port) result(bytes_written)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        character(len=*), intent(in) :: data
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        integer :: bytes_written
        
        call this%init_socket()
        
        ! Create socket if not already created
        if (.not. c_associated(this%socket_handle)) then
            this%socket_handle = create_udp_socket()
            if (.not. c_associated(this%socket_handle)) then
                bytes_written = -1
                return
            end if
        end if
        
        bytes_written = socket_sendto(this%socket_handle, data, len(data), host, port)
    end function udpsocket_write_datagram

    function udpsocket_read_datagram(this, max_size) result(data)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        integer, intent(in) :: max_size
        character(len=:), allocatable :: data
        character(len=4096) :: buffer
        integer :: bytes_read, read_size
        
        if (.not. c_associated(this%socket_handle)) then
            allocate(character(len=0) :: data)
            return
        end if
        
        read_size = min(max_size, 4096)
        bytes_read = socket_recvfrom(this%socket_handle, buffer, read_size)
        
        if (bytes_read > 0) then
            data = buffer(1:bytes_read)
            call this%ready_read%emit()
        else
            allocate(character(len=0) :: data)
        end if
    end function udpsocket_read_datagram

    function udpsocket_has_pending(this) result(has_pending)
        class(QUdpSocket), intent(in) :: this
        logical :: has_pending
        
        ! Would need to use select() or WSAPoll to check without blocking
        ! For now, conservative approach
        has_pending = c_associated(this%socket_handle) .and. &
                     this%state%value == BoundState
    end function udpsocket_has_pending

    subroutine udpsocket_close(this)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        
        if (c_associated(this%socket_handle)) then
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
        end if
        
        this%state%value = UnconnectedState
    end subroutine udpsocket_close

end module forge_socket

