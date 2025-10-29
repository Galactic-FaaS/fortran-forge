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

    !> @brief Host address with IPv4/IPv6 support
    type :: QHostAddress
        private
        character(len=45) :: address = ""  ! IPv4 or IPv6
        integer :: port = 0
        logical :: is_ipv6 = .false.
    contains
        procedure :: set_address => hostaddress_set
        procedure :: get_address => hostaddress_get
        procedure :: set_port => hostaddress_set_port
        procedure :: get_port => hostaddress_get_port
        procedure :: resolve => hostaddress_resolve
        procedure :: is_ipv6_addr => hostaddress_is_ipv6
        procedure :: to_string => hostaddress_to_string
    end type QHostAddress

    !> @brief TCP socket with full server/client support
    type :: QTcpSocket
        private
        type(c_ptr) :: socket_handle = c_null_ptr
        type(SocketState) :: state
        type(QHostAddress) :: peer_address
        type(QHostAddress) :: local_address
        character(len=:), allocatable :: read_buffer
        integer :: buffer_size = 0
        logical :: winsock_initialized = .false.
        logical :: is_server_socket = .false.
        integer :: error_code = 0
        character(len=256) :: error_string = ""
        type(signal_void) :: connected
        type(signal_void) :: disconnected
        type(signal_void) :: ready_read
        type(signal_int) :: bytes_written
        type(signal_int) :: error_occurred
        type(signal_void) :: state_changed
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
        procedure :: local_addr => tcpsocket_local_address
        procedure :: wait_for_connected => tcpsocket_wait_connected
        procedure :: wait_for_ready_read => tcpsocket_wait_read
        procedure :: wait_for_bytes_written => tcpsocket_wait_written
        procedure :: wait_for_disconnected => tcpsocket_wait_disconnected
        procedure :: set_socket_option => tcpsocket_set_option
        procedure :: get_error => tcpsocket_error
        procedure :: get_error_string => tcpsocket_error_string
        procedure :: listen => tcpsocket_listen
        procedure :: accept => tcpsocket_accept
        procedure :: set_nonblocking => tcpsocket_set_nonblocking
    end type QTcpSocket

    !> @brief UDP socket with multicast support
    type :: QUdpSocket
        private
        type(c_ptr) :: socket_handle = c_null_ptr
        type(SocketState) :: state
        logical :: winsock_initialized = .false.
        type(QHostAddress) :: bound_address
        integer :: error_code = 0
        character(len=256) :: error_string = ""
        type(signal_void) :: ready_read
        type(signal_int) :: bytes_written
        type(signal_int) :: error_occurred
        type(signal_void) :: state_changed
    contains
        procedure :: bind => udpsocket_bind
        procedure :: write_datagram => udpsocket_write_datagram
        procedure :: read_datagram => udpsocket_read_datagram
        procedure :: has_pending_datagrams => udpsocket_has_pending
        procedure :: pending_datagram_size => udpsocket_pending_size
        procedure :: close => udpsocket_close
        procedure :: init_socket => udpsocket_init
        procedure :: join_multicast_group => udpsocket_join_multicast
        procedure :: leave_multicast_group => udpsocket_leave_multicast
        procedure :: set_socket_option => udpsocket_set_option
        procedure :: get_error => udpsocket_error
        procedure :: get_error_string => udpsocket_error_string
        procedure :: bound_addr => udpsocket_bound_address
        procedure :: set_nonblocking => udpsocket_set_nonblocking
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

    subroutine hostaddress_resolve(this, hostname)
        use forge_winsock
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        character(len=256) :: resolved_ip
        logical :: success

        success = resolve_hostname(hostname, this%port, resolved_ip)
        if (success) then
            this%address = trim(resolved_ip)
            ! Check if IPv6 (contains ':')
            this%is_ipv6 = (index(resolved_ip, ':') > 0)
        else
            this%address = ""
            this%is_ipv6 = .false.
        end if
    end subroutine hostaddress_resolve

    function hostaddress_is_ipv6(this) result(is_ipv6)
        class(QHostAddress), intent(in) :: this
        logical :: is_ipv6
        is_ipv6 = this%is_ipv6
    end function hostaddress_is_ipv6

    function hostaddress_to_string(this) result(str)
        class(QHostAddress), intent(in) :: this
        character(len=:), allocatable :: str
        str = trim(this%address)
        if (this%port > 0) then
            str = str // ":" // int_to_string(this%port)
        end if
    end function hostaddress_to_string

    function int_to_string(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
        str = trim(adjustl(str))
    end function int_to_string

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

    function tcpsocket_local_address(this) result(address)
        class(QTcpSocket), intent(in) :: this
        type(QHostAddress) :: address
        address = this%local_address
    end function tcpsocket_local_address

    function tcpsocket_wait_written(this, msecs) result(success)
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in), optional :: msecs
        logical :: success
        ! For now, assume write is immediate
        success = (this%state%value == ConnectedState)
    end function tcpsocket_wait_written

    function tcpsocket_wait_disconnected(this, msecs) result(success)
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in), optional :: msecs
        logical :: success
        integer :: timeout, elapsed

        timeout = 30000  ! 30 seconds default
        if (present(msecs)) timeout = msecs

        elapsed = 0
        do while (elapsed < timeout .and. this%state%value /= UnconnectedState)
            call sleep_ms(10)
            elapsed = elapsed + 10
        end do

        success = (this%state%value == UnconnectedState)
    end function tcpsocket_wait_disconnected

    subroutine tcpsocket_set_option(this, option, value)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in) :: option
        integer, intent(in) :: value

        if (.not. c_associated(this%socket_handle)) return

        ! For now, only support basic options
        ! In full implementation, would use setsockopt
    end subroutine tcpsocket_set_option

    function tcpsocket_error(this) result(error_code)
        class(QTcpSocket), intent(in) :: this
        integer :: error_code
        error_code = this%error_code
    end function tcpsocket_error

    function tcpsocket_error_string(this) result(error_str)
        class(QTcpSocket), intent(in) :: this
        character(len=:), allocatable :: error_str
        error_str = trim(this%error_string)
    end function tcpsocket_error_string

    subroutine tcpsocket_listen(this, port, backlog)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in) :: port
        integer, intent(in), optional :: backlog
        logical :: success
        integer :: max_conn

        max_conn = 5
        if (present(backlog)) max_conn = backlog

        call this%init_winsock()

        this%socket_handle = create_tcp_socket()
        if (.not. c_associated(this%socket_handle)) then
            this%error_code = -1
            this%error_string = "Failed to create listening socket"
            return
        end if

        success = socket_bind(this%socket_handle, port)
        if (.not. success) then
            this%error_code = -2
            this%error_string = "Failed to bind to port"
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            return
        end if

        success = socket_listen(this%socket_handle, max_conn)
        if (.not. success) then
            this%error_code = -3
            this%error_string = "Failed to listen on socket"
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            return
        end if

        this%state%value = ListeningState
        this%is_server_socket = .true.
        call this%local_address%set("", port)
        call this%state_changed%emit()
    end subroutine tcpsocket_listen

    function tcpsocket_accept(this) result(client_socket)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        type(QTcpSocket) :: client_socket
        type(c_ptr) :: client_handle

        if (this%state%value /= ListeningState) then
            client_socket%error_code = -4
            client_socket%error_string = "Socket not in listening state"
            return
        end if

        client_handle = socket_accept(this%socket_handle)
        if (.not. c_associated(client_handle)) then
            client_socket%error_code = -5
            client_socket%error_string = "Accept failed"
            return
        end if

        ! Initialize client socket
        client_socket%socket_handle = client_handle
        client_socket%state%value = ConnectedState
        client_socket%winsock_initialized = .true.
        client_socket%is_server_socket = .false.
        ! Would need to get peer address from accept
        call client_socket%connected%emit()
        call client_socket%state_changed%emit()
    end function tcpsocket_accept

    subroutine tcpsocket_set_nonblocking(this, nonblocking)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        logical, intent(in) :: nonblocking
        logical :: success

        if (c_associated(this%socket_handle)) then
            success = socket_set_nonblocking(this%socket_handle, nonblocking)
            if (.not. success) then
                this%error_code = -6
                this%error_string = "Failed to set non-blocking mode"
            end if
        end if
    end subroutine tcpsocket_set_nonblocking

    subroutine init_winsock(this)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        logical :: success

        if (.not. this%winsock_initialized) then
            success = winsock_init()
            if (.not. success) then
                this%error_code = -7
                this%error_string = "Failed to initialize Winsock"
            else
                this%winsock_initialized = .true.
            end if
        end if
    end subroutine init_winsock

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
        call this%state_changed%emit()
    end subroutine udpsocket_close

    subroutine udpsocket_join_multicast(this, group_address, interface_address)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        character(len=*), intent(in) :: group_address
        character(len=*), intent(in), optional :: interface_address
        character(len=256) :: iface
        logical :: success

        if (.not. c_associated(this%socket_handle)) return

        iface = "0.0.0.0"  ! Default interface
        if (present(interface_address)) iface = interface_address

        success = set_multicast_group(this%socket_handle, group_address, iface, .true.)
        if (.not. success) then
            this%error_code = -8
            this%error_string = "Failed to join multicast group"
            call this%error_occurred%emit(this%error_code)
        end if
    end subroutine udpsocket_join_multicast

    subroutine udpsocket_leave_multicast(this, group_address, interface_address)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        character(len=*), intent(in) :: group_address
        character(len=*), intent(in), optional :: interface_address
        character(len=256) :: iface
        logical :: success

        if (.not. c_associated(this%socket_handle)) return

        iface = "0.0.0.0"  ! Default interface
        if (present(interface_address)) iface = interface_address

        success = set_multicast_group(this%socket_handle, group_address, iface, .false.)
        if (.not. success) then
            this%error_code = -9
            this%error_string = "Failed to leave multicast group"
            call this%error_occurred%emit(this%error_code)
        end if
    end subroutine udpsocket_leave_multicast

    function udpsocket_pending_size(this) result(size)
        class(QUdpSocket), intent(in) :: this
        integer :: size
        ! Would need to peek at datagram size
        size = 0  ! Placeholder
    end function udpsocket_pending_size

    subroutine udpsocket_set_option(this, option, value)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        integer, intent(in) :: option
        integer, intent(in) :: value

        if (.not. c_associated(this%socket_handle)) return

        ! For now, only support basic options
        ! In full implementation, would use setsockopt
    end subroutine udpsocket_set_option

    function udpsocket_error(this) result(error_code)
        class(QUdpSocket), intent(in) :: this
        integer :: error_code
        error_code = this%error_code
    end function udpsocket_error

    function udpsocket_error_string(this) result(error_str)
        class(QUdpSocket), intent(in) :: this
        character(len=:), allocatable :: error_str
        error_str = trim(this%error_string)
    end function udpsocket_error_string

    function udpsocket_bound_address(this) result(address)
        class(QUdpSocket), intent(in) :: this
        type(QHostAddress) :: address
        address = this%bound_address
    end function udpsocket_bound_address

    subroutine udpsocket_set_nonblocking(this, nonblocking)
        use forge_winsock
        class(QUdpSocket), intent(inout) :: this
        logical, intent(in) :: nonblocking
        logical :: success

        if (c_associated(this%socket_handle)) then
            success = socket_set_nonblocking(this%socket_handle, nonblocking)
            if (.not. success) then
                this%error_code = -10
                this%error_string = "Failed to set non-blocking mode"
                call this%error_occurred%emit(this%error_code)
            end if
        end if
    end subroutine udpsocket_set_nonblocking

end module forge_socket

