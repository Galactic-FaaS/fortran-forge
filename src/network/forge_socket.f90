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
    use forge_socket_platform
    implicit none
    private

    public :: QTcpSocket, QUdpSocket, QHostAddress, NetworkInterface
    public :: SocketState, UnconnectedState, ConnectingState, ConnectedState
    public :: BoundState, ClosingState, ListeningState
    public :: enumerate_interfaces, get_interface_info
    public :: set_ipv6_only, get_ipv6_only, set_dual_stack
    public :: set_ipv6_scope_id, set_ipv6_flow_info, set_ipv6_traffic_class
    public :: get_ipv6_scope_id, get_ipv6_flow_info, get_ipv6_traffic_class

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
        integer :: scope_id = 0  ! IPv6 scope ID
        integer :: flow_info = 0  ! IPv6 flow info
        integer :: traffic_class = 0  ! IPv6 traffic class
    contains
        procedure :: set_address => hostaddress_set
        procedure :: get_address => hostaddress_get
        procedure :: set_port => hostaddress_set_port
        procedure :: get_port => hostaddress_get_port
        procedure :: resolve => hostaddress_resolve
        procedure :: is_ipv6_addr => hostaddress_is_ipv6
        procedure :: to_string => hostaddress_to_string
        procedure :: parse_address => hostaddress_parse
        procedure :: is_valid => hostaddress_is_valid
        procedure :: set_scope_id => hostaddress_set_scope_id
        procedure :: get_scope_id => hostaddress_get_scope_id
        procedure :: set_flow_info => hostaddress_set_flow_info
        procedure :: get_flow_info => hostaddress_get_flow_info
        procedure :: set_traffic_class => hostaddress_set_traffic_class
        procedure :: get_traffic_class => hostaddress_get_traffic_class
        procedure :: clear => hostaddress_clear
        procedure :: is_null => hostaddress_is_null
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
        logical :: dual_stack_enabled = .true.  ! Enable IPv4/IPv6 dual-stack by default
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
        procedure :: set_dual_stack => tcpsocket_set_dual_stack
        procedure :: is_dual_stack => tcpsocket_is_dual_stack
    end type QTcpSocket

    !> @brief UDP socket with multicast support
    type :: QUdpSocket
        private
        type(c_ptr) :: socket_handle = c_null_ptr
        type(SocketState) :: state
        logical :: winsock_initialized = .false.
        logical :: dual_stack_enabled = .true.  ! Enable IPv4/IPv6 dual-stack by default
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
        procedure :: set_dual_stack => udpsocket_set_dual_stack
        procedure :: is_dual_stack => udpsocket_is_dual_stack
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
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        character(len=256) :: resolved_ip
        logical :: success

        ! Initialize Winsock if not already done
        if (.not. winsock_initialized()) then
            success = socket_init()
            if (.not. success) then
                this%address = ""
                this%is_ipv6 = .false.
                return
            end if
        end if

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

    !> @brief Resolve hostname to IPv6 address specifically
    subroutine hostaddress_resolve_ipv6(this, hostname)
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        character(len=256) :: resolved_ip
        logical :: success

        success = resolve_hostname_ipv6(hostname, this%port, resolved_ip)
        if (success) then
            this%address = trim(resolved_ip)
            this%is_ipv6 = .true.
        else
            this%address = ""
            this%is_ipv6 = .false.
        end if
    end subroutine hostaddress_resolve_ipv6

    !> @brief Resolve hostname to IPv4 address specifically
    subroutine hostaddress_resolve_ipv4(this, hostname)
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        character(len=256) :: resolved_ip
        logical :: success

        success = resolve_hostname_ipv4(hostname, this%port, resolved_ip)
        if (success) then
            this%address = trim(resolved_ip)
            this%is_ipv6 = .false.
        else
            this%address = ""
            this%is_ipv6 = .false.
        end if
    end subroutine hostaddress_resolve_ipv4

    !> @brief Resolve hostname to IPv4 and IPv6 addresses
    subroutine hostaddress_resolve_dual(this, hostname, ipv4_addr, ipv6_addr)
        use forge_winsock
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        type(QHostAddress), intent(out), optional :: ipv4_addr, ipv6_addr
        character(len=256) :: ipv4_ip, ipv6_ip
        logical :: ipv4_success, ipv6_success

        ! Try IPv4 resolution
        ipv4_success = resolve_hostname_ipv4(hostname, this%port, ipv4_ip)
        if (ipv4_success .and. present(ipv4_addr)) then
            ipv4_addr%address = trim(ipv4_ip)
            ipv4_addr%port = this%port
            ipv4_addr%is_ipv6 = .false.
        end if

        ! Try IPv6 resolution
        ipv6_success = resolve_hostname_ipv6(hostname, this%port, ipv6_ip)
        if (ipv6_success .and. present(ipv6_addr)) then
            ipv6_addr%address = trim(ipv6_ip)
            ipv6_addr%port = this%port
            ipv6_addr%is_ipv6 = .true.
        end if

        ! Set this to first successful resolution (prefer IPv4 for compatibility)
        if (ipv4_success) then
            this%address = trim(ipv4_ip)
            this%is_ipv6 = .false.
        else if (ipv6_success) then
            this%address = trim(ipv6_ip)
            this%is_ipv6 = .true.
        else
            this%address = ""
            this%is_ipv6 = .false.
        end if
    end subroutine hostaddress_resolve_dual

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

    subroutine hostaddress_parse(this, address_string)
        class(QHostAddress), intent(inout) :: this
        character(len=*), intent(in) :: address_string
        integer :: colon_pos, percent_pos, bracket_start, bracket_end
        character(len=:), allocatable :: addr_part, port_part, scope_part

        ! Clear current state
        call this%clear()

        ! Check for IPv6 in brackets [addr]:port or addr%scope
        bracket_start = index(address_string, '[')
        bracket_end = index(address_string, ']')

        if (bracket_start > 0 .and. bracket_end > bracket_start) then
            ! IPv6 with brackets: [addr]:port or [addr%scope]:port
            addr_part = address_string(bracket_start+1:bracket_end-1)

            ! Check for port after brackets
            if (bracket_end < len(address_string) .and. address_string(bracket_end+1:bracket_end+1) == ':') then
                port_part = address_string(bracket_end+2:)
                read(port_part, *, iostat=ios) this%port
            end if
        else
            ! IPv4 or bare IPv6
            colon_pos = index(address_string, ':', back=.true.)
            percent_pos = index(address_string, '%')

            if (colon_pos > 0) then
                if (percent_pos > 0 .and. percent_pos > colon_pos) then
                    ! IPv6 with scope: addr%scope
                    addr_part = address_string(1:percent_pos-1)
                    scope_part = address_string(percent_pos+1:)
                    read(scope_part, *, iostat=ios) this%scope_id
                else
                    ! Could be IPv6 or IPv4:port
                    addr_part = address_string(1:colon_pos-1)
                    port_part = address_string(colon_pos+1:)
                    read(port_part, *, iostat=ios) this%port

                    ! Check if addr_part contains ':' (IPv6)
                    if (index(addr_part, ':') > 0) then
                        this%is_ipv6 = .true.
                    end if
                end if
            else
                ! No colon, just address
                addr_part = trim(address_string)
                if (index(addr_part, ':') > 0) then
                    this%is_ipv6 = .true.
                end if
            end if
        end if

        ! Set the address
        this%address = trim(addr_part)
        this%is_ipv6 = (index(this%address, ':') > 0)
    end subroutine hostaddress_parse

    function hostaddress_is_valid(this) result(valid)
        class(QHostAddress), intent(in) :: this
        logical :: valid

        if (len_trim(this%address) == 0) then
            valid = .false.
            return
        end if

        if (this%is_ipv6) then
            valid = is_valid_ipv6(this%address)
        else
            valid = is_valid_ipv4(this%address)
        end if
    end function hostaddress_is_valid

    function is_valid_ipv4(address) result(valid)
        character(len=*), intent(in) :: address
        logical :: valid
        integer :: dot_count, i, num
        character(len=1) :: c

        valid = .false.
        dot_count = 0

        do i = 1, len_trim(address)
            c = address(i:i)
            if (c == '.') then
                dot_count = dot_count + 1
                if (dot_count > 3) return
            else if (.not. is_digit(c)) then
                return
            end if
        end do

        if (dot_count /= 3) return

        ! Parse each octet
        read(address, *, iostat=ios) num
        if (ios /= 0 .or. num < 0 .or. num > 255) return

        valid = .true.
    end function is_valid_ipv4

    function is_valid_ipv6(address) result(valid)
        character(len=*), intent(in) :: address
        logical :: valid
        integer :: colon_count, double_colon_count, i
        character(len=1) :: c

        valid = .false.
        colon_count = 0
        double_colon_count = 0

        do i = 1, len_trim(address)
            c = address(i:i)
            if (c == ':') then
                colon_count = colon_count + 1
                if (i < len_trim(address) .and. address(i+1:i+1) == ':') then
                    double_colon_count = double_colon_count + 1
                    if (double_colon_count > 1) return
                end if
            else if (.not. is_hex_digit(c)) then
                return
            end if
        end do

        ! IPv6 should have 7 or 8 colons, or fewer with ::
        if (colon_count < 2 .or. colon_count > 7) return
        if (double_colon_count > 1) return

        valid = .true.
    end function is_valid_ipv6

    function is_digit(c) result(digit)
        character(len=1), intent(in) :: c
        logical :: digit
        digit = (c >= '0' .and. c <= '9')
    end function is_digit

    function is_hex_digit(c) result(hex_digit)
        character(len=1), intent(in) :: c
        logical :: hex_digit
        hex_digit = is_digit(c) .or. (c >= 'a' .and. c <= 'f') .or. (c >= 'A' .and. c <= 'F')
    end function is_hex_digit

    subroutine hostaddress_set_scope_id(this, scope_id)
        class(QHostAddress), intent(inout) :: this
        integer, intent(in) :: scope_id
        this%scope_id = scope_id
    end subroutine hostaddress_set_scope_id

    function hostaddress_get_scope_id(this) result(scope_id)
        class(QHostAddress), intent(in) :: this
        integer :: scope_id
        scope_id = this%scope_id
    end function hostaddress_get_scope_id

    subroutine hostaddress_set_flow_info(this, flow_info)
        class(QHostAddress), intent(inout) :: this
        integer, intent(in) :: flow_info
        this%flow_info = flow_info
    end subroutine hostaddress_set_flow_info

    function hostaddress_get_flow_info(this) result(flow_info)
        class(QHostAddress), intent(in) :: this
        integer :: flow_info
        flow_info = this%flow_info
    end function hostaddress_get_flow_info

    subroutine hostaddress_set_traffic_class(this, traffic_class)
        class(QHostAddress), intent(inout) :: this
        integer, intent(in) :: traffic_class
        this%traffic_class = traffic_class
    end subroutine hostaddress_set_traffic_class

    function hostaddress_get_traffic_class(this) result(traffic_class)
        class(QHostAddress), intent(in) :: this
        integer :: traffic_class
        traffic_class = this%traffic_class
    end function hostaddress_get_traffic_class

    subroutine hostaddress_clear(this)
        class(QHostAddress), intent(inout) :: this
        this%address = ""
        this%port = 0
        this%is_ipv6 = .false.
        this%scope_id = 0
        this%flow_info = 0
        this%traffic_class = 0
    end subroutine hostaddress_clear

    function hostaddress_is_null(this) result(is_null)
        class(QHostAddress), intent(in) :: this
        logical :: is_null
        is_null = (len_trim(this%address) == 0)
    end function hostaddress_is_null

    function int_to_string(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
        str = trim(adjustl(str))
    end function int_to_string

    ! ========== QTcpSocket Implementation ==========

    subroutine tcpsocket_connect(this, host, port)
        class(QTcpSocket), intent(inout) :: this
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        logical :: success

        ! Initialize socket subsystem if not already done
        if (.not. this%winsock_initialized) then
            success = winsock_init()
            if (.not. success) then
                this%state%value = UnconnectedState
                this%error_code = -1
                this%error_string = "Failed to initialize Winsock"
                call this%error_occurred%emit(this%error_code)
                return
            end if
            this%winsock_initialized = .true.
        end if

        ! Create dual-stack socket for IPv4/IPv6 support
        this%socket_handle = create_dual_socket(IPPROTO_TCP)
        if (.not. c_associated(this%socket_handle)) then
            this%state%value = UnconnectedState
            this%error_code = -2
            this%error_string = "Failed to create socket"
            call this%error_occurred%emit(this%error_code)
            return
        end if

        this%state%value = ConnectingState
        call this%peer_address%set(host, port)
        call this%state_changed%emit()

        ! Connect to host (supports both IPv4 and IPv6)
        success = socket_connect(this%socket_handle, host, port)

        if (success) then
            this%state%value = ConnectedState
            call this%local_address%set("", 0)  ! Would need to get actual local address
            call this%connected%emit()
            call this%state_changed%emit()
        else
            this%error_code = -3
            this%error_string = "Connection failed"
            call this%error_occurred%emit(this%error_code)
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            this%state%value = UnconnectedState
            call this%state_changed%emit()
        end if
    end subroutine tcpsocket_connect

    subroutine tcpsocket_disconnect(this)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this

        if (this%state%value == ConnectedState .or. this%state%value == ConnectingState) then
            this%state%value = ClosingState
            call this%state_changed%emit()

            if (c_associated(this%socket_handle)) then
                call socket_close(this%socket_handle)
                this%socket_handle = c_null_ptr
            end if

            call this%disconnected%emit()
            this%state%value = UnconnectedState
            call this%state_changed%emit()
        end if
    end subroutine tcpsocket_disconnect

    function tcpsocket_write(this, data) result(bytes_written)
        use forge_winsock
        class(QTcpSocket), intent(inout) :: this
        character(len=*), intent(in) :: data
        integer :: bytes_written

        if (this%state%value /= ConnectedState) then
            bytes_written = -1
            this%error_code = -4
            this%error_string = "Socket not connected"
            call this%error_occurred%emit(this%error_code)
            return
        end if

        bytes_written = socket_send(this%socket_handle, data, len(data))

        if (bytes_written > 0) then
            call this%bytes_written%emit(bytes_written)
        else if (bytes_written < 0) then
            this%error_code = -5
            this%error_string = "Send failed"
            call this%error_occurred%emit(this%error_code)
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
            call this%ready_read%emit()
        else if (bytes_read == 0) then
            ! Connection closed
            call this%disconnect_from_host()
            allocate(character(len=0) :: data)
        else
            this%error_code = -6
            this%error_string = "Receive failed"
            call this%error_occurred%emit(this%error_code)
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
        class(QTcpSocket), intent(inout) :: this
        integer, intent(in) :: port
        integer, intent(in), optional :: backlog
        logical :: success
        integer :: max_conn

        max_conn = 5
        if (present(backlog)) max_conn = backlog

        ! Initialize socket subsystem
        if (.not. this%winsock_initialized) then
            success = winsock_init()
            if (.not. success) then
                this%error_code = -7
                this%error_string = "Failed to initialize Winsock"
                call this%error_occurred%emit(this%error_code)
                return
            end if
            this%winsock_initialized = .true.
        end if

        ! Create dual-stack socket for IPv4/IPv6 support
        this%socket_handle = create_dual_socket(IPPROTO_TCP)
        if (.not. c_associated(this%socket_handle)) then
            this%error_code = -1
            this%error_string = "Failed to create listening socket"
            call this%error_occurred%emit(this%error_code)
            return
        end if

        success = socket_bind(this%socket_handle, port)
        if (.not. success) then
            this%error_code = -2
            this%error_string = "Failed to bind to port"
            call this%error_occurred%emit(this%error_code)
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            return
        end if

        success = socket_listen(this%socket_handle, max_conn)
        if (.not. success) then
            this%error_code = -3
            this%error_string = "Failed to listen on socket"
            call this%error_occurred%emit(this%error_code)
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
            call client_socket%error_occurred%emit(client_socket%error_code)
            return
        end if

        client_handle = socket_accept(this%socket_handle)
        if (.not. c_associated(client_handle)) then
            client_socket%error_code = -5
            client_socket%error_string = "Accept failed"
            call client_socket%error_occurred%emit(client_socket%error_code)
            return
        end if

        ! Initialize client socket
        client_socket%socket_handle = client_handle
        client_socket%state%value = ConnectedState
        client_socket%winsock_initialized = .true.
        client_socket%is_server_socket = .false.
        client_socket%dual_stack_enabled = this%dual_stack_enabled
        ! Would need to get peer address from accept - simplified for now
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
        class(QTcpSocket), intent(inout) :: this
        logical :: success

        if (.not. this%winsock_initialized) then
            success = socket_init()
            if (.not. success) then
                this%error_code = -7
                this%error_string = "Failed to initialize socket subsystem"
            else
                this%winsock_initialized = .true.
            end if
        end if
    end subroutine init_winsock

    subroutine tcpsocket_set_dual_stack(this, dual_stack)
        class(QTcpSocket), intent(inout) :: this
        logical, intent(in) :: dual_stack

        this%dual_stack_enabled = dual_stack

        ! Apply to existing socket if connected
        if (c_associated(this%socket_handle)) then
            call set_dual_stack(this%socket_handle, dual_stack)
        end if
    end subroutine tcpsocket_set_dual_stack

    function tcpsocket_is_dual_stack(this) result(dual_stack)
        class(QTcpSocket), intent(in) :: this
        logical :: dual_stack
        dual_stack = this%dual_stack_enabled
    end function tcpsocket_is_dual_stack

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
        class(QUdpSocket), intent(inout) :: this
        integer, intent(in) :: port
        integer, intent(in), optional :: mode
        logical :: success

        call this%init_socket()

        ! Create dual-stack UDP socket for IPv4/IPv6 support
        this%socket_handle = create_dual_socket(IPPROTO_UDP)
        if (.not. c_associated(this%socket_handle)) then
            this%state%value = UnconnectedState
            this%error_code = -1
            this%error_string = "Failed to create UDP socket"
            call this%error_occurred%emit(this%error_code)
            return
        end if

        ! Bind to port (supports both IPv4 and IPv6)
        success = socket_bind(this%socket_handle, port)

        if (success) then
            this%state%value = BoundState
            call this%bound_address%set("", port)
            call this%state_changed%emit()
        else
            this%error_code = -2
            this%error_string = "Failed to bind UDP socket"
            call this%error_occurred%emit(this%error_code)
            call socket_close(this%socket_handle)
            this%socket_handle = c_null_ptr
            this%state%value = UnconnectedState
        end if
    end subroutine udpsocket_bind

    function udpsocket_write_datagram(this, data, host, port) result(bytes_written)
        class(QUdpSocket), intent(inout) :: this
        character(len=*), intent(in) :: data
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        integer :: bytes_written

        call this%init_socket()

        ! Create socket if not already created
        if (.not. c_associated(this%socket_handle)) then
            this%socket_handle = create_dual_socket(IPPROTO_UDP)
            if (.not. c_associated(this%socket_handle)) then
                bytes_written = -1
                this%error_code = -3
                this%error_string = "Failed to create UDP socket for sending"
                call this%error_occurred%emit(this%error_code)
                return
            end if
        end if

        bytes_written = socket_sendto(this%socket_handle, data, len(data), host, port)

        if (bytes_written > 0) then
            call this%bytes_written%emit(bytes_written)
        else if (bytes_written < 0) then
            this%error_code = -4
            this%error_string = "Failed to send UDP datagram"
            call this%error_occurred%emit(this%error_code)
        end if
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
        else if (bytes_read < 0) then
            this%error_code = -5
            this%error_string = "Failed to receive UDP datagram"
            call this%error_occurred%emit(this%error_code)
            allocate(character(len=0) :: data)
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

    subroutine udpsocket_set_dual_stack(this, dual_stack)
        class(QUdpSocket), intent(inout) :: this
        logical, intent(in) :: dual_stack

        this%dual_stack_enabled = dual_stack

        ! Apply to existing socket if bound
        if (c_associated(this%socket_handle)) then
            call set_dual_stack(this%socket_handle, dual_stack)
        end if
    end subroutine udpsocket_set_dual_stack

    function udpsocket_is_dual_stack(this) result(dual_stack)
        class(QUdpSocket), intent(in) :: this
        logical :: dual_stack
        dual_stack = this%dual_stack_enabled
    end function udpsocket_is_dual_stack

! ========== Network Interface Support ==========

!> @brief Network interface information structure
type :: NetworkInterface
    character(len=256) :: name = ""
    character(len=45) :: address = ""
    character(len=45) :: netmask = ""
    character(len=45) :: broadcast = ""
    integer :: index = 0
    logical :: is_up = .false.
    logical :: is_ipv6 = .false.
end type NetworkInterface

!> @brief Enumerate all available network interfaces
function enumerate_interfaces() result(interfaces)
    type(NetworkInterface), allocatable :: interfaces(:)
    interfaces = enumerate_interfaces()
end function enumerate_interfaces

!> @brief Get information about a specific network interface
function get_interface_info(if_index) result(iface)
    integer, intent(in) :: if_index
    type(NetworkInterface) :: iface
    iface = get_interface_info(if_index)
end function get_interface_info

! ========== IPv6 Socket Options ==========

!> @brief Set IPv6-only socket option
function set_ipv6_only(sock, ipv6_only) result(success)
    type(c_ptr), intent(in) :: sock
    logical, intent(in) :: ipv6_only
    logical :: success
    success = set_ipv6_only(sock, ipv6_only)
end function set_ipv6_only

!> @brief Get IPv6-only socket option
function get_ipv6_only(sock) result(ipv6_only)
    type(c_ptr), intent(in) :: sock
    logical :: ipv6_only
    ipv6_only = get_ipv6_only(sock)
end function get_ipv6_only

!> @brief Set dual-stack socket option (IPv4/IPv6)
function set_dual_stack(sock, dual_stack) result(success)
    type(c_ptr), intent(in) :: sock
    logical, intent(in) :: dual_stack
    logical :: success
    success = set_dual_stack(sock, dual_stack)
end function set_dual_stack

!> @brief Set IPv6 scope ID
function set_ipv6_scope_id(sock, scope_id) result(success)
    type(c_ptr), intent(in) :: sock
    integer, intent(in) :: scope_id
    logical :: success
    success = set_ipv6_scope_id(sock, scope_id)
end function set_ipv6_scope_id

!> @brief Get IPv6 scope ID
function get_ipv6_scope_id(sock) result(scope_id)
    type(c_ptr), intent(in) :: sock
    integer :: scope_id
    scope_id = get_ipv6_scope_id(sock)
end function get_ipv6_scope_id

!> @brief Set IPv6 flow info
function set_ipv6_flow_info(sock, flow_info) result(success)
    type(c_ptr), intent(in) :: sock
    integer, intent(in) :: flow_info
    logical :: success
    success = set_ipv6_flow_info(sock, flow_info)
end function set_ipv6_flow_info

!> @brief Get IPv6 flow info
function get_ipv6_flow_info(sock) result(flow_info)
    type(c_ptr), intent(in) :: sock
    integer :: flow_info
    flow_info = get_ipv6_flow_info(sock)
end function get_ipv6_flow_info

!> @brief Set IPv6 traffic class
function set_ipv6_traffic_class(sock, traffic_class) result(success)
    type(c_ptr), intent(in) :: sock
    integer, intent(in) :: traffic_class
    logical :: success
    success = set_ipv6_traffic_class(sock, traffic_class)
end function set_ipv6_traffic_class

!> @brief Get IPv6 traffic class
function get_ipv6_traffic_class(sock) result(traffic_class)
    type(c_ptr), intent(in) :: sock
    integer :: traffic_class
    traffic_class = get_ipv6_traffic_class(sock)
end function get_ipv6_traffic_class

end module forge_socket

