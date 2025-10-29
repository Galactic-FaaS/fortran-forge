!> @brief POSIX socket implementation for Linux/macOS
!> @details Full TCP/UDP socket support via POSIX socket API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_posix_socket
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: posix_init, posix_cleanup
    public :: create_tcp_socket, create_udp_socket, create_dual_socket
    public :: socket_connect, socket_bind, socket_listen, socket_accept
    public :: socket_send, socket_recv, socket_sendto, socket_recvfrom
    public :: socket_close, socket_set_nonblocking
    public :: resolve_hostname, resolve_hostname_ipv4, resolve_hostname_ipv6
    public :: get_ip_string, set_multicast_group
    public :: set_ipv6_only, get_ipv6_only, set_dual_stack
    public :: set_ipv6_scope_id, set_ipv6_flow_info, set_ipv6_traffic_class
    public :: get_ipv6_scope_id, get_ipv6_flow_info, get_ipv6_traffic_class
    public :: enumerate_interfaces, get_interface_info
    public :: SOCKET_TYPE, INVALID_SOCKET

    !> Socket type
    type(c_ptr) :: SOCKET_TYPE
    type(c_ptr), parameter :: INVALID_SOCKET = c_null_ptr

    !> POSIX constants
    integer(c_int), parameter :: AF_INET = 2
    integer(c_int), parameter :: AF_INET6 = 10
    integer(c_int), parameter :: SOCK_STREAM = 1
    integer(c_int), parameter :: SOCK_DGRAM = 2
    integer(c_int), parameter :: IPPROTO_TCP = 6
    integer(c_int), parameter :: IPPROTO_UDP = 17
    integer(c_int), parameter :: IPPROTO_IP = 0
    integer(c_int), parameter :: SOMAXCONN = 128
    integer(c_int), parameter :: FIONBIO = int(z'5421', c_int)

    !> sockaddr_in structure for IPv4
    type, bind(C) :: sockaddr_in
        integer(c_short) :: sin_family
        integer(c_short) :: sin_port  ! Network byte order
        integer(c_int) :: sin_addr    ! Network byte order
        character(kind=c_char) :: sin_zero(8)
    end type sockaddr_in

    !> in_addr structure for IPv4
    type, bind(C) :: in_addr
        integer(c_int) :: s_addr
    end type in_addr

    !> in6_addr structure for IPv6
    type, bind(C) :: in6_addr
        character(kind=c_char) :: s6_addr(16)  ! 16 bytes for IPv6
    end type in6_addr

    !> sockaddr_in6 structure for IPv6
    type, bind(C) :: sockaddr_in6
        integer(c_short) :: sin6_family
        integer(c_short) :: sin6_port
        integer(c_int) :: sin6_flowinfo
        type(in6_addr) :: sin6_addr
        integer(c_int) :: sin6_scope_id
    end type sockaddr_in6

    !> addrinfo structure for getaddrinfo
    type, bind(C) :: addrinfo
        integer(c_int) :: ai_flags
        integer(c_int) :: ai_family
        integer(c_int) :: ai_socktype
        integer(c_int) :: ai_protocol
        integer(c_int) :: ai_addrlen
        type(c_ptr) :: ai_addr
        type(c_ptr) :: ai_canonname
        type(c_ptr) :: ai_next
    end type addrinfo

    !> POSIX socket API bindings
    interface

        ! Socket creation and management
        function socket(af, socket_type, protocol) bind(C, name="socket")
            import :: c_int, c_ptr
            integer(c_int), value :: af, socket_type, protocol
            type(c_ptr) :: socket
        end function socket

        function close(sock) bind(C, name="close")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock
            integer(c_int) :: close
        end function close

        ! Connection operations
        function connect(sock, name, namelen) bind(C, name="connect")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, name
            integer(c_int), value :: namelen
            integer(c_int) :: connect
        end function connect

        function bind(sock, name, namelen) bind(C, name="bind")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, name
            integer(c_int), value :: namelen
            integer(c_int) :: bind
        end function bind

        function listen(sock, backlog) bind(C, name="listen")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock
            integer(c_int), value :: backlog
            integer(c_int) :: listen
        end function listen

        function accept(sock, addr, addrlen) bind(C, name="accept")
            import :: c_ptr
            type(c_ptr), value :: sock, addr, addrlen
            type(c_ptr) :: accept
        end function accept

        ! Data transfer
        function send(sock, buf, len, flags) bind(C, name="send")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, buf
            integer(c_int), value :: len, flags
            integer(c_int) :: send
        end function send

        function recv(sock, buf, len, flags) bind(C, name="recv")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, buf
            integer(c_int), value :: len, flags
            integer(c_int) :: recv
        end function recv

        function sendto(sock, buf, len, flags, to_addr, tolen) bind(C, name="sendto")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, buf, to_addr
            integer(c_int), value :: len, flags, tolen
            integer(c_int) :: sendto
        end function sendto

        function recvfrom(sock, buf, len, flags, from_addr, fromlen) bind(C, name="recvfrom")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, buf, from_addr, fromlen
            integer(c_int), value :: len, flags
            integer(c_int) :: recvfrom
        end function recvfrom

        ! Socket options
        function fcntl(sock, cmd, arg) bind(C, name="fcntl")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock
            integer(c_int), value :: cmd, arg
            integer(c_int) :: fcntl
        end function fcntl

        ! Host/network conversion
        function htons(hostshort) bind(C, name="htons")
            import :: c_short
            integer(c_short), value :: hostshort
            integer(c_short) :: htons
        end function htons

        function htonl(hostlong) bind(C, name="htonl")
            import :: c_int
            integer(c_int), value :: hostlong
            integer(c_int) :: htonl
        end function htonl

        function ntohs(netshort) bind(C, name="ntohs")
            import :: c_short
            integer(c_short), value :: netshort
            integer(c_short) :: ntohs
        end function ntohs

        function ntohl(netlong) bind(C, name="ntohl")
            import :: c_int
            integer(c_int), value :: netlong
            integer(c_int) :: ntohl
        end function ntohl

        ! Name resolution
        function getaddrinfo(pNodeName, pServiceName, pHints, ppResult) bind(C, name="getaddrinfo")
            import :: c_ptr, c_int
            type(c_ptr), value :: pNodeName, pServiceName, pHints, ppResult
            integer(c_int) :: getaddrinfo
        end function getaddrinfo

        subroutine freeaddrinfo(pAddrInfo) bind(C, name="freeaddrinfo")
            import :: c_ptr
            type(c_ptr), value :: pAddrInfo
        end subroutine freeaddrinfo

        function getnameinfo(pSockaddr, SockaddrLength, pNodeBuffer, NodeBufferSize, &
                           pServiceBuffer, ServiceBufferSize, Flags) bind(C, name="getnameinfo")
            import :: c_ptr, c_int
            type(c_ptr), value :: pSockaddr, pNodeBuffer, pServiceBuffer
            integer(c_int), value :: SockaddrLength, NodeBufferSize, ServiceBufferSize, Flags
            integer(c_int) :: getnameinfo
        end function getnameinfo

        function inet_pton(af, src, dst) bind(C, name="inet_pton")
            import :: c_ptr, c_int
            integer(c_int), value :: af
            type(c_ptr), value :: src, dst
            integer(c_int) :: inet_pton
        end function inet_pton

        function inet_ntop(af, src, dst, size) bind(C, name="inet_ntop")
            import :: c_ptr, c_int
            integer(c_int), value :: af
            type(c_ptr), value :: src, dst
            integer(c_int), value :: size
            type(c_ptr) :: inet_ntop
        end function inet_ntop

        ! Socket options
        function setsockopt(sock, level, optname, optval, optlen) bind(C, name="setsockopt")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, optval
            integer(c_int), value :: level, optname, optlen
            integer(c_int) :: setsockopt
        end function setsockopt

        function getsockopt(sock, level, optname, optval, optlen) bind(C, name="getsockopt")
            import :: c_ptr, c_int
            type(c_ptr), value :: sock, optval
            integer(c_int), value :: level, optname, optlen
            integer(c_int) :: getsockopt
        end function getsockopt

    end interface

contains

    !> @brief Initialize POSIX sockets (no-op on Unix-like systems)
    function posix_init() result(success)
        logical :: success
        success = .true.
    end function posix_init

    !> @brief Cleanup POSIX sockets (no-op on Unix-like systems)
    subroutine posix_cleanup()
        ! No cleanup needed on POSIX systems
    end subroutine posix_cleanup

    !> @brief Create TCP socket
    function create_tcp_socket() result(sock)
        type(c_ptr) :: sock

        sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)

        if (.not. c_associated(sock)) then
            write(*, '(A,I0)') "Failed to create TCP socket. Error: ", errno()
        end if
    end function create_tcp_socket

    !> @brief Create UDP socket
    function create_udp_socket() result(sock)
        type(c_ptr) :: sock

        sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)

        if (.not. c_associated(sock)) then
            write(*, '(A,I0)') "Failed to create UDP socket. Error: ", errno()
        end if
    end function create_udp_socket

    !> @brief Create dual-stack socket (IPv4/IPv6)
    function create_dual_socket(protocol) result(sock)
        integer, intent(in) :: protocol  ! IPPROTO_TCP or IPPROTO_UDP
        type(c_ptr) :: sock
        integer(c_int) :: af

        ! Try IPv6 first for dual-stack
        af = AF_INET6
        sock = socket(af, merge(SOCK_STREAM, SOCK_DGRAM, protocol == IPPROTO_TCP), protocol)

        if (.not. c_associated(sock)) then
            ! Fallback to IPv4
            af = AF_INET
            sock = socket(af, merge(SOCK_STREAM, SOCK_DGRAM, protocol == IPPROTO_TCP), protocol)

            if (.not. c_associated(sock)) then
                write(*, '(A,I0)') "Failed to create dual-stack socket. Error: ", errno()
            end if
        else
            ! Enable dual-stack on IPv6 socket
            call set_dual_stack(sock, .true.)
        end if
    end function create_dual_socket

    !> @brief Connect socket to host
    function socket_connect(sock, host, port) result(success)
        type(c_ptr), intent(in) :: sock
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        logical :: success
        type(sockaddr_in), target :: server_addr
        integer(c_int) :: result, addr_int
        character(len=:), allocatable, target :: host_c

        ! Setup address structure
        server_addr%sin_family = AF_INET
        server_addr%sin_port = htons(int(port, c_short))

        ! Convert hostname to IP
        host_c = trim(host) // c_null_char
        addr_int = inet_addr(c_loc(host_c))
        server_addr%sin_addr = addr_int
        server_addr%sin_zero = c_null_char

        ! Connect
        result = connect(sock, c_loc(server_addr), int(sizeof(server_addr), c_int))
        success = (result == 0)

        if (.not. success) then
            write(*, '(A,I0)') "Connect failed. Error: ", errno()
        end if
    end function socket_connect

    !> @brief Bind socket to port
    function socket_bind(sock, port) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: port
        logical :: success
        type(sockaddr_in), target :: server_addr
        integer(c_int) :: result

        server_addr%sin_family = AF_INET
        server_addr%sin_port = htons(int(port, c_short))
        server_addr%sin_addr = 0  ! INADDR_ANY
        server_addr%sin_zero = c_null_char

        result = bind(sock, c_loc(server_addr), int(sizeof(server_addr), c_int))
        success = (result == 0)

        if (.not. success) then
            write(*, '(A,I0)') "Bind failed. Error: ", errno()
        end if
    end function socket_bind

    !> @brief Listen for connections
    function socket_listen(sock, backlog) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in), optional :: backlog
        logical :: success
        integer(c_int) :: result, max_conn

        max_conn = SOMAXCONN
        if (present(backlog)) max_conn = backlog

        result = listen(sock, max_conn)
        success = (result == 0)

        if (.not. success) then
            write(*, '(A,I0)') "Listen failed. Error: ", errno()
        end if
    end function socket_listen

    !> @brief Accept incoming connection
    function socket_accept(sock) result(client_sock)
        type(c_ptr), intent(in) :: sock
        type(c_ptr) :: client_sock

        client_sock = accept(sock, c_null_ptr, c_null_ptr)

        if (.not. c_associated(client_sock)) then
            write(*, '(A,I0)') "Accept failed. Error: ", errno()
        end if
    end function socket_accept

    !> @brief Send data on TCP socket
    function socket_send(sock, data, length) result(bytes_sent)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(in) :: data
        integer, intent(in) :: length
        integer :: bytes_sent
        integer(c_int) :: result

        result = send(sock, c_loc(data), length, 0)
        bytes_sent = result

        if (result < 0) then
            write(*, '(A,I0)') "Send failed. Error: ", errno()
            bytes_sent = -1
        end if
    end function socket_send

    !> @brief Receive data from TCP socket
    function socket_recv(sock, buffer, buffer_size) result(bytes_received)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(inout) :: buffer
        integer, intent(in) :: buffer_size
        integer :: bytes_received
        integer(c_int) :: result

        result = recv(sock, c_loc(buffer), buffer_size, 0)
        bytes_received = result

        if (result < 0) then
            write(*, '(A,I0)') "Recv failed. Error: ", errno()
            bytes_received = -1
        else if (result == 0) then
            ! Connection closed
            bytes_received = 0
        end if
    end function socket_recv

    !> @brief Send UDP datagram
    function socket_sendto(sock, data, length, dest_ip, dest_port) result(bytes_sent)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(in) :: data
        integer, intent(in) :: length
        character(len=*), intent(in) :: dest_ip
        integer, intent(in) :: dest_port
        integer :: bytes_sent
        type(sockaddr_in), target :: dest_addr
        integer(c_int) :: result, addr_int
        character(len=:), allocatable, target :: ip_c

        dest_addr%sin_family = AF_INET
        dest_addr%sin_port = htons(int(dest_port, c_short))

        ip_c = trim(dest_ip) // c_null_char
        addr_int = inet_addr(c_loc(ip_c))
        dest_addr%sin_addr = addr_int
        dest_addr%sin_zero = c_null_char

        result = sendto(sock, c_loc(data), length, 0, c_loc(dest_addr), &
                       int(sizeof(dest_addr), c_int))
        bytes_sent = result

        if (result < 0) then
            write(*, '(A,I0)') "Sendto failed. Error: ", errno()
            bytes_sent = -1
        end if
    end function socket_sendto

    !> @brief Receive UDP datagram
    function socket_recvfrom(sock, buffer, buffer_size, source_ip, source_port) result(bytes_received)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(inout) :: buffer
        integer, intent(in) :: buffer_size
        character(len=*), intent(out), optional :: source_ip
        integer, intent(out), optional :: source_port
        integer :: bytes_received
        type(sockaddr_in), target :: from_addr
        integer(c_int), target :: fromlen
        integer(c_int) :: result

        fromlen = sizeof(from_addr)

        result = recvfrom(sock, c_loc(buffer), buffer_size, 0, &
                         c_loc(from_addr), c_loc(fromlen))
        bytes_received = result

        if (result < 0) then
            write(*, '(A,I0)') "Recvfrom failed. Error: ", errno()
            bytes_received = -1
        else
            if (present(source_port)) then
                source_port = ntohs(from_addr%sin_port)
            end if
            ! Convert from_addr%sin_addr to IP string for source_ip
            ! Note: This would require inet_ntoa or similar function
            ! For now, source_ip remains empty
        end if
    end function socket_recvfrom

    !> @brief Close socket
    subroutine socket_close(sock)
        type(c_ptr), intent(in) :: sock
        integer(c_int) :: result

        if (c_associated(sock)) then
            result = close(sock)
            if (result /= 0) then
                write(*, '(A,I0)') "Close socket failed. Error: ", errno()
            end if
        end if
    end subroutine socket_close

    !> @brief Set socket to non-blocking mode
    function socket_set_nonblocking(sock, nonblocking) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: nonblocking
        logical :: success
        integer(c_int) :: flags, result

        ! Get current flags
        flags = fcntl(sock, 3, 0)  ! F_GETFL
        if (flags < 0) then
            success = .false.
            write(*, '(A,I0)') "fcntl F_GETFL failed. Error: ", errno()
            return
        end if

        ! Set or clear O_NONBLOCK flag
        if (nonblocking) then
            flags = ior(flags, 2048)  ! O_NONBLOCK
        else
            flags = iand(flags, not(2048))  ! Clear O_NONBLOCK
        end if

        result = fcntl(sock, 4, flags)  ! F_SETFL
        success = (result == 0)

        if (.not. success) then
            write(*, '(A,I0)') "fcntl F_SETFL failed. Error: ", errno()
        end if
    end function socket_set_nonblocking

    !> @brief Resolve hostname to IP address (legacy, prefers IPv4)
    function resolve_hostname(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success

        ! Try IPv4 first for backward compatibility
        success = resolve_hostname_ipv4(hostname, port, ip_addr)
        if (.not. success) then
            success = resolve_hostname_ipv6(hostname, port, ip_addr)
        end if
    end function resolve_hostname

    !> @brief Resolve hostname to IPv4 address
    function resolve_hostname_ipv4(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success
        type(addrinfo), target :: hints
        type(c_ptr) :: result_addrinfo, addr_ptr
        type(sockaddr_in), pointer :: sockaddr_ipv4
        character(len=256), target :: ip_buffer
        integer(c_int) :: ret
        character(len=:), allocatable, target :: host_c
        type(c_ptr) :: hints_ptr, result_ptr

        success = .false.
        ip_addr = ""

        ! Setup hints for IPv4 only
        hints%ai_family = AF_INET
        hints%ai_socktype = SOCK_STREAM
        hints%ai_protocol = IPPROTO_TCP
        hints%ai_flags = 0

        host_c = trim(hostname) // c_null_char

        hints_ptr = c_loc(hints)
        result_ptr = c_null_ptr
        ret = getaddrinfo(c_loc(host_c), c_null_ptr, hints_ptr, result_ptr)

        if (ret /= 0) then
            return
        end if

        ! Get first IPv4 result
        addr_ptr = result_ptr
        if (c_associated(addr_ptr)) then
            call c_f_pointer(addr_ptr, sockaddr_ipv4)
            if (sockaddr_ipv4%sin_family == AF_INET) then
                ret = getnameinfo(addr_ptr, 16, &  ! sizeof(sockaddr_in)
                                c_loc(ip_buffer), len(ip_buffer), c_null_ptr, 0, 0)
                if (ret == 0) then
                    ip_addr = trim(ip_buffer)
                    success = .true.
                end if
            end if
        end if

        call freeaddrinfo(result_ptr)
    end function resolve_hostname_ipv4

    !> @brief Resolve hostname to IPv6 address
    function resolve_hostname_ipv6(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success
        type(addrinfo), target :: hints
        type(c_ptr) :: result_addrinfo, addr_ptr
        type(sockaddr_in6), pointer :: sockaddr_ipv6
        character(len=256), target :: ip_buffer
        integer(c_int) :: ret
        character(len=:), allocatable, target :: host_c
        type(c_ptr) :: hints_ptr, result_ptr

        success = .false.
        ip_addr = ""

        ! Setup hints for IPv6 only
        hints%ai_family = AF_INET6
        hints%ai_socktype = SOCK_STREAM
        hints%ai_protocol = IPPROTO_TCP
        hints%ai_flags = 0

        host_c = trim(hostname) // c_null_char

        hints_ptr = c_loc(hints)
        result_ptr = c_null_ptr
        ret = getaddrinfo(c_loc(host_c), c_null_ptr, hints_ptr, result_ptr)

        if (ret /= 0) then
            return
        end if

        ! Get first IPv6 result
        addr_ptr = result_ptr
        if (c_associated(addr_ptr)) then
            call c_f_pointer(addr_ptr, sockaddr_ipv6)
            if (sockaddr_ipv6%sin6_family == AF_INET6) then
                ret = getnameinfo(addr_ptr, 28, &  ! sizeof(sockaddr_in6)
                                c_loc(ip_buffer), len(ip_buffer), c_null_ptr, 0, 0)
                if (ret == 0) then
                    ip_addr = trim(ip_buffer)
                    success = .true.
                end if
            end if
        end if

        call freeaddrinfo(result_ptr)
    end function resolve_hostname_ipv6

    !> @brief Convert IP address to string
    function get_ip_string(sockaddr_ptr, family) result(ip_str)
        type(c_ptr), intent(in) :: sockaddr_ptr
        integer(c_int), intent(in) :: family
        character(len=:), allocatable :: ip_str
        character(len=256), target :: buffer
        integer(c_int) :: ret

        ip_str = ""

        if (family == AF_INET) then
            ret = getnameinfo(sockaddr_ptr, 16, &
                            c_loc(buffer), len(buffer), c_null_ptr, 0, 0)
        else if (family == AF_INET6) then
            ret = getnameinfo(sockaddr_ptr, 28, &
                            c_loc(buffer), len(buffer), c_null_ptr, 0, 0)
        else
            return
        end if

        if (ret == 0) then
            ip_str = trim(buffer)
        end if
    end function get_ip_string

    !> @brief Set multicast group membership
    function set_multicast_group(sock, group_addr, interface_addr, join) result(success)
        type(c_ptr), intent(in) :: sock
        character(len=*), intent(in) :: group_addr, interface_addr
        logical, intent(in) :: join
        logical :: success
        type :: ip_mreq
            integer(c_int) :: imr_multiaddr
            integer(c_int) :: imr_interface
        end type ip_mreq
        type(ip_mreq), target :: mreq
        integer(c_int) :: level, optname, ret
        character(len=:), allocatable, target :: group_c, iface_c

        success = .false.

        level = IPPROTO_IP
        if (join) then
            optname = 35  ! IP_ADD_MEMBERSHIP
        else
            optname = 36  ! IP_DROP_MEMBERSHIP
        end if

        group_c = trim(group_addr) // c_null_char
        iface_c = trim(interface_addr) // c_null_char

        mreq%imr_multiaddr = inet_addr(c_loc(group_c))
        mreq%imr_interface = inet_addr(c_loc(iface_c))

        ret = setsockopt(sock, level, optname, c_loc(mreq), int(sizeof(mreq), c_int))
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt multicast failed. Error: ", errno()
        end if
    end function set_multicast_group

    !> @brief Set IPv6-only socket option
    function set_ipv6_only(sock, ipv6_only) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: ipv6_only
        logical :: success
        integer(c_int), target :: opt_val
        integer(c_int) :: ret

        opt_val = merge(1, 0, ipv6_only)
        ret = setsockopt(sock, IPPROTO_IPV6, 26, c_loc(opt_val), int(sizeof(opt_val), c_int))  ! IPV6_V6ONLY
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt IPV6_V6ONLY failed. Error: ", errno()
        end if
    end function set_ipv6_only

    !> @brief Get IPv6-only socket option
    function get_ipv6_only(sock) result(ipv6_only)
        type(c_ptr), intent(in) :: sock
        logical :: ipv6_only
        integer(c_int), target :: opt_val
        integer(c_int), target :: opt_len
        integer(c_int) :: ret

        opt_len = sizeof(opt_val)
        ret = getsockopt(sock, IPPROTO_IPV6, 26, c_loc(opt_val), c_loc(opt_len))  ! IPV6_V6ONLY
        if (ret == 0) then
            ipv6_only = (opt_val /= 0)
        else
            ipv6_only = .false.
            write(*, '(A,I0)') "getsockopt IPV6_V6ONLY failed. Error: ", errno()
        end if
    end function get_ipv6_only

    !> @brief Set dual-stack socket option (IPv4/IPv6)
    function set_dual_stack(sock, dual_stack) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: dual_stack
        logical :: success

        ! Dual-stack is enabled by disabling IPv6-only
        success = set_ipv6_only(sock, .not. dual_stack)
    end function set_dual_stack

    !> @brief Set IPv6 scope ID
    function set_ipv6_scope_id(sock, scope_id) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: scope_id
        logical :: success
        integer(c_int), target :: opt_val
        integer(c_int) :: ret

        opt_val = scope_id
        ret = setsockopt(sock, IPPROTO_IPV6, 17, c_loc(opt_val), int(sizeof(opt_val), c_int))  ! IPV6_UNICAST_IF
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt IPV6_UNICAST_IF failed. Error: ", errno()
        end if
    end function set_ipv6_scope_id

    !> @brief Get IPv6 scope ID
    function get_ipv6_scope_id(sock) result(scope_id)
        type(c_ptr), intent(in) :: sock
        integer :: scope_id
        integer(c_int), target :: opt_val
        integer(c_int), target :: opt_len
        integer(c_int) :: ret

        opt_len = sizeof(opt_val)
        ret = getsockopt(sock, IPPROTO_IPV6, 17, c_loc(opt_val), c_loc(opt_len))  ! IPV6_UNICAST_IF
        if (ret == 0) then
            scope_id = opt_val
        else
            scope_id = 0
            write(*, '(A,I0)') "getsockopt IPV6_UNICAST_IF failed. Error: ", errno()
        end if
    end function get_ipv6_scope_id

    !> @brief Set IPv6 flow info
    function set_ipv6_flow_info(sock, flow_info) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: flow_info
        logical :: success
        integer(c_int), target :: opt_val
        integer(c_int) :: ret

        opt_val = flow_info
        ret = setsockopt(sock, IPPROTO_IPV6, 11, c_loc(opt_val), int(sizeof(opt_val), c_int))  ! IPV6_FLOWINFO
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt IPV6_FLOWINFO failed. Error: ", errno()
        end if
    end function set_ipv6_flow_info

    !> @brief Get IPv6 flow info
    function get_ipv6_flow_info(sock) result(flow_info)
        type(c_ptr), intent(in) :: sock
        integer :: flow_info
        integer(c_int), target :: opt_val
        integer(c_int), target :: opt_len
        integer(c_int) :: ret

        opt_len = sizeof(opt_val)
        ret = getsockopt(sock, IPPROTO_IPV6, 11, c_loc(opt_val), c_loc(opt_len))  ! IPV6_FLOWINFO
        if (ret == 0) then
            flow_info = opt_val
        else
            flow_info = 0
            write(*, '(A,I0)') "getsockopt IPV6_FLOWINFO failed. Error: ", errno()
        end if
    end function get_ipv6_flow_info

    !> @brief Set IPv6 traffic class
    function set_ipv6_traffic_class(sock, traffic_class) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: traffic_class
        logical :: success
        integer(c_int), target :: opt_val
        integer(c_int) :: ret

        opt_val = traffic_class
        ret = setsockopt(sock, IPPROTO_IPV6, 67, c_loc(opt_val), int(sizeof(opt_val), c_int))  ! IPV6_TCLASS
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt IPV6_TCLASS failed. Error: ", errno()
        end if
    end function set_ipv6_traffic_class

    !> @brief Get IPv6 traffic class
    function get_ipv6_traffic_class(sock) result(traffic_class)
        type(c_ptr), intent(in) :: sock
        integer :: traffic_class
        integer(c_int), target :: opt_val
        integer(c_int), target :: opt_len
        integer(c_int) :: ret

        opt_len = sizeof(opt_val)
        ret = getsockopt(sock, IPPROTO_IPV6, 67, c_loc(opt_val), c_loc(opt_len))  ! IPV6_TCLASS
        if (ret == 0) then
            traffic_class = opt_val
        else
            traffic_class = 0
            write(*, '(A,I0)') "getsockopt IPV6_TCLASS failed. Error: ", errno()
        end if
    end function get_ipv6_traffic_class

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

    !> @brief Enumerate network interfaces
    function enumerate_interfaces() result(interfaces)
        type(NetworkInterface), allocatable :: interfaces(:)
        ! POSIX-specific interface enumeration would go here
        ! For now, return empty array
        allocate(interfaces(0))
    end function enumerate_interfaces

    !> @brief Get interface information by index
    function get_interface_info(if_index) result(iface)
        integer, intent(in) :: if_index
        type(NetworkInterface) :: iface
        ! POSIX-specific interface info retrieval would go here
        iface%index = if_index
    end function get_interface_info

    !> @brief Get errno value
    function errno() result(err)
        integer :: err
        interface
            function c_errno() bind(C, name="__errno_location")
                import :: c_ptr
                type(c_ptr) :: c_errno
            end function c_errno
        end interface
        integer, pointer :: errno_ptr

        call c_f_pointer(c_errno(), errno_ptr)
        err = errno_ptr
    end function errno

end module forge_posix_socket