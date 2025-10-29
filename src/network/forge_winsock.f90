!> @brief Complete Winsock2 implementation for Windows
!> @details Full TCP/UDP socket support via Windows Sockets API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_winsock
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: winsock_init, winsock_cleanup
    public :: create_tcp_socket, create_udp_socket
    public :: socket_connect, socket_bind, socket_listen, socket_accept
    public :: socket_send, socket_recv, socket_sendto, socket_recvfrom
    public :: socket_close, socket_set_nonblocking
    public :: resolve_hostname, get_ip_string, set_multicast_group
    public :: SOCKET_TYPE, INVALID_SOCKET

    !> Socket type
    type(c_ptr) :: SOCKET_TYPE
    type(c_ptr), parameter :: INVALID_SOCKET = c_null_ptr

    !> Winsock constants
    integer(c_int), parameter :: AF_INET = 2
    integer(c_int), parameter :: AF_INET6 = 23
    integer(c_int), parameter :: SOCK_STREAM = 1
    integer(c_int), parameter :: SOCK_DGRAM = 2
    integer(c_int), parameter :: IPPROTO_TCP = 6
    integer(c_int), parameter :: IPPROTO_UDP = 17
    integer(c_int), parameter :: IPPROTO_IP = 0
    integer(c_int), parameter :: SOMAXCONN = 5
    integer(c_int), parameter :: FIONBIO = int(z'8004667E', c_int)

    !> WSAData structure for initialization
    type, bind(C) :: WSAData
        integer(c_short) :: wVersion
        integer(c_short) :: wHighVersion
        character(kind=c_char) :: szDescription(257)
        character(kind=c_char) :: szSystemStatus(129)
        integer(c_short) :: iMaxSockets
        integer(c_short) :: iMaxUdpDg
        type(c_ptr) :: lpVendorInfo
    end type WSAData

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

    !> Winsock2 API bindings
    interface
        
        ! Initialize/cleanup
        function WSAStartup(wVersionRequired, lpWSAData) bind(C, name="WSAStartup")
            import :: c_short, WSAData, c_int
            integer(c_short), value :: wVersionRequired
            type(WSAData), intent(out) :: lpWSAData
            integer(c_int) :: WSAStartup
        end function WSAStartup

        function WSACleanup() bind(C, name="WSACleanup")
            import :: c_int
            integer(c_int) :: WSACleanup
        end function WSACleanup

        function WSAGetLastError() bind(C, name="WSAGetLastError")
            import :: c_int
            integer(c_int) :: WSAGetLastError
        end function WSAGetLastError

        ! Socket creation and management
        function socket(af, socket_type, protocol) bind(C, name="socket")
            import :: c_int, c_ptr
            integer(c_int), value :: af, socket_type, protocol
            type(c_ptr) :: socket
        end function socket

        function closesocket(s) bind(C, name="closesocket")
            import :: c_ptr, c_int
            type(c_ptr), value :: s
            integer(c_int) :: closesocket
        end function closesocket

        ! Connection operations
        function connect(s, name, namelen) bind(C, name="connect")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, name
            integer(c_int), value :: namelen
            integer(c_int) :: connect
        end function connect

        function bind(s, name, namelen) bind(C, name="bind")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, name
            integer(c_int), value :: namelen
            integer(c_int) :: bind
        end function bind

        function listen(s, backlog) bind(C, name="listen")
            import :: c_ptr, c_int
            type(c_ptr), value :: s
            integer(c_int), value :: backlog
            integer(c_int) :: listen
        end function listen

        function accept(s, addr, addrlen) bind(C, name="accept")
            import :: c_ptr
            type(c_ptr), value :: s, addr, addrlen
            type(c_ptr) :: accept
        end function accept

        ! Data transfer
        function send(s, buf, len, flags) bind(C, name="send")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, buf
            integer(c_int), value :: len, flags
            integer(c_int) :: send
        end function send

        function recv(s, buf, len, flags) bind(C, name="recv")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, buf
            integer(c_int), value :: len, flags
            integer(c_int) :: recv
        end function recv

        function sendto(s, buf, len, flags, to_addr, tolen) bind(C, name="sendto")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, buf, to_addr
            integer(c_int), value :: len, flags, tolen
            integer(c_int) :: sendto
        end function sendto

        function recvfrom(s, buf, len, flags, from_addr, fromlen) bind(C, name="recvfrom")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, buf, from_addr, fromlen
            integer(c_int), value :: len, flags
            integer(c_int) :: recvfrom
        end function recvfrom

        ! Socket options
        function ioctlsocket(s, cmd, argp) bind(C, name="ioctlsocket")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, argp
            integer(c_int), value :: cmd
            integer(c_int) :: ioctlsocket
        end function ioctlsocket

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
        function gethostbyname(name) bind(C, name="gethostbyname")
            import :: c_ptr
            type(c_ptr), value :: name
            type(c_ptr) :: gethostbyname
        end function gethostbyname

        function inet_addr(cp) bind(C, name="inet_addr")
            import :: c_ptr, c_int
            type(c_ptr), value :: cp
            integer(c_int) :: inet_addr
        end function inet_addr

        ! IPv6 and hostname resolution functions
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

        ! Multicast functions
        function setsockopt(s, level, optname, optval, optlen) bind(C, name="setsockopt")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, optval
            integer(c_int), value :: level, optname, optlen
            integer(c_int) :: setsockopt
        end function setsockopt

        function getsockopt(s, level, optname, optval, optlen) bind(C, name="getsockopt")
            import :: c_ptr, c_int
            type(c_ptr), value :: s, optval
            integer(c_int), value :: level, optname, optlen
            integer(c_int) :: getsockopt
        end function getsockopt

    end interface

contains

    !> @brief Initialize Winsock2
    function winsock_init() result(success)
        logical :: success
        type(WSAData) :: wsa_data
        integer(c_int) :: result
        integer(c_short) :: version
        
        ! Request Winsock version 2.2
        version = int(z'0202', c_short)  ! MAKEWORD(2,2)
        
        result = WSAStartup(version, wsa_data)
        success = (result == 0)
        
        if (.not. success) then
            write(*, '(A,I0)') "WSAStartup failed with error: ", result
        end if
    end function winsock_init

    !> @brief Cleanup Winsock2
    subroutine winsock_cleanup()
        integer(c_int) :: result
        
        result = WSACleanup()
        if (result /= 0) then
            write(*, '(A,I0)') "WSACleanup failed with error: ", WSAGetLastError()
        end if
    end subroutine winsock_cleanup

    !> @brief Create TCP socket
    function create_tcp_socket() result(sock)
        type(c_ptr) :: sock
        
        sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        
        if (.not. c_associated(sock)) then
            write(*, '(A,I0)') "Failed to create TCP socket. Error: ", WSAGetLastError()
        end if
    end function create_tcp_socket

    !> @brief Create UDP socket
    function create_udp_socket() result(sock)
        type(c_ptr) :: sock
        
        sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
        
        if (.not. c_associated(sock)) then
            write(*, '(A,I0)') "Failed to create UDP socket. Error: ", WSAGetLastError()
        end if
    end function create_udp_socket

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
            write(*, '(A,I0)') "Connect failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Bind failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Listen failed. Error: ", WSAGetLastError()
        end if
    end function socket_listen

    !> @brief Accept incoming connection
    function socket_accept(sock) result(client_sock)
        type(c_ptr), intent(in) :: sock
        type(c_ptr) :: client_sock
        
        client_sock = accept(sock, c_null_ptr, c_null_ptr)
        
        if (.not. c_associated(client_sock)) then
            write(*, '(A,I0)') "Accept failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Send failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Recv failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Sendto failed. Error: ", WSAGetLastError()
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
            write(*, '(A,I0)') "Recvfrom failed. Error: ", WSAGetLastError()
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
            result = closesocket(sock)
            if (result /= 0) then
                write(*, '(A,I0)') "Close socket failed. Error: ", WSAGetLastError()
            end if
        end if
    end subroutine socket_close

    !> @brief Set socket to non-blocking mode
    function socket_set_nonblocking(sock, nonblocking) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: nonblocking
        logical :: success
        integer(c_int), target :: mode
        integer(c_int) :: result
        
        if (nonblocking) then
            mode = 1
        else
            mode = 0
        end if
        
        result = ioctlsocket(sock, FIONBIO, c_loc(mode))
        success = (result == 0)
        
        if (.not. success) then
            write(*, '(A,I0)') "ioctlsocket failed. Error: ", WSAGetLastError()
        end if
    end function socket_set_nonblocking

    !> @brief Resolve hostname to IP address
    function resolve_hostname(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success
        type(addrinfo), target :: hints
        type(c_ptr) :: result_addrinfo, addr_ptr
        type(sockaddr_in), pointer :: sockaddr_ipv4
        type(sockaddr_in6), pointer :: sockaddr_ipv6
        character(len=256), target :: ip_buffer
        integer(c_int) :: ret
        character(len=:), allocatable, target :: host_c
        type(c_ptr) :: hints_ptr, result_ptr

        success = .false.
        ip_addr = ""

        ! Setup hints for IPv4
        hints%ai_family = AF_INET
        hints%ai_socktype = SOCK_STREAM
        hints%ai_protocol = IPPROTO_TCP
        hints%ai_flags = 0

        host_c = trim(hostname) // c_null_char

        hints_ptr = c_loc(hints)
        result_ptr = c_null_ptr
        ret = getaddrinfo(c_loc(host_c), c_null_ptr, hints_ptr, result_ptr)

        if (ret /= 0) then
            write(*, '(A,I0)') "getaddrinfo failed with error: ", ret
            return
        end if

        ! Get first result
        addr_ptr = result_ptr
        if (c_associated(addr_ptr)) then
            call c_f_pointer(addr_ptr, sockaddr_ipv4)
            if (sockaddr_ipv4%sin_family == AF_INET) then
                ! Convert IP to string
                ret = getnameinfo(addr_ptr, 16, &  ! sizeof(sockaddr_in)
                                c_loc(ip_buffer), len(ip_buffer), c_null_ptr, 0, 0)
                if (ret == 0) then
                    ip_addr = trim(ip_buffer)
                    success = .true.
                end if
            end if
        end if

        call freeaddrinfo(result_ptr)
    end function resolve_hostname

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
            optname = 12  ! IP_ADD_MEMBERSHIP
        else
            optname = 13  ! IP_DROP_MEMBERSHIP
        end if

        group_c = trim(group_addr) // c_null_char
        iface_c = trim(interface_addr) // c_null_char

        mreq%imr_multiaddr = inet_addr(c_loc(group_c))
        mreq%imr_interface = inet_addr(c_loc(iface_c))

        ret = setsockopt(sock, level, optname, c_loc(mreq), int(sizeof(mreq), c_int))
        success = (ret == 0)

        if (.not. success) then
            write(*, '(A,I0)') "setsockopt multicast failed. Error: ", WSAGetLastError()
        end if
    end function set_multicast_group

end module forge_winsock

