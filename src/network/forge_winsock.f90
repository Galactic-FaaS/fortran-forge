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
    public :: SOCKET_TYPE, INVALID_SOCKET

    !> Socket type
    type(c_ptr) :: SOCKET_TYPE
    type(c_ptr), parameter :: INVALID_SOCKET = c_null_ptr

    !> Winsock constants
    integer(c_int), parameter :: AF_INET = 2
    integer(c_int), parameter :: SOCK_STREAM = 1
    integer(c_int), parameter :: SOCK_DGRAM = 2
    integer(c_int), parameter :: IPPROTO_TCP = 6
    integer(c_int), parameter :: IPPROTO_UDP = 17
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
            write(error_unit, '(A,I0)') "WSAStartup failed with error: ", result
        end if
    end function winsock_init

    !> @brief Cleanup Winsock2
    subroutine winsock_cleanup()
        integer(c_int) :: result
        
        result = WSACleanup()
        if (result /= 0) then
            write(error_unit, '(A,I0)') "WSACleanup failed with error: ", WSAGetLastError()
        end if
    end subroutine winsock_cleanup

    !> @brief Create TCP socket
    function create_tcp_socket() result(sock)
        type(c_ptr) :: sock
        
        sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        
        if (.not. c_associated(sock)) then
            write(error_unit, '(A,I0)') "Failed to create TCP socket. Error: ", WSAGetLastError()
        end if
    end function create_tcp_socket

    !> @brief Create UDP socket
    function create_udp_socket() result(sock)
        type(c_ptr) :: sock
        
        sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
        
        if (.not. c_associated(sock)) then
            write(error_unit, '(A,I0)') "Failed to create UDP socket. Error: ", WSAGetLastError()
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
        character(len=:), allocatable :: host_c
        
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
            write(error_unit, '(A,I0)') "Connect failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "Bind failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "Listen failed. Error: ", WSAGetLastError()
        end if
    end function socket_listen

    !> @brief Accept incoming connection
    function socket_accept(sock) result(client_sock)
        type(c_ptr), intent(in) :: sock
        type(c_ptr) :: client_sock
        
        client_sock = accept(sock, c_null_ptr, c_null_ptr)
        
        if (.not. c_associated(client_sock)) then
            write(error_unit, '(A,I0)') "Accept failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "Send failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "Recv failed. Error: ", WSAGetLastError()
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
        character(len=:), allocatable :: ip_c
        
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
            write(error_unit, '(A,I0)') "Sendto failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "Recvfrom failed. Error: ", WSAGetLastError()
            bytes_received = -1
        else
            if (present(source_port)) then
                source_port = ntohs(from_addr%sin_port)
            end if
            ! TODO: Convert from_addr%sin_addr to IP string for source_ip
        end if
    end function socket_recvfrom

    !> @brief Close socket
    subroutine socket_close(sock)
        type(c_ptr), intent(in) :: sock
        integer(c_int) :: result
        
        if (c_associated(sock)) then
            result = closesocket(sock)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Close socket failed. Error: ", WSAGetLastError()
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
            write(error_unit, '(A,I0)') "ioctlsocket failed. Error: ", WSAGetLastError()
        end if
    end function socket_set_nonblocking

end module forge_winsock

