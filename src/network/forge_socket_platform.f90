!> @brief Cross-platform socket abstraction layer
!> @details Platform-independent socket API that works on Windows, Linux, and macOS
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_socket_platform
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: socket_init, socket_cleanup
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

    !> Platform detection
    logical, parameter :: IS_WINDOWS = .false.  ! Will be set by build system
    logical, parameter :: IS_POSIX = .true.     ! Will be set by build system

contains

    !> @brief Initialize socket subsystem
    function socket_init() result(success)
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: winsock_init
            success = winsock_init()
        else if (IS_POSIX) then
            use forge_posix_socket, only: posix_init
            success = posix_init()
        else
            success = .false.
        end if
    end function socket_init

    !> @brief Cleanup socket subsystem
    subroutine socket_cleanup()
        if (IS_WINDOWS) then
            use forge_winsock, only: winsock_cleanup
            call winsock_cleanup()
        else if (IS_POSIX) then
            use forge_posix_socket, only: posix_cleanup
            call posix_cleanup()
        end if
    end subroutine socket_cleanup

    !> @brief Create TCP socket
    function create_tcp_socket() result(sock)
        type(c_ptr) :: sock

        if (IS_WINDOWS) then
            use forge_winsock, only: create_tcp_socket
            sock = create_tcp_socket()
        else if (IS_POSIX) then
            use forge_posix_socket, only: create_tcp_socket
            sock = create_tcp_socket()
        else
            sock = INVALID_SOCKET
        end if
    end function create_tcp_socket

    !> @brief Create UDP socket
    function create_udp_socket() result(sock)
        type(c_ptr) :: sock

        if (IS_WINDOWS) then
            use forge_winsock, only: create_udp_socket
            sock = create_udp_socket()
        else if (IS_POSIX) then
            use forge_posix_socket, only: create_udp_socket
            sock = create_udp_socket()
        else
            sock = INVALID_SOCKET
        end if
    end function create_udp_socket

    !> @brief Create dual-stack socket (IPv4/IPv6)
    function create_dual_socket(protocol) result(sock)
        integer, intent(in) :: protocol
        type(c_ptr) :: sock

        if (IS_WINDOWS) then
            use forge_winsock, only: create_dual_socket
            sock = create_dual_socket(protocol)
        else if (IS_POSIX) then
            use forge_posix_socket, only: create_dual_socket
            sock = create_dual_socket(protocol)
        else
            sock = INVALID_SOCKET
        end if
    end function create_dual_socket

    !> @brief Connect socket to host
    function socket_connect(sock, host, port) result(success)
        type(c_ptr), intent(in) :: sock
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_connect
            success = socket_connect(sock, host, port)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_connect
            success = socket_connect(sock, host, port)
        else
            success = .false.
        end if
    end function socket_connect

    !> @brief Bind socket to port
    function socket_bind(sock, port) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: port
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_bind
            success = socket_bind(sock, port)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_bind
            success = socket_bind(sock, port)
        else
            success = .false.
        end if
    end function socket_bind

    !> @brief Listen for connections
    function socket_listen(sock, backlog) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in), optional :: backlog
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_listen
            success = socket_listen(sock, backlog)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_listen
            success = socket_listen(sock, backlog)
        else
            success = .false.
        end if
    end function socket_listen

    !> @brief Accept incoming connection
    function socket_accept(sock) result(client_sock)
        type(c_ptr), intent(in) :: sock
        type(c_ptr) :: client_sock

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_accept
            client_sock = socket_accept(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_accept
            client_sock = socket_accept(sock)
        else
            client_sock = INVALID_SOCKET
        end if
    end function socket_accept

    !> @brief Send data on TCP socket
    function socket_send(sock, data, length) result(bytes_sent)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(in) :: data
        integer, intent(in) :: length
        integer :: bytes_sent

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_send
            bytes_sent = socket_send(sock, data, length)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_send
            bytes_sent = socket_send(sock, data, length)
        else
            bytes_sent = -1
        end if
    end function socket_send

    !> @brief Receive data from TCP socket
    function socket_recv(sock, buffer, buffer_size) result(bytes_received)
        type(c_ptr), intent(in) :: sock
        character(len=*), target, intent(inout) :: buffer
        integer, intent(in) :: buffer_size
        integer :: bytes_received

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_recv
            bytes_received = socket_recv(sock, buffer, buffer_size)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_recv
            bytes_received = socket_recv(sock, buffer, buffer_size)
        else
            bytes_received = -1
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

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_sendto
            bytes_sent = socket_sendto(sock, data, length, dest_ip, dest_port)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_sendto
            bytes_sent = socket_sendto(sock, data, length, dest_ip, dest_port)
        else
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

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_recvfrom
            bytes_received = socket_recvfrom(sock, buffer, buffer_size, source_ip, source_port)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_recvfrom
            bytes_received = socket_recvfrom(sock, buffer, buffer_size, source_ip, source_port)
        else
            bytes_received = -1
        end if
    end function socket_recvfrom

    !> @brief Close socket
    subroutine socket_close(sock)
        type(c_ptr), intent(in) :: sock

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_close
            call socket_close(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_close
            call socket_close(sock)
        end if
    end subroutine socket_close

    !> @brief Set socket to non-blocking mode
    function socket_set_nonblocking(sock, nonblocking) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: nonblocking
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: socket_set_nonblocking
            success = socket_set_nonblocking(sock, nonblocking)
        else if (IS_POSIX) then
            use forge_posix_socket, only: socket_set_nonblocking
            success = socket_set_nonblocking(sock, nonblocking)
        else
            success = .false.
        end if
    end function socket_set_nonblocking

    !> @brief Resolve hostname to IP address (legacy, prefers IPv4)
    function resolve_hostname(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: resolve_hostname
            success = resolve_hostname(hostname, port, ip_addr)
        else if (IS_POSIX) then
            use forge_posix_socket, only: resolve_hostname
            success = resolve_hostname(hostname, port, ip_addr)
        else
            success = .false.
        end if
    end function resolve_hostname

    !> @brief Resolve hostname to IPv4 address
    function resolve_hostname_ipv4(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: resolve_hostname_ipv4
            success = resolve_hostname_ipv4(hostname, port, ip_addr)
        else if (IS_POSIX) then
            use forge_posix_socket, only: resolve_hostname_ipv4
            success = resolve_hostname_ipv4(hostname, port, ip_addr)
        else
            success = .false.
        end if
    end function resolve_hostname_ipv4

    !> @brief Resolve hostname to IPv6 address
    function resolve_hostname_ipv6(hostname, port, ip_addr) result(success)
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port
        character(len=*), intent(out) :: ip_addr
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: resolve_hostname_ipv6
            success = resolve_hostname_ipv6(hostname, port, ip_addr)
        else if (IS_POSIX) then
            use forge_posix_socket, only: resolve_hostname_ipv6
            success = resolve_hostname_ipv6(hostname, port, ip_addr)
        else
            success = .false.
        end if
    end function resolve_hostname_ipv6

    !> @brief Convert IP address to string
    function get_ip_string(sockaddr_ptr, family) result(ip_str)
        type(c_ptr), intent(in) :: sockaddr_ptr
        integer(c_int), intent(in) :: family
        character(len=:), allocatable :: ip_str

        if (IS_WINDOWS) then
            use forge_winsock, only: get_ip_string
            ip_str = get_ip_string(sockaddr_ptr, family)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_ip_string
            ip_str = get_ip_string(sockaddr_ptr, family)
        else
            ip_str = ""
        end if
    end function get_ip_string

    !> @brief Set multicast group membership
    function set_multicast_group(sock, group_addr, interface_addr, join) result(success)
        type(c_ptr), intent(in) :: sock
        character(len=*), intent(in) :: group_addr, interface_addr
        logical, intent(in) :: join
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_multicast_group
            success = set_multicast_group(sock, group_addr, interface_addr, join)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_multicast_group
            success = set_multicast_group(sock, group_addr, interface_addr, join)
        else
            success = .false.
        end if
    end function set_multicast_group

    !> @brief Set IPv6-only socket option
    function set_ipv6_only(sock, ipv6_only) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: ipv6_only
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_ipv6_only
            success = set_ipv6_only(sock, ipv6_only)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_ipv6_only
            success = set_ipv6_only(sock, ipv6_only)
        else
            success = .false.
        end if
    end function set_ipv6_only

    !> @brief Get IPv6-only socket option
    function get_ipv6_only(sock) result(ipv6_only)
        type(c_ptr), intent(in) :: sock
        logical :: ipv6_only

        if (IS_WINDOWS) then
            use forge_winsock, only: get_ipv6_only
            ipv6_only = get_ipv6_only(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_ipv6_only
            ipv6_only = get_ipv6_only(sock)
        else
            ipv6_only = .false.
        end if
    end function get_ipv6_only

    !> @brief Set dual-stack socket option (IPv4/IPv6)
    function set_dual_stack(sock, dual_stack) result(success)
        type(c_ptr), intent(in) :: sock
        logical, intent(in) :: dual_stack
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_dual_stack
            success = set_dual_stack(sock, dual_stack)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_dual_stack
            success = set_dual_stack(sock, dual_stack)
        else
            success = .false.
        end if
    end function set_dual_stack

    !> @brief Set IPv6 scope ID
    function set_ipv6_scope_id(sock, scope_id) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: scope_id
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_ipv6_scope_id
            success = set_ipv6_scope_id(sock, scope_id)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_ipv6_scope_id
            success = set_ipv6_scope_id(sock, scope_id)
        else
            success = .false.
        end if
    end function set_ipv6_scope_id

    !> @brief Get IPv6 scope ID
    function get_ipv6_scope_id(sock) result(scope_id)
        type(c_ptr), intent(in) :: sock
        integer :: scope_id

        if (IS_WINDOWS) then
            use forge_winsock, only: get_ipv6_scope_id
            scope_id = get_ipv6_scope_id(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_ipv6_scope_id
            scope_id = get_ipv6_scope_id(sock)
        else
            scope_id = 0
        end if
    end function get_ipv6_scope_id

    !> @brief Set IPv6 flow info
    function set_ipv6_flow_info(sock, flow_info) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: flow_info
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_ipv6_flow_info
            success = set_ipv6_flow_info(sock, flow_info)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_ipv6_flow_info
            success = set_ipv6_flow_info(sock, flow_info)
        else
            success = .false.
        end if
    end function set_ipv6_flow_info

    !> @brief Get IPv6 flow info
    function get_ipv6_flow_info(sock) result(flow_info)
        type(c_ptr), intent(in) :: sock
        integer :: flow_info

        if (IS_WINDOWS) then
            use forge_winsock, only: get_ipv6_flow_info
            flow_info = get_ipv6_flow_info(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_ipv6_flow_info
            flow_info = get_ipv6_flow_info(sock)
        else
            flow_info = 0
        end if
    end function get_ipv6_flow_info

    !> @brief Set IPv6 traffic class
    function set_ipv6_traffic_class(sock, traffic_class) result(success)
        type(c_ptr), intent(in) :: sock
        integer, intent(in) :: traffic_class
        logical :: success

        if (IS_WINDOWS) then
            use forge_winsock, only: set_ipv6_traffic_class
            success = set_ipv6_traffic_class(sock, traffic_class)
        else if (IS_POSIX) then
            use forge_posix_socket, only: set_ipv6_traffic_class
            success = set_ipv6_traffic_class(sock, traffic_class)
        else
            success = .false.
        end if
    end function set_ipv6_traffic_class

    !> @brief Get IPv6 traffic class
    function get_ipv6_traffic_class(sock) result(traffic_class)
        type(c_ptr), intent(in) :: sock
        integer :: traffic_class

        if (IS_WINDOWS) then
            use forge_winsock, only: get_ipv6_traffic_class
            traffic_class = get_ipv6_traffic_class(sock)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_ipv6_traffic_class
            traffic_class = get_ipv6_traffic_class(sock)
        else
            traffic_class = 0
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

        if (IS_WINDOWS) then
            use forge_winsock, only: enumerate_interfaces
            interfaces = enumerate_interfaces()
        else if (IS_POSIX) then
            use forge_posix_socket, only: enumerate_interfaces
            interfaces = enumerate_interfaces()
        else
            allocate(interfaces(0))
        end if
    end function enumerate_interfaces

    !> @brief Get interface information by index
    function get_interface_info(if_index) result(iface)
        integer, intent(in) :: if_index
        type(NetworkInterface) :: iface

        if (IS_WINDOWS) then
            use forge_winsock, only: get_interface_info
            iface = get_interface_info(if_index)
        else if (IS_POSIX) then
            use forge_posix_socket, only: get_interface_info
            iface = get_interface_info(if_index)
        else
            iface%index = if_index
        end if
    end function get_interface_info

end module forge_socket_platform