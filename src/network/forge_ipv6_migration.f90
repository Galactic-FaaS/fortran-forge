!> @brief IPv4-to-IPv6 migration support utilities
!> @details Helper functions for transitioning from IPv4 to IPv6 networks
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_ipv6_migration
    use iso_c_binding
    use forge_types
    use forge_socket
    implicit none
    private

    public :: ipv4_to_ipv6_mapped, ipv6_to_ipv4_mapped
    public :: is_ipv4_mapped_ipv6, extract_ipv4_from_mapped
    public :: ipv6_to_ipv4_compatible, is_ipv4_compatible_ipv6
    public :: create_dual_stack_server, create_ipv6_only_server
    public :: migrate_ipv4_client_to_ipv6, check_ipv6_connectivity
    public :: TeredoInfo, detect_teredo, configure_teredo
    public :: SixToFourInfo, detect_6to4, configure_6to4

    !> @brief Teredo tunneling information
    type :: TeredoInfo
        logical :: available = .false.
        character(len=45) :: client_ipv6 = ""
        character(len=15) :: server_ipv4 = ""
        integer :: client_port = 0
        integer :: relay_port = 0
    end type TeredoInfo

    !> @brief 6to4 tunneling information
    type :: SixToFourInfo
        logical :: available = .false.
        character(len=45) :: ipv6_prefix = ""
        character(len=15) :: relay_ipv4 = ""
    end type SixToFourInfo

contains

    !> @brief Convert IPv4 address to IPv4-mapped IPv6 address (::ffff:x.x.x.x)
    function ipv4_to_ipv6_mapped(ipv4_addr) result(ipv6_addr)
        character(len=*), intent(in) :: ipv4_addr
        character(len=:), allocatable :: ipv6_addr
        integer :: octets(4), i
        character(len=20) :: temp

        ! Parse IPv4 address
        read(ipv4_addr, *, iostat=ios) octets(1), octets(2), octets(3), octets(4)
        if (ios /= 0) then
            ipv6_addr = ""
            return
        end if

        ! Create IPv4-mapped IPv6 address
        write(temp, '("::ffff:",I0,".",I0,".",I0,".",I0)') octets(1), octets(2), octets(3), octets(4)
        ipv6_addr = trim(temp)
    end function ipv4_to_ipv6_mapped

    !> @brief Extract IPv4 address from IPv4-mapped IPv6 address
    function ipv6_to_ipv4_mapped(ipv6_addr) result(ipv4_addr)
        character(len=*), intent(in) :: ipv6_addr
        character(len=:), allocatable :: ipv4_addr
        integer :: pos, octets(4)
        character(len=45) :: temp

        temp = trim(ipv6_addr)

        ! Check if it's an IPv4-mapped IPv6 address
        if (index(temp, "::ffff:") == 1) then
            ! Extract the IPv4 part
            pos = index(temp, "::ffff:")
            temp = temp(pos+7:)

            ! Parse the IPv4 address
            read(temp, *, iostat=ios) octets(1), octets(2), octets(3), octets(4)
            if (ios == 0) then
                write(ipv4_addr, '(I0,".",I0,".",I0,".",I0)') octets(1), octets(2), octets(3), octets(4)
                return
            end if
        end if

        ipv4_addr = ""
    end function ipv6_to_ipv4_mapped

    !> @brief Check if an IPv6 address is an IPv4-mapped address
    function is_ipv4_mapped_ipv6(ipv6_addr) result(is_mapped)
        character(len=*), intent(in) :: ipv6_addr
        logical :: is_mapped

        is_mapped = (index(trim(ipv6_addr), "::ffff:") == 1)
    end function is_ipv4_mapped_ipv6

    !> @brief Extract IPv4 address from IPv4-mapped IPv6 address
    function extract_ipv4_from_mapped(ipv6_addr) result(ipv4_addr)
        character(len=*), intent(in) :: ipv6_addr
        character(len=:), allocatable :: ipv4_addr

        if (is_ipv4_mapped_ipv6(ipv6_addr)) then
            ipv4_addr = ipv6_to_ipv4_mapped(ipv6_addr)
        else
            ipv4_addr = ""
        end if
    end function extract_ipv4_from_mapped

    !> @brief Convert IPv4 address to IPv4-compatible IPv6 address (::x.x.x.x)
    function ipv6_to_ipv4_compatible(ipv4_addr) result(ipv6_addr)
        character(len=*), intent(in) :: ipv4_addr
        character(len=:), allocatable :: ipv6_addr
        integer :: octets(4)
        character(len=20) :: temp

        ! Parse IPv4 address
        read(ipv4_addr, *, iostat=ios) octets(1), octets(2), octets(3), octets(4)
        if (ios /= 0) then
            ipv6_addr = ""
            return
        end if

        ! Create IPv4-compatible IPv6 address
        write(temp, '("::",I0,".",I0,".",I0,".",I0)') octets(1), octets(2), octets(3), octets(4)
        ipv6_addr = trim(temp)
    end function ipv6_to_ipv4_compatible

    !> @brief Check if an IPv6 address is IPv4-compatible
    function is_ipv4_compatible_ipv6(ipv6_addr) result(is_compatible)
        character(len=*), intent(in) :: ipv6_addr
        logical :: is_compatible
        character(len=45) :: temp

        temp = trim(ipv6_addr)

        ! IPv4-compatible addresses start with "::" followed by IPv4 format
        is_compatible = (index(temp, "::") == 1 .and. index(temp, "::ffff:") /= 1 .and. &
                        len_trim(temp) > 2)
    end function is_ipv4_compatible_ipv6

    !> @brief Create a dual-stack server that accepts both IPv4 and IPv6 connections
    subroutine create_dual_stack_server(server_socket, port, backlog)
        type(QTcpSocket), intent(inout) :: server_socket
        integer, intent(in) :: port
        integer, intent(in), optional :: backlog

        ! Enable dual-stack mode
        call server_socket%set_dual_stack(.true.)

        ! Listen on the specified port (will accept both IPv4 and IPv6)
        call server_socket%listen(port, backlog)
    end subroutine create_dual_stack_server

    !> @brief Create an IPv6-only server
    subroutine create_ipv6_only_server(server_socket, port, backlog)
        type(QTcpSocket), intent(inout) :: server_socket
        integer, intent(in) :: port
        integer, intent(in), optional :: backlog

        ! Disable dual-stack mode to make it IPv6-only
        call server_socket%set_dual_stack(.false.)

        ! Listen on the specified port (IPv6 only)
        call server_socket%listen(port, backlog)
    end subroutine create_ipv6_only_server

    !> @brief Migrate an IPv4 client to use IPv6
    subroutine migrate_ipv4_client_to_ipv6(client_socket, ipv4_host, port)
        type(QTcpSocket), intent(inout) :: client_socket
        character(len=*), intent(in) :: ipv4_host
        integer, intent(in) :: port
        type(QHostAddress) :: ipv6_addr

        ! First try to resolve the hostname to IPv6
        call ipv6_addr%resolve_ipv6(ipv4_host)

        if (ipv6_addr%is_valid()) then
            ! IPv6 address available, use it
            call client_socket%connect_to_host(ipv6_addr%get_address(), port)
        else
            ! Fall back to IPv4-mapped IPv6 address
            ipv6_addr%set_address(ipv4_to_ipv6_mapped(ipv4_host), port)
            call client_socket%connect_to_host(ipv6_addr%get_address(), port)
        end if
    end subroutine migrate_ipv4_client_to_ipv6

    !> @brief Check IPv6 connectivity by attempting to reach a known IPv6 host
    function check_ipv6_connectivity(test_host) result(has_ipv6)
        character(len=*), intent(in), optional :: test_host
        logical :: has_ipv6
        type(QHostAddress) :: test_addr
        character(len=256) :: host

        if (present(test_host)) then
            host = test_host
        else
            host = "ipv6.google.com"  ! Well-known IPv6 host
        end if

        ! Try to resolve to IPv6
        call test_addr%resolve_ipv6(host)
        has_ipv6 = test_addr%is_valid()
    end function check_ipv6_connectivity

    !> @brief Detect Teredo tunneling configuration
    function detect_teredo() result(info)
        type(TeredoInfo) :: info
        ! Platform-specific Teredo detection would go here
        ! For now, return unavailable
        info%available = .false.
    end function detect_teredo

    !> @brief Configure Teredo tunneling
    subroutine configure_teredo(server_ipv4, enable)
        character(len=*), intent(in) :: server_ipv4
        logical, intent(in) :: enable
        ! Platform-specific Teredo configuration would go here
    end subroutine configure_teredo

    !> @brief Detect 6to4 tunneling configuration
    function detect_6to4() result(info)
        type(SixToFourInfo) :: info
        ! Platform-specific 6to4 detection would go here
        ! For now, return unavailable
        info%available = .false.
    end function detect_6to4

    !> @brief Configure 6to4 tunneling
    subroutine configure_6to4(relay_ipv4, enable)
        character(len=*), intent(in) :: relay_ipv4
        logical, intent(in) :: enable
        ! Platform-specific 6to4 configuration would go here
    end subroutine configure_6to4

end module forge_ipv6_migration