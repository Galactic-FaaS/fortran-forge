!> @brief TCP Server Example
!> @details Demonstrates TCP server functionality for accepting client connections
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_tcp_server_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_tcp_server) :: tcp_server
    type(forge_tcp_socket), allocatable :: client_sockets(:)
    type(forge_label) :: status_label, connection_label
    type(forge_button) :: start_button, stop_button
    type(forge_text_view) :: log_view
    type(forge_entry) :: port_entry
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: server_port = 8080
    logical :: server_running = .false.
    integer :: client_count = 0

    print '(A)', "=== TCP Server Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - TCP server creation and configuration"
    print '(A)', "  - Accepting client connections"
    print '(A)', "  - Handling multiple concurrent connections"
    print '(A)', "  - Server event handling"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - TCP server won't actually run"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("TCP Server Example")
        call builder%set_size(600, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create TCP server
    print '(A)', "Creating TCP server..."
    call tcp_server%set_max_connections(10)
    call tcp_server%on_new_connection(on_new_connection)
    call tcp_server%on_data_received(on_data_received)

    ! Create UI elements
    print '(A)', "Creating TCP server interface..."

    ! Port entry
    call port_entry%set_placeholder_text("Port number")
    call port_entry%set_text("8080")
    call port_entry%set_name("port_entry")

    ! Control buttons
    call start_button%set_label("Start Server")
    call start_button%set_name("start_button")
    call start_button%on_click(on_start_clicked)

    call stop_button%set_label("Stop Server")
    call stop_button%set_name("stop_button")
    call stop_button%on_click(on_stop_clicked)
    call stop_button%set_enabled(.false.)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("Server stopped - enter port and click Start")

    call connection_label%set_name("connection_label")
    call connection_label%set_text("Connections: 0")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Server log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing TCP server interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (server_running) then
        call tcp_server%stop()
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Start the TCP server
    subroutine start_server()
        character(len=10) :: port_str
        integer :: port_num

        ! Get port from entry
        port_str = trim(port_entry%get_text())
        read(port_str, *, iostat=ios) port_num
        if (ios /= 0 .or. port_num < 1024 .or. port_num > 65535) then
            call status_label%set_text("Error: Invalid port number (1024-65535)")
            return
        end if

        server_port = port_num

        ! Start server
        call tcp_server%listen(server_port, status)
        if (status%is_error()) then
            call status_label%set_text("Error: Failed to start server")
            call status%print()
            return
        end if

        server_running = .true.
        call start_button%set_enabled(.false.)
        call stop_button%set_enabled(.true.)
        call port_entry%set_enabled(.false.)

        write(status_text, '("Server started on port ", I0)') server_port
        call status_label%set_text(trim(status_text))

        call log_view%set_text(log_view%get_text() // "Server started on port " // trim(port_str) // "\n")

        print '(A,I0)', "  → TCP server started on port ", server_port
    end subroutine start_server

    !> @brief Stop the TCP server
    subroutine stop_server()
        call tcp_server%stop()
        server_running = .false.
        client_count = 0

        call start_button%set_enabled(.true.)
        call stop_button%set_enabled(.false.)
        call port_entry%set_enabled(.true.)

        call status_label%set_text("Server stopped")
        call connection_label%set_text("Connections: 0")

        call log_view%set_text(log_view%get_text() // "Server stopped\n")

        print '(A)', "  → TCP server stopped"
    end subroutine stop_server

    !> @brief Handler for new client connections
    subroutine on_new_connection(socket)
        type(forge_tcp_socket), intent(in) :: socket
        character(len=100) :: log_entry

        client_count = client_count + 1

        write(log_entry, '("New client connected from ", A, " (total: ", I0, ")")') &
            trim(socket%get_peer_address()), client_count
        call log_view%set_text(log_view%get_text() // trim(log_entry) // "\n")

        write(connection_text, '("Connections: ", I0)') client_count
        call connection_label%set_text(trim(connection_text))

        print '(A,A,I0)', "  → New client connected: ", trim(socket%get_peer_address()), client_count
    end subroutine on_new_connection

    !> @brief Handler for received data
    subroutine on_data_received(socket, data)
        type(forge_tcp_socket), intent(in) :: socket
        character(len=*), intent(in) :: data
        character(len=200) :: log_entry, response

        write(log_entry, '("Received from ", A, ": ", A)') &
            trim(socket%get_peer_address()), trim(data)
        call log_view%set_text(log_view%get_text() // trim(log_entry) // "\n")

        ! Echo response
        write(response, '("Echo: ", A)') trim(data)
        call socket%write(trim(response) // "\n")

        print '(A,A,A)', "  → Data received from ", trim(socket%get_peer_address()), ": " // trim(data)
    end subroutine on_data_received

    !> @brief Handler for start button click
    subroutine on_start_clicked(event)
        type(forge_event), intent(in) :: event
        call start_server()
    end subroutine on_start_clicked

    !> @brief Handler for stop button click
    subroutine on_stop_clicked(event)
        type(forge_event), intent(in) :: event
        call stop_server()
    end subroutine on_stop_clicked

end program network_tcp_server_example