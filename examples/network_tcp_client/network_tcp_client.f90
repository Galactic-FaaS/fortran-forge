!> @brief TCP Client Example
!> @details Demonstrates TCP client functionality for connecting to servers
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_tcp_client_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_tcp_socket) :: tcp_socket
    type(forge_label) :: status_label, connection_label
    type(forge_button) :: connect_button, disconnect_button, send_button
    type(forge_entry) :: host_entry, port_entry, message_entry
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: connected = .false.

    print '(A)', "=== TCP Client Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - TCP client socket creation"
    print '(A)', "  - Connecting to TCP servers"
    print '(A)', "  - Sending and receiving data"
    print '(A)', "  - Connection state management"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - TCP connections won't actually work"
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
        call builder%set_title("TCP Client Example")
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

    ! Create TCP socket
    print '(A)', "Creating TCP client socket..."
    call tcp_socket%set_read_buffer_size(4096)
    call tcp_socket%on_connected(on_connected)
    call tcp_socket%on_disconnected(on_disconnected)
    call tcp_socket%on_data_received(on_data_received)
    call tcp_socket%on_error(on_socket_error)

    ! Create UI elements
    print '(A)', "Creating TCP client interface..."

    ! Connection controls
    call host_entry%set_placeholder_text("Server hostname/IP")
    call host_entry%set_text("localhost")
    call host_entry%set_name("host_entry")

    call port_entry%set_placeholder_text("Port number")
    call port_entry%set_text("8080")
    call port_entry%set_name("port_entry")

    call connect_button%set_label("Connect")
    call connect_button%set_name("connect_button")
    call connect_button%on_click(on_connect_clicked)

    call disconnect_button%set_label("Disconnect")
    call disconnect_button%set_name("disconnect_button")
    call disconnect_button%on_click(on_disconnect_clicked)
    call disconnect_button%set_enabled(.false.)

    ! Message controls
    call message_entry%set_placeholder_text("Enter message to send")
    call message_entry%set_name("message_entry")

    call send_button%set_label("Send")
    call send_button%set_name("send_button")
    call send_button%on_click(on_send_clicked)
    call send_button%set_enabled(.false.)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("Disconnected - enter host/port and click Connect")

    call connection_label%set_name("connection_label")
    call connection_label%set_text("Not connected")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Connection log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing TCP client interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (connected) then
        call tcp_socket%disconnect()
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Connect to TCP server
    subroutine connect_to_server()
        character(len=:), allocatable :: host
        character(len=10) :: port_str
        integer :: port_num

        host = trim(host_entry%get_text())
        port_str = trim(port_entry%get_text())

        if (len_trim(host) == 0) then
            call status_label%set_text("Error: Please enter a hostname")
            return
        end if

        read(port_str, *, iostat=ios) port_num
        if (ios /= 0 .or. port_num < 1 .or. port_num > 65535) then
            call status_label%set_text("Error: Invalid port number")
            return
        end if

        print '(A,A,A,I0)', "  Connecting to ", host, ":", port_num

        ! Attempt connection (simulated)
        call simulate_connection(host, port_num)
    end subroutine connect_to_server

    !> @brief Disconnect from server
    subroutine disconnect_from_server()
        call tcp_socket%disconnect()
        connected = .false.

        call connect_button%set_enabled(.true.)
        call disconnect_button%set_enabled(.false.)
        call send_button%set_enabled(.false.)
        call host_entry%set_enabled(.true.)
        call port_entry%set_enabled(.true.)

        call status_label%set_text("Disconnected from server")
        call connection_label%set_text("Not connected")

        call log_view%set_text(log_view%get_text() // "Disconnected from server\n")

        print '(A)', "  → Disconnected from server"
    end subroutine disconnect_from_server

    !> @brief Send message to server
    subroutine send_message()
        character(len=:), allocatable :: message

        message = trim(message_entry%get_text())
        if (len_trim(message) == 0) return

        print '(A,A)', "  Sending message: ", message

        ! Send message (simulated)
        call tcp_socket%write(trim(message) // "\n")

        call log_view%set_text(log_view%get_text() // "Sent: " // trim(message) // "\n")
        call message_entry%set_text("")  ! Clear entry
    end subroutine send_message

    !> @brief Simulate TCP connection
    subroutine simulate_connection(host, port)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port

        ! Simulate connection delay
        print '(A)', "  [Simulating connection attempt...]"

        ! Simulate successful connection
        connected = .true.

        call connect_button%set_enabled(.false.)
        call disconnect_button%set_enabled(.true.)
        call send_button%set_enabled(.true.)
        call host_entry%set_enabled(.false.)
        call port_entry%set_enabled(.false.)

        write(status_text, '("Connected to ", A, ":", I0)') host, port
        call status_label%set_text(trim(status_text))

        write(connection_text, '("Connected to ", A, ":", I0)') host, port
        call connection_label%set_text(trim(connection_text))

        call log_view%set_text(log_view%get_text() // "Connected to " // trim(host) // ":" // trim(port_str) // "\n")

        ! Trigger connected event
        call on_connected()

        print '(A,A,A,I0)', "  → Connected to ", host, ":", port
    end subroutine simulate_connection

    !> @brief Handler for successful connection
    subroutine on_connected()
        print '(A)', "  → Connection established"
    end subroutine on_connected

    !> @brief Handler for disconnection
    subroutine on_disconnected()
        print '(A)', "  → Connection lost"
    end subroutine on_disconnected

    !> @brief Handler for received data
    subroutine on_data_received(data)
        character(len=*), intent(in) :: data

        call log_view%set_text(log_view%get_text() // "Received: " // trim(data))

        print '(A,A)', "  → Data received: ", trim(data)
    end subroutine on_data_received

    !> @brief Handler for socket errors
    subroutine on_socket_error(error_msg)
        character(len=*), intent(in) :: error_msg

        call status_label%set_text("Connection error: " // trim(error_msg))
        call log_view%set_text(log_view%get_text() // "Error: " // trim(error_msg) // "\n")

        print '(A,A)', "  → Socket error: ", trim(error_msg)
    end subroutine on_socket_error

    !> @brief Handler for connect button click
    subroutine on_connect_clicked(event)
        type(forge_event), intent(in) :: event
        call connect_to_server()
    end subroutine on_connect_clicked

    !> @brief Handler for disconnect button click
    subroutine on_disconnect_clicked(event)
        type(forge_event), intent(in) :: event
        call disconnect_from_server()
    end subroutine on_disconnect_clicked

    !> @brief Handler for send button click
    subroutine on_send_clicked(event)
        type(forge_event), intent(in) :: event
        call send_message()
    end subroutine on_send_clicked

end program network_tcp_client_example