!> @brief SSL/TLS Client Example
!> @details Demonstrates secure SSL/TLS connections for encrypted communication
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_ssl_client_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_ssl_socket) :: ssl_socket
    type(forge_label) :: status_label, security_label
    type(forge_button) :: connect_button, disconnect_button, send_button
    type(forge_entry) :: host_entry, port_entry, message_entry
    type(forge_text_view) :: log_view
    type(forge_checkbox) :: verify_peer_check, verify_host_check
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: connected = .false.

    print '(A)', "=== SSL/TLS Client Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - SSL/TLS socket creation and configuration"
    print '(A)', "  - Certificate verification options"
    print '(A)', "  - Encrypted communication"
    print '(A)', "  - Secure connection establishment"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - SSL connections won't actually work"
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
        call builder%set_title("SSL/TLS Client Example")
        call builder%set_size(650, 550)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create SSL socket
    print '(A)', "Creating SSL/TLS client socket..."
    call ssl_socket%set_protocol("TLSv1.2")  ! TLS 1.2
    call ssl_socket%set_verify_mode(SSL_VERIFY_PEER or SSL_VERIFY_HOST)
    call ssl_socket%set_ca_certificates_path("/etc/ssl/certs")
    call ssl_socket%on_encrypted(on_encrypted)
    call ssl_socket%on_ssl_errors(on_ssl_errors)

    ! Inherit TCP socket event handlers
    call ssl_socket%tcp_socket%on_connected(on_connected)
    call ssl_socket%tcp_socket%on_disconnected(on_disconnected)
    call ssl_socket%tcp_socket%on_data_received(on_data_received)
    call ssl_socket%tcp_socket%on_error(on_socket_error)

    ! Create UI elements
    print '(A)', "Creating SSL client interface..."

    ! Connection controls
    call host_entry%set_placeholder_text("Server hostname (e.g., example.com)")
    call host_entry%set_text("httpbin.org")
    call host_entry%set_name("host_entry")

    call port_entry%set_placeholder_text("Port number")
    call port_entry%set_text("443")
    call port_entry%set_name("port_entry")

    ! SSL options
    call verify_peer_check%set_label("Verify peer certificate")
    call verify_peer_check%set_name("verify_peer_check")
    call verify_peer_check%set_checked(.true.)

    call verify_host_check%set_label("Verify host name")
    call verify_host_check%set_name("verify_host_check")
    call verify_host_check%set_checked(.true.)

    ! Control buttons
    call connect_button%set_label("Connect (SSL)")
    call connect_button%set_name("connect_button")
    call connect_button%on_click(on_connect_clicked)

    call disconnect_button%set_label("Disconnect")
    call disconnect_button%set_name("disconnect_button")
    call disconnect_button%on_click(on_disconnect_clicked)
    call disconnect_button%set_enabled(.false.)

    ! Message controls
    call message_entry%set_placeholder_text("Enter message to send over SSL")
    call message_entry%set_name("message_entry")

    call send_button%set_label("Send Secure")
    call send_button%set_name("send_button")
    call send_button%on_click(on_send_clicked)
    call send_button%set_enabled(.false.)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("SSL client ready - enter host/port and click Connect")

    call security_label%set_name("security_label")
    call security_label%set_text("Security: Not connected")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("SSL connection log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing SSL client interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (connected) then
        call ssl_socket%tcp_socket%disconnect()
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Connect to SSL server
    subroutine connect_ssl_server()
        character(len=:), allocatable :: host
        character(len=10) :: port_str
        integer :: port_num, verify_mode

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

        ! Configure SSL verification
        verify_mode = 0
        if (verify_peer_check%is_checked()) verify_mode = verify_mode + SSL_VERIFY_PEER
        if (verify_host_check%is_checked()) verify_mode = verify_mode + SSL_VERIFY_HOST
        call ssl_socket%set_verify_mode(verify_mode)

        print '(A,A,A,I0,A,I0)', "  Connecting via SSL to ", host, ":", port_num, " (verify_mode=", verify_mode, ")"

        ! Attempt SSL connection (simulated)
        call simulate_ssl_connection(host, port_num)
    end subroutine connect_ssl_server

    !> @brief Disconnect from SSL server
    subroutine disconnect_ssl_server()
        call ssl_socket%tcp_socket%disconnect()
        connected = .false.

        call connect_button%set_enabled(.true.)
        call disconnect_button%set_enabled(.false.)
        call send_button%set_enabled(.false.)
        call host_entry%set_enabled(.true.)
        call port_entry%set_enabled(.true.)

        call status_label%set_text("SSL connection closed")
        call security_label%set_text("Security: Not connected")

        call log_view%set_text(log_view%get_text() // "SSL connection closed\n")

        print '(A)', "  → SSL connection closed"
    end subroutine disconnect_ssl_server

    !> @brief Send message over SSL
    subroutine send_ssl_message()
        character(len=:), allocatable :: message

        message = trim(message_entry%get_text())
        if (len_trim(message) == 0) return

        print '(A,A)', "  Sending encrypted message: ", message

        ! Send message over SSL (simulated)
        call ssl_socket%tcp_socket%write(trim(message) // "\n")

        call log_view%set_text(log_view%get_text() // "Sent (encrypted): " // trim(message) // "\n")
        call message_entry%set_text("")  ! Clear entry
    end subroutine send_ssl_message

    !> @brief Simulate SSL connection
    subroutine simulate_ssl_connection(host, port)
        character(len=*), intent(in) :: host
        integer, intent(in) :: port

        ! Simulate SSL handshake
        print '(A)', "  [Simulating SSL handshake...]"

        ! Simulate successful SSL connection
        connected = .true.

        call connect_button%set_enabled(.false.)
        call disconnect_button%set_enabled(.true.)
        call send_button%set_enabled(.true.)
        call host_entry%set_enabled(.false.)
        call port_entry%set_enabled(.false.)

        write(status_text, '("SSL connected to ", A, ":", I0)') host, port
        call status_label%set_text(trim(status_text))

        call security_label%set_text("Security: TLS 1.2, AES-256-GCM")

        call log_view%set_text(log_view%get_text() // &
            "SSL connection established to " // trim(host) // ":" // trim(port_str) // "\n" // &
            "Cipher: ECDHE-RSA-AES256-GCM-SHA384\n" // &
            "Certificate: Valid (simulated)\n")

        ! Trigger SSL events
        call on_encrypted()

        print '(A,A,A,I0)', "  → SSL connection established to ", host, ":", port
    end subroutine simulate_ssl_connection

    !> @brief Handler for SSL encryption established
    subroutine on_encrypted()
        print '(A)', "  → SSL encryption established"
    end subroutine on_encrypted

    !> @brief Handler for SSL errors
    subroutine on_ssl_errors(errors)
        character(len=*), intent(in) :: errors

        call status_label%set_text("SSL Error: " // trim(errors))
        call log_view%set_text(log_view%get_text() // "SSL Error: " // trim(errors) // "\n")

        print '(A,A)', "  → SSL error: ", trim(errors)
    end subroutine on_ssl_errors

    !> @brief Handler for connection established
    subroutine on_connected()
        print '(A)', "  → TCP connection established (SSL handshake pending)"
    end subroutine on_connected

    !> @brief Handler for disconnection
    subroutine on_disconnected()
        print '(A)', "  → Connection lost"
    end subroutine on_disconnected

    !> @brief Handler for received data
    subroutine on_data_received(data)
        character(len=*), intent(in) :: data

        call log_view%set_text(log_view%get_text() // "Received (encrypted): " // trim(data))

        print '(A,A)', "  → Encrypted data received: ", trim(data)
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
        call connect_ssl_server()
    end subroutine on_connect_clicked

    !> @brief Handler for disconnect button click
    subroutine on_disconnect_clicked(event)
        type(forge_event), intent(in) :: event
        call disconnect_ssl_server()
    end subroutine on_disconnect_clicked

    !> @brief Handler for send button click
    subroutine on_send_clicked(event)
        type(forge_event), intent(in) :: event
        call send_ssl_message()
    end subroutine on_send_clicked

end program network_ssl_client_example