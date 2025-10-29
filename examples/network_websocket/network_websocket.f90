!> @brief WebSocket Client Example
!> @details Demonstrates WebSocket connections for real-time bidirectional communication
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_websocket_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_websocket) :: websocket
    type(forge_label) :: status_label, connection_label
    type(forge_button) :: connect_button, disconnect_button, send_button
    type(forge_entry) :: url_entry, message_entry
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: connected = .false.

    print '(A)', "=== WebSocket Client Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - WebSocket client creation and connection"
    print '(A)', "  - Real-time bidirectional messaging"
    print '(A)', "  - WebSocket protocol handling"
    print '(A)', "  - Connection state management"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - WebSocket connections won't actually work"
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
        call builder%set_title("WebSocket Client Example")
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

    ! Create WebSocket client
    print '(A)', "Creating WebSocket client..."
    call websocket%set_subprotocols(["chat", "echo"])
    call websocket%set_origin("https://example.com")
    call websocket%on_connected(on_ws_connected)
    call websocket%on_disconnected(on_ws_disconnected)
    call websocket%on_text_message(on_text_message)
    call websocket%on_binary_message(on_binary_message)
    call websocket%on_error(on_ws_error)

    ! Create UI elements
    print '(A)', "Creating WebSocket client interface..."

    ! Connection controls
    call url_entry%set_placeholder_text("WebSocket URL (ws:// or wss://)")
    call url_entry%set_text("ws://echo.websocket.org")
    call url_entry%set_name("url_entry")

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

    call send_button%set_label("Send Message")
    call send_button%set_name("send_button")
    call send_button%on_click(on_send_clicked)
    call send_button%set_enabled(.false.)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("WebSocket client ready - enter URL and click Connect")

    call connection_label%set_name("connection_label")
    call connection_label%set_text("Not connected")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("WebSocket connection log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing WebSocket client interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (connected) then
        call websocket%close(1000, "Application closing")
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Connect to WebSocket server
    subroutine connect_websocket()
        character(len=:), allocatable :: url

        url = trim(url_entry%get_text())
        if (len_trim(url) == 0) then
            call status_label%set_text("Error: Please enter a WebSocket URL")
            return
        end if

        print '(A,A)', "  Connecting to WebSocket: ", url

        ! Attempt WebSocket connection (simulated)
        call simulate_websocket_connection(url)
    end subroutine connect_websocket

    !> @brief Disconnect from WebSocket server
    subroutine disconnect_websocket()
        call websocket%close(1000, "Client disconnecting")
        connected = .false.

        call connect_button%set_enabled(.true.)
        call disconnect_button%set_enabled(.false.)
        call send_button%set_enabled(.false.)
        call url_entry%set_enabled(.true.)

        call status_label%set_text("WebSocket disconnected")
        call connection_label%set_text("Not connected")

        call log_view%set_text(log_view%get_text() // "WebSocket disconnected\n")

        print '(A)', "  → WebSocket disconnected"
    end subroutine disconnect_websocket

    !> @brief Send message via WebSocket
    subroutine send_websocket_message()
        character(len=:), allocatable :: message

        message = trim(message_entry%get_text())
        if (len_trim(message) == 0) return

        print '(A,A)', "  Sending WebSocket message: ", message

        ! Send message (simulated)
        call websocket%send_text_message(trim(message))

        call log_view%set_text(log_view%get_text() // "Sent: " // trim(message) // "\n")
        call message_entry%set_text("")  ! Clear entry
    end subroutine send_websocket_message

    !> @brief Simulate WebSocket connection
    subroutine simulate_websocket_connection(url)
        character(len=*), intent(in) :: url

        ! Simulate WebSocket handshake
        print '(A)', "  [Simulating WebSocket handshake...]"

        ! Simulate successful connection
        connected = .true.

        call connect_button%set_enabled(.false.)
        call disconnect_button%set_enabled(.true.)
        call send_button%set_enabled(.true.)
        call url_entry%set_enabled(.false.)

        write(status_text, '("Connected to ", A)') url
        call status_label%set_text(trim(status_text))

        call connection_label%set_text("Connected (WebSocket)")

        call log_view%set_text(log_view%get_text() // &
            "WebSocket connected to " // trim(url) // "\n" // &
            "Protocol: chat\n" // &
            "Extensions: permessage-deflate\n")

        ! Trigger connected event
        call on_ws_connected()

        print '(A,A)', "  → WebSocket connected to ", url
    end subroutine simulate_websocket_connection

    !> @brief Handler for WebSocket connection established
    subroutine on_ws_connected()
        print '(A)', "  → WebSocket handshake completed"
    end subroutine on_ws_connected

    !> @brief Handler for WebSocket disconnection
    subroutine on_ws_disconnected(code, reason)
        integer, intent(in) :: code
        character(len=*), intent(in) :: reason

        print '(A,I0,A,A)', "  → WebSocket closed: ", code, " - ", trim(reason)
    end subroutine on_ws_disconnected

    !> @brief Handler for text messages
    subroutine on_text_message(message)
        character(len=*), intent(in) :: message

        call log_view%set_text(log_view%get_text() // "Received: " // trim(message) // "\n")

        print '(A,A)', "  → Text message received: ", trim(message)
    end subroutine on_text_message

    !> @brief Handler for binary messages
    subroutine on_binary_message(data)
        character(len=*), intent(in) :: data

        call log_view%set_text(log_view%get_text() // "Received binary data (" // &
            trim(int_to_string(len(data))) // " bytes)\n")

        print '(A,I0,A)', "  → Binary message received: ", len(data), " bytes"
    end subroutine on_binary_message

    !> @brief Handler for WebSocket errors
    subroutine on_ws_error(error_msg)
        character(len=*), intent(in) :: error_msg

        call status_label%set_text("WebSocket error: " // trim(error_msg))
        call log_view%set_text(log_view%get_text() // "Error: " // trim(error_msg) // "\n")

        print '(A,A)', "  → WebSocket error: ", trim(error_msg)
    end subroutine on_ws_error

    !> @brief Handler for connect button click
    subroutine on_connect_clicked(event)
        type(forge_event), intent(in) :: event
        call connect_websocket()
    end subroutine on_connect_clicked

    !> @brief Handler for disconnect button click
    subroutine on_disconnect_clicked(event)
        type(forge_event), intent(in) :: event
        call disconnect_websocket()
    end subroutine on_disconnect_clicked

    !> @brief Handler for send button click
    subroutine on_send_clicked(event)
        type(forge_event), intent(in) :: event
        call send_websocket_message()
    end subroutine on_send_clicked

    !> @brief Convert integer to string
    function int_to_string(val) result(str)
        integer, intent(in) :: val
        character(len=20) :: str
        write(str, '(I0)') val
        str = trim(adjustl(str))
    end function int_to_string

end program network_websocket_example