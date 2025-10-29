!> @brief Custom Event Example
!> @details Demonstrates creating and handling custom events
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_custom_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: status_label, data_label
    type(forge_button) :: send_event_button, broadcast_button
    type(forge_text_view) :: event_log
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: custom_event_count = 0

    ! Define custom event types
    integer, parameter :: CUSTOM_EVENT_DATA = 1001
    integer, parameter :: CUSTOM_EVENT_STATUS = 1002
    integer, parameter :: CUSTOM_EVENT_PROGRESS = 1003

    print '(A)', "=== Custom Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating custom event types"
    print '(A)', "  - Sending custom events"
    print '(A)', "  - Handling custom events"
    print '(A)', "  - Event data passing"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - events won't actually fire"
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
        call builder%set_title("Custom Events")
        call builder%set_size(500, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Set up custom event handler
    call window%on_custom_event(on_custom_event)

    ! Create UI elements
    print '(A)', "Creating custom event interface..."

    call status_label%set_text("Custom Events: Ready")
    call status_label%set_name("status_display")

    call data_label%set_text("Event Data: None")
    call data_label%set_name("data_display")

    call send_event_button%set_label("Send Custom Event")
    call send_event_button%set_name("send_button")
    call send_event_button%on_click(on_send_event_clicked)

    call broadcast_button%set_label("Broadcast Event")
    call broadcast_button%set_name("broadcast_button")
    call broadcast_button%on_click(on_broadcast_clicked)

    call event_log%set_name("event_log")
    call event_log%set_editable(.false.)
    call event_log%set_text("Custom Event Log:\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with custom event handling..."
    call window%show()

    ! Simulate custom events
    print '(A)', ""
    print '(A)', "Simulating custom events:"

    ! Send different types of custom events
    print '(A)', "  Sending data event..."
    call send_custom_event(CUSTOM_EVENT_DATA, "Hello, World!")

    print '(A)', "  Sending status event..."
    call send_custom_event(CUSTOM_EVENT_STATUS, "System Ready")

    print '(A)', "  Sending progress event..."
    call send_custom_event(CUSTOM_EVENT_PROGRESS, "50%")

    print '(A)', "  Sending another data event..."
    call send_custom_event(CUSTOM_EVENT_DATA, "Updated Data")

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Handler for custom events
    subroutine on_custom_event(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: event_type, log_entry

        custom_event_count = custom_event_count + 1

        ! Determine event type
        select case (event%custom_type)
        case (CUSTOM_EVENT_DATA)
            event_type = "Data Event"
            call data_label%set_text("Event Data: " // trim(event%custom_data))
        case (CUSTOM_EVENT_STATUS)
            event_type = "Status Event"
            call status_label%set_text("Custom Events: " // trim(event%custom_data))
        case (CUSTOM_EVENT_PROGRESS)
            event_type = "Progress Event"
            call status_label%set_text("Custom Events: Progress - " // trim(event%custom_data))
        case default
            event_type = "Unknown Event"
        end select

        ! Log the event
        write(log_entry, '(A,I0,A,A,A,A)') &
            "Event #", custom_event_count, ": ", trim(event_type), &
            " - ", trim(event%custom_data)

        call event_log%set_text(event_log%get_text() // trim(log_entry) // "\n")

        print '(A,A,A,A)', "  → Custom event: ", trim(event_type), " with data: ", trim(event%custom_data)
    end subroutine on_custom_event

    !> @brief Handler for send event button click
    subroutine on_send_event_clicked(event)
        type(forge_event), intent(in) :: event

        call send_custom_event(CUSTOM_EVENT_DATA, "Button Triggered Event")
    end subroutine on_send_event_clicked

    !> @brief Handler for broadcast button click
    subroutine on_broadcast_clicked(event)
        type(forge_event), intent(in) :: event

        call send_custom_event(CUSTOM_EVENT_STATUS, "Broadcast Message")
    end subroutine on_broadcast_clicked

    !> @brief Send a custom event
    subroutine send_custom_event(event_type, data)
        integer, intent(in) :: event_type
        character(len=*), intent(in) :: data
        type(forge_event) :: custom_event

        ! Create custom event
        custom_event%custom_type = event_type
        custom_event%custom_data = trim(data)

        ! Send to window
        call window%send_custom_event(custom_event)
    end subroutine send_custom_event

end program event_custom_example