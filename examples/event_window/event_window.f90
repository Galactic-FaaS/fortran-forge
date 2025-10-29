!> @brief Window Event Example
!> @details Demonstrates handling window events like resize, move, focus
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_window_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: size_label, position_label, state_label
    type(forge_button) :: minimize_button, maximize_button, fullscreen_button
    type(forge_text_view) :: event_log
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: window_event_count = 0

    print '(A)', "=== Window Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Window resize events"
    print '(A)', "  - Window move events"
    print '(A)', "  - Window focus/blur events"
    print '(A)', "  - Window state changes (minimize, maximize)"
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
        call builder%set_title("Window Events")
        call builder%set_size(500, 400)
        call builder%set_position(100, 100)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Set up window event handlers
    call window%on_resize(on_window_resize)
    call window%on_move(on_window_move)
    call window%on_focus(on_window_focus)
    call window%on_blur(on_window_blur)
    call window%on_minimize(on_window_minimize)
    call window%on_maximize(on_window_maximize)
    call window%on_restore(on_window_restore)

    ! Create UI elements
    print '(A)', "Creating window event display..."

    call size_label%set_text("Window Size: 500x400")
    call size_label%set_name("size_display")

    call position_label%set_text("Window Position: (100, 100)")
    call position_label%set_name("position_display")

    call state_label%set_text("Window State: Normal")
    call state_label%set_name("state_display")

    call minimize_button%set_label("Minimize")
    call minimize_button%set_name("minimize_button")
    call minimize_button%on_click(on_minimize_clicked)

    call maximize_button%set_label("Maximize")
    call maximize_button%set_name("maximize_button")
    call maximize_button%on_click(on_maximize_clicked)

    call fullscreen_button%set_label("Toggle Fullscreen")
    call fullscreen_button%set_name("fullscreen_button")
    call fullscreen_button%on_click(on_fullscreen_clicked)

    call event_log%set_name("event_log")
    call event_log%set_editable(.false.)
    call event_log%set_text("Window Event Log:\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with window event handling..."
    call window%show()

    ! Simulate window events
    print '(A)', ""
    print '(A)', "Simulating window events:"

    ! Simulate resize
    print '(A)', "  Resizing window to 600x500..."
    call simulate_window_resize(600, 500)

    print '(A)', "  Resizing window to 400x300..."
    call simulate_window_resize(400, 300)

    ! Simulate move
    print '(A)', "  Moving window to (200, 150)..."
    call simulate_window_move(200, 150)

    print '(A)', "  Moving window to (50, 50)..."
    call simulate_window_move(50, 50)

    ! Simulate focus changes
    print '(A)', "  Window gaining focus..."
    call on_window_focus(create_test_event())

    print '(A)', "  Window losing focus..."
    call on_window_blur(create_test_event())

    ! Simulate state changes
    print '(A)', "  Minimizing window..."
    call on_window_minimize(create_test_event())

    print '(A)', "  Restoring window..."
    call on_window_restore(create_test_event())

    print '(A)', "  Maximizing window..."
    call on_window_maximize(create_test_event())

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

    !> @brief Create a test event for simulation
    function create_test_event() result(event)
        type(forge_event) :: event
        ! In a real implementation, this would be created by the event system
        ! For simulation, we just return a dummy event
    end function create_test_event

    !> @brief Handler for window resize events
    subroutine on_window_resize(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: size_text, log_entry

        window_event_count = window_event_count + 1

        write(size_text, '(A,I0,A,I0)') "Window Size: ", event%width, "x", event%height
        call size_label%set_text(trim(size_text))

        write(log_entry, '(A,I0,A,I0,A,I0,A)') &
            "Resize #", window_event_count, ": ", event%width, "x", event%height

        call event_log%set_text(event_log%get_text() // trim(log_entry) // "\n")

        print '(A,I0,A,I0)', "  → Window resized to: ", event%width, "x", event%height
    end subroutine on_window_resize

    !> @brief Handler for window move events
    subroutine on_window_move(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: pos_text

        write(pos_text, '(A,I0,A,I0,A)') "Window Position: (", event%x, ", ", event%y, ")"
        call position_label%set_text(trim(pos_text))

        print '(A,I0,A,I0)', "  → Window moved to: (", event%x, ", ", event%y, ")"
    end subroutine on_window_move

    !> @brief Handler for window focus events
    subroutine on_window_focus(event)
        type(forge_event), intent(in) :: event

        call state_label%set_text("Window State: Focused")

        print '(A)', "  → Window gained focus"
    end subroutine on_window_focus

    !> @brief Handler for window blur events
    subroutine on_window_blur(event)
        type(forge_event), intent(in) :: event

        call state_label%set_text("Window State: Unfocused")

        print '(A)', "  → Window lost focus"
    end subroutine on_window_blur

    !> @brief Handler for window minimize events
    subroutine on_window_minimize(event)
        type(forge_event), intent(in) :: event

        call state_label%set_text("Window State: Minimized")

        print '(A)', "  → Window minimized"
    end subroutine on_window_minimize

    !> @brief Handler for window maximize events
    subroutine on_window_maximize(event)
        type(forge_event), intent(in) :: event

        call state_label%set_text("Window State: Maximized")

        print '(A)', "  → Window maximized"
    end subroutine on_window_maximize

    !> @brief Handler for window restore events
    subroutine on_window_restore(event)
        type(forge_event), intent(in) :: event

        call state_label%set_text("Window State: Normal")

        print '(A)', "  → Window restored"
    end subroutine on_window_restore

    !> @brief Handler for minimize button click
    subroutine on_minimize_clicked(event)
        type(forge_event), intent(in) :: event

        call on_window_minimize(event)
    end subroutine on_minimize_clicked

    !> @brief Handler for maximize button click
    subroutine on_maximize_clicked(event)
        type(forge_event), intent(in) :: event

        call on_window_maximize(event)
    end subroutine on_maximize_clicked

    !> @brief Handler for fullscreen button click
    subroutine on_fullscreen_clicked(event)
        type(forge_event), intent(in) :: event

        print '(A)', "  → Fullscreen toggle clicked"
    end subroutine on_fullscreen_clicked

    !> @brief Simulate window resize
    subroutine simulate_window_resize(width, height)
        integer, intent(in) :: width, height
        type(forge_event) :: event

        event%width = width
        event%height = height

        call on_window_resize(event)
    end subroutine simulate_window_resize

    !> @brief Simulate window move
    subroutine simulate_window_move(x, y)
        integer, intent(in) :: x, y
        type(forge_event) :: event

        event%x = x
        event%y = y

        call on_window_move(event)
    end subroutine simulate_window_move

end program event_window_example