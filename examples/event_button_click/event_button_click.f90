!> @brief Button Click Event Example
!> @details Demonstrates handling button click events with callbacks
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_button_click_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: click_button, counter_button, toggle_button
    type(forge_label) :: status_label, counter_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: click_count = 0
    logical :: toggle_state = .false.

    print '(A)', "=== Button Click Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Button click event handling"
    print '(A)', "  - Callback functions for events"
    print '(A)', "  - Event-driven programming"
    print '(A)', "  - State management in event handlers"
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
        call builder%set_title("Button Click Events")
        call builder%set_size(400, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create buttons
    print '(A)', "Creating buttons with click handlers..."

    ! Simple click button
    call click_button%set_label("Click Me!")
    call click_button%set_name("button_click")
    call click_button%on_click(on_simple_click)

    ! Counter button
    call counter_button%set_label("Counter Button")
    call counter_button%set_name("button_counter")
    call counter_button%on_click(on_counter_click)

    ! Toggle button
    call toggle_button%set_label("Toggle: OFF")
    call toggle_button%set_name("button_toggle")
    call toggle_button%on_click(on_toggle_click)

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Click the buttons above to trigger events")

    call counter_label%set_name("counter_label")
    call counter_label%set_text("Counter: 0")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with event-handling buttons..."
    call window%show()

    ! Simulate button clicks
    print '(A)', ""
    print '(A)', "Simulating button clicks:"

    ! Simulate simple click
    print '(A)', "  Clicking 'Click Me!' button..."
    call on_simple_click(create_test_event())

    ! Simulate counter clicks
    print '(A)', "  Clicking 'Counter Button' 3 times..."
    call on_counter_click(create_test_event())
    call on_counter_click(create_test_event())
    call on_counter_click(create_test_event())

    ! Simulate toggle clicks
    print '(A)', "  Clicking 'Toggle' button twice..."
    call on_toggle_click(create_test_event())
    call on_toggle_click(create_test_event())

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

    !> @brief Handler for simple button click
    subroutine on_simple_click(event)
        type(forge_event), intent(in) :: event
        print '(A)', "  → Simple button clicked!"
        call status_label%set_text("Simple button was clicked!")
    end subroutine on_simple_click

    !> @brief Handler for counter button click
    subroutine on_counter_click(event)
        type(forge_event), intent(in) :: event
        character(len=50) :: counter_text

        click_count = click_count + 1
        write(counter_text, '("Counter: ", I0)') click_count
        call counter_label%set_text(trim(counter_text))

        write(counter_text, '("Counter button clicked ", I0, " times")') click_count
        call status_label%set_text(trim(counter_text))

        print '(A,I0,A)', "  → Counter button clicked! Count: ", click_count
    end subroutine on_counter_click

    !> @brief Handler for toggle button click
    subroutine on_toggle_click(event)
        type(forge_event), intent(in) :: event
        character(len=50) :: button_text, status_text

        toggle_state = .not. toggle_state

        if (toggle_state) then
            button_text = "Toggle: ON"
            status_text = "Toggle button: ON"
        else
            button_text = "Toggle: OFF"
            status_text = "Toggle button: OFF"
        end if

        call toggle_button%set_label(trim(button_text))
        call status_label%set_text(trim(status_text))

        print '(A,L1)', "  → Toggle button clicked! State: ", toggle_state
    end subroutine on_toggle_click

end program event_button_click_example