!> @brief Mouse Event Example
!> @details Demonstrates handling mouse events and interactions
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_mouse_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: instruction_label, position_label, button_label
    type(forge_button) :: click_area
    type(forge_text_view) :: event_log
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: mouse_event_count = 0

    print '(A)', "=== Mouse Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Mouse button press/release events"
    print '(A)', "  - Mouse movement tracking"
    print '(A)', "  - Mouse wheel events"
    print '(A)', "  - Coordinate reporting"
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
        call builder%set_title("Mouse Events")
        call builder%set_size(600, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Set up mouse event handlers on window
    call window%on_mouse_press(on_mouse_press)
    call window%on_mouse_release(on_mouse_release)
    call window%on_mouse_move(on_mouse_move)
    call window%on_mouse_wheel(on_mouse_wheel)

    ! Create UI elements
    print '(A)', "Creating mouse event display..."

    call instruction_label%set_text("Move mouse, click buttons, and scroll wheel to see events.")
    call instruction_label%set_name("instruction")

    call position_label%set_text("Mouse Position: (0, 0)")
    call position_label%set_name("position_display")

    call button_label%set_text("Last Button: None")
    call button_label%set_name("button_display")

    call click_area%set_label("Click Me!")
    call click_area%set_name("click_area")
    call click_area%on_click(on_click_area_clicked)

    call event_log%set_name("event_log")
    call event_log%set_editable(.false.)
    call event_log%set_text("Mouse Event Log:\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with mouse event handling..."
    call window%show()

    ! Simulate mouse events
    print '(A)', ""
    print '(A)', "Simulating mouse events:"

    ! Simulate mouse movement
    print '(A)', "  Moving mouse to (100, 50)..."
    call simulate_mouse_move(100, 50)

    print '(A)', "  Moving mouse to (250, 150)..."
    call simulate_mouse_move(250, 150)

    ! Simulate button presses
    print '(A)', "  Left mouse button press at (200, 100)..."
    call simulate_mouse_press(200, 100, 1)  ! Left button

    print '(A)', "  Left mouse button release..."
    call simulate_mouse_release(200, 100, 1)

    print '(A)', "  Right mouse button press at (300, 200)..."
    call simulate_mouse_press(300, 200, 2)  ! Right button

    print '(A)', "  Right mouse button release..."
    call simulate_mouse_release(300, 200, 2)

    ! Simulate mouse wheel
    print '(A)', "  Mouse wheel scroll up..."
    call simulate_mouse_wheel(150, 75, 1)

    print '(A)', "  Mouse wheel scroll down..."
    call simulate_mouse_wheel(150, 75, -1)

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

    !> @brief Handler for mouse press events
    subroutine on_mouse_press(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: button_text, log_entry

        mouse_event_count = mouse_event_count + 1

        button_text = get_button_name(event%button)
        call button_label%set_text("Last Button: " // trim(button_text))

        write(log_entry, '(A,I0,A,A,A,I0,A,I0,A)') &
            "Press #", mouse_event_count, ": ", trim(button_text), &
            " at (", event%x, ", ", event%y, ")"

        call event_log%set_text(event_log%get_text() // trim(log_entry) // "\n")

        print '(A,A,A,I0,A,I0,A)', "  → Mouse press: ", trim(button_text), " at (", event%x, ", ", event%y, ")"
    end subroutine on_mouse_press

    !> @brief Handler for mouse release events
    subroutine on_mouse_release(event)
        type(forge_event), intent(in) :: event
        character(len:50) :: button_text

        button_text = get_button_name(event%button)

        print '(A,A,A,I0,A,I0,A)', "  → Mouse release: ", trim(button_text), " at (", event%x, ", ", event%y, ")"
    end subroutine on_mouse_release

    !> @brief Handler for mouse move events
    subroutine on_mouse_move(event)
        type(forge_event), intent(in) :: event
        character(len:50) :: pos_text

        write(pos_text, '(A,I0,A,I0,A)') "Mouse Position: (", event%x, ", ", event%y, ")"
        call position_label%set_text(trim(pos_text))

        print '(A,I0,A,I0,A)', "  → Mouse moved to: (", event%x, ", ", event%y, ")"
    end subroutine on_mouse_move

    !> @brief Handler for mouse wheel events
    subroutine on_mouse_wheel(event)
        type(forge_event), intent(in) :: event
        character(len:50) :: direction

        if (event%delta > 0) then
            direction = "up"
        else
            direction = "down"
        end if

        print '(A,A,A,I0,A,I0,A,I0,A)', "  → Mouse wheel: ", trim(direction), &
              " at (", event%x, ", ", event%y, "), delta=", event%delta
    end subroutine on_mouse_wheel

    !> @brief Handler for click area button
    subroutine on_click_area_clicked(event)
        type(forge_event), intent(in) :: event

        print '(A)', "  → Click area button clicked!"
    end subroutine on_click_area_clicked

    !> @brief Simulate mouse movement
    subroutine simulate_mouse_move(x, y)
        integer, intent(in) :: x, y
        type(forge_event) :: event

        event%x = x
        event%y = y

        call on_mouse_move(event)
    end subroutine simulate_mouse_move

    !> @brief Simulate mouse button press
    subroutine simulate_mouse_press(x, y, button)
        integer, intent(in) :: x, y, button
        type(forge_event) :: event

        event%x = x
        event%y = y
        event%button = button

        call on_mouse_press(event)
    end subroutine simulate_mouse_press

    !> @brief Simulate mouse button release
    subroutine simulate_mouse_release(x, y, button)
        integer, intent(in) :: x, y, button
        type(forge_event) :: event

        event%x = x
        event%y = y
        event%button = button

        call on_mouse_release(event)
    end subroutine simulate_mouse_release

    !> @brief Simulate mouse wheel event
    subroutine simulate_mouse_wheel(x, y, delta)
        integer, intent(in) :: x, y, delta
        type(forge_event) :: event

        event%x = x
        event%y = y
        event%delta = delta

        call on_mouse_wheel(event)
    end subroutine simulate_mouse_wheel

    !> @brief Get human-readable button name
    function get_button_name(button) result(name)
        integer, intent(in) :: button
        character(len:20) :: name

        select case (button)
        case (1)
            name = "Left Button"
        case (2)
            name = "Right Button"
        case (3)
            name = "Middle Button"
        case (4)
            name = "X1 Button"
        case (5)
            name = "X2 Button"
        case default
            write(name, '("Button ", I0)') button
        end select
    end function get_button_name

end program event_mouse_example