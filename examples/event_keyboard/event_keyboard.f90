!> @brief Keyboard Event Example
!> @details Demonstrates handling keyboard events and key presses
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_keyboard_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: instruction_label, key_label, modifier_label
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: key_press_count = 0

    print '(A)', "=== Keyboard Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Keyboard key press events"
    print '(A)', "  - Modifier key detection (Ctrl, Alt, Shift)"
    print '(A)', "  - Special key handling"
    print '(A)', "  - Key event logging"
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
        call builder%set_title("Keyboard Events")
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

    ! Set up keyboard event handler on window
    call window%on_key_press(on_key_press)
    call window%on_key_release(on_key_release)

    ! Create UI elements
    print '(A)', "Creating keyboard event display..."

    call instruction_label%set_text("Press keys on your keyboard to see events. Try combinations with Ctrl, Alt, Shift.")
    call instruction_label%set_name("instruction")

    call key_label%set_text("Last Key: None")
    call key_label%set_name("key_display")

    call modifier_label%set_text("Modifiers: None")
    call modifier_label%set_name("modifier_display")

    call log_view%set_name("event_log")
    call log_view%set_editable(.false.)
    call log_view%set_text("Event Log:\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with keyboard event handling..."
    call window%show()

    ! Simulate keyboard events
    print '(A)', ""
    print '(A)', "Simulating keyboard events:"

    ! Simulate various key presses
    print '(A)', "  Pressing 'A' key..."
    call simulate_key_press(65, 0)  ! 'A' key, no modifiers

    print '(A)', "  Pressing 'Ctrl+C'..."
    call simulate_key_press(67, 1)  ! 'C' key, Ctrl modifier

    print '(A)', "  Pressing 'Shift+F1'..."
    call simulate_key_press(112, 2)  ! F1 key, Shift modifier

    print '(A)', "  Pressing 'Ctrl+Alt+Delete'..."
    call simulate_key_press(127, 5)  ! Delete key, Ctrl+Alt modifiers

    print '(A)', "  Pressing 'Enter' key..."
    call simulate_key_press(13, 0)  ! Enter key, no modifiers

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

    !> @brief Handler for key press events
    subroutine on_key_press(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: key_text, modifier_text, log_entry

        key_press_count = key_press_count + 1

        ! Get key information (simplified for demo)
        key_text = get_key_name(event%key_code)
        modifier_text = get_modifier_string(event%modifiers)

        ! Update displays
        call key_label%set_text("Last Key: " // trim(key_text))
        call modifier_label%set_text("Modifiers: " // trim(modifier_text))

        ! Log the event
        write(log_entry, '(A,I0,A,A,A,A)') &
            "Key Press #", key_press_count, ": ", trim(key_text), &
            " (", trim(modifier_text), ")"

        call log_view%set_text(log_view%get_text() // trim(log_entry) // "\n")

        print '(A,A,A,A)', "  → Key pressed: ", trim(key_text), " with ", trim(modifier_text)
    end subroutine on_key_press

    !> @brief Handler for key release events
    subroutine on_key_release(event)
        type(forge_event), intent(in) :: event
        character(len:50) :: key_text

        key_text = get_key_name(event%key_code)

        print '(A,A)', "  → Key released: ", trim(key_text)
    end subroutine on_key_release

    !> @brief Simulate a key press event
    subroutine simulate_key_press(key_code, modifiers)
        integer, intent(in) :: key_code, modifiers
        type(forge_event) :: event

        ! Create simulated event
        event%key_code = key_code
        event%modifiers = modifiers

        call on_key_press(event)
    end subroutine simulate_key_press

    !> @brief Get human-readable key name
    function get_key_name(key_code) result(name)
        integer, intent(in) :: key_code
        character(len:20) :: name

        select case (key_code)
        case (13)
            name = "Enter"
        case (27)
            name = "Escape"
        case (32)
            name = "Space"
        case (65:90)
            name = char(key_code)  ! A-Z
        case (112:123)
            write(name, '("F", I0)') key_code - 111  ! F1-F12
        case (127)
            name = "Delete"
        case default
            write(name, '("Key_", I0)') key_code
        end select
    end function get_key_name

    !> @brief Get modifier string
    function get_modifier_string(modifiers) result(str)
        integer, intent(in) :: modifiers
        character(len:50) :: str

        str = ""
        if (iand(modifiers, 1) /= 0) str = trim(str) // "Ctrl+"
        if (iand(modifiers, 2) /= 0) str = trim(str) // "Alt+"
        if (iand(modifiers, 4) /= 0) str = trim(str) // "Shift+"
        if (iand(modifiers, 8) /= 0) str = trim(str) // "Meta+"

        if (len_trim(str) == 0) then
            str = "None"
        else
            ! Remove trailing +
            str = str(1:len_trim(str)-1)
        end if
    end function get_modifier_string

end program event_keyboard_example