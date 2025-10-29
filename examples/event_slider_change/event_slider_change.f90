!> @brief Slider Value Change Event Example
!> @details Demonstrates handling slider value change events
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_slider_change_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_slider) :: volume_slider, brightness_slider, speed_slider
    type(forge_label) :: status_label, value_label
    type(forge_progress_bar) :: volume_bar, brightness_bar
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Slider Value Change Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Slider value change events"
    print '(A)', "  - Real-time UI updates from slider changes"
    print '(A)', "  - Multiple slider types (horizontal/vertical)"
    print '(A)', "  - Connected UI elements responding to changes"
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
        call builder%set_title("Slider Value Change Events")
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

    ! Create sliders
    print '(A)', "Creating sliders with value change handlers..."

    ! Volume slider (horizontal)
    call volume_slider%set_name("volume_slider")
    call volume_slider%set_orientation(0)  ! Horizontal
    call volume_slider%set_range(0, 100)
    call volume_slider%set_value(50)
    call volume_slider%set_step(5)
    call volume_slider%on_value_changed(on_volume_changed)

    ! Brightness slider (vertical)
    call brightness_slider%set_name("brightness_slider")
    call brightness_slider%set_orientation(1)  ! Vertical
    call brightness_slider%set_range(0, 255)
    call brightness_slider%set_value(128)
    call brightness_slider%set_step(10)
    call brightness_slider%on_value_changed(on_brightness_changed)

    ! Speed slider (horizontal, wide range)
    call speed_slider%set_name("speed_slider")
    call speed_slider%set_orientation(0)  ! Horizontal
    call speed_slider%set_range(1, 1000)
    call speed_slider%set_value(100)
    call speed_slider%set_step(25)
    call speed_slider%on_value_changed(on_speed_changed)

    ! Progress bars to show slider values
    call volume_bar%set_name("volume_bar")
    call volume_bar%set_range(0, 100)
    call volume_bar%set_value(50)
    call volume_bar%set_text_visible(.true.)
    call volume_bar%set_format("Volume: %p%")

    call brightness_bar%set_name("brightness_bar")
    call brightness_bar%set_range(0, 255)
    call brightness_bar%set_value(128)
    call brightness_bar%set_text_visible(.true.)
    call brightness_bar%set_format("Brightness: %v/255")

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Move the sliders to see value change events")

    call value_label%set_name("value_label")
    call value_label%set_text("Volume: 50, Brightness: 128, Speed: 100")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with event-handling sliders..."
    call window%show()

    ! Simulate slider value changes
    print '(A)', ""
    print '(A)', "Simulating slider value changes:"

    ! Change volume
    print '(A)', "  Setting volume slider to 75..."
    call volume_slider%set_value(75)
    call on_volume_changed(create_test_event())

    ! Change brightness
    print '(A)', "  Setting brightness slider to 200..."
    call brightness_slider%set_value(200)
    call on_brightness_changed(create_test_event())

    ! Change speed
    print '(A)', "  Setting speed slider to 500..."
    call speed_slider%set_value(500)
    call on_speed_changed(create_test_event())

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

    !> @brief Handler for volume slider value change
    subroutine on_volume_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        call volume_bar%set_value(volume_slider%get_value())

        write(status_text, '(A,I0,A)') &
            "Volume changed to: ", volume_slider%get_value(), "%"
        call status_label%set_text(trim(status_text))

        print '(A,I0)', "  → Volume slider: ", volume_slider%get_value()

        call update_values()
    end subroutine on_volume_changed

    !> @brief Handler for brightness slider value change
    subroutine on_brightness_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        call brightness_bar%set_value(brightness_slider%get_value())

        write(status_text, '(A,I0,A)') &
            "Brightness changed to: ", brightness_slider%get_value(), "/255"
        call status_label%set_text(trim(status_text))

        print '(A,I0)', "  → Brightness slider: ", brightness_slider%get_value()

        call update_values()
    end subroutine on_brightness_changed

    !> @brief Handler for speed slider value change
    subroutine on_speed_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        write(status_text, '(A,I0)') &
            "Speed changed to: ", speed_slider%get_value()
        call status_label%set_text(trim(status_text))

        print '(A,I0)', "  → Speed slider: ", speed_slider%get_value()

        call update_values()
    end subroutine on_speed_changed

    !> @brief Update values display
    subroutine update_values()
        character(len:100) :: value_text

        write(value_text, '(A,I0,A,I0,A,I0)') &
            "Volume: ", volume_slider%get_value(), &
            ", Brightness: ", brightness_slider%get_value(), &
            ", Speed: ", speed_slider%get_value()
        call value_label%set_text(trim(value_text))
    end subroutine update_values

end program event_slider_change_example