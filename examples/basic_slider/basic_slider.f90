!> @brief Basic Slider Example
!> @details Demonstrates slider widgets for value selection
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_slider_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_slider) :: slider1, slider2, slider3
    type(forge_label) :: value_label, status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Slider Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating slider widgets"
    print '(A)', "  - Setting slider ranges and values"
    print '(A)', "  - Horizontal and vertical orientations"
    print '(A)', "  - Reading slider values"
    print '(A)', "  - Slider step sizes"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - no actual GUI will appear"
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
        call builder%set_title("Basic Slider Example")
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
    print '(A)', "Creating sliders..."

    ! Horizontal slider (0-100, default 50)
    call slider1%set_name("slider_volume")
    call slider1%set_orientation(0)  ! Horizontal
    call slider1%set_range(0, 100)
    call slider1%set_value(50)
    call slider1%set_step(5)
    print '(A,I3,A,I3)', "  Slider 1: value=", slider1%get_value(), ", range=0-", slider1%get_maximum()

    ! Vertical slider (0-255, default 128)
    call slider2%set_name("slider_brightness")
    call slider2%set_orientation(1)  ! Vertical
    call slider2%set_range(0, 255)
    call slider2%set_value(128)
    call slider2%set_step(1)
    print '(A,I3,A,I3)', "  Slider 2: value=", slider2%get_value(), ", range=0-", slider2%get_maximum()

    ! Fine control slider (-50 to +50, default 0)
    call slider3%set_name("slider_fine_tune")
    call slider3%set_orientation(0)  ! Horizontal
    call slider3%set_range(-50, 50)
    call slider3%set_value(0)
    call slider3%set_step(1)
    print '(A,I3,A,I3,A,I3)', "  Slider 3: value=", slider3%get_value(), ", range=", slider3%get_minimum(), "-", slider3%get_maximum()

    ! Value display label
    call value_label%set_name("value_display")
    call value_label%set_text("Current Values: Volume=50, Brightness=128, Fine Tune=0")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Move the sliders to change values")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate slider movements
    print '(A)', ""
    print '(A)', "Simulating slider movements:"

    call slider1%set_value(75)
    print '(A,I3)', "  Volume slider set to: ", slider1%get_value()

    call slider2%set_value(200)
    print '(A,I3)', "  Brightness slider set to: ", slider2%get_value()

    call slider3%set_value(-25)
    print '(A,I3)', "  Fine tune slider set to: ", slider3%get_value()

    ! Update display
    call update_value_display()

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

    subroutine update_value_display()
        character(len=100) :: display_text

        write(display_text, '(A,I3,A,I3,A,I3)') &
            "Current Values: Volume=", slider1%get_value(), &
            ", Brightness=", slider2%get_value(), &
            ", Fine Tune=", slider3%get_value()
        call value_label%set_text(trim(display_text))
        print '(A,A)', "  Display: ", trim(display_text)
    end subroutine update_value_display

end program basic_slider_example