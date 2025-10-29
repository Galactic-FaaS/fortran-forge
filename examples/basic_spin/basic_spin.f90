!> @brief Basic Spin Button Example
!> @details Demonstrates spin button widgets for numeric input
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_spin_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_spin_button) :: spin1, spin2, spin3
    type(forge_label) :: value_label, status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Spin Button Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating spin button widgets"
    print '(A)', "  - Setting numeric ranges and values"
    print '(A)', "  - Different step sizes"
    print '(A)', "  - Integer and decimal spin buttons"
    print '(A)', "  - Reading spin button values"
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
        call builder%set_title("Basic Spin Button Example")
        call builder%set_size(500, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create spin buttons
    print '(A)', "Creating spin buttons..."

    ! Integer spin button (1-100, step 1)
    call spin1%set_name("spin_quantity")
    call spin1%set_range(1, 100)
    call spin1%set_value(10)
    call spin1%set_step(1)
    print '(A,I3,A,I3,A,I3)', "  Spin 1: value=", spin1%get_value(), ", range=", spin1%get_minimum(), "-", spin1%get_maximum()

    ! Integer spin button with larger step (0-1000, step 10)
    call spin2%set_name("spin_amount")
    call spin2%set_range(0, 1000)
    call spin2%set_value(100)
    call spin2%set_step(10)
    print '(A,I4,A,I4,A,I4)', "  Spin 2: value=", spin2%get_value(), ", range=", spin2%get_minimum(), "-", spin2%get_maximum()

    ! Decimal spin button (-10.0 to +10.0, step 0.5)
    call spin3%set_name("spin_precision")
    call spin3%set_range(-10.0d0, 10.0d0)
    call spin3%set_value(0.0d0)
    call spin3%set_step(0.5d0)
    call spin3%set_decimals(2)
    print '(A,F5.2,A,F5.1,A,F5.1)', "  Spin 3: value=", spin3%get_value(), ", range=", spin3%get_minimum(), "-", spin3%get_maximum()

    ! Value display label
    call value_label%set_name("value_display")
    call value_label%set_text("Current Values: Qty=10, Amount=100, Precision=0.00")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Use arrow buttons or type values in the spin boxes")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate value changes
    print '(A)', ""
    print '(A)', "Simulating value changes:"

    call spin1%set_value(25)
    print '(A,I3)', "  Quantity spin set to: ", spin1%get_value()

    call spin2%set_value(250)
    print '(A,I4)', "  Amount spin set to: ", spin2%get_value()

    call spin3%set_value(3.75d0)
    print '(A,F5.2)', "  Precision spin set to: ", spin3%get_value()

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

        write(display_text, '(A,I3,A,I4,A,F5.2)') &
            "Current Values: Qty=", spin1%get_value(), &
            ", Amount=", spin2%get_value(), &
            ", Precision=", spin3%get_value()
        call value_label%set_text(trim(display_text))
        print '(A,A)', "  Display: ", trim(display_text)
    end subroutine update_value_display

end program basic_spin_example