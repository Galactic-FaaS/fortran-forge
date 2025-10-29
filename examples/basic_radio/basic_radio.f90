!> @brief Basic Radio Button Example
!> @details Demonstrates radio button groups and exclusive selection
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_radio_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_radiobutton) :: radio1, radio2, radio3, radio4
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Radio Button Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating radio button widgets"
    print '(A)', "  - Grouping radio buttons"
    print '(A)', "  - Exclusive selection behavior"
    print '(A)', "  - Reading selected states"
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
        call builder%set_title("Basic Radio Button Example")
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

    ! Create radio buttons
    print '(A)', "Creating radio buttons..."

    ! Group 1: Color selection
    call radio1%set_name("radio_red")
    call radio1%set_label("Red")
    call radio1%set_group_id(1)
    call radio1%set_checked(.true.)  ! Default selection
    print '(A,A,L1)', "  Radio 1: ", trim(radio1%get_label()), ", checked: ", radio1%is_checked()

    call radio2%set_name("radio_blue")
    call radio2%set_label("Blue")
    call radio2%set_group_id(1)
    call radio2%set_checked(.false.)
    print '(A,A,L1)', "  Radio 2: ", trim(radio2%get_label()), ", checked: ", radio2%is_checked()

    ! Group 2: Size selection
    call radio3%set_name("radio_small")
    call radio3%set_label("Small")
    call radio3%set_group_id(2)
    call radio3%set_checked(.true.)  ! Default selection
    print '(A,A,L1)', "  Radio 3: ", trim(radio3%get_label()), ", checked: ", radio3%is_checked()

    call radio4%set_name("radio_large")
    call radio4%set_label("Large")
    call radio4%set_group_id(2)
    call radio4%set_checked(.false.)
    print '(A,A,L1)', "  Radio 4: ", trim(radio4%get_label()), ", checked: ", radio4%is_checked()

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Select options above")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate selections
    print '(A)', ""
    print '(A)', "Simulating user selections:"

    ! Change color to blue (should uncheck red automatically)
    call radio2%set_checked(.true.)
    print '(A)', "  Selected Blue - Red should be unchecked"
    print '(A,L1,A,L1)', "    Red: ", radio1%is_checked(), ", Blue: ", radio2%is_checked()

    ! Change size to large (should uncheck small automatically)
    call radio4%set_checked(.true.)
    print '(A)', "  Selected Large - Small should be unchecked"
    print '(A,L1,A,L1)', "    Small: ", radio3%is_checked(), ", Large: ", radio4%is_checked()

    ! Update status
    call update_status()

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

    subroutine update_status()
        character(len=100) :: status_text
        character(len=10) :: color, size

        if (radio1%is_checked()) then
            color = "Red"
        else if (radio2%is_checked()) then
            color = "Blue"
        else
            color = "None"
        end if

        if (radio3%is_checked()) then
            size = "Small"
        else if (radio4%is_checked()) then
            size = "Large"
        else
            size = "None"
        end if

        write(status_text, '(A,A,A,A,A)') &
            "Selected: Color=", trim(color), &
            ", Size=", trim(size)
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program basic_radio_example