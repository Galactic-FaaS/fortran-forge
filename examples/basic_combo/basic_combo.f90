!> @brief Basic Combo Box Example
!> @details Demonstrates combo box widgets with dropdown lists
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_combo_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_combo_box) :: combo1, combo2, combo3
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Combo Box Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating combo box widgets"
    print '(A)', "  - Adding items to combo boxes"
    print '(A)', "  - Setting current selection"
    print '(A)', "  - Reading selected values"
    print '(A)', "  - Editable vs read-only combo boxes"
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
        call builder%set_title("Basic Combo Box Example")
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

    ! Create combo boxes
    print '(A)', "Creating combo boxes..."

    ! Read-only combo box with predefined items
    call combo1%set_name("combo_colors")
    call combo1%set_editable(.false.)
    call combo1%add_item("Red")
    call combo1%add_item("Green")
    call combo1%add_item("Blue")
    call combo1%add_item("Yellow")
    call combo1%set_current_index(0)  ! Select first item
    print '(A,A,A,I1)', "  Combo 1: ", trim(combo1%get_current_text()), " (index ", combo1%get_current_index(), ")"

    ! Editable combo box
    call combo2%set_name("combo_fruits")
    call combo2%set_editable(.true.)
    call combo2%add_item("Apple")
    call combo2%add_item("Banana")
    call combo2%add_item("Orange")
    call combo2%set_current_text("Grape")  ! Custom text
    print '(A,A)', "  Combo 2: ", trim(combo2%get_current_text())

    ! Combo box with many items
    call combo3%set_name("combo_numbers")
    call combo3%set_editable(.false.)
    call populate_numbers(combo3)
    call combo3%set_current_index(5)  ! Select "6"
    print '(A,A,A,I1)', "  Combo 3: ", trim(combo3%get_current_text()), " (index ", combo3%get_current_index(), ")"

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Select items from the combo boxes above")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate selections
    print '(A)', ""
    print '(A)', "Simulating user selections:"

    call combo1%set_current_index(2)  ! Select "Blue"
    print '(A,A)', "  Combo 1 selected: ", trim(combo1%get_current_text())

    call combo2%set_current_text("Pineapple")  ! Custom entry
    print '(A,A)', "  Combo 2 entered: ", trim(combo2%get_current_text())

    call combo3%set_current_index(9)  ! Select "10"
    print '(A,A)', "  Combo 3 selected: ", trim(combo3%get_current_text())

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

    subroutine populate_numbers(combo)
        type(forge_combo_box), intent(inout) :: combo
        integer :: i
        character(len=3) :: num_str

        do i = 1, 20
            write(num_str, '(I2)') i
            call combo%add_item(trim(adjustl(num_str)))
        end do
    end subroutine populate_numbers

    subroutine update_status()
        character(len=200) :: status_text

        write(status_text, '(A,A,A,A,A,A,A,A)') &
            "Selected: Color=", trim(combo1%get_current_text()), &
            ", Fruit=", trim(combo2%get_current_text()), &
            ", Number=", trim(combo3%get_current_text())
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program basic_combo_example