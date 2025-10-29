!> @brief Combo Box Selection Event Example
!> @details Demonstrates handling combo box selection change events
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_combo_select_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_combo_box) :: color_combo, size_combo, editable_combo
    type(forge_label) :: status_label, info_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Combo Box Selection Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Combo box selection change events"
    print '(A)', "  - Editable vs read-only combo boxes"
    print '(A)', "  - Dynamic response to selections"
    print '(A)', "  - Combo box text changes"
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
        call builder%set_title("Combo Box Selection Events")
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
    print '(A)', "Creating combo boxes with selection handlers..."

    ! Color selection combo (read-only)
    call color_combo%set_name("color_combo")
    call color_combo%set_editable(.false.)
    call color_combo%add_item("Red")
    call color_combo%add_item("Green")
    call color_combo%add_item("Blue")
    call color_combo%add_item("Yellow")
    call color_combo%add_item("Purple")
    call color_combo%set_current_index(0)
    call color_combo%on_current_index_changed(on_color_changed)

    ! Size selection combo (read-only)
    call size_combo%set_name("size_combo")
    call size_combo%set_editable(.false.)
    call size_combo%add_item("Small")
    call size_combo%add_item("Medium")
    call size_combo%add_item("Large")
    call size_combo%add_item("Extra Large")
    call size_combo%set_current_index(1)
    call size_combo%on_current_index_changed(on_size_changed)

    ! Editable combo
    call editable_combo%set_name("editable_combo")
    call editable_combo%set_editable(.true.)
    call editable_combo%add_item("Apple")
    call editable_combo%add_item("Banana")
    call editable_combo%add_item("Orange")
    call editable_combo%set_current_text("Custom fruit...")
    call editable_combo%on_current_text_changed(on_text_changed)

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Change combo box selections to trigger events")

    call info_label%set_name("info_label")
    call info_label%set_text("Color: Red, Size: Medium, Custom: Custom fruit...")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with event-handling combo boxes..."
    call window%show()

    ! Simulate combo box selections
    print '(A)', ""
    print '(A)', "Simulating combo box selections:"

    ! Change color selection
    print '(A)', "  Selecting 'Blue' from color combo..."
    call color_combo%set_current_index(2)
    call on_color_changed(create_test_event())

    ! Change size selection
    print '(A)', "  Selecting 'Large' from size combo..."
    call size_combo%set_current_index(2)
    call on_size_changed(create_test_event())

    ! Change editable text
    print '(A)', "  Typing 'Pineapple' in editable combo..."
    call editable_combo%set_current_text("Pineapple")
    call on_text_changed(create_test_event())

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

    !> @brief Handler for color combo selection change
    subroutine on_color_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        write(status_text, '(A,A,A,I0)') &
            "Color changed to: ", trim(color_combo%get_current_text()), &
            " (index ", color_combo%get_current_index(), ")"
        call status_label%set_text(trim(status_text))

        print '(A,A)', "  → Color selection: ", trim(color_combo%get_current_text())

        call update_info()
    end subroutine on_color_changed

    !> @brief Handler for size combo selection change
    subroutine on_size_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        write(status_text, '(A,A,A,I0)') &
            "Size changed to: ", trim(size_combo%get_current_text()), &
            " (index ", size_combo%get_current_index(), ")"
        call status_label%set_text(trim(status_text))

        print '(A,A)', "  → Size selection: ", trim(size_combo%get_current_text())

        call update_info()
    end subroutine on_size_changed

    !> @brief Handler for editable combo text change
    subroutine on_text_changed(event)
        type(forge_event), intent(in) :: event
        character(len:100) :: status_text

        write(status_text, '(A,A)') &
            "Text changed to: ", trim(editable_combo%get_current_text())
        call status_label%set_text(trim(status_text))

        print '(A,A)', "  → Custom text: ", trim(editable_combo%get_current_text())

        call update_info()
    end subroutine on_text_changed

    !> @brief Update info display
    subroutine update_info()
        character(len:150) :: info_text

        write(info_text, '(A,A,A,A,A,A,A,A)') &
            "Color: ", trim(color_combo%get_current_text()), &
            ", Size: ", trim(size_combo%get_current_text()), &
            ", Custom: ", trim(editable_combo%get_current_text())
        call info_label%set_text(trim(info_text))
    end subroutine update_info

end program event_combo_select_example