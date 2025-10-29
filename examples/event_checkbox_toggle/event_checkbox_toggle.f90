!> @brief Checkbox Toggle Event Example
!> @details Demonstrates handling checkbox state change events
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program event_checkbox_toggle_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_checkbox) :: option1_check, option2_check, option3_check, master_check
    type(forge_label) :: status_label, summary_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Checkbox Toggle Event Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Checkbox state change events"
    print '(A)', "  - Tri-state checkbox handling"
    print '(A)', "  - Master/slave checkbox relationships"
    print '(A)', "  - Event-driven UI updates"
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
        call builder%set_title("Checkbox Toggle Events")
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

    ! Create checkboxes
    print '(A)', "Creating checkboxes with toggle handlers..."

    ! Master checkbox (tri-state)
    call master_check%set_label("Select All")
    call master_check%set_name("master_check")
    call master_check%set_tristate(.true.)
    call master_check%set_checked(.false.)
    call master_check%on_toggled(on_master_toggled)

    ! Individual option checkboxes
    call option1_check%set_label("Option A")
    call option1_check%set_name("option1_check")
    call option1_check%set_checked(.true.)
    call option1_check%on_toggled(on_option_toggled)

    call option2_check%set_label("Option B")
    call option2_check%set_name("option2_check")
    call option2_check%set_checked(.false.)
    call option2_check%on_toggled(on_option_toggled)

    call option3_check%set_label("Option C")
    call option3_check%set_name("option3_check")
    call option3_check%set_checked(.true.)
    call option3_check%on_toggled(on_option_toggled)

    ! Labels
    call status_label%set_name("status_label")
    call status_label%set_text("Toggle checkboxes to see events")

    call summary_label%set_name("summary_label")
    call summary_label%set_text("Summary: A=ON, B=OFF, C=ON")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with event-handling checkboxes..."
    call window%show()

    ! Simulate checkbox toggles
    print '(A)', ""
    print '(A)', "Simulating checkbox toggles:"

    ! Toggle individual options
    print '(A)', "  Toggling Option B..."
    call on_option_toggled(create_test_event())

    print '(A)', "  Toggling Option A..."
    call on_option_toggled(create_test_event())

    ! Use master checkbox
    print '(A)', "  Checking 'Select All'..."
    call on_master_toggled(create_test_event())

    print '(A)', "  Unchecking 'Select All'..."
    call on_master_toggled(create_test_event())

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

    !> @brief Handler for master checkbox toggle
    subroutine on_master_toggled(event)
        type(forge_event), intent(in) :: event
        logical :: all_checked, none_checked

        ! Determine master state based on individual checkboxes
        all_checked = option1_check%is_checked() .and. &
                     option2_check%is_checked() .and. &
                     option3_check%is_checked()

        none_checked = .not. option1_check%is_checked() .and. &
                      .not. option2_check%is_checked() .and. &
                      .not. option3_check%is_checked()

        if (all_checked) then
            call master_check%set_check_state(2)  ! Fully checked
            call status_label%set_text("All options selected")
            print '(A)', "  → Master checkbox: All selected"
        else if (none_checked) then
            call master_check%set_check_state(0)  ! Unchecked
            call status_label%set_text("No options selected")
            print '(A)', "  → Master checkbox: None selected"
        else
            call master_check%set_check_state(1)  ! Partially checked
            call status_label%set_text("Some options selected")
            print '(A)', "  → Master checkbox: Partially selected"
        end if

        call update_summary()
    end subroutine on_master_toggled

    !> @brief Handler for individual option checkbox toggles
    subroutine on_option_toggled(event)
        type(forge_event), intent(in) :: event
        character(len:50) :: status_text

        write(status_text, '(A,L1,A,L1,A,L1)') &
            "Options: A=", option1_check%is_checked(), &
            ", B=", option2_check%is_checked(), &
            ", C=", option3_check%is_checked()
        call status_label%set_text(trim(status_text))

        print '(A,A)', "  → Option toggled: ", trim(status_text)

        call update_summary()
        call on_master_toggled(event)  ! Update master state
    end subroutine on_option_toggled

    !> @brief Update summary display
    subroutine update_summary()
        character(len:50) :: summary_text

        write(summary_text, '(A,L1,A,L1,A,L1)') &
            "Summary: A=", option1_check%is_checked(), &
            ", B=", option2_check%is_checked(), &
            ", C=", option3_check%is_checked()
        call summary_label%set_text(trim(summary_text))
    end subroutine update_summary

end program event_checkbox_toggle_example