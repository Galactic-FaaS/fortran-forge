!> @brief Basic Checkbox Example
!> @details Demonstrates checkbox widgets and state management
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_checkbox_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_checkbox) :: checkbox1, checkbox2, checkbox3
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Checkbox Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating checkbox widgets"
    print '(A)', "  - Setting checkbox states"
    print '(A)', "  - Reading checkbox states"
    print '(A)', "  - Tri-state checkboxes"
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
        call builder%set_title("Basic Checkbox Example")
        call builder%set_size(400, 250)
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
    print '(A)', "Creating checkboxes..."

    ! Regular checkbox - unchecked
    call checkbox1%set_name("checkbox_option1")
    call checkbox1%set_label("Enable feature A")
    call checkbox1%set_checked(.false.)
    print '(A,A,L1)', "  Checkbox 1: ", trim(checkbox1%get_label()), ", checked: ", checkbox1%is_checked()

    ! Regular checkbox - checked
    call checkbox2%set_name("checkbox_option2")
    call checkbox2%set_label("Enable feature B")
    call checkbox2%set_checked(.true.)
    print '(A,A,L1)', "  Checkbox 2: ", trim(checkbox2%get_label()), ", checked: ", checkbox2%is_checked()

    ! Tri-state checkbox
    call checkbox3%set_name("checkbox_option3")
    call checkbox3%set_label("Mixed state option")
    call checkbox3%set_tristate(.true.)
    call checkbox3%set_check_state(1)  ! Partially checked
    print '(A,A,I1)', "  Checkbox 3: ", trim(checkbox3%get_label()), ", state: ", checkbox3%get_check_state()

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Check the boxes above to see state changes")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window..."
    call window%show()

    ! Simulate state changes
    print '(A)', ""
    print '(A)', "Simulating user interactions:"
    call checkbox1%set_checked(.true.)
    print '(A,L1)', "  Checkbox 1 now checked: ", checkbox1%is_checked()

    call checkbox2%set_checked(.false.)
    print '(A,L1)', "  Checkbox 2 now unchecked: ", checkbox2%is_checked()

    call checkbox3%set_check_state(2)  ! Fully checked
    print '(A,I1)', "  Checkbox 3 state changed to: ", checkbox3%get_check_state()

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
        write(status_text, '(A,L1,A,L1,A,I1)') &
            "Status: A=", checkbox1%is_checked(), &
            ", B=", checkbox2%is_checked(), &
            ", C=", checkbox3%get_check_state()
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program basic_checkbox_example