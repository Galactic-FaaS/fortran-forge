!> @brief Progress Bar Widget Example
!> @details Demonstrates progress bar widgets for showing operation progress
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program widget_progressbar_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_progress_bar) :: progress1, progress2, progress3
    type(forge_label) :: label1, label2, label3
    type(forge_button) :: start_button, reset_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i

    print '(A)', "=== Progress Bar Widget Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating progress bar widgets"
    print '(A)', "  - Setting progress ranges and values"
    print '(A)', "  - Different progress bar styles"
    print '(A)', "  - Text display on progress bars"
    print '(A)', "  - Indeterminate progress bars"
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
        call builder%set_title("Progress Bar Widget Example")
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

    ! Create progress bars
    print '(A)', "Creating progress bars..."

    ! Determinate progress bar (0-100)
    call progress1%set_name("progress_determinate")
    call progress1%set_range(0, 100)
    call progress1%set_value(25)
    call progress1%set_text_visible(.true.)
    call progress1%set_format("%p%")  ! Show percentage

    ! Indeterminate progress bar
    call progress2%set_name("progress_indeterminate")
    call progress2%set_range(0, 0)  ! Indeterminate mode
    call progress2%set_text_visible(.true.)
    call progress2%set_format("Processing...")

    ! Custom range progress bar
    call progress3%set_name("progress_custom")
    call progress3%set_range(0, 1000)
    call progress3%set_value(750)
    call progress3%set_text_visible(.true.)
    call progress3%set_format("Step %v of %m")

    ! Labels
    call label1%set_text("File Download:")
    call label2%set_text("System Update:")
    call label3%set_text("Data Processing:")

    ! Control buttons
    call start_button%set_label("Start Progress")
    call start_button%set_name("button_start")

    call reset_button%set_label("Reset")
    call reset_button%set_name("button_reset")

    print '(A,I0,A,I0)', "  Progress 1: ", progress1%get_value(), "/", progress1%get_maximum()
    print '(A,I0,A,I0)', "  Progress 3: ", progress3%get_value(), "/", progress3%get_maximum()

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with progress bars..."
    call window%show()

    ! Simulate progress updates
    print '(A)', ""
    print '(A)', "Simulating progress updates:"

    ! Update determinate progress
    do i = 30, 80, 10
        call progress1%set_value(i)
        print '(A,I0,A)', "  Download progress: ", progress1%get_value(), "%"
    end do

    ! Update custom progress
    call progress3%set_value(850)
    print '(A,I0,A,I0)', "  Processing progress: ", progress3%get_value(), " of ", progress3%get_maximum()

    ! Complete operations
    call progress1%set_value(100)
    print '(A)', "  Download complete!"

    call progress3%set_value(1000)
    print '(A)', "  Processing complete!"

    ! Reset progress bars
    print '(A)', ""
    print '(A)', "Resetting progress bars:"
    call progress1%set_value(0)
    call progress3%set_value(0)
    print '(A)', "  All progress bars reset to 0"

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program widget_progressbar_example