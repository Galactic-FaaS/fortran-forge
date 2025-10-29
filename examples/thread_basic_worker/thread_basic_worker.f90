!> @brief Basic Worker Thread Example
!> @details Demonstrates basic worker thread creation and management
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program thread_basic_worker_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_worker_thread) :: worker_thread
    type(forge_label) :: status_label, progress_label
    type(forge_button) :: start_button, stop_button, pause_button
    type(forge_progress_bar) :: progress_bar
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    logical :: worker_running = .false.
    integer :: progress_value = 0

    print '(A)', "=== Basic Worker Thread Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Worker thread creation and lifecycle"
    print '(A)', "  - Background task execution"
    print '(A)', "  - Thread communication with main UI"
    print '(A)', "  - Progress reporting from worker threads"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - threads won't actually run concurrently"
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
        call builder%set_title("Basic Worker Thread Example")
        call builder%set_size(500, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create worker thread
    print '(A)', "Creating worker thread..."
    call worker_thread%set_name("background_worker")
    call worker_thread%on_started(on_worker_started)
    call worker_thread%on_finished(on_worker_finished)
    call worker_thread%on_progress(on_worker_progress)
    call worker_thread%on_error(on_worker_error)

    ! Create UI elements
    print '(A)', "Creating worker thread control interface..."

    ! Control buttons
    call start_button%set_label("Start Worker")
    call start_button%set_name("start_button")
    call start_button%on_click(on_start_clicked)

    call stop_button%set_label("Stop Worker")
    call stop_button%set_name("stop_button")
    call stop_button%on_click(on_stop_clicked)
    call stop_button%set_enabled(.false.)

    call pause_button%set_label("Pause/Resume")
    call pause_button%set_name("pause_button")
    call pause_button%on_click(on_pause_clicked)
    call pause_button%set_enabled(.false.)

    ! Progress display
    call progress_bar%set_name("progress_bar")
    call progress_bar%set_range(0, 100)
    call progress_bar%set_value(0)
    call progress_bar%set_text_visible(.true.)
    call progress_bar%set_format("Progress: %p%")

    ! Status labels
    call status_label%set_name("status_label")
    call status_label%set_text("Worker thread ready - click Start to begin")

    call progress_label%set_name("progress_label")
    call progress_label%set_text("Progress: 0%")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Worker thread log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing worker thread interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    if (worker_running) then
        call worker_thread%stop()
    end if
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Start the worker thread
    subroutine start_worker()
        print '(A)', "  Starting worker thread..."

        worker_running = .true.
        progress_value = 0

        call start_button%set_enabled(.false.)
        call stop_button%set_enabled(.true.)
        call pause_button%set_enabled(.true.)

        call status_label%set_text("Worker thread starting...")

        ! Start worker thread (simulated)
        call simulate_worker_start()
    end subroutine start_worker

    !> @brief Stop the worker thread
    subroutine stop_worker()
        print '(A)', "  Stopping worker thread..."

        worker_running = .false.

        call start_button%set_enabled(.true.)
        call stop_button%set_enabled(.false.)
        call pause_button%set_enabled(.false.)

        call status_label%set_text("Worker thread stopped")

        call log_view%set_text(log_view%get_text() // "Worker thread stopped by user\n")

        ! Trigger finished event
        call on_worker_finished()
    end subroutine stop_worker

    !> @brief Toggle pause/resume of worker
    subroutine toggle_pause_worker()
        static logical :: paused = .false.

        paused = .not. paused

        if (paused) then
            call status_label%set_text("Worker thread paused")
            call pause_button%set_label("Resume")
            print '(A)', "  Worker thread paused"
        else
            call status_label%set_text("Worker thread resumed")
            call pause_button%set_label("Pause")
            print '(A)', "  Worker thread resumed"
        end if
    end subroutine toggle_pause_worker

    !> @brief Simulate worker thread starting
    subroutine simulate_worker_start()
        call log_view%set_text(log_view%get_text() // "Worker thread started\n")

        ! Trigger started event
        call on_worker_started()

        ! Simulate progress updates
        call simulate_worker_progress()
    end subroutine simulate_worker_start

    !> @brief Simulate worker progress updates
    subroutine simulate_worker_progress()
        integer :: i

        do i = 1, 10
            if (.not. worker_running) exit

            progress_value = progress_value + 10
            call progress_bar%set_value(progress_value)

            write(progress_text, '("Progress: ", I0, "%")') progress_value
            call progress_label%set_text(trim(progress_text))

            ! Trigger progress event
            call on_worker_progress(progress_value)

            ! Simulate work delay
            print '(A,I0,A)', "  Worker progress: ", progress_value, "%"
        end do

        if (worker_running) then
            call on_worker_finished()
        end if
    end subroutine simulate_worker_progress

    !> @brief Handler for worker thread started
    subroutine on_worker_started()
        call status_label%set_text("Worker thread running")
        print '(A)', "  → Worker thread started"
    end subroutine on_worker_started

    !> @brief Handler for worker thread finished
    subroutine on_worker_finished()
        worker_running = .false.

        call start_button%set_enabled(.true.)
        call stop_button%set_enabled(.false.)
        call pause_button%set_enabled(.false.)

        call status_label%set_text("Worker thread completed")
        call log_view%set_text(log_view%get_text() // "Worker thread completed successfully\n")

        print '(A)', "  → Worker thread finished"
    end subroutine on_worker_finished

    !> @brief Handler for worker progress updates
    subroutine on_worker_progress(progress)
        integer, intent(in) :: progress

        write(log_text, '("Progress update: ", I0, "%\n")') progress
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,I0,A)', "  → Worker progress: ", progress, "%"
    end subroutine on_worker_progress

    !> @brief Handler for worker errors
    subroutine on_worker_error(error_msg)
        character(len=*), intent(in) :: error_msg

        call status_label%set_text("Worker error: " // trim(error_msg))
        call log_view%set_text(log_view%get_text() // "Error: " // trim(error_msg) // "\n")

        print '(A,A)', "  → Worker error: ", trim(error_msg)
    end subroutine on_worker_error

    !> @brief Handler for start button click
    subroutine on_start_clicked(event)
        type(forge_event), intent(in) :: event
        call start_worker()
    end subroutine on_start_clicked

    !> @brief Handler for stop button click
    subroutine on_stop_clicked(event)
        type(forge_event), intent(in) :: event
        call stop_worker()
    end subroutine on_stop_clicked

    !> @brief Handler for pause button click
    subroutine on_pause_clicked(event)
        type(forge_event), intent(in) :: event
        call toggle_pause_worker()
    end subroutine on_pause_clicked

end program thread_basic_worker_example