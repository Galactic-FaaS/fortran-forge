!> @brief Thread Pool Example
!> @details Demonstrates thread pool for managing multiple worker threads efficiently
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program thread_thread_pool_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_thread_pool) :: thread_pool
    type(forge_label) :: status_label, pool_info_label
    type(forge_button) :: submit_task_button, resize_pool_button
    type(forge_entry) :: pool_size_entry, task_count_entry
    type(forge_progress_bar) :: pool_progress_bar
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: pool_size = 4
    integer :: submitted_tasks = 0
    integer :: completed_tasks = 0

    print '(A)', "=== Thread Pool Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Thread pool creation and management"
    print '(A)', "  - Task submission to thread pools"
    print '(A)', "  - Dynamic thread pool resizing"
    print '(A)', "  - Task queuing and execution"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - thread pool won't actually run concurrently"
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
        call builder%set_title("Thread Pool Example")
        call builder%set_size(600, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create thread pool
    print '(A,I0,A)', "Creating thread pool with ", pool_size, " threads..."
    call thread_pool%set_max_threads(pool_size)
    call thread_pool%set_name("worker_pool")
    call thread_pool%on_task_completed(on_task_completed)
    call thread_pool%on_pool_resized(on_pool_resized)

    ! Create UI elements
    print '(A)', "Creating thread pool management interface..."

    ! Configuration entries
    call pool_size_entry%set_placeholder_text("Pool size")
    call pool_size_entry%set_text("4")
    call pool_size_entry%set_name("pool_size_entry")

    call task_count_entry%set_placeholder_text("Number of tasks")
    call task_count_entry%set_text("8")
    call task_count_entry%set_name("task_count_entry")

    ! Control buttons
    call submit_task_button%set_label("Submit Tasks")
    call submit_task_button%set_name("submit_button")
    call submit_task_button%on_click(on_submit_tasks_clicked)

    call resize_pool_button%set_label("Resize Pool")
    call resize_pool_button%set_name("resize_button")
    call resize_pool_button%on_click(on_resize_pool_clicked)

    ! Progress and status
    call pool_progress_bar%set_name("pool_progress")
    call pool_progress_bar%set_range(0, 100)
    call pool_progress_bar%set_value(0)
    call pool_progress_bar%set_text_visible(.true.)
    call pool_progress_bar%set_format("Pool utilization: %p%")

    call status_label%set_name("status_label")
    call status_label%set_text("Thread pool ready - configure and submit tasks")

    call pool_info_label%set_name("pool_info_label")
    call pool_info_label%set_text("Pool: 4 threads, 0 active, 0 queued")

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Thread pool activity log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing thread pool interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call thread_pool%shutdown()
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Submit multiple tasks to the thread pool
    subroutine submit_tasks()
        integer :: num_tasks, i
        character(len=10) :: task_str

        ! Get number of tasks from entry
        read(task_count_entry%get_text(), *, iostat=ios) num_tasks
        if (ios /= 0 .or. num_tasks < 1 .or. num_tasks > 100) then
            call status_label%set_text("Error: Invalid task count (1-100)")
            return
        end if

        print '(A,I0,A)', "  Submitting ", num_tasks, " tasks to thread pool..."

        ! Submit tasks (simulated)
        do i = 1, num_tasks
            write(task_str, '("Task_", I0)') i + submitted_tasks
            call submit_single_task(trim(task_str))
        end do

        submitted_tasks = submitted_tasks + num_tasks

        write(status_text, '(I0, A)') num_tasks, " tasks submitted to pool"
        call status_label%set_text(trim(status_text))

        call update_pool_info()
    end subroutine submit_tasks

    !> @brief Submit a single task to the pool
    subroutine submit_single_task(task_name)
        character(len=*), intent(in) :: task_name

        ! Submit task to pool (simulated)
        call thread_pool%submit_task(task_name, on_task_execute)

        write(log_text, '(A,A,A)', "Submitted: ", trim(task_name), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,A)', "  → Submitted task: ", trim(task_name)
    end subroutine submit_single_task

    !> @brief Resize the thread pool
    subroutine resize_thread_pool()
        integer :: new_size

        ! Get new size from entry
        read(pool_size_entry%get_text(), *, iostat=ios) new_size
        if (ios /= 0 .or. new_size < 1 .or. new_size > 16) then
            call status_label%set_text("Error: Invalid pool size (1-16)")
            return
        end if

        print '(A,I0,A,I0,A)', "  Resizing thread pool from ", pool_size, " to ", new_size, " threads..."

        ! Resize pool
        call thread_pool%resize(new_size)
        pool_size = new_size

        write(status_text, '("Thread pool resized to ", I0, " threads")') pool_size
        call status_label%set_text(trim(status_text))

        call update_pool_info()
    end subroutine resize_thread_pool

    !> @brief Update pool information display
    subroutine update_pool_info()
        integer :: active_count, queued_count
        real :: utilization

        ! Get pool statistics (simulated)
        active_count = min(submitted_tasks - completed_tasks, pool_size)
        queued_count = max(0, submitted_tasks - completed_tasks - pool_size)

        if (pool_size > 0) then
            utilization = real(active_count) / real(pool_size) * 100.0
        else
            utilization = 0.0
        end if

        call pool_progress_bar%set_value(int(utilization))

        write(pool_text, '("Pool: ", I0, " threads, ", I0, " active, ", I0, " queued")') &
            pool_size, active_count, queued_count
        call pool_info_label%set_text(trim(pool_text))
    end subroutine update_pool_info

    !> @brief Task execution function (called by thread pool)
    subroutine on_task_execute(task_name)
        character(len=*), intent(in) :: task_name

        write(log_text, '(A,A,A)', "Executing: ", trim(task_name), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        ! Simulate task execution time
        print '(A,A)', "  → Executing task: ", trim(task_name)

        ! Complete task
        call on_task_completed(trim(task_name))
    end subroutine on_task_execute

    !> @brief Handler for task completion
    subroutine on_task_completed(task_name)
        character(len=*), intent(in) :: task_name

        completed_tasks = completed_tasks + 1

        write(log_text, '(A,A,A)', "Completed: ", trim(task_name), "\n")
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,A)', "  → Task completed: ", trim(task_name)

        call update_pool_info()
    end subroutine on_task_completed

    !> @brief Handler for pool resize
    subroutine on_pool_resized(new_size)
        integer, intent(in) :: new_size

        write(log_text, '("Pool resized to ", I0, " threads\n")') new_size
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,I0,A)', "  → Thread pool resized to ", new_size, " threads"
    end subroutine on_pool_resized

    !> @brief Handler for submit tasks button click
    subroutine on_submit_tasks_clicked(event)
        type(forge_event), intent(in) :: event
        call submit_tasks()
    end subroutine on_submit_tasks_clicked

    !> @brief Handler for resize pool button click
    subroutine on_resize_pool_clicked(event)
        type(forge_event), intent(in) :: event
        call resize_thread_pool()
    end subroutine on_resize_pool_clicked

end program thread_thread_pool_example