!> @brief Concurrent Tasks Example
!> @details Demonstrates running multiple concurrent tasks using threads
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program thread_concurrent_tasks_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_worker_thread), allocatable :: worker_threads(:)
    type(forge_label) :: status_label, active_label
    type(forge_button) :: start_all_button, stop_all_button, add_task_button
    type(forge_progress_bar), allocatable :: task_progress(:)
    type(forge_text_view) :: log_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: num_tasks = 3
    logical, allocatable :: task_running(:)
    integer, allocatable :: task_progress_values(:)

    print '(A)', "=== Concurrent Tasks Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Multiple concurrent worker threads"
    print '(A)', "  - Task coordination and synchronization"
    print '(A)', "  - Progress tracking for multiple tasks"
    print '(A)', "  - Dynamic task management"
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
        call builder%set_title("Concurrent Tasks Example")
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

    ! Allocate arrays for tasks
    allocate(worker_threads(num_tasks))
    allocate(task_running(num_tasks))
    allocate(task_progress(num_tasks))
    allocate(task_progress_values(num_tasks))

    ! Initialize task states
    task_running = .false.
    task_progress_values = 0

    ! Create worker threads
    print '(A,I0,A)', "Creating ", num_tasks, " concurrent worker threads..."
    call initialize_worker_threads()

    ! Create UI elements
    print '(A)', "Creating concurrent tasks interface..."

    ! Control buttons
    call start_all_button%set_label("Start All Tasks")
    call start_all_button%set_name("start_all_button")
    call start_all_button%on_click(on_start_all_clicked)

    call stop_all_button%set_label("Stop All Tasks")
    call stop_all_button%set_name("stop_all_button")
    call stop_all_button%on_click(on_stop_all_clicked)

    call add_task_button%set_label("Add New Task")
    call add_task_button%set_name("add_task_button")
    call add_task_button%on_click(on_add_task_clicked)

    ! Status labels
    call status_label%set_name("status_label")
    call status_label%set_text("Concurrent tasks ready - click Start All to begin")

    call active_label%set_name("active_label")
    call active_label%set_text("Active tasks: 0")

    ! Create progress bars for each task
    call create_task_progress_bars()

    ! Log view
    call log_view%set_name("log_view")
    call log_view%set_editable(.false.)
    call log_view%set_text("Task execution log will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing concurrent tasks interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call stop_all_tasks()
    deallocate(worker_threads)
    deallocate(task_running)
    deallocate(task_progress)
    deallocate(task_progress_values)
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Initialize worker threads
    subroutine initialize_worker_threads()
        integer :: i
        character(len=20) :: thread_name

        do i = 1, num_tasks
            write(thread_name, '("worker_task_", I0)') i
            call worker_threads(i)%set_name(trim(thread_name))
            call worker_threads(i)%on_started(on_task_started)
            call worker_threads(i)%on_finished(on_task_finished)
            call worker_threads(i)%on_progress(on_task_progress)
        end do
    end subroutine initialize_worker_threads

    !> @brief Create progress bars for tasks
    subroutine create_task_progress_bars()
        integer :: i
        character(len=20) :: bar_name

        do i = 1, num_tasks
            write(bar_name, '("progress_task_", I0)') i
            call task_progress(i)%set_name(trim(bar_name))
            call task_progress(i)%set_range(0, 100)
            call task_progress(i)%set_value(0)
            call task_progress(i)%set_text_visible(.true.)
            write(format_str, '("Task ", I0, ": %p%")') i
            call task_progress(i)%set_format(trim(format_str))
        end do
    end subroutine create_task_progress_bars

    !> @brief Start all worker tasks
    subroutine start_all_tasks()
        integer :: i

        print '(A,I0,A)', "  Starting ", num_tasks, " concurrent tasks..."

        do i = 1, num_tasks
            task_running(i) = .true.
            task_progress_values(i) = 0

            ! Start task (simulated)
            call simulate_task_start(i)
        end do

        call start_all_button%set_enabled(.false.)
        call stop_all_button%set_enabled(.true.)

        write(status_text, '(I0, A)') num_tasks, " tasks started"
        call status_label%set_text(trim(status_text))

        call update_active_count()
    end subroutine start_all_tasks

    !> @brief Stop all worker tasks
    subroutine stop_all_tasks()
        integer :: i

        print '(A)', "  Stopping all concurrent tasks..."

        do i = 1, num_tasks
            if (task_running(i)) then
                task_running(i) = .false.
                call on_task_finished(i)
            end if
        end do

        call start_all_button%set_enabled(.true.)
        call stop_all_button%set_enabled(.false.)

        call status_label%set_text("All tasks stopped")
        call update_active_count()
    end subroutine stop_all_tasks

    !> @brief Add a new task dynamically
    subroutine add_new_task()
        type(forge_worker_thread), allocatable :: temp_threads(:)
        logical, allocatable :: temp_running(:)
        type(forge_progress_bar), allocatable :: temp_progress(:)
        integer, allocatable :: temp_values(:)
        character(len=20) :: thread_name, bar_name, format_str

        ! Increase task count
        num_tasks = num_tasks + 1

        ! Resize arrays
        allocate(temp_threads(num_tasks))
        allocate(temp_running(num_tasks))
        allocate(temp_progress(num_tasks))
        allocate(temp_values(num_tasks))

        ! Copy existing data
        temp_threads(1:num_tasks-1) = worker_threads
        temp_running(1:num_tasks-1) = task_running
        temp_progress(1:num_tasks-1) = task_progress
        temp_values(1:num_tasks-1) = task_progress_values

        ! Create new task
        write(thread_name, '("worker_task_", I0)') num_tasks
        call temp_threads(num_tasks)%set_name(trim(thread_name))
        call temp_threads(num_tasks)%on_started(on_task_started)
        call temp_threads(num_tasks)%on_finished(on_task_finished)
        call temp_threads(num_tasks)%on_progress(on_task_progress)

        write(bar_name, '("progress_task_", I0)') num_tasks
        call temp_progress(num_tasks)%set_name(trim(bar_name))
        call temp_progress(num_tasks)%set_range(0, 100)
        call temp_progress(num_tasks)%set_value(0)
        call temp_progress(num_tasks)%set_text_visible(.true.)
        write(format_str, '("Task ", I0, ": %p%")') num_tasks
        call temp_progress(num_tasks)%set_format(trim(format_str))

        temp_running(num_tasks) = .false.
        temp_values(num_tasks) = 0

        ! Replace arrays
        deallocate(worker_threads, task_running, task_progress, task_progress_values)
        allocate(worker_threads(num_tasks), task_running(num_tasks), &
                task_progress(num_tasks), task_progress_values(num_tasks))

        worker_threads = temp_threads
        task_running = temp_running
        task_progress = temp_progress
        task_progress_values = temp_values

        deallocate(temp_threads, temp_running, temp_progress, temp_values)

        write(log_text, '("Added new task #", I0, "\n")') num_tasks
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,I0)', "  → Added new task, total tasks now: ", num_tasks
    end subroutine add_new_task

    !> @brief Simulate starting a task
    subroutine simulate_task_start(task_id)
        integer, intent(in) :: task_id

        write(log_text, '("Task ", I0, " started\n")') task_id
        call log_view%set_text(log_view%get_text() // trim(log_text))

        ! Trigger started event
        call on_task_started(task_id)

        ! Simulate progress
        call simulate_task_progress(task_id)
    end subroutine simulate_task_start

    !> @brief Simulate task progress
    subroutine simulate_task_progress(task_id)
        integer, intent(in) :: task_id
        integer :: i

        do i = 1, 10
            if (.not. task_running(task_id)) exit

            task_progress_values(task_id) = task_progress_values(task_id) + 10
            call task_progress(task_id)%set_value(task_progress_values(task_id))

            ! Trigger progress event
            call on_task_progress(task_id, task_progress_values(task_id))
        end do

        if (task_running(task_id)) then
            call on_task_finished(task_id)
        end if
    end subroutine simulate_task_progress

    !> @brief Update active task count display
    subroutine update_active_count()
        integer :: active_count

        active_count = count(task_running)
        write(active_text, '("Active tasks: ", I0, " / ", I0)') active_count, num_tasks
        call active_label%set_text(trim(active_text))
    end subroutine update_active_count

    !> @brief Handler for task started
    subroutine on_task_started(task_id)
        integer, intent(in) :: task_id

        write(log_text, '("Task ", I0, " started\n")') task_id
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,I0,A)', "  → Task ", task_id, " started"
        call update_active_count()
    end subroutine on_task_started

    !> @brief Handler for task finished
    subroutine on_task_finished(task_id)
        integer, intent(in) :: task_id

        task_running(task_id) = .false.

        write(log_text, '("Task ", I0, " completed\n")') task_id
        call log_view%set_text(log_view%get_text() // trim(log_text))

        print '(A,I0,A)', "  → Task ", task_id, " finished"
        call update_active_count()
    end subroutine on_task_finished

    !> @brief Handler for task progress
    subroutine on_task_progress(task_id, progress)
        integer, intent(in) :: task_id, progress

        print '(A,I0,A,I0,A)', "  → Task ", task_id, " progress: ", progress, "%"
    end subroutine on_task_progress

    !> @brief Handler for start all button click
    subroutine on_start_all_clicked(event)
        type(forge_event), intent(in) :: event
        call start_all_tasks()
    end subroutine on_start_all_clicked

    !> @brief Handler for stop all button click
    subroutine on_stop_all_clicked(event)
        type(forge_event), intent(in) :: event
        call stop_all_tasks()
    end subroutine on_stop_all_clicked

    !> @brief Handler for add task button click
    subroutine on_add_task_clicked(event)
        type(forge_event), intent(in) :: event
        call add_new_task()
    end subroutine on_add_task_clicked

end program thread_concurrent_tasks_example