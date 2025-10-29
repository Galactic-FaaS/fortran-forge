!> @brief Threading abstraction
!> @details Cross-platform threading (QThread equivalent)
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_thread
    use iso_c_binding
    use forge_errors
    use forge_signals
    implicit none
    private

    public :: QThread, QMutex, QSemaphore, QWaitCondition
    public :: lock_mutex, unlock_mutex

    !> @brief Thread abstraction
    type :: QThread
        private
        type(c_ptr) :: thread_handle = c_null_ptr
        integer :: thread_id = 0
        logical :: running = .false.
        logical :: finished_flag = .false.
        logical :: terminated_flag = .false.
        integer :: priority = 0  ! Thread priority
        type(signal_void) :: started
        type(signal_void) :: finished
        type(signal_void) :: terminated
    contains
        procedure :: start => thread_start
        procedure :: wait => thread_wait
        procedure :: is_running => thread_is_running
        procedure :: is_finished => thread_is_finished
        procedure :: is_terminated => thread_is_terminated
        procedure :: terminate => thread_terminate
        procedure :: set_priority => thread_set_priority
        procedure :: priority => thread_get_priority
        procedure :: run => thread_run  ! Virtual method to override
    end type QThread

    !> @brief Mutex for thread synchronization
    type :: QMutex
        private
        type(c_ptr) :: mutex_handle = c_null_ptr
        logical :: initialized = .false.
        logical :: recursive = .false.
        integer :: recursion_count = 0
        integer :: owning_thread_id = 0
    contains
        procedure :: lock => mutex_lock
        procedure :: unlock => mutex_unlock
        procedure :: try_lock => mutex_try_lock
        procedure :: init => mutex_init
        procedure :: destroy => mutex_destroy
        procedure :: is_recursive => mutex_is_recursive
    end type QMutex

    !> @brief Semaphore
    type :: QSemaphore
        private
        type(c_ptr) :: semaphore_handle = c_null_ptr
        integer :: count = 0
    contains
        procedure :: acquire => semaphore_acquire
        procedure :: release => semaphore_release
        procedure :: try_acquire => semaphore_try_acquire
        procedure :: available => semaphore_available
    end type QSemaphore

    !> @brief Wait condition
    type :: QWaitCondition
        private
        type(c_ptr) :: condition_handle = c_null_ptr
    contains
        procedure :: wait => condition_wait
        procedure :: wake_one => condition_wake_one
        procedure :: wake_all => condition_wake_all
    end type QWaitCondition

contains

    ! ========== QThread Implementation ==========

    subroutine thread_start(this, priority)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QThread), intent(inout) :: this
        integer, intent(in), optional :: priority

        if (this%running) return

        if (present(priority)) this%priority = priority

#ifdef _WIN32
        ! Create thread with proper procedure
        this%thread_handle = win_create_thread(c_funloc(thread_wrapper), c_loc(this))
        if (.not. c_associated(this%thread_handle)) then
            call forge_error("Failed to create thread")
            return
        end if
#endif

        this%running = .true.
        this%finished_flag = .false.
        this%terminated_flag = .false.
        call this%started%emit()
    end subroutine thread_start

    subroutine thread_wait(this, time)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QThread), intent(inout) :: this
        integer, intent(in), optional :: time  ! milliseconds
        logical :: success

        if (.not. c_associated(this%thread_handle)) return

#ifdef _WIN32
        success = win_wait_thread(this%thread_handle, time)
        if (success) then
            call win_close_thread(this%thread_handle)
            this%thread_handle = c_null_ptr
        end if
#endif

        this%running = .false.
        this%finished_flag = .true.
        call this%finished%emit()
    end subroutine thread_wait

    function thread_is_running(this) result(running)
        class(QThread), intent(in) :: this
        logical :: running
        running = this%running
    end function thread_is_running

    function thread_is_finished(this) result(finished)
        class(QThread), intent(in) :: this
        logical :: finished
        finished = this%finished_flag
    end function thread_is_finished

    function thread_is_terminated(this) result(terminated)
        class(QThread), intent(in) :: this
        logical :: terminated
        terminated = this%terminated_flag
    end function thread_is_terminated

    subroutine thread_terminate(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QThread), intent(inout) :: this

        if (c_associated(this%thread_handle)) then
#ifdef _WIN32
            ! Force terminate (unsafe!)
            interface
                function TerminateThread(h, code) bind(C, name="TerminateThread")
                    import :: c_ptr, c_int
                    type(c_ptr), value :: h
                    integer(c_int), value :: code
                    integer(c_int) :: TerminateThread
                end function
            end interface
            call TerminateThread(this%thread_handle, 0)
            call win_close_thread(this%thread_handle)
            this%thread_handle = c_null_ptr
#endif
        end if

        this%running = .false.
        this%finished_flag = .true.
        this%terminated_flag = .true.
        call this%terminated%emit()
    end subroutine thread_terminate

    subroutine thread_set_priority(this, priority)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QThread), intent(inout) :: this
        integer, intent(in) :: priority

        this%priority = priority

#ifdef _WIN32
        if (c_associated(this%thread_handle)) then
            call win_set_thread_priority(this%thread_handle, priority)
        end if
#endif
    end subroutine thread_set_priority

    function thread_get_priority(this) result(priority)
        class(QThread), intent(in) :: this
        integer :: priority
        priority = this%priority
    end function thread_get_priority

    subroutine thread_run(this)
        class(QThread), intent(inout) :: this
        ! Virtual method - override in derived classes
        ! Default implementation does nothing
    end subroutine thread_run

    ! ========== QMutex Implementation ==========

    subroutine mutex_init(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this

        if (this%initialized) return

#ifdef _WIN32
        this%mutex_handle = win_create_mutex()
#endif

        this%initialized = c_associated(this%mutex_handle)
    end subroutine mutex_init

    subroutine mutex_lock(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        logical :: success
        integer :: current_thread_id

        if (.not. this%initialized) call this%init()

        ! Get current thread ID (simplified - in real implementation would use GetCurrentThreadId)
        current_thread_id = 1  ! Placeholder

        if (this%recursive .and. this%owning_thread_id == current_thread_id) then
            this%recursion_count = this%recursion_count + 1
            return
        end if

#ifdef _WIN32
        success = win_lock_mutex(this%mutex_handle)
        if (success) then
            this%owning_thread_id = current_thread_id
            this%recursion_count = 1
        end if
#endif
    end subroutine mutex_lock

    subroutine mutex_unlock(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this

        if (this%recursion_count > 1) then
            this%recursion_count = this%recursion_count - 1
            return
        end if

#ifdef _WIN32
        call win_unlock_mutex(this%mutex_handle)
#endif

        this%owning_thread_id = 0
        this%recursion_count = 0
    end subroutine mutex_unlock

    function mutex_try_lock(this) result(success)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        logical :: success
        integer :: current_thread_id

        if (.not. this%initialized) call this%init()

        current_thread_id = 1  ! Placeholder

        if (this%recursive .and. this%owning_thread_id == current_thread_id) then
            this%recursion_count = this%recursion_count + 1
            success = .true.
            return
        end if

#ifdef _WIN32
        success = win_lock_mutex(this%mutex_handle, timeout_ms=0)
        if (success) then
            this%owning_thread_id = current_thread_id
            this%recursion_count = 1
        end if
#endif
    end function mutex_try_lock

    function mutex_is_recursive(this) result(recursive)
        class(QMutex), intent(in) :: this
        logical :: recursive
        recursive = this%recursive
    end function mutex_is_recursive

    subroutine mutex_destroy(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        
        if (.not. this%initialized) return
        
#ifdef _WIN32
        call win_destroy_mutex(this%mutex_handle)
#endif
        
        this%initialized = .false.
    end subroutine mutex_destroy

    ! ========== QSemaphore Implementation ==========

    subroutine semaphore_acquire(this, n)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n
        integer :: acquire_count
        logical :: success

        acquire_count = 1
        if (present(n)) acquire_count = n

        if (.not. c_associated(this%semaphore_handle)) then
            this%semaphore_handle = win_create_semaphore(this%count, 1000)  ! Max count 1000
        end if

#ifdef _WIN32
        success = win_acquire_semaphore(this%semaphore_handle)
        if (success) then
            this%count = this%count - acquire_count
        end if
#endif
    end subroutine semaphore_acquire

    subroutine semaphore_release(this, n)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n
        integer :: release_count

        release_count = 1
        if (present(n)) release_count = n

        if (.not. c_associated(this%semaphore_handle)) then
            this%semaphore_handle = win_create_semaphore(this%count, 1000)
        end if

#ifdef _WIN32
        call win_release_semaphore(this%semaphore_handle, release_count)
        this%count = this%count + release_count
#endif
    end subroutine semaphore_release

    function semaphore_try_acquire(this, n, timeout) result(success)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n, timeout
        logical :: success
        integer :: acquire_count, timeout_ms

        acquire_count = 1
        if (present(n)) acquire_count = n
        timeout_ms = 0
        if (present(timeout)) timeout_ms = timeout

        if (.not. c_associated(this%semaphore_handle)) then
            this%semaphore_handle = win_create_semaphore(this%count, 1000)
        end if

#ifdef _WIN32
        success = win_acquire_semaphore(this%semaphore_handle, timeout_ms)
        if (success) then
            this%count = this%count - acquire_count
        end if
#endif
    end function semaphore_try_acquire

    function semaphore_available(this) result(available)
        class(QSemaphore), intent(in) :: this
        integer :: available
        available = this%count
    end function semaphore_available

    ! ========== QWaitCondition Implementation ==========

    subroutine condition_wait(this, mutex, time)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QWaitCondition), intent(inout) :: this
        type(QMutex), intent(inout) :: mutex
        integer, intent(in), optional :: time
        logical :: success
        integer :: timeout_ms

        timeout_ms = -1  ! Infinite
        if (present(time)) timeout_ms = time

        if (.not. c_associated(this%condition_handle)) then
            this%condition_handle = win_create_event(.false., .false.)  ! Auto-reset, non-signaled
        end if

        call mutex%unlock()

#ifdef _WIN32
        success = win_wait_event(this%condition_handle, timeout_ms)
#endif

        call mutex%lock()
    end subroutine condition_wait

    subroutine condition_wake_one(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QWaitCondition), intent(inout) :: this

        if (.not. c_associated(this%condition_handle)) then
            this%condition_handle = win_create_event(.false., .false.)
        end if

#ifdef _WIN32
        call win_set_event(this%condition_handle)
#endif
    end subroutine condition_wake_one

    subroutine condition_wake_all(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QWaitCondition), intent(inout) :: this

        if (.not. c_associated(this%condition_handle)) then
            this%condition_handle = win_create_event(.true., .false.)  ! Manual reset
        end if

#ifdef _WIN32
        call win_set_event(this%condition_handle)
#endif
    end subroutine condition_wake_all

    ! ========== Helper Functions ==========

    subroutine lock_mutex(mutex)
        type(QMutex), intent(inout) :: mutex
        call mutex%lock()
    end subroutine lock_mutex

    subroutine unlock_mutex(mutex)
        type(QMutex), intent(inout) :: mutex
        call mutex%unlock()
    end subroutine unlock_mutex

    ! ========== Thread Wrapper Function ==========

    subroutine thread_wrapper(param) bind(C)
        type(c_ptr), value :: param
        type(QThread), pointer :: thread_ptr

        call c_f_pointer(param, thread_ptr)
        call thread_ptr%run()
        thread_ptr%running = .false.
        thread_ptr%finished_flag = .true.
        call thread_ptr%finished%emit()
    end subroutine thread_wrapper

end module forge_thread

