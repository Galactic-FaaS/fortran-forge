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
        logical :: running = .false.
        logical :: finished_flag = .false.
        type(signal_void) :: started
        type(signal_void) :: finished
    contains
        procedure :: start => thread_start
        procedure :: wait => thread_wait
        procedure :: is_running => thread_is_running
        procedure :: is_finished => thread_is_finished
        procedure :: terminate => thread_terminate
    end type QThread

    !> @brief Mutex for thread synchronization
    type :: QMutex
        private
        type(c_ptr) :: mutex_handle = c_null_ptr
        logical :: initialized = .false.
    contains
        procedure :: lock => mutex_lock
        procedure :: unlock => mutex_unlock
        procedure :: try_lock => mutex_try_lock
        procedure :: init => mutex_init
        procedure :: destroy => mutex_destroy
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
        
#ifdef _WIN32
        ! CreateThread on Windows
        ! Note: In real implementation, would pass thread procedure
        ! For now, create handle placeholder
        ! this%thread_handle = win_create_thread(c_funloc(thread_proc), c_null_ptr)
#else
        ! pthread_create on POSIX
        ! this%thread_handle = pthread_create(...)
#endif
        
        this%running = .true.
        this%finished_flag = .false.
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
        call win_close_thread(this%thread_handle)
#else
        ! pthread_join on POSIX
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
#endif
        end if
        
        this%running = .false.
        this%finished_flag = .true.
    end subroutine thread_terminate

    ! ========== QMutex Implementation ==========

    subroutine mutex_init(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        
        if (this%initialized) return
        
#ifdef _WIN32
        this%mutex_handle = win_create_mutex()
#else
        ! pthread_mutex_init
#endif
        
        this%initialized = c_associated(this%mutex_handle)
    end subroutine mutex_init

    subroutine mutex_lock(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        logical :: success
        
        if (.not. this%initialized) call this%init()
        
#ifdef _WIN32
        success = win_lock_mutex(this%mutex_handle)
#else
        ! pthread_mutex_lock
#endif
    end subroutine mutex_lock

    subroutine mutex_unlock(this)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        
#ifdef _WIN32
        call win_unlock_mutex(this%mutex_handle)
#else
        ! pthread_mutex_unlock
#endif
    end subroutine mutex_unlock

    function mutex_try_lock(this) result(success)
#ifdef _WIN32
        use forge_thread_windows
#endif
        class(QMutex), intent(inout) :: this
        logical :: success
        
        if (.not. this%initialized) call this%init()
        
#ifdef _WIN32
        success = win_lock_mutex(this%mutex_handle, timeout_ms=0)
#else
        ! pthread_mutex_trylock
        success = .false.
#endif
    end function mutex_try_lock

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
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n
        integer :: acquire_count
        
        acquire_count = 1
        if (present(n)) acquire_count = n
        
        ! TODO: Semaphore acquire
        this%count = this%count - acquire_count
    end subroutine semaphore_acquire

    subroutine semaphore_release(this, n)
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n
        integer :: release_count
        
        release_count = 1
        if (present(n)) release_count = n
        
        ! TODO: Semaphore release
        this%count = this%count + release_count
    end subroutine semaphore_release

    function semaphore_try_acquire(this, n, timeout) result(success)
        class(QSemaphore), intent(inout) :: this
        integer, intent(in), optional :: n, timeout
        logical :: success
        
        ! TODO: Try acquire with timeout
        success = .false.
    end function semaphore_try_acquire

    function semaphore_available(this) result(available)
        class(QSemaphore), intent(in) :: this
        integer :: available
        available = this%count
    end function semaphore_available

    ! ========== QWaitCondition Implementation ==========

    subroutine condition_wait(this, mutex, time)
        class(QWaitCondition), intent(inout) :: this
        type(QMutex), intent(inout) :: mutex
        integer, intent(in), optional :: time
        
        ! TODO: Wait on condition variable
    end subroutine condition_wait

    subroutine condition_wake_one(this)
        class(QWaitCondition), intent(inout) :: this
        
        ! TODO: Wake one waiting thread
    end subroutine condition_wake_one

    subroutine condition_wake_all(this)
        class(QWaitCondition), intent(inout) :: this
        
        ! TODO: Wake all waiting threads
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

end module forge_thread

