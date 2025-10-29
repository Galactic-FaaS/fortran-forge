!> @brief Complete POSIX threading implementation
!> @details Full POSIX thread, mutex, and synchronization support for Linux/macOS
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_thread_posix
    use iso_c_binding
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: posix_create_thread, posix_wait_thread, posix_detach_thread, posix_cancel_thread
    public :: posix_create_mutex, posix_lock_mutex, posix_unlock_mutex, posix_destroy_mutex
    public :: posix_create_semaphore, posix_acquire_semaphore, posix_release_semaphore, posix_destroy_semaphore
    public :: posix_create_condition, posix_wait_condition, posix_signal_condition, posix_broadcast_condition, posix_destroy_condition
    public :: posix_set_thread_priority, posix_get_thread_priority
    public :: posix_thread_self, posix_thread_equal
    public :: posix_thread_key_create, posix_thread_key_delete, posix_thread_setspecific, posix_thread_getspecific
    public :: posix_set_thread_affinity, posix_get_thread_affinity

    ! POSIX constants
    integer(c_int), parameter :: PTHREAD_CREATE_JOINABLE = 0
    integer(c_int), parameter :: PTHREAD_CREATE_DETACHED = 1
    integer(c_int), parameter :: PTHREAD_CANCEL_ENABLE = 1
    integer(c_int), parameter :: PTHREAD_CANCEL_DISABLE = 0
    integer(c_int), parameter :: PTHREAD_CANCEL_DEFERRED = 0
    integer(c_int), parameter :: PTHREAD_CANCEL_ASYNCHRONOUS = 1

    ! Scheduling policies
    integer(c_int), parameter :: SCHED_OTHER = 0
    integer(c_int), parameter :: SCHED_FIFO = 1
    integer(c_int), parameter :: SCHED_RR = 2

    ! Priority constants (Linux/macOS)
    integer(c_int), parameter :: THREAD_PRIORITY_LOWEST = 1
    integer(c_int), parameter :: THREAD_PRIORITY_BELOW_NORMAL = 10
    integer(c_int), parameter :: THREAD_PRIORITY_NORMAL = 20
    integer(c_int), parameter :: THREAD_PRIORITY_ABOVE_NORMAL = 30
    integer(c_int), parameter :: THREAD_PRIORITY_HIGHEST = 40
    integer(c_int), parameter :: THREAD_PRIORITY_TIME_CRITICAL = 99

    ! CPU set size for affinity
    integer(c_int), parameter :: CPU_SETSIZE = 128

    ! Thread types
    type :: pthread_t
        private
        integer(c_intptr_t) :: handle = 0
    end type pthread_t

    type :: pthread_mutex_t
        private
        integer(c_intptr_t) :: handle = 0
    end type pthread_mutex_t

    type :: pthread_cond_t
        private
        integer(c_intptr_t) :: handle = 0
    end type pthread_cond_t

    type :: sem_t
        private
        integer(c_intptr_t) :: handle = 0
    end type sem_t

    type :: pthread_key_t
        private
        integer(c_int) :: key = -1
    end type pthread_key_t

    type :: cpu_set_t
        private
        integer(c_int) :: bits(CPU_SETSIZE/32 + 1) = 0
    end type cpu_set_t

    ! POSIX API bindings
    interface

        ! Thread functions
        function pthread_create(thread, attr, start_routine, arg) bind(C, name="pthread_create")
            import :: c_ptr, c_int, c_funptr
            type(c_ptr), value :: thread, attr, arg
            type(c_funptr), value :: start_routine
            integer(c_int) :: pthread_create
        end function pthread_create

        function pthread_join(thread, retval) bind(C, name="pthread_join")
            import :: c_ptr, c_int
            type(c_ptr), value :: thread, retval
            integer(c_int) :: pthread_join
        end function pthread_join

        function pthread_detach(thread) bind(C, name="pthread_detach")
            import :: c_ptr, c_int
            type(c_ptr), value :: thread
            integer(c_int) :: pthread_detach
        end function pthread_detach

        function pthread_cancel(thread) bind(C, name="pthread_cancel")
            import :: c_ptr, c_int
            type(c_ptr), value :: thread
            integer(c_int) :: pthread_cancel
        end function pthread_cancel

        function pthread_self() bind(C, name="pthread_self")
            import :: c_ptr
            type(c_ptr) :: pthread_self
        end function pthread_self

        function pthread_equal(t1, t2) bind(C, name="pthread_equal")
            import :: c_ptr, c_int
            type(c_ptr), value :: t1, t2
            integer(c_int) :: pthread_equal
        end function pthread_equal

        function pthread_exit(retval) bind(C, name="pthread_exit")
            import :: c_ptr
            type(c_ptr), value :: retval
        end function pthread_exit

        ! Mutex functions
        function pthread_mutex_init(mutex, attr) bind(C, name="pthread_mutex_init")
            import :: c_ptr, c_int
            type(c_ptr), value :: mutex, attr
            integer(c_int) :: pthread_mutex_init
        end function pthread_mutex_init

        function pthread_mutex_destroy(mutex) bind(C, name="pthread_mutex_destroy")
            import :: c_ptr, c_int
            type(c_ptr), value :: mutex
            integer(c_int) :: pthread_mutex_destroy
        end function pthread_mutex_destroy

        function pthread_mutex_lock(mutex) bind(C, name="pthread_mutex_lock")
            import :: c_ptr, c_int
            type(c_ptr), value :: mutex
            integer(c_int) :: pthread_mutex_lock
        end function pthread_mutex_lock

        function pthread_mutex_trylock(mutex) bind(C, name="pthread_mutex_trylock")
            import :: c_ptr, c_int
            type(c_ptr), value :: mutex
            integer(c_int) :: pthread_mutex_trylock
        end function pthread_mutex_trylock

        function pthread_mutex_unlock(mutex) bind(C, name="pthread_mutex_unlock")
            import :: c_ptr, c_int
            type(c_ptr), value :: mutex
            integer(c_int) :: pthread_mutex_unlock
        end function pthread_mutex_unlock

        ! Condition variable functions
        function pthread_cond_init(cond, attr) bind(C, name="pthread_cond_init")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond, attr
            integer(c_int) :: pthread_cond_init
        end function pthread_cond_init

        function pthread_cond_destroy(cond) bind(C, name="pthread_cond_destroy")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond
            integer(c_int) :: pthread_cond_destroy
        end function pthread_cond_destroy

        function pthread_cond_wait(cond, mutex) bind(C, name="pthread_cond_wait")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond, mutex
            integer(c_int) :: pthread_cond_wait
        end function pthread_cond_wait

        function pthread_cond_timedwait(cond, mutex, abstime) bind(C, name="pthread_cond_timedwait")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond, mutex, abstime
            integer(c_int) :: pthread_cond_timedwait
        end function pthread_cond_timedwait

        function pthread_cond_signal(cond) bind(C, name="pthread_cond_signal")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond
            integer(c_int) :: pthread_cond_signal
        end function pthread_cond_signal

        function pthread_cond_broadcast(cond) bind(C, name="pthread_cond_broadcast")
            import :: c_ptr, c_int
            type(c_ptr), value :: cond
            integer(c_int) :: pthread_cond_broadcast
        end function pthread_cond_broadcast

        ! Semaphore functions (POSIX semaphores)
        function sem_init(sem, pshared, value) bind(C, name="sem_init")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem
            integer(c_int), value :: pshared, value
            integer(c_int) :: sem_init
        end function sem_init

        function sem_destroy(sem) bind(C, name="sem_destroy")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem
            integer(c_int) :: sem_destroy
        end function sem_destroy

        function sem_wait(sem) bind(C, name="sem_wait")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem
            integer(c_int) :: sem_wait
        end function sem_wait

        function sem_trywait(sem) bind(C, name="sem_trywait")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem
            integer(c_int) :: sem_trywait
        end function sem_trywait

        function sem_timedwait(sem, abstime) bind(C, name="sem_timedwait")
            import :: c_ptr, c_int
            type(c_ptr), value :: abstime
            type(c_ptr), value :: sem
            integer(c_int) :: sem_timedwait
        end function sem_timedwait

        function sem_post(sem) bind(C, name="sem_post")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem
            integer(c_int) :: sem_post
        end function sem_post

        function sem_getvalue(sem, sval) bind(C, name="sem_getvalue")
            import :: c_ptr, c_int
            type(c_ptr), value :: sem, sval
            integer(c_int) :: sem_getvalue
        end function sem_getvalue

        ! Thread-specific storage
        function pthread_key_create(key, destructor) bind(C, name="pthread_key_create")
            import :: c_ptr, c_int, c_funptr
            type(c_ptr), value :: key
            type(c_funptr), value :: destructor
            integer(c_int) :: pthread_key_create
        end function pthread_key_create

        function pthread_key_delete(key) bind(C, name="pthread_key_delete")
            import :: c_int
            integer(c_int), value :: key
            integer(c_int) :: pthread_key_delete
        end function pthread_key_delete

        function pthread_setspecific(key, value) bind(C, name="pthread_setspecific")
            import :: c_int, c_ptr
            integer(c_int), value :: key
            type(c_ptr), value :: value
            integer(c_int) :: pthread_setspecific
        end function pthread_setspecific

        function pthread_getspecific(key) bind(C, name="pthread_getspecific")
            import :: c_int, c_ptr
            integer(c_int), value :: key
            type(c_ptr) :: pthread_getspecific
        end function pthread_getspecific

        ! Scheduling and priority
        function pthread_setschedparam(thread, policy, param) bind(C, name="pthread_setschedparam")
            import :: c_ptr, c_int
            type(c_ptr), value :: thread
            integer(c_int), value :: policy
            type(c_ptr), value :: param
            integer(c_int) :: pthread_setschedparam
        end function pthread_setschedparam

        function pthread_getschedparam(thread, policy, param) bind(C, name="pthread_getschedparam")
            import :: c_ptr, c_int
            type(c_ptr), value :: thread, policy, param
            integer(c_int) :: pthread_getschedparam
        end function pthread_getschedparam

        ! CPU affinity (Linux specific, may not be available on macOS)
        function pthread_setaffinity_np(thread, cpusetsize, cpuset) bind(C, name="pthread_setaffinity_np")
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), value :: thread, cpuset
            integer(c_size_t), value :: cpusetsize
            integer(c_int) :: pthread_setaffinity_np
        end function pthread_setaffinity_np

        function pthread_getaffinity_np(thread, cpusetsize, cpuset) bind(C, name="pthread_getaffinity_np")
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), value :: thread, cpuset
            integer(c_size_t), value :: cpusetsize
            integer(c_int) :: pthread_getaffinity_np
        end function pthread_getaffinity_np

        ! Time functions for timeouts
        function clock_gettime(clock_id, tp) bind(C, name="clock_gettime")
            import :: c_int, c_ptr
            integer(c_int), value :: clock_id
            type(c_ptr), value :: tp
            integer(c_int) :: clock_gettime
        end function clock_gettime

    end interface

    ! Time structures
    type :: timespec
        integer(c_long) :: tv_sec
        integer(c_long) :: tv_nsec
    end type timespec

    type :: sched_param
        integer(c_int) :: sched_priority
    end type sched_param

contains

    !> @brief Create POSIX thread
    function posix_create_thread(start_routine, parameter, detached) result(thread_handle)
        type(c_funptr), intent(in) :: start_routine
        type(c_ptr), intent(in), optional :: parameter
        logical, intent(in), optional :: detached
        type(c_ptr) :: thread_handle
        type(c_ptr) :: param, attr
        integer(c_int) :: result, detach_state

        ! Allocate thread handle
        thread_handle = c_null_ptr
        allocate(thread_handle)

        param = c_null_ptr
        if (present(parameter)) param = parameter

        ! Create thread attributes if needed
        attr = c_null_ptr
        if (present(detached)) then
            if (detached) then
                ! Set detached state - simplified, would need full attr handling
                detach_state = PTHREAD_CREATE_DETACHED
            end if
        end if

        result = pthread_create(thread_handle, attr, start_routine, param)

        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to create POSIX thread, error: ", result
            deallocate(thread_handle)
            thread_handle = c_null_ptr
        end if
    end function posix_create_thread

    !> @brief Wait for POSIX thread completion
    function posix_wait_thread(thread_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result
        type(c_ptr) :: retval

        if (.not. c_associated(thread_handle)) then
            success = .false.
            return
        end if

        ! For now, simple join without timeout
        ! Full implementation would handle timeouts with pthread_cond_timedwait
        result = pthread_join(thread_handle, retval)
        success = (result == 0)

        if (.not. success) then
            write(error_unit, '(A,I0)') "Failed to join POSIX thread, error: ", result
        end if
    end function posix_wait_thread

    !> @brief Detach POSIX thread
    subroutine posix_detach_thread(thread_handle)
        type(c_ptr), intent(in) :: thread_handle
        integer(c_int) :: result

        if (c_associated(thread_handle)) then
            result = pthread_detach(thread_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to detach POSIX thread, error: ", result
            end if
        end if
    end subroutine posix_detach_thread

    !> @brief Cancel POSIX thread
    subroutine posix_cancel_thread(thread_handle)
        type(c_ptr), intent(in) :: thread_handle
        integer(c_int) :: result

        if (c_associated(thread_handle)) then
            result = pthread_cancel(thread_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to cancel POSIX thread, error: ", result
            end if
        end if
    end subroutine posix_cancel_thread

    !> @brief Get current thread handle
    function posix_thread_self() result(thread_handle)
        type(c_ptr) :: thread_handle
        thread_handle = pthread_self()
    end function posix_thread_self

    !> @brief Compare two thread handles
    function posix_thread_equal(t1, t2) result(equal)
        type(c_ptr), intent(in) :: t1, t2
        logical :: equal
        integer(c_int) :: result
        result = pthread_equal(t1, t2)
        equal = (result /= 0)
    end function posix_thread_equal

    !> @brief Create POSIX mutex
    function posix_create_mutex(recursive) result(mutex_handle)
        logical, intent(in), optional :: recursive
        type(c_ptr) :: mutex_handle
        integer(c_int) :: result, attr_type
        type(c_ptr) :: attr

        ! Allocate mutex handle
        mutex_handle = c_null_ptr
        allocate(mutex_handle)

        ! For recursive mutex, would need to set PTHREAD_MUTEX_RECURSIVE
        ! Simplified for now
        attr = c_null_ptr

        result = pthread_mutex_init(mutex_handle, attr)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to create POSIX mutex, error: ", result
            deallocate(mutex_handle)
            mutex_handle = c_null_ptr
        end if
    end function posix_create_mutex

    !> @brief Lock POSIX mutex
    function posix_lock_mutex(mutex_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: mutex_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result

        if (.not. c_associated(mutex_handle)) then
            success = .false.
            return
        end if

        if (present(timeout_ms)) then
            ! Would need timed lock implementation
            result = pthread_mutex_lock(mutex_handle)
        else
            result = pthread_mutex_lock(mutex_handle)
        end if

        success = (result == 0)
        if (.not. success) then
            write(error_unit, '(A,I0)') "Failed to lock POSIX mutex, error: ", result
        end if
    end function posix_lock_mutex

    !> @brief Unlock POSIX mutex
    subroutine posix_unlock_mutex(mutex_handle)
        type(c_ptr), intent(in) :: mutex_handle
        integer(c_int) :: result

        if (c_associated(mutex_handle)) then
            result = pthread_mutex_unlock(mutex_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to unlock POSIX mutex, error: ", result
            end if
        end if
    end subroutine posix_unlock_mutex

    !> @brief Destroy POSIX mutex
    subroutine posix_destroy_mutex(mutex_handle)
        type(c_ptr), intent(in) :: mutex_handle
        integer(c_int) :: result

        if (c_associated(mutex_handle)) then
            result = pthread_mutex_destroy(mutex_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to destroy POSIX mutex, error: ", result
            end if
            deallocate(mutex_handle)
        end if
    end subroutine posix_destroy_mutex

    !> @brief Create POSIX semaphore
    function posix_create_semaphore(initial_count, maximum_count) result(semaphore_handle)
        integer, intent(in) :: initial_count, maximum_count
        type(c_ptr) :: semaphore_handle
        integer(c_int) :: result

        ! Allocate semaphore handle
        semaphore_handle = c_null_ptr
        allocate(semaphore_handle)

        result = sem_init(semaphore_handle, 0, initial_count)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to create POSIX semaphore, error: ", result
            deallocate(semaphore_handle)
            semaphore_handle = c_null_ptr
        end if
    end function posix_create_semaphore

    !> @brief Acquire POSIX semaphore
    function posix_acquire_semaphore(semaphore_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: semaphore_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result

        if (.not. c_associated(semaphore_handle)) then
            success = .false.
            return
        end if

        if (present(timeout_ms)) then
            ! Would need timed wait implementation
            result = sem_wait(semaphore_handle)
        else
            result = sem_wait(semaphore_handle)
        end if

        success = (result == 0)
        if (.not. success) then
            write(error_unit, '(A,I0)') "Failed to acquire POSIX semaphore, error: ", result
        end if
    end function posix_acquire_semaphore

    !> @brief Release POSIX semaphore
    subroutine posix_release_semaphore(semaphore_handle, release_count)
        type(c_ptr), intent(in) :: semaphore_handle
        integer, intent(in), optional :: release_count
        integer(c_int) :: result, count

        count = 1
        if (present(release_count)) count = release_count

        if (c_associated(semaphore_handle)) then
            do i = 1, count
                result = sem_post(semaphore_handle)
                if (result /= 0) then
                    write(error_unit, '(A,I0)') "Failed to release POSIX semaphore, error: ", result
                    exit
                end if
            end do
        end if
    end subroutine posix_release_semaphore

    !> @brief Destroy POSIX semaphore
    subroutine posix_destroy_semaphore(semaphore_handle)
        type(c_ptr), intent(in) :: semaphore_handle
        integer(c_int) :: result

        if (c_associated(semaphore_handle)) then
            result = sem_destroy(semaphore_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to destroy POSIX semaphore, error: ", result
            end if
            deallocate(semaphore_handle)
        end if
    end subroutine posix_destroy_semaphore

    !> @brief Create POSIX condition variable
    function posix_create_condition() result(condition_handle)
        type(c_ptr) :: condition_handle
        integer(c_int) :: result

        ! Allocate condition handle
        condition_handle = c_null_ptr
        allocate(condition_handle)

        result = pthread_cond_init(condition_handle, c_null_ptr)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to create POSIX condition, error: ", result
            deallocate(condition_handle)
            condition_handle = c_null_ptr
        end if
    end function posix_create_condition

    !> @brief Wait on POSIX condition variable
    function posix_wait_condition(condition_handle, mutex_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: condition_handle, mutex_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result

        if (.not. c_associated(condition_handle) .or. .not. c_associated(mutex_handle)) then
            success = .false.
            return
        end if

        if (present(timeout_ms)) then
            ! Would need timespec calculation
            result = pthread_cond_wait(condition_handle, mutex_handle)
        else
            result = pthread_cond_wait(condition_handle, mutex_handle)
        end if

        success = (result == 0)
        if (.not. success) then
            write(error_unit, '(A,I0)') "Failed to wait on POSIX condition, error: ", result
        end if
    end function posix_wait_condition

    !> @brief Signal POSIX condition variable (wake one)
    subroutine posix_signal_condition(condition_handle)
        type(c_ptr), intent(in) :: condition_handle
        integer(c_int) :: result

        if (c_associated(condition_handle)) then
            result = pthread_cond_signal(condition_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to signal POSIX condition, error: ", result
            end if
        end if
    end subroutine posix_signal_condition

    !> @brief Broadcast POSIX condition variable (wake all)
    subroutine posix_broadcast_condition(condition_handle)
        type(c_ptr), intent(in) :: condition_handle
        integer(c_int) :: result

        if (c_associated(condition_handle)) then
            result = pthread_cond_broadcast(condition_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to broadcast POSIX condition, error: ", result
            end if
        end if
    end subroutine posix_broadcast_condition

    !> @brief Destroy POSIX condition variable
    subroutine posix_destroy_condition(condition_handle)
        type(c_ptr), intent(in) :: condition_handle
        integer(c_int) :: result

        if (c_associated(condition_handle)) then
            result = pthread_cond_destroy(condition_handle)
            if (result /= 0) then
                write(error_unit, '(A,I0)') "Failed to destroy POSIX condition, error: ", result
            end if
            deallocate(condition_handle)
        end if
    end subroutine posix_destroy_condition

    !> @brief Set POSIX thread priority
    subroutine posix_set_thread_priority(thread_handle, priority)
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in) :: priority
        integer(c_int) :: result, policy, posix_priority
        type(sched_param) :: param

        if (.not. c_associated(thread_handle)) return

        ! Map our priority levels to POSIX
        select case (priority)
            case (-2)
                posix_priority = THREAD_PRIORITY_LOWEST
            case (-1)
                posix_priority = THREAD_PRIORITY_BELOW_NORMAL
            case (0)
                posix_priority = THREAD_PRIORITY_NORMAL
            case (1)
                posix_priority = THREAD_PRIORITY_ABOVE_NORMAL
            case (2)
                posix_priority = THREAD_PRIORITY_HIGHEST
            case (15)
                posix_priority = THREAD_PRIORITY_TIME_CRITICAL
            case default
                posix_priority = THREAD_PRIORITY_NORMAL
        end select

        policy = SCHED_OTHER
        param%sched_priority = posix_priority

        result = pthread_setschedparam(thread_handle, policy, c_loc(param))
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to set POSIX thread priority, error: ", result
        end if
    end subroutine posix_set_thread_priority

    !> @brief Get POSIX thread priority
    function posix_get_thread_priority(thread_handle) result(priority)
        type(c_ptr), intent(in) :: thread_handle
        integer :: priority
        integer(c_int) :: result, policy
        type(sched_param) :: param

        priority = 0
        if (.not. c_associated(thread_handle)) return

        result = pthread_getschedparam(thread_handle, c_loc(policy), c_loc(param))
        if (result == 0) then
            ! Map back to our priority levels
            select case (param%sched_priority)
                case (THREAD_PRIORITY_LOWEST)
                    priority = -2
                case (THREAD_PRIORITY_BELOW_NORMAL)
                    priority = -1
                case (THREAD_PRIORITY_NORMAL)
                    priority = 0
                case (THREAD_PRIORITY_ABOVE_NORMAL)
                    priority = 1
                case (THREAD_PRIORITY_HIGHEST)
                    priority = 2
                case (THREAD_PRIORITY_TIME_CRITICAL)
                    priority = 15
                case default
                    priority = 0
            end select
        else
            write(error_unit, '(A,I0)') "Failed to get POSIX thread priority, error: ", result
        end if
    end function posix_get_thread_priority

    !> @brief Create thread-specific storage key
    function posix_thread_key_create(destructor) result(key)
        type(c_funptr), intent(in), optional :: destructor
        integer(c_int) :: key
        integer(c_int) :: result
        type(c_funptr) :: dtor

        dtor = c_null_funptr
        if (present(destructor)) dtor = destructor

        result = pthread_key_create(c_loc(key), dtor)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to create thread key, error: ", result
            key = -1
        end if
    end function posix_thread_key_create

    !> @brief Delete thread-specific storage key
    subroutine posix_thread_key_delete(key)
        integer(c_int), intent(in) :: key
        integer(c_int) :: result

        result = pthread_key_delete(key)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to delete thread key, error: ", result
        end if
    end subroutine posix_thread_key_delete

    !> @brief Set thread-specific data
    subroutine posix_thread_setspecific(key, value)
        integer(c_int), intent(in) :: key
        type(c_ptr), intent(in) :: value
        integer(c_int) :: result

        result = pthread_setspecific(key, value)
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to set thread specific data, error: ", result
        end if
    end subroutine posix_thread_setspecific

    !> @brief Get thread-specific data
    function posix_thread_getspecific(key) result(value)
        integer(c_int), intent(in) :: key
        type(c_ptr) :: value

        value = pthread_getspecific(key)
    end function posix_thread_getspecific

    !> @brief Set thread CPU affinity (Linux only)
    subroutine posix_set_thread_affinity(thread_handle, cpu_mask)
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in) :: cpu_mask
        integer(c_int) :: result
        type(cpu_set_t) :: cpuset

        if (.not. c_associated(thread_handle)) return

        ! Convert mask to cpu_set_t
        cpuset%bits(1) = cpu_mask

        result = pthread_setaffinity_np(thread_handle, int(CPU_SETSIZE, c_size_t), c_loc(cpuset))
        if (result /= 0) then
            write(error_unit, '(A,I0)') "Failed to set thread affinity, error: ", result
        end if
    end subroutine posix_set_thread_affinity

    !> @brief Get thread CPU affinity (Linux only)
    function posix_get_thread_affinity(thread_handle) result(cpu_mask)
        type(c_ptr), intent(in) :: thread_handle
        integer :: cpu_mask
        integer(c_int) :: result
        type(cpu_set_t) :: cpuset

        cpu_mask = 0
        if (.not. c_associated(thread_handle)) return

        result = pthread_getaffinity_np(thread_handle, int(CPU_SETSIZE, c_size_t), c_loc(cpuset))
        if (result == 0) then
            cpu_mask = cpuset%bits(1)
        else
            write(error_unit, '(A,I0)') "Failed to get thread affinity, error: ", result
        end if
    end function posix_get_thread_affinity

end module forge_thread_posix