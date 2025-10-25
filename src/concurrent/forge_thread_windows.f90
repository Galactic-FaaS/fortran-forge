!> @brief Complete Windows threading implementation
!> @details Full Win32 thread, mutex, and synchronization support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_thread_windows
    use iso_c_binding
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: win_create_thread, win_wait_thread, win_close_thread
    public :: win_create_mutex, win_lock_mutex, win_unlock_mutex, win_destroy_mutex
    public :: win_create_semaphore, win_acquire_semaphore, win_release_semaphore
    public :: win_create_event, win_wait_event, win_set_event, win_reset_event

    ! Win32 constants
    integer(c_int), parameter :: INFINITE = -1
    integer(c_int), parameter :: WAIT_OBJECT_0 = 0
    integer(c_int), parameter :: WAIT_TIMEOUT = 258

    ! Win32 API bindings
    interface
        
        ! Thread functions
        function CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress, &
                             lpParameter, dwCreationFlags, lpThreadId) &
                bind(C, name="CreateThread")
            import :: c_ptr, c_int, c_funptr
            type(c_ptr), value :: lpThreadAttributes, lpParameter
            integer(c_int), value :: dwStackSize, dwCreationFlags
            type(c_funptr), value :: lpStartAddress
            type(c_ptr) :: lpThreadId
            type(c_ptr) :: CreateThread
        end function CreateThread

        function WaitForSingleObject(hHandle, dwMilliseconds) &
                bind(C, name="WaitForSingleObject")
            import :: c_ptr, c_int
            type(c_ptr), value :: hHandle
            integer(c_int), value :: dwMilliseconds
            integer(c_int) :: WaitForSingleObject
        end function WaitForSingleObject

        function CloseHandle(hObject) bind(C, name="CloseHandle")
            import :: c_ptr, c_int
            type(c_ptr), value :: hObject
            integer(c_int) :: CloseHandle
        end function CloseHandle

        function TerminateThread(hThread, dwExitCode) bind(C, name="TerminateThread")
            import :: c_ptr, c_int
            type(c_ptr), value :: hThread
            integer(c_int), value :: dwExitCode
            integer(c_int) :: TerminateThread
        end function TerminateThread

        ! Mutex functions
        function CreateMutexA(lpMutexAttributes, bInitialOwner, lpName) &
                bind(C, name="CreateMutexA")
            import :: c_ptr, c_int
            type(c_ptr), value :: lpMutexAttributes, lpName
            integer(c_int), value :: bInitialOwner
            type(c_ptr) :: CreateMutexA
        end function CreateMutexA

        function ReleaseMutex(hMutex) bind(C, name="ReleaseMutex")
            import :: c_ptr, c_int
            type(c_ptr), value :: hMutex
            integer(c_int) :: ReleaseMutex
        end function ReleaseMutex

        ! Semaphore functions
        function CreateSemaphoreA(lpSemaphoreAttributes, lInitialCount, &
                                 lMaximumCount, lpName) &
                bind(C, name="CreateSemaphoreA")
            import :: c_ptr, c_int
            type(c_ptr), value :: lpSemaphoreAttributes, lpName
            integer(c_int), value :: lInitialCount, lMaximumCount
            type(c_ptr) :: CreateSemaphoreA
        end function CreateSemaphoreA

        function ReleaseSemaphore(hSemaphore, lReleaseCount, lpPreviousCount) &
                bind(C, name="ReleaseSemaphore")
            import :: c_ptr, c_int
            type(c_ptr), value :: hSemaphore, lpPreviousCount
            integer(c_int), value :: lReleaseCount
            integer(c_int) :: ReleaseSemaphore
        end function ReleaseSemaphore

        ! Event functions (condition variables)
        function CreateEventA(lpEventAttributes, bManualReset, bInitialState, lpName) &
                bind(C, name="CreateEventA")
            import :: c_ptr, c_int
            type(c_ptr), value :: lpEventAttributes, lpName
            integer(c_int), value :: bManualReset, bInitialState
            type(c_ptr) :: CreateEventA
        end function CreateEventA

        function SetEvent(hEvent) bind(C, name="SetEvent")
            import :: c_ptr, c_int
            type(c_ptr), value :: hEvent
            integer(c_int) :: SetEvent
        end function SetEvent

        function ResetEvent(hEvent) bind(C, name="ResetEvent")
            import :: c_ptr, c_int
            type(c_ptr), value :: hEvent
            integer(c_int) :: ResetEvent
        end function ResetEvent

    end interface

contains

    !> @brief Create Windows thread
    function win_create_thread(start_routine, parameter) result(thread_handle)
        type(c_funptr), intent(in) :: start_routine
        type(c_ptr), intent(in), optional :: parameter
        type(c_ptr) :: thread_handle
        type(c_ptr) :: param, thread_id
        
        if (present(parameter)) then
            param = parameter
        else
            param = c_null_ptr
        end if
        
        thread_handle = CreateThread(c_null_ptr, 0, start_routine, param, 0, thread_id)
        
        if (.not. c_associated(thread_handle)) then
            write(error_unit, '(A)') "Failed to create thread"
        end if
    end function win_create_thread

    !> @brief Wait for thread completion
    function win_wait_thread(thread_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result, timeout
        
        timeout = INFINITE
        if (present(timeout_ms)) timeout = timeout_ms
        
        result = WaitForSingleObject(thread_handle, timeout)
        success = (result == WAIT_OBJECT_0)
    end function win_wait_thread

    !> @brief Close thread handle
    subroutine win_close_thread(thread_handle)
        type(c_ptr), intent(in) :: thread_handle
        integer(c_int) :: result
        
        if (c_associated(thread_handle)) then
            result = CloseHandle(thread_handle)
        end if
    end subroutine win_close_thread

    !> @brief Create mutex
    function win_create_mutex() result(mutex_handle)
        type(c_ptr) :: mutex_handle
        
        mutex_handle = CreateMutexA(c_null_ptr, 0, c_null_ptr)
        
        if (.not. c_associated(mutex_handle)) then
            write(error_unit, '(A)') "Failed to create mutex"
        end if
    end function win_create_mutex

    !> @brief Lock mutex
    function win_lock_mutex(mutex_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: mutex_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result, timeout
        
        timeout = INFINITE
        if (present(timeout_ms)) timeout = timeout_ms
        
        result = WaitForSingleObject(mutex_handle, timeout)
        success = (result == WAIT_OBJECT_0)
    end function win_lock_mutex

    !> @brief Unlock mutex
    subroutine win_unlock_mutex(mutex_handle)
        type(c_ptr), intent(in) :: mutex_handle
        integer(c_int) :: result
        
        if (c_associated(mutex_handle)) then
            result = ReleaseMutex(mutex_handle)
        end if
    end subroutine win_unlock_mutex

    !> @brief Destroy mutex
    subroutine win_destroy_mutex(mutex_handle)
        type(c_ptr), intent(in) :: mutex_handle
        integer(c_int) :: result
        
        if (c_associated(mutex_handle)) then
            result = CloseHandle(mutex_handle)
        end if
    end subroutine win_destroy_mutex

    !> @brief Create semaphore
    function win_create_semaphore(initial_count, maximum_count) result(semaphore_handle)
        integer, intent(in) :: initial_count, maximum_count
        type(c_ptr) :: semaphore_handle
        
        semaphore_handle = CreateSemaphoreA(c_null_ptr, initial_count, maximum_count, c_null_ptr)
        
        if (.not. c_associated(semaphore_handle)) then
            write(error_unit, '(A)') "Failed to create semaphore"
        end if
    end function win_create_semaphore

    !> @brief Acquire semaphore
    function win_acquire_semaphore(semaphore_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: semaphore_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result, timeout
        
        timeout = INFINITE
        if (present(timeout_ms)) timeout = timeout_ms
        
        result = WaitForSingleObject(semaphore_handle, timeout)
        success = (result == WAIT_OBJECT_0)
    end function win_acquire_semaphore

    !> @brief Release semaphore
    subroutine win_release_semaphore(semaphore_handle, release_count)
        type(c_ptr), intent(in) :: semaphore_handle
        integer, intent(in), optional :: release_count
        integer(c_int) :: result, count
        
        count = 1
        if (present(release_count)) count = release_count
        
        if (c_associated(semaphore_handle)) then
            result = ReleaseSemaphore(semaphore_handle, count, c_null_ptr)
        end if
    end subroutine win_release_semaphore

    !> @brief Create event (for condition variables)
    function win_create_event(manual_reset, initial_state) result(event_handle)
        logical, intent(in), optional :: manual_reset, initial_state
        type(c_ptr) :: event_handle
        integer(c_int) :: manual, initial
        
        manual = 0
        if (present(manual_reset)) then
            if (manual_reset) manual = 1
        end if
        
        initial = 0
        if (present(initial_state)) then
            if (initial_state) initial = 1
        end if
        
        event_handle = CreateEventA(c_null_ptr, manual, initial, c_null_ptr)
        
        if (.not. c_associated(event_handle)) then
            write(error_unit, '(A)') "Failed to create event"
        end if
    end function win_create_event

    !> @brief Wait on event
    function win_wait_event(event_handle, timeout_ms) result(success)
        type(c_ptr), intent(in) :: event_handle
        integer, intent(in), optional :: timeout_ms
        logical :: success
        integer(c_int) :: result, timeout
        
        timeout = INFINITE
        if (present(timeout_ms)) timeout = timeout_ms
        
        result = WaitForSingleObject(event_handle, timeout)
        success = (result == WAIT_OBJECT_0)
    end function win_wait_event

    !> @brief Set event (signal)
    subroutine win_set_event(event_handle)
        type(c_ptr), intent(in) :: event_handle
        integer(c_int) :: result
        
        if (c_associated(event_handle)) then
            result = SetEvent(event_handle)
        end if
    end subroutine win_set_event

    !> @brief Reset event
    subroutine win_reset_event(event_handle)
        type(c_ptr), intent(in) :: event_handle
        integer(c_int) :: result
        
        if (c_associated(event_handle)) then
            result = ResetEvent(event_handle)
        end if
    end subroutine win_reset_event

end module forge_thread_windows

