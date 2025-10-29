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
    public :: win_set_thread_priority
    public :: win_get_last_error, win_get_current_thread_id
    public :: win_tls_alloc, win_tls_get_value, win_tls_set_value, win_tls_free
    public :: win_initialize_critical_section, win_enter_critical_section
    public :: win_leave_critical_section, win_delete_critical_section
    public :: win_set_thread_affinity_mask, win_get_current_processor_number

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

        function SetThreadPriority(hThread, nPriority) bind(C, name="SetThreadPriority")
            import :: c_ptr, c_int
            type(c_ptr), value :: hThread
            integer(c_int), value :: nPriority
            integer(c_int) :: SetThreadPriority
        end function SetThreadPriority

        ! Error handling
        function GetLastError() bind(C, name="GetLastError")
            import :: c_int
            integer(c_int) :: GetLastError
        end function GetLastError

        ! Thread ID
        function GetCurrentThreadId() bind(C, name="GetCurrentThreadId")
            import :: c_int
            integer(c_int) :: GetCurrentThreadId
        end function GetCurrentThreadId

        ! Thread-local storage
        function TlsAlloc() bind(C, name="TlsAlloc")
            import :: c_int
            integer(c_int) :: TlsAlloc
        end function TlsAlloc

        function TlsGetValue(dwTlsIndex) bind(C, name="TlsGetValue")
            import :: c_int, c_ptr
            integer(c_int), value :: dwTlsIndex
            type(c_ptr) :: TlsGetValue
        end function TlsGetValue

        function TlsSetValue(dwTlsIndex, lpTlsValue) bind(C, name="TlsSetValue")
            import :: c_int, c_ptr
            integer(c_int), value :: dwTlsIndex
            type(c_ptr), value :: lpTlsValue
            integer(c_int) :: TlsSetValue
        end function TlsSetValue

        function TlsFree(dwTlsIndex) bind(C, name="TlsFree")
            import :: c_int
            integer(c_int), value :: dwTlsIndex
            integer(c_int) :: TlsFree
        end function TlsFree

        ! Critical sections
        subroutine InitializeCriticalSection(lpCriticalSection) bind(C, name="InitializeCriticalSection")
            import :: c_ptr
            type(c_ptr), value :: lpCriticalSection
        end subroutine InitializeCriticalSection

        subroutine EnterCriticalSection(lpCriticalSection) bind(C, name="EnterCriticalSection")
            import :: c_ptr
            type(c_ptr), value :: lpCriticalSection
        end subroutine EnterCriticalSection

        subroutine LeaveCriticalSection(lpCriticalSection) bind(C, name="LeaveCriticalSection")
            import :: c_ptr
            type(c_ptr), value :: lpCriticalSection
        end subroutine LeaveCriticalSection

        subroutine DeleteCriticalSection(lpCriticalSection) bind(C, name="DeleteCriticalSection")
            import :: c_ptr
            type(c_ptr), value :: lpCriticalSection
        end subroutine DeleteCriticalSection

        ! Thread affinity
        function SetThreadAffinityMask(hThread, dwThreadAffinityMask) bind(C, name="SetThreadAffinityMask")
            import :: c_ptr, c_intptr_t
            type(c_ptr), value :: hThread
            integer(c_intptr_t), value :: dwThreadAffinityMask
            integer(c_intptr_t) :: SetThreadAffinityMask
        end function SetThreadAffinityMask

        function GetCurrentProcessorNumber() bind(C, name="GetCurrentProcessorNumber")
            import :: c_int
            integer(c_int) :: GetCurrentProcessorNumber
        end function GetCurrentProcessorNumber

    end interface

    ! Thread priority constants
    integer(c_int), parameter :: THREAD_PRIORITY_LOWEST = -2
    integer(c_int), parameter :: THREAD_PRIORITY_BELOW_NORMAL = -1
    integer(c_int), parameter :: THREAD_PRIORITY_NORMAL = 0
    integer(c_int), parameter :: THREAD_PRIORITY_ABOVE_NORMAL = 1
    integer(c_int), parameter :: THREAD_PRIORITY_HIGHEST = 2
    integer(c_int), parameter :: THREAD_PRIORITY_TIME_CRITICAL = 15

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
            write(error_unit, '(A, I0)') "Failed to create thread, error code: ", win_get_last_error()
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
            write(error_unit, '(A, I0)') "Failed to create mutex, error code: ", win_get_last_error()
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
            write(error_unit, '(A, I0)') "Failed to create semaphore, error code: ", win_get_last_error()
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
            write(error_unit, '(A, I0)') "Failed to create event, error code: ", win_get_last_error()
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

    !> @brief Set thread priority
    subroutine win_set_thread_priority(thread_handle, priority)
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in) :: priority
        integer(c_int) :: win_priority, result

        ! Map our priority levels to Windows constants
        select case (priority)
            case (-2)
                win_priority = THREAD_PRIORITY_LOWEST
            case (-1)
                win_priority = THREAD_PRIORITY_BELOW_NORMAL
            case (0)
                win_priority = THREAD_PRIORITY_NORMAL
            case (1)
                win_priority = THREAD_PRIORITY_ABOVE_NORMAL
            case (2)
                win_priority = THREAD_PRIORITY_HIGHEST
            case (15)
                win_priority = THREAD_PRIORITY_TIME_CRITICAL
            case default
                win_priority = THREAD_PRIORITY_NORMAL
        end select

        if (c_associated(thread_handle)) then
            result = SetThreadPriority(thread_handle, win_priority)
        end if
    end subroutine win_set_thread_priority

    !> @brief Get last error code
    function win_get_last_error() result(error_code)
        integer(c_int) :: error_code
        error_code = GetLastError()
    end function win_get_last_error

    !> @brief Get current thread ID
    function win_get_current_thread_id() result(thread_id)
        integer(c_int) :: thread_id
        thread_id = GetCurrentThreadId()
    end function win_get_current_thread_id

    !> @brief Allocate thread-local storage index
    function win_tls_alloc() result(tls_index)
        integer(c_int) :: tls_index
        tls_index = TlsAlloc()
        if (tls_index == -1) then
            write(error_unit, '(A)') "Failed to allocate TLS index"
        end if
    end function win_tls_alloc

    !> @brief Get thread-local storage value
    function win_tls_get_value(tls_index) result(value)
        integer(c_int), intent(in) :: tls_index
        type(c_ptr) :: value
        value = TlsGetValue(tls_index)
    end function win_tls_get_value

    !> @brief Set thread-local storage value
    function win_tls_set_value(tls_index, value) result(success)
        integer(c_int), intent(in) :: tls_index
        type(c_ptr), intent(in) :: value
        logical :: success
        integer(c_int) :: result_code
        result_code = TlsSetValue(tls_index, value)
        success = (result_code /= 0)
        if (.not. success) then
            write(error_unit, '(A)') "Failed to set TLS value"
        end if
    end function win_tls_set_value

    !> @brief Free thread-local storage index
    function win_tls_free(tls_index) result(success)
        integer(c_int), intent(in) :: tls_index
        logical :: success
        integer(c_int) :: result_code
        result_code = TlsFree(tls_index)
        success = (result_code /= 0)
        if (.not. success) then
            write(error_unit, '(A)') "Failed to free TLS index"
        end if
    end function win_tls_free

    !> @brief Initialize critical section
    subroutine win_initialize_critical_section(lpCriticalSection)
        type(c_ptr), intent(in) :: lpCriticalSection
        call InitializeCriticalSection(lpCriticalSection)
    end subroutine win_initialize_critical_section

    !> @brief Enter critical section
    subroutine win_enter_critical_section(lpCriticalSection)
        type(c_ptr), intent(in) :: lpCriticalSection
        call EnterCriticalSection(lpCriticalSection)
    end subroutine win_enter_critical_section

    !> @brief Leave critical section
    subroutine win_leave_critical_section(lpCriticalSection)
        type(c_ptr), intent(in) :: lpCriticalSection
        call LeaveCriticalSection(lpCriticalSection)
    end subroutine win_leave_critical_section

    !> @brief Delete critical section
    subroutine win_delete_critical_section(lpCriticalSection)
        type(c_ptr), intent(in) :: lpCriticalSection
        call DeleteCriticalSection(lpCriticalSection)
    end subroutine win_delete_critical_section

    !> @brief Set thread affinity mask
    function win_set_thread_affinity_mask(hThread, dwThreadAffinityMask) result(old_mask)
        type(c_ptr), intent(in) :: hThread
        integer(c_intptr_t), intent(in) :: dwThreadAffinityMask
        integer(c_intptr_t) :: old_mask
        old_mask = SetThreadAffinityMask(hThread, dwThreadAffinityMask)
        if (old_mask == 0) then
            write(error_unit, '(A)') "Failed to set thread affinity mask"
        end if
    end function win_set_thread_affinity_mask

    !> @brief Get current processor number
    function win_get_current_processor_number() result(processor_number)
        integer(c_int) :: processor_number
        processor_number = GetCurrentProcessorNumber()
    end function win_get_current_processor_number

end module forge_thread_windows

