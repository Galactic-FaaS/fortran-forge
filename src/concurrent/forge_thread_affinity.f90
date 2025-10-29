!> @brief Thread affinity management
!> @details Cross-platform thread CPU affinity control
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_thread_affinity
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: set_thread_affinity, get_thread_affinity, get_cpu_count

    ! CPU mask type
    type :: cpu_mask_t
        private
        integer(c_intptr_t), dimension(:), allocatable :: mask
    end type cpu_mask_t

contains

    !> @brief Set thread CPU affinity
    subroutine set_thread_affinity(thread_handle, cpu_mask)
#ifdef _WIN32
        ! Windows implementation would use SetThreadAffinityMask
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_set_thread_affinity
#endif
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in) :: cpu_mask

#ifdef _WIN32
        ! Windows SetThreadAffinityMask implementation
        interface
            function SetThreadAffinityMask(hThread, dwThreadAffinityMask) bind(C, name="SetThreadAffinityMask")
                import :: c_ptr, c_intptr_t, c_intptr_t
                type(c_ptr), value :: hThread
                integer(c_intptr_t), value :: dwThreadAffinityMask
                integer(c_intptr_t) :: SetThreadAffinityMask
            end function
        end interface

        if (c_associated(thread_handle)) then
            call SetThreadAffinityMask(thread_handle, int(cpu_mask, c_intptr_t))
        end if
#endif
#ifndef _WIN32
        call posix_set_thread_affinity(thread_handle, cpu_mask)
#endif
    end subroutine set_thread_affinity

    !> @brief Get thread CPU affinity
    function get_thread_affinity(thread_handle) result(cpu_mask)
#ifdef _WIN32
        ! Windows implementation
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_get_thread_affinity
#endif
        type(c_ptr), intent(in) :: thread_handle
        integer :: cpu_mask

#ifdef _WIN32
        ! Windows GetThreadAffinityMask implementation
        interface
            function GetThreadAffinityMask(hThread, lpProcessAffinityMask, lpSystemAffinityMask) &
                    bind(C, name="GetThreadAffinityMask")
                import :: c_ptr, c_intptr_t
                type(c_ptr), value :: hThread, lpProcessAffinityMask, lpSystemAffinityMask
                integer(c_int) :: GetThreadAffinityMask
            end function
        end interface

        cpu_mask = 0
        if (c_associated(thread_handle)) then
            ! Simplified - would need proper mask handling
            cpu_mask = 1
        end if
#endif
#ifndef _WIN32
        cpu_mask = posix_get_thread_affinity(thread_handle)
#endif
    end function get_thread_affinity

    !> @brief Get number of available CPUs
    function get_cpu_count() result(count)
#ifdef _WIN32
        interface
            function GetSystemInfo(lpSystemInfo) bind(C, name="GetSystemInfo")
                import :: c_ptr, c_int
                type(c_ptr), value :: lpSystemInfo
                integer(c_int) :: GetSystemInfo
            end function
        end interface
#endif
#ifndef _WIN32
        ! Use sysconf(_SC_NPROCESSORS_ONLN)
        interface
            function sysconf(name) bind(C, name="sysconf")
                import :: c_int
                integer(c_int), value :: name
                integer(c_int) :: sysconf
            end function
        end interface
        integer(c_int), parameter :: _SC_NPROCESSORS_ONLN = 84
#endif

#ifdef _WIN32
        ! Windows implementation
        count = 1  ! Placeholder
#endif
#ifndef _WIN32
        count = sysconf(_SC_NPROCESSORS_ONLN)
        if (count < 1) count = 1
#endif
    end function get_cpu_count

end module forge_thread_affinity