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
        use forge_thread_windows, only: win_set_thread_affinity_mask
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_set_thread_affinity
#endif
        type(c_ptr), intent(in) :: thread_handle
        integer, intent(in) :: cpu_mask

#ifdef _WIN32
        ! Use real Windows SetThreadAffinityMask implementation
        if (c_associated(thread_handle)) then
            call win_set_thread_affinity_mask(thread_handle, int(cpu_mask, c_intptr_t))
        end if
#endif
#ifndef _WIN32
        call posix_set_thread_affinity(thread_handle, cpu_mask)
#endif
    end subroutine set_thread_affinity

    !> @brief Get thread CPU affinity
    function get_thread_affinity(thread_handle) result(cpu_mask)
#ifdef _WIN32
        ! Windows implementation - simplified for now
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_get_thread_affinity
#endif
        type(c_ptr), intent(in) :: thread_handle
        integer :: cpu_mask

#ifdef _WIN32
        ! Windows GetThreadAffinityMask implementation
        ! For now, return current processor number as a simple affinity mask
        use forge_thread_windows, only: win_get_current_processor_number
        cpu_mask = win_get_current_processor_number()
#endif
#ifndef _WIN32
        cpu_mask = posix_get_thread_affinity(thread_handle)
#endif
    end function get_thread_affinity

    !> @brief Get number of available CPUs
    function get_cpu_count() result(count)
#ifdef _WIN32
        ! Windows implementation using GetSystemInfo
        interface
            function GetSystemInfo(lpSystemInfo) bind(C, name="GetSystemInfo")
                import :: c_ptr, c_int
                type(c_ptr), value :: lpSystemInfo
                integer(c_int) :: GetSystemInfo
            end function
        end interface
        ! SYSTEM_INFO structure - simplified to get dwNumberOfProcessors
        type :: SYSTEM_INFO
            integer(c_int) :: dwNumberOfProcessors
        end type SYSTEM_INFO
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
        type(SYSTEM_INFO) :: sys_info
        call GetSystemInfo(c_loc(sys_info))
        count = sys_info%dwNumberOfProcessors
#endif
#ifndef _WIN32
        count = sysconf(_SC_NPROCESSORS_ONLN)
        if (count < 1) count = 1
#endif
    end function get_cpu_count

end module forge_thread_affinity