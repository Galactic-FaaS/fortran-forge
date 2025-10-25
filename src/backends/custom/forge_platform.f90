!> @brief Platform abstraction interface for custom GUI backend
!> @details Defines abstract interface that each platform must implement
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform
    use iso_c_binding
    use forge_types
    use forge_errors
    implicit none
    private

    public :: forge_platform_base, platform_window_handle, platform_surface_handle

    !> @brief Opaque platform window handle
    type :: platform_window_handle
        type(c_ptr) :: native_handle = c_null_ptr
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
    end type platform_window_handle

    !> @brief Opaque platform drawing surface handle
    type :: platform_surface_handle
        type(c_ptr) :: surface = c_null_ptr
        type(c_ptr) :: context = c_null_ptr
    end type platform_surface_handle

    !> @brief Abstract platform interface
    type, abstract :: forge_platform_base
        logical :: initialized = .false.
    contains
        procedure(platform_init_interface), deferred :: init
        procedure(platform_shutdown_interface), deferred :: shutdown
        procedure(platform_create_window_interface), deferred :: create_window
        procedure(platform_destroy_window_interface), deferred :: destroy_window
        procedure(platform_show_window_interface), deferred :: show_window
        procedure(platform_hide_window_interface), deferred :: hide_window
        procedure(platform_process_events_interface), deferred :: process_events
        procedure(platform_get_surface_interface), deferred :: get_surface
    end type forge_platform_base

    !> Abstract interfaces for platform operations
    abstract interface
        
        subroutine platform_init_interface(this, status)
            import :: forge_platform_base, forge_status
            class(forge_platform_base), intent(inout) :: this
            type(forge_status), intent(out) :: status
        end subroutine platform_init_interface

        subroutine platform_shutdown_interface(this)
            import :: forge_platform_base
            class(forge_platform_base), intent(inout) :: this
        end subroutine platform_shutdown_interface

        subroutine platform_create_window_interface(this, handle, title, width, height, status)
            import :: forge_platform_base, platform_window_handle, forge_status, c_int
            class(forge_platform_base), intent(inout) :: this
            type(platform_window_handle), intent(out) :: handle
            character(len=*), intent(in) :: title
            integer(c_int), intent(in) :: width, height
            type(forge_status), intent(out) :: status
        end subroutine platform_create_window_interface

        subroutine platform_destroy_window_interface(this, handle)
            import :: forge_platform_base, platform_window_handle
            class(forge_platform_base), intent(inout) :: this
            type(platform_window_handle), intent(in) :: handle
        end subroutine platform_destroy_window_interface

        subroutine platform_show_window_interface(this, handle)
            import :: forge_platform_base, platform_window_handle
            class(forge_platform_base), intent(inout) :: this
            type(platform_window_handle), intent(in) :: handle
        end subroutine platform_show_window_interface

        subroutine platform_hide_window_interface(this, handle)
            import :: forge_platform_base, platform_window_handle
            class(forge_platform_base), intent(inout) :: this
            type(platform_window_handle), intent(in) :: handle
        end subroutine platform_hide_window_interface

        subroutine platform_process_events_interface(this, should_quit)
            import :: forge_platform_base, c_bool
            class(forge_platform_base), intent(inout) :: this
            logical(c_bool), intent(out) :: should_quit
        end subroutine platform_process_events_interface

        subroutine platform_get_surface_interface(this, window, surface, status)
            import :: forge_platform_base, platform_window_handle, platform_surface_handle, forge_status
            class(forge_platform_base), intent(inout) :: this
            type(platform_window_handle), intent(in) :: window
            type(platform_surface_handle), intent(out) :: surface
            type(forge_status), intent(out) :: status
        end subroutine platform_get_surface_interface

    end interface

end module forge_platform

