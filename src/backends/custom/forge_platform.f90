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
    public :: platform_lifecycle_state, platform_orientation

    !> @brief Platform lifecycle states
    integer, parameter :: LIFECYCLE_CREATED = 0
    integer, parameter :: LIFECYCLE_STARTED = 1
    integer, parameter :: LIFECYCLE_RESUMED = 2
    integer, parameter :: LIFECYCLE_PAUSED = 3
    integer, parameter :: LIFECYCLE_STOPPED = 4
    integer, parameter :: LIFECYCLE_DESTROYED = 5

    !> @brief Device orientation
    integer, parameter :: ORIENTATION_PORTRAIT = 0
    integer, parameter :: ORIENTATION_LANDSCAPE = 1
    integer, parameter :: ORIENTATION_PORTRAIT_UPSIDE_DOWN = 2
    integer, parameter :: ORIENTATION_LANDSCAPE_LEFT = 3
    integer, parameter :: ORIENTATION_LANDSCAPE_RIGHT = 4

    !> @brief Platform lifecycle state
    type :: platform_lifecycle_state
        integer :: current_state = LIFECYCLE_CREATED
        logical :: is_background = .false.
        logical :: is_foreground = .true.
    end type platform_lifecycle_state

    !> @brief Device orientation info
    type :: platform_orientation
        integer :: current_orientation = ORIENTATION_PORTRAIT
        integer :: screen_width = 0
        integer :: screen_height = 0
    end type platform_orientation

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
        type(platform_lifecycle_state) :: lifecycle
        type(platform_orientation) :: orientation
    contains
        procedure(platform_init_interface), deferred :: init
        procedure(platform_shutdown_interface), deferred :: shutdown
        procedure(platform_create_window_interface), deferred :: create_window
        procedure(platform_destroy_window_interface), deferred :: destroy_window
        procedure(platform_show_window_interface), deferred :: show_window
        procedure(platform_hide_window_interface), deferred :: hide_window
        procedure(platform_process_events_interface), deferred :: process_events
        procedure(platform_get_surface_interface), deferred :: get_surface
        procedure(platform_get_lifecycle_interface), deferred :: get_lifecycle
        procedure(platform_get_orientation_interface), deferred :: get_orientation
        procedure(platform_set_orientation_interface), deferred :: set_orientation
        procedure(platform_handle_back_button_interface), deferred :: handle_back_button
        procedure(platform_show_status_bar_interface), deferred :: show_status_bar
        procedure(platform_hide_status_bar_interface), deferred :: hide_status_bar
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

        subroutine platform_get_lifecycle_interface(this, lifecycle)
            import :: forge_platform_base, platform_lifecycle_state
            class(forge_platform_base), intent(inout) :: this
            type(platform_lifecycle_state), intent(out) :: lifecycle
        end subroutine platform_get_lifecycle_interface

        subroutine platform_get_orientation_interface(this, orientation)
            import :: forge_platform_base, platform_orientation
            class(forge_platform_base), intent(inout) :: this
            type(platform_orientation), intent(out) :: orientation
        end subroutine platform_get_orientation_interface

        subroutine platform_set_orientation_interface(this, orientation, status)
            import :: forge_platform_base, platform_orientation, forge_status
            class(forge_platform_base), intent(inout) :: this
            type(platform_orientation), intent(in) :: orientation
            type(forge_status), intent(out) :: status
        end subroutine platform_set_orientation_interface

        subroutine platform_handle_back_button_interface(this, handled)
            import :: forge_platform_base, c_bool
            class(forge_platform_base), intent(inout) :: this
            logical(c_bool), intent(out) :: handled
        end subroutine platform_handle_back_button_interface

        subroutine platform_show_status_bar_interface(this, status)
            import :: forge_platform_base, forge_status
            class(forge_platform_base), intent(inout) :: this
            type(forge_status), intent(out) :: status
        end subroutine platform_show_status_bar_interface

        subroutine platform_hide_status_bar_interface(this, status)
            import :: forge_platform_base, forge_status
            class(forge_platform_base), intent(inout) :: this
            type(forge_status), intent(out) :: status
        end subroutine platform_hide_status_bar_interface

    end interface

end module forge_platform

