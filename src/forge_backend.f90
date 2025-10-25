!> @brief Backend abstraction layer for ForGE
!> @details Defines abstract interfaces for GUI backends
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_backend
    use iso_c_binding
    use forge_types
    use forge_errors
    implicit none
    private

    public :: forge_backend_base, forge_widget_handle, forge_window_handle
    public :: forge_backend_type, BACKEND_NONE, BACKEND_TCL_TK, BACKEND_GTK4
    public :: BACKEND_QT, BACKEND_CUSTOM

    !> Backend type identifiers
    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_TCL_TK = 1
    integer, parameter :: BACKEND_GTK4 = 2
    integer, parameter :: BACKEND_QT = 3
    integer, parameter :: BACKEND_CUSTOM = 4

    !> @brief Backend type selector
    type :: forge_backend_type
        integer :: id = BACKEND_NONE
    end type forge_backend_type

    !> @brief Opaque widget handle
    type :: forge_widget_handle
        type(c_ptr) :: ptr = c_null_ptr
        integer :: widget_id = 0
    end type forge_widget_handle

    !> @brief Opaque window handle
    type :: forge_window_handle
        type(c_ptr) :: ptr = c_null_ptr
        integer :: window_id = 0
    end type forge_window_handle

    !> @brief Abstract base class for GUI backends
    type, abstract :: forge_backend_base
        logical :: initialized = .false.
        type(forge_backend_type) :: backend_type
        type(forge_error) :: error
    contains
        !> Initialize the backend
        procedure(backend_init_interface), deferred :: init
        
        !> Shutdown the backend
        procedure(backend_shutdown_interface), deferred :: shutdown
        
        !> Run the main event loop
        procedure(backend_run_interface), deferred :: run
        
        !> Process pending events
        procedure(backend_process_events_interface), deferred :: process_events
        
        !> Create a window
        procedure(backend_create_window_interface), deferred :: create_window
        
        !> Destroy a window
        procedure(backend_destroy_window_interface), deferred :: destroy_window
        
        !> Show a window
        procedure(backend_show_window_interface), deferred :: show_window
        
        !> Hide a window
        procedure(backend_hide_window_interface), deferred :: hide_window
        
        !> Create a widget
        procedure(backend_create_widget_interface), deferred :: create_widget
        
        !> Destroy a widget
        procedure(backend_destroy_widget_interface), deferred :: destroy_widget
        
        !> Get backend name
        procedure(backend_get_name_interface), deferred :: get_name
    end type forge_backend_base

    !> Abstract interface for backend initialization
    abstract interface
        subroutine backend_init_interface(this, status)
            import :: forge_backend_base, forge_status
            class(forge_backend_base), intent(inout) :: this
            type(forge_status), intent(out) :: status
        end subroutine backend_init_interface

        subroutine backend_shutdown_interface(this)
            import :: forge_backend_base
            class(forge_backend_base), intent(inout) :: this
        end subroutine backend_shutdown_interface

        subroutine backend_run_interface(this)
            import :: forge_backend_base
            class(forge_backend_base), intent(inout) :: this
        end subroutine backend_run_interface

        subroutine backend_process_events_interface(this)
            import :: forge_backend_base
            class(forge_backend_base), intent(inout) :: this
        end subroutine backend_process_events_interface

        subroutine backend_create_window_interface(this, handle, title, width, height, status)
            import :: forge_backend_base, forge_window_handle, forge_status, c_int
            class(forge_backend_base), intent(inout) :: this
            type(forge_window_handle), intent(out) :: handle
            character(len=*), intent(in) :: title
            integer(c_int), intent(in) :: width, height
            type(forge_status), intent(out) :: status
        end subroutine backend_create_window_interface

        subroutine backend_destroy_window_interface(this, handle)
            import :: forge_backend_base, forge_window_handle
            class(forge_backend_base), intent(inout) :: this
            type(forge_window_handle), intent(in) :: handle
        end subroutine backend_destroy_window_interface

        subroutine backend_show_window_interface(this, handle)
            import :: forge_backend_base, forge_window_handle
            class(forge_backend_base), intent(inout) :: this
            type(forge_window_handle), intent(in) :: handle
        end subroutine backend_show_window_interface

        subroutine backend_hide_window_interface(this, handle)
            import :: forge_backend_base, forge_window_handle
            class(forge_backend_base), intent(inout) :: this
            type(forge_window_handle), intent(in) :: handle
        end subroutine backend_hide_window_interface

        subroutine backend_create_widget_interface(this, handle, widget_type, parent, status)
            import :: forge_backend_base, forge_widget_handle, forge_window_handle, forge_status
            class(forge_backend_base), intent(inout) :: this
            type(forge_widget_handle), intent(out) :: handle
            character(len=*), intent(in) :: widget_type
            type(forge_window_handle), intent(in) :: parent
            type(forge_status), intent(out) :: status
        end subroutine backend_create_widget_interface

        subroutine backend_destroy_widget_interface(this, handle)
            import :: forge_backend_base, forge_widget_handle
            class(forge_backend_base), intent(inout) :: this
            type(forge_widget_handle), intent(in) :: handle
        end subroutine backend_destroy_widget_interface

        function backend_get_name_interface(this) result(name)
            import :: forge_backend_base
            class(forge_backend_base), intent(in) :: this
            character(len=:), allocatable :: name
        end function backend_get_name_interface
    end interface

end module forge_backend

