!> @brief Main ForGE module - Modern Fortran GUI Environment
!> @details Entry point module that re-exports all public interfaces
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later
!>
!> ForGE (Fortran GUI Environment) is a modern, cross-platform GUI library
!> for Fortran applications. It provides an object-oriented API with support
!> for multiple GUI backends.
!>
!> @par Quick Start:
!> @code{.f90}
!> use forge
!> type(forge_application) :: app
!> type(forge_window_t) :: window
!> 
!> call app%init(BACKEND_TCL_TK)
!> window = app%create_window("Hello ForGE", 800, 600)
!> call window%show()
!> call app%run()
!> @endcode

module forge
    ! Re-export all public interfaces
    use forge_types
    use forge_errors
    use forge_backend
    use forge_events
    use forge_window
    use forge_widgets
    use forge_layout
    use forge_custom_backend
    
    implicit none
    private

    ! Re-export public symbols
    public :: forge_application
    
    ! From forge_types
    public :: forge_string, forge_color, forge_size, forge_position, forge_rect
    public :: c_int, c_bool, c_double
    
    ! From forge_errors
    public :: forge_error, forge_status, forge_check_status
    public :: FORGE_SUCCESS, FORGE_ERROR_GENERIC, FORGE_ERROR_NULL_PTR
    public :: FORGE_ERROR_INVALID_ARG, FORGE_ERROR_BACKEND, FORGE_ERROR_NOT_IMPLEMENTED
    
    ! From forge_backend
    public :: forge_backend_base, forge_backend_type
    public :: BACKEND_NONE, BACKEND_TCL_TK, BACKEND_GTK4, BACKEND_QT, BACKEND_CUSTOM
    
    ! From forge_events
    public :: forge_event, forge_event_handler, forge_event_type, event_callback_interface
    public :: EVENT_BUTTON_CLICKED, EVENT_WINDOW_CLOSED, EVENT_KEY_PRESSED
    public :: EVENT_MOUSE_MOVED, EVENT_TEXT_CHANGED, EVENT_VALUE_CHANGED
    
    ! From forge_window
    public :: forge_window_t, forge_window_builder
    
    ! From forge_widgets
    public :: forge_widget, forge_button, forge_label, forge_entry
    public :: forge_text_view, forge_progress_bar, forge_separator
    public :: WIDGET_BUTTON, WIDGET_LABEL, WIDGET_ENTRY, WIDGET_TEXT_VIEW
    
    ! From forge_layout
    public :: forge_layout_base, forge_grid_layout, forge_box_layout, forge_stack_layout
    public :: LAYOUT_HORIZONTAL, LAYOUT_VERTICAL

    !> ForGE version
    character(len=*), parameter, public :: FORGE_VERSION = "1.0.0"
    character(len=*), parameter, public :: FORGE_VERSION_MAJOR = "1"
    character(len=*), parameter, public :: FORGE_VERSION_MINOR = "0"
    character(len=*), parameter, public :: FORGE_VERSION_PATCH = "0"

    !> @brief Main application class
    !> @details Manages application lifecycle, backend initialization, and event loop
    type :: forge_application
        private
        class(forge_backend_base), allocatable :: backend
        logical :: initialized = .false.
        type(forge_backend_type) :: backend_type
        type(forge_error) :: error
    contains
        procedure :: init => forge_application_init
        procedure :: shutdown => forge_application_shutdown
        procedure :: run => forge_application_run
        procedure :: process_events => forge_application_process_events
        procedure :: is_initialized => forge_application_is_initialized
        procedure :: get_backend_type => forge_application_get_backend_type
        procedure :: create_window => forge_application_create_window
    end type forge_application

contains

    !> @brief Initialize the application with specified backend
    !> @param backend_id Backend type identifier (BACKEND_TCL_TK, etc.)
    !> @param status Optional status output
    subroutine forge_application_init(this, backend_id, status)
        class(forge_application), intent(inout) :: this
        integer, intent(in) :: backend_id
        type(forge_status), intent(out), optional :: status
        type(forge_status) :: local_status
        
        if (this%initialized) then
            call local_status%set(FORGE_ERROR_GENERIC, "Application already initialized")
            if (present(status)) status = local_status
            return
        end if
        
        this%backend_type%id = backend_id
        
        ! Allocate appropriate backend
        select case (backend_id)
        case (BACKEND_TCL_TK)
            ! Tcl/Tk backend not yet implemented - could be added in future
            call local_status%set(FORGE_ERROR_NOT_IMPLEMENTED, "Tcl/Tk backend not yet implemented")

        case (BACKEND_GTK4)
            ! GTK4 backend not yet implemented - could be added in future
            call local_status%set(FORGE_ERROR_NOT_IMPLEMENTED, "GTK4 backend not yet implemented")

        case (BACKEND_QT)
            ! Qt backend not yet implemented - could be added in future
            call local_status%set(FORGE_ERROR_NOT_IMPLEMENTED, "Qt backend not yet implemented")

        case (BACKEND_CUSTOM)
            ! Use custom GUI framework backend
            allocate(forge_custom_backend_t :: this%backend)

        case default
            call local_status%set(FORGE_ERROR_INVALID_ARG, "Unknown backend type")
        end select
        
        ! Initialize backend if allocated
        if (allocated(this%backend) .and. local_status%is_ok()) then
            call this%backend%init(local_status)
            if (local_status%is_ok()) then
                this%initialized = .true.
            end if
        end if
        
        if (present(status)) status = local_status
    end subroutine forge_application_init

    !> @brief Shutdown the application and cleanup resources
    subroutine forge_application_shutdown(this)
        class(forge_application), intent(inout) :: this
        
        if (this%initialized .and. allocated(this%backend)) then
            call this%backend%shutdown()
        end if
        
        if (allocated(this%backend)) deallocate(this%backend)
        this%initialized = .false.
    end subroutine forge_application_shutdown

    !> @brief Run the main event loop (blocks until app closes)
    subroutine forge_application_run(this)
        class(forge_application), intent(inout) :: this
        
        if (this%initialized .and. allocated(this%backend)) then
            call this%backend%run()
        end if
    end subroutine forge_application_run

    !> @brief Process pending events (non-blocking)
    subroutine forge_application_process_events(this)
        class(forge_application), intent(inout) :: this
        
        if (this%initialized .and. allocated(this%backend)) then
            call this%backend%process_events()
        end if
    end subroutine forge_application_process_events

    !> @brief Check if application is initialized
    function forge_application_is_initialized(this) result(initialized)
        class(forge_application), intent(in) :: this
        logical :: initialized
        
        initialized = this%initialized
    end function forge_application_is_initialized

    !> @brief Get current backend type
    function forge_application_get_backend_type(this) result(backend_type)
        class(forge_application), intent(in) :: this
        type(forge_backend_type) :: backend_type
        
        backend_type = this%backend_type
    end function forge_application_get_backend_type

    !> @brief Create a window using the application's backend
    !> @param title Window title
    !> @param width Window width in pixels
    !> @param height Window height in pixels
    !> @return Newly created window
    function forge_application_create_window(this, title, width, height) result(window)
        class(forge_application), intent(inout) :: this
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_window_t) :: window
        type(forge_window_builder) :: builder
        type(forge_status) :: status
        
        if (.not. this%initialized) then
            call this%error%raise(FORGE_ERROR_GENERIC, "Application not initialized")
            return
        end if
        
        if (allocated(this%backend)) then
            call builder%set_title(title)
            call builder%set_size(width, height)
            call builder%set_backend(this%backend)
            window = builder%build(status)
            
            if (status%is_error()) then
                call status%print()
            end if
        end if
    end function forge_application_create_window

end module forge

