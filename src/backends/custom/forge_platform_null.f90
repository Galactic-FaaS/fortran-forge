!> @brief Platform-agnostic (null) backend
!> @details For new OS development, embedded systems, testing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_null
    use iso_c_binding
    use iso_fortran_env, only: output_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    implicit none
    private

    public :: forge_null_platform, inject_event, get_framebuffer

    !> @brief Framebuffer structure
    type :: framebuffer
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
        integer(c_int) :: stride = 0
        integer(c_int), pointer :: pixels(:) => null()  ! ARGB32 format
    end type framebuffer

    !> @brief Null platform (no OS dependencies)
    type, extends(forge_platform_base) :: forge_null_platform
        private
        type(framebuffer), allocatable :: windows(:)
        integer :: window_count = 0
        logical :: quit_flag = .false.
    contains
        procedure :: init => null_init
        procedure :: shutdown => null_shutdown
        procedure :: create_window => null_create_window
        procedure :: destroy_window => null_destroy_window
        procedure :: show_window => null_show_window
        procedure :: hide_window => null_hide_window
        procedure :: process_events => null_process_events
        procedure :: get_surface => null_get_surface
    end type forge_null_platform

    ! Global state for event injection
    type(forge_mouse_state), save :: g_injected_mouse_state
    type(forge_keyboard_state), save :: g_injected_keyboard_state
    logical, save :: g_event_pending = .false.

contains

    !> @brief Initialize null platform
    subroutine null_init(this, status)
        class(forge_null_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status
        
        write(output_unit, '(A)') "[NULL] Initializing platform-agnostic backend"
        write(output_unit, '(A)') "[NULL] No OS dependencies - pure framebuffer rendering"
        write(output_unit, '(A)') "[NULL] Use get_framebuffer() to access pixel data"
        write(output_unit, '(A)') "[NULL] Use inject_event() to simulate user input"
        
        allocate(this%windows(10))
        this%initialized = .true.
        call status%clear()
    end subroutine null_init

    !> @brief Shutdown null platform
    subroutine null_shutdown(this)
        class(forge_null_platform), intent(inout) :: this
        integer :: i
        
        write(output_unit, '(A)') "[NULL] Shutting down platform-agnostic backend"
        
        ! Free framebuffers
        if (allocated(this%windows)) then
            do i = 1, this%window_count
                if (associated(this%windows(i)%pixels)) then
                    deallocate(this%windows(i)%pixels)
                end if
            end do
            deallocate(this%windows)
        end if
        
        this%initialized = .false.
    end subroutine null_shutdown

    !> @brief Create a window (framebuffer)
    subroutine null_create_window(this, handle, title, width, height, status)
        class(forge_null_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        
        if (this%window_count >= size(this%windows)) then
            call status%set(FORGE_ERROR_BACKEND, "Maximum windows reached")
            return
        end if
        
        this%window_count = this%window_count + 1
        
        ! Create framebuffer
        this%windows(this%window_count)%width = width
        this%windows(this%window_count)%height = height
        this%windows(this%window_count)%stride = width * 4  ! 4 bytes per pixel (ARGB32)
        
        allocate(this%windows(this%window_count)%pixels(width * height))
        this%windows(this%window_count)%pixels = 0  ! Clear to black
        
        handle%window_id = this%window_count
        handle%width = width
        handle%height = height
        
        write(output_unit, '(A,I0,A,A,A,I0,A,I0,A)') &
            "[NULL] Created window #", this%window_count, &
            ' "', trim(title), '" (', width, 'x', height, ')'
        write(output_unit, '(A,I0,A)') &
            "       Framebuffer allocated: ", width * height * 4, " bytes"
        
        call status%clear()
    end subroutine null_create_window

    !> @brief Destroy window (free framebuffer)
    subroutine null_destroy_window(this, handle)
        class(forge_null_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        
        if (handle%window_id > 0 .and. handle%window_id <= this%window_count) then
            if (associated(this%windows(handle%window_id)%pixels)) then
                deallocate(this%windows(handle%window_id)%pixels)
            end if
            write(output_unit, '(A,I0)') "[NULL] Destroyed window #", handle%window_id
        end if
    end subroutine null_destroy_window

    !> @brief Show window (no-op, but logged)
    subroutine null_show_window(this, handle)
        class(forge_null_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0,A)') "[NULL] Window #", handle%window_id, " visible"
    end subroutine null_show_window

    !> @brief Hide window (no-op, but logged)
    subroutine null_hide_window(this, handle)
        class(forge_null_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0,A)') "[NULL] Window #", handle%window_id, " hidden"
    end subroutine null_hide_window

    !> @brief Process events (check for injected events)
    subroutine null_process_events(this, should_quit)
        class(forge_null_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        
        should_quit = this%quit_flag
        
        if (g_event_pending) then
            write(output_unit, '(A,I0,A,I0)') &
                "[NULL] Event injected: Mouse (", &
                g_injected_mouse_state%x, ", ", g_injected_mouse_state%y, ")"
            g_event_pending = .false.
        end if
    end subroutine null_process_events

    !> @brief Get drawing surface (Cairo image surface)
    subroutine null_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_null_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status
        integer :: window_id
        
        window_id = window%window_id
        
        if (window_id < 1 .or. window_id > this%window_count) then
            call status%set(FORGE_ERROR_INVALID_ARG, "Invalid window ID")
            return
        end if
        
        if (.not. associated(this%windows(window_id)%pixels)) then
            call status%set(FORGE_ERROR_NULL_PTR, "Window framebuffer not allocated")
            return
        end if
        
        ! Create Cairo image surface from framebuffer
        surface%surface = cairo_image_surface_create_for_data( &
            c_loc(this%windows(window_id)%pixels(1)), &
            CAIRO_FORMAT_ARGB32, &
            this%windows(window_id)%width, &
            this%windows(window_id)%height, &
            this%windows(window_id)%stride)
        
        if (.not. c_associated(surface%surface)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo surface")
            return
        end if
        
        surface%context = cairo_create(surface%surface)
        if (.not. c_associated(surface%context)) then
            call cairo_surface_destroy(surface%surface)
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo context")
            return
        end if
        
        call status%clear()
    end subroutine null_get_surface

    !> @brief Inject event (for testing/simulation)
    subroutine inject_event(mouse_x, mouse_y, button_state)
        integer(c_int), intent(in), optional :: mouse_x, mouse_y
        logical, intent(in), optional :: button_state
        
        if (present(mouse_x)) g_injected_mouse_state%x = mouse_x
        if (present(mouse_y)) g_injected_mouse_state%y = mouse_y
        if (present(button_state)) g_injected_mouse_state%left_button = button_state
        
        g_event_pending = .true.
    end subroutine inject_event

    !> @brief Get framebuffer for external rendering/display
    function get_framebuffer(platform, window_id) result(fb)
        type(forge_null_platform), intent(in) :: platform
        integer, intent(in) :: window_id
        type(framebuffer) :: fb
        
        if (window_id >= 1 .and. window_id <= platform%window_count) then
            fb = platform%windows(window_id)
        else
            fb%width = 0
            fb%height = 0
        end if
    end function get_framebuffer

    ! Cairo helper (need to add to bindings)
    interface
        function cairo_image_surface_create_for_data(data, format, width, height, stride) &
                bind(C, name="cairo_image_surface_create_for_data")
            import :: c_ptr, c_int
            type(c_ptr), value :: data
            integer(c_int), value :: format, width, height, stride
            type(c_ptr) :: cairo_image_surface_create_for_data
        end function cairo_image_surface_create_for_data
    end interface

end module forge_platform_null

