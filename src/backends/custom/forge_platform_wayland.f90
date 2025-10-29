!> @brief Wayland platform implementation using Wayland client library
!> @details Provides windowing and event handling for Linux Wayland
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_wayland
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    use forge_events
    implicit none
    private

    public :: forge_wayland_platform
    public :: set_window_render_callback

    ! Wayland protocol constants
    integer(c_int), parameter :: WL_SHM_FORMAT_ARGB8888 = 0
    integer(c_int), parameter :: WL_SHM_FORMAT_XRGB8888 = 1

    ! Wayland interface versions
    integer(c_int), parameter :: WL_COMPOSITOR_VERSION = 4
    integer(c_int), parameter :: WL_SHM_VERSION = 1
    integer(c_int), parameter :: WL_SEAT_VERSION = 7
    integer(c_int), parameter :: WL_POINTER_VERSION = 7
    integer(c_int), parameter :: WL_KEYBOARD_VERSION = 7
    integer(c_int), parameter :: XDG_WM_BASE_VERSION = 2
    integer(c_int), parameter :: XDG_SURFACE_VERSION = 2
    integer(c_int), parameter :: XDG_TOPLEVEL_VERSION = 2

    ! Wayland event types
    integer(c_int), parameter :: WL_POINTER_BUTTON_STATE_PRESSED = 1
    integer(c_int), parameter :: WL_POINTER_BUTTON_STATE_RELEASED = 0
    integer(c_int), parameter :: WL_KEYBOARD_KEY_STATE_PRESSED = 1
    integer(c_int), parameter :: WL_KEYBOARD_KEY_STATE_RELEASED = 0

    ! Wayland button codes
    integer(c_int), parameter :: BTN_LEFT = 272
    integer(c_int), parameter :: BTN_MIDDLE = 273
    integer(c_int), parameter :: BTN_RIGHT = 274

    ! Wayland structures (simplified for Fortran binding)
    type :: wl_display
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_display

    type :: wl_registry
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_registry

    type :: wl_compositor
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_compositor

    type :: wl_surface
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_surface

    type :: wl_shm
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_shm

    type :: wl_buffer
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_buffer

    type :: wl_seat
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_seat

    type :: wl_pointer
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_pointer

    type :: wl_keyboard
        type(c_ptr) :: ptr = c_null_ptr
    end type wl_keyboard

    type :: xdg_wm_base
        type(c_ptr) :: ptr = c_null_ptr
    end type xdg_wm_base

    type :: xdg_surface
        type(c_ptr) :: ptr = c_null_ptr
    end type xdg_surface

    type :: xdg_toplevel
        type(c_ptr) :: ptr = c_null_ptr
    end type xdg_toplevel

    ! Wayland API bindings
    interface

        ! Display management
        function wl_display_connect(display_name) bind(C, name="wl_display_connect")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: display_name(*)
            type(c_ptr) :: wl_display_connect
        end function wl_display_connect

        function wl_display_disconnect(display) bind(C, name="wl_display_disconnect")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: wl_display_disconnect
        end function wl_display_disconnect

        function wl_display_roundtrip(display) bind(C, name="wl_display_roundtrip")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: wl_display_roundtrip
        end function wl_display_roundtrip

        function wl_display_dispatch(display) bind(C, name="wl_display_dispatch")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: wl_display_dispatch
        end function wl_display_dispatch

        function wl_display_get_registry(display) bind(C, name="wl_display_get_registry")
            import :: c_ptr
            type(c_ptr), value :: display
            type(c_ptr) :: wl_display_get_registry
        end function wl_display_get_registry

        ! Registry
        subroutine wl_registry_add_listener(registry, listener, data) &
                bind(C, name="wl_registry_add_listener")
            import :: c_ptr
            type(c_ptr), value :: registry, listener, data
        end subroutine wl_registry_add_listener

        ! Compositor
        function wl_compositor_create_surface(compositor) &
                bind(C, name="wl_compositor_create_surface")
            import :: c_ptr
            type(c_ptr), value :: compositor
            type(c_ptr) :: wl_compositor_create_surface
        end function wl_compositor_create_surface

        ! Surface
        subroutine wl_surface_commit(surface) bind(C, name="wl_surface_commit")
            import :: c_ptr
            type(c_ptr), value :: surface
        end subroutine wl_surface_commit

        subroutine wl_surface_destroy(surface) bind(C, name="wl_surface_destroy")
            import :: c_ptr
            type(c_ptr), value :: surface
        end subroutine wl_surface_destroy

        ! SHM
        function wl_shm_create_pool(shm, fd, size) bind(C, name="wl_shm_create_pool")
            import :: c_ptr, c_int
            type(c_ptr), value :: shm
            integer(c_int), value :: fd, size
            type(c_ptr) :: wl_shm_create_pool
        end function wl_shm_create_pool

        function wl_shm_pool_create_buffer(pool, offset, width, height, stride, format) &
                bind(C, name="wl_shm_pool_create_buffer")
            import :: c_ptr, c_int
            type(c_ptr), value :: pool
            integer(c_int), value :: offset, width, height, stride, format
            type(c_ptr) :: wl_shm_pool_create_buffer
        end function wl_shm_pool_create_buffer

        ! Buffer
        subroutine wl_buffer_destroy(buffer) bind(C, name="wl_buffer_destroy")
            import :: c_ptr
            type(c_ptr), value :: buffer
        end subroutine wl_buffer_destroy

        ! XDG shell
        function xdg_wm_base_get_xdg_surface(wm_base, surface) &
                bind(C, name="xdg_wm_base_get_xdg_surface")
            import :: c_ptr
            type(c_ptr), value :: wm_base, surface
            type(c_ptr) :: xdg_wm_base_get_xdg_surface
        end function xdg_wm_base_get_xdg_surface

        function xdg_surface_get_toplevel(xdg_surface) &
                bind(C, name="xdg_surface_get_toplevel")
            import :: c_ptr
            type(c_ptr), value :: xdg_surface
            type(c_ptr) :: xdg_surface_get_toplevel
        end function xdg_surface_get_toplevel

        subroutine xdg_toplevel_set_title(toplevel, title) &
                bind(C, name="xdg_toplevel_set_title")
            import :: c_ptr, c_char
            type(c_ptr), value :: toplevel
            character(kind=c_char), intent(in) :: title(*)
        end subroutine xdg_toplevel_set_title

        subroutine xdg_surface_ack_configure(xdg_surface, serial) &
                bind(C, name="xdg_surface_ack_configure")
            import :: c_ptr, c_int
            type(c_ptr), value :: xdg_surface
            integer(c_int), value :: serial
        end subroutine xdg_surface_ack_configure

        ! Seat and input
        subroutine wl_seat_add_listener(seat, listener, data) &
                bind(C, name="wl_seat_add_listener")
            import :: c_ptr
            type(c_ptr), value :: seat, listener, data
        end subroutine wl_seat_add_listener

        subroutine wl_pointer_add_listener(pointer, listener, data) &
                bind(C, name="wl_pointer_add_listener")
            import :: c_ptr
            type(c_ptr), value :: pointer, listener, data
        end subroutine wl_pointer_add_listener

        subroutine wl_keyboard_add_listener(keyboard, listener, data) &
                bind(C, name="wl_keyboard_add_listener")
            import :: c_ptr
            type(c_ptr), value :: keyboard, listener, data
        end subroutine wl_keyboard_add_listener

    end interface

    ! Global state
    type(wl_display) :: g_display
    type(wl_registry) :: g_registry
    type(wl_compositor) :: g_compositor
    type(wl_shm) :: g_shm
    type(wl_seat) :: g_seat
    type(wl_pointer) :: g_pointer
    type(wl_keyboard) :: g_keyboard
    type(xdg_wm_base) :: g_xdg_wm_base

    ! Mouse and keyboard state
    type(forge_mouse_state), save :: g_mouse_state
    type(forge_keyboard_state), save :: g_keyboard_state

    ! Render callback
    abstract interface
        subroutine render_callback_interface(window_handle, context)
            import :: c_ptr
            type(c_ptr), intent(in) :: window_handle, context
        end subroutine render_callback_interface
    end interface

    procedure(render_callback_interface), pointer, save :: g_render_callback => null()

    !> @brief Wayland platform implementation
    type, extends(forge_platform_base) :: forge_wayland_platform
        private
        type(wl_surface) :: surface
        type(xdg_surface) :: xdg_surface
        type(xdg_toplevel) :: toplevel
        type(wl_buffer) :: buffer
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
        integer(c_int), pointer :: pixels(:) => null()
        logical :: configured = .false.
    contains
        procedure :: init => wayland_init
        procedure :: shutdown => wayland_shutdown
        procedure :: create_window => wayland_create_window
        procedure :: destroy_window => wayland_destroy_window
        procedure :: show_window => wayland_show_window
        procedure :: hide_window => wayland_hide_window
        procedure :: process_events => wayland_process_events
        procedure :: get_surface => wayland_get_surface
    end type forge_wayland_platform

contains

    !> @brief Initialize Wayland platform
    subroutine wayland_init(this, status)
        class(forge_wayland_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        write(output_unit, '(A)') "[Wayland] Initializing Wayland platform..."

        ! Connect to display
        g_display%ptr = wl_display_connect(c_null_char)
        if (.not. c_associated(g_display%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to connect to Wayland display")
            return
        end if

        ! Get registry
        g_registry%ptr = wl_display_get_registry(g_display%ptr)
        if (.not. c_associated(g_registry%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get Wayland registry")
            return
        end if

        ! Add registry listener
        call wl_registry_add_listener(g_registry%ptr, c_funloc(registry_listener), c_null_ptr)

        ! Roundtrip to get initial globals
        call wl_display_roundtrip(g_display%ptr)

        ! Check if we got required interfaces
        if (.not. c_associated(g_compositor%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Wayland compositor not available")
            return
        end if
        if (.not. c_associated(g_shm%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Wayland SHM not available")
            return
        end if
        if (.not. c_associated(g_xdg_wm_base%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "XDG WM base not available")
            return
        end if

        this%initialized = .true.
        call status%clear()
        write(output_unit, '(A)') "[Wayland] Platform initialized successfully"
    end subroutine wayland_init

    !> @brief Shutdown Wayland platform
    subroutine wayland_shutdown(this)
        class(forge_wayland_platform), intent(inout) :: this

        write(output_unit, '(A)') "[Wayland] Shutting down Wayland platform"

        ! Clean up window resources
        if (associated(this%pixels)) deallocate(this%pixels)
        if (c_associated(this%buffer%ptr)) call wl_buffer_destroy(this%buffer%ptr)
        if (c_associated(this%toplevel%ptr)) then
            ! XDG toplevel cleanup would go here
        end if
        if (c_associated(this%xdg_surface%ptr)) then
            ! XDG surface cleanup would go here
        end if
        if (c_associated(this%surface%ptr)) call wl_surface_destroy(this%surface%ptr)

        ! Disconnect display
        if (c_associated(g_display%ptr)) then
            call wl_display_disconnect(g_display%ptr)
            g_display%ptr = c_null_ptr
        end if

        this%initialized = .false.
    end subroutine wayland_shutdown

    !> @brief Create a window
    subroutine wayland_create_window(this, handle, title, width, height, status)
        class(forge_wayland_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        character(len=:), allocatable :: title_c

        ! Create surface
        this%surface%ptr = wl_compositor_create_surface(g_compositor%ptr)
        if (.not. c_associated(this%surface%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Wayland surface")
            return
        end if

        ! Create XDG surface
        this%xdg_surface%ptr = xdg_wm_base_get_xdg_surface(g_xdg_wm_base%ptr, this%surface%ptr)
        if (.not. c_associated(this%xdg_surface%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create XDG surface")
            return
        end if

        ! Create toplevel
        this%toplevel%ptr = xdg_surface_get_toplevel(this%xdg_surface%ptr)
        if (.not. c_associated(this%toplevel%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create XDG toplevel")
            return
        end if

        ! Set title
        title_c = trim(title) // c_null_char
        call xdg_toplevel_set_title(this%toplevel%ptr, title_c)

        ! Add listeners
        call xdg_surface_add_listener(this%xdg_surface%ptr, c_funloc(xdg_surface_listener), c_loc(this))
        call xdg_toplevel_add_listener(this%toplevel%ptr, c_funloc(xdg_toplevel_listener), c_loc(this))

        ! Commit surface
        call wl_surface_commit(this%surface%ptr)

        this%width = width
        this%height = height

        handle%native_handle = this%surface%ptr
        handle%width = width
        handle%height = height

        call status%clear()
        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[Wayland] Created window '", trim(title), "' (", width, "x", height, ")"
    end subroutine wayland_create_window

    !> @brief Destroy a window
    subroutine wayland_destroy_window(this, handle)
        class(forge_wayland_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        write(output_unit, '(A)') "[Wayland] Window destroyed"
        ! Resources cleaned up in shutdown
    end subroutine wayland_destroy_window

    !> @brief Show a window
    subroutine wayland_show_window(this, handle)
        class(forge_wayland_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        write(output_unit, '(A)') "[Wayland] Window shown"
        ! Window is shown when configured
    end subroutine wayland_show_window

    !> @brief Hide a window
    subroutine wayland_hide_window(this, handle)
        class(forge_wayland_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        write(output_unit, '(A)') "[Wayland] Window hidden"
        ! Would need to unmap surface
    end subroutine wayland_hide_window

    !> @brief Process pending events
    subroutine wayland_process_events(this, should_quit)
        class(forge_wayland_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        integer(c_int) :: result

        should_quit = .false.

        ! Dispatch Wayland events
        result = wl_display_dispatch(g_display%ptr)
        if (result == -1) then
            write(error_unit, '(A)') "[Wayland] Display dispatch failed"
            should_quit = .true.
        end if
    end subroutine wayland_process_events

    !> @brief Get drawing surface for window
    subroutine wayland_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_wayland_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status

        if (.not. this%configured) then
            call status%set(FORGE_ERROR_BACKEND, "Window not yet configured")
            return
        end if

        ! Allocate pixel buffer if needed
        if (.not. associated(this%pixels)) then
            allocate(this%pixels(this%width * this%height))
            this%pixels = 0  ! Clear to black
        end if

        ! Create Cairo image surface from pixel buffer
        surface%surface = cairo_image_surface_create_for_data( &
            c_loc(this%pixels(1)), &
            CAIRO_FORMAT_ARGB32, &
            this%width, this%height, &
            this%width * 4)

        if (.not. c_associated(surface%surface)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo surface")
            return
        end if

        ! Create Cairo context
        surface%context = cairo_create(surface%surface)
        if (.not. c_associated(surface%context)) then
            call cairo_surface_destroy(surface%surface)
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo context")
            return
        end if

        call status%clear()
    end subroutine wayland_get_surface

    !> @brief Set render callback
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Registry listener
    subroutine registry_listener(data, registry, id, interface, version) bind(C)
        type(c_ptr), value :: data, registry
        integer(c_int), value :: id, version
        character(kind=c_char), intent(in) :: interface(*)
        character(len=:), allocatable :: interface_name

        interface_name = trim(c_to_f_string(interface))

        select case (interface_name)
        case ("wl_compositor")
            g_compositor%ptr = wl_registry_bind(registry, id, c_loc("wl_compositor"), WL_COMPOSITOR_VERSION)
        case ("wl_shm")
            g_shm%ptr = wl_registry_bind(registry, id, c_loc("wl_shm"), WL_SHM_VERSION)
        case ("wl_seat")
            g_seat%ptr = wl_registry_bind(registry, id, c_loc("wl_seat"), WL_SEAT_VERSION)
            if (c_associated(g_seat%ptr)) then
                call wl_seat_add_listener(g_seat%ptr, c_funloc(seat_listener), c_null_ptr)
            end if
        case ("xdg_wm_base")
            g_xdg_wm_base%ptr = wl_registry_bind(registry, id, c_loc("xdg_wm_base"), XDG_WM_BASE_VERSION)
        end select
    end subroutine registry_listener

    !> @brief Seat listener
    subroutine seat_listener(data, seat, capabilities) bind(C)
        type(c_ptr), value :: data, seat
        integer(c_int), value :: capabilities

        ! Handle pointer and keyboard capabilities
        if (iand(capabilities, 1) /= 0) then  ! WL_SEAT_CAPABILITY_POINTER
            g_pointer%ptr = wl_seat_get_pointer(seat)
            if (c_associated(g_pointer%ptr)) then
                call wl_pointer_add_listener(g_pointer%ptr, c_funloc(pointer_listener), c_null_ptr)
            end if
        end if

        if (iand(capabilities, 2) /= 0) then  ! WL_SEAT_CAPABILITY_KEYBOARD
            g_keyboard%ptr = wl_seat_get_keyboard(seat)
            if (c_associated(g_keyboard%ptr)) then
                call wl_keyboard_add_listener(g_keyboard%ptr, c_funloc(keyboard_listener), c_null_ptr)
            end if
        end if
    end subroutine seat_listener

    !> @brief Pointer listener
    subroutine pointer_listener(data, pointer, event_type, args) bind(C)
        type(c_ptr), value :: data, pointer
        integer(c_int), value :: event_type
        type(c_ptr), value :: args

        select case (event_type)
        case (0)  ! WL_POINTER_ENTER
            g_mouse_state%over_window = .true.
        case (1)  ! WL_POINTER_LEAVE
            g_mouse_state%over_window = .false.
        case (2)  ! WL_POINTER_MOTION
            ! Extract coordinates from args
            call handle_pointer_motion(args)
        case (3)  ! WL_POINTER_BUTTON
            call handle_pointer_button(args)
        end select
    end subroutine pointer_listener

    !> @brief Keyboard listener
    subroutine keyboard_listener(data, keyboard, event_type, args) bind(C)
        type(c_ptr), value :: data, keyboard
        integer(c_int), value :: event_type
        type(c_ptr), value :: args

        select case (event_type)
        case (0)  ! WL_KEYBOARD_KEY
            call handle_keyboard_key(args)
        end select
    end subroutine keyboard_listener

    !> @brief XDG surface listener
    subroutine xdg_surface_listener(data, xdg_surface, event_type, args) bind(C)
        type(c_ptr), value :: data, xdg_surface
        integer(c_int), value :: event_type
        type(c_ptr), value :: args
        type(forge_wayland_platform), pointer :: platform

        call c_f_pointer(data, platform)

        select case (event_type)
        case (0)  ! XDG_SURFACE_CONFIGURE
            call handle_xdg_configure(platform, args)
        end select
    end subroutine xdg_surface_listener

    !> @brief XDG toplevel listener
    subroutine xdg_toplevel_listener(data, toplevel, event_type, args) bind(C)
        type(c_ptr), value :: data, toplevel
        integer(c_int), value :: event_type
        type(c_ptr), value :: args

        select case (event_type)
        case (0)  ! XDG_TOPLEVEL_CLOSE
            write(output_unit, '(A)') "[Wayland] Close requested"
        end select
    end subroutine xdg_toplevel_listener

    ! Helper functions (simplified implementations)
    function wl_registry_bind(registry, id, interface, version) result(proxy)
        type(c_ptr), value :: registry
        integer(c_int), value :: id, version
        type(c_ptr), value :: interface
        type(c_ptr) :: proxy
        ! This would call wl_proxy_marshal_constructor
        proxy = c_null_ptr
    end function wl_registry_bind

    function wl_seat_get_pointer(seat) result(pointer)
        type(c_ptr), value :: seat
        type(c_ptr) :: pointer
        ! This would call wl_proxy_marshal_constructor
        pointer = c_null_ptr
    end function wl_seat_get_pointer

    function wl_seat_get_keyboard(seat) result(keyboard)
        type(c_ptr), value :: seat
        type(c_ptr) :: keyboard
        ! This would call wl_proxy_marshal_constructor
        keyboard = c_null_ptr
    end function wl_seat_get_keyboard

    subroutine xdg_surface_add_listener(xdg_surface, listener, data)
        type(c_ptr), value :: xdg_surface, listener, data
        ! Add listener implementation
    end subroutine xdg_surface_add_listener

    subroutine xdg_toplevel_add_listener(toplevel, listener, data)
        type(c_ptr), value :: toplevel, listener, data
        ! Add listener implementation
    end subroutine xdg_toplevel_add_listener

    subroutine handle_pointer_motion(args)
        type(c_ptr), value :: args
        ! Extract surface_x, surface_y from args
        call dispatch_mouse_event(c_null_ptr, EVENT_MOUSE_MOVED, 0, 0, 0)
    end subroutine handle_pointer_motion

    subroutine handle_pointer_button(args)
        type(c_ptr), value :: args
        ! Extract button, state from args
        call dispatch_mouse_event(c_null_ptr, EVENT_MOUSE_PRESSED, 0, 0, MOUSE_BUTTON_LEFT)
    end subroutine handle_pointer_button

    subroutine handle_keyboard_key(args)
        type(c_ptr), value :: args
        ! Extract key, state from args
        call dispatch_keyboard_event(c_null_ptr, EVENT_KEY_PRESSED, 0)
    end subroutine handle_keyboard_key

    subroutine handle_xdg_configure(platform, args)
        type(forge_wayland_platform), intent(inout) :: platform
        type(c_ptr), value :: args
        ! Extract width, height, serial from args
        platform%configured = .true.
        call xdg_surface_ack_configure(platform%xdg_surface%ptr, 0)
    end subroutine handle_xdg_configure

    !> @brief Dispatch mouse event
    subroutine dispatch_mouse_event(window_handle, event_type, x, y, button)
        type(c_ptr), intent(in) :: window_handle
        integer, intent(in) :: event_type, x, y, button
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = window_handle
        event%mouse_x = x
        event%mouse_y = y
        event%mouse_button = button

        write(output_unit, '(A,I0,A,I0,A,I0,A,I0)') "[Wayland] Mouse event: ", &
            event_type, " at (", x, ",", y, ") button=", button
    end subroutine dispatch_mouse_event

    !> @brief Dispatch keyboard event
    subroutine dispatch_keyboard_event(window_handle, event_type, key_code)
        type(c_ptr), intent(in) :: window_handle
        integer, intent(in) :: event_type, key_code
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = window_handle
        event%key_code = key_code

        write(output_unit, '(A,I0,A,I0)') "[Wayland] Keyboard event: ", &
            event_type, " key=", key_code
    end subroutine dispatch_keyboard_event

    ! Utility function
    function c_to_f_string(c_string) result(f_string)
        character(kind=c_char), intent(in) :: c_string(*)
        character(len=:), allocatable :: f_string
        integer :: i

        i = 1
        do while (c_string(i) /= c_null_char)
            i = i + 1
        end do
        allocate(character(len=i-1) :: f_string)
        f_string = transfer(c_string(1:i-1), f_string)
    end function c_to_f_string

end module forge_platform_wayland