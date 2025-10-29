!> @brief X11 platform implementation using Xlib
!> @details Provides windowing and event handling for Linux X11
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_x11
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    use forge_events
    implicit none
    private

    public :: forge_x11_platform
    public :: set_window_render_callback

    ! X11 API constants
    integer(c_int), parameter :: None = 0
    integer(c_int), parameter :: InputOutput = 1
    integer(c_int), parameter :: CWBackPixmap = 1
    integer(c_int), parameter :: CWBackPixel = 2
    integer(c_int), parameter :: CWBorderPixmap = 4
    integer(c_int), parameter :: CWBorderPixel = 8
    integer(c_int), parameter :: CWBitGravity = 16
    integer(c_int), parameter :: CWWinGravity = 32
    integer(c_int), parameter :: CWBackingStore = 64
    integer(c_int), parameter :: CWBackingPlanes = 128
    integer(c_int), parameter :: CWBackingPixel = 256
    integer(c_int), parameter :: CWOverrideRedirect = 512
    integer(c_int), parameter :: CWSaveUnder = 1024
    integer(c_int), parameter :: CWEventMask = 2048
    integer(c_int), parameter :: CWDontPropagate = 4096
    integer(c_int), parameter :: CWColormap = 8192
    integer(c_int), parameter :: CWCursor = 16384

    ! Event masks
    integer(c_long), parameter :: ExposureMask = int(z'80000', c_long)
    integer(c_long), parameter :: KeyPressMask = int(z'100000', c_long)
    integer(c_long), parameter :: KeyReleaseMask = int(z'200000', c_long)
    integer(c_long), parameter :: ButtonPressMask = int(z'400000', c_long)
    integer(c_long), parameter :: ButtonReleaseMask = int(z'800000', c_long)
    integer(c_long), parameter :: PointerMotionMask = int(z'4000000', c_long)
    integer(c_long), parameter :: StructureNotifyMask = int(z'20000', c_long)

    ! Event types
    integer(c_int), parameter :: Expose = 12
    integer(c_int), parameter :: KeyPress = 2
    integer(c_int), parameter :: KeyRelease = 3
    integer(c_int), parameter :: ButtonPress = 4
    integer(c_int), parameter :: ButtonRelease = 5
    integer(c_int), parameter :: MotionNotify = 6
    integer(c_int), parameter :: ConfigureNotify = 22
    integer(c_int), parameter :: ClientMessage = 33

    ! X11 structures
    type, bind(C) :: XSetWindowAttributes
        type(c_ptr) :: background_pixmap
        integer(c_long) :: background_pixel
        type(c_ptr) :: border_pixmap
        integer(c_long) :: border_pixel
        integer(c_int) :: bit_gravity
        integer(c_int) :: win_gravity
        integer(c_int) :: backing_store
        integer(c_long) :: backing_planes
        integer(c_long) :: backing_pixel
        logical(c_bool) :: save_under
        integer(c_long) :: event_mask
        integer(c_long) :: do_not_propagate_mask
        logical(c_bool) :: override_redirect
        type(c_ptr) :: colormap
        type(c_ptr) :: cursor
    end type XSetWindowAttributes

    type, bind(C) :: XEvent
        integer(c_int) :: type
        integer(c_long) :: serial
        logical(c_bool) :: send_event
        type(c_ptr) :: display
        integer(c_long) :: window
        integer(c_long) :: root
        integer(c_long) :: subwindow
        integer(c_long) :: time
        integer(c_int) :: x, y
        integer(c_int) :: x_root, y_root
        integer(c_int) :: state
        integer(c_int) :: keycode
        logical(c_bool) :: same_screen
        integer(c_int) :: button
        integer(c_int) :: width, height
        integer(c_int) :: count
        type(c_ptr) :: data
    end type XEvent

    type, bind(C) :: XWindowAttributes
        integer(c_int) :: x, y
        integer(c_int) :: width, height
        integer(c_int) :: border_width
        integer(c_int) :: depth
        type(c_ptr) :: visual
        integer(c_long) :: root
        integer(c_int) :: class
        integer(c_int) :: bit_gravity
        integer(c_int) :: win_gravity
        integer(c_int) :: backing_store
        integer(c_long) :: backing_planes
        integer(c_long) :: backing_pixel
        logical(c_bool) :: save_under
        type(c_ptr) :: colormap
        logical(c_bool) :: map_installed
        integer(c_int) :: map_state
        integer(c_long) :: all_event_masks
        integer(c_long) :: your_event_mask
        integer(c_long) :: do_not_propagate_mask
        logical(c_bool) :: override_redirect
        type(c_ptr) :: screen
    end type XWindowAttributes

    ! X11 API bindings
    interface

        ! Display management
        function XOpenDisplay(display_name) bind(C, name="XOpenDisplay")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: display_name(*)
            type(c_ptr) :: XOpenDisplay
        end function XOpenDisplay

        function XCloseDisplay(display) bind(C, name="XCloseDisplay")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: XCloseDisplay
        end function XCloseDisplay

        function XDefaultScreen(display) bind(C, name="XDefaultScreen")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: XDefaultScreen
        end function XDefaultScreen

        function XDefaultRootWindow(display) bind(C, name="XDefaultRootWindow")
            import :: c_ptr, c_long
            type(c_ptr), value :: display
            integer(c_long) :: XDefaultRootWindow
        end function XDefaultRootWindow

        ! Window management
        function XCreateWindow(display, parent, x, y, width, height, border_width, depth, class, visual, valuemask, attributes) &
                bind(C, name="XCreateWindow")
            import :: c_ptr, c_int, c_long, XSetWindowAttributes
            type(c_ptr), value :: display
            integer(c_long), value :: parent
            integer(c_int), value :: x, y, width, height, border_width, depth, class
            type(c_ptr), value :: visual
            integer(c_long), value :: valuemask
            type(XSetWindowAttributes), intent(in) :: attributes
            integer(c_long) :: XCreateWindow
        end function XCreateWindow

        function XDestroyWindow(display, w) bind(C, name="XDestroyWindow")
            import :: c_ptr, c_long, c_int
            type(c_ptr), value :: display
            integer(c_long), value :: w
            integer(c_int) :: XDestroyWindow
        end function XDestroyWindow

        function XMapWindow(display, w) bind(C, name="XMapWindow")
            import :: c_ptr, c_long, c_int
            type(c_ptr), value :: display
            integer(c_long), value :: w
            integer(c_int) :: XMapWindow
        end function XMapWindow

        function XUnmapWindow(display, w) bind(C, name="XUnmapWindow")
            import :: c_ptr, c_long, c_int
            type(c_ptr), value :: display
            integer(c_long), value :: w
            integer(c_int) :: XUnmapWindow
        end function XUnmapWindow

        function XStoreName(display, w, window_name) bind(C, name="XStoreName")
            import :: c_ptr, c_long, c_int
            type(c_ptr), value :: display
            integer(c_long), value :: w
            character(kind=c_char), intent(in) :: window_name(*)
            integer(c_int) :: XStoreName
        end function XStoreName

        ! Event handling
        function XNextEvent(display, event_return) bind(C, name="XNextEvent")
            import :: c_ptr, XEvent, c_int
            type(c_ptr), value :: display
            type(XEvent), intent(out) :: event_return
            integer(c_int) :: XNextEvent
        end function XNextEvent

        function XPending(display) bind(C, name="XPending")
            import :: c_ptr, c_int
            type(c_ptr), value :: display
            integer(c_int) :: XPending
        end function XPending

        function XCheckTypedEvent(display, event_type, event_return) bind(C, name="XCheckTypedEvent")
            import :: c_ptr, c_int, XEvent
            type(c_ptr), value :: display
            integer(c_int), value :: event_type
            type(XEvent), intent(out) :: event_return
            integer(c_int) :: XCheckTypedEvent
        end function XCheckTypedEvent

        ! Atom management
        function XInternAtom(display, atom_name, only_if_exists) bind(C, name="XInternAtom")
            import :: c_ptr, c_char, c_int, c_long
            type(c_ptr), value :: display
            character(kind=c_char), intent(in) :: atom_name(*)
            logical(c_bool), value :: only_if_exists
            integer(c_long) :: XInternAtom
        end function XInternAtom

        ! Window properties
        function XGetWindowAttributes(display, w, window_attributes_return) bind(C, name="XGetWindowAttributes")
            import :: c_ptr, c_long, XWindowAttributes, c_int
            type(c_ptr), value :: display
            integer(c_long), value :: w
            type(XWindowAttributes), intent(out) :: window_attributes_return
            integer(c_int) :: XGetWindowAttributes
        end function XGetWindowAttributes

    end interface

    ! Mouse and keyboard state tracking
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

    !> @brief X11 platform implementation
    type, extends(forge_platform_base) :: forge_x11_platform
        private
        type(c_ptr) :: display = c_null_ptr
        integer(c_int) :: screen = 0
        integer(c_long) :: root_window = 0
        logical :: display_opened = .false.
    contains
        procedure :: init => x11_init
        procedure :: shutdown => x11_shutdown
        procedure :: create_window => x11_create_window
        procedure :: destroy_window => x11_destroy_window
        procedure :: show_window => x11_show_window
        procedure :: hide_window => x11_hide_window
        procedure :: process_events => x11_process_events
        procedure :: get_surface => x11_get_surface
    end type forge_x11_platform

contains

    !> @brief Initialize X11 platform
    subroutine x11_init(this, status)
        class(forge_x11_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        write(output_unit, '(A)') "[X11] Initializing X11 platform..."

        ! Open display
        this%display = XOpenDisplay(c_null_char)
        if (.not. c_associated(this%display)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to open X11 display")
            return
        end if

        this%screen = XDefaultScreen(this%display)
        this%root_window = XDefaultRootWindow(this%display)
        this%display_opened = .true.
        this%initialized = .true.

        call status%clear()
        write(output_unit, '(A)') "[X11] Platform initialized successfully"
    end subroutine x11_init

    !> @brief Shutdown X11 platform
    subroutine x11_shutdown(this)
        class(forge_x11_platform), intent(inout) :: this

        write(output_unit, '(A)') "[X11] Shutting down X11 platform"

        if (this%display_opened .and. c_associated(this%display)) then
            call XCloseDisplay(this%display)
            this%display = c_null_ptr
        end if

        this%display_opened = .false.
        this%initialized = .false.
    end subroutine x11_shutdown

    !> @brief Create a window
    subroutine x11_create_window(this, handle, title, width, height, status)
        class(forge_x11_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        integer(c_long) :: window_id
        type(XSetWindowAttributes) :: wa
        character(len=:), allocatable :: title_c

        ! Null-terminate title
        title_c = trim(title) // c_null_char

        ! Set window attributes
        wa%event_mask = ExposureMask + KeyPressMask + KeyReleaseMask + &
                       ButtonPressMask + ButtonReleaseMask + PointerMotionMask + &
                       StructureNotifyMask
        wa%background_pixel = 0  ! Black background

        ! Create window
        window_id = XCreateWindow(this%display, this%root_window, 100, 100, &
                                 width, height, 0, 0, InputOutput, c_null_ptr, &
                                 CWEventMask + CWBackPixel, wa)

        if (window_id == 0) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create X11 window")
            return
        end if

        ! Set window title
        call XStoreName(this%display, window_id, title_c)

        handle%native_handle = transfer(window_id, c_null_ptr)
        handle%width = width
        handle%height = height

        call status%clear()
        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[X11] Created window '", trim(title), "' (", width, "x", height, ")"
    end subroutine x11_create_window

    !> @brief Destroy a window
    subroutine x11_destroy_window(this, handle)
        class(forge_x11_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_long) :: window_id
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            window_id = transfer(handle%native_handle, 0_c_long)
            result = XDestroyWindow(this%display, window_id)
            write(output_unit, '(A)') "[X11] Window destroyed"
        end if
    end subroutine x11_destroy_window

    !> @brief Show a window
    subroutine x11_show_window(this, handle)
        class(forge_x11_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_long) :: window_id
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            window_id = transfer(handle%native_handle, 0_c_long)
            result = XMapWindow(this%display, window_id)
            write(output_unit, '(A)') "[X11] Window shown"
        end if
    end subroutine x11_show_window

    !> @brief Hide a window
    subroutine x11_hide_window(this, handle)
        class(forge_x11_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_long) :: window_id
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            window_id = transfer(handle%native_handle, 0_c_long)
            result = XUnmapWindow(this%display, window_id)
            write(output_unit, '(A)') "[X11] Window hidden"
        end if
    end subroutine x11_hide_window

    !> @brief Process pending events
    subroutine x11_process_events(this, should_quit)
        class(forge_x11_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        type(XEvent) :: event
        integer(c_int) :: result

        should_quit = .false.

        ! Process all pending events
        do while (XPending(this%display) > 0)
            result = XNextEvent(this%display, event)

            select case (event%type)
            case (Expose)
                ! Repaint event
                if (associated(g_render_callback)) then
                    call handle_expose_event(event)
                end if

            case (KeyPress)
                call handle_key_event(event, .true.)
            case (KeyRelease)
                call handle_key_event(event, .false.)

            case (ButtonPress)
                call handle_button_event(event, .true.)
            case (ButtonRelease)
                call handle_button_event(event, .false.)

            case (MotionNotify)
                call handle_motion_event(event)

            case (ConfigureNotify)
                call handle_configure_event(event)

            case (ClientMessage)
                ! Check for WM_DELETE_WINDOW
                should_quit = .true.
                write(output_unit, '(A)') "[X11] Quit message received"

            end select
        end do
    end subroutine x11_process_events

    !> @brief Get drawing surface for window
    subroutine x11_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_x11_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status
        integer(c_long) :: window_id

        if (.not. c_associated(window%native_handle)) then
            call status%set(FORGE_ERROR_NULL_PTR, "Invalid window handle")
            return
        end if

        window_id = transfer(window%native_handle, 0_c_long)

        ! Create Cairo X11 surface
        surface%surface = cairo_xlib_surface_create(this%display, window_id, &
                                                   c_null_ptr, window%width, window%height)

        if (.not. c_associated(surface%surface)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo X11 surface")
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
    end subroutine x11_get_surface

    !> @brief Set render callback for expose events
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Handle expose event
    subroutine handle_expose_event(event)
        type(XEvent), intent(in) :: event
        type(c_ptr) :: cairo_surface, cairo_ctx

        if (.not. associated(g_render_callback)) return

        ! Create temporary Cairo context for the expose event
        cairo_surface = cairo_xlib_surface_create(event%display, event%window, &
                                                 c_null_ptr, event%width, event%height)
        if (c_associated(cairo_surface)) then
            cairo_ctx = cairo_create(cairo_surface)
            if (c_associated(cairo_ctx)) then
                call g_render_callback(transfer(event%window, c_null_ptr), cairo_ctx)
                call cairo_surface_flush(cairo_surface)
                call cairo_destroy(cairo_ctx)
            end if
            call cairo_surface_destroy(cairo_surface)
        end if
    end subroutine handle_expose_event

    !> @brief Handle keyboard event
    subroutine handle_key_event(event, pressed)
        type(XEvent), intent(in) :: event
        logical, intent(in) :: pressed
        integer :: event_type

        if (pressed) then
            event_type = EVENT_KEY_PRESSED
        else
            event_type = EVENT_KEY_RELEASED
        end if

        call dispatch_keyboard_event(transfer(event%window, c_null_ptr), &
                                   event_type, int(event%keycode))
    end subroutine handle_key_event

    !> @brief Handle mouse button event
    subroutine handle_button_event(event, pressed)
        type(XEvent), intent(in) :: event
        logical, intent(in) :: pressed
        integer :: event_type, button

        if (pressed) then
            event_type = EVENT_MOUSE_PRESSED
        else
            event_type = EVENT_MOUSE_RELEASED
        end if

        select case (event%button)
        case (1)
            button = MOUSE_BUTTON_LEFT
        case (2)
            button = MOUSE_BUTTON_MIDDLE
        case (3)
            button = MOUSE_BUTTON_RIGHT
        case default
            button = 0
        end select

        call update_mouse_state(g_mouse_state, event%x, event%y)
        call dispatch_mouse_event(transfer(event%window, c_null_ptr), &
                                event_type, event%x, event%y, button)
    end subroutine handle_button_event

    !> @brief Handle mouse motion event
    subroutine handle_motion_event(event)
        type(XEvent), intent(in) :: event

        call update_mouse_state(g_mouse_state, event%x, event%y)
        g_mouse_state%over_window = .true.
        call dispatch_mouse_event(transfer(event%window, c_null_ptr), &
                                EVENT_MOUSE_MOVED, event%x, event%y, 0)
    end subroutine handle_motion_event

    !> @brief Handle configure event (resize)
    subroutine handle_configure_event(event)
        type(XEvent), intent(in) :: event

        write(output_unit, '(A,I0,A,I0)') "[X11] Window resized to ", &
            event%width, "x", event%height
        ! Could dispatch EVENT_WINDOW_RESIZED here if needed
    end subroutine handle_configure_event

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

        write(output_unit, '(A,I0,A,I0,A,I0,A,I0)') "[X11] Mouse event: ", &
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

        write(output_unit, '(A,I0,A,I0)') "[X11] Keyboard event: ", &
            event_type, " key=", key_code
    end subroutine dispatch_keyboard_event

    ! Cairo X11 helper function
    function cairo_xlib_surface_create(display, drawable, visual, width, height) result(surface)
        use forge_cairo_bindings
        type(c_ptr), value :: display, drawable, visual
        integer(c_int), value :: width, height
        type(c_ptr) :: surface

        ! This would need cairo-xlib bindings, for now return null
        surface = c_null_ptr
        write(error_unit, '(A)') "[X11] Cairo X11 surface creation not implemented - requires cairo-xlib"
    end function cairo_xlib_surface_create

end module forge_platform_x11