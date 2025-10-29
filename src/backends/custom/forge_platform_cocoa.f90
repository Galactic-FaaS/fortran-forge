!> @brief Cocoa platform implementation using macOS NSWindow/NSView
!> @details Provides windowing and event handling for macOS
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_cocoa
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    use forge_events
    implicit none
    private

    public :: forge_cocoa_platform
    public :: set_window_render_callback

    ! Cocoa constants
    integer(c_long), parameter :: NSApplicationActivationPolicyRegular = 0
    integer(c_long), parameter :: NSTitledWindowMask = 1
    integer(c_long), parameter :: NSClosableWindowMask = 2
    integer(c_long), parameter :: NSMiniaturizableWindowMask = 4
    integer(c_long), parameter :: NSResizableWindowMask = 8
    integer(c_long), parameter :: NSBorderlessWindowMask = 0

    ! NSEvent types
    integer(c_long), parameter :: NSLeftMouseDown = 1
    integer(c_long), parameter :: NSLeftMouseUp = 2
    integer(c_long), parameter :: NSRightMouseDown = 3
    integer(c_long), parameter :: NSRightMouseUp = 4
    integer(c_long), parameter :: NSMouseMoved = 5
    integer(c_long), parameter :: NSLeftMouseDragged = 6
    integer(c_long), parameter :: NSRightMouseDragged = 7
    integer(c_long), parameter :: NSMouseEntered = 8
    integer(c_long), parameter :: NSMouseExited = 9
    integer(c_long), parameter :: NSKeyDown = 10
    integer(c_long), parameter :: NSKeyUp = 11
    integer(c_long), parameter :: NSFlagsChanged = 12

    ! NSEvent modifier flags
    integer(c_long), parameter :: NSShiftKeyMask = 131072
    integer(c_long), parameter :: NSControlKeyMask = 262144
    integer(c_long), parameter :: NSAlternateKeyMask = 524288
    integer(c_long), parameter :: NSCommandKeyMask = 1048576

    ! Cocoa structures (simplified for Fortran binding)
    type :: NSApplication
        type(c_ptr) :: ptr = c_null_ptr
    end type NSApplication

    type :: NSWindow
        type(c_ptr) :: ptr = c_null_ptr
    end type NSWindow

    type :: NSView
        type(c_ptr) :: ptr = c_null_ptr
    end type NSView

    type :: NSEvent
        type(c_ptr) :: ptr = c_null_ptr
    end type NSEvent

    type :: NSRect
        real(c_double) :: x, y, width, height
    end type NSRect

    type :: NSSize
        real(c_double) :: width, height
    end type NSSize

    type :: NSPoint
        real(c_double) :: x, y
    end type NSPoint

    ! Cocoa API bindings
    interface

        ! Application management
        function NSApplication_sharedApplication() bind(C, name="NSApplication_sharedApplication")
            import :: c_ptr
            type(c_ptr) :: NSApplication_sharedApplication
        end function NSApplication_sharedApplication

        subroutine NSApplication_setActivationPolicy(app, policy) &
                bind(C, name="NSApplication_setActivationPolicy")
            import :: c_ptr, c_long
            type(c_ptr), value :: app
            integer(c_long), value :: policy
        end subroutine NSApplication_setActivationPolicy

        subroutine NSApplication_run(app) bind(C, name="NSApplication_run")
            import :: c_ptr
            type(c_ptr), value :: app
        end subroutine NSApplication_run

        function NSApplication_nextEventMatchingMask(app, mask, expiration, mode, dequeue) &
                bind(C, name="NSApplication_nextEventMatchingMask")
            import :: c_ptr, c_long, c_double
            type(c_ptr), value :: app
            integer(c_long), value :: mask
            real(c_double), value :: expiration
            type(c_ptr), value :: mode, dequeue
            type(c_ptr) :: NSApplication_nextEventMatchingMask
        end function NSApplication_nextEventMatchingMask

        subroutine NSApplication_sendEvent(app, event) bind(C, name="NSApplication_sendEvent")
            import :: c_ptr
            type(c_ptr), value :: app, event
        end subroutine NSApplication_sendEvent

        ! Window management
        function NSWindow_alloc() bind(C, name="NSWindow_alloc")
            import :: c_ptr
            type(c_ptr) :: NSWindow_alloc
        end function NSWindow_alloc

        function NSWindow_initWithContentRect(window, rect, styleMask, backing, defer) &
                bind(C, name="NSWindow_initWithContentRect")
            import :: c_ptr, NSRect, c_long
            type(c_ptr), value :: window
            type(NSRect), intent(in) :: rect
            integer(c_long), value :: styleMask, backing, defer
            type(c_ptr) :: NSWindow_initWithContentRect
        end function NSWindow_initWithContentRect

        subroutine NSWindow_setTitle(window, title) bind(C, name="NSWindow_setTitle")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr), value :: title
        end subroutine NSWindow_setTitle

        subroutine NSWindow_makeKeyAndOrderFront(window, sender) &
                bind(C, name="NSWindow_makeKeyAndOrderFront")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr), value :: sender
        end subroutine NSWindow_makeKeyAndOrderFront

        subroutine NSWindow_orderOut(window, sender) bind(C, name="NSWindow_orderOut")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr), value :: sender
        end subroutine NSWindow_orderOut

        subroutine NSWindow_close(window) bind(C, name="NSWindow_close")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine NSWindow_close

        function NSWindow_contentView(window) bind(C, name="NSWindow_contentView")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr) :: NSWindow_contentView
        end function NSWindow_contentView

        ! View management
        function NSView_alloc() bind(C, name="NSView_alloc")
            import :: c_ptr
            type(c_ptr) :: NSView_alloc
        end function NSView_alloc

        function NSView_initWithFrame(view, frame) bind(C, name="NSView_initWithFrame")
            import :: c_ptr, NSRect
            type(c_ptr), value :: view
            type(NSRect), intent(in) :: frame
            type(c_ptr) :: NSView_initWithFrame
        end function NSView_initWithFrame

        subroutine NSWindow_setContentView(window, view) bind(C, name="NSWindow_setContentView")
            import :: c_ptr
            type(c_ptr), value :: window, view
        end subroutine NSWindow_setContentView

        ! Event handling
        function NSEvent_type(event) bind(C, name="NSEvent_type")
            import :: c_ptr, c_long
            type(c_ptr), value :: event
            integer(c_long) :: NSEvent_type
        end function NSEvent_type

        function NSEvent_locationInWindow(event) bind(C, name="NSEvent_locationInWindow")
            import :: c_ptr, NSPoint
            type(c_ptr), value :: event
            type(NSPoint) :: NSEvent_locationInWindow
        end function NSEvent_locationInWindow

        function NSEvent_buttonNumber(event) bind(C, name="NSEvent_buttonNumber")
            import :: c_ptr, c_long
            type(c_ptr), value :: event
            integer(c_long) :: NSEvent_buttonNumber
        end function NSEvent_buttonNumber

        function NSEvent_keyCode(event) bind(C, name="NSEvent_keyCode")
            import :: c_ptr, c_short
            type(c_ptr), value :: event
            integer(c_short) :: NSEvent_keyCode
        end function NSEvent_keyCode

        function NSEvent_modifierFlags(event) bind(C, name="NSEvent_modifierFlags")
            import :: c_ptr, c_long
            type(c_ptr), value :: event
            integer(c_long) :: NSEvent_modifierFlags
        end function NSEvent_modifierFlags

        ! String utilities
        function NSString_stringWithUTF8String(string) bind(C, name="NSString_stringWithUTF8String")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: string(*)
            type(c_ptr) :: NSString_stringWithUTF8String
        end function NSString_stringWithUTF8String

    end interface

    ! Global state
    type(NSApplication) :: g_app
    logical :: g_app_running = .false.

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

    !> @brief Cocoa platform implementation
    type, extends(forge_platform_base) :: forge_cocoa_platform
        private
        type(NSWindow) :: window
        type(NSView) :: view
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
        logical :: window_created = .false.
    contains
        procedure :: init => cocoa_init
        procedure :: shutdown => cocoa_shutdown
        procedure :: create_window => cocoa_create_window
        procedure :: destroy_window => cocoa_destroy_window
        procedure :: show_window => cocoa_show_window
        procedure :: hide_window => cocoa_hide_window
        procedure :: process_events => cocoa_process_events
        procedure :: get_surface => cocoa_get_surface
    end type forge_cocoa_platform

contains

    !> @brief Initialize Cocoa platform
    subroutine cocoa_init(this, status)
        class(forge_cocoa_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        write(output_unit, '(A)') "[Cocoa] Initializing Cocoa platform..."

        ! Get shared application
        g_app%ptr = NSApplication_sharedApplication()
        if (.not. c_associated(g_app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get NSApplication")
            return
        end if

        ! Set activation policy
        call NSApplication_setActivationPolicy(g_app%ptr, NSApplicationActivationPolicyRegular)

        this%initialized = .true.
        call status%clear()
        write(output_unit, '(A)') "[Cocoa] Platform initialized successfully"
    end subroutine cocoa_init

    !> @brief Shutdown Cocoa platform
    subroutine cocoa_shutdown(this)
        class(forge_cocoa_platform), intent(inout) :: this

        write(output_unit, '(A)') "[Cocoa] Shutting down Cocoa platform"

        ! Close window if it exists
        if (this%window_created) then
            call NSWindow_close(this%window%ptr)
            this%window_created = .false.
        end if

        this%initialized = .false.
    end subroutine cocoa_shutdown

    !> @brief Create a window
    subroutine cocoa_create_window(this, handle, title, width, height, status)
        class(forge_cocoa_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(NSRect) :: rect
        type(c_ptr) :: title_string
        integer(c_long) :: style_mask

        ! Create window
        this%window%ptr = NSWindow_alloc()
        if (.not. c_associated(this%window%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to allocate NSWindow")
            return
        end if

        ! Set window rect (origin at bottom-left)
        rect%x = 100.0_c_double
        rect%y = 100.0_c_double
        rect%width = real(width, c_double)
        rect%height = real(height, c_double)

        ! Window style
        style_mask = NSTitledWindowMask + NSClosableWindowMask + NSMiniaturizableWindowMask + NSResizableWindowMask

        ! Initialize window
        this%window%ptr = NSWindow_initWithContentRect(this%window%ptr, rect, style_mask, 2, 0)  ! NSBackingStoreBuffered
        if (.not. c_associated(this%window%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to initialize NSWindow")
            return
        end if

        ! Set title
        title_string = NSString_stringWithUTF8String(trim(title) // c_null_char)
        call NSWindow_setTitle(this%window%ptr, title_string)

        ! Create content view
        this%view%ptr = NSView_alloc()
        if (c_associated(this%view%ptr)) then
            this%view%ptr = NSView_initWithFrame(this%view%ptr, rect)
            call NSWindow_setContentView(this%window%ptr, this%view%ptr)
        end if

        this%width = width
        this%height = height
        this%window_created = .true.

        handle%native_handle = this%window%ptr
        handle%width = width
        handle%height = height

        call status%clear()
        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[Cocoa] Created window '", trim(title), "' (", width, "x", height, ")"
    end subroutine cocoa_create_window

    !> @brief Destroy a window
    subroutine cocoa_destroy_window(this, handle)
        class(forge_cocoa_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        if (this%window_created) then
            call NSWindow_close(this%window%ptr)
            this%window_created = .false.
            write(output_unit, '(A)') "[Cocoa] Window destroyed"
        end if
    end subroutine cocoa_destroy_window

    !> @brief Show a window
    subroutine cocoa_show_window(this, handle)
        class(forge_cocoa_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        if (this%window_created) then
            call NSWindow_makeKeyAndOrderFront(this%window%ptr, c_null_ptr)
            write(output_unit, '(A)') "[Cocoa] Window shown"
        end if
    end subroutine cocoa_show_window

    !> @brief Hide a window
    subroutine cocoa_hide_window(this, handle)
        class(forge_cocoa_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        if (this%window_created) then
            call NSWindow_orderOut(this%window%ptr, c_null_ptr)
            write(output_unit, '(A)') "[Cocoa] Window hidden"
        end if
    end subroutine cocoa_hide_window

    !> @brief Process pending events
    subroutine cocoa_process_events(this, should_quit)
        class(forge_cocoa_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        type(NSEvent) :: event
        integer(c_long) :: event_type

        should_quit = .false.

        ! Process one event at a time
        event%ptr = NSApplication_nextEventMatchingMask(g_app%ptr, -1_c_long, 0.0_c_double, c_null_ptr, 1)
        if (c_associated(event%ptr)) then
            event_type = NSEvent_type(event%ptr)

            select case (event_type)
            case (NSLeftMouseDown, NSRightMouseDown)
                call handle_mouse_event(event, .true.)
            case (NSLeftMouseUp, NSRightMouseUp)
                call handle_mouse_event(event, .false.)
            case (NSMouseMoved, NSLeftMouseDragged, NSRightMouseDragged)
                call handle_mouse_motion(event)
            case (NSKeyDown)
                call handle_key_event(event, .true.)
            case (NSKeyUp)
                call handle_key_event(event, .false.)
            case (NSFlagsChanged)
                call handle_modifier_change(event)
            end select

            ! Send event to application
            call NSApplication_sendEvent(g_app%ptr, event%ptr)
        end if
    end subroutine cocoa_process_events

    !> @brief Get drawing surface for window
    subroutine cocoa_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_cocoa_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status

        ! For Cocoa, we create a Cairo image surface
        ! In a real implementation, this would integrate with CoreGraphics/CGContext
        surface%surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, this%width, this%height)

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
    end subroutine cocoa_get_surface

    !> @brief Set render callback
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Handle mouse event
    subroutine handle_mouse_event(event, pressed)
        type(NSEvent), intent(in) :: event
        logical, intent(in) :: pressed
        type(NSPoint) :: location
        integer :: event_type, button

        location = NSEvent_locationInWindow(event%ptr)

        if (pressed) then
            event_type = EVENT_MOUSE_PRESSED
        else
            event_type = EVENT_MOUSE_RELEASED
        end if

        select case (NSEvent_type(event%ptr))
        case (NSLeftMouseDown, NSLeftMouseUp)
            button = MOUSE_BUTTON_LEFT
        case (NSRightMouseDown, NSRightMouseUp)
            button = MOUSE_BUTTON_RIGHT
        case default
            button = 0
        end select

        call update_mouse_state(g_mouse_state, int(location%x), int(location%y))
        call dispatch_mouse_event(c_null_ptr, event_type, int(location%x), int(location%y), button)
    end subroutine handle_mouse_event

    !> @brief Handle mouse motion event
    subroutine handle_mouse_motion(event)
        type(NSEvent), intent(in) :: event
        type(NSPoint) :: location

        location = NSEvent_locationInWindow(event%ptr)

        call update_mouse_state(g_mouse_state, int(location%x), int(location%y))
        g_mouse_state%over_window = .true.
        call dispatch_mouse_event(c_null_ptr, EVENT_MOUSE_MOVED, int(location%x), int(location%y), 0)
    end subroutine handle_mouse_motion

    !> @brief Handle keyboard event
    subroutine handle_key_event(event, pressed)
        type(NSEvent), intent(in) :: event
        logical, intent(in) :: pressed
        integer :: event_type
        integer(c_short) :: key_code

        if (pressed) then
            event_type = EVENT_KEY_PRESSED
        else
            event_type = EVENT_KEY_RELEASED
        end if

        key_code = NSEvent_keyCode(event%ptr)
        call dispatch_keyboard_event(c_null_ptr, event_type, int(key_code))
    end subroutine handle_key_event

    !> @brief Handle modifier key change
    subroutine handle_modifier_change(event)
        type(NSEvent), intent(in) :: event
        integer(c_long) :: modifiers

        modifiers = NSEvent_modifierFlags(event%ptr)

        g_keyboard_state%shift_pressed = (iand(modifiers, NSShiftKeyMask) /= 0)
        g_keyboard_state%ctrl_pressed = (iand(modifiers, NSControlKeyMask) /= 0)
        g_keyboard_state%alt_pressed = (iand(modifiers, NSAlternateKeyMask) /= 0)
    end subroutine handle_modifier_change

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

        write(output_unit, '(A,I0,A,I0,A,I0,A,I0)') "[Cocoa] Mouse event: ", &
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

        write(output_unit, '(A,I0,A,I0)') "[Cocoa] Keyboard event: ", &
            event_type, " key=", key_code
    end subroutine dispatch_keyboard_event

end module forge_platform_cocoa