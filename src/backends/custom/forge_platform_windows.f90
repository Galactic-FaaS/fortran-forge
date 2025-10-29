!> @brief Windows platform implementation using Win32 API
!> @details Provides windowing and event handling for Windows
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_windows
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    implicit none
    private

    public :: forge_windows_platform
    public :: set_window_render_callback

    !> Win32 API constants
    integer(c_int), parameter :: WS_OVERLAPPEDWINDOW = int(z'00CF0000', c_int)
    integer(c_int), parameter :: WS_VISIBLE = int(z'10000000', c_int)
    integer(c_int), parameter :: SW_SHOW = 5
    integer(c_int), parameter :: SW_HIDE = 0
    integer(c_int), parameter :: WM_QUIT = int(z'0012', c_int)
    integer(c_int), parameter :: WM_DESTROY = int(z'0002', c_int)
    integer(c_int), parameter :: WM_CLOSE = int(z'0010', c_int)
    integer(c_int), parameter :: WM_PAINT = int(z'000F', c_int)
    integer(c_int), parameter :: WM_MOUSEMOVE = int(z'0200', c_int)
    integer(c_int), parameter :: WM_LBUTTONDOWN = int(z'0201', c_int)
    integer(c_int), parameter :: WM_LBUTTONUP = int(z'0202', c_int)
    integer(c_int), parameter :: WM_RBUTTONDOWN = int(z'0204', c_int)
    integer(c_int), parameter :: WM_RBUTTONUP = int(z'0205', c_int)
    integer(c_int), parameter :: WM_KEYDOWN = int(z'0100', c_int)
    integer(c_int), parameter :: WM_KEYUP = int(z'0101', c_int)
    integer(c_int), parameter :: WM_CHAR = int(z'0102', c_int)
    integer(c_int), parameter :: PM_REMOVE = 1
    
    ! Mouse state tracking
    type(forge_mouse_state), save :: g_mouse_state
    type(forge_keyboard_state), save :: g_keyboard_state
    
    ! Callback for rendering
    abstract interface
        subroutine render_callback_interface(window_handle, context)
            import :: c_ptr
            type(c_ptr), intent(in) :: window_handle, context
        end subroutine render_callback_interface
    end interface
    
    procedure(render_callback_interface), pointer, save :: g_render_callback => null()

    !> WNDCLASSEX structure
    type, bind(C) :: WNDCLASSEXA
        integer(c_int) :: cbSize
        integer(c_int) :: style
        type(c_funptr) :: lpfnWndProc
        integer(c_int) :: cbClsExtra
        integer(c_int) :: cbWndExtra
        type(c_ptr) :: hInstance
        type(c_ptr) :: hIcon
        type(c_ptr) :: hCursor
        type(c_ptr) :: hbrBackground
        type(c_ptr) :: lpszMenuName
        type(c_ptr) :: lpszClassName
        type(c_ptr) :: hIconSm
    end type WNDCLASSEXA

    !> MSG structure
    type, bind(C) :: MSG
        type(c_ptr) :: hwnd
        integer(c_int) :: message
        type(c_ptr) :: wParam
        type(c_ptr) :: lParam
        integer(c_int) :: time
        integer(c_int) :: pt_x
        integer(c_int) :: pt_y
    end type MSG

    !> Win32 API bindings
    interface
        
        ! GDI functions for rendering
        function GetDC(hWnd) bind(C, name="GetDC")
            import :: c_ptr
            type(c_ptr), value :: hWnd
            type(c_ptr) :: GetDC
        end function GetDC

        function ReleaseDC(hWnd, hDC) bind(C, name="ReleaseDC")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd, hDC
            integer(c_int) :: ReleaseDC
        end function ReleaseDC

        function BeginPaint(hWnd, lpPaint) bind(C, name="BeginPaint")
            import :: c_ptr
            type(c_ptr), value :: hWnd, lpPaint
            type(c_ptr) :: BeginPaint
        end function BeginPaint

        function EndPaint(hWnd, lpPaint) bind(C, name="EndPaint")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd, lpPaint
            integer(c_int) :: EndPaint
        end function EndPaint

        function InvalidateRect(hWnd, lpRect, bErase) bind(C, name="InvalidateRect")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd, lpRect
            integer(c_int), value :: bErase
            integer(c_int) :: InvalidateRect
        end function InvalidateRect

        ! Window class registration
        function RegisterClassExA(wndclass) bind(C, name="RegisterClassExA")
            import :: WNDCLASSEXA, c_short
            type(WNDCLASSEXA), intent(in) :: wndclass
            integer(c_short) :: RegisterClassExA
        end function RegisterClassExA

        ! Window creation
        function CreateWindowExA(dwExStyle, lpClassName, lpWindowName, dwStyle, &
                                x, y, nWidth, nHeight, hWndParent, hMenu, &
                                hInstance, lpParam) bind(C, name="CreateWindowExA")
            import :: c_int, c_ptr
            integer(c_int), value :: dwExStyle
            type(c_ptr), value :: lpClassName
            type(c_ptr), value :: lpWindowName
            integer(c_int), value :: dwStyle
            integer(c_int), value :: x, y, nWidth, nHeight
            type(c_ptr), value :: hWndParent, hMenu, hInstance, lpParam
            type(c_ptr) :: CreateWindowExA
        end function CreateWindowExA

        ! Window destruction
        function DestroyWindow(hWnd) bind(C, name="DestroyWindow")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd
            integer(c_int) :: DestroyWindow
        end function DestroyWindow

        ! Show/hide window
        function ShowWindow(hWnd, nCmdShow) bind(C, name="ShowWindow")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd
            integer(c_int), value :: nCmdShow
            integer(c_int) :: ShowWindow
        end function ShowWindow

        ! Message loop
        function PeekMessageA(lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax, wRemoveMsg) &
                bind(C, name="PeekMessageA")
            import :: MSG, c_ptr, c_int
            type(MSG), intent(out) :: lpMsg
            type(c_ptr), value :: hWnd
            integer(c_int), value :: wMsgFilterMin, wMsgFilterMax, wRemoveMsg
            integer(c_int) :: PeekMessageA
        end function PeekMessageA

        function TranslateMessage(lpMsg) bind(C, name="TranslateMessage")
            import :: MSG, c_int
            type(MSG), intent(in) :: lpMsg
            integer(c_int) :: TranslateMessage
        end function TranslateMessage

        function DispatchMessageA(lpMsg) bind(C, name="DispatchMessageA")
            import :: MSG, c_ptr
            type(MSG), intent(in) :: lpMsg
            type(c_ptr) :: DispatchMessageA
        end function DispatchMessageA

        ! Default window procedure
        function DefWindowProcA(hWnd, Msg, wParam, lParam) bind(C, name="DefWindowProcA")
            import :: c_ptr, c_int
            type(c_ptr), value :: hWnd, wParam, lParam
            integer(c_int), value :: Msg
            type(c_ptr) :: DefWindowProcA
        end function DefWindowProcA

        ! Get module handle
        function GetModuleHandleA(lpModuleName) bind(C, name="GetModuleHandleA")
            import :: c_ptr
            type(c_ptr), value :: lpModuleName
            type(c_ptr) :: GetModuleHandleA
        end function GetModuleHandleA

        ! Post quit message
        subroutine PostQuitMessage(nExitCode) bind(C, name="PostQuitMessage")
            import :: c_int
            integer(c_int), value :: nExitCode
        end subroutine PostQuitMessage

    end interface

    !> @brief Windows platform implementation
    type, extends(forge_platform_base) :: forge_windows_platform
        private
        type(c_ptr) :: hInstance = c_null_ptr
        character(len=256) :: window_class_name = ""
        logical :: class_registered = .false.
    contains
        procedure :: init => windows_init
        procedure :: shutdown => windows_shutdown
        procedure :: create_window => windows_create_window
        procedure :: destroy_window => windows_destroy_window
        procedure :: show_window => windows_show_window
        procedure :: hide_window => windows_hide_window
        procedure :: process_events => windows_process_events
        procedure :: get_surface => windows_get_surface
    end type forge_windows_platform

contains

    !> @brief Initialize Windows platform
    subroutine windows_init(this, status)
        class(forge_windows_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status
        type(WNDCLASSEXA) :: wc
        integer(c_short) :: class_atom

        write(output_unit, '(A)') "[WIN32] Initializing Windows platform..."

        ! Get module handle
        this%hInstance = GetModuleHandleA(c_null_ptr)
        if (.not. c_associated(this%hInstance)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get module handle")
            return
        end if

        ! Register window class
        this%window_class_name = "ForGE_Window_Class" // c_null_char
        
        wc%cbSize = sizeof(wc)
        wc%style = 0
        wc%lpfnWndProc = c_funloc(window_procedure)
        wc%cbClsExtra = 0
        wc%cbWndExtra = 0
        wc%hInstance = this%hInstance
        wc%hIcon = c_null_ptr
        wc%hCursor = c_null_ptr
        wc%hbrBackground = c_null_ptr
        wc%lpszMenuName = c_null_ptr
        wc%lpszClassName = c_loc(this%window_class_name)
        wc%hIconSm = c_null_ptr

        class_atom = RegisterClassExA(wc)
        if (class_atom == 0) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to register window class")
            return
        end if

        this%class_registered = .true.
        this%initialized = .true.
        call status%clear()

        write(output_unit, '(A)') "[WIN32] Platform initialized successfully"
    end subroutine windows_init

    !> @brief Shutdown Windows platform
    subroutine windows_shutdown(this)
        class(forge_windows_platform), intent(inout) :: this
        
        write(output_unit, '(A)') "[WIN32] Shutting down Windows platform"
        this%initialized = .false.
        this%class_registered = .false.
    end subroutine windows_shutdown

    !> @brief Create a window
    subroutine windows_create_window(this, handle, title, width, height, status)
        class(forge_windows_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(c_ptr) :: hwnd
        character(len=:), allocatable :: title_c

        ! Null-terminate title
        title_c = trim(title) // c_null_char

        ! Create window
        hwnd = CreateWindowExA( &
            0, &  ! dwExStyle
            c_loc(this%window_class_name), &
            c_loc(title_c), &
            WS_OVERLAPPEDWINDOW, &
            100, 100, &  ! x, y (CW_USEDEFAULT would be better)
            width, height, &
            c_null_ptr, &  ! parent
            c_null_ptr, &  ! menu
            this%hInstance, &
            c_null_ptr)    ! lpParam

        if (.not. c_associated(hwnd)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create window")
            return
        end if

        handle%native_handle = hwnd
        handle%width = width
        handle%height = height
        call status%clear()

        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[WIN32] Created window '", trim(title), "' (", width, "x", height, ")"
    end subroutine windows_create_window

    !> @brief Destroy a window
    subroutine windows_destroy_window(this, handle)
        class(forge_windows_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            result = DestroyWindow(handle%native_handle)
            write(output_unit, '(A)') "[WIN32] Window destroyed"
        end if
    end subroutine windows_destroy_window

    !> @brief Show a window
    subroutine windows_show_window(this, handle)
        class(forge_windows_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            result = ShowWindow(handle%native_handle, SW_SHOW)
            write(output_unit, '(A)') "[WIN32] Window shown"
        end if
    end subroutine windows_show_window

    !> @brief Hide a window
    subroutine windows_hide_window(this, handle)
        class(forge_windows_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle
        integer(c_int) :: result

        if (c_associated(handle%native_handle)) then
            result = ShowWindow(handle%native_handle, SW_HIDE)
            write(output_unit, '(A)') "[WIN32] Window hidden"
        end if
    end subroutine windows_hide_window

    !> @brief Process pending events
    subroutine windows_process_events(this, should_quit)
        class(forge_windows_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        type(MSG) :: msg
        integer(c_int) :: result, trans

        should_quit = .false.

        ! Process all pending messages
        do while (PeekMessageA(msg, c_null_ptr, 0, 0, PM_REMOVE) /= 0)
            if (msg%message == WM_QUIT) then
                should_quit = .true.
                write(output_unit, '(A)') "[WIN32] Quit message received"
                exit
            end if

            trans = TranslateMessage(msg)
            result = c_associated(DispatchMessageA(msg))
        end do
    end subroutine windows_process_events

    !> @brief Get drawing surface for window
    subroutine windows_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_windows_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status
        type(c_ptr) :: hdc

        if (.not. c_associated(window%native_handle)) then
            call status%set(FORGE_ERROR_NULL_PTR, "Invalid window handle")
            return
        end if

        ! Get device context
        hdc = GetDC(window%native_handle)
        if (.not. c_associated(hdc)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get device context")
            return
        end if

        ! Create Cairo surface from DC
        surface%surface = cairo_win32_surface_create(hdc)
        if (.not. c_associated(surface%surface)) then
            ! Release DC if surface creation failed
            call ReleaseDC(window%native_handle, hdc)
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo surface")
            return
        end if

        ! Create Cairo context
        surface%context = cairo_create(surface%surface)
        if (.not. c_associated(surface%context)) then
            call cairo_surface_destroy(surface%surface)
            call ReleaseDC(window%native_handle, hdc)
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Cairo context")
            return
        end if

        call status%clear()
    end subroutine windows_get_surface

    !> @brief Set render callback for WM_PAINT
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Window procedure callback
    function window_procedure(hWnd, uMsg, wParam, lParam) bind(C) result(lResult)
        use forge_cairo_bindings
        type(c_ptr), value :: hWnd, wParam, lParam
        integer(c_int), value :: uMsg
        type(c_ptr) :: lResult
        type(c_ptr) :: hdc, cairo_surface, cairo_ctx
        integer(c_int) :: x, y
        integer(c_int) :: mouse_x, mouse_y

        select case (uMsg)
        
        case (WM_DESTROY, WM_CLOSE)
            ! Window closing
            call PostQuitMessage(0)
            lResult = c_null_ptr
            return

        case (WM_PAINT)
            ! Repaint window
            if (associated(g_render_callback)) then
                ! Get DC and create Cairo context
                hdc = GetDC(hWnd)
                if (c_associated(hdc)) then
                    cairo_surface = cairo_win32_surface_create(hdc)
                    if (c_associated(cairo_surface)) then
                        cairo_ctx = cairo_create(cairo_surface)
                        if (c_associated(cairo_ctx)) then
                            ! Call user render callback
                            call g_render_callback(hWnd, cairo_ctx)
                            
                            ! Cleanup
                            call cairo_surface_flush(cairo_surface)
                            call cairo_destroy(cairo_ctx)
                        end if
                        call cairo_surface_destroy(cairo_surface)
                    end if
                    call ReleaseDC(hWnd, hdc)
                end if
            end if
            lResult = c_null_ptr
            return

        case (WM_MOUSEMOVE)
            ! Extract mouse coordinates from lParam
            mouse_x = iand(transfer(lParam, 0_c_int), int(z'0000FFFF', c_int))
            mouse_y = ishft(transfer(lParam, 0_c_int), -16)
            call update_mouse_state(g_mouse_state, mouse_x, mouse_y)
            g_mouse_state%over_window = .true.
            ! Dispatch mouse move event
            call dispatch_mouse_event(hWnd, EVENT_MOUSE_MOVED, mouse_x, mouse_y, 0)
            lResult = c_null_ptr
            return

        case (WM_LBUTTONDOWN)
            mouse_x = iand(transfer(lParam, 0_c_int), int(z'0000FFFF', c_int))
            mouse_y = ishft(transfer(lParam, 0_c_int), -16)
            call update_mouse_state(g_mouse_state, mouse_x, mouse_y, left_down=.true.)
            write(output_unit, '(A,I0,A,I0)') "[WIN32] Left button down at (", mouse_x, ", ", mouse_y, ")"
            ! Dispatch mouse button event
            call dispatch_mouse_event(hWnd, EVENT_MOUSE_PRESSED, mouse_x, mouse_y, MOUSE_BUTTON_LEFT)
            lResult = c_null_ptr
            return

        case (WM_LBUTTONUP)
            mouse_x = iand(transfer(lParam, 0_c_int), int(z'0000FFFF', c_int))
            mouse_y = ishft(transfer(lParam, 0_c_int), -16)
            call update_mouse_state(g_mouse_state, mouse_x, mouse_y, left_down=.false.)
            write(output_unit, '(A,I0,A,I0)') "[WIN32] Left button up at (", mouse_x, ", ", mouse_y, ")"
            ! Dispatch mouse button event
            call dispatch_mouse_event(hWnd, EVENT_MOUSE_RELEASED, mouse_x, mouse_y, MOUSE_BUTTON_LEFT)
            lResult = c_null_ptr
            return

        case (WM_RBUTTONDOWN)
            mouse_x = iand(transfer(lParam, 0_c_int), int(z'0000FFFF', c_int))
            mouse_y = ishft(transfer(lParam, 0_c_int), -16)
            call update_mouse_state(g_mouse_state, mouse_x, mouse_y, right_down=.true.)
            write(output_unit, '(A,I0,A,I0)') "[WIN32] Right button down at (", mouse_x, ", ", mouse_y, ")"
            ! Dispatch mouse button event
            call dispatch_mouse_event(hWnd, EVENT_MOUSE_PRESSED, mouse_x, mouse_y, MOUSE_BUTTON_RIGHT)
            lResult = c_null_ptr
            return

        case (WM_RBUTTONUP)
            mouse_x = iand(transfer(lParam, 0_c_int), int(z'0000FFFF', c_int))
            mouse_y = ishft(transfer(lParam, 0_c_int), -16)
            call update_mouse_state(g_mouse_state, mouse_x, mouse_y, right_down=.false.)
            write(output_unit, '(A,I0,A,I0)') "[WIN32] Right button up at (", mouse_x, ", ", mouse_y, ")"
            ! Dispatch mouse button event
            call dispatch_mouse_event(hWnd, EVENT_MOUSE_RELEASED, mouse_x, mouse_y, MOUSE_BUTTON_RIGHT)
            lResult = c_null_ptr
            return

        case (WM_KEYDOWN)
            write(output_unit, '(A,I0)') "[WIN32] Key down: ", transfer(wParam, 0_c_int)
            ! Dispatch keyboard event
            call dispatch_keyboard_event(hWnd, EVENT_KEY_PRESSED, transfer(wParam, 0_c_int))
            lResult = c_null_ptr
            return

        case (WM_CHAR)
            write(output_unit, '(A,I0)') "[WIN32] Char input: ", transfer(wParam, 0_c_int)
            ! Dispatch character event
            call dispatch_character_event(hWnd, EVENT_TEXT_CHANGED, transfer(wParam, 0_c_int))
    !> @brief Dispatch mouse event
    subroutine dispatch_mouse_event(hWnd, event_type, x, y, button)
        type(c_ptr), intent(in) :: hWnd
        integer, intent(in) :: event_type, x, y, button
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = hWnd
        event%mouse_x = x
        event%mouse_y = y
        event%mouse_button = button

        ! Route event to appropriate widget/event handler
        ! For now, just log the event (full routing would require widget hierarchy)
        write(output_unit, '(A,I0,A,I0,A,I0)') "[WIN32] Mouse event: ", event_type, " at (", x, ",", y, ") button=", button
    end subroutine dispatch_mouse_event

    !> @brief Dispatch keyboard event
    subroutine dispatch_keyboard_event(hWnd, event_type, key_code)
        type(c_ptr), intent(in) :: hWnd
        integer, intent(in) :: event_type, key_code
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = hWnd
        event%key_code = key_code

        ! Route event to appropriate widget/event handler
        ! For now, just log the event (full routing would require widget hierarchy)
        write(output_unit, '(A,I0,A,I0)') "[WIN32] Keyboard event: ", event_type, " key=", key_code
    end subroutine dispatch_keyboard_event

    !> @brief Dispatch character event
    subroutine dispatch_character_event(hWnd, event_type, char_code)
        type(c_ptr), intent(in) :: hWnd
        integer, intent(in) :: event_type, char_code
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = hWnd
        event%key_char = achar(char_code)

        ! Route event to appropriate widget/event handler
        ! For now, just log the event (full routing would require widget hierarchy)
        write(output_unit, '(A,I0,A,A)') "[WIN32] Character event: ", event_type, " char='", achar(char_code), "'"
    end subroutine dispatch_character_event

end module forge_platform_windows
            lResult = c_null_ptr
            return

        end select

        ! Default handling for unhandled messages
        lResult = DefWindowProcA(hWnd, uMsg, wParam, lParam)
    end function window_procedure

end module forge_platform_windows

