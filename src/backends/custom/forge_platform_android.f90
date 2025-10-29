!> @brief Android platform implementation using Android NDK and JNI
!> @details Provides windowing and event handling for Android with Activity/Fragment integration
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_android
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    use forge_events
    implicit none
    private

    public :: forge_android_platform
    public :: set_window_render_callback

    ! Android constants
    integer(c_int), parameter :: ANDROID_APP_CMD_INIT_WINDOW = 0
    integer(c_int), parameter :: ANDROID_APP_CMD_TERM_WINDOW = 1
    integer(c_int), parameter :: ANDROID_APP_CMD_WINDOW_RESIZED = 2
    integer(c_int), parameter :: ANDROID_APP_CMD_WINDOW_REDRAW_NEEDED = 3
    integer(c_int), parameter :: ANDROID_APP_CMD_CONTENT_RECT_CHANGED = 4
    integer(c_int), parameter :: ANDROID_APP_CMD_GAINED_FOCUS = 5
    integer(c_int), parameter :: ANDROID_APP_CMD_LOST_FOCUS = 6
    integer(c_int), parameter :: ANDROID_APP_CMD_CONFIG_CHANGED = 7
    integer(c_int), parameter :: ANDROID_APP_CMD_LOW_MEMORY = 8
    integer(c_int), parameter :: ANDROID_APP_CMD_START = 9
    integer(c_int), parameter :: ANDROID_APP_CMD_RESUME = 10
    integer(c_int), parameter :: ANDROID_APP_CMD_SAVE_STATE = 11
    integer(c_int), parameter :: ANDROID_APP_CMD_PAUSE = 12
    integer(c_int), parameter :: ANDROID_APP_CMD_STOP = 13
    integer(c_int), parameter :: ANDROID_APP_CMD_DESTROY = 14
    integer(c_int), parameter :: ANDROID_APP_CMD_WINDOW_INSETS_CHANGED = 15

    ! Android motion event actions
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_DOWN = 0
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_UP = 1
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_MOVE = 2
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_CANCEL = 3
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_OUTSIDE = 4
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_POINTER_DOWN = 5
    integer(c_int), parameter :: AMOTION_EVENT_ACTION_POINTER_UP = 6

    ! Android key event actions
    integer(c_int), parameter :: AKEY_EVENT_ACTION_DOWN = 0
    integer(c_int), parameter :: AKEY_EVENT_ACTION_UP = 1
    integer(c_int), parameter :: AKEY_EVENT_ACTION_MULTIPLE = 2

    ! Android key codes
    integer(c_int), parameter :: AKEYCODE_BACK = 4
    integer(c_int), parameter :: AKEYCODE_ENTER = 66

    ! Android structures (simplified for Fortran binding)
    type :: android_app
        type(c_ptr) :: userData = c_null_ptr
        type(c_ptr) :: activity = c_null_ptr
        type(c_ptr) :: config = c_null_ptr
        type(c_ptr) :: savedState = c_null_ptr
        integer(c_size_t) :: savedStateSize = 0
        type(c_ptr) :: looper = c_null_ptr
        type(c_ptr) :: inputQueue = c_null_ptr
        type(c_ptr) :: window = c_null_ptr
        type(c_ptr) :: contentRect = c_null_ptr
        integer(c_int) :: activityState = 0
        integer(c_int) :: destroyRequested = 0
        type(c_ptr) :: onAppCmd = c_null_ptr
        type(c_ptr) :: onInputEvent = c_null_ptr
        type(c_ptr) :: activityClass = c_null_ptr
    end type android_app

    type :: AInputEvent
        type(c_ptr) :: ptr = c_null_ptr
    end type AInputEvent

    type :: ARect
        integer(c_int) :: left = 0
        integer(c_int) :: top = 0
        integer(c_int) :: right = 0
        integer(c_int) :: bottom = 0
    end type ARect

    ! Android NDK API bindings
    interface

        ! App management
        function android_app_create() bind(C, name="android_app_create")
            import :: c_ptr
            type(c_ptr) :: android_app_create
        end function android_app_create

        subroutine android_app_destroy(app) bind(C, name="android_app_destroy")
            import :: c_ptr
            type(c_ptr), value :: app
        end subroutine android_app_destroy

        function android_app_read_cmd(app) bind(C, name="android_app_read_cmd")
            import :: c_ptr, c_int
            type(c_ptr), value :: app
            integer(c_int) :: android_app_read_cmd
        end function android_app_read_cmd

        subroutine android_app_pre_exec_cmd(app, cmd) bind(C, name="android_app_pre_exec_cmd")
            import :: c_ptr, c_int
            type(c_ptr), value :: app
            integer(c_int), value :: cmd
        end subroutine android_app_pre_exec_cmd

        subroutine android_app_post_exec_cmd(app, cmd) bind(C, name="android_app_post_exec_cmd")
            import :: c_ptr, c_int
            type(c_ptr), value :: app
            integer(c_int), value :: cmd
        end subroutine android_app_post_exec_cmd

        ! Input event handling
        function AInputQueue_getEvent(queue, event) bind(C, name="AInputQueue_getEvent")
            import :: c_ptr, c_int
            type(c_ptr), value :: queue
            type(c_ptr), intent(out) :: event
            integer(c_int) :: AInputQueue_getEvent
        end function AInputQueue_getEvent

        function AInputQueue_preDispatchEvent(queue, event) bind(C, name="AInputQueue_preDispatchEvent")
            import :: c_ptr, c_int
            type(c_ptr), value :: queue
            type(c_ptr), value :: event
            integer(c_int) :: AInputQueue_preDispatchEvent
        end function AInputQueue_preDispatchEvent

        subroutine AInputQueue_finishEvent(queue, event, handled) bind(C, name="AInputQueue_finishEvent")
            import :: c_ptr, c_int
            type(c_ptr), value :: queue
            type(c_ptr), value :: event
            integer(c_int), value :: handled
        end subroutine AInputQueue_finishEvent

        function AInputEvent_getType(event) bind(C, name="AInputEvent_getType")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AInputEvent_getType
        end function AInputEvent_getType

        function AInputEvent_getSource(event) bind(C, name="AInputEvent_getSource")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AInputEvent_getSource
        end function AInputEvent_getSource

        ! Motion events
        function AMotionEvent_getAction(event) bind(C, name="AMotionEvent_getAction")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AMotionEvent_getAction
        end function AMotionEvent_getAction

        function AMotionEvent_getPointerCount(event) bind(C, name="AMotionEvent_getPointerCount")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AMotionEvent_getPointerCount
        end function AMotionEvent_getPointerCount

        function AMotionEvent_getPointerId(event, pointerIndex) bind(C, name="AMotionEvent_getPointerId")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int), value :: pointerIndex
            integer(c_int) :: AMotionEvent_getPointerId
        end function AMotionEvent_getPointerId

        function AMotionEvent_getX(event, pointerIndex) bind(C, name="AMotionEvent_getX")
            import :: c_ptr, c_float
            type(c_ptr), value :: event
            integer(c_int), value :: pointerIndex
            real(c_float) :: AMotionEvent_getX
        end function AMotionEvent_getX

        function AMotionEvent_getY(event, pointerIndex) bind(C, name="AMotionEvent_getY")
            import :: c_ptr, c_float
            type(c_ptr), value :: event
            integer(c_int), value :: pointerIndex
            real(c_float) :: AMotionEvent_getY
        end function AMotionEvent_getY

        function AMotionEvent_getPressure(event, pointerIndex) bind(C, name="AMotionEvent_getPressure")
            import :: c_ptr, c_float
            type(c_ptr), value :: event
            integer(c_int), value :: pointerIndex
            real(c_float) :: AMotionEvent_getPressure
        end function AMotionEvent_getPressure

        ! Key events
        function AKeyEvent_getAction(event) bind(C, name="AKeyEvent_getAction")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AKeyEvent_getAction
        end function AKeyEvent_getAction

        function AKeyEvent_getKeyCode(event) bind(C, name="AKeyEvent_getKeyCode")
            import :: c_ptr, c_int
            type(c_ptr), value :: event
            integer(c_int) :: AKeyEvent_getKeyCode
        end function AKeyEvent_getKeyCode

        ! Window management
        function ANativeWindow_getWidth(window) bind(C, name="ANativeWindow_getWidth")
            import :: c_ptr, c_int
            type(c_ptr), value :: window
            integer(c_int) :: ANativeWindow_getWidth
        end function ANativeWindow_getWidth

        function ANativeWindow_getHeight(window) bind(C, name="ANativeWindow_getHeight")
            import :: c_ptr, c_int
            type(c_ptr), value :: window
            integer(c_int) :: ANativeWindow_getHeight
        end function ANativeWindow_getHeight

        ! JNI for Activity integration
        function JNI_GetActivityClass() bind(C, name="JNI_GetActivityClass")
            import :: c_ptr
            type(c_ptr) :: JNI_GetActivityClass
        end function JNI_GetActivityClass

        subroutine JNI_CallVoidMethod(obj, method, args) bind(C, name="JNI_CallVoidMethod")
            import :: c_ptr
            type(c_ptr), value :: obj, method
            type(c_ptr), value :: args
        end subroutine JNI_CallVoidMethod

    end interface

    ! Global state
    type(android_app) :: g_android_app
    type(forge_touch_state), save :: g_touch_state
    type(forge_keyboard_state), save :: g_keyboard_state

    ! Render callback
    abstract interface
        subroutine render_callback_interface(window_handle, context)
            import :: c_ptr
            type(c_ptr), intent(in) :: window_handle, context
        end subroutine render_callback_interface
    end interface

    procedure(render_callback_interface), pointer, save :: g_render_callback => null()

    !> @brief Android platform implementation
    type, extends(forge_platform_base) :: forge_android_platform
        private
        type(android_app) :: app
        logical :: window_created = .false.
        integer(c_int) :: window_width = 0
        integer(c_int) :: window_height = 0
    contains
        procedure :: init => android_init
        procedure :: shutdown => android_shutdown
        procedure :: create_window => android_create_window
        procedure :: destroy_window => android_destroy_window
        procedure :: show_window => android_show_window
        procedure :: hide_window => android_hide_window
        procedure :: process_events => android_process_events
        procedure :: get_surface => android_get_surface
        procedure :: get_lifecycle => android_get_lifecycle
        procedure :: get_orientation => android_get_orientation
        procedure :: set_orientation => android_set_orientation
        procedure :: handle_back_button => android_handle_back_button
        procedure :: show_status_bar => android_show_status_bar
        procedure :: hide_status_bar => android_hide_status_bar
    end type forge_android_platform

contains

    !> @brief Initialize Android platform
    subroutine android_init(this, status)
        class(forge_android_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        write(output_unit, '(A)') "[Android] Initializing Android platform..."

        ! Create Android app instance
        this%app%ptr = android_app_create()
        if (.not. c_associated(this%app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Android app")
            return
        end if

        ! Set up lifecycle
        this%lifecycle%current_state = LIFECYCLE_CREATED
        this%lifecycle%is_background = .false.
        this%lifecycle%is_foreground = .true.

        ! Set up default orientation
        this%orientation%current_orientation = ORIENTATION_PORTRAIT
        this%orientation%screen_width = 0
        this%orientation%screen_height = 0

        this%initialized = .true.
        call status%clear()
        write(output_unit, '(A)') "[Android] Platform initialized successfully"
    end subroutine android_init

    !> @brief Shutdown Android platform
    subroutine android_shutdown(this)
        class(forge_android_platform), intent(inout) :: this

        write(output_unit, '(A)') "[Android] Shutting down Android platform"

        if (c_associated(this%app%ptr)) then
            call android_app_destroy(this%app%ptr)
            this%app%ptr = c_null_ptr
        end if

        this%initialized = .false.
    end subroutine android_shutdown

    !> @brief Create a window (Android Activity)
    subroutine android_create_window(this, handle, title, width, height, status)
        class(forge_android_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status

        ! For Android, the window is managed by the Activity
        ! We just store the dimensions
        this%window_width = width
        this%window_height = height
        this%window_created = .true.

        handle%native_handle = this%app%ptr  ! Use app as window handle
        handle%width = width
        handle%height = height

        call status%clear()
        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[Android] Created window '", trim(title), "' (", width, "x", height, ")"
    end subroutine android_create_window

    !> @brief Destroy a window
    subroutine android_destroy_window(this, handle)
        class(forge_android_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        this%window_created = .false.
        write(output_unit, '(A)') "[Android] Window destroyed"
    end subroutine android_destroy_window

    !> @brief Show a window (bring Activity to front)
    subroutine android_show_window(this, handle)
        class(forge_android_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        ! For Android, this is handled by the Activity lifecycle
        write(output_unit, '(A)') "[Android] Window shown"
    end subroutine android_show_window

    !> @brief Hide a window
    subroutine android_hide_window(this, handle)
        class(forge_android_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        ! For Android, this is handled by the Activity lifecycle
        write(output_unit, '(A)') "[Android] Window hidden"
    end subroutine android_hide_window

    !> @brief Process pending events
    subroutine android_process_events(this, should_quit)
        class(forge_android_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        integer(c_int) :: cmd, event_result
        type(c_ptr) :: event

        should_quit = .false.

        ! Process app commands
        cmd = android_app_read_cmd(this%app%ptr)
        if (cmd >= 0) then
            call android_app_pre_exec_cmd(this%app%ptr, cmd)

            select case (cmd)
            case (ANDROID_APP_CMD_INIT_WINDOW)
                call handle_cmd_init_window(this)
            case (ANDROID_APP_CMD_TERM_WINDOW)
                call handle_cmd_term_window(this)
            case (ANDROID_APP_CMD_WINDOW_RESIZED)
                call handle_cmd_window_resized(this)
            case (ANDROID_APP_CMD_START)
                this%lifecycle%current_state = LIFECYCLE_STARTED
                call dispatch_app_event(EVENT_APP_FOREGROUND)
            case (ANDROID_APP_CMD_RESUME)
                this%lifecycle%current_state = LIFECYCLE_RESUMED
                this%lifecycle%is_background = .false.
                this%lifecycle%is_foreground = .true.
                call dispatch_app_event(EVENT_APP_FOREGROUND)
            case (ANDROID_APP_CMD_PAUSE)
                this%lifecycle%current_state = LIFECYCLE_PAUSED
                this%lifecycle%is_background = .true.
                this%lifecycle%is_foreground = .false.
                call dispatch_app_event(EVENT_APP_BACKGROUND)
            case (ANDROID_APP_CMD_STOP)
                this%lifecycle%current_state = LIFECYCLE_STOPPED
            case (ANDROID_APP_CMD_DESTROY)
                this%lifecycle%current_state = LIFECYCLE_DESTROYED
                should_quit = .true.
            case (ANDROID_APP_CMD_CONFIG_CHANGED)
                call handle_cmd_config_changed(this)
            end select

            call android_app_post_exec_cmd(this%app%ptr, cmd)
        end if

        ! Process input events
        if (c_associated(this%app%inputQueue)) then
            event_result = AInputQueue_getEvent(this%app%inputQueue, event)
            if (event_result >= 0) then
                if (AInputQueue_preDispatchEvent(this%app%inputQueue, event) == 0) then
                    call process_input_event(event)
                    call AInputQueue_finishEvent(this%app%inputQueue, event, 1)
                end if
            end if
        end if
    end subroutine android_process_events

    !> @brief Get drawing surface for window
    subroutine android_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_android_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status

        if (.not. this%window_created .or. .not. c_associated(this%app%window)) then
            call status%set(FORGE_ERROR_NULL_PTR, "Android window not available")
            return
        end if

        ! Create Cairo surface for Android native window
        ! This would integrate with Android's native window through NDK
        surface%surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, &
                                                   this%window_width, this%window_height)

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
    end subroutine android_get_surface

    !> @brief Get lifecycle state
    subroutine android_get_lifecycle(this, lifecycle)
        class(forge_android_platform), intent(inout) :: this
        type(platform_lifecycle_state), intent(out) :: lifecycle

        lifecycle = this%lifecycle
    end subroutine android_get_lifecycle

    !> @brief Get current orientation
    subroutine android_get_orientation(this, orientation)
        class(forge_android_platform), intent(inout) :: this
        type(platform_orientation), intent(out) :: orientation

        orientation = this%orientation
    end subroutine android_get_orientation

    !> @brief Set orientation
    subroutine android_set_orientation(this, orientation, status)
        class(forge_android_platform), intent(inout) :: this
        type(platform_orientation), intent(in) :: orientation
        type(forge_status), intent(out) :: status

        ! For Android, orientation changes are handled by the Activity
        ! This would call JNI methods to request orientation change
        this%orientation = orientation
        call dispatch_app_event(EVENT_ORIENTATION_CHANGED)
        call status%clear()
    end subroutine android_set_orientation

    !> @brief Handle back button
    subroutine android_handle_back_button(this, handled)
        class(forge_android_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: handled

        ! Default behavior: let Android handle back button
        handled = .false.
        call dispatch_app_event(EVENT_BACK_BUTTON)
    end subroutine android_handle_back_button

    !> @brief Show status bar
    subroutine android_show_status_bar(this, status)
        class(forge_android_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        ! This would use JNI to call Activity methods to show status bar
        call status%clear()
    end subroutine android_show_status_bar

    !> @brief Hide status bar
    subroutine android_hide_status_bar(this, status)
        class(forge_android_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        ! This would use JNI to call Activity methods to hide status bar
        call status%clear()
    end subroutine android_hide_status_bar

    !> @brief Handle command: init window
    subroutine handle_cmd_init_window(this)
        class(forge_android_platform), intent(inout) :: this

        if (c_associated(this%app%window)) then
            this%window_width = ANativeWindow_getWidth(this%app%window)
            this%window_height = ANativeWindow_getHeight(this%app%window)
            this%window_created = .true.
            write(output_unit, '(A,I0,A,I0)') "[Android] Window initialized: ", &
                this%window_width, "x", this%window_height
        end if
    end subroutine handle_cmd_init_window

    !> @brief Handle command: term window
    subroutine handle_cmd_term_window(this)
        class(forge_android_platform), intent(inout) :: this

        this%window_created = .false.
        write(output_unit, '(A)') "[Android] Window terminated"
    end subroutine handle_cmd_term_window

    !> @brief Handle command: window resized
    subroutine handle_cmd_window_resized(this)
        class(forge_android_platform), intent(inout) :: this

        if (c_associated(this%app%window)) then
            this%window_width = ANativeWindow_getWidth(this%app%window)
            this%window_height = ANativeWindow_getHeight(this%app%window)
            write(output_unit, '(A,I0,A,I0)') "[Android] Window resized: ", &
                this%window_width, "x", this%window_height
        end if
    end subroutine handle_cmd_window_resized

    !> @brief Handle command: config changed
    subroutine handle_cmd_config_changed(this)
        class(forge_android_platform), intent(inout) :: this

        ! Handle orientation changes
        call update_orientation_from_config(this)
        call dispatch_app_event(EVENT_ORIENTATION_CHANGED)
    end subroutine handle_cmd_config_changed

    !> @brief Process input event
    subroutine process_input_event(event)
        type(c_ptr), intent(in) :: event
        integer(c_int) :: event_type, action, pointer_count, i

        event_type = AInputEvent_getType(event)

        select case (event_type)
        case (1)  ! AINPUT_EVENT_TYPE_MOTION
            action = AMotionEvent_getAction(event)
            pointer_count = AMotionEvent_getPointerCount(event)

            select case (action .and. int(z'FF', c_int))  ! Mask for action
            case (AMOTION_EVENT_ACTION_DOWN, AMOTION_EVENT_ACTION_POINTER_DOWN)
                call handle_touch_began(event, pointer_count)
            case (AMOTION_EVENT_ACTION_MOVE)
                call handle_touch_moved(event, pointer_count)
            case (AMOTION_EVENT_ACTION_UP, AMOTION_EVENT_ACTION_POINTER_UP)
                call handle_touch_ended(event, pointer_count)
            case (AMOTION_EVENT_ACTION_CANCEL)
                call handle_touch_cancelled(event, pointer_count)
            end select

        case (2)  ! AINPUT_EVENT_TYPE_KEY
            action = AKeyEvent_getAction(event)
            select case (action)
            case (AKEY_EVENT_ACTION_DOWN)
                call handle_key_down(event)
            case (AKEY_EVENT_ACTION_UP)
                call handle_key_up(event)
            end select
        end select
    end subroutine process_input_event

    !> @brief Handle touch began
    subroutine handle_touch_began(event, pointer_count)
        type(c_ptr), intent(in) :: event
        integer(c_int), intent(in) :: pointer_count
        integer :: i, touch_id
        real(c_float) :: x, y, pressure

        do i = 0, pointer_count - 1
            touch_id = AMotionEvent_getPointerId(event, i)
            x = AMotionEvent_getX(event, i)
            y = AMotionEvent_getY(event, i)
            pressure = AMotionEvent_getPressure(event, i)

            call update_touch_state(g_touch_state, touch_id, int(x), int(y), pressure, 0)
            call dispatch_touch_event(EVENT_TOUCH_BEGAN, touch_id, int(x), int(y), pressure)
        end do
    end subroutine handle_touch_began

    !> @brief Handle touch moved
    subroutine handle_touch_moved(event, pointer_count)
        type(c_ptr), intent(in) :: event
        integer(c_int), intent(in) :: pointer_count
        integer :: i, touch_id
        real(c_float) :: x, y, pressure

        do i = 0, pointer_count - 1
            touch_id = AMotionEvent_getPointerId(event, i)
            x = AMotionEvent_getX(event, i)
            y = AMotionEvent_getY(event, i)
            pressure = AMotionEvent_getPressure(event, i)

            call update_touch_state(g_touch_state, touch_id, int(x), int(y), pressure, 1)
            call dispatch_touch_event(EVENT_TOUCH_MOVED, touch_id, int(x), int(y), pressure)
        end do
    end subroutine handle_touch_moved

    !> @brief Handle touch ended
    subroutine handle_touch_ended(event, pointer_count)
        type(c_ptr), intent(in) :: event
        integer(c_int), intent(in) :: pointer_count
        integer :: i, touch_id
        real(c_float) :: x, y, pressure

        do i = 0, pointer_count - 1
            touch_id = AMotionEvent_getPointerId(event, i)
            x = AMotionEvent_getX(event, i)
            y = AMotionEvent_getY(event, i)
            pressure = AMotionEvent_getPressure(event, i)

            call update_touch_state(g_touch_state, touch_id, int(x), int(y), pressure, 2)
            call dispatch_touch_event(EVENT_TOUCH_ENDED, touch_id, int(x), int(y), pressure)
        end do
    end subroutine handle_touch_ended

    !> @brief Handle touch cancelled
    subroutine handle_touch_cancelled(event, pointer_count)
        type(c_ptr), intent(in) :: event
        integer(c_int), intent(in) :: pointer_count
        integer :: i, touch_id
        real(c_float) :: x, y, pressure

        do i = 0, pointer_count - 1
            touch_id = AMotionEvent_getPointerId(event, i)
            x = AMotionEvent_getX(event, i)
            y = AMotionEvent_getY(event, i)
            pressure = AMotionEvent_getPressure(event, i)

            call update_touch_state(g_touch_state, touch_id, int(x), int(y), pressure, 3)
            call dispatch_touch_event(EVENT_TOUCH_CANCELLED, touch_id, int(x), int(y), pressure)
        end do
    end subroutine handle_touch_cancelled

    !> @brief Handle key down
    subroutine handle_key_down(event)
        type(c_ptr), intent(in) :: event
        integer(c_int) :: key_code

        key_code = AKeyEvent_getKeyCode(event)
        call dispatch_keyboard_event(c_null_ptr, EVENT_KEY_PRESSED, int(key_code))
    end subroutine handle_key_down

    !> @brief Handle key up
    subroutine handle_key_up(event)
        type(c_ptr), intent(in) :: event
        integer(c_int) :: key_code

        key_code = AKeyEvent_getKeyCode(event)
        if (key_code == AKEYCODE_BACK) then
            call dispatch_app_event(EVENT_BACK_BUTTON)
        end if
        call dispatch_keyboard_event(c_null_ptr, EVENT_KEY_RELEASED, int(key_code))
    end subroutine handle_key_up

    !> @brief Update orientation from config
    subroutine update_orientation_from_config(this)
        class(forge_android_platform), intent(inout) :: this

        ! This would query the current orientation from Android config
        ! For now, assume portrait
        this%orientation%current_orientation = ORIENTATION_PORTRAIT
        this%orientation%screen_width = this%window_width
        this%orientation%screen_height = this%window_height
    end subroutine update_orientation_from_config

    !> @brief Set render callback
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Dispatch touch event
    subroutine dispatch_touch_event(event_type, touch_id, x, y, pressure)
        integer, intent(in) :: event_type, touch_id, x, y
        real(c_float), intent(in) :: pressure
        type(forge_event) :: event

        event%event_type%id = event_type
        event%touch_count = 1
        event%touches(1)%touch_id = touch_id
        event%touches(1)%x = x
        event%touches(1)%y = y
        event%touches(1)%pressure = pressure
        event%touches(1)%phase = event_type - EVENT_TOUCH_BEGAN

        write(output_unit, '(A,I0,A,I0,A,I0,A,F4.2)') "[Android] Touch event: ", &
            event_type, " id=", touch_id, " at (", x, ",", y, ") pressure=", pressure
    end subroutine dispatch_touch_event

    !> @brief Dispatch app event
    subroutine dispatch_app_event(event_type)
        integer, intent(in) :: event_type
        type(forge_event) :: event

        event%event_type%id = event_type

        write(output_unit, '(A,I0)') "[Android] App event: ", event_type
    end subroutine dispatch_app_event

    !> @brief Dispatch keyboard event
    subroutine dispatch_keyboard_event(window_handle, event_type, key_code)
        type(c_ptr), intent(in) :: window_handle
        integer, intent(in) :: event_type, key_code
        type(forge_event) :: event

        event%event_type%id = event_type
        event%window_handle = window_handle
        event%key_code = key_code

        write(output_unit, '(A,I0,A,I0)') "[Android] Keyboard event: ", &
            event_type, " key=", key_code
    end subroutine dispatch_keyboard_event

end module forge_platform_android