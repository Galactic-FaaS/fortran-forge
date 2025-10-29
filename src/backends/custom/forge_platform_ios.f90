!> @brief iOS platform implementation using UIKit
!> @details Provides windowing and event handling for iOS with UIViewController/UIView integration
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_platform_ios
    use iso_c_binding
    use iso_fortran_env, only: output_unit, error_unit
    use forge_platform
    use forge_types
    use forge_errors
    use forge_input
    use forge_events
    implicit none
    private

    public :: forge_ios_platform
    public :: set_window_render_callback

    ! iOS/UIKit constants
    integer(c_long), parameter :: UIApplicationMain = 0
    integer(c_long), parameter :: UIInterfaceOrientationPortrait = 1
    integer(c_long), parameter :: UIInterfaceOrientationLandscapeLeft = 2
    integer(c_long), parameter :: UIInterfaceOrientationLandscapeRight = 3
    integer(c_long), parameter :: UIInterfaceOrientationPortraitUpsideDown = 4

    ! UIApplication states
    integer(c_long), parameter :: UIApplicationStateActive = 0
    integer(c_long), parameter :: UIApplicationStateInactive = 1
    integer(c_long), parameter :: UIApplicationStateBackground = 2

    ! UITouch phases
    integer(c_long), parameter :: UITouchPhaseBegan = 0
    integer(c_long), parameter :: UITouchPhaseMoved = 1
    integer(c_long), parameter :: UITouchPhaseStationary = 2
    integer(c_long), parameter :: UITouchPhaseEnded = 3
    integer(c_long), parameter :: UITouchPhaseCancelled = 4

    ! UIEvent types
    integer(c_long), parameter :: UIEventTypeTouches = 0
    integer(c_long), parameter :: UIEventTypeMotion = 1
    integer(c_long), parameter :: UIEventTypeRemoteControl = 2

    ! iOS structures (simplified for Fortran binding)
    type :: UIApplication
        type(c_ptr) :: ptr = c_null_ptr
    end type UIApplication

    type :: UIWindow
        type(c_ptr) :: ptr = c_null_ptr
    end type UIWindow

    type :: UIViewController
        type(c_ptr) :: ptr = c_null_ptr
    end type UIViewController

    type :: UIView
        type(c_ptr) :: ptr = c_null_ptr
    end type UIView

    type :: UITouch
        type(c_ptr) :: ptr = c_null_ptr
    end type UITouch

    type :: UIEvent
        type(c_ptr) :: ptr = c_null_ptr
    end type UIEvent

    type :: CGRect
        real(c_double) :: x, y, width, height
    end type CGRect

    type :: CGPoint
        real(c_double) :: x, y
    end type CGPoint

    ! UIKit API bindings
    interface

        ! Application management
        function UIApplication_sharedApplication() bind(C, name="UIApplication_sharedApplication")
            import :: c_ptr
            type(c_ptr) :: UIApplication_sharedApplication
        end function UIApplication_sharedApplication

        function UIApplication_applicationState(app) bind(C, name="UIApplication_applicationState")
            import :: c_ptr, c_long
            type(c_ptr), value :: app
            integer(c_long) :: UIApplication_applicationState
        end function UIApplication_applicationState

        ! Window management
        function UIWindow_alloc() bind(C, name="UIWindow_alloc")
            import :: c_ptr
            type(c_ptr) :: UIWindow_alloc
        end function UIWindow_alloc

        function UIWindow_initWithFrame(window, frame) bind(C, name="UIWindow_initWithFrame")
            import :: c_ptr, CGRect
            type(c_ptr), value :: window
            type(CGRect), intent(in) :: frame
            type(c_ptr) :: UIWindow_initWithFrame
        end function UIWindow_initWithFrame

        subroutine UIWindow_setRootViewController(window, controller) &
                bind(C, name="UIWindow_setRootViewController")
            import :: c_ptr
            type(c_ptr), value :: window, controller
        end subroutine UIWindow_setRootViewController

        subroutine UIWindow_makeKeyAndVisible(window) bind(C, name="UIWindow_makeKeyAndVisible")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine UIWindow_makeKeyAndVisible

        ! View controller management
        function UIViewController_alloc() bind(C, name="UIViewController_alloc")
            import :: c_ptr
            type(c_ptr) :: UIViewController_alloc
        end function UIViewController_alloc

        function UIViewController_init(controller) bind(C, name="UIViewController_init")
            import :: c_ptr
            type(c_ptr), value :: controller
            type(c_ptr) :: UIViewController_init
        end function UIViewController_init

        function UIViewController_view(controller) bind(C, name="UIViewController_view")
            import :: c_ptr
            type(c_ptr), value :: controller
            type(c_ptr) :: UIViewController_view
        end function UIViewController_view

        ! View management
        function UIView_alloc() bind(C, name="UIView_alloc")
            import :: c_ptr
            type(c_ptr) :: UIView_alloc
        end function UIView_alloc

        function UIView_initWithFrame(view, frame) bind(C, name="UIView_initWithFrame")
            import :: c_ptr, CGRect
            type(c_ptr), value :: view
            type(CGRect), intent(in) :: frame
            type(c_ptr) :: UIView_initWithFrame
        end function UIView_initWithFrame

        subroutine UIView_setBackgroundColor(view, color) bind(C, name="UIView_setBackgroundColor")
            import :: c_ptr
            type(c_ptr), value :: view, color
        end subroutine UIView_setBackgroundColor

        ! Touch event handling
        function UIEvent_type(event) bind(C, name="UIEvent_type")
            import :: c_ptr, c_long
            type(c_ptr), value :: event
            integer(c_long) :: UIEvent_type
        end function UIEvent_type

        function UIEvent_allTouches(event) bind(C, name="UIEvent_allTouches")
            import :: c_ptr
            type(c_ptr), value :: event
            type(c_ptr) :: UIEvent_allTouches
        end function UIEvent_allTouches

        function UITouch_locationInView(touch, view) bind(C, name="UITouch_locationInView")
            import :: c_ptr, CGPoint
            type(c_ptr), value :: touch, view
            type(CGPoint) :: UITouch_locationInView
        end function UITouch_locationInView

        function UITouch_phase(touch) bind(C, name="UITouch_phase")
            import :: c_ptr, c_long
            type(c_ptr), value :: touch
            integer(c_long) :: UITouch_phase
        end function UITouch_phase

        function UITouch_tapCount(touch) bind(C, name="UITouch_tapCount")
            import :: c_ptr, c_long
            type(c_ptr), value :: touch
            integer(c_long) :: UITouch_tapCount
        end function UITouch_tapCount

        ! Set management
        function NSSet_count(set) bind(C, name="NSSet_count")
            import :: c_ptr, c_long
            type(c_ptr), value :: set
            integer(c_long) :: NSSet_count
        end function NSSet_count

        function NSSet_allObjects(set) bind(C, name="NSSet_allObjects")
            import :: c_ptr
            type(c_ptr), value :: set
            type(c_ptr) :: NSSet_allObjects
        end function NSSet_allObjects

        function NSArray_objectAtIndex(array, index) bind(C, name="NSArray_objectAtIndex")
            import :: c_ptr, c_long
            type(c_ptr), value :: array
            integer(c_long), value :: index
            type(c_ptr) :: NSArray_objectAtIndex
        end function NSArray_objectAtIndex

        ! Screen and orientation
        function UIScreen_mainScreen() bind(C, name="UIScreen_mainScreen")
            import :: c_ptr
            type(c_ptr) :: UIScreen_mainScreen
        end function UIScreen_mainScreen

        function UIScreen_bounds(screen) bind(C, name="UIScreen_bounds")
            import :: c_ptr, CGRect
            type(c_ptr), value :: screen
            type(CGRect) :: UIScreen_bounds
        end function UIScreen_bounds

        ! Status bar management
        subroutine UIApplication_setStatusBarHidden(app, hidden, animated) &
                bind(C, name="UIApplication_setStatusBarHidden")
            import :: c_ptr, c_bool
            type(c_ptr), value :: app
            logical(c_bool), value :: hidden, animated
        end subroutine UIApplication_setStatusBarHidden

    end interface

    ! Global state
    type(UIApplication) :: g_app
    type(UIWindow) :: g_window
    type(UIViewController) :: g_view_controller
    type(UIView) :: g_view

    ! Touch and input state
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

    !> @brief iOS platform implementation
    type, extends(forge_platform_base) :: forge_ios_platform
        private
        logical :: window_created = .false.
        integer(c_int) :: window_width = 0
        integer(c_int) :: window_height = 0
    contains
        procedure :: init => ios_init
        procedure :: shutdown => ios_shutdown
        procedure :: create_window => ios_create_window
        procedure :: destroy_window => ios_destroy_window
        procedure :: show_window => ios_show_window
        procedure :: hide_window => ios_hide_window
        procedure :: process_events => ios_process_events
        procedure :: get_surface => ios_get_surface
        procedure :: get_lifecycle => ios_get_lifecycle
        procedure :: get_orientation => ios_get_orientation
        procedure :: set_orientation => ios_set_orientation
        procedure :: handle_back_button => ios_handle_back_button
        procedure :: show_status_bar => ios_show_status_bar
        procedure :: hide_status_bar => ios_hide_status_bar
    end type forge_ios_platform

contains

    !> @brief Initialize iOS platform
    subroutine ios_init(this, status)
        class(forge_ios_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        write(output_unit, '(A)') "[iOS] Initializing iOS platform..."

        ! Get shared application
        g_app%ptr = UIApplication_sharedApplication()
        if (.not. c_associated(g_app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get UIApplication")
            return
        end if

        ! Set up lifecycle
        this%lifecycle%current_state = LIFECYCLE_CREATED
        this%lifecycle%is_background = .false.
        this%lifecycle%is_foreground = .true.

        ! Set up default orientation
        this%orientation%current_orientation = ORIENTATION_PORTRAIT
        call update_screen_bounds(this)

        this%initialized = .true.
        call status%clear()
        write(output_unit, '(A)') "[iOS] Platform initialized successfully"
    end subroutine ios_init

    !> @brief Shutdown iOS platform
    subroutine ios_shutdown(this)
        class(forge_ios_platform), intent(inout) :: this

        write(output_unit, '(A)') "[iOS] Shutting down iOS platform"

        ! Clean up window and views
        if (this%window_created) then
            ! iOS handles cleanup automatically
            this%window_created = .false.
        end if

        this%initialized = .false.
    end subroutine ios_shutdown

    !> @brief Create a window (UIWindow with UIViewController)
    subroutine ios_create_window(this, handle, title, width, height, status)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(CGRect) :: frame

        ! Create window
        g_window%ptr = UIWindow_alloc()
        if (.not. c_associated(g_window%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to allocate UIWindow")
            return
        end if

        ! Get screen bounds
        frame = get_screen_bounds()

        ! Initialize window with screen bounds
        g_window%ptr = UIWindow_initWithFrame(g_window%ptr, frame)
        if (.not. c_associated(g_window%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to initialize UIWindow")
            return
        end if

        ! Create view controller
        g_view_controller%ptr = UIViewController_alloc()
        if (c_associated(g_view_controller%ptr)) then
            g_view_controller%ptr = UIViewController_init(g_view_controller%ptr)
        end if

        ! Create main view
        g_view%ptr = UIView_alloc()
        if (c_associated(g_view%ptr)) then
            g_view%ptr = UIView_initWithFrame(g_view%ptr, frame)
            ! Set view controller's view
            if (c_associated(g_view_controller%ptr)) then
                ! In real implementation, this would set the view controller's view
            end if
        end if

        ! Set root view controller
        if (c_associated(g_view_controller%ptr)) then
            call UIWindow_setRootViewController(g_window%ptr, g_view_controller%ptr)
        end if

        this%window_width = int(frame%width)
        this%window_height = int(frame%height)
        this%window_created = .true.

        handle%native_handle = g_window%ptr
        handle%width = this%window_width
        handle%height = this%window_height

        call status%clear()
        write(output_unit, '(A,A,A,I0,A,I0,A)') &
            "[iOS] Created window '", trim(title), "' (", this%window_width, "x", this%window_height, ")"
    end subroutine ios_create_window

    !> @brief Destroy a window
    subroutine ios_destroy_window(this, handle)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        this%window_created = .false.
        write(output_unit, '(A)') "[iOS] Window destroyed"
    end subroutine ios_destroy_window

    !> @brief Show a window (make key and visible)
    subroutine ios_show_window(this, handle)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        if (this%window_created) then
            call UIWindow_makeKeyAndVisible(g_window%ptr)
            write(output_unit, '(A)') "[iOS] Window shown"
        end if
    end subroutine ios_show_window

    !> @brief Hide a window
    subroutine ios_hide_window(this, handle)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: handle

        ! iOS windows are typically full-screen and can't be hidden
        write(output_unit, '(A)') "[iOS] Window hide not supported"
    end subroutine ios_hide_window

    !> @brief Process pending events
    subroutine ios_process_events(this, should_quit)
        class(forge_ios_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: should_quit
        integer(c_long) :: app_state

        should_quit = .false.

        ! Check application state for lifecycle events
        if (c_associated(g_app%ptr)) then
            app_state = UIApplication_applicationState(g_app%ptr)

            select case (app_state)
            case (UIApplicationStateActive)
                if (this%lifecycle%current_state /= LIFECYCLE_RESUMED) then
                    this%lifecycle%current_state = LIFECYCLE_RESUMED
                    this%lifecycle%is_background = .false.
                    this%lifecycle%is_foreground = .true.
                    call dispatch_app_event(EVENT_APP_FOREGROUND)
                end if
            case (UIApplicationStateBackground)
                if (.not. this%lifecycle%is_background) then
                    this%lifecycle%current_state = LIFECYCLE_PAUSED
                    this%lifecycle%is_background = .true.
                    this%lifecycle%is_foreground = .false.
                    call dispatch_app_event(EVENT_APP_BACKGROUND)
                end if
            case (UIApplicationStateInactive)
                ! Application is transitioning
            end select
        end if

        ! iOS handles events through UIKit run loop automatically
        ! Touch events are handled by UIView touch event methods
    end subroutine ios_process_events

    !> @brief Get drawing surface for window
    subroutine ios_get_surface(this, window, surface, status)
        use forge_cairo_bindings
        class(forge_ios_platform), intent(inout) :: this
        type(platform_window_handle), intent(in) :: window
        type(platform_surface_handle), intent(out) :: surface
        type(forge_status), intent(out) :: status

        if (.not. this%window_created) then
            call status%set(FORGE_ERROR_NULL_PTR, "iOS window not available")
            return
        end if

        ! Create Cairo surface for iOS view
        ! This would integrate with CoreGraphics/CGContext
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
    end subroutine ios_get_surface

    !> @brief Get lifecycle state
    subroutine ios_get_lifecycle(this, lifecycle)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_lifecycle_state), intent(out) :: lifecycle

        lifecycle = this%lifecycle
    end subroutine ios_get_lifecycle

    !> @brief Get current orientation
    subroutine ios_get_orientation(this, orientation)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_orientation), intent(out) :: orientation

        orientation = this%orientation
    end subroutine ios_get_orientation

    !> @brief Set orientation
    subroutine ios_set_orientation(this, orientation, status)
        class(forge_ios_platform), intent(inout) :: this
        type(platform_orientation), intent(in) :: orientation
        type(forge_status), intent(out) :: status

        ! For iOS, orientation changes are handled by UIViewController
        ! This would call methods to request orientation change
        this%orientation = orientation
        call dispatch_app_event(EVENT_ORIENTATION_CHANGED)
        call status%clear()
    end subroutine ios_set_orientation

    !> @brief Handle back button (iOS doesn't have hardware back button like Android)
    subroutine ios_handle_back_button(this, handled)
        class(forge_ios_platform), intent(inout) :: this
        logical(c_bool), intent(out) :: handled

        ! iOS doesn't have a system back button like Android
        ! This could be handled by custom UI navigation
        handled = .false.
    end subroutine ios_handle_back_button

    !> @brief Show status bar
    subroutine ios_show_status_bar(this, status)
        class(forge_ios_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (c_associated(g_app%ptr)) then
            call UIApplication_setStatusBarHidden(g_app%ptr, .false., .true.)
        end if
        call status%clear()
    end subroutine ios_show_status_bar

    !> @brief Hide status bar
    subroutine ios_hide_status_bar(this, status)
        class(forge_ios_platform), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (c_associated(g_app%ptr)) then
            call UIApplication_setStatusBarHidden(g_app%ptr, .true., .true.)
        end if
        call status%clear()
    end subroutine ios_hide_status_bar

    !> @brief Update screen bounds and orientation
    subroutine update_screen_bounds(this)
        class(forge_ios_platform), intent(inout) :: this
        type(CGRect) :: bounds

        bounds = get_screen_bounds()
        this%orientation%screen_width = int(bounds%width)
        this%orientation%screen_height = int(bounds%height)

        ! Determine orientation based on dimensions
        if (bounds%width > bounds%height) then
            this%orientation%current_orientation = ORIENTATION_LANDSCAPE
        else
            this%orientation%current_orientation = ORIENTATION_PORTRAIT
        end if
    end subroutine update_screen_bounds

    !> @brief Get screen bounds
    function get_screen_bounds() result(bounds)
        type(CGRect) :: bounds
        type(c_ptr) :: screen

        screen = UIScreen_mainScreen()
        if (c_associated(screen)) then
            bounds = UIScreen_bounds(screen)
        else
            ! Default bounds
            bounds%x = 0.0_c_double
            bounds%y = 0.0_c_double
            bounds%width = 375.0_c_double  ! iPhone width
            bounds%height = 667.0_c_double ! iPhone height
        end if
    end function get_screen_bounds

    !> @brief Set render callback
    subroutine set_window_render_callback(callback)
        procedure(render_callback_interface) :: callback
        g_render_callback => callback
    end subroutine set_window_render_callback

    !> @brief Dispatch app event
    subroutine dispatch_app_event(event_type)
        integer, intent(in) :: event_type
        type(forge_event) :: event

        event%event_type%id = event_type

        write(output_unit, '(A,I0)') "[iOS] App event: ", event_type
    end subroutine dispatch_app_event

end module forge_platform_ios