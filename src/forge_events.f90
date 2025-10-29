!> @brief Event handling system for ForGE
!> @details Defines event types and callback mechanisms
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_events
    use iso_c_binding
    use forge_types
    implicit none
    private

    public :: forge_event, forge_event_handler, forge_event_type, event_callback_interface
    public :: EVENT_NONE, EVENT_BUTTON_CLICKED, EVENT_WINDOW_CLOSED
    public :: EVENT_KEY_PRESSED, EVENT_KEY_RELEASED, EVENT_MOUSE_MOVED
    public :: EVENT_MOUSE_PRESSED, EVENT_MOUSE_RELEASED, EVENT_TEXT_CHANGED
    public :: EVENT_VALUE_CHANGED, EVENT_WINDOW_RESIZED, EVENT_TOGGLED
    public :: EVENT_SELECTION_CHANGED, EVENT_TOUCH_BEGAN, EVENT_TOUCH_MOVED
    public :: EVENT_TOUCH_ENDED, EVENT_TOUCH_CANCELLED, EVENT_APP_BACKGROUND
    public :: EVENT_APP_FOREGROUND, EVENT_ORIENTATION_CHANGED, EVENT_BACK_BUTTON

    !> Event type identifiers
    integer, parameter :: EVENT_NONE = 0
    integer, parameter :: EVENT_BUTTON_CLICKED = 1
    integer, parameter :: EVENT_WINDOW_CLOSED = 2
    integer, parameter :: EVENT_KEY_PRESSED = 3
    integer, parameter :: EVENT_KEY_RELEASED = 4
    integer, parameter :: EVENT_MOUSE_MOVED = 5
    integer, parameter :: EVENT_MOUSE_PRESSED = 6
    integer, parameter :: EVENT_MOUSE_RELEASED = 7
    integer, parameter :: EVENT_TEXT_CHANGED = 8
    integer, parameter :: EVENT_VALUE_CHANGED = 9
    integer, parameter :: EVENT_WINDOW_RESIZED = 10
    integer, parameter :: EVENT_TOGGLED = 11
    integer, parameter :: EVENT_SELECTION_CHANGED = 12
    integer, parameter :: EVENT_TOUCH_BEGAN = 13
    integer, parameter :: EVENT_TOUCH_MOVED = 14
    integer, parameter :: EVENT_TOUCH_ENDED = 15
    integer, parameter :: EVENT_TOUCH_CANCELLED = 16
    integer, parameter :: EVENT_APP_BACKGROUND = 17
    integer, parameter :: EVENT_APP_FOREGROUND = 18
    integer, parameter :: EVENT_ORIENTATION_CHANGED = 19
    integer, parameter :: EVENT_BACK_BUTTON = 20

    !> @brief Event type selector
    type :: forge_event_type
        integer :: id = EVENT_NONE
        character(len=64) :: name = ""
    end type forge_event_type

    !> @brief Mouse button identifiers
    integer, parameter, public :: MOUSE_BUTTON_LEFT = 1
    integer, parameter, public :: MOUSE_BUTTON_MIDDLE = 2
    integer, parameter, public :: MOUSE_BUTTON_RIGHT = 3

    !> @brief Touch identifiers
    integer, parameter, public :: MAX_TOUCH_POINTS = 10

    !> @brief Touch point data
    type :: forge_touch_point
        integer(c_int) :: touch_id = 0
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
        real(c_float) :: pressure = 1.0
        integer(c_int) :: phase = 0  ! 0=began, 1=moved, 2=ended, 3=cancelled
    end type forge_touch_point

    !> @brief Event data structure
    type :: forge_event
        type(forge_event_type) :: event_type
        type(c_ptr) :: widget_handle = c_null_ptr
        type(c_ptr) :: window_handle = c_null_ptr

        ! Mouse event data
        integer(c_int) :: mouse_x = 0
        integer(c_int) :: mouse_y = 0
        integer(c_int) :: mouse_button = 0

        ! Touch event data
        integer(c_int) :: touch_count = 0
        type(forge_touch_point) :: touches(MAX_TOUCH_POINTS)

        ! Keyboard event data
        integer(c_int) :: key_code = 0
        character(len=32) :: key_char = ""

        ! Modifier keys
        logical(c_bool) :: shift_pressed = .false.
        logical(c_bool) :: ctrl_pressed = .false.
        logical(c_bool) :: alt_pressed = .false.

        ! Mobile-specific data
        integer(c_int) :: orientation = 0  ! 0=portrait, 1=landscape
        integer(c_int) :: screen_width = 0
        integer(c_int) :: screen_height = 0

        ! Generic data pointer for custom events
        type(c_ptr) :: user_data = c_null_ptr
    end type forge_event

    !> @brief Abstract interface for event callbacks
    abstract interface
        subroutine event_callback_interface(event)
            import :: forge_event
            type(forge_event), intent(in) :: event
        end subroutine event_callback_interface
    end interface

    !> @brief Event handler container
    type :: forge_event_handler
        procedure(event_callback_interface), pointer, nopass :: callback => null()
        type(forge_event_type) :: event_type
    contains
        procedure :: set_callback => forge_event_handler_set_callback
        procedure :: call => forge_event_handler_call
        procedure :: is_set => forge_event_handler_is_set
    end type forge_event_handler

contains

    !> @brief Set event callback
    subroutine forge_event_handler_set_callback(this, callback, event_type_id)
        class(forge_event_handler), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        integer, intent(in), optional :: event_type_id
        
        this%callback => callback
        
        if (present(event_type_id)) then
            this%event_type%id = event_type_id
        end if
    end subroutine forge_event_handler_set_callback

    !> @brief Call event callback
    subroutine forge_event_handler_call(this, event)
        class(forge_event_handler), intent(in) :: this
        type(forge_event), intent(in) :: event
        
        if (associated(this%callback)) then
            call this%callback(event)
        end if
    end subroutine forge_event_handler_call

    !> @brief Check if callback is set
    function forge_event_handler_is_set(this) result(is_set)
        class(forge_event_handler), intent(in) :: this
        logical :: is_set
        
        is_set = associated(this%callback)
    end function forge_event_handler_is_set

end module forge_events

