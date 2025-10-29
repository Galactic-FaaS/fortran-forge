!> @brief Input handling for custom GUI backend
!> @details Mouse and keyboard event processing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_input
    use iso_c_binding
    use forge_types
    use forge_events
    implicit none
    private

    public :: forge_mouse_state, forge_keyboard_state, forge_touch_state
    public :: update_mouse_state, create_mouse_event
    public :: create_keyboard_event, update_touch_state

    !> @brief Mouse state tracking
    type :: forge_mouse_state
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
        logical :: left_button = .false.
        logical :: middle_button = .false.
        logical :: right_button = .false.
        logical :: over_window = .false.
    end type forge_mouse_state

    !> @brief Keyboard state tracking
    type :: forge_keyboard_state
        logical :: shift_pressed = .false.
        logical :: ctrl_pressed = .false.
        logical :: alt_pressed = .false.
    end type forge_keyboard_state

    !> @brief Touch state tracking
    type :: forge_touch_state
        integer(c_int) :: touch_count = 0
        type(forge_touch_point) :: touches(MAX_TOUCH_POINTS)
    end type forge_touch_state

contains

    !> @brief Update mouse state from coordinates and button state
    subroutine update_mouse_state(state, x, y, left_down, middle_down, right_down)
        type(forge_mouse_state), intent(inout) :: state
        integer(c_int), intent(in) :: x, y
        logical, intent(in), optional :: left_down, middle_down, right_down

        state%x = x
        state%y = y
        
        if (present(left_down)) state%left_button = left_down
        if (present(middle_down)) state%middle_button = middle_down
        if (present(right_down)) state%right_button = right_down
    end subroutine update_mouse_state

    !> @brief Create a ForGE mouse event from state
    function create_mouse_event(event_type_id, state, widget_handle, window_handle) result(event)
        integer, intent(in) :: event_type_id
        type(forge_mouse_state), intent(in) :: state
        type(c_ptr), intent(in), optional :: widget_handle, window_handle
        type(forge_event) :: event

        event%event_type%id = event_type_id
        event%mouse_x = state%x
        event%mouse_y = state%y

        ! Determine which button
        if (state%left_button) then
            event%mouse_button = MOUSE_BUTTON_LEFT
        else if (state%middle_button) then
            event%mouse_button = MOUSE_BUTTON_MIDDLE
        else if (state%right_button) then
            event%mouse_button = MOUSE_BUTTON_RIGHT
        else
            event%mouse_button = 0
        end if

        if (present(widget_handle)) event%widget_handle = widget_handle
        if (present(window_handle)) event%window_handle = window_handle
    end function create_mouse_event

    !> @brief Create a ForGE keyboard event
    function create_keyboard_event(event_type_id, key_code, key_char, keyboard_state) result(event)
        integer, intent(in) :: event_type_id
        integer(c_int), intent(in) :: key_code
        character(len=*), intent(in), optional :: key_char
        type(forge_keyboard_state), intent(in), optional :: keyboard_state
        type(forge_event) :: event

        event%event_type%id = event_type_id
        event%key_code = key_code
        
        if (present(key_char)) then
            event%key_char = key_char
        end if

        if (present(keyboard_state)) then
            event%shift_pressed = keyboard_state%shift_pressed
            event%ctrl_pressed = keyboard_state%ctrl_pressed
            event%alt_pressed = keyboard_state%alt_pressed
        end if
    end function create_keyboard_event

    !> @brief Update touch state from touch event data
    subroutine update_touch_state(state, touch_id, x, y, pressure, phase)
        type(forge_touch_state), intent(inout) :: state
        integer(c_int), intent(in) :: touch_id, x, y, phase
        real(c_float), intent(in) :: pressure
        integer :: i, found_index

        found_index = -1
        do i = 1, state%touch_count
            if (state%touches(i)%touch_id == touch_id) then
                found_index = i
                exit
            end if
        end do

        if (found_index == -1) then
            ! New touch
            if (state%touch_count < MAX_TOUCH_POINTS) then
                state%touch_count = state%touch_count + 1
                found_index = state%touch_count
            else
                ! Too many touches, ignore
                return
            end if
        end if

        ! Update touch data
        state%touches(found_index)%touch_id = touch_id
        state%touches(found_index)%x = x
        state%touches(found_index)%y = y
        state%touches(found_index)%pressure = pressure
        state%touches(found_index)%phase = phase

        ! Remove ended/cancelled touches
        if (phase == 2 .or. phase == 3) then  ! ended or cancelled
            if (found_index < state%touch_count) then
                state%touches(found_index) = state%touches(state%touch_count)
            end if
            state%touch_count = state%touch_count - 1
        end if
    end subroutine update_touch_state

end module forge_input

