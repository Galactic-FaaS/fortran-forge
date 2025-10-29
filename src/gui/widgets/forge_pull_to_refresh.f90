!> @brief Pull-to-refresh widget for mobile interfaces
!> @details Scroll view with pull-to-refresh functionality
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_pull_to_refresh
    use iso_c_binding
    use forge_types
    use forge_widgets
    use forge_touch
    implicit none
    private

    public :: forge_pull_to_refresh_view
    public :: pull_to_refresh_create, pull_to_refresh_begin_refreshing
    public :: pull_to_refresh_end_refreshing

    !> @brief Pull-to-refresh states
    integer, parameter :: REFRESH_STATE_IDLE = 0
    integer, parameter :: REFRESH_STATE_PULLING = 1
    integer, parameter :: REFRESH_STATE_TRIGGERED = 2
    integer, parameter :: REFRESH_STATE_REFRESHING = 3

    !> @brief Pull-to-refresh view
    type, extends(forge_widget) :: forge_pull_to_refresh_view
        private
        type(c_ptr) :: scroll_view = c_null_ptr
        type(c_ptr) :: refresh_control = c_null_ptr
        integer :: refresh_state = REFRESH_STATE_IDLE
        real(c_float) :: pull_threshold = 80.0  ! pixels
        real(c_float) :: pull_distance = 0.0
        type(forge_gesture_recognizer) :: gesture_recognizer
        procedure(refresh_callback_interface), pointer, nopass :: refresh_callback => null()
    contains
        procedure :: begin_refreshing => pull_refresh_begin_refreshing
        procedure :: end_refreshing => pull_refresh_end_refreshing
        procedure :: set_refresh_callback => pull_refresh_set_callback
        procedure :: handle_touch_event => pull_refresh_handle_touch
        procedure :: handle_scroll => pull_refresh_handle_scroll
    end type forge_pull_to_refresh_view

    !> @brief Refresh callback interface
    abstract interface
        subroutine refresh_callback_interface()
        end subroutine refresh_callback_interface
    end interface

contains

    !> @brief Create pull-to-refresh view
    function pull_to_refresh_create(x, y, width, height) result(view)
        integer(c_int), intent(in) :: x, y, width, height
        type(forge_pull_to_refresh_view) :: view

        ! Initialize base widget
        view%rect%x = x
        view%rect%y = y
        view%rect%width = width
        view%rect%height = height
        view%visible = .true.
        view%enabled = .true.

        ! Initialize pull-to-refresh specific properties
        view%refresh_state = REFRESH_STATE_IDLE
        view%pull_threshold = 80.0
        view%pull_distance = 0.0

        ! Initialize gesture recognizer
        call init_gesture_recognizer(view%gesture_recognizer)

        ! Create scroll view and refresh control (would integrate with actual widgets)
        view%scroll_view = c_null_ptr
        view%refresh_control = c_null_ptr
    end function pull_to_refresh_create

    !> @brief Begin refreshing programmatically
    subroutine pull_to_refresh_begin_refreshing(view)
        type(forge_pull_to_refresh_view), intent(inout) :: view

        view%refresh_state = REFRESH_STATE_REFRESHING
        ! Show refresh indicator and trigger callback
        if (associated(view%refresh_callback)) then
            call view%refresh_callback()
        end if
    end subroutine pull_to_refresh_begin_refreshing

    !> @brief End refreshing
    subroutine pull_to_refresh_end_refreshing(view)
        type(forge_pull_to_refresh_view), intent(inout) :: view

        view%refresh_state = REFRESH_STATE_IDLE
        view%pull_distance = 0.0
        ! Hide refresh indicator
    end subroutine pull_to_refresh_end_refreshing

    !> @brief Begin refreshing (method version)
    subroutine pull_refresh_begin_refreshing(this)
        class(forge_pull_to_refresh_view), intent(inout) :: this

        call pull_to_refresh_begin_refreshing(this)
    end subroutine pull_refresh_begin_refreshing

    !> @brief End refreshing (method version)
    subroutine pull_refresh_end_refreshing(this)
        class(forge_pull_to_refresh_view), intent(inout) :: this

        call pull_to_refresh_end_refreshing(this)
    end subroutine pull_refresh_end_refreshing

    !> @brief Set refresh callback
    subroutine pull_refresh_set_callback(this, callback)
        class(forge_pull_to_refresh_view), intent(inout) :: this
        procedure(refresh_callback_interface) :: callback

        this%refresh_callback => callback
    end subroutine pull_refresh_set_callback

    !> @brief Handle touch event
    subroutine pull_refresh_handle_touch(this, event)
        class(forge_pull_to_refresh_view), intent(inout) :: this
        type(forge_event), intent(in) :: event
        type(forge_touch_gesture) :: gestures(10)
        integer :: num_gestures, i

        ! Process touch events through gesture recognizer
        call process_touch_events(this%gesture_recognizer, [event], 1, gestures, num_gestures)

        ! Handle recognized gestures
        do i = 1, num_gestures
            select case (gestures(i)%gesture_type)
            case (GESTURE_SWIPE_DOWN)
                if (this%refresh_state == REFRESH_STATE_IDLE) then
                    this%refresh_state = REFRESH_STATE_PULLING
                end if
            end select
        end do
    end subroutine pull_refresh_handle_touch

    !> @brief Handle scroll event
    subroutine pull_refresh_handle_scroll(this, scroll_distance)
        class(forge_pull_to_refresh_view), intent(inout) :: this
        real(c_float), intent(in) :: scroll_distance

        if (scroll_distance > 0.0) then  ! Pulling down
            this%pull_distance = scroll_distance

            select case (this%refresh_state)
            case (REFRESH_STATE_IDLE, REFRESH_STATE_PULLING)
                if (this%pull_distance >= this%pull_threshold) then
                    this%refresh_state = REFRESH_STATE_TRIGGERED
                    ! Show "release to refresh" indicator
                else
                    this%refresh_state = REFRESH_STATE_PULLING
                end if
            end select
        else
            ! Handle release
            if (this%refresh_state == REFRESH_STATE_TRIGGERED) then
                this%refresh_state = REFRESH_STATE_REFRESHING
                this%pull_distance = this%pull_threshold  ! Keep at threshold during refresh

                ! Trigger refresh callback
                if (associated(this%refresh_callback)) then
                    call this%refresh_callback()
                end if
            else if (this%refresh_state == REFRESH_STATE_PULLING) then
                this%refresh_state = REFRESH_STATE_IDLE
                this%pull_distance = 0.0
            end if
        end if
    end subroutine pull_refresh_handle_scroll

end module forge_pull_to_refresh