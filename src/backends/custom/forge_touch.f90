!> @brief Touch event handling and gesture recognition for mobile platforms
!> @details Multi-touch gesture processing and recognition
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_touch
    use iso_c_binding
    use forge_types
    use forge_events
    use forge_input
    implicit none
    private

    public :: forge_gesture_recognizer, forge_touch_gesture
    public :: init_gesture_recognizer, process_touch_events
    public :: GESTURE_TAP, GESTURE_DOUBLE_TAP, GESTURE_LONG_PRESS
    public :: GESTURE_SWIPE_LEFT, GESTURE_SWIPE_RIGHT, GESTURE_SWIPE_UP, GESTURE_SWIPE_DOWN
    public :: GESTURE_PINCH_IN, GESTURE_PINCH_OUT, GESTURE_ROTATE

    ! Gesture types
    integer, parameter, public :: GESTURE_TAP = 1
    integer, parameter, public :: GESTURE_DOUBLE_TAP = 2
    integer, parameter, public :: GESTURE_LONG_PRESS = 3
    integer, parameter, public :: GESTURE_SWIPE_LEFT = 4
    integer, parameter, public :: GESTURE_SWIPE_RIGHT = 5
    integer, parameter, public :: GESTURE_SWIPE_UP = 6
    integer, parameter, public :: GESTURE_SWIPE_DOWN = 7
    integer, parameter, public :: GESTURE_PINCH_IN = 8
    integer, parameter, public :: GESTURE_PINCH_OUT = 9
    integer, parameter, public :: GESTURE_ROTATE = 10

    ! Touch phases
    integer, parameter :: TOUCH_PHASE_BEGAN = 0
    integer, parameter :: TOUCH_PHASE_MOVED = 1
    integer, parameter :: TOUCH_PHASE_STATIONARY = 2
    integer, parameter :: TOUCH_PHASE_ENDED = 3
    integer, parameter :: TOUCH_PHASE_CANCELLED = 4

    ! Gesture recognition parameters
    real(c_float), parameter :: TAP_MAX_DURATION = 0.25  ! seconds
    real(c_float), parameter :: TAP_MAX_MOVEMENT = 10.0  ! pixels
    real(c_float), parameter :: DOUBLE_TAP_MAX_INTERVAL = 0.3  ! seconds
    real(c_float), parameter :: LONG_PRESS_MIN_DURATION = 0.5  ! seconds
    real(c_float), parameter :: SWIPE_MIN_DISTANCE = 50.0  ! pixels
    real(c_float), parameter :: SWIPE_MAX_DURATION = 0.5  ! seconds
    real(c_float), parameter :: PINCH_MIN_DISTANCE = 20.0  ! pixels

    !> @brief Touch gesture data
    type :: forge_touch_gesture
        integer :: gesture_type = 0
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
        real(c_float) :: scale = 1.0  ! For pinch gestures
        real(c_float) :: rotation = 0.0  ! For rotation gestures (radians)
        real(c_float) :: velocity_x = 0.0
        real(c_float) :: velocity_y = 0.0
        integer :: touch_count = 0
    end type forge_touch_gesture

    !> @brief Touch point with history
    type :: touch_point_history
        integer(c_int) :: touch_id = 0
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
        real(c_float) :: pressure = 1.0
        integer :: phase = TOUCH_PHASE_CANCELLED
        real(c_double) :: timestamp = 0.0
        ! History for velocity calculation
        integer(c_int) :: prev_x = 0
        integer(c_int) :: prev_y = 0
        real(c_double) :: prev_timestamp = 0.0
    end type touch_point_history

    !> @brief Gesture recognizer state
    type :: gesture_state
        logical :: active = .false.
        integer :: gesture_type = 0
        real(c_double) :: start_time = 0.0
        integer(c_int) :: start_x = 0
        integer(c_int) :: start_y = 0
        integer :: touch_count = 0
        ! For multi-touch gestures
        real(c_float) :: initial_distance = 0.0
        real(c_float) :: initial_angle = 0.0
        ! For tap/double tap
        integer :: tap_count = 0
        real(c_double) :: last_tap_time = 0.0
    end type gesture_state

    !> @brief Gesture recognizer
    type :: forge_gesture_recognizer
        private
        type(touch_point_history) :: touch_history(MAX_TOUCH_POINTS)
        type(gesture_state) :: current_gesture
        logical :: enabled = .true.
    contains
        procedure :: process_touch_event => gesture_recognizer_process_touch
        procedure :: get_current_gesture => gesture_recognizer_get_gesture
        procedure :: reset => gesture_recognizer_reset
        procedure :: enable => gesture_recognizer_enable
        procedure :: disable => gesture_recognizer_disable
    end type forge_gesture_recognizer

contains

    !> @brief Initialize gesture recognizer
    subroutine init_gesture_recognizer(recognizer)
        type(forge_gesture_recognizer), intent(out) :: recognizer

        recognizer%enabled = .true.
        call recognizer%reset()
    end subroutine init_gesture_recognizer

    !> @brief Process touch events and recognize gestures
    subroutine process_touch_events(recognizer, touch_events, num_events, gestures, num_gestures)
        type(forge_gesture_recognizer), intent(inout) :: recognizer
        type(forge_event), intent(in) :: touch_events(:)
        integer, intent(in) :: num_events
        type(forge_touch_gesture), intent(out) :: gestures(:)
        integer, intent(out) :: num_gestures
        integer :: i

        num_gestures = 0

        do i = 1, num_events
            if (touch_events(i)%event_type%id >= EVENT_TOUCH_BEGAN .and. &
                touch_events(i)%event_type%id <= EVENT_TOUCH_CANCELLED) then

                call recognizer%process_touch_event(touch_events(i))

                ! Check if a gesture was recognized
                if (recognizer%current_gesture%active) then
                    if (num_gestures < size(gestures)) then
                        num_gestures = num_gestures + 1
                        gestures(num_gestures) = recognizer%get_current_gesture()
                    end if
                end if
            end if
        end do
    end subroutine process_touch_events

    !> @brief Process a single touch event
    subroutine gesture_recognizer_process_touch(this, event)
        class(forge_gesture_recognizer), intent(inout) :: this
        type(forge_event), intent(in) :: event
        integer :: touch_index, i
        real(c_double) :: current_time
        real(c_float) :: distance, angle

        if (.not. this%enabled) return

        ! Get current time (simplified - would use system time)
        current_time = get_current_time()

        ! Update touch history
        do i = 1, event%touch_count
            touch_index = find_touch_index(this%touch_history, event%touches(i)%touch_id)
            if (touch_index == 0) then
                ! New touch
                touch_index = find_free_touch_slot(this%touch_history)
                if (touch_index > 0) then
                    this%touch_history(touch_index)%touch_id = event%touches(i)%touch_id
                end if
            end if

            if (touch_index > 0) then
                ! Update touch history
                this%touch_history(touch_index)%prev_x = this%touch_history(touch_index)%x
                this%touch_history(touch_index)%prev_y = this%touch_history(touch_index)%y
                this%touch_history(touch_index)%prev_timestamp = this%touch_history(touch_index)%timestamp

                this%touch_history(touch_index)%x = event%touches(i)%x
                this%touch_history(touch_index)%y = event%touches(i)%y
                this%touch_history(touch_index)%pressure = event%touches(i)%pressure
                this%touch_history(touch_index)%phase = event%touches(i)%phase
                this%touch_history(touch_index)%timestamp = current_time
            end if
        end do

        ! Process gesture recognition based on touch phase
        select case (event%event_type%id)
        case (EVENT_TOUCH_BEGAN)
            call this%handle_touch_began(event, current_time)
        case (EVENT_TOUCH_MOVED)
            call this%handle_touch_moved(event, current_time)
        case (EVENT_TOUCH_ENDED)
            call this%handle_touch_ended(event, current_time)
        case (EVENT_TOUCH_CANCELLED)
            call this%handle_touch_cancelled(event, current_time)
        end select
    end subroutine gesture_recognizer_process_touch

    !> @brief Handle touch began
    subroutine handle_touch_began(this, event, current_time)
        class(forge_gesture_recognizer), intent(inout) :: this
        type(forge_event), intent(in) :: event
        real(c_double), intent(in) :: current_time

        if (event%touch_count == 1) then
            ! Single touch - potential tap or long press
            this%current_gesture%active = .false.
            this%current_gesture%start_time = current_time
            this%current_gesture%start_x = event%touches(1)%x
            this%current_gesture%start_y = event%touches(1)%y
            this%current_gesture%touch_count = 1

        else if (event%touch_count == 2) then
            ! Two touches - potential pinch or rotate
            call calculate_two_touch_params(event%touches(1), event%touches(2), &
                                          this%current_gesture%initial_distance, &
                                          this%current_gesture%initial_angle)
            this%current_gesture%active = .false.
            this%current_gesture%start_time = current_time
            this%current_gesture%touch_count = 2
        end if
    end subroutine handle_touch_began

    !> @brief Handle touch moved
    subroutine handle_touch_moved(this, event, current_time)
        class(forge_gesture_recognizer), intent(inout) :: this
        type(forge_event), intent(in) :: event
        real(c_double), intent(in) :: current_time
        real(c_float) :: current_distance, current_angle, scale, rotation

        if (event%touch_count == 1) then
            ! Check if movement exceeds tap threshold
            if (.not. this%current_gesture%active) then
                if (movement_exceeds_threshold(event%touches(1)%x, event%touches(1)%y, &
                                             this%current_gesture%start_x, &
                                             this%current_gesture%start_y, &
                                             TAP_MAX_MOVEMENT)) then
                    ! Movement too large for tap - potential swipe
                    this%current_gesture%gesture_type = 0  ! Reset
                end if
            end if

        else if (event%touch_count == 2) then
            ! Two finger gesture
            call calculate_two_touch_params(event%touches(1), event%touches(2), &
                                          current_distance, current_angle)

            if (this%current_gesture%initial_distance > 0.0) then
                scale = current_distance / this%current_gesture%initial_distance
                rotation = current_angle - this%current_gesture%initial_angle

                ! Check for pinch gesture
                if (abs(scale - 1.0) > 0.1) then  ! Scale changed significantly
                    if (.not. this%current_gesture%active) then
                        this%current_gesture%active = .true.
                        if (scale > 1.0) then
                            this%current_gesture%gesture_type = GESTURE_PINCH_OUT
                        else
                            this%current_gesture%gesture_type = GESTURE_PINCH_IN
                        end if
                        this%current_gesture%scale = scale
                        this%current_gesture%x = (event%touches(1)%x + event%touches(2)%x) / 2
                        this%current_gesture%y = (event%touches(1)%y + event%touches(2)%y) / 2
                    else
                        this%current_gesture%scale = scale
                    end if
                end if

                ! Check for rotation gesture
                if (abs(rotation) > 0.2) then  ! Rotation changed significantly
                    if (.not. this%current_gesture%active .or. &
                        this%current_gesture%gesture_type /= GESTURE_ROTATE) then
                        this%current_gesture%active = .true.
                        this%current_gesture%gesture_type = GESTURE_ROTATE
                        this%current_gesture%rotation = rotation
                        this%current_gesture%x = (event%touches(1)%x + event%touches(2)%x) / 2
                        this%current_gesture%y = (event%touches(1)%y + event%touches(2)%y) / 2
                    else
                        this%current_gesture%rotation = rotation
                    end if
                end if
            end if
        end if
    end subroutine handle_touch_moved

    !> @brief Handle touch ended
    subroutine handle_touch_ended(this, event, current_time)
        class(forge_gesture_recognizer), intent(inout) :: this
        type(forge_event), intent(in) :: event
        real(c_double), intent(in) :: current_time
        real(c_double) :: duration
        real(c_float) :: distance, velocity_x, velocity_y

        duration = current_time - this%current_gesture%start_time

        if (event%touch_count == 1) then
            if (.not. this%current_gesture%active) then
                ! Check for tap gestures
                if (duration <= TAP_MAX_DURATION) then
                    ! Check if this could be a double tap
                    if (current_time - this%current_gesture%last_tap_time <= DOUBLE_TAP_MAX_INTERVAL) then
                        this%current_gesture%tap_count = this%current_gesture%tap_count + 1
                        if (this%current_gesture%tap_count == 2) then
                            this%current_gesture%active = .true.
                            this%current_gesture%gesture_type = GESTURE_DOUBLE_TAP
                            this%current_gesture%x = event%touches(1)%x
                            this%current_gesture%y = event%touches(1)%y
                            this%current_gesture%tap_count = 0
                        end if
                    else
                        ! Single tap
                        this%current_gesture%active = .true.
                        this%current_gesture%gesture_type = GESTURE_TAP
                        this%current_gesture%x = event%touches(1)%x
                        this%current_gesture%y = event%touches(1)%y
                        this%current_gesture%last_tap_time = current_time
                        this%current_gesture%tap_count = 1
                    end if
                else if (duration >= LONG_PRESS_MIN_DURATION) then
                    ! Long press
                    this%current_gesture%active = .true.
                    this%current_gesture%gesture_type = GESTURE_LONG_PRESS
                    this%current_gesture%x = event%touches(1)%x
                    this%current_gesture%y = event%touches(1)%y
                end if
            else
                ! Check for swipe gesture
                distance = calculate_distance(event%touches(1)%x, event%touches(1)%y, &
                                            this%current_gesture%start_x, &
                                            this%current_gesture%start_y)
                if (distance >= SWIPE_MIN_DISTANCE .and. duration <= SWIPE_MAX_DURATION) then
                    call calculate_velocity(this%touch_history, event%touches(1)%touch_id, &
                                          velocity_x, velocity_y)

                    this%current_gesture%active = .true.
                    this%current_gesture%velocity_x = velocity_x
                    this%current_gesture%velocity_y = velocity_y

                    ! Determine swipe direction
                    if (abs(event%touches(1)%x - this%current_gesture%start_x) > &
                        abs(event%touches(1)%y - this%current_gesture%start_y)) then
                        ! Horizontal swipe
                        if (event%touches(1)%x > this%current_gesture%start_x) then
                            this%current_gesture%gesture_type = GESTURE_SWIPE_RIGHT
                        else
                            this%current_gesture%gesture_type = GESTURE_SWIPE_LEFT
                        end if
                    else
                        ! Vertical swipe
                        if (event%touches(1)%y > this%current_gesture%start_y) then
                            this%current_gesture%gesture_type = GESTURE_SWIPE_DOWN
                        else
                            this%current_gesture%gesture_type = GESTURE_SWIPE_UP
                        end if
                    end if
                end if
            end if
        end if

        ! Clean up ended touches
        call cleanup_ended_touches(this%touch_history, event)
    end subroutine handle_touch_ended

    !> @brief Handle touch cancelled
    subroutine handle_touch_cancelled(this, event, current_time)
        class(forge_gesture_recognizer), intent(inout) :: this
        type(forge_event), intent(in) :: event
        real(c_double), intent(in) :: current_time

        ! Cancel any ongoing gesture
        this%current_gesture%active = .false.
        call cleanup_ended_touches(this%touch_history, event)
    end subroutine handle_touch_cancelled

    !> @brief Get current recognized gesture
    function gesture_recognizer_get_gesture(this) result(gesture)
        class(forge_gesture_recognizer), intent(in) :: this
        type(forge_touch_gesture) :: gesture

        gesture%gesture_type = this%current_gesture%gesture_type
        gesture%x = this%current_gesture%start_x
        gesture%y = this%current_gesture%start_y
        gesture%scale = this%current_gesture%scale
        gesture%rotation = this%current_gesture%rotation
        gesture%velocity_x = this%current_gesture%velocity_x
        gesture%velocity_y = this%current_gesture%velocity_y
        gesture%touch_count = this%current_gesture%touch_count
    end function gesture_recognizer_get_gesture

    !> @brief Reset gesture recognizer
    subroutine gesture_recognizer_reset(this)
        class(forge_gesture_recognizer), intent(inout) :: this

        this%current_gesture%active = .false.
        this%current_gesture%gesture_type = 0
        this%current_gesture%start_time = 0.0
        this%current_gesture%start_x = 0
        this%current_gesture%start_y = 0
        this%current_gesture%touch_count = 0
        this%current_gesture%initial_distance = 0.0
        this%current_gesture%initial_angle = 0.0
        this%current_gesture%tap_count = 0
        this%current_gesture%last_tap_time = 0.0

        this%touch_history%touch_id = 0
        this%touch_history%phase = TOUCH_PHASE_CANCELLED
    end subroutine gesture_recognizer_reset

    !> @brief Enable gesture recognizer
    subroutine gesture_recognizer_enable(this)
        class(forge_gesture_recognizer), intent(inout) :: this
        this%enabled = .true.
    end subroutine gesture_recognizer_enable

    !> @brief Disable gesture recognizer
    subroutine gesture_recognizer_disable(this)
        class(forge_gesture_recognizer), intent(inout) :: this
        this%enabled = .false.
        call this%reset()
    end subroutine gesture_recognizer_disable

    !> @brief Calculate distance and angle between two touch points
    subroutine calculate_two_touch_params(touch1, touch2, distance, angle)
        type(forge_touch_point), intent(in) :: touch1, touch2
        real(c_float), intent(out) :: distance, angle
        real(c_float) :: dx, dy

        dx = real(touch2%x - touch1%x)
        dy = real(touch2%y - touch1%y)
        distance = sqrt(dx*dx + dy*dy)
        angle = atan2(dy, dx)
    end subroutine calculate_two_touch_params

    !> @brief Check if movement exceeds threshold
    function movement_exceeds_threshold(x1, y1, x2, y2, threshold) result(exceeds)
        integer(c_int), intent(in) :: x1, y1, x2, y2
        real(c_float), intent(in) :: threshold
        logical :: exceeds
        real(c_float) :: distance

        distance = sqrt(real((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)))
        exceeds = distance > threshold
    end function movement_exceeds_threshold

    !> @brief Calculate distance between two points
    function calculate_distance(x1, y1, x2, y2) result(distance)
        integer(c_int), intent(in) :: x1, y1, x2, y2
        real(c_float) :: distance

        distance = sqrt(real((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)))
    end function calculate_distance

    !> @brief Calculate velocity from touch history
    subroutine calculate_velocity(touch_history, touch_id, velocity_x, velocity_y)
        type(touch_point_history), intent(in) :: touch_history(:)
        integer(c_int), intent(in) :: touch_id
        real(c_float), intent(out) :: velocity_x, velocity_y
        integer :: index
        real(c_double) :: time_delta

        index = find_touch_index(touch_history, touch_id)
        if (index > 0) then
            time_delta = touch_history(index)%timestamp - touch_history(index)%prev_timestamp
            if (time_delta > 0.0) then
                velocity_x = real(touch_history(index)%x - touch_history(index)%prev_x) / real(time_delta)
                velocity_y = real(touch_history(index)%y - touch_history(index)%prev_y) / real(time_delta)
            else
                velocity_x = 0.0
                velocity_y = 0.0
            end if
        else
            velocity_x = 0.0
            velocity_y = 0.0
        end if
    end subroutine calculate_velocity

    !> @brief Find touch index in history
    function find_touch_index(touch_history, touch_id) result(index)
        type(touch_point_history), intent(in) :: touch_history(:)
        integer(c_int), intent(in) :: touch_id
        integer :: index, i

        index = 0
        do i = 1, size(touch_history)
            if (touch_history(i)%touch_id == touch_id) then
                index = i
                exit
            end if
        end do
    end function find_touch_index

    !> @brief Find free touch slot
    function find_free_touch_slot(touch_history) result(index)
        type(touch_point_history), intent(in) :: touch_history(:)
        integer :: index, i

        index = 0
        do i = 1, size(touch_history)
            if (touch_history(i)%touch_id == 0) then
                index = i
                exit
            end if
        end do
    end function find_free_touch_slot

    !> @brief Clean up ended/cancelled touches
    subroutine cleanup_ended_touches(touch_history, event)
        type(touch_point_history), intent(inout) :: touch_history(:)
        type(forge_event), intent(in) :: event
        integer :: i, history_index

        do i = 1, event%touch_count
            if (event%touches(i)%phase == 2 .or. event%touches(i)%phase == 3) then  ! ended or cancelled
                history_index = find_touch_index(touch_history, event%touches(i)%touch_id)
                if (history_index > 0) then
                    touch_history(history_index)%touch_id = 0
                    touch_history(history_index)%phase = TOUCH_PHASE_CANCELLED
                end if
            end if
        end do
    end subroutine cleanup_ended_touches

    !> @brief Get current time (simplified implementation)
    function get_current_time() result(time)
        real(c_double) :: time
        ! This would use system time in a real implementation
        ! For now, return a dummy value
        time = 0.0_c_double
    end function get_current_time

end module forge_touch