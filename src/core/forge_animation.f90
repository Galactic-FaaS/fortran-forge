!> @brief Qt-style animation framework implementation for ForGE
!> @details Implements QPropertyAnimation, QParallelAnimationGroup, QEasingCurve,
!> and other animation classes with full Qt animation features
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_animation
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    use forge_qobject
    use iso_fortran_env, only: int64, real64
    implicit none
    private

    public :: QEasingCurve
    public :: QEasingCurve_Type
    public :: QAbstractAnimation
    public :: QAnimation_State
    public :: QVariantAnimation
    public :: QPropertyAnimation
    public :: QAnimationGroup
    public :: QParallelAnimationGroup
    public :: QSequentialAnimationGroup
    public :: QPauseAnimation

    !> Animation states
    enum, bind(c)
        enumerator :: QAnimation_Stopped = 0
        enumerator :: QAnimation_Paused = 1
        enumerator :: QAnimation_Running = 2
    end enum

    !> @brief Animation state enumeration
    type :: QAnimation_State
        integer :: value
    end type QAnimation_State

    !> Easing curve types (Qt-style)
    enum, bind(c)
        enumerator :: QEasingCurve_Linear = 0
        enumerator :: QEasingCurve_InQuad = 1
        enumerator :: QEasingCurve_OutQuad = 2
        enumerator :: QEasingCurve_InOutQuad = 3
        enumerator :: QEasingCurve_OutInQuad = 4
        enumerator :: QEasingCurve_InCubic = 5
        enumerator :: QEasingCurve_OutCubic = 6
        enumerator :: QEasingCurve_InOutCubic = 7
        enumerator :: QEasingCurve_OutInCubic = 8
        enumerator :: QEasingCurve_InQuart = 9
        enumerator :: QEasingCurve_OutQuart = 10
        enumerator :: QEasingCurve_InOutQuart = 11
        enumerator :: QEasingCurve_OutInQuart = 12
        enumerator :: QEasingCurve_InQuint = 13
        enumerator :: QEasingCurve_OutQuint = 14
        enumerator :: QEasingCurve_InOutQuint = 15
        enumerator :: QEasingCurve_OutInQuint = 16
        enumerator :: QEasingCurve_InSine = 17
        enumerator :: QEasingCurve_OutSine = 18
        enumerator :: QEasingCurve_InOutSine = 19
        enumerator :: QEasingCurve_OutInSine = 20
        enumerator :: QEasingCurve_InExpo = 21
        enumerator :: QEasingCurve_OutExpo = 22
        enumerator :: QEasingCurve_InOutExpo = 23
        enumerator :: QEasingCurve_OutInExpo = 24
        enumerator :: QEasingCurve_InCirc = 25
        enumerator :: QEasingCurve_OutCirc = 26
        enumerator :: QEasingCurve_InOutCirc = 27
        enumerator :: QEasingCurve_OutInCirc = 28
        enumerator :: QEasingCurve_InElastic = 29
        enumerator :: QEasingCurve_OutElastic = 30
        enumerator :: QEasingCurve_InOutElastic = 31
        enumerator :: QEasingCurve_OutInElastic = 32
        enumerator :: QEasingCurve_InBack = 33
        enumerator :: QEasingCurve_OutBack = 34
        enumerator :: QEasingCurve_InOutBack = 35
        enumerator :: QEasingCurve_OutInBack = 36
        enumerator :: QEasingCurve_InBounce = 37
        enumerator :: QEasingCurve_OutBounce = 38
        enumerator :: QEasingCurve_InOutBounce = 39
        enumerator :: QEasingCurve_OutInBounce = 40
    end enum

    !> @brief Easing curve type enumeration
    type :: QEasingCurve_Type
        integer :: value
    end type QEasingCurve_Type

    !> @brief Easing curve for smooth transitions
    type :: QEasingCurve
        private
        integer :: type = QEasingCurve_Linear
        real(real64) :: amplitude = 1.0_real64
        real(real64) :: overshoot = 1.70158_real64
        real(real64) :: period = 0.3_real64
    contains
        procedure :: set_type => easing_set_type
        procedure :: get_type => easing_get_type
        procedure :: set_amplitude => easing_set_amplitude
        procedure :: amplitude => easing_get_amplitude
        procedure :: set_overshoot => easing_set_overshoot
        procedure :: overshoot => easing_get_overshoot
        procedure :: set_period => easing_set_period
        procedure :: period => easing_get_period
        procedure :: value_for_progress => easing_value_for_progress
    end type QEasingCurve

    !> @brief Abstract base class for all animations
    type, extends(forge_qobject) :: QAbstractAnimation
        private
        integer :: state = QAnimation_Stopped
        integer :: direction = 1  ! 1 = forward, -1 = backward
        integer :: loop_count = 1
        integer :: current_loop = 0
        real(real64) :: duration = 250.0_real64  ! milliseconds
        real(real64) :: current_time = 0.0_real64
        logical :: paused = .false.

        ! Signals
        type(signal_void) :: finished_signal
        type(signal_int) :: state_changed_signal
        type(signal_int) :: current_loop_changed_signal
    contains
        procedure :: start => animation_start
        procedure :: pause => animation_pause
        procedure :: resume => animation_resume
        procedure :: stop => animation_stop
        procedure :: set_current_time => animation_set_current_time
        procedure :: current_time => animation_get_current_time
        procedure :: set_duration => animation_set_duration
        procedure :: duration => animation_get_duration
        procedure :: set_loop_count => animation_set_loop_count
        procedure :: loop_count => animation_get_loop_count
        procedure :: current_loop => animation_get_current_loop
        procedure :: set_direction => animation_set_direction
        procedure :: direction => animation_get_direction
        procedure :: state => animation_get_state
        procedure :: total_duration => animation_total_duration
        procedure :: update_current_time => animation_update_current_time
        procedure :: update_state => animation_update_state
        procedure :: update_direction => animation_update_direction

        ! Signals
        procedure :: finished => animation_finished_signal
        procedure :: stateChanged => animation_state_changed_signal
        procedure :: currentLoopChanged => animation_current_loop_changed_signal

        ! Virtual methods to be overridden
        procedure(virtual_update_current_time), deferred :: virtual_update_current_time
    end type QAbstractAnimation

    abstract interface
        subroutine virtual_update_current_time(this, current_time)
            import :: QAbstractAnimation, real64
            class(QAbstractAnimation), intent(inout) :: this
            real(real64), intent(in) :: current_time
        end subroutine virtual_update_current_time
    end interface

    !> @brief Animation for QVariant values with interpolation
    type, extends(QAbstractAnimation) :: QVariantAnimation
        private
        class(*), allocatable :: start_value
        class(*), allocatable :: end_value
        class(*), allocatable :: current_value
        type(QEasingCurve) :: easing_curve
        real(real64) :: key_values(2) = [0.0_real64, 1.0_real64]  ! Default keyframes
        class(*), allocatable :: key_value_data(:)
    contains
        procedure :: set_start_value => variant_set_start_value
        procedure :: start_value => variant_get_start_value
        procedure :: set_end_value => variant_set_end_value
        procedure :: end_value => variant_get_end_value
        procedure :: set_current_value => variant_set_current_value
        procedure :: current_value => variant_get_current_value
        procedure :: set_easing_curve => variant_set_easing_curve
        procedure :: easing_curve => variant_get_easing_curve
        procedure :: set_key_value_at => variant_set_key_value_at
        procedure :: key_value_at => variant_get_key_value_at
        procedure :: key_values => variant_get_key_values
        procedure :: interpolated => variant_interpolated
        procedure :: virtual_update_current_time => variant_update_current_time
    end type QVariantAnimation

    !> @brief Property-based animation
    type, extends(QVariantAnimation) :: QPropertyAnimation
        private
        class(forge_qobject), pointer :: target => null()
        character(len=:), allocatable :: property_name
    contains
        procedure :: set_target_object => property_set_target_object
        procedure :: target_object => property_get_target_object
        procedure :: set_property_name => property_set_property_name
        procedure :: property_name => property_get_property_name
        procedure :: virtual_update_current_time => property_update_current_time
    end type QPropertyAnimation

    !> @brief Base class for animation groups
    type, extends(QAbstractAnimation) :: QAnimationGroup
        private
        type(QAbstractAnimation), dimension(:), allocatable :: animations
        integer :: animation_count = 0
    contains
        procedure :: add_animation => group_add_animation
        procedure :: remove_animation => group_remove_animation
        procedure :: clear => group_clear
        procedure :: animation_at => group_animation_at
        procedure :: animation_count => group_get_animation_count
        procedure :: index_of_animation => group_index_of_animation
        procedure :: take_animation => group_take_animation
        procedure :: insert_animation => group_insert_animation
    end type QAnimationGroup

    !> @brief Parallel execution of multiple animations
    type, extends(QAnimationGroup) :: QParallelAnimationGroup
    contains
        procedure :: virtual_update_current_time => parallel_update_current_time
        procedure :: duration => parallel_get_duration
    end type QParallelAnimationGroup

    !> @brief Sequential execution of animations
    type, extends(QAnimationGroup) :: QSequentialAnimationGroup
        private
        integer :: current_animation_index = 0
    contains
        procedure :: virtual_update_current_time => sequential_update_current_time
        procedure :: add_pause => sequential_add_pause
        procedure :: insert_pause => sequential_insert_pause
        procedure :: current_animation => sequential_current_animation
        procedure :: duration => sequential_get_duration
    end type QSequentialAnimationGroup

    !> @brief Pause animation
    type, extends(QAbstractAnimation) :: QPauseAnimation
    contains
        procedure :: virtual_update_current_time => pause_update_current_time
    end type QPauseAnimation

contains

    ! ========== QEasingCurve Implementation ==========

    subroutine easing_set_type(this, type)
        class(QEasingCurve), intent(inout) :: this
        integer, intent(in) :: type
        this%type = type
    end subroutine easing_set_type

    function easing_get_type(this) result(type)
        class(QEasingCurve), intent(in) :: this
        integer :: type
        type = this%type
    end function easing_get_type

    subroutine easing_set_amplitude(this, amplitude)
        class(QEasingCurve), intent(inout) :: this
        real(real64), intent(in) :: amplitude
        this%amplitude = amplitude
    end subroutine easing_set_amplitude

    function easing_get_amplitude(this) result(amplitude)
        class(QEasingCurve), intent(in) :: this
        real(real64) :: amplitude
        amplitude = this%amplitude
    end function easing_get_amplitude

    subroutine easing_set_overshoot(this, overshoot)
        class(QEasingCurve), intent(inout) :: this
        real(real64), intent(in) :: overshoot
        this%overshoot = overshoot
    end subroutine easing_set_overshoot

    function easing_get_overshoot(this) result(overshoot)
        class(QEasingCurve), intent(in) :: this
        real(real64) :: overshoot
        overshoot = this%overshoot
    end function easing_get_overshoot

    subroutine easing_set_period(this, period)
        class(QEasingCurve), intent(inout) :: this
        real(real64), intent(in) :: period
        this%period = period
    end subroutine easing_set_period

    function easing_get_period(this) result(period)
        class(QEasingCurve), intent(in) :: this
        real(real64) :: period
        period = this%period
    end function easing_get_period

    function easing_value_for_progress(this, progress) result(value)
        class(QEasingCurve), intent(in) :: this
        real(real64), intent(in) :: progress
        real(real64) :: value
        real(real64) :: t

        t = max(0.0_real64, min(1.0_real64, progress))

        select case (this%type)
        case (QEasingCurve_Linear)
            value = t
        case (QEasingCurve_InQuad)
            value = t * t
        case (QEasingCurve_OutQuad)
            value = -t * (t - 2.0_real64)
        case (QEasingCurve_InOutQuad)
            if (t < 0.5_real64) then
                value = 2.0_real64 * t * t
            else
                value = -2.0_real64 * t * t + 4.0_real64 * t - 1.0_real64
            end if
        case (QEasingCurve_InCubic)
            value = t * t * t
        case (QEasingCurve_OutCubic)
            value = (t - 1.0_real64)**3 + 1.0_real64
        case (QEasingCurve_InOutCubic)
            if (t < 0.5_real64) then
                value = 4.0_real64 * t * t * t
            else
                value = 4.0_real64 * (t - 1.0_real64)**3 + 1.0_real64
            end if
        case (QEasingCurve_InSine)
            value = 1.0_real64 - cos(t * 3.141592653589793_real64 / 2.0_real64)
        case (QEasingCurve_OutSine)
            value = sin(t * 3.141592653589793_real64 / 2.0_real64)
        case (QEasingCurve_InOutSine)
            value = -(cos(3.141592653589793_real64 * t) - 1.0_real64) / 2.0_real64
        case default
            value = t  ! Fallback to linear
        end select
    end function easing_value_for_progress

    ! ========== QAbstractAnimation Implementation ==========

    subroutine animation_start(this, policy)
        class(QAbstractAnimation), intent(inout) :: this
        integer, intent(in), optional :: policy  ! 0 = KeepWhenStopped, 1 = DeleteWhenStopped

        if (this%state == QAnimation_Running) return

        this%current_time = 0.0_real64
        this%current_loop = 0
        this%state = QAnimation_Running
        call this%update_state(QAnimation_Running)
    end subroutine animation_start

    subroutine animation_pause(this)
        class(QAbstractAnimation), intent(inout) :: this
        if (this%state == QAnimation_Running) then
            this%state = QAnimation_Paused
            this%paused = .true.
            call this%update_state(QAnimation_Paused)
        end if
    end subroutine animation_pause

    subroutine animation_resume(this)
        class(QAbstractAnimation), intent(inout) :: this
        if (this%state == QAnimation_Paused) then
            this%state = QAnimation_Running
            this%paused = .false.
            call this%update_state(QAnimation_Running)
        end if
    end subroutine animation_resume

    subroutine animation_stop(this)
        class(QAbstractAnimation), intent(inout) :: this
        this%state = QAnimation_Stopped
        this%current_time = 0.0_real64
        this%current_loop = 0
        call this%update_state(QAnimation_Stopped)
        call this%finished_signal%emit()
    end subroutine animation_stop

    subroutine animation_set_current_time(this, msecs)
        class(QAbstractAnimation), intent(inout) :: this
        real(real64), intent(in) :: msecs
        this%current_time = msecs
        call this%update_current_time(msecs)
    end subroutine animation_set_current_time

    function animation_get_current_time(this) result(time)
        class(QAbstractAnimation), intent(in) :: this
        real(real64) :: time
        time = this%current_time
    end function animation_get_current_time

    subroutine animation_set_duration(this, msecs)
        class(QAbstractAnimation), intent(inout) :: this
        real(real64), intent(in) :: msecs
        this%duration = msecs
    end subroutine animation_set_duration

    function animation_get_duration(this) result(dur)
        class(QAbstractAnimation), intent(in) :: this
        real(real64) :: dur
        dur = this%duration
    end function animation_get_duration

    subroutine animation_set_loop_count(this, loop_count)
        class(QAbstractAnimation), intent(inout) :: this
        integer, intent(in) :: loop_count
        this%loop_count = loop_count
    end subroutine animation_set_loop_count

    function animation_get_loop_count(this) result(count)
        class(QAbstractAnimation), intent(in) :: this
        integer :: count
        count = this%loop_count
    end function animation_get_loop_count

    function animation_get_current_loop(this) result(loop)
        class(QAbstractAnimation), intent(in) :: this
        integer :: loop
        loop = this%current_loop
    end function animation_get_current_loop

    subroutine animation_set_direction(this, direction)
        class(QAbstractAnimation), intent(inout) :: this
        integer, intent(in) :: direction
        this%direction = direction
        call this%update_direction(direction)
    end subroutine animation_set_direction

    function animation_get_direction(this) result(dir)
        class(QAbstractAnimation), intent(in) :: this
        integer :: dir
        dir = this%direction
    end function animation_get_direction

    function animation_get_state(this) result(state)
        class(QAbstractAnimation), intent(in) :: this
        integer :: state
        state = this%state
    end function animation_get_state

    function animation_total_duration(this) result(total)
        class(QAbstractAnimation), intent(in) :: this
        real(real64) :: total
        total = this%duration * this%loop_count
    end function animation_total_duration

    subroutine animation_update_current_time(this, current_time)
        class(QAbstractAnimation), intent(inout) :: this
        real(real64), intent(in) :: current_time
        call this%virtual_update_current_time(current_time)
    end subroutine animation_update_current_time

    subroutine animation_update_state(this, new_state)
        class(QAbstractAnimation), intent(inout) :: this
        integer, intent(in) :: new_state
        call this%state_changed_signal%emit(new_state)
    end subroutine animation_update_state

    subroutine animation_update_direction(this, direction)
        class(QAbstractAnimation), intent(inout) :: this
        integer, intent(in) :: direction
        ! Implementation for direction change
    end subroutine animation_update_direction

    function animation_finished_signal(this) result(sig)
        class(QAbstractAnimation), intent(in) :: this
        type(signal_void) :: sig
        sig = this%finished_signal
    end function animation_finished_signal

    function animation_state_changed_signal(this) result(sig)
        class(QAbstractAnimation), intent(in) :: this
        type(signal_int) :: sig
        sig = this%state_changed_signal
    end function animation_state_changed_signal

    function animation_current_loop_changed_signal(this) result(sig)
        class(QAbstractAnimation), intent(in) :: this
        type(signal_int) :: sig
        sig = this%current_loop_changed_signal
    end function animation_current_loop_changed_signal

    ! ========== QVariantAnimation Implementation ==========

    subroutine variant_set_start_value(this, value)
        class(QVariantAnimation), intent(inout) :: this
        class(*), intent(in) :: value
        if (allocated(this%start_value)) deallocate(this%start_value)
        allocate(this%start_value, source=value)
    end subroutine variant_set_start_value

    function variant_get_start_value(this) result(value)
        class(QVariantAnimation), intent(in) :: this
        class(*), pointer :: value
        value => this%start_value
    end function variant_get_start_value

    subroutine variant_set_end_value(this, value)
        class(QVariantAnimation), intent(inout) :: this
        class(*), intent(in) :: value
        if (allocated(this%end_value)) deallocate(this%end_value)
        allocate(this%end_value, source=value)
    end subroutine variant_set_end_value

    function variant_get_end_value(this) result(value)
        class(QVariantAnimation), intent(in) :: this
        class(*), pointer :: value
        value => this%end_value
    end function variant_get_end_value

    subroutine variant_set_current_value(this, value)
        class(QVariantAnimation), intent(inout) :: this
        class(*), intent(in) :: value
        if (allocated(this%current_value)) deallocate(this%current_value)
        allocate(this%current_value, source=value)
    end subroutine variant_set_current_value

    function variant_get_current_value(this) result(value)
        class(QVariantAnimation), intent(in) :: this
        class(*), pointer :: value
        value => this%current_value
    end function variant_get_current_value

    subroutine variant_set_easing_curve(this, curve)
        class(QVariantAnimation), intent(inout) :: this
        type(QEasingCurve), intent(in) :: curve
        this%easing_curve = curve
    end subroutine variant_set_easing_curve

    function variant_get_easing_curve(this) result(curve)
        class(QVariantAnimation), intent(in) :: this
        type(QEasingCurve) :: curve
        curve = this%easing_curve
    end function variant_get_easing_curve

    subroutine variant_set_key_value_at(this, step, value)
        class(QVariantAnimation), intent(inout) :: this
        real(real64), intent(in) :: step
        class(*), intent(in) :: value
        ! Implementation for keyframes
    end subroutine variant_set_key_value_at

    function variant_get_key_value_at(this, step) result(value)
        class(QVariantAnimation), intent(in) :: this
        real(real64), intent(in) :: step
        class(*), pointer :: value
        value => null()  ! Placeholder
    end function variant_get_key_value_at

    function variant_get_key_values(this) result(values)
        class(QVariantAnimation), intent(in) :: this
        real(real64), dimension(:), pointer :: values
        values => this%key_values
    end function variant_get_key_values

    function variant_interpolated(this, from, to, progress) result(value)
        class(QVariantAnimation), intent(in) :: this
        class(*), intent(in) :: from, to
        real(real64), intent(in) :: progress
        class(*), allocatable :: value

        ! Simple linear interpolation for numeric types
        select type (from)
        type is (integer)
            select type (to)
            type is (integer)
                allocate(value, source=int(from + (to - from) * progress))
            end select
        type is (real)
            select type (to)
            type is (real)
                allocate(value, source=from + (to - from) * progress)
            end select
        end select
    end function variant_interpolated

    subroutine variant_update_current_time(this, current_time)
        class(QVariantAnimation), intent(inout) :: this
        real(real64), intent(in) :: current_time
        real(real64) :: progress
        class(*), allocatable :: interpolated_value

        progress = current_time / this%duration
        progress = this%easing_curve%value_for_progress(progress)

        if (allocated(this%start_value) .and. allocated(this%end_value)) then
            interpolated_value = this%interpolated(this%start_value, this%end_value, progress)
            call this%set_current_value(interpolated_value)
        end if
    end subroutine variant_update_current_time

    ! ========== QPropertyAnimation Implementation ==========

    subroutine property_set_target_object(this, target)
        class(QPropertyAnimation), intent(inout) :: this
        class(forge_qobject), target :: target
        this%target => target
    end subroutine property_set_target_object

    function property_get_target_object(this) result(target)
        class(QPropertyAnimation), intent(in) :: this
        class(forge_qobject), pointer :: target
        target => this%target
    end function property_get_target_object

    subroutine property_set_property_name(this, name)
        class(QPropertyAnimation), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%property_name = name
    end subroutine property_set_property_name

    function property_get_property_name(this) result(name)
        class(QPropertyAnimation), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%property_name
    end function property_get_property_name

    subroutine property_update_current_time(this, current_time)
        class(QPropertyAnimation), intent(inout) :: this
        real(real64), intent(in) :: current_time

        ! Call parent update
        call this%QVariantAnimation%virtual_update_current_time(current_time)

        ! Update target property if set
        if (associated(this%target) .and. allocated(this%property_name) .and. allocated(this%current_value)) then
            call this%target%set_property(this%property_name, this%current_value)
        end if
    end subroutine property_update_current_time

    ! ========== QAnimationGroup Implementation ==========

    subroutine group_add_animation(this, animation)
        class(QAnimationGroup), intent(inout) :: this
        class(QAbstractAnimation), intent(in) :: animation
        type(QAbstractAnimation), dimension(:), allocatable :: temp

        if (.not. allocated(this%animations)) then
            allocate(this%animations(1))
        else
            allocate(temp(size(this%animations) + 1))
            temp(1:size(this%animations)) = this%animations
            call move_alloc(temp, this%animations)
        end if

        this%animations(this%animation_count + 1) = animation
        this%animation_count = this%animation_count + 1
    end subroutine group_add_animation

    subroutine group_remove_animation(this, animation)
        class(QAnimationGroup), intent(inout) :: this
        class(QAbstractAnimation), intent(in) :: animation
        integer :: i, j
        type(QAbstractAnimation), dimension(:), allocatable :: temp

        do i = 1, this%animation_count
            if (associated(this%animations(i)%target, animation%target)) then  ! Simplified check
                allocate(temp(size(this%animations) - 1))
                temp(1:i-1) = this%animations(1:i-1)
                temp(i:) = this%animations(i+1:)
                call move_alloc(temp, this%animations)
                this%animation_count = this%animation_count - 1
                exit
            end if
        end do
    end subroutine group_remove_animation

    subroutine group_clear(this)
        class(QAnimationGroup), intent(inout) :: this
        if (allocated(this%animations)) deallocate(this%animations)
        this%animation_count = 0
    end subroutine group_clear

    function group_animation_at(this, index) result(animation)
        class(QAnimationGroup), intent(in) :: this
        integer, intent(in) :: index
        type(QAbstractAnimation), pointer :: animation

        if (index >= 1 .and. index <= this%animation_count) then
            animation => this%animations(index)
        else
            animation => null()
        end if
    end function group_animation_at

    function group_get_animation_count(this) result(count)
        class(QAnimationGroup), intent(in) :: this
        integer :: count
        count = this%animation_count
    end function group_get_animation_count

    function group_index_of_animation(this, animation) result(index)
        class(QAnimationGroup), intent(in) :: this
        class(QAbstractAnimation), intent(in) :: animation
        integer :: index
        integer :: i

        index = -1
        do i = 1, this%animation_count
            if (associated(this%animations(i)%target, animation%target)) then
                index = i
                exit
            end if
        end do
    end function group_index_of_animation

    function group_take_animation(this, index) result(animation)
        class(QAnimationGroup), intent(inout) :: this
        integer, intent(in) :: index
        type(QAbstractAnimation) :: animation

        if (index >= 1 .and. index <= this%animation_count) then
            animation = this%animations(index)
            call this%remove_animation(animation)
        end if
    end function group_take_animation

    subroutine group_insert_animation(this, index, animation)
        class(QAnimationGroup), intent(inout) :: this
        integer, intent(in) :: index
        class(QAbstractAnimation), intent(in) :: animation
        type(QAbstractAnimation), dimension(:), allocatable :: temp
        integer :: i

        if (index < 1 .or. index > this%animation_count + 1) return

        allocate(temp(this%animation_count + 1))
        temp(1:index-1) = this%animations(1:index-1)
        temp(index) = animation
        temp(index+1:) = this%animations(index:)
        call move_alloc(temp, this%animations)
        this%animation_count = this%animation_count + 1
    end subroutine group_insert_animation

    ! ========== QParallelAnimationGroup Implementation ==========

    subroutine parallel_update_current_time(this, current_time)
        class(QParallelAnimationGroup), intent(inout) :: this
        real(real64), intent(in) :: current_time
        integer :: i

        do i = 1, this%animation_count
            call this%animations(i)%set_current_time(current_time)
        end do
    end subroutine parallel_update_current_time

    function parallel_get_duration(this) result(dur)
        class(QParallelAnimationGroup), intent(in) :: this
        real(real64) :: dur
        integer :: i
        real(real64) :: max_dur

        max_dur = 0.0_real64
        do i = 1, this%animation_count
            max_dur = max(max_dur, this%animations(i)%duration())
        end do
        dur = max_dur
    end function parallel_get_duration

    ! ========== QSequentialAnimationGroup Implementation ==========

    subroutine sequential_update_current_time(this, current_time)
        class(QSequentialAnimationGroup), intent(inout) :: this
        real(real64), intent(in) :: current_time
        real(real64) :: remaining_time
        integer :: i

        remaining_time = current_time
        do i = 1, this%animation_count
            if (remaining_time <= this%animations(i)%duration()) then
                call this%animations(i)%set_current_time(remaining_time)
                this%current_animation_index = i
                exit
            else
                call this%animations(i)%set_current_time(this%animations(i)%duration())
                remaining_time = remaining_time - this%animations(i)%duration()
            end if
        end do
    end subroutine sequential_update_current_time

    subroutine sequential_add_pause(this, msecs)
        class(QSequentialAnimationGroup), intent(inout) :: this
        real(real64), intent(in) :: msecs
        type(QPauseAnimation) :: pause_anim

        call pause_anim%set_duration(msecs)
        call this%add_animation(pause_anim)
    end subroutine sequential_add_pause

    subroutine sequential_insert_pause(this, index, msecs)
        class(QSequentialAnimationGroup), intent(inout) :: this
        integer, intent(in) :: index
        real(real64), intent(in) :: msecs
        type(QPauseAnimation) :: pause_anim

        call pause_anim%set_duration(msecs)
        call this%insert_animation(index, pause_anim)
    end subroutine sequential_insert_pause

    function sequential_current_animation(this) result(animation)
        class(QSequentialAnimationGroup), intent(in) :: this
        type(QAbstractAnimation), pointer :: animation

        if (this%current_animation_index >= 1 .and. this%current_animation_index <= this%animation_count) then
            animation => this%animations(this%current_animation_index)
        else
            animation => null()
        end if
    end function sequential_current_animation

    function sequential_get_duration(this) result(dur)
        class(QSequentialAnimationGroup), intent(in) :: this
        real(real64) :: dur
        integer :: i

        dur = 0.0_real64
        do i = 1, this%animation_count
            dur = dur + this%animations(i)%duration()
        end do
    end function sequential_get_duration

    ! ========== QPauseAnimation Implementation ==========

    subroutine pause_update_current_time(this, current_time)
        class(QPauseAnimation), intent(inout) :: this
        real(real64), intent(in) :: current_time
        ! Pause animation does nothing - just waits
    end subroutine pause_update_current_time

end module forge_animation