!> @brief Comprehensive animation framework demonstration
!> @details Shows QPropertyAnimation, QParallelAnimationGroup, QSequentialAnimationGroup,
!> QEasingCurve, and all animation features
!> @author ForGE Contributors
!> @date 2025

program animation_demo
    use iso_c_binding
    use forge_types
    use forge_qobject
    use forge_animation
    use forge_signals
    implicit none

    type :: DemoObject
        type(forge_qobject) :: qobj
        integer :: x = 0
        integer :: y = 0
        real :: opacity = 1.0
        character(len=:), allocatable :: color
    contains
        procedure :: set_x => demo_set_x
        procedure :: get_x => demo_get_x
        procedure :: set_y => demo_set_y
        procedure :: get_y => demo_get_y
        procedure :: set_opacity => demo_set_opacity
        procedure :: get_opacity => demo_get_opacity
        procedure :: set_color => demo_set_color
        procedure :: get_color => demo_get_color
    end type DemoObject

    type(DemoObject) :: demo_obj
    type(QPropertyAnimation) :: x_anim, y_anim, opacity_anim
    type(QParallelAnimationGroup) :: parallel_group
    type(QSequentialAnimationGroup) :: sequential_group
    type(QEasingCurve) :: easing_curve
    type(QPauseAnimation) :: pause_anim
    real(real64) :: time_step = 50.0_real64  ! milliseconds
    integer :: i

    ! Initialize demo object
    call demo_obj%qobj%init()
    call demo_obj%qobj%set_object_name("DemoObject")
    demo_obj%color = "red"

    print *, "=== ForGE Animation Framework Demo ==="
    print *, "Initial state: x=", demo_obj%get_x(), ", y=", demo_obj%get_y(), &
             ", opacity=", demo_obj%get_opacity(), ", color=", demo_obj%get_color()

    ! ===== Basic Property Animation Demo =====
    print *, ""
    print *, "=== Basic Property Animation ==="

    ! Animate X property
    call x_anim%set_target_object(demo_obj%qobj)
    call x_anim%set_property_name("x")
    call x_anim%set_start_value(0)
    call x_anim%set_end_value(100)
    call x_anim%set_duration(1000.0_real64)  ! 1 second

    ! Set easing curve
    call easing_curve%set_type(QEasingCurve_InOutQuad)
    call x_anim%set_easing_curve(easing_curve)

    ! Start animation
    call x_anim%start()

    ! Simulate animation frames
    print *, "Animating X from 0 to 100..."
    do i = 1, 20
        call sleep_ms(int(time_step))
        call x_anim%set_current_time(real(i, real64) * 50.0_real64)
        print *, "Frame", i, ": x=", demo_obj%get_x()
    end do

    ! ===== Parallel Animation Group Demo =====
    print *, ""
    print *, "=== Parallel Animation Group ==="

    ! Create parallel animations
    call y_anim%set_target_object(demo_obj%qobj)
    call y_anim%set_property_name("y")
    call y_anim%set_start_value(0)
    call y_anim%set_end_value(50)
    call y_anim%set_duration(500.0_real64)

    call opacity_anim%set_target_object(demo_obj%qobj)
    call opacity_anim%set_property_name("opacity")
    call opacity_anim%set_start_value(1.0)
    call opacity_anim%set_end_value(0.0)
    call opacity_anim%set_duration(500.0_real64)

    ! Add to parallel group
    call parallel_group%add_animation(y_anim)
    call parallel_group%add_animation(opacity_anim)

    ! Start parallel animation
    call parallel_group%start()

    print *, "Running parallel animations (Y and Opacity)..."
    do i = 1, 10
        call sleep_ms(int(time_step))
        call parallel_group%set_current_time(real(i, real64) * 50.0_real64)
        print *, "Frame", i, ": y=", demo_obj%get_y(), ", opacity=", demo_obj%get_opacity()
    end do

    ! ===== Sequential Animation Group Demo =====
    print *, ""
    print *, "=== Sequential Animation Group ==="

    ! Reset positions
    demo_obj%x = 0
    demo_obj%y = 0

    ! Create sequential animations
    call x_anim%set_start_value(0)
    call x_anim%set_end_value(200)
    call x_anim%set_duration(300.0_real64)

    call y_anim%set_start_value(0)
    call y_anim%set_end_value(100)
    call y_anim%set_duration(300.0_real64)

    ! Add pause
    call pause_anim%set_duration(200.0_real64)

    ! Create sequential group
    call sequential_group%add_animation(x_anim)
    call sequential_group%add_pause(200.0_real64)  ! 200ms pause
    call sequential_group%add_animation(y_anim)

    ! Start sequential animation
    call sequential_group%start()

    print *, "Running sequential animations (X -> Pause -> Y)..."
    do i = 1, 16  ! 800ms total
        call sleep_ms(int(time_step))
        call sequential_group%set_current_time(real(i, real64) * 50.0_real64)
        print *, "Frame", i, ": x=", demo_obj%get_x(), ", y=", demo_obj%get_y()
    end do

    ! ===== Easing Curves Demo =====
    print *, ""
    print *, "=== Easing Curves Demo ==="

    demo_obj%x = 0

    ! Test different easing curves
    call test_easing_curve("Linear", QEasingCurve_Linear)
    call test_easing_curve("InQuad", QEasingCurve_InQuad)
    call test_easing_curve("OutQuad", QEasingCurve_OutQuad)
    call test_easing_curve("InOutQuad", QEasingCurve_InOutQuad)
    call test_easing_curve("InSine", QEasingCurve_InSine)
    call test_easing_curve("OutSine", QEasingCurve_OutSine)
    call test_easing_curve("InOutSine", QEasingCurve_InOutSine)

    ! ===== Animation States and Signals Demo =====
    print *, ""
    print *, "=== Animation States and Signals ==="

    demo_obj%x = 0
    call x_anim%set_start_value(0)
    call x_anim%set_end_value(50)
    call x_anim%set_duration(500.0_real64)

    ! Connect to signals
    call x_anim%finished()%connect(animation_finished_callback)
    call x_anim%stateChanged()%connect(animation_state_changed_callback)

    print *, "Starting animation with signal connections..."
    call x_anim%start()

    do i = 1, 10
        call sleep_ms(int(time_step))
        call x_anim%set_current_time(real(i, real64) * 50.0_real64)
        print *, "Frame", i, ": x=", demo_obj%get_x(), ", state=", get_state_name(x_anim%state())
    end do

    ! Pause and resume demo
    print *, "Pausing animation..."
    call x_anim%pause()
    print *, "State after pause:", get_state_name(x_anim%state())

    call sleep_ms(200)

    print *, "Resuming animation..."
    call x_anim%resume()
    print *, "State after resume:", get_state_name(x_anim%state())

    do i = 11, 15
        call sleep_ms(int(time_step))
        call x_anim%set_current_time(real(i, real64) * 50.0_real64)
        print *, "Frame", i, ": x=", demo_obj%get_x(), ", state=", get_state_name(x_anim%state())
    end do

    ! ===== Loop Control Demo =====
    print *, ""
    print *, "=== Loop Control Demo ==="

    demo_obj%x = 0
    call x_anim%set_start_value(0)
    call x_anim%set_end_value(25)
    call x_anim%set_duration(200.0_real64)
    call x_anim%set_loop_count(3)  ! Loop 3 times

    print *, "Starting animation with 3 loops..."
    call x_anim%start()

    do i = 1, 30  ! Enough frames for 3 loops
        call sleep_ms(int(time_step / 2.0))
        call x_anim%set_current_time(real(i, real64) * 20.0_real64)
        print *, "Frame", i, ": x=", demo_obj%get_x(), ", loop=", x_anim%current_loop()
        if (x_anim%state() == QAnimation_Stopped) exit
    end do

    print *, ""
    print *, "=== Demo Complete ==="
    print *, "Final state: x=", demo_obj%get_x(), ", y=", demo_obj%get_y(), &
             ", opacity=", demo_obj%get_opacity(), ", color=", demo_obj%get_color()

contains

    subroutine demo_set_x(this, value)
        class(DemoObject), intent(inout) :: this
        integer, intent(in) :: value
        this%x = value
    end subroutine demo_set_x

    function demo_get_x(this) result(value)
        class(DemoObject), intent(in) :: this
        integer :: value
        value = this%x
    end function demo_get_x

    subroutine demo_set_y(this, value)
        class(DemoObject), intent(inout) :: this
        integer, intent(in) :: value
        this%y = value
    end subroutine demo_set_y

    function demo_get_y(this) result(value)
        class(DemoObject), intent(in) :: this
        integer :: value
        value = this%y
    end function demo_get_y

    subroutine demo_set_opacity(this, value)
        class(DemoObject), intent(inout) :: this
        real, intent(in) :: value
        this%opacity = value
    end subroutine demo_set_opacity

    function demo_get_opacity(this) result(value)
        class(DemoObject), intent(in) :: this
        real :: value
        value = this%opacity
    end function demo_get_opacity

    subroutine demo_set_color(this, value)
        class(DemoObject), intent(inout) :: this
        character(len=*), intent(in) :: value
        this%color = value
    end subroutine demo_set_color

    function demo_get_color(this) result(value)
        class(DemoObject), intent(in) :: this
        character(len=:), allocatable :: value
        value = this%color
    end function demo_get_color

    subroutine test_easing_curve(name, curve_type)
        character(len=*), intent(in) :: name
        integer, intent(in) :: curve_type
        type(QPropertyAnimation) :: test_anim
        type(QEasingCurve) :: test_curve
        integer :: j

        demo_obj%x = 0
        call test_anim%set_target_object(demo_obj%qobj)
        call test_anim%set_property_name("x")
        call test_anim%set_start_value(0)
        call test_anim%set_end_value(10)
        call test_anim%set_duration(500.0_real64)

        call test_curve%set_type(curve_type)
        call test_anim%set_easing_curve(test_curve)

        call test_anim%start()

        print *, "Testing", name, "easing:"
        do j = 1, 10
            call test_anim%set_current_time(real(j, real64) * 50.0_real64)
            print *, "  ", name, "frame", j, ": x=", demo_obj%get_x()
        end do
        print *, ""
    end subroutine test_easing_curve

    function get_state_name(state) result(name)
        integer, intent(in) :: state
        character(len=:), allocatable :: name

        select case (state)
        case (QAnimation_Stopped)
            name = "Stopped"
        case (QAnimation_Paused)
            name = "Paused"
        case (QAnimation_Running)
            name = "Running"
        case default
            name = "Unknown"
        end select
    end function get_state_name

    subroutine animation_finished_callback()
        print *, "Signal: Animation finished!"
    end subroutine animation_finished_callback

    subroutine animation_state_changed_callback(new_state)
        integer, intent(in) :: new_state
        print *, "Signal: Animation state changed to", get_state_name(new_state)
    end subroutine animation_state_changed_callback

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        integer :: start_time, current_time, rate
        call system_clock(count=start_time, count_rate=rate)
        do
            call system_clock(count=current_time)
            if (real(current_time - start_time) / real(rate) >= milliseconds / 1000.0) exit
        end do
    end subroutine sleep_ms

end program animation_demo