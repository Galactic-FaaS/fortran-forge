!> @brief Basic Animation Example
!> @details Demonstrates basic animation techniques in graphics
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program graphics_animation_basic_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_graphics_view) :: graphics_view
    type(forge_scene) :: scene
    type(forge_label) :: status_label
    type(forge_button) :: start_button, stop_button, reset_button
    type(forge_graphics_item) :: moving_circle, bouncing_ball
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    real :: circle_x = 50.0, circle_y = 100.0
    real :: ball_x = 200.0, ball_y = 200.0, ball_vx = 2.0, ball_vy = 1.5
    logical :: animation_running = .false.

    print '(A)', "=== Basic Animation Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Basic animation techniques"
    print '(A)', "  - Object movement and positioning"
    print '(A)', "  - Animation timing and updates"
    print '(A)', "  - Collision detection and response"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - animations won't actually run"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Basic Animation Example")
        call builder%set_size(600, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create graphics scene
    print '(A)', "Creating graphics scene for animation..."
    call scene%set_scene_rect(0.0d0, 0.0d0, 500.0d0, 400.0d0)
    call scene%set_name("animation_scene")

    ! Create graphics view
    call graphics_view%set_scene(scene)
    call graphics_view%set_name("graphics_view")

    ! Create control buttons
    call start_button%set_label("Start Animation")
    call start_button%set_name("start_button")
    call start_button%on_click(on_start_clicked)

    call stop_button%set_label("Stop Animation")
    call stop_button%set_name("stop_button")
    call stop_button%on_click(on_stop_clicked)

    call reset_button%set_label("Reset")
    call reset_button%set_name("reset_button")
    call reset_button%on_click(on_reset_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Click Start to begin animation")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with animation capabilities..."
    call window%show()

    ! Create initial animated objects
    print '(A)', ""
    print '(A)', "Creating animated objects:"
    call create_animated_objects()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Create the animated objects
    subroutine create_animated_objects()
        type(forge_pen) :: pen
        type(forge_brush) :: brush

        ! Moving circle
        call pen%set_color(forge_color_blue)
        call pen%set_width(2)
        call brush%set_color(forge_color_light_blue)
        call brush%set_style(1)  ! Solid

        call moving_circle%set_rect(circle_x, circle_y, 30.0d0, 30.0d0)  ! Circle
        call moving_circle%set_pen(pen)
        call moving_circle%set_brush(brush)
        call scene%add_item(moving_circle)
        print '(A)', "  Created moving circle"

        ! Bouncing ball
        call pen%set_color(forge_color_red)
        call brush%set_color(forge_color_pink)

        call bouncing_ball%set_rect(ball_x, ball_y, 25.0d0, 25.0d0)  ! Circle
        call bouncing_ball%set_pen(pen)
        call bouncing_ball%set_brush(brush)
        call scene%add_item(bouncing_ball)
        print '(A)', "  Created bouncing ball"
    end subroutine create_animated_objects

    !> @brief Update animation frame
    subroutine update_animation()
        if (.not. animation_running) return

        ! Move the circle horizontally
        circle_x = circle_x + 1.0
        if (circle_x > 470.0) circle_x = 50.0  ! Wrap around

        call moving_circle%set_rect(circle_x, circle_y, 30.0d0, 30.0d0)

        ! Move the bouncing ball
        ball_x = ball_x + ball_vx
        ball_y = ball_y + ball_vy

        ! Bounce off walls
        if (ball_x <= 0.0 .or. ball_x >= 475.0) then
            ball_vx = -ball_vx
            ball_x = max(0.0, min(ball_x, 475.0))
        end if

        if (ball_y <= 0.0 .or. ball_y >= 375.0) then
            ball_vy = -ball_vy
            ball_y = max(0.0, min(ball_y, 375.0))
        end if

        call bouncing_ball%set_rect(ball_x, ball_y, 25.0d0, 25.0d0)

        ! Update status
        write(status_label%get_text(), '("Animation running - Circle: (", F5.1, ",", F5.1, ") Ball: (", F5.1, ",", F5.1, ")")') &
            circle_x, circle_y, ball_x, ball_y
    end subroutine update_animation

    !> @brief Handler for start button click
    subroutine on_start_clicked(event)
        type(forge_event), intent(in) :: event
        animation_running = .true.
        call status_label%set_text("Animation started")
        print '(A)', "  Animation started"
        ! In a real implementation, this would start a timer to call update_animation periodically
    end subroutine on_start_clicked

    !> @brief Handler for stop button click
    subroutine on_stop_clicked(event)
        type(forge_event), intent(in) :: event
        animation_running = .false.
        call status_label%set_text("Animation stopped")
        print '(A)', "  Animation stopped"
    end subroutine on_stop_clicked

    !> @brief Handler for reset button click
    subroutine on_reset_clicked(event)
        type(forge_event), intent(in) :: event
        animation_running = .false.
        circle_x = 50.0
        circle_y = 100.0
        ball_x = 200.0
        ball_y = 200.0
        ball_vx = 2.0
        ball_vy = 1.5

        call moving_circle%set_rect(circle_x, circle_y, 30.0d0, 30.0d0)
        call bouncing_ball%set_rect(ball_x, ball_y, 25.0d0, 25.0d0)

        call status_label%set_text("Animation reset - click Start to begin")
        print '(A)', "  Animation reset"
    end subroutine on_reset_clicked

end program graphics_animation_basic_example