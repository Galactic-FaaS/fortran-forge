!> @brief Graphics Colors Example
!> @details Demonstrates color usage in graphics operations
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program graphics_colors_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_graphics_view) :: graphics_view
    type(forge_scene) :: scene
    type(forge_label) :: status_label
    type(forge_button) :: rgb_button, named_button, alpha_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Graphics Colors Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - RGB color specification"
    print '(A)', "  - Named color constants"
    print '(A)', "  - Alpha transparency"
    print '(A)', "  - Color gradients and patterns"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - graphics won't actually render"
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
        call builder%set_title("Graphics Colors Example")
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
    print '(A)', "Creating graphics scene for color demonstration..."
    call scene%set_scene_rect(0.0d0, 0.0d0, 500.0d0, 400.0d0)
    call scene%set_name("colors_scene")

    ! Create graphics view
    call graphics_view%set_scene(scene)
    call graphics_view%set_name("graphics_view")

    ! Create control buttons
    call rgb_button%set_label("RGB Colors")
    call rgb_button%set_name("rgb_button")
    call rgb_button%on_click(on_rgb_clicked)

    call named_button%set_label("Named Colors")
    call named_button%set_name("named_button")
    call named_button%on_click(on_named_clicked)

    call alpha_button%set_label("Alpha/Transparency")
    call alpha_button%set_name("alpha_button")
    call alpha_button%on_click(on_alpha_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Click buttons to see different color demonstrations")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with color graphics..."
    call window%show()

    ! Draw initial RGB colors
    print '(A)', ""
    print '(A)', "Drawing initial RGB color demonstration:"
    call draw_rgb_colors()

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

    !> @brief Draw RGB color demonstration
    subroutine draw_rgb_colors()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: rect_item
        integer :: i, j
        real :: r, g, b

        call scene%clear()

        ! Draw a grid of RGB colors
        do i = 0, 7
            do j = 0, 7
                r = real(i) / 7.0
                g = real(j) / 7.0
                b = 0.5  ! Fixed blue component

                call brush%set_color_rgb(r, g, b, 1.0)  ! Opaque
                call pen%set_color_rgb(0.0, 0.0, 0.0, 1.0)  ! Black border
                call pen%set_width(1)

                call rect_item%set_rect(real(i*50), real(j*50), 50.0d0, 50.0d0)
                call rect_item%set_pen(pen)
                call rect_item%set_brush(brush)
                call scene%add_item(rect_item)
            end do
        end do

        print '(A)', "  Added 64 RGB color rectangles (8x8 grid)"
        call status_label%set_text("RGB Colors: Red varies horizontally, Green vertically, Blue fixed")
    end subroutine draw_rgb_colors

    !> @brief Draw named color demonstration
    subroutine draw_named_colors()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: rect_item

        call scene%clear()

        ! Red rectangle
        call brush%set_color(forge_color_red)
        call pen%set_color(forge_color_black)
        call rect_item%set_rect(50.0d0, 50.0d0, 100.0d0, 80.0d0)
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added red rectangle"

        ! Green circle
        call brush%set_color(forge_color_green)
        call rect_item%set_rect(200.0d0, 50.0d0, 100.0d0, 100.0d0)  ! Circle
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added green circle"

        ! Blue triangle
        call brush%set_color(forge_color_blue)
        call rect_item%set_polygon([350.0d0, 50.0d0, 400.0d0, 130.0d0, 300.0d0, 130.0d0, 350.0d0, 50.0d0])
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added blue triangle"

        ! Yellow rectangle
        call brush%set_color(forge_color_yellow)
        call rect_item%set_rect(50.0d0, 200.0d0, 100.0d0, 80.0d0)
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added yellow rectangle"

        ! Purple ellipse
        call brush%set_color(forge_color_purple)
        call rect_item%set_rect(200.0d0, 200.0d0, 150.0d0, 80.0d0)
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added purple ellipse"

        call status_label%set_text("Named Colors: Red, Green, Blue, Yellow, Purple")
    end subroutine draw_named_colors

    !> @brief Draw alpha transparency demonstration
    subroutine draw_alpha_colors()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: rect_item
        real :: alpha
        integer :: i

        call scene%clear()

        ! Draw overlapping rectangles with different alpha values
        do i = 1, 10
            alpha = real(i) / 10.0

            call brush%set_color_rgb(1.0, 0.0, 0.0, alpha)  ! Red with varying alpha
            call pen%set_color_rgb(0.0, 0.0, 0.0, 1.0)  ! Black border
            call pen%set_width(2)

            call rect_item%set_rect(50.0d0 + real(i*20), 50.0d0 + real(i*15), 200.0d0, 150.0d0)
            call rect_item%set_pen(pen)
            call rect_item%set_brush(brush)
            call scene%add_item(rect_item)
        end do

        print '(A)', "  Added 10 overlapping rectangles with varying alpha (0.1 to 1.0)"
        call status_label%set_text("Alpha Transparency: 10 overlapping red rectangles with increasing opacity")
    end subroutine draw_alpha_colors

    !> @brief Handler for RGB button click
    subroutine on_rgb_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_rgb_colors()
    end subroutine on_rgb_clicked

    !> @brief Handler for named colors button click
    subroutine on_named_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_named_colors()
    end subroutine on_named_clicked

    !> @brief Handler for alpha button click
    subroutine on_alpha_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_alpha_colors()
    end subroutine on_alpha_clicked

end program graphics_colors_example