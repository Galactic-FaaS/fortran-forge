!> @brief Basic Drawing Example
!> @details Demonstrates basic graphics drawing operations
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program graphics_basic_drawing_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_graphics_view) :: graphics_view
    type(forge_scene) :: scene
    type(forge_label) :: status_label
    type(forge_button) :: redraw_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Basic Drawing Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating graphics scenes"
    print '(A)', "  - Basic drawing operations (lines, rectangles, circles)"
    print '(A)', "  - Setting colors and pens"
    print '(A)', "  - Graphics view widgets"
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
        call builder%set_title("Basic Drawing Example")
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
    print '(A)', "Creating graphics scene..."
    call scene%set_scene_rect(0.0d0, 0.0d0, 400.0d0, 300.0d0)
    call scene%set_name("drawing_scene")

    ! Create graphics view
    call graphics_view%set_scene(scene)
    call graphics_view%set_name("graphics_view")

    ! Create control buttons
    call redraw_button%set_label("Redraw Scene")
    call redraw_button%set_name("redraw_button")
    call redraw_button%on_click(on_redraw_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Graphics scene ready - click redraw to see drawing operations")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with graphics scene..."
    call window%show()

    ! Perform initial drawing
    print '(A)', ""
    print '(A)', "Performing basic drawing operations:"

    call perform_basic_drawing()

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

    !> @brief Perform basic drawing operations
    subroutine perform_basic_drawing()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: line_item, rect_item, ellipse_item

        print '(A)', "  Drawing line from (50,50) to (350,50)..."
        call pen%set_color(forge_color_red)
        call pen%set_width(2)
        call line_item%set_line(50.0d0, 50.0d0, 350.0d0, 50.0d0)
        call line_item%set_pen(pen)
        call scene%add_item(line_item)

        print '(A)', "  Drawing rectangle at (100,100) size 150x100..."
        call pen%set_color(forge_color_blue)
        call brush%set_color(forge_color_light_blue)
        call brush%set_style(1)  ! Solid
        call rect_item%set_rect(100.0d0, 100.0d0, 150.0d0, 100.0d0)
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)

        print '(A)', "  Drawing circle at (250,200) radius 50..."
        call pen%set_color(forge_color_green)
        call brush%set_color(forge_color_light_green)
        call ellipse_item%set_rect(200.0d0, 150.0d0, 100.0d0, 100.0d0)  ! Circle
        call ellipse_item%set_pen(pen)
        call ellipse_item%set_brush(brush)
        call scene%add_item(ellipse_item)

        print '(A,I0,A)', "  Scene now contains ", scene%get_item_count(), " items"
    end subroutine perform_basic_drawing

    !> @brief Handler for redraw button click
    subroutine on_redraw_clicked(event)
        type(forge_event), intent(in) :: event

        print '(A)', "  Redrawing scene..."
        call scene%clear()
        call perform_basic_drawing()
        call status_label%set_text("Scene redrawn with basic shapes")
    end subroutine on_redraw_clicked

end program graphics_basic_drawing_example