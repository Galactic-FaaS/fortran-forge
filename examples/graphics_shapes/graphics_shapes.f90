!> @brief Graphics Shapes Example
!> @details Demonstrates drawing various geometric shapes
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program graphics_shapes_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_graphics_view) :: graphics_view
    type(forge_scene) :: scene
    type(forge_label) :: status_label
    type(forge_button) :: shapes_button, polygons_button, paths_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Graphics Shapes Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Drawing various geometric shapes"
    print '(A)', "  - Rectangles, ellipses, polygons"
    print '(A)', "  - Complex paths and curves"
    print '(A)', "  - Shape styling and filling"
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
        call builder%set_title("Graphics Shapes Example")
        call builder%set_size(700, 500)
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
    print '(A)', "Creating graphics scene for shapes..."
    call scene%set_scene_rect(0.0d0, 0.0d0, 600.0d0, 400.0d0)
    call scene%set_name("shapes_scene")

    ! Create graphics view
    call graphics_view%set_scene(scene)
    call graphics_view%set_name("graphics_view")

    ! Create control buttons
    call shapes_button%set_label("Draw Basic Shapes")
    call shapes_button%set_name("shapes_button")
    call shapes_button%on_click(on_shapes_clicked)

    call polygons_button%set_label("Draw Polygons")
    call polygons_button%set_name("polygons_button")
    call polygons_button%on_click(on_polygons_clicked)

    call paths_button%set_label("Draw Paths")
    call paths_button%set_name("paths_button")
    call paths_button%on_click(on_paths_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Click buttons to draw different types of shapes")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with shape drawing capabilities..."
    call window%show()

    ! Draw initial shapes
    print '(A)', ""
    print '(A)', "Drawing initial basic shapes:"
    call draw_basic_shapes()

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

    !> @brief Draw basic geometric shapes
    subroutine draw_basic_shapes()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: rect_item, ellipse_item, triangle_item

        call scene%clear()

        ! Rectangle
        call pen%set_color(forge_color_blue)
        call pen%set_width(3)
        call brush%set_color(forge_color_light_blue)
        call brush%set_style(1)  ! Solid
        call rect_item%set_rect(50.0d0, 50.0d0, 100.0d0, 80.0d0)
        call rect_item%set_pen(pen)
        call rect_item%set_brush(brush)
        call scene%add_item(rect_item)
        print '(A)', "  Added rectangle"

        ! Ellipse/Circle
        call pen%set_color(forge_color_red)
        call brush%set_color(forge_color_pink)
        call ellipse_item%set_rect(200.0d0, 50.0d0, 100.0d0, 100.0d0)  ! Circle
        call ellipse_item%set_pen(pen)
        call ellipse_item%set_brush(brush)
        call scene%add_item(ellipse_item)
        print '(A)', "  Added circle"

        ! Triangle (polygon)
        call pen%set_color(forge_color_green)
        call brush%set_color(forge_color_light_green)
        call triangle_item%set_polygon([300.0d0, 50.0d0, 350.0d0, 130.0d0, 250.0d0, 130.0d0, 300.0d0, 50.0d0])  ! Triangle
        call triangle_item%set_pen(pen)
        call triangle_item%set_brush(brush)
        call scene%add_item(triangle_item)
        print '(A)', "  Added triangle"

        call status_label%set_text("Basic shapes: Rectangle, Circle, Triangle")
    end subroutine draw_basic_shapes

    !> @brief Draw polygon shapes
    subroutine draw_polygons()
        type(forge_pen) :: pen
        type(forge_brush) :: brush
        type(forge_graphics_item) :: pentagon_item, hexagon_item, star_item

        call scene%clear()

        ! Pentagon
        call pen%set_color(forge_color_purple)
        call pen%set_width(2)
        call brush%set_color(forge_color_lavender)
        call brush%set_style(1)
        ! Pentagon coordinates would be calculated here
        call pentagon_item%set_polygon([450.0d0, 50.0d0, 500.0d0, 80.0d0, 480.0d0, 130.0d0, 420.0d0, 130.0d0, 400.0d0, 80.0d0, 450.0d0, 50.0d0])
        call pentagon_item%set_pen(pen)
        call pentagon_item%set_brush(brush)
        call scene%add_item(pentagon_item)
        print '(A)', "  Added pentagon"

        ! Hexagon
        call pen%set_color(forge_color_orange)
        call brush%set_color(forge_color_light_orange)
        ! Hexagon coordinates
        call hexagon_item%set_polygon([50.0d0, 200.0d0, 100.0d0, 200.0d0, 125.0d0, 250.0d0, 100.0d0, 300.0d0, 50.0d0, 300.0d0, 25.0d0, 250.0d0, 50.0d0, 200.0d0])
        call hexagon_item%set_pen(pen)
        call hexagon_item%set_brush(brush)
        call scene%add_item(hexagon_item)
        print '(A)', "  Added hexagon"

        ! Star (complex polygon)
        call pen%set_color(forge_color_dark_yellow)
        call brush%set_color(forge_color_yellow)
        ! Star coordinates (simplified)
        call star_item%set_polygon([250.0d0, 200.0d0, 270.0d0, 240.0d0, 310.0d0, 240.0d0, 280.0d0, 270.0d0, 290.0d0, 310.0d0, 250.0d0, 285.0d0, 210.0d0, 310.0d0, 220.0d0, 270.0d0, 190.0d0, 240.0d0, 230.0d0, 240.0d0, 250.0d0, 200.0d0])
        call star_item%set_pen(pen)
        call star_item%set_brush(brush)
        call scene%add_item(star_item)
        print '(A)', "  Added star"

        call status_label%set_text("Polygons: Pentagon, Hexagon, Star")
    end subroutine draw_polygons

    !> @brief Draw complex paths
    subroutine draw_paths()
        type(forge_pen) :: pen
        type(forge_graphics_item) :: path_item1, path_item2, path_item3

        call scene%clear()

        ! Curved path
        call pen%set_color(forge_color_dark_blue)
        call pen%set_width(3)
        ! Path with curves would be defined here
        call path_item1%set_path(create_curved_path())
        call path_item1%set_pen(pen)
        call scene%add_item(path_item1)
        print '(A)', "  Added curved path"

        ! Angular path
        call pen%set_color(forge_color_dark_red)
        call pen%set_width(2)
        call path_item2%set_path(create_angular_path())
        call path_item2%set_pen(pen)
        call scene%add_item(path_item2)
        print '(A)', "  Added angular path"

        ! Complex path
        call pen%set_color(forge_color_dark_green)
        call pen%set_width(4)
        call path_item3%set_path(create_complex_path())
        call path_item3%set_pen(pen)
        call scene%add_item(path_item3)
        print '(A)', "  Added complex path"

        call status_label%set_text("Paths: Curved, Angular, Complex")
    end subroutine draw_paths

    !> @brief Create a curved path (simplified)
    function create_curved_path() result(path)
        type(forge_path) :: path
        ! In a real implementation, this would create a path with curves
        ! For demo, we'll just create a simple path
        call path%move_to(50.0d0, 50.0d0)
        call path%line_to(150.0d0, 50.0d0)
        call path%line_to(150.0d0, 150.0d0)
        call path%line_to(50.0d0, 150.0d0)
        call path%close_subpath()
    end function create_curved_path

    !> @brief Create an angular path
    function create_angular_path() result(path)
        type(forge_path) :: path
        call path%move_to(250.0d0, 50.0d0)
        call path%line_to(350.0d0, 50.0d0)
        call path%line_to(300.0d0, 100.0d0)
        call path%line_to(350.0d0, 150.0d0)
        call path%line_to(250.0d0, 150.0d0)
        call path%close_subpath()
    end function create_angular_path

    !> @brief Create a complex path
    function create_complex_path() result(path)
        type(forge_path) :: path
        call path%move_to(450.0d0, 50.0d0)
        call path%line_to(500.0d0, 80.0d0)
        call path%line_to(480.0d0, 120.0d0)
        call path%line_to(520.0d0, 150.0d0)
        call path%line_to(450.0d0, 130.0d0)
        call path%line_to(420.0d0, 100.0d0)
        call path%close_subpath()
    end function create_complex_path

    !> @brief Handler for shapes button click
    subroutine on_shapes_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_basic_shapes()
    end subroutine on_shapes_clicked

    !> @brief Handler for polygons button click
    subroutine on_polygons_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_polygons()
    end subroutine on_polygons_clicked

    !> @brief Handler for paths button click
    subroutine on_paths_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_paths()
    end subroutine on_paths_clicked

end program graphics_shapes_example