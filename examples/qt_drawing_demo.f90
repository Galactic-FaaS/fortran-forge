!> @brief Qt-like drawing framework demonstration
!> @details Shows how to use the new QPainter, QPen, QBrush, QPixmap, QFont, and QPath classes
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qt_drawing_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize application with custom backend
    call app%init(BACKEND_CUSTOM, status)
    if (status%is_error()) then
        call status%print()
        stop 1
    end if

    ! Create window
    window = app%create_window("Qt-like Drawing Demo", 800, 600)

    ! Show window
    call window%show()

    ! Run event loop
    call app%run()

    ! Cleanup
    call app%shutdown()

contains

    !> @brief Custom drawing function that demonstrates Qt-like drawing
    subroutine demo_qt_drawing(cr)
        use forge_cairo_bindings
        type(c_ptr), intent(in) :: cr
        type(forge_qpainter_t) :: painter
        type(forge_qpen_t) :: pen
        type(forge_qbrush_t) :: brush
        type(forge_qfont_t) :: font
        type(forge_qpath_t) :: path
        type(forge_qpixmap_t) :: pixmap
        type(forge_rect) :: rect
        real(c_double) :: points(2,5)
        integer :: i

        ! Begin painting
        call painter%begin(cr)

        ! Set up pen for drawing outlines
        call pen%set_color(forge_color(0.0_c_double, 0.0_c_double, 1.0_c_double, 1.0_c_double))
        call pen%set_width(2.0_c_double)
        call pen%set_style(PEN_STYLE_SOLID)
        call painter%set_pen(pen)

        ! Set up brush for filling
        call brush%set_color(forge_color(1.0_c_double, 0.8_c_double, 0.0_c_double, 0.7_c_double))
        call painter%set_brush(brush)

        ! Draw a filled rectangle with rounded corners
        rect%pos%x = 50
        rect%pos%y = 50
        rect%size%width = 200
        rect%size%height = 100
        call painter%fill_rect(rect, brush)

        ! Draw rectangle outline
        call painter%draw_rect(rect)

        ! Draw some lines
        call painter%draw_line(300.0_c_double, 50.0_c_double, 500.0_c_double, 150.0_c_double)
        call painter%draw_line(300.0_c_double, 150.0_c_double, 500.0_c_double, 50.0_c_double)

        ! Draw ellipse
        call painter%draw_ellipse(400.0_c_double, 250.0_c_double, 80.0_c_double, 60.0_c_double)

        ! Create and draw a polygon
        points(1,1) = 100.0_c_double; points(2,1) = 200.0_c_double
        points(1,2) = 150.0_c_double; points(2,2) = 250.0_c_double
        points(1,3) = 200.0_c_double; points(2,3) = 200.0_c_double
        points(1,4) = 180.0_c_double; points(2,4) = 180.0_c_double
        points(1,5) = 120.0_c_double; points(2,5) = 180.0_c_double
        call painter%draw_polygon(points, .true.)

        ! Set up font for text rendering
        call font%set_family("Sans")
        call font%set_point_size(16.0_c_double)
        call font%set_weight(FONT_WEIGHT_BOLD)

        ! Draw text
        call painter%draw_text(50.0_c_double, 350.0_c_double, "Qt-like Drawing in ForGE!")

        ! Create a path with Bezier curves
        call path%move_to(500.0_c_double, 300.0_c_double)
        call path%curve_to(550.0_c_double, 250.0_c_double, 650.0_c_double, 350.0_c_double, 700.0_c_double, 300.0_c_double)
        call path%line_to(700.0_c_double, 400.0_c_double)
        call path%curve_to(650.0_c_double, 450.0_c_double, 550.0_c_double, 350.0_c_double, 500.0_c_double, 400.0_c_double)
        call path%close_subpath()

        ! Draw the path
        call painter%stroke_path(path, pen)

        ! Create a gradient brush
        call brush%set_linear_gradient(50.0_c_double, 450.0_c_double, 250.0_c_double, 450.0_c_double)
        call brush%add_gradient_stop(0.0_c_double, forge_color(1.0_c_double, 0.0_c_double, 0.0_c_double, 1.0_c_double))
        call brush%add_gradient_stop(0.5_c_double, forge_color(0.0_c_double, 1.0_c_double, 0.0_c_double, 1.0_c_double))
        call brush%add_gradient_stop(1.0_c_double, forge_color(0.0_c_double, 0.0_c_double, 1.0_c_double, 1.0_c_double))

        ! Draw gradient rectangle
        rect%pos%x = 50
        rect%pos%y = 450
        rect%size%width = 200
        rect%size%height = 50
        call painter%fill_rect(rect, brush)

        ! Demonstrate transformations
        call painter%save()
        call painter%translate(600.0_c_double, 500.0_c_double)
        call painter%rotate(0.3_c_double)
        call painter%scale(1.5_c_double, 0.8_c_double)

        ! Draw transformed text
        call painter%draw_text(-50.0_c_double, 0.0_c_double, "Transformed Text!")

        call painter%restore()

        ! Create a pixmap and draw it
        call pixmap%create(100_c_int, 100_c_int, PIXMAP_FORMAT_ARGB32)
        if (.not. pixmap%is_null()) then
            ! Draw something on the pixmap
            call painter%begin_pixmap(pixmap)

            ! Clear with blue
            call pixmap%fill(forge_color(0.0_c_double, 0.0_c_double, 1.0_c_double, 1.0_c_double))

            ! Draw a white circle
            call brush%set_color(forge_color(1.0_c_double, 1.0_c_double, 1.0_c_double, 1.0_c_double))
            call painter%set_brush(brush)
            call painter%draw_ellipse(50.0_c_double, 50.0_c_double, 30.0_c_double, 30.0_c_double)

            call painter%end()

            ! Draw the pixmap on screen
            call painter%draw_pixmap(650.0_c_double, 50.0_c_double, pixmap)
        end if

        ! End painting
        call painter%end()

        ! Cleanup
        call pixmap%cleanup()
    end subroutine demo_qt_drawing

end program qt_drawing_demo