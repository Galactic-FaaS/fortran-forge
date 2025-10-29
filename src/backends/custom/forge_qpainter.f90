!> @brief QPainter-like module for 2D graphics painting
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: target_rect
        type(forge_qpixmap_t), intent(in) :: image
        type(forge_rect), intent(in), optional :: source_rect
        
        if (.not. this%is_active() .or. image%is_null()) return
        
        if (present(source_rect)) then
            call cairo_set_source_surface(this%cr_, image%painter(), &
                real(target_rect%pos%x - source_rect%pos%x, c_double), &
                real(target_rect%pos%y - source_rect%pos%y, c_double))
        else
            call cairo_set_source_surface(this%cr_, image%painter(), &
                real(target_rect%pos%x, c_double), real(target_rect%pos%y, c_double))
        end if
        
        if (this%smooth_pixmap_transform_) then
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_BILINEAR)
        else
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_NEAREST)
        end if
        
        call cairo_rectangle(this%cr_, real(target_rect%pos%x, c_double), real(target_rect%pos%y, c_double), &
                            real(target_rect%size%width, c_double), real(target_rect%size%height, c_double))
        call cairo_fill(this%cr_)
    end subroutine forge_qpainter_draw_image_rect
    public :: PAINTER_COMPOSITION_MODE_PLUS, PAINTER_COMPOSITION_MODE_MULTIPLY
    public :: PAINTER_COMPOSITION_MODE_SCREEN, PAINTER_COMPOSITION_MODE_OVERLAY
    public :: PAINTER_COMPOSITION_MODE_DARKEN, PAINTER_COMPOSITION_MODE_LIGHTEN
    public :: PAINTER_COMPOSITION_MODE_COLOR_DODGE, PAINTER_COMPOSITION_MODE_COLOR_BURN
    public :: PAINTER_COMPOSITION_MODE_HARD_LIGHT, PAINTER_COMPOSITION_MODE_SOFT_LIGHT
    public :: PAINTER_COMPOSITION_MODE_DIFFERENCE, PAINTER_COMPOSITION_MODE_EXCLUSION

    ! Antialiasing constants
    integer, parameter :: PAINTER_ANTIALIASING_NONE = 0
    integer, parameter :: PAINTER_ANTIALIASING_DEFAULT = 1
    integer, parameter :: PAINTER_ANTIALIASING_GRAY = 2
    integer, parameter :: PAINTER_ANTIALIASING_SUBPIXEL = 3
    integer, parameter :: PAINTER_ANTIALIASING_FAST = 4
    integer, parameter :: PAINTER_ANTIALIASING_GOOD = 5
    integer, parameter :: PAINTER_ANTIALIASING_BEST = 6

    ! Composition mode constants (matching Qt)
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOURCE_OVER = 0
    integer, parameter :: PAINTER_COMPOSITION_MODE_DESTINATION_OVER = 1
    integer, parameter :: PAINTER_COMPOSITION_MODE_CLEAR = 2
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOURCE = 3
    integer, parameter :: PAINTER_COMPOSITION_MODE_DESTINATION = 4
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOURCE_IN = 5
    integer, parameter :: PAINTER_COMPOSITION_MODE_DESTINATION_IN = 6
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOURCE_OUT = 7
    integer, parameter :: PAINTER_COMPOSITION_MODE_DESTINATION_OUT = 8
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOURCE_ATOP = 9
    integer, parameter :: PAINTER_COMPOSITION_MODE_DESTINATION_ATOP = 10
    integer, parameter :: PAINTER_COMPOSITION_MODE_XOR = 11
    integer, parameter :: PAINTER_COMPOSITION_MODE_PLUS = 12
    integer, parameter :: PAINTER_COMPOSITION_MODE_MULTIPLY = 13
    integer, parameter :: PAINTER_COMPOSITION_MODE_SCREEN = 14
    integer, parameter :: PAINTER_COMPOSITION_MODE_OVERLAY = 15
    integer, parameter :: PAINTER_COMPOSITION_MODE_DARKEN = 16
    integer, parameter :: PAINTER_COMPOSITION_MODE_LIGHTEN = 17
    integer, parameter :: PAINTER_COMPOSITION_MODE_COLOR_DODGE = 18
    integer, parameter :: PAINTER_COMPOSITION_MODE_COLOR_BURN = 19
    integer, parameter :: PAINTER_COMPOSITION_MODE_HARD_LIGHT = 20
    integer, parameter :: PAINTER_COMPOSITION_MODE_SOFT_LIGHT = 21
    integer, parameter :: PAINTER_COMPOSITION_MODE_DIFFERENCE = 22
    integer, parameter :: PAINTER_COMPOSITION_MODE_EXCLUSION = 23

    !> @brief QPainter-like class for 2D graphics painting
    type :: forge_qpainter_t
        private
        type(c_ptr) :: cr_ = c_null_ptr                    !< Cairo context
        type(forge_qpen_t) :: pen_                         !< Current pen
        type(forge_qbrush_t) :: brush_                     !< Current brush
        type(forge_color) :: background_color_             !< Background color
        integer :: antialiasing_ = PAINTER_ANTIALIASING_DEFAULT
        integer :: composition_mode_ = PAINTER_COMPOSITION_MODE_SOURCE_OVER
        logical :: has_clipping_ = .false.
        type(c_ptr) :: clip_surface_ = c_null_ptr          !< For complex clipping
        logical :: owns_context_ = .false.                 !< Whether we created the context
        real(c_double) :: opacity_ = 1.0_c_double          !< Global opacity
        logical :: smooth_pixmap_transform_ = .true.       !< Smooth pixmap transformations
    contains
        procedure :: begin => forge_qpainter_begin
        procedure :: begin_pixmap => forge_qpainter_begin_pixmap
        procedure :: end => forge_qpainter_end
        procedure :: is_active => forge_qpainter_is_active
        procedure :: set_pen => forge_qpainter_set_pen
        procedure :: pen => forge_qpainter_get_pen
        procedure :: set_brush => forge_qpainter_set_brush
        procedure :: brush => forge_qpainter_get_brush
        procedure :: set_background => forge_qpainter_set_background
        procedure :: background => forge_qpainter_get_background
        procedure :: set_opacity => forge_qpainter_set_opacity
        procedure :: opacity => forge_qpainter_get_opacity
        procedure :: set_composition_mode => forge_qpainter_set_composition_mode
        procedure :: composition_mode => forge_qpainter_get_composition_mode
        procedure :: set_antialiasing => forge_qpainter_set_antialiasing
        procedure :: antialiasing => forge_qpainter_get_antialiasing
        procedure :: set_smooth_pixmap_transform => forge_qpainter_set_smooth_pixmap_transform
        procedure :: smooth_pixmap_transform => forge_qpainter_get_smooth_pixmap_transform

        ! Drawing operations
        procedure :: draw_point => forge_qpainter_draw_point
        procedure :: draw_points => forge_qpainter_draw_points
        procedure :: draw_line => forge_qpainter_draw_line
        procedure :: draw_lines => forge_qpainter_draw_lines
        procedure :: draw_rect => forge_qpainter_draw_rect
        procedure :: draw_rects => forge_qpainter_draw_rects
        procedure :: fill_rect => forge_qpainter_fill_rect
        procedure :: fill_rects => forge_qpainter_fill_rects
        procedure :: draw_ellipse => forge_qpainter_draw_ellipse
        procedure :: draw_ellipses => forge_qpainter_draw_ellipses
        procedure :: draw_arc => forge_qpainter_draw_arc
        procedure :: draw_pie => forge_qpainter_draw_pie
        procedure :: draw_chord => forge_qpainter_draw_chord
        procedure :: draw_polygon => forge_qpainter_draw_polygon
        procedure :: draw_polyline => forge_qpainter_draw_polyline
        procedure :: draw_path => forge_qpainter_draw_path
        procedure :: fill_path => forge_qpainter_fill_path
        procedure :: stroke_path => forge_qpainter_stroke_path

        ! Text drawing
        procedure :: draw_text => forge_qpainter_draw_text
        procedure :: draw_text_rect => forge_qpainter_draw_text_rect

        ! Image/pixmap drawing
        procedure :: draw_pixmap => forge_qpainter_draw_pixmap
        procedure :: draw_pixmap_rect => forge_qpainter_draw_pixmap_rect
        procedure :: draw_image => forge_qpainter_draw_image
        procedure :: draw_image_rect => forge_qpainter_draw_image_rect

        ! Coordinate transformations
        procedure :: translate => forge_qpainter_translate
        procedure :: rotate => forge_qpainter_rotate
        procedure :: scale => forge_qpainter_scale
        procedure :: shear => forge_qpainter_shear
        procedure :: transform => forge_qpainter_transform
        procedure :: set_transform => forge_qpainter_set_transform
        procedure :: reset_transform => forge_qpainter_reset_transform
        procedure :: save => forge_qpainter_save
        procedure :: restore => forge_qpainter_restore

        ! Clipping
        procedure :: set_clip_rect => forge_qpainter_set_clip_rect
        procedure :: set_clip_region => forge_qpainter_set_clip_region
        procedure :: set_clip_path => forge_qpainter_set_clip_path
        procedure :: set_clipping => forge_qpainter_set_clipping
        procedure :: has_clipping => forge_qpainter_has_clipping
        procedure :: clip_bounding_rect => forge_qpainter_clip_bounding_rect

        ! Utility
        procedure :: bounding_rect => forge_qpainter_bounding_rect
        procedure :: erase_rect => forge_qpainter_erase_rect
        procedure :: set_background_mode => forge_qpainter_set_background_mode
        procedure :: background_mode => forge_qpainter_get_background_mode
        procedure :: set_render_hint => forge_qpainter_set_render_hint
        procedure :: test_render_hint => forge_qpainter_test_render_hint
    end type forge_qpainter_t

contains

    !> @brief Begin painting on a Cairo surface
    subroutine forge_qpainter_begin(this, surface)
        class(forge_qpainter_t), intent(inout) :: this
        type(c_ptr), intent(in) :: surface

        if (this%is_active()) call this%end()

        this%cr_ = cairo_create(surface)
        this%owns_context_ = .true.

        ! Set default state
        call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_DEFAULT)
        call cairo_set_operator(this%cr_, CAIRO_OPERATOR_OVER)
        call cairo_set_line_width(this%cr_, 1.0_c_double)
    end subroutine forge_qpainter_begin

    !> @brief Begin painting on a pixmap
    subroutine forge_qpainter_begin_pixmap(this, pixmap)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_qpixmap_t), intent(in) :: pixmap

        if (pixmap%is_null()) return
        call this%begin(pixmap%painter())
    end subroutine forge_qpainter_begin_pixmap

    !> @brief End painting
    subroutine forge_qpainter_end(this)
        class(forge_qpainter_t), intent(inout) :: this

        if (.not. this%is_active()) return

        if (c_associated(this%clip_surface_)) then
            call cairo_surface_destroy(this%clip_surface_)
            this%clip_surface_ = c_null_ptr
        end if

        if (this%owns_context_) then
            call cairo_destroy(this%cr_)
        end if

        this%cr_ = c_null_ptr
        this%owns_context_ = .false.
        this%has_clipping_ = .false.
    end subroutine forge_qpainter_end

    !> @brief Check if painter is active
    function forge_qpainter_is_active(this) result(active)
        class(forge_qpainter_t), intent(in) :: this
        logical :: active
        active = c_associated(this%cr_)
    end function forge_qpainter_is_active

    !> @brief Set current pen
    subroutine forge_qpainter_set_pen(this, pen)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_qpen_t), intent(in) :: pen

        if (.not. this%is_active()) return

        this%pen_ = pen
        call this%pen_%apply_to_cairo(this%cr_)
    end subroutine forge_qpainter_set_pen

    !> @brief Get current pen
    function forge_qpainter_get_pen(this) result(pen)
        class(forge_qpainter_t), intent(in) :: this
        type(forge_qpen_t) :: pen
        pen = this%pen_
    end function forge_qpainter_get_pen

    !> @brief Set current brush
    subroutine forge_qpainter_set_brush(this, brush)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_qbrush_t), intent(in) :: brush

        if (.not. this%is_active()) return

        this%brush_ = brush
    end subroutine forge_qpainter_set_brush

    !> @brief Get current brush
    function forge_qpainter_get_brush(this) result(brush)
        class(forge_qpainter_t), intent(in) :: this
        type(forge_qbrush_t) :: brush
        brush = this%brush_
    end function forge_qpainter_get_brush

    !> @brief Set background color
    subroutine forge_qpainter_set_background(this, color)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_color), intent(in) :: color
        this%background_color_ = color
    end subroutine forge_qpainter_set_background

    !> @brief Get background color
    function forge_qpainter_get_background(this) result(color)
        class(forge_qpainter_t), intent(in) :: this
        type(forge_color) :: color
        color = this%background_color_
    end function forge_qpainter_get_background

    !> @brief Set global opacity
    subroutine forge_qpainter_set_opacity(this, opacity)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: opacity

        if (.not. this%is_active()) return

        this%opacity_ = max(0.0_c_double, min(1.0_c_double, opacity))
        call cairo_paint_with_alpha(this%cr_, this%opacity_)
    end subroutine forge_qpainter_set_opacity

    !> @brief Get global opacity
    function forge_qpainter_get_opacity(this) result(opacity)
        class(forge_qpainter_t), intent(in) :: this
        real(c_double) :: opacity
        opacity = this%opacity_
    end function forge_qpainter_get_opacity

    !> @brief Set composition mode
    subroutine forge_qpainter_set_composition_mode(this, mode)
        class(forge_qpainter_t), intent(inout) :: this
        integer, intent(in) :: mode

        if (.not. this%is_active()) return

        this%composition_mode_ = mode

        ! Map to Cairo operators
        select case (mode)
        case (PAINTER_COMPOSITION_MODE_CLEAR)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_CLEAR)
        case (PAINTER_COMPOSITION_MODE_SOURCE)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_SOURCE)
        case (PAINTER_COMPOSITION_MODE_DESTINATION)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_DEST)
        case (PAINTER_COMPOSITION_MODE_SOURCE_OVER)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_OVER)
        case (PAINTER_COMPOSITION_MODE_DESTINATION_OVER)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_DEST_OVER)
        case (PAINTER_COMPOSITION_MODE_SOURCE_IN)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_IN)
        case (PAINTER_COMPOSITION_MODE_DESTINATION_IN)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_DEST_IN)
        case (PAINTER_COMPOSITION_MODE_SOURCE_OUT)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_OUT)
        case (PAINTER_COMPOSITION_MODE_DESTINATION_OUT)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_DEST_OUT)
        case (PAINTER_COMPOSITION_MODE_SOURCE_ATOP)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_ATOP)
        case (PAINTER_COMPOSITION_MODE_DESTINATION_ATOP)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_DEST_ATOP)
        case (PAINTER_COMPOSITION_MODE_XOR)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_XOR)
        case (PAINTER_COMPOSITION_MODE_PLUS)
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_ADD)
        case default
            call cairo_set_operator(this%cr_, CAIRO_OPERATOR_OVER)
        end select
    end subroutine forge_qpainter_set_composition_mode

    !> @brief Get composition mode
    function forge_qpainter_get_composition_mode(this) result(mode)
        class(forge_qpainter_t), intent(in) :: this
        integer :: mode
        mode = this%composition_mode_
    end function forge_qpainter_get_composition_mode

    !> @brief Set antialiasing mode
    subroutine forge_qpainter_set_antialiasing(this, antialiasing)
        class(forge_qpainter_t), intent(inout) :: this
        integer, intent(in) :: antialiasing

        if (.not. this%is_active()) return

        this%antialiasing_ = antialiasing

        select case (antialiasing)
        case (PAINTER_ANTIALIASING_NONE)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_NONE)
        case (PAINTER_ANTIALIASING_GRAY)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_GRAY)
        case (PAINTER_ANTIALIASING_SUBPIXEL)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_SUBPIXEL)
        case (PAINTER_ANTIALIASING_FAST)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_FAST)
        case (PAINTER_ANTIALIASING_GOOD)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_GOOD)
        case (PAINTER_ANTIALIASING_BEST)
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_BEST)
        case default
            call cairo_set_antialias(this%cr_, CAIRO_ANTIALIAS_DEFAULT)
        end select
    end subroutine forge_qpainter_set_antialiasing

    !> @brief Get antialiasing mode
    function forge_qpainter_get_antialiasing(this) result(antialiasing)
        class(forge_qpainter_t), intent(in) :: this
        integer :: antialiasing
        antialiasing = this%antialiasing_
    end function forge_qpainter_get_antialiasing

    !> @brief Set smooth pixmap transform
    subroutine forge_qpainter_set_smooth_pixmap_transform(this, smooth)
        class(forge_qpainter_t), intent(inout) :: this
        logical, intent(in) :: smooth
        this%smooth_pixmap_transform_ = smooth
    end subroutine forge_qpainter_set_smooth_pixmap_transform

    !> @brief Get smooth pixmap transform setting
    function forge_qpainter_get_smooth_pixmap_transform(this) result(smooth)
        class(forge_qpainter_t), intent(in) :: this
        logical :: smooth
        smooth = this%smooth_pixmap_transform_
    end function forge_qpainter_get_smooth_pixmap_transform

    !> @brief Draw a point
    subroutine forge_qpainter_draw_point(this, x, y)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y

        if (.not. this%is_active()) return

        call cairo_move_to(this%cr_, x, y)
        call cairo_line_to(this%cr_, x, y)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_point

    !> @brief Draw multiple points
    subroutine forge_qpainter_draw_points(this, points)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: points(:,:)
        integer :: i

        if (.not. this%is_active()) return

        do i = 1, size(points, 2)
            call this%draw_point(points(1,i), points(2,i))
        end do
    end subroutine forge_qpainter_draw_points

    !> @brief Draw a line
    subroutine forge_qpainter_draw_line(this, x1, y1, x2, y2)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: x1, y1, x2, y2

        if (.not. this%is_active()) return

        call cairo_move_to(this%cr_, x1, y1)
        call cairo_line_to(this%cr_, x2, y2)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_line

    !> @brief Draw multiple lines
    subroutine forge_qpainter_draw_lines(this, lines)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: lines(:,:)
        integer :: i

        if (.not. this%is_active()) return

        do i = 1, size(lines, 2), 2
            call this%draw_line(lines(1,i), lines(2,i), lines(1,i+1), lines(2,i+1))
        end do
    end subroutine forge_qpainter_draw_lines

    !> @brief Draw a rectangle
    subroutine forge_qpainter_draw_rect(this, rect)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double) :: x, y, w, h

        if (.not. this%is_active()) return

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        call cairo_rectangle(this%cr_, x, y, w, h)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_rect

    !> @brief Draw multiple rectangles
    subroutine forge_qpainter_draw_rects(this, rects)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rects(:)
        integer :: i

        if (.not. this%is_active()) return

        do i = 1, size(rects)
            call this%draw_rect(rects(i))
        end do
    end subroutine forge_qpainter_draw_rects

    !> @brief Fill a rectangle
    subroutine forge_qpainter_fill_rect(this, rect, brush)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        type(forge_qbrush_t), intent(in), optional :: brush
        real(c_double) :: x, y, w, h

        if (.not. this%is_active()) return

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        if (present(brush)) then
            call brush%apply_to_cairo(this%cr_)
        else
            call this%brush_%apply_to_cairo(this%cr_)
        end if

        call cairo_rectangle(this%cr_, x, y, w, h)
        call cairo_fill(this%cr_)
    end subroutine forge_qpainter_fill_rect

    !> @brief Fill multiple rectangles
    subroutine forge_qpainter_fill_rects(this, rects, brush)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rects(:)
        type(forge_qbrush_t), intent(in), optional :: brush
        integer :: i

        if (.not. this%is_active()) return

        do i = 1, size(rects)
            call this%fill_rect(rects(i), brush)
        end do
    end subroutine forge_qpainter_fill_rects

    !> @brief Draw an ellipse
    subroutine forge_qpainter_draw_ellipse(this, center_x, center_y, radius_x, radius_y)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: center_x, center_y, radius_x, radius_y

        if (.not. this%is_active()) return

        call cairo_save(this%cr_)
        call cairo_translate(this%cr_, center_x, center_y)
        call cairo_scale(this%cr_, radius_x, radius_y)
        call cairo_arc(this%cr_, 0.0_c_double, 0.0_c_double, 1.0_c_double, 0.0_c_double, 2.0_c_double * 3.141592653589793_c_double)
        call cairo_restore(this%cr_)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_ellipse

    !> @brief Draw multiple ellipses
    subroutine forge_qpainter_draw_ellipses(this, ellipses)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: ellipses(:,:)
        integer :: i

        if (.not. this%is_active()) return

        do i = 1, size(ellipses, 2)
            call this%draw_ellipse(ellipses(1,i), ellipses(2,i), ellipses(3,i), ellipses(4,i))
        end do
    end subroutine forge_qpainter_draw_ellipses

    !> @brief Draw an arc
    subroutine forge_qpainter_draw_arc(this, rect, start_angle, span_angle)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: start_angle, span_angle
        real(c_double) :: x, y, w, h, cx, cy, radius

        if (.not. this%is_active()) return

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        cx = x + w/2.0_c_double
        cy = y + h/2.0_c_double
        radius = min(w, h) / 2.0_c_double

        call cairo_arc(this%cr_, cx, cy, radius, start_angle, start_angle + span_angle)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_arc

    !> @brief Draw a pie slice
    subroutine forge_qpainter_draw_pie(this, rect, start_angle, span_angle)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: start_angle, span_angle
        real(c_double) :: x, y, w, h, cx, cy, radius

        if (.not. this%is_active()) return

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        cx = x + w/2.0_c_double
        cy = y + h/2.0_c_double
        radius = min(w, h) / 2.0_c_double

        call cairo_move_to(this%cr_, cx, cy)
        call cairo_arc(this%cr_, cx, cy, radius, start_angle, start_angle + span_angle)
        call cairo_close_path(this%cr_)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_pie

    !> @brief Draw a chord
    subroutine forge_qpainter_draw_chord(this, rect, start_angle, span_angle)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: start_angle, span_angle
        real(c_double) :: x, y, w, h, cx, cy, radius

        if (.not. this%is_active()) return

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        cx = x + w/2.0_c_double
        cy = y + h/2.0_c_double
        radius = min(w, h) / 2.0_c_double

        call cairo_arc(this%cr_, cx, cy, radius, start_angle, start_angle + span_angle)
        call cairo_close_path(this%cr_)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_chord

    !> @brief Draw a polygon
    subroutine forge_qpainter_draw_polygon(this, points, fill)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: points(:,:)
        logical, intent(in), optional :: fill
        integer :: i
        logical :: do_fill

        if (.not. this%is_active()) return

        do_fill = .false.
        if (present(fill)) do_fill = fill

        if (size(points, 2) > 0) then
            call cairo_move_to(this%cr_, points(1,1), points(2,1))
            do i = 2, size(points, 2)
                call cairo_line_to(this%cr_, points(1,i), points(2,i))
            end do
            call cairo_close_path(this%cr_)

            if (do_fill) then
                call this%brush_%apply_to_cairo(this%cr_)
                call cairo_fill_preserve(this%cr_)
            end if
            call cairo_stroke(this%cr_)
        end if
    end subroutine forge_qpainter_draw_polygon

    !> @brief Draw a polyline
    subroutine forge_qpainter_draw_polyline(this, points)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: points(:,:)
        integer :: i

        if (.not. this%is_active()) return

        if (size(points, 2) > 0) then
            call cairo_move_to(this%cr_, points(1,1), points(2,1))
            do i = 2, size(points, 2)
                call cairo_line_to(this%cr_, points(1,i), points(2,i))
            end do
            call cairo_stroke(this%cr_)
        end if
    end subroutine forge_qpainter_draw_polyline

    !> @brief Draw a path
    subroutine forge_qpainter_draw_path(this, path)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_qpath_t), intent(in) :: path
        
        if (.not. this%is_active()) return
        
        call path%apply_to_cairo(this%cr_)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_draw_path
        type(c_ptr), intent(in) :: path
        type(forge_qbrush_t), intent(in), optional :: brush

        if (.not. this%is_active()) return

        if (present(brush)) then
            call brush%apply_to_cairo(this%cr_)
        else
            call this%brush_%apply_to_cairo(this%cr_)
        end if

        call cairo_append_path(this%cr_, path)
        call cairo_fill(this%cr_)
    end subroutine forge_qpainter_fill_path

    !> @brief Stroke a path
    subroutine forge_qpainter_stroke_path(this, path, pen)
        class(forge_qpainter_t), intent(inout) :: this
        type(c_ptr), intent(in) :: path
        type(forge_qpen_t), intent(in), optional :: pen

        if (.not. this%is_active()) return

        if (present(pen)) then
            call pen%apply_to_cairo(this%cr_)
        else
            call this%pen_%apply_to_cairo(this%cr_)
        end if

        call cairo_append_path(this%cr_, path)
        call cairo_stroke(this%cr_)
    end subroutine forge_qpainter_stroke_path

    !> @brief Draw text at position
    subroutine forge_qpainter_draw_text(this, x, y, text)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y
        character(len=*), intent(in) :: text

        if (.not. this%is_active()) return

        call cairo_move_to(this%cr_, x, y)
        call cairo_show_text(this%cr_, trim(text)//c_null_char)
    end subroutine forge_qpainter_draw_text

    !> @brief Draw text in rectangle
    subroutine forge_qpainter_draw_text_rect(this, rect, text, alignment)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: alignment

        if (.not. this%is_active()) return

        ! Simple implementation - center text
        call this%draw_text(real(rect%pos%x + rect%size%width/2, c_double) - 50.0_c_double, &
                           real(rect%pos%y + rect%size%height/2, c_double) + 5.0_c_double, text)
    end subroutine forge_qpainter_draw_text_rect

    !> @brief Draw pixmap
    subroutine forge_qpainter_draw_pixmap(this, x, y, pixmap)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y
        type(forge_qpixmap_t), intent(in) :: pixmap

        if (.not. this%is_active() .or. pixmap%is_null()) return

        call cairo_set_source_surface(this%cr_, pixmap%painter(), x, y)
        if (this%smooth_pixmap_transform_) then
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_BILINEAR)
        else
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_NEAREST)
        end if
        call cairo_paint(this%cr_)
    end subroutine forge_qpainter_draw_pixmap

    !> @brief Draw pixmap in rectangle
    subroutine forge_qpainter_draw_pixmap_rect(this, target_rect, pixmap, source_rect)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: target_rect
        type(forge_qpixmap_t), intent(in) :: pixmap
        type(forge_rect), intent(in), optional :: source_rect

        if (.not. this%is_active() .or. pixmap%is_null()) return

        if (present(source_rect)) then
            call cairo_set_source_surface(this%cr_, pixmap%painter(), &
                real(target_rect%pos%x - source_rect%pos%x, c_double), &
                real(target_rect%pos%y - source_rect%pos%y, c_double))
        else
            call cairo_set_source_surface(this%cr_, pixmap%painter(), &
                real(target_rect%pos%x, c_double), real(target_rect%pos%y, c_double))
        end if

        if (this%smooth_pixmap_transform_) then
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_BILINEAR)
        else
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_NEAREST)
        end if

        call cairo_rectangle(this%cr_, real(target_rect%pos%x, c_double), real(target_rect%pos%y, c_double), &
                           real(target_rect%size%width, c_double), real(target_rect%size%height, c_double))
        call cairo_fill(this%cr_)
    end subroutine forge_qpainter_draw_pixmap_rect

    !> @brief Draw image
    subroutine forge_qpainter_draw_image(this, x, y, image)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y
        type(forge_qpixmap_t), intent(in) :: image
        
        if (.not. this%is_active() .or. image%is_null()) return
        
        call cairo_set_source_surface(this%cr_, image%painter(), x, y)
        if (this%smooth_pixmap_transform_) then
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_BILINEAR)
        else
            call cairo_pattern_set_filter(cairo_get_source(this%cr_), CAIRO_FILTER_NEAREST)
        end if
        call cairo_paint(this%cr_)
    end subroutine forge_qpainter_draw_image

    !> @brief Translate coordinate system
    subroutine forge_qpainter_translate(this, dx, dy)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: dx, dy

        if (.not. this%is_active()) return

        call cairo_translate(this%cr_, dx, dy)
    end subroutine forge_qpainter_translate

    !> @brief Rotate coordinate system
    subroutine forge_qpainter_rotate(this, angle)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: angle

        if (.not. this%is_active()) return

        call cairo_rotate(this%cr_, angle)
    end subroutine forge_qpainter_rotate

    !> @brief Scale coordinate system
    subroutine forge_qpainter_scale(this, sx, sy)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: sx, sy

        if (.not. this%is_active()) return

        call cairo_scale(this%cr_, sx, sy)
    end subroutine forge_qpainter_scale

    !> @brief Shear coordinate system (not directly supported in Cairo)
    subroutine forge_qpainter_shear(this, sh, sv)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: sh, sv
        real(c_double) :: matrix(6)

        if (.not. this%is_active()) return

        ! Get current matrix, modify for shear, set back
        call cairo_get_matrix(this%cr_, c_loc(matrix))
        matrix(3) = matrix(3) + sh * matrix(1)  ! xx component
        matrix(4) = matrix(4) + sh * matrix(2)  ! xy component
        matrix(5) = matrix(5) + sv * matrix(3)  ! yx component
        matrix(6) = matrix(6) + sv * matrix(4)  ! yy component
        call cairo_set_matrix(this%cr_, c_loc(matrix))
    end subroutine forge_qpainter_shear

    !> @brief Apply transformation matrix
    subroutine forge_qpainter_transform(this, matrix)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: matrix(6)

        if (.not. this%is_active()) return

        call cairo_transform(this%cr_, c_loc(matrix))
    end subroutine forge_qpainter_transform

    !> @brief Set transformation matrix
    subroutine forge_qpainter_set_transform(this, matrix)
        class(forge_qpainter_t), intent(inout) :: this
        real(c_double), intent(in) :: matrix(6)

        if (.not. this%is_active()) return

        call cairo_set_matrix(this%cr_, c_loc(matrix))
    end subroutine forge_qpainter_set_transform

    !> @brief Reset transformation to identity
    subroutine forge_qpainter_reset_transform(this)
        class(forge_qpainter_t), intent(inout) :: this

        if (.not. this%is_active()) return

        call cairo_identity_matrix(this%cr_)
    end subroutine forge_qpainter_reset_transform

    !> @brief Save painter state
    subroutine forge_qpainter_save(this)
        class(forge_qpainter_t), intent(inout) :: this

        if (.not. this%is_active()) return

        call cairo_save(this%cr_)
    end subroutine forge_qpainter_save

    !> @brief Restore painter state
    subroutine forge_qpainter_restore(this)
        class(forge_qpainter_t), intent(inout) :: this

        if (.not. this%is_active()) return

        call cairo_restore(this%cr_)
    end subroutine forge_qpainter_restore

    !> @brief Set clipping rectangle
    subroutine forge_qpainter_set_clip_rect(this, rect, operation)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        integer, intent(in), optional :: operation

        if (.not. this%is_active()) return

        call cairo_rectangle(this%cr_, real(rect%pos%x, c_double), real(rect%pos%y, c_double), &
                           real(rect%size%width, c_double), real(rect%size%height, c_double))
        call cairo_clip(this%cr_)
        this%has_clipping_ = .true.
    end subroutine forge_qpainter_set_clip_rect

    !> @brief Set clipping region
    subroutine forge_qpainter_set_clip_region(this, region, operation)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_qpath_t), intent(in) :: region
        integer, intent(in), optional :: operation
        
        if (.not. this%is_active()) return
        
        call region%apply_to_cairo(this%cr_)
        call cairo_clip(this%cr_)
        this%has_clipping_ = .true.
    end subroutine forge_qpainter_set_clip_region
        type(c_ptr), intent(in) :: path
        integer, intent(in), optional :: operation

        if (.not. this%is_active()) return

        call cairo_append_path(this%cr_, path)
        call cairo_clip(this%cr_)
        this%has_clipping_ = .true.
    end subroutine forge_qpainter_set_clip_path

    !> @brief Enable/disable clipping
    subroutine forge_qpainter_set_clipping(this, enable)
        class(forge_qpainter_t), intent(inout) :: this
        logical, intent(in) :: enable

        if (.not. this%is_active()) return

        if (enable) then
            ! Reset clipping to full area
            call cairo_reset_clip(this%cr_)
            this%has_clipping_ = .false.
        else
            call cairo_reset_clip(this%cr_)
            this%has_clipping_ = .false.
        end if
    end subroutine forge_qpainter_set_clipping

    !> @brief Check if clipping is active
    function forge_qpainter_has_clipping(this) result(has_clipping)
        class(forge_qpainter_t), intent(in) :: this
        logical :: has_clipping
        has_clipping = this%has_clipping_
    end function forge_qpainter_has_clipping

    !> @brief Get clipping bounding rectangle
    function forge_qpainter_clip_bounding_rect(this) result(rect)
        class(forge_qpainter_t), intent(in) :: this
        type(forge_rect) :: rect
        ! Implementation would require getting clip extents from Cairo
        rect%pos%x = 0
        rect%pos%y = 0
        rect%size%width = 0
        rect%size%height = 0
    end function forge_qpainter_clip_bounding_rect

    !> @brief Get bounding rectangle of text
    function forge_qpainter_bounding_rect(this, rect, text) result(bounding_rect)
        class(forge_qpainter_t), intent(in) :: this
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        type(forge_rect) :: bounding_rect
        ! Simple implementation
        bounding_rect = rect
    end function forge_qpainter_bounding_rect

    !> @brief Erase rectangle with background color
    subroutine forge_qpainter_erase_rect(this, rect)
        class(forge_qpainter_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect

        if (.not. this%is_active()) return

        call cairo_set_source_rgba(this%cr_, this%background_color_%r, &
                                 this%background_color_%g, this%background_color_%b, &
                                 this%background_color_%a)
        call cairo_rectangle(this%cr_, real(rect%pos%x, c_double), real(rect%pos%y, c_double), &
                           real(rect%size%width, c_double), real(rect%size%height, c_double))
        call cairo_fill(this%cr_)
    end subroutine forge_qpainter_erase_rect

    !> @brief Set background mode
    subroutine forge_qpainter_set_background_mode(this, mode)
        class(forge_qpainter_t), intent(inout) :: this
        integer, intent(in) :: mode
        ! Background mode affects how background is handled during text drawing
        ! For now, store the mode - full implementation would affect text rendering
    end subroutine forge_qpainter_set_background_mode
    end subroutine forge_qpainter_set_background_mode

    !> @brief Get background mode
    function forge_qpainter_get_background_mode(this) result(mode)
        class(forge_qpainter_t), intent(in) :: this
        integer :: mode
        mode = 0  ! Opaque mode
    end function forge_qpainter_get_background_mode

    !> @brief Set render hint
    subroutine forge_qpainter_set_render_hint(this, hint, on)
        class(forge_qpainter_t), intent(inout) :: this
        integer, intent(in) :: hint
        logical, intent(in), optional :: on
        logical :: enable_hint
        
        enable_hint = .true.
        if (present(on)) enable_hint = on
        
        select case (hint)
        case (1) ! Antialiasing
            if (enable_hint) then
                call this%set_antialiasing(PAINTER_ANTIALIASING_GOOD)
            else
                call this%set_antialiasing(PAINTER_ANTIALIASING_NONE)
            end if
