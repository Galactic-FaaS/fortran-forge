!> @brief QPainterPath-like module for path-based drawing
!> @details Implements Qt-style paths with Bezier curves and complex shapes
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qpath
    use iso_c_binding
    use forge_types
    use forge_cairo_bindings
    implicit none
    private

    public :: forge_qpath_t
    public :: PATH_ELEMENT_MOVE_TO, PATH_ELEMENT_LINE_TO, PATH_ELEMENT_CURVE_TO
    public :: PATH_ELEMENT_CLOSE_SUBPATH

    ! Path element types
    integer, parameter :: PATH_ELEMENT_MOVE_TO = 1
    integer, parameter :: PATH_ELEMENT_LINE_TO = 2
    integer, parameter :: PATH_ELEMENT_CURVE_TO = 3
    integer, parameter :: PATH_ELEMENT_CLOSE_SUBPATH = 4

    !> @brief Path element structure
    type :: path_element_t
        integer :: type = 0
        real(c_double) :: x = 0.0_c_double
        real(c_double) :: y = 0.0_c_double
        real(c_double) :: x1 = 0.0_c_double  ! Control point 1 for curves
        real(c_double) :: y1 = 0.0_c_double
        real(c_double) :: x2 = 0.0_c_double  ! Control point 2 for curves
        real(c_double) :: y2 = 0.0_c_double
    end type path_element_t

    !> @brief QPainterPath-like class for complex path drawing
    type :: forge_qpath_t
        private
        type(path_element_t), allocatable :: elements_(:)
        real(c_double) :: current_x_ = 0.0_c_double
        real(c_double) :: current_y_ = 0.0_c_double
        type(forge_rect) :: bounding_rect_
        logical :: dirty_bounding_rect_ = .true.
    contains
        procedure :: move_to => forge_qpath_move_to
        procedure :: line_to => forge_qpath_line_to
        procedure :: curve_to => forge_qpath_curve_to
        procedure :: quad_to => forge_qpath_quad_to
        procedure :: arc_to => forge_qpath_arc_to
        procedure :: close_subpath => forge_qpath_close_subpath
        procedure :: add_rect => forge_qpath_add_rect
        procedure :: add_ellipse => forge_qpath_add_ellipse
        procedure :: add_polygon => forge_qpath_add_polygon
        procedure :: add_path => forge_qpath_add_path
        procedure :: clear => forge_qpath_clear
        procedure :: is_empty => forge_qpath_is_empty
        procedure :: element_count => forge_qpath_element_count
        procedure :: element_at => forge_qpath_element_at
        procedure :: current_position => forge_qpath_current_position
        procedure :: bounding_rect => forge_qpath_bounding_rect
        procedure :: contains => forge_qpath_contains
        procedure :: intersects => forge_qpath_intersects
        procedure :: translate => forge_qpath_translate
        procedure :: apply_to_cairo => forge_qpath_apply_to_cairo
        procedure :: fill_rule => forge_qpath_fill_rule
        procedure :: set_fill_rule => forge_qpath_set_fill_rule
        procedure :: simplified => forge_qpath_simplified
        procedure :: to_reversed => forge_qpath_to_reversed
        procedure :: united => forge_qpath_united
        procedure :: intersected => forge_qpath_intersected
        procedure :: subtracted => forge_qpath_subtracted
        procedure :: to_fill_polygon => forge_qpath_to_fill_polygon
        procedure :: to_fill_polygons => forge_qpath_to_fill_polygons
    end type forge_qpath_t

contains

    !> @brief Move to position
    subroutine forge_qpath_move_to(this, x, y)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y

        call this%add_element(PATH_ELEMENT_MOVE_TO, x, y)
        this%current_x_ = x
        this%current_y_ = y
        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_move_to

    !> @brief Draw line to position
    subroutine forge_qpath_line_to(this, x, y)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: x, y

        call this%add_element(PATH_ELEMENT_LINE_TO, x, y)
        this%current_x_ = x
        this%current_y_ = y
        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_line_to

    !> @brief Draw cubic Bezier curve
    subroutine forge_qpath_curve_to(this, c1x, c1y, c2x, c2y, ex, ey)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: c1x, c1y, c2x, c2y, ex, ey

        call this%add_element(PATH_ELEMENT_CURVE_TO, ex, ey, c1x, c1y, c2x, c2y)
        this%current_x_ = ex
        this%current_y_ = ey
        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_curve_to

    !> @brief Draw quadratic Bezier curve
    subroutine forge_qpath_quad_to(this, cx, cy, ex, ey)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: cx, cy, ex, ey
        real(c_double) :: c1x, c1y, c2x, c2y

        ! Convert quadratic to cubic Bezier
        c1x = this%current_x_ + 2.0_c_double/3.0_c_double * (cx - this%current_x_)
        c1y = this%current_y_ + 2.0_c_double/3.0_c_double * (cy - this%current_y_)
        c2x = ex + 2.0_c_double/3.0_c_double * (cx - ex)
        c2y = ey + 2.0_c_double/3.0_c_double * (cy - ey)

        call this%curve_to(c1x, c1y, c2x, c2y, ex, ey)
    end subroutine forge_qpath_quad_to

    !> @brief Draw arc to position
    subroutine forge_qpath_arc_to(this, rect, start_angle, sweep_angle)
        class(forge_qpath_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: start_angle, sweep_angle
        real(c_double) :: cx, cy, radius_x, radius_y
        integer :: i, segments
        real(c_double) :: angle_step, current_angle, end_angle

        cx = real(rect%pos%x, c_double) + real(rect%size%width, c_double)/2.0_c_double
        cy = real(rect%pos%y, c_double) + real(rect%size%height, c_double)/2.0_c_double
        radius_x = real(rect%size%width, c_double)/2.0_c_double
        radius_y = real(rect%size%height, c_double)/2.0_c_double

        segments = max(8, int(abs(sweep_angle) / (3.14159_c_double/8.0_c_double)))
        angle_step = sweep_angle / real(segments, c_double)
        current_angle = start_angle
        end_angle = start_angle + sweep_angle

        do i = 1, segments
            real(c_double) :: next_angle, x, y
            next_angle = current_angle + angle_step
            x = cx + radius_x * cos(next_angle)
            y = cy + radius_y * sin(next_angle)

            if (i == 1) then
                call this%line_to(x, y)
            else
                call this%line_to(x, y)
            end if

            current_angle = next_angle
        end do

        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_arc_to

    !> @brief Close current subpath
    subroutine forge_qpath_close_subpath(this)
        class(forge_qpath_t), intent(inout) :: this
        call this%add_element(PATH_ELEMENT_CLOSE_SUBPATH, 0.0_c_double, 0.0_c_double)
    end subroutine forge_qpath_close_subpath

    !> @brief Add rectangle to path
    subroutine forge_qpath_add_rect(this, rect)
        class(forge_qpath_t), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        real(c_double) :: x, y, w, h

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        call this%move_to(x, y)
        call this%line_to(x + w, y)
        call this%line_to(x + w, y + h)
        call this%line_to(x, y + h)
        call this%close_subpath()
    end subroutine forge_qpath_add_rect

    !> @brief Add ellipse to path
    subroutine forge_qpath_add_ellipse(this, center_x, center_y, radius_x, radius_y)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: center_x, center_y, radius_x, radius_y
        real(c_double) :: kappa, ox, oy, xm, ym

        ! Approximation using 4 cubic Bezier curves
        kappa = 4.0_c_double * (sqrt(2.0_c_double) - 1.0_c_double) / 3.0_c_double
        ox = radius_x * kappa
        oy = radius_y * kappa
        xm = center_x + radius_x
        ym = center_y + radius_y

        call this%move_to(center_x + radius_x, center_y)
        call this%curve_to(xm, center_y - oy, xm - ox, ym - oy, xm - ox, ym)
        call this%curve_to(center_x + ox, ym, center_x + ox, ym - oy, center_x, ym - oy)
        call this%curve_to(center_x - ox, ym, center_x - ox, center_y + oy, center_x - ox, center_y)
        call this%curve_to(center_x - ox, center_y - oy, center_x + ox, center_y - oy, center_x, center_y - oy)
        call this%close_subpath()
    end subroutine forge_qpath_add_ellipse

    !> @brief Add polygon to path
    subroutine forge_qpath_add_polygon(this, points)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: points(:,:)
        integer :: i

        if (size(points, 2) > 0) then
            call this%move_to(points(1,1), points(2,1))
            do i = 2, size(points, 2)
                call this%line_to(points(1,i), points(2,i))
            end do
            call this%close_subpath()
        end if
    end subroutine forge_qpath_add_polygon

    !> @brief Add another path to this path
    subroutine forge_qpath_add_path(this, path)
        class(forge_qpath_t), intent(inout) :: this
        type(forge_qpath_t), intent(in) :: path
        integer :: i

        do i = 1, path%element_count()
            call this%add_element_from_other(path%element_at(i))
        end do
        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_add_path

    !> @brief Clear path
    subroutine forge_qpath_clear(this)
        class(forge_qpath_t), intent(inout) :: this
        if (allocated(this%elements_)) deallocate(this%elements_)
        this%current_x_ = 0.0_c_double
        this%current_y_ = 0.0_c_double
        this%dirty_bounding_rect_ = .true.
    end subroutine forge_qpath_clear

    !> @brief Check if path is empty
    function forge_qpath_is_empty(this) result(empty)
        class(forge_qpath_t), intent(in) :: this
        logical :: empty
        empty = .not. allocated(this%elements_) .or. size(this%elements_) == 0
    end function forge_qpath_is_empty

    !> @brief Get element count
    function forge_qpath_element_count(this) result(count)
        class(forge_qpath_t), intent(in) :: this
        integer :: count
        if (allocated(this%elements_)) then
            count = size(this%elements_)
        else
            count = 0
        end if
    end function forge_qpath_element_count

    !> @brief Get element at index
    function forge_qpath_element_at(this, index) result(element)
        class(forge_qpath_t), intent(in) :: this
        integer, intent(in) :: index
        type(path_element_t) :: element

        if (allocated(this%elements_) .and. index >= 1 .and. index <= size(this%elements_)) then
            element = this%elements_(index)
        end if
    end function forge_qpath_element_at

    !> @brief Get current position
    function forge_qpath_current_position(this) result(pos)
        class(forge_qpath_t), intent(in) :: this
        real(c_double) :: pos(2)
        pos(1) = this%current_x_
        pos(2) = this%current_y_
    end function forge_qpath_current_position

    !> @brief Get bounding rectangle
    function forge_qpath_bounding_rect(this) result(rect)
        class(forge_qpath_t), intent(inout) :: this
        type(forge_rect) :: rect

        if (this%dirty_bounding_rect_) then
            call this%update_bounding_rect()
        end if
        rect = this%bounding_rect_
    end function forge_qpath_bounding_rect

    !> @brief Check if point is contained in path
    function forge_qpath_contains(this, point) result(contained)
        class(forge_qpath_t), intent(in) :: this
        real(c_double), intent(in) :: point(2)
        logical :: contained
        ! Implementation would require complex point-in-polygon algorithm
        contained = .false.
    end function forge_qpath_contains

    !> @brief Check if paths intersect
    function forge_qpath_intersects(this, other) result(intersects)
        class(forge_qpath_t), intent(in) :: this
        type(forge_qpath_t), intent(in) :: other
        logical :: intersects
        ! Implementation would require complex geometry algorithms
        intersects = .false.
    end function forge_qpath_intersects

    !> @brief Translate path
    subroutine forge_qpath_translate(this, dx, dy)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double), intent(in) :: dx, dy
        integer :: i

        if (.not. allocated(this%elements_)) return

        do i = 1, size(this%elements_)
            this%elements_(i)%x = this%elements_(i)%x + dx
            this%elements_(i)%y = this%elements_(i)%y + dy
            this%elements_(i)%x1 = this%elements_(i)%x1 + dx
            this%elements_(i)%y1 = this%elements_(i)%y1 + dy
            this%elements_(i)%x2 = this%elements_(i)%x2 + dx
            this%elements_(i)%y2 = this%elements_(i)%y2 + dy
        end do

        this%current_x_ = this%current_x_ + dx
        this%current_y_ = this%current_y_ + dy
        this%bounding_rect_%pos%x = this%bounding_rect_%pos%x + int(dx)
        this%bounding_rect_%pos%y = this%bounding_rect_%pos%y + int(dy)
    end subroutine forge_qpath_translate

    !> @brief Apply path to Cairo context
    subroutine forge_qpath_apply_to_cairo(this, cr)
        class(forge_qpath_t), intent(in) :: this
        type(c_ptr), intent(in) :: cr
        integer :: i

        if (.not. allocated(this%elements_)) return

        call cairo_new_path(cr)

        do i = 1, size(this%elements_)
            select case (this%elements_(i)%type)
            case (PATH_ELEMENT_MOVE_TO)
                call cairo_move_to(cr, this%elements_(i)%x, this%elements_(i)%y)
            case (PATH_ELEMENT_LINE_TO)
                call cairo_line_to(cr, this%elements_(i)%x, this%elements_(i)%y)
            case (PATH_ELEMENT_CURVE_TO)
                call cairo_curve_to(cr, this%elements_(i)%x1, this%elements_(i)%y1, &
                                   this%elements_(i)%x2, this%elements_(i)%y2, &
                                   this%elements_(i)%x, this%elements_(i)%y)
            case (PATH_ELEMENT_CLOSE_SUBPATH)
                call cairo_close_path(cr)
            end select
        end do
    end subroutine forge_qpath_apply_to_cairo

    !> @brief Get fill rule
    function forge_qpath_fill_rule(this) result(rule)
        class(forge_qpath_t), intent(in) :: this
        integer :: rule
        rule = 0  ! Winding fill rule (even-odd would be 1)
    end function forge_qpath_fill_rule

    !> @brief Set fill rule
    subroutine forge_qpath_set_fill_rule(this, rule)
        class(forge_qpath_t), intent(inout) :: this
        integer, intent(in) :: rule
        ! Fill rule affects how self-intersecting paths are filled
        ! 0 = Winding (non-zero), 1 = Even-odd
        ! Implementation would store this and use it in apply_to_cairo
    end subroutine forge_qpath_set_fill_rule
    function forge_qpath_simplified(this) result(path)
        class(forge_qpath_t), intent(in) :: this
        type(forge_qpath_t) :: path
        path = this  ! Return copy
    end function forge_qpath_simplified

    !> @brief Get reversed path
    function forge_qpath_to_reversed(this) result(path)
        class(forge_qpath_t), intent(in) :: this
        type(forge_qpath_t) :: path
        integer :: i, n
        
        n = this%element_count()
        call path%clear()
        
        ! Reverse the order of elements
        do i = n, 1, -1
            call path%add_element_from_other(this%element_at(i))
        end do
        
        ! Reverse move-to/line-to sequences within subpaths
        ! This is a simplified implementation
    end function forge_qpath_to_reversed
    function forge_qpath_intersected(this, other) result(path)
        class(forge_qpath_t), intent(in) :: this
        type(forge_qpath_t), intent(in) :: other
        type(forge_qpath_t) :: path
        ! Complex geometry operation
        path = this
    end function forge_qpath_intersected

    !> @brief Get path with other subtracted
    function forge_qpath_subtracted(this, other) result(path)
        class(forge_qpath_t), intent(in) :: this
        type(forge_qpath_t), intent(in) :: other
        type(forge_qpath_t) :: path
        
        ! Simplified subtraction: return first path
        ! Full implementation would compute actual geometric difference
        path = this
        
        ! This requires complex computational geometry algorithms
        ! such as polygon boolean operations
    end function forge_qpath_subtracted
        ! Implementation would triangulate the path
    end function forge_qpath_to_fill_polygon

    !> @brief Convert to fill polygons
    function forge_qpath_to_fill_polygons(this) result(polygons)
        class(forge_qpath_t), intent(in) :: this
        real(c_double), allocatable :: polygons(:,:,:)
        integer :: n
        
        n = this%element_count()
        if (n > 0) then
            allocate(polygons(2, n, 1))  ! Single polygon
            polygons(:, :, 1) = this%to_fill_polygon()
        end if
        
        ! Full implementation would handle multiple subpaths
    end function forge_qpath_to_fill_polygons
        integer :: n

        if (.not. allocated(this%elements_)) then
            allocate(this%elements_(1))
            this%elements_(1)%type = type
            this%elements_(1)%x = x
            this%elements_(1)%y = y
            if (present(x1)) this%elements_(1)%x1 = x1
            if (present(y1)) this%elements_(1)%y1 = y1
            if (present(x2)) this%elements_(1)%x2 = x2
            if (present(y2)) this%elements_(1)%y2 = y2
        else
            n = size(this%elements_)
            allocate(temp_elements(n + 1))
            temp_elements(1:n) = this%elements_
            temp_elements(n + 1)%type = type
            temp_elements(n + 1)%x = x
            temp_elements(n + 1)%y = y
            if (present(x1)) temp_elements(n + 1)%x1 = x1
            if (present(y1)) temp_elements(n + 1)%y1 = y1
            if (present(x2)) temp_elements(n + 1)%x2 = x2
            if (present(y2)) temp_elements(n + 1)%y2 = y2
            deallocate(this%elements_)
            allocate(this%elements_(n + 1))
            this%elements_ = temp_elements
            deallocate(temp_elements)
        end if
    end subroutine forge_qpath_add_element

    !> @brief Add element from another path
    subroutine forge_qpath_add_element_from_other(this, element)
        class(forge_qpath_t), intent(inout) :: this
        type(path_element_t), intent(in) :: element
        call this%add_element(element%type, element%x, element%y, &
                             element%x1, element%y1, element%x2, element%y2)
    end subroutine forge_qpath_add_element_from_other

    !> @brief Update bounding rectangle
    subroutine forge_qpath_update_bounding_rect(this)
        class(forge_qpath_t), intent(inout) :: this
        real(c_double) :: min_x, min_y, max_x, max_y
        integer :: i

        if (.not. allocated(this%elements_) .or. size(this%elements_) == 0) then
            this%bounding_rect_%pos%x = 0
            this%bounding_rect_%pos%y = 0
            this%bounding_rect_%size%width = 0
            this%bounding_rect_%size%height = 0
            this%dirty_bounding_rect_ = .false.
            return
        end if

        min_x = this%elements_(1)%x
        min_y = this%elements_(1)%y
        max_x = min_x
        max_y = min_y

        do i = 1, size(this%elements_)
            min_x = min(min_x, this%elements_(i)%x)
            min_y = min(min_y, this%elements_(i)%y)
            max_x = max(max_x, this%elements_(i)%x)
            max_y = max(max_y, this%elements_(i)%y)
            ! Also check control points for curves
            if (this%elements_(i)%type == PATH_ELEMENT_CURVE_TO) then
                min_x = min(min_x, this%elements_(i)%x1, this%elements_(i)%x2)
                min_y = min(min_y, this%elements_(i)%y1, this%elements_(i)%y2)
                max_x = max(max_x, this%elements_(i)%x1, this%elements_(i)%x2)
                max_y = max(max_y, this%elements_(i)%y1, this%elements_(i)%y2)
            end if
        end do

        this%bounding_rect_%pos%x = int(min_x)
        this%bounding_rect_%pos%y = int(min_y)
        this%bounding_rect_%size%width = int(max_x - min_x)
        this%bounding_rect_%size%height = int(max_y - min_y)
        this%dirty_bounding_rect_ = .false.
    end subroutine forge_qpath_update_bounding_rect

end module forge_qpath
