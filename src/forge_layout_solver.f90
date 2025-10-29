!> @brief Layout constraint solver for ForGE
!> @details Provides advanced constraint solving for layout management
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_layout_solver
    use iso_c_binding
    use forge_types
    use forge_widgets
    implicit none
    private

    public :: forge_layout_solver_base, forge_box_layout_solver, forge_grid_layout_solver
    public :: forge_form_layout_solver, forge_stack_layout_solver
    public :: layout_constraint, layout_solution

    !> @brief Layout constraint structure
    type :: layout_constraint
        integer(c_int) :: min_width = 0
        integer(c_int) :: min_height = 0
        integer(c_int) :: max_width = 999999
        integer(c_int) :: max_height = 999999
        integer(c_int) :: preferred_width = 0
        integer(c_int) :: preferred_height = 0
        integer(c_int) :: stretch_factor = 0
        logical :: expanding = .false.
        logical :: fixed = .false.
    end type layout_constraint

    !> @brief Layout solution structure
    type :: layout_solution
        integer(c_int) :: x = 0
        integer(c_int) :: y = 0
        integer(c_int) :: width = 0
        integer(c_int) :: height = 0
    end type layout_solution

    !> @brief Base layout solver class
    type, abstract :: forge_layout_solver_base
        private
        integer(c_int) :: available_width = 0
        integer(c_int) :: available_height = 0
        integer(c_int) :: padding = 5
        integer(c_int) :: spacing = 5
    contains
        procedure :: set_available_size => forge_layout_solver_set_available_size
        procedure :: set_padding => forge_layout_solver_set_padding
        procedure :: set_spacing => forge_layout_solver_set_spacing
        procedure :: get_available_width => forge_layout_solver_get_available_width
        procedure :: get_available_height => forge_layout_solver_get_available_height
        procedure :: get_padding => forge_layout_solver_get_padding
        procedure :: get_spacing => forge_layout_solver_get_spacing
        procedure(solver_solve_interface), deferred :: solve
        procedure(solver_get_constraint_interface), deferred :: get_constraint
    end type forge_layout_solver_base

    !> @brief Box layout solver
    type, extends(forge_layout_solver_base) :: forge_box_layout_solver
        private
        integer :: orientation = 1 ! 1=horizontal, 2=vertical
    contains
        procedure :: set_orientation => forge_box_layout_solver_set_orientation
        procedure :: solve => forge_box_layout_solver_solve
        procedure :: get_constraint => forge_box_layout_solver_get_constraint
    end type forge_box_layout_solver

    !> @brief Grid layout solver
    type, extends(forge_layout_solver_base) :: forge_grid_layout_solver
        private
        integer(c_int) :: rows = 1
        integer(c_int) :: columns = 1
    contains
        procedure :: set_dimensions => forge_grid_layout_solver_set_dimensions
        procedure :: solve => forge_grid_layout_solver_solve
        procedure :: get_constraint => forge_grid_layout_solver_get_constraint
    end type forge_grid_layout_solver

    !> @brief Form layout solver
    type, extends(forge_layout_solver_base) :: forge_form_layout_solver
    contains
        procedure :: solve => forge_form_layout_solver_solve
        procedure :: get_constraint => forge_form_layout_solver_get_constraint
    end type forge_form_layout_solver

    !> @brief Stack layout solver
    type, extends(forge_layout_solver_base) :: forge_stack_layout_solver
        private
        integer(c_int) :: current_index = 1
    contains
        procedure :: set_current_index => forge_stack_layout_solver_set_current_index
        procedure :: solve => forge_stack_layout_solver_solve
        procedure :: get_constraint => forge_stack_layout_solver_get_constraint
    end type forge_stack_layout_solver

    !> Abstract interfaces
    abstract interface
        subroutine solver_solve_interface(this, widgets, solutions)
            import :: forge_layout_solver_base, forge_widget, layout_solution
            class(forge_layout_solver_base), intent(inout) :: this
            class(forge_widget), intent(in) :: widgets(:)
            type(layout_solution), intent(out), allocatable :: solutions(:)
        end subroutine solver_solve_interface

        function solver_get_constraint_interface(this, widget) result(constraint)
            import :: forge_layout_solver_base, forge_widget, layout_constraint
            class(forge_layout_solver_base), intent(in) :: this
            class(forge_widget), intent(in) :: widget
            type(layout_constraint) :: constraint
        end function solver_get_constraint_interface
    end interface

contains

    ! ========== Base Solver Methods ==========

    subroutine forge_layout_solver_set_available_size(this, width, height)
        class(forge_layout_solver_base), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        this%available_width = width
        this%available_height = height
    end subroutine forge_layout_solver_set_available_size

    subroutine forge_layout_solver_set_padding(this, padding)
        class(forge_layout_solver_base), intent(inout) :: this
        integer(c_int), intent(in) :: padding
        this%padding = padding
    end subroutine forge_layout_solver_set_padding

    subroutine forge_layout_solver_set_spacing(this, spacing)
        class(forge_layout_solver_base), intent(inout) :: this
        integer(c_int), intent(in) :: spacing
        this%spacing = spacing
    end subroutine forge_layout_solver_set_spacing

    function forge_layout_solver_get_available_width(this) result(width)
        class(forge_layout_solver_base), intent(in) :: this
        integer(c_int) :: width
        width = this%available_width
    end function forge_layout_solver_get_available_width

    function forge_layout_solver_get_available_height(this) result(height)
        class(forge_layout_solver_base), intent(in) :: this
        integer(c_int) :: height
        height = this%available_height
    end function forge_layout_solver_get_available_height

    function forge_layout_solver_get_padding(this) result(padding)
        class(forge_layout_solver_base), intent(in) :: this
        integer(c_int) :: padding
        padding = this%padding
    end function forge_layout_solver_get_padding

    function forge_layout_solver_get_spacing(this) result(spacing)
        class(forge_layout_solver_base), intent(in) :: this
        integer(c_int) :: spacing
        spacing = this%spacing
    end function forge_layout_solver_get_spacing

    ! ========== Box Layout Solver ==========

    subroutine forge_box_layout_solver_set_orientation(this, orientation)
        class(forge_box_layout_solver), intent(inout) :: this
        integer, intent(in) :: orientation
        this%orientation = orientation
    end subroutine forge_box_layout_solver_set_orientation

    subroutine forge_box_layout_solver_solve(this, widgets, solutions)
        class(forge_box_layout_solver), intent(inout) :: this
        class(forge_widget), intent(in) :: widgets(:)
        type(layout_solution), intent(out), allocatable :: solutions(:)

        integer :: i, num_widgets
        integer(c_int) :: total_fixed, total_stretch, stretch_space, x_pos, y_pos
        integer(c_int) :: widget_width, widget_height
        type(layout_constraint) :: constraint
        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        num_widgets = size(widgets)
        allocate(solutions(num_widgets))

        if (num_widgets == 0) return

        ! Calculate total fixed space and stretch factors
        total_fixed = 0
        total_stretch = 0

        do i = 1, num_widgets
            constraint = this%get_constraint(widgets(i))
            if (this%orientation == 1) then ! Horizontal
                if (.not. constraint%expanding) then
                    total_fixed = total_fixed + constraint%preferred_width
                else
                    total_stretch = total_stretch + constraint%stretch_factor
                end if
            else ! Vertical
                if (.not. constraint%expanding) then
                    total_fixed = total_fixed + constraint%preferred_height
                else
                    total_stretch = total_stretch + constraint%stretch_factor
                end if
            end if
        end do

        ! Calculate spacing
        total_fixed = total_fixed + (num_widgets - 1) * this%get_spacing()

        ! Calculate stretch space
        if (this%orientation == 1) then ! Horizontal
            stretch_space = max(0, this%get_available_width() - 2 * this%get_padding() - total_fixed) / max(1, total_stretch)
        else ! Vertical
            stretch_space = max(0, this%get_available_height() - 2 * this%get_padding() - total_fixed) / max(1, total_stretch)
        end if

        ! Position widgets
        x_pos = this%get_padding()
        y_pos = this%get_padding()

        do i = 1, num_widgets
            constraint = this%get_constraint(widgets(i))
            hint = widgets(i)%get_size_hint()
            min_size = widgets(i)%get_minimum_size()
            max_size = widgets(i)%get_maximum_size()
            policy = widgets(i)%get_size_policy()

            if (this%orientation == 1) then ! Horizontal
                widget_height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, &
                                                              this%get_available_height() - 2 * this%get_padding())

                if (.not. constraint%expanding) then
                    widget_width = constraint%preferred_width
                else
                    widget_width = stretch_space * constraint%stretch_factor
                end if

                solutions(i)%x = x_pos
                solutions(i)%y = y_pos
                solutions(i)%width = widget_width
                solutions(i)%height = widget_height

                x_pos = x_pos + widget_width + this%get_spacing()
            else ! Vertical
                widget_width = policy%calculate_horizontal_size(hint%width, min_size%width, max_size%width, &
                                                               this%get_available_width() - 2 * this%get_padding())

                if (.not. constraint%expanding) then
                    widget_height = constraint%preferred_height
                else
                    widget_height = stretch_space * constraint%stretch_factor
                end if

                solutions(i)%x = x_pos
                solutions(i)%y = y_pos
                solutions(i)%width = widget_width
                solutions(i)%height = widget_height

                y_pos = y_pos + widget_height + this%get_spacing()
            end if
        end do
    end subroutine forge_box_layout_solver_solve

    function forge_box_layout_solver_get_constraint(this, widget) result(constraint)
        class(forge_box_layout_solver), intent(in) :: this
        class(forge_widget), intent(in) :: widget
        type(layout_constraint) :: constraint

        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        hint = widget%get_size_hint()
        min_size = widget%get_minimum_size()
        max_size = widget%get_maximum_size()
        policy = widget%get_size_policy()

        constraint%min_width = min_size%width
        constraint%min_height = min_size%height
        constraint%max_width = max_size%width
        constraint%max_height = max_size%height
        constraint%preferred_width = hint%width
        constraint%preferred_height = hint%height

        if (this%orientation == 1) then ! Horizontal
            constraint%expanding = policy%is_horizontal_expanding()
            constraint%stretch_factor = policy%get_horizontal_stretch_factor()
            constraint%fixed = policy%is_horizontal_fixed()
        else ! Vertical
            constraint%expanding = policy%is_vertical_expanding()
            constraint%stretch_factor = policy%get_vertical_stretch_factor()
            constraint%fixed = policy%is_vertical_fixed()
        end if
    end function forge_box_layout_solver_get_constraint

    ! ========== Grid Layout Solver ==========

    subroutine forge_grid_layout_solver_set_dimensions(this, rows, columns)
        class(forge_grid_layout_solver), intent(inout) :: this
        integer(c_int), intent(in) :: rows, columns
        this%rows = rows
        this%columns = columns
    end subroutine forge_grid_layout_solver_set_dimensions

    subroutine forge_grid_layout_solver_solve(this, widgets, solutions)
        class(forge_grid_layout_solver), intent(inout) :: this
        class(forge_widget), intent(in) :: widgets(:)
        type(layout_solution), intent(out), allocatable :: solutions(:)

        integer :: i, num_widgets, row, col
        integer(c_int) :: cell_width, cell_height, x_pos, y_pos
        type(layout_constraint) :: constraint
        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        num_widgets = size(widgets)
        allocate(solutions(num_widgets))

        if (num_widgets == 0) return

        ! Calculate cell dimensions
        cell_width = (this%get_available_width() - 2 * this%get_padding() - (this%columns - 1) * this%get_spacing()) / this%columns
        cell_height = (this%get_available_height() - 2 * this%get_padding() - (this%rows - 1) * this%get_spacing()) / this%rows

        do i = 1, num_widgets
            ! Calculate grid position (simple row-major order for now)
            row = (i - 1) / this%columns + 1
            col = mod(i - 1, this%columns) + 1

            constraint = this%get_constraint(widgets(i))
            hint = widgets(i)%get_size_hint()
            min_size = widgets(i)%get_minimum_size()
            max_size = widgets(i)%get_maximum_size()
            policy = widgets(i)%get_size_policy()

            x_pos = this%get_padding() + (col - 1) * (cell_width + this%get_spacing())
            y_pos = this%get_padding() + (row - 1) * (cell_height + this%get_spacing())

            solutions(i)%x = x_pos
            solutions(i)%y = y_pos
            solutions(i)%width = policy%calculate_horizontal_size(hint%width, min_size%width, max_size%width, cell_width)
            solutions(i)%height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, cell_height)
        end do
    end subroutine forge_grid_layout_solver_solve

    function forge_grid_layout_solver_get_constraint(this, widget) result(constraint)
        class(forge_grid_layout_solver), intent(in) :: this
        class(forge_widget), intent(in) :: widget
        type(layout_constraint) :: constraint

        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        hint = widget%get_size_hint()
        min_size = widget%get_minimum_size()
        max_size = widget%get_maximum_size()
        policy = widget%get_size_policy()

        constraint%min_width = min_size%width
        constraint%min_height = min_size%height
        constraint%max_width = max_size%width
        constraint%max_height = max_size%height
        constraint%preferred_width = hint%width
        constraint%preferred_height = hint%height
        constraint%expanding = policy%is_horizontal_expanding() .or. policy%is_vertical_expanding()
        constraint%stretch_factor = max(policy%get_horizontal_stretch_factor(), policy%get_vertical_stretch_factor())
        constraint%fixed = policy%is_horizontal_fixed() .and. policy%is_vertical_fixed()
    end function forge_grid_layout_solver_get_constraint

    ! ========== Form Layout Solver ==========

    subroutine forge_form_layout_solver_solve(this, widgets, solutions)
        class(forge_form_layout_solver), intent(inout) :: this
        class(forge_widget), intent(in) :: widgets(:)
        type(layout_solution), intent(out), allocatable :: solutions(:)

        integer :: i, num_widgets, num_pairs
        integer(c_int) :: max_label_width, field_width, y_pos
        type(layout_constraint) :: constraint
        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        num_widgets = size(widgets)
        allocate(solutions(num_widgets))

        if (num_widgets == 0) return

        ! Assume widgets are in label-field pairs
        num_pairs = num_widgets / 2

        ! Calculate maximum label width
        max_label_width = 0
        do i = 1, num_pairs
            hint = widgets(2*i-1)%get_size_hint()
            max_label_width = max(max_label_width, hint%width)
        end do

        field_width = this%get_available_width() - 2 * this%get_padding() - max_label_width - this%get_spacing()

        y_pos = this%get_padding()

        do i = 1, num_pairs
            ! Label
            constraint = this%get_constraint(widgets(2*i-1))
            hint = widgets(2*i-1)%get_size_hint()
            min_size = widgets(2*i-1)%get_minimum_size()
            max_size = widgets(2*i-1)%get_maximum_size()
            policy = widgets(2*i-1)%get_size_policy()

            solutions(2*i-1)%x = this%get_padding()
            solutions(2*i-1)%y = y_pos
            solutions(2*i-1)%width = max_label_width
            solutions(2*i-1)%height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, &
                                                                   this%get_available_height() - 2 * this%get_padding())

            ! Field
            constraint = this%get_constraint(widgets(2*i))
            hint = widgets(2*i)%get_size_hint()
            min_size = widgets(2*i)%get_minimum_size()
            max_size = widgets(2*i)%get_maximum_size()
            policy = widgets(2*i)%get_size_policy()

            solutions(2*i)%x = this%get_padding() + max_label_width + this%get_spacing()
            solutions(2*i)%y = y_pos
            solutions(2*i)%width = policy%calculate_horizontal_size(hint%width, min_size%width, max_size%width, field_width)
            solutions(2*i)%height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, &
                                                                  this%get_available_height() - 2 * this%get_padding())

            y_pos = y_pos + max(solutions(2*i-1)%height, solutions(2*i)%height) + this%get_spacing()
        end do
    end subroutine forge_form_layout_solver_solve

    function forge_form_layout_solver_get_constraint(this, widget) result(constraint)
        class(forge_form_layout_solver), intent(in) :: this
        class(forge_widget), intent(in) :: widget
        type(layout_constraint) :: constraint

        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        hint = widget%get_size_hint()
        min_size = widget%get_minimum_size()
        max_size = widget%get_maximum_size()
        policy = widget%get_size_policy()

        constraint%min_width = min_size%width
        constraint%min_height = min_size%height
        constraint%max_width = max_size%width
        constraint%max_height = max_size%height
        constraint%preferred_width = hint%width
        constraint%preferred_height = hint%height
        constraint%expanding = policy%is_horizontal_expanding() .or. policy%is_vertical_expanding()
        constraint%stretch_factor = max(policy%get_horizontal_stretch_factor(), policy%get_vertical_stretch_factor())
        constraint%fixed = policy%is_horizontal_fixed() .and. policy%is_vertical_fixed()
    end function forge_form_layout_solver_get_constraint

    ! ========== Stack Layout Solver ==========

    subroutine forge_stack_layout_solver_set_current_index(this, index)
        class(forge_stack_layout_solver), intent(inout) :: this
        integer(c_int), intent(in) :: index
        this%current_index = index
    end subroutine forge_stack_layout_solver_set_current_index

    subroutine forge_stack_layout_solver_solve(this, widgets, solutions)
        class(forge_stack_layout_solver), intent(inout) :: this
        class(forge_widget), intent(in) :: widgets(:)
        type(layout_solution), intent(out), allocatable :: solutions(:)

        integer :: i, num_widgets
        type(layout_constraint) :: constraint
        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        num_widgets = size(widgets)
        allocate(solutions(num_widgets))

        if (num_widgets == 0) return

        do i = 1, num_widgets
            constraint = this%get_constraint(widgets(i))
            hint = widgets(i)%get_size_hint()
            min_size = widgets(i)%get_minimum_size()
            max_size = widgets(i)%get_maximum_size()
            policy = widgets(i)%get_size_policy()

            if (i == this%current_index) then
                ! Show current widget
                solutions(i)%x = this%get_padding()
                solutions(i)%y = this%get_padding()
                solutions(i)%width = policy%calculate_horizontal_size(hint%width, min_size%width, max_size%width, &
                                                                    this%get_available_width() - 2 * this%get_padding())
                solutions(i)%height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, &
                                                                   this%get_available_height() - 2 * this%get_padding())
            else
                ! Hide other widgets
                solutions(i)%x = 0
                solutions(i)%y = 0
                solutions(i)%width = 0
                solutions(i)%height = 0
            end if
        end do
    end subroutine forge_stack_layout_solver_solve

    function forge_stack_layout_solver_get_constraint(this, widget) result(constraint)
        class(forge_stack_layout_solver), intent(in) :: this
        class(forge_widget), intent(in) :: widget
        type(layout_constraint) :: constraint

        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy

        hint = widget%get_size_hint()
        min_size = widget%get_minimum_size()
        max_size = widget%get_maximum_size()
        policy = widget%get_size_policy()

        constraint%min_width = min_size%width
        constraint%min_height = min_size%height
        constraint%max_width = max_size%width
        constraint%max_height = max_size%height
        constraint%preferred_width = hint%width
        constraint%preferred_height = hint%height
        constraint%expanding = policy%is_horizontal_expanding() .or. policy%is_vertical_expanding()
        constraint%stretch_factor = max(policy%get_horizontal_stretch_factor(), policy%get_vertical_stretch_factor())
        constraint%fixed = policy%is_horizontal_fixed() .and. policy%is_vertical_fixed()
    end function forge_stack_layout_solver_get_constraint

end module forge_layout_solver