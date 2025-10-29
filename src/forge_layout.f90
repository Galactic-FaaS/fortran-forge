!> @brief Layout managers for ForGE
!> @details Provides layout management systems for organizing widgets
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_layout
    use iso_c_binding
    use forge_types
    use forge_widgets
    use forge_containers
    use forge_layout_solver
    implicit none
    private

    public :: forge_layout_base, forge_grid_layout, forge_box_layout, forge_stack_layout, forge_form_layout
    public :: QHBoxLayout, QVBoxLayout, QGridLayout, QFormLayout, QStackedLayout
    public :: LAYOUT_HORIZONTAL, LAYOUT_VERTICAL, QSizePolicy_Fixed, QSizePolicy_Minimum
    public :: QSizePolicy_Maximum, QSizePolicy_Preferred, QSizePolicy_Expanding
    public :: QSizePolicy_MinimumExpanding, QSizePolicy_Ignored
    public :: Qt_AlignLeft, Qt_AlignRight, Qt_AlignHCenter, Qt_AlignTop, Qt_AlignBottom
    public :: Qt_AlignVCenter, Qt_AlignCenter

    !> Layout orientation constants
    integer, parameter :: LAYOUT_HORIZONTAL = 1
    integer, parameter :: LAYOUT_VERTICAL = 2

    !> @brief Base abstract layout class
    type, abstract :: forge_layout_base
        private
        integer :: spacing = 5
        integer :: padding = 5
        logical :: needs_recalc = .true.
        integer(c_int) :: parent_width = 400
        integer(c_int) :: parent_height = 300
    contains
        procedure :: set_spacing => forge_layout_set_spacing
        procedure :: set_padding => forge_layout_set_padding
        procedure :: get_spacing => forge_layout_get_spacing
        procedure :: get_padding => forge_layout_get_padding
        procedure :: invalidate => forge_layout_invalidate
        procedure :: set_parent_size => forge_layout_set_parent_size
        procedure :: get_parent_width => forge_layout_get_parent_width
        procedure :: get_parent_height => forge_layout_get_parent_height
        procedure(layout_add_widget_interface), deferred :: add_widget
        procedure(layout_remove_widget_interface), deferred :: remove_widget
        procedure(layout_compute_interface), deferred :: compute
    end type forge_layout_base

    !> @brief Grid layout (table-like arrangement)
    type, extends(forge_layout_base) :: forge_grid_layout
        private
        type(QList_widget) :: widgets
        integer, allocatable :: row_indices(:)
        integer, allocatable :: col_indices(:)
        integer, allocatable :: row_spans(:)
        integer, allocatable :: col_spans(:)
        integer :: rows = 1
        integer :: columns = 1
        logical :: homogeneous = .false.
    contains
        procedure :: set_dimensions => forge_grid_layout_set_dimensions
        procedure :: set_homogeneous => forge_grid_layout_set_homogeneous
        procedure :: add_widget => forge_grid_layout_add_widget
        procedure :: remove_widget => forge_grid_layout_remove_widget
        procedure :: compute => forge_grid_layout_compute
    end type forge_grid_layout

    !> @brief Box layout (horizontal or vertical)
    type, extends(forge_layout_base) :: forge_box_layout
        private
        type(QList_widget) :: widgets
        integer :: orientation = LAYOUT_HORIZONTAL
    contains
        procedure :: set_orientation => forge_box_layout_set_orientation
        procedure :: add_widget => forge_box_layout_add_widget
        procedure :: remove_widget => forge_box_layout_remove_widget
        procedure :: compute => forge_box_layout_compute
    end type forge_box_layout

    !> @brief Stack layout (widgets stacked on top of each other)
    type, extends(forge_layout_base) :: forge_stack_layout
        private
        type(QList_widget) :: widgets
        integer :: current_index = 1
    contains
        procedure :: set_current => forge_stack_layout_set_current
        procedure :: get_current => forge_stack_layout_get_current
        procedure :: add_widget => forge_stack_layout_add_widget
        procedure :: remove_widget => forge_stack_layout_remove_widget
        procedure :: compute => forge_stack_layout_compute
    end type forge_stack_layout

    !> @brief Form layout (two-column label-field arrangement)
    type, extends(forge_layout_base) :: forge_form_layout
        private
        type(QList_widget) :: labels
        type(QList_widget) :: fields
    contains
        procedure :: add_row => forge_form_layout_add_row
        procedure :: add_widget => forge_form_layout_add_widget
        procedure :: remove_widget => forge_form_layout_remove_widget
        procedure :: compute => forge_form_layout_compute
    end type forge_form_layout

    !> @brief Horizontal box layout (Qt QHBoxLayout equivalent)
    type, extends(forge_box_layout) :: QHBoxLayout
    contains
        procedure :: init => qhbox_layout_init
    end type QHBoxLayout

    !> @brief Vertical box layout (Qt QVBoxLayout equivalent)
    type, extends(forge_box_layout) :: QVBoxLayout
    contains
        procedure :: init => qvbox_layout_init
    end type QVBoxLayout

    !> @brief Grid layout (Qt QGridLayout equivalent)
    type, extends(forge_grid_layout) :: QGridLayout
    end type QGridLayout

    !> @brief Form layout (Qt QFormLayout equivalent)
    type, extends(forge_form_layout) :: QFormLayout
    end type QFormLayout

    !> @brief Stacked layout (Qt QStackedLayout equivalent)
    type, extends(forge_stack_layout) :: QStackedLayout
    end type QStackedLayout

    !> @brief Size policy constants (Qt-style)
    integer, parameter, public :: QSizePolicy_Fixed = 0
    integer, parameter, public :: QSizePolicy_Minimum = 1
    integer, parameter, public :: QSizePolicy_Maximum = 2
    integer, parameter, public :: QSizePolicy_Preferred = 3
    integer, parameter, public :: QSizePolicy_Expanding = 4
    integer, parameter, public :: QSizePolicy_MinimumExpanding = 5
    integer, parameter, public :: QSizePolicy_Ignored = 6

    !> @brief Alignment constants
    integer, parameter, public :: Qt_AlignLeft = 1
    integer, parameter, public :: Qt_AlignRight = 2
    integer, parameter, public :: Qt_AlignHCenter = 4
    integer, parameter, public :: Qt_AlignTop = 32
    integer, parameter, public :: Qt_AlignBottom = 64
    integer, parameter, public :: Qt_AlignVCenter = 128
    integer, parameter, public :: Qt_AlignCenter = Qt_AlignHCenter + Qt_AlignVCenter

    !> Abstract interface for layout operations
    abstract interface
        subroutine layout_add_widget_interface(this, widget, row, col, row_span, col_span)
            import :: forge_layout_base, forge_widget, c_int
            class(forge_layout_base), intent(inout) :: this
            class(forge_widget), intent(in) :: widget
            integer(c_int), intent(in), optional :: row, col, row_span, col_span
        end subroutine layout_add_widget_interface

        subroutine layout_remove_widget_interface(this, widget)
            import :: forge_layout_base, forge_widget
            class(forge_layout_base), intent(inout) :: this
            class(forge_widget), intent(in) :: widget
        end subroutine layout_remove_widget_interface

        subroutine layout_compute_interface(this)
            import :: forge_layout_base
            class(forge_layout_base), intent(inout) :: this
        end subroutine layout_compute_interface
    end interface

contains

    ! ========== Base Layout Methods ==========

    subroutine forge_layout_set_spacing(this, spacing)
        class(forge_layout_base), intent(inout) :: this
        integer, intent(in) :: spacing
        this%spacing = spacing
    end subroutine forge_layout_set_spacing

    subroutine forge_layout_set_padding(this, padding)
        class(forge_layout_base), intent(inout) :: this
        integer, intent(in) :: padding
        this%padding = padding
    end subroutine forge_layout_set_padding

    function forge_layout_get_spacing(this) result(spacing)
        class(forge_layout_base), intent(in) :: this
        integer :: spacing
        spacing = this%spacing
    end function forge_layout_get_spacing

    function forge_layout_get_padding(this) result(padding)
        class(forge_layout_base), intent(in) :: this
        integer :: padding
        padding = this%padding
    end function forge_layout_get_padding

    subroutine forge_layout_invalidate(this)
        class(forge_layout_base), intent(inout) :: this
        this%needs_recalc = .true.
    end subroutine forge_layout_invalidate

    subroutine forge_layout_set_parent_size(this, width, height)
        class(forge_layout_base), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        if (this%parent_width /= width .or. this%parent_height /= height) then
            this%parent_width = width
            this%parent_height = height
            call this%invalidate()
        end if
    end subroutine forge_layout_set_parent_size

    function forge_layout_get_parent_width(this) result(width)
        class(forge_layout_base), intent(in) :: this
        integer(c_int) :: width
        width = this%parent_width
    end function forge_layout_get_parent_width

    function forge_layout_get_parent_height(this) result(height)
        class(forge_layout_base), intent(in) :: this
        integer(c_int) :: height
        height = this%parent_height
    end function forge_layout_get_parent_height

    ! ========== Grid Layout Methods ==========

    subroutine forge_grid_layout_set_dimensions(this, rows, columns)
        class(forge_grid_layout), intent(inout) :: this
        integer, intent(in) :: rows, columns
        this%rows = rows
        this%columns = columns
    end subroutine forge_grid_layout_set_dimensions

    subroutine forge_grid_layout_set_homogeneous(this, homogeneous)
        class(forge_grid_layout), intent(inout) :: this
        logical, intent(in) :: homogeneous
        this%homogeneous = homogeneous
    end subroutine forge_grid_layout_set_homogeneous

    subroutine forge_grid_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_grid_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span
        integer :: r, c, rs, cs

        r = 0
        c = 0
        rs = 1
        cs = 1
        if (present(row)) r = row
        if (present(col)) c = col
        if (present(row_span)) rs = row_span
        if (present(col_span)) cs = col_span

        call this%widgets%append(widget)

        ! Store grid position data
        if (.not. allocated(this%row_indices)) then
            allocate(this%row_indices(10))
            allocate(this%col_indices(10))
            allocate(this%row_spans(10))
            allocate(this%col_spans(10))
        end if

        if (size(this%row_indices) < this%widgets%size()) then
            ! Resize arrays
            this%row_indices = [this%row_indices, [(0, integer :: i=1, size(this%row_indices))]]
            this%col_indices = [this%col_indices, [(0, integer :: i=1, size(this%col_indices))]]
            this%row_spans = [this%row_spans, [(1, integer :: i=1, size(this%row_spans))]]
            this%col_spans = [this%col_spans, [(1, integer :: i=1, size(this%col_spans))]]
        end if

        this%row_indices(this%widgets%size()) = r
        this%col_indices(this%widgets%size()) = c
        this%row_spans(this%widgets%size()) = rs
        this%col_spans(this%widgets%size()) = cs

        ! Invalidate layout when widgets are added
        call this%invalidate()
    end subroutine forge_grid_layout_add_widget

    subroutine forge_grid_layout_remove_widget(this, widget)
        class(forge_grid_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer :: index

        index = this%widgets%index_of(widget)
        if (index > 0) then
            call this%widgets%remove(index)
            ! Shift grid data
            if (index < this%widgets%size() + 1) then
                this%row_indices(index:this%widgets%size()) = this%row_indices(index+1:this%widgets%size()+1)
                this%col_indices(index:this%widgets%size()) = this%col_indices(index+1:this%widgets%size()+1)
                this%row_spans(index:this%widgets%size()) = this%row_spans(index+1:this%widgets%size()+1)
                this%col_spans(index:this%widgets%size()) = this%col_spans(index+1:this%widgets%size()+1)
            end if
            ! Invalidate layout when widgets are removed
            call this%invalidate()
        end if
    end subroutine forge_grid_layout_remove_widget

    subroutine forge_grid_layout_compute(this)
        class(forge_grid_layout), intent(inout) :: this
        integer :: i, num_widgets
        integer :: cell_width, cell_height, x_pos, y_pos
        integer :: widget_width, widget_height
        type(forge_size) :: hint, min_size, max_size
        type(forge_size_policy) :: policy
        class(forge_widget), pointer :: widget_ptr
        type(forge_grid_layout_solver) :: solver
        type(layout_solution), allocatable :: solutions(:)
        class(forge_widget), allocatable :: widget_array(:)

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Check if recalculation is needed
        if (.not. this%needs_recalc) return
        this%needs_recalc = .false.

        ! Create widget array for solver
        allocate(widget_array(num_widgets))
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (associated(widget_ptr)) then
                widget_array(i) = widget_ptr
            end if
        end do

        ! Setup solver
        call solver%set_available_size(this%get_parent_width(), this%get_parent_height())
        call solver%set_padding(this%get_padding())
        call solver%set_spacing(this%get_spacing())
        call solver%set_dimensions(this%rows, this%columns)

        ! Solve layout
        call solver%solve(widget_array, solutions)

        ! Apply solutions
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            ! Calculate position based on grid indices with spans
            x_pos = this%get_padding() + this%col_indices(i) * (cell_width + this%get_spacing())
            y_pos = this%get_padding() + this%row_indices(i) * (cell_height + this%get_spacing())

            ! Use solver results but adjust for spans
            widget_width = solutions(i)%width * this%col_spans(i) + (this%col_spans(i) - 1) * this%get_spacing()
            widget_height = solutions(i)%height * this%row_spans(i) + (this%row_spans(i) - 1) * this%get_spacing()

            ! Apply size policy constraints
            hint = widget_ptr%get_size_hint()
            min_size = widget_ptr%get_minimum_size()
            max_size = widget_ptr%get_maximum_size()
            policy = widget_ptr%get_size_policy()

            widget_width = policy%calculate_horizontal_size(hint%width, min_size%width, max_size%width, widget_width)
            widget_height = policy%calculate_vertical_size(hint%height, min_size%height, max_size%height, widget_height)

            call widget_ptr%set_position(x_pos, y_pos)
            call widget_ptr%set_size(widget_width, widget_height)
        end do

        deallocate(widget_array, solutions)
    end subroutine forge_grid_layout_compute

    ! ========== Box Layout Methods ==========

    subroutine forge_box_layout_set_orientation(this, orientation)
        class(forge_box_layout), intent(inout) :: this
        integer, intent(in) :: orientation
        this%orientation = orientation
    end subroutine forge_box_layout_set_orientation

    subroutine forge_box_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_box_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span

        call this%widgets%append(widget)
        ! Invalidate layout when widgets are added
        call this%invalidate()
    end subroutine forge_box_layout_add_widget

    subroutine forge_box_layout_remove_widget(this, widget)
        class(forge_box_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer :: index

        index = this%widgets%index_of(widget)
        if (index > 0) then
            call this%widgets%remove(index)
            ! Invalidate layout when widgets are removed
            call this%invalidate()
        end if
    end subroutine forge_box_layout_remove_widget

    subroutine forge_box_layout_compute(this)
        class(forge_box_layout), intent(inout) :: this
        integer :: i, num_widgets
        type(forge_box_layout_solver) :: solver
        type(layout_solution), allocatable :: solutions(:)
        class(forge_widget), allocatable :: widget_array(:)
        class(forge_widget), pointer :: widget_ptr

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Check if recalculation is needed
        if (.not. this%needs_recalc) return
        this%needs_recalc = .false.

        ! Create widget array for solver
        allocate(widget_array(num_widgets))
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (associated(widget_ptr)) then
                widget_array(i) = widget_ptr
            end if
        end do

        ! Setup solver
        call solver%set_available_size(this%get_parent_width(), this%get_parent_height())
        call solver%set_padding(this%get_padding())
        call solver%set_spacing(this%get_spacing())
        call solver%set_orientation(this%orientation)

        ! Solve layout
        call solver%solve(widget_array, solutions)

        ! Apply solutions
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            call widget_ptr%set_position(solutions(i)%x, solutions(i)%y)
            call widget_ptr%set_size(solutions(i)%width, solutions(i)%height)
        end do

        deallocate(widget_array, solutions)
    end subroutine forge_box_layout_compute

    ! ========== Stack Layout Methods ==========

    subroutine forge_stack_layout_set_current(this, index)
        class(forge_stack_layout), intent(inout) :: this
        integer, intent(in) :: index
        this%current_index = max(1, min(index, this%widgets%size()))
        ! Invalidate layout when current widget changes
        call this%invalidate()
    end subroutine forge_stack_layout_set_current

    function forge_stack_layout_get_current(this) result(index)
        class(forge_stack_layout), intent(in) :: this
        integer :: index
        index = this%current_index
    end function forge_stack_layout_get_current

    ! ========== Form Layout Methods ==========

    subroutine forge_form_layout_add_row(this, label, field)
        class(forge_form_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: label, field

        call this%labels%append(label)
        call this%fields%append(field)
        ! Invalidate layout when widgets are added
        call this%invalidate()
    end subroutine forge_form_layout_add_row

    subroutine forge_form_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_form_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span

        ! For form layout, add as field if no specific column specified
        if (.not. present(col) .or. col == 1) then
            call this%labels%append(widget)
        else
            call this%fields%append(widget)
        end if
    end subroutine forge_form_layout_add_widget

    subroutine forge_form_layout_remove_widget(this, widget)
        class(forge_form_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer :: index

        index = this%labels%index_of(widget)
        if (index > 0) then
            call this%labels%remove(index)
            call this%fields%remove(index)  ! Remove corresponding field
            ! Invalidate layout when widgets are removed
            call this%invalidate()
            return
        end if

        index = this%fields%index_of(widget)
        if (index > 0) then
            call this%fields%remove(index)
            call this%labels%remove(index)  ! Remove corresponding label
            ! Invalidate layout when widgets are removed
            call this%invalidate()
        end if
    end subroutine forge_form_layout_remove_widget

    subroutine forge_form_layout_compute(this)
        class(forge_form_layout), intent(inout) :: this
        integer :: i, num_rows, num_widgets
        type(forge_form_layout_solver) :: solver
        type(layout_solution), allocatable :: solutions(:)
        class(forge_widget), allocatable :: widget_array(:)
        class(forge_widget), pointer :: widget_ptr

        num_rows = min(this%labels%size(), this%fields%size())
        if (num_rows == 0) return

        ! Check if recalculation is needed
        if (.not. this%needs_recalc) return
        this%needs_recalc = .false.

        ! Create widget array for solver (labels and fields interleaved)
        num_widgets = num_rows * 2
        allocate(widget_array(num_widgets))

        do i = 1, num_rows
            widget_ptr => this%labels%at(i)
            if (associated(widget_ptr)) then
                widget_array(2*i-1) = widget_ptr
            end if
            widget_ptr => this%fields%at(i)
            if (associated(widget_ptr)) then
                widget_array(2*i) = widget_ptr
            end if
        end do

        ! Setup solver
        call solver%set_available_size(this%get_parent_width(), this%get_parent_height())
        call solver%set_padding(this%get_padding())
        call solver%set_spacing(this%get_spacing())

        ! Solve layout
        call solver%solve(widget_array, solutions)

        ! Apply solutions
        do i = 1, num_rows
            widget_ptr => this%labels%at(i)
            if (associated(widget_ptr)) then
                call widget_ptr%set_position(solutions(2*i-1)%x, solutions(2*i-1)%y)
                call widget_ptr%set_size(solutions(2*i-1)%width, solutions(2*i-1)%height)
            end if

            widget_ptr => this%fields%at(i)
            if (associated(widget_ptr)) then
                call widget_ptr%set_position(solutions(2*i)%x, solutions(2*i)%y)
                call widget_ptr%set_size(solutions(2*i)%width, solutions(2*i)%height)
            end if
        end do

        deallocate(widget_array, solutions)
    end subroutine forge_form_layout_compute

    subroutine forge_stack_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_stack_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span

        call this%widgets%append(widget)
        ! Invalidate layout when widgets are added
        call this%invalidate()
    end subroutine forge_stack_layout_add_widget

    subroutine forge_stack_layout_remove_widget(this, widget)
        class(forge_stack_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer :: index

        index = this%widgets%index_of(widget)
        if (index > 0) then
            call this%widgets%remove(index)
            if (this%current_index > this%widgets%size()) then
                this%current_index = this%widgets%size()
            end if
            ! Invalidate layout when widgets are removed
            call this%invalidate()
        end if
    end subroutine forge_stack_layout_remove_widget

    subroutine forge_stack_layout_compute(this)
        class(forge_stack_layout), intent(inout) :: this
        integer :: i, num_widgets
        type(forge_stack_layout_solver) :: solver
        type(layout_solution), allocatable :: solutions(:)
        class(forge_widget), allocatable :: widget_array(:)
        class(forge_widget), pointer :: widget_ptr

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Check if recalculation is needed
        if (.not. this%needs_recalc) return
        this%needs_recalc = .false.

        ! Create widget array for solver
        allocate(widget_array(num_widgets))
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (associated(widget_ptr)) then
                widget_array(i) = widget_ptr
            end if
        end do

        ! Setup solver
        call solver%set_available_size(this%get_parent_width(), this%get_parent_height())
        call solver%set_padding(this%get_padding())
        call solver%set_spacing(this%get_spacing())
        call solver%set_current_index(this%current_index)

        ! Solve layout
        call solver%solve(widget_array, solutions)

        ! Apply solutions
        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            if (solutions(i)%width > 0 .and. solutions(i)%height > 0) then
                ! Show current widget
                call widget_ptr%show()
                call widget_ptr%set_position(solutions(i)%x, solutions(i)%y)
                call widget_ptr%set_size(solutions(i)%width, solutions(i)%height)
            else
                ! Hide other widgets
                call widget_ptr%hide()
            end if
        end do

        deallocate(widget_array, solutions)
    end subroutine forge_stack_layout_compute

    ! ========== Qt-style Layout Constructors ==========

    subroutine qhbox_layout_init(this)
        class(QHBoxLayout), intent(inout) :: this
        call this%set_orientation(LAYOUT_HORIZONTAL)
    end subroutine qhbox_layout_init

    subroutine qvbox_layout_init(this)
        class(QVBoxLayout), intent(inout) :: this
        call this%set_orientation(LAYOUT_VERTICAL)
    end subroutine qvbox_layout_init

end module forge_layout

