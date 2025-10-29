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
    contains
        procedure :: set_spacing => forge_layout_set_spacing
        procedure :: set_padding => forge_layout_set_padding
        procedure :: get_spacing => forge_layout_get_spacing
        procedure :: get_padding => forge_layout_get_padding
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
        end if
    end subroutine forge_grid_layout_remove_widget

    subroutine forge_grid_layout_compute(this)
        class(forge_grid_layout), intent(inout) :: this
        integer :: i, num_widgets, parent_width, parent_height
        integer :: cell_width, cell_height, x_pos, y_pos
        integer :: widget_width, widget_height
        type(forge_size) :: hint
        class(forge_widget), pointer :: widget_ptr

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Get parent dimensions from the first widget's parent (assuming all widgets have the same parent)
        if (num_widgets > 0) then
            widget_ptr => this%widgets%at(1)
            if (associated(widget_ptr)) then
                ! In a real implementation, widgets would have a parent reference
                ! For now, use default values, but this would be retrieved from the parent widget
                parent_width = 400
                parent_height = 300
            else
                parent_width = 400
                parent_height = 300
            end if
        else
            parent_width = 400
            parent_height = 300
        end if

        cell_width = (parent_width - 2 * this%get_padding() - (this%columns - 1) * this%get_spacing()) / this%columns
        cell_height = (parent_height - 2 * this%get_padding() - (this%rows - 1) * this%get_spacing()) / this%rows

        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            hint = widget_ptr%get_size_hint()

            ! Calculate position based on grid indices
            x_pos = this%get_padding() + this%col_indices(i) * (cell_width + this%get_spacing())
            y_pos = this%get_padding() + this%row_indices(i) * (cell_height + this%get_spacing())

            ! Calculate size based on spans
            widget_width = this%col_spans(i) * cell_width + (this%col_spans(i) - 1) * this%get_spacing()
            widget_height = this%row_spans(i) * cell_height + (this%row_spans(i) - 1) * this%get_spacing()

            ! Apply size policy constraints
            if (widget_ptr%get_size_policy()%horizontal_policy == 0) then ! Fixed
                widget_width = min(widget_width, hint%width)
            end if
            if (widget_ptr%get_size_policy()%vertical_policy == 0) then ! Fixed
                widget_height = min(widget_height, hint%height)
            end if

            call widget_ptr%set_position(x_pos, y_pos)
            call widget_ptr%set_size(widget_width, widget_height)
        end do
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
    end subroutine forge_box_layout_add_widget

    subroutine forge_box_layout_remove_widget(this, widget)
        class(forge_box_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer :: index

        index = this%widgets%index_of(widget)
        if (index > 0) then
            call this%widgets%remove(index)
        end if
    end subroutine forge_box_layout_remove_widget

    subroutine forge_box_layout_compute(this)
        class(forge_box_layout), intent(inout) :: this
        integer :: i, num_widgets, total_stretch, available_space
        integer :: x_pos, y_pos, widget_width, widget_height
        integer :: fixed_width, fixed_height, stretch_space
        integer :: parent_width, parent_height
        type(forge_size) :: hint
        class(forge_widget), pointer :: widget_ptr

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Get parent dimensions from the first widget's parent (assuming all widgets have the same parent)
        if (num_widgets > 0) then
            widget_ptr => this%widgets%at(1)
            if (associated(widget_ptr)) then
                ! In a real implementation, widgets would have a parent reference
                ! For now, use default values, but this would be retrieved from the parent widget
                parent_width = 400
                parent_height = 300
            else
                parent_width = 400
                parent_height = 300
            end if
        else
            parent_width = 400
            parent_height = 300
        end if

        ! Calculate total stretch factors
        total_stretch = 0
        fixed_width = 0
        fixed_height = 0

        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            hint = widget_ptr%get_size_hint()

            if (this%orientation == LAYOUT_HORIZONTAL) then
                select case (widget_ptr%get_size_policy()%horizontal_policy)
                case (0) ! Fixed
                    fixed_width = fixed_width + hint%width
                case (4) ! Expanding
                    total_stretch = total_stretch + max(1, widget_ptr%get_size_policy()%horizontal_stretch)
                case default
                    fixed_width = fixed_width + hint%width
                end select
            else ! Vertical
                select case (widget_ptr%get_size_policy()%vertical_policy)
                case (0) ! Fixed
                    fixed_height = fixed_height + hint%height
                case (4) ! Expanding
                    total_stretch = total_stretch + max(1, widget_ptr%get_size_policy()%vertical_stretch)
                case default
                    fixed_height = fixed_height + hint%height
                end select
            end if
        end do

        ! Calculate available space after spacing and padding
        if (this%orientation == LAYOUT_HORIZONTAL) then
            available_space = parent_width - 2 * this%get_padding() - (num_widgets - 1) * this%get_spacing() - fixed_width
            stretch_space = available_space / max(1, total_stretch)
        else
            available_space = parent_height - 2 * this%get_padding() - (num_widgets - 1) * this%get_spacing() - fixed_height
            stretch_space = available_space / max(1, total_stretch)
        end if

        ! Position widgets
        x_pos = this%get_padding()
        y_pos = this%get_padding()

        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            hint = widget_ptr%get_size_hint()

            if (this%orientation == LAYOUT_HORIZONTAL) then
                widget_height = hint%height
                select case (widget_ptr%get_size_policy()%horizontal_policy)
                case (0) ! Fixed
                    widget_width = hint%width
                case (4) ! Expanding
                    widget_width = stretch_space * max(1, widget_ptr%get_size_policy()%horizontal_stretch)
                case default
                    widget_width = hint%width
                end select

                call widget_ptr%set_position(x_pos, y_pos)
                call widget_ptr%set_size(widget_width, widget_height)
                x_pos = x_pos + widget_width + this%get_spacing()
            else ! Vertical
                widget_width = hint%width
                select case (widget_ptr%get_size_policy()%vertical_policy)
                case (0) ! Fixed
                    widget_height = hint%height
                case (4) ! Expanding
                    widget_height = stretch_space * max(1, widget_ptr%get_size_policy()%vertical_stretch)
                case default
                    widget_height = hint%height
                end select

                call widget_ptr%set_position(x_pos, y_pos)
                call widget_ptr%set_size(widget_width, widget_height)
                y_pos = y_pos + widget_height + this%get_spacing()
            end if
        end do
    end subroutine forge_box_layout_compute

    ! ========== Stack Layout Methods ==========

    subroutine forge_stack_layout_set_current(this, index)
        class(forge_stack_layout), intent(inout) :: this
        integer, intent(in) :: index
        this%current_index = max(1, min(index, this%widgets%size()))
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
            return
        end if

        index = this%fields%index_of(widget)
        if (index > 0) then
            call this%fields%remove(index)
            call this%labels%remove(index)  ! Remove corresponding label
        end if
    end subroutine forge_form_layout_remove_widget

    subroutine forge_form_layout_compute(this)
        class(forge_form_layout), intent(inout) :: this
        integer :: i, num_rows, parent_width, parent_height
        integer :: label_width, field_width, row_height, y_pos
        integer :: max_label_width
        type(forge_size) :: label_hint, field_hint
        class(forge_widget), pointer :: label_ptr, field_ptr

        num_rows = min(this%labels%size(), this%fields%size())
        if (num_rows == 0) return

        ! Get parent dimensions from the first label's parent (assuming all widgets have the same parent)
        if (num_rows > 0) then
            label_ptr => this%labels%at(1)
            if (associated(label_ptr)) then
                ! In a real implementation, widgets would have a parent reference
                ! For now, use default values, but this would be retrieved from the parent widget
                parent_width = 400
                parent_height = 300
            else
                parent_width = 400
                parent_height = 300
            end if
        else
            parent_width = 400
            parent_height = 300
        end if

        ! Calculate maximum label width
        max_label_width = 0
        do i = 1, num_rows
            label_ptr => this%labels%at(i)
            if (associated(label_ptr)) then
                label_hint = label_ptr%get_size_hint()
                max_label_width = max(max_label_width, label_hint%width)
            end if
        end do

        ! Set field width to remaining space
        field_width = parent_width - 2 * this%get_padding() - max_label_width - this%get_spacing()

        y_pos = this%get_padding()

        do i = 1, num_rows
            label_ptr => this%labels%at(i)
            field_ptr => this%fields%at(i)

            if (associated(label_ptr)) then
                label_hint = label_ptr%get_size_hint()
                call label_ptr%set_position(this%get_padding(), y_pos)
                call label_ptr%set_size(max_label_width, label_hint%height)
            end if

            if (associated(field_ptr)) then
                field_hint = field_ptr%get_size_hint()
                call field_ptr%set_position(this%get_padding() + max_label_width + this%get_spacing(), y_pos)
                call field_ptr%set_size(field_width, field_hint%height)
            end if

            row_height = 0
            if (associated(label_ptr)) row_height = max(row_height, label_ptr%get_size_hint()%height)
            if (associated(field_ptr)) row_height = max(row_height, field_ptr%get_size_hint()%height)

            y_pos = y_pos + row_height + this%get_spacing()
        end do
    end subroutine forge_form_layout_compute

    subroutine forge_stack_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_stack_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span

        call this%widgets%append(widget)
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
        end if
    end subroutine forge_stack_layout_remove_widget

    subroutine forge_stack_layout_compute(this)
        class(forge_stack_layout), intent(inout) :: this
        integer :: i, num_widgets, parent_width, parent_height
        class(forge_widget), pointer :: widget_ptr

        num_widgets = this%widgets%size()
        if (num_widgets == 0) return

        ! Get parent dimensions from the first widget's parent (assuming all widgets have the same parent)
        if (num_widgets > 0) then
            widget_ptr => this%widgets%at(1)
            if (associated(widget_ptr)) then
                ! In a real implementation, widgets would have a parent reference
                ! For now, use default values, but this would be retrieved from the parent widget
                parent_width = 400
                parent_height = 300
            else
                parent_width = 400
                parent_height = 300
            end if
        else
            parent_width = 400
            parent_height = 300
        end if

        do i = 1, num_widgets
            widget_ptr => this%widgets%at(i)
            if (.not. associated(widget_ptr)) cycle

            if (i == this%current_index) then
                ! Show current widget
                call widget_ptr%show()
                call widget_ptr%set_position(this%get_padding(), this%get_padding())
                call widget_ptr%set_size(parent_width - 2 * this%get_padding(), parent_height - 2 * this%get_padding())
            else
                ! Hide other widgets
                call widget_ptr%hide()
            end if
        end do
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

