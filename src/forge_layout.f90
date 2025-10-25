!> @brief Layout managers for ForGE
!> @details Provides layout management systems for organizing widgets
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_layout
    use iso_c_binding
    use forge_types
    use forge_widgets
    implicit none
    private

    public :: forge_layout_base, forge_grid_layout, forge_box_layout, forge_stack_layout
    public :: LAYOUT_HORIZONTAL, LAYOUT_VERTICAL

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
        integer :: current_index = 1
    contains
        procedure :: set_current => forge_stack_layout_set_current
        procedure :: get_current => forge_stack_layout_get_current
        procedure :: add_widget => forge_stack_layout_add_widget
        procedure :: remove_widget => forge_stack_layout_remove_widget
        procedure :: compute => forge_stack_layout_compute
    end type forge_stack_layout

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
        
        ! TODO: Implement widget placement in grid
        ! This will interact with the backend to position widgets
    end subroutine forge_grid_layout_add_widget

    subroutine forge_grid_layout_remove_widget(this, widget)
        class(forge_grid_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        
        ! TODO: Implement widget removal from grid
    end subroutine forge_grid_layout_remove_widget

    subroutine forge_grid_layout_compute(this)
        class(forge_grid_layout), intent(inout) :: this
        
        ! TODO: Compute layout positions and sizes
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
        
        ! TODO: Implement widget addition to box
    end subroutine forge_box_layout_add_widget

    subroutine forge_box_layout_remove_widget(this, widget)
        class(forge_box_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        
        ! TODO: Implement widget removal from box
    end subroutine forge_box_layout_remove_widget

    subroutine forge_box_layout_compute(this)
        class(forge_box_layout), intent(inout) :: this
        
        ! TODO: Compute layout for box
    end subroutine forge_box_layout_compute

    ! ========== Stack Layout Methods ==========

    subroutine forge_stack_layout_set_current(this, index)
        class(forge_stack_layout), intent(inout) :: this
        integer, intent(in) :: index
        this%current_index = index
    end subroutine forge_stack_layout_set_current

    function forge_stack_layout_get_current(this) result(index)
        class(forge_stack_layout), intent(in) :: this
        integer :: index
        index = this%current_index
    end function forge_stack_layout_get_current

    subroutine forge_stack_layout_add_widget(this, widget, row, col, row_span, col_span)
        class(forge_stack_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        integer(c_int), intent(in), optional :: row, col, row_span, col_span
        
        ! TODO: Implement widget addition to stack
    end subroutine forge_stack_layout_add_widget

    subroutine forge_stack_layout_remove_widget(this, widget)
        class(forge_stack_layout), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        
        ! TODO: Implement widget removal from stack
    end subroutine forge_stack_layout_remove_widget

    subroutine forge_stack_layout_compute(this)
        class(forge_stack_layout), intent(inout) :: this
        
        ! TODO: Compute stack layout
    end subroutine forge_stack_layout_compute

end module forge_layout

