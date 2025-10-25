!> @brief ScrollArea widget
!> @details Scrollable container for large content
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_scrollarea
    use forge_widgets
    use forge_types
    implicit none
    private

    public :: QScrollArea, QScrollBar, ScrollBarPolicy
    public :: ScrollBarAlwaysOff, ScrollBarAlwaysOn, ScrollBarAsNeeded

    !> ScrollBar policy constants
    integer, parameter :: ScrollBarAlwaysOff = 0
    integer, parameter :: ScrollBarAlwaysOn = 1
    integer, parameter :: ScrollBarAsNeeded = 2

    type :: ScrollBarPolicy
        integer :: value = ScrollBarAsNeeded
    end type ScrollBarPolicy

    !> @brief ScrollBar widget
    type, extends(forge_widget) :: QScrollBar
        private
        integer :: value = 0
        integer :: minimum = 0
        integer :: maximum = 99
        integer :: page_step = 10
        integer :: single_step = 1
        integer :: orientation = 1  ! 1=horizontal, 2=vertical
        ! Would need signal_int for value_changed
    contains
        procedure :: set_value => scrollbar_set_value
        procedure :: get_value => scrollbar_get_value
        procedure :: set_range => scrollbar_set_range
        procedure :: set_orientation => scrollbar_set_orientation
    end type QScrollBar

    !> @brief Scroll area widget
    type, extends(forge_widget) :: QScrollArea
        private
        class(forge_widget), allocatable :: widget_content
        type(ScrollBarPolicy) :: horizontal_policy
        type(ScrollBarPolicy) :: vertical_policy
        type(QScrollBar) :: horizontal_scrollbar
        type(QScrollBar) :: vertical_scrollbar
        logical :: widget_resizable = .false.
    contains
        procedure :: set_widget => scrollarea_set_widget
        procedure :: get_widget => scrollarea_get_widget
        procedure :: set_horizontal_policy => scrollarea_set_h_policy
        procedure :: set_vertical_policy => scrollarea_set_v_policy
        procedure :: set_widget_resizable => scrollarea_set_resizable
    end type QScrollArea

contains

    ! ========== QScrollBar Implementation ==========

    subroutine scrollbar_set_value(this, value)
        class(QScrollBar), intent(inout) :: this
        integer, intent(in) :: value
        this%value = max(this%minimum, min(this%maximum, value))
    end subroutine scrollbar_set_value

    function scrollbar_get_value(this) result(value)
        class(QScrollBar), intent(in) :: this
        integer :: value
        value = this%value
    end function scrollbar_get_value

    subroutine scrollbar_set_range(this, minimum, maximum)
        class(QScrollBar), intent(inout) :: this
        integer, intent(in) :: minimum, maximum
        this%minimum = minimum
        this%maximum = maximum
        call this%set_value(this%value)  ! Re-clamp
    end subroutine scrollbar_set_range

    subroutine scrollbar_set_orientation(this, orientation)
        class(QScrollBar), intent(inout) :: this
        integer, intent(in) :: orientation
        this%orientation = orientation
    end subroutine scrollbar_set_orientation

    ! ========== QScrollArea Implementation ==========

    subroutine scrollarea_set_widget(this, widget)
        class(QScrollArea), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        
        if (allocated(this%widget_content)) deallocate(this%widget_content)
        allocate(this%widget_content, source=widget)
    end subroutine scrollarea_set_widget

    function scrollarea_get_widget(this) result(widget)
        class(QScrollArea), intent(in) :: this
        class(forge_widget), allocatable :: widget
        
        if (allocated(this%widget_content)) then
            allocate(widget, source=this%widget_content)
        end if
    end function scrollarea_get_widget

    subroutine scrollarea_set_h_policy(this, policy)
        class(QScrollArea), intent(inout) :: this
        integer, intent(in) :: policy
        this%horizontal_policy%value = policy
    end subroutine scrollarea_set_h_policy

    subroutine scrollarea_set_v_policy(this, policy)
        class(QScrollArea), intent(inout) :: this
        integer, intent(in) :: policy
        this%vertical_policy%value = policy
    end subroutine scrollarea_set_v_policy

    subroutine scrollarea_set_resizable(this, resizable)
        class(QScrollArea), intent(inout) :: this
        logical, intent(in) :: resizable
        this%widget_resizable = resizable
    end subroutine scrollarea_set_resizable

end module forge_scrollarea

