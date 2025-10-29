!> @brief Navigation controller for mobile UI patterns
!> @details Stack-based navigation with push/pop transitions
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_navigation_controller
    use iso_c_binding
    use forge_types
    use forge_widgets
    use forge_layout
    implicit none
    private

    public :: forge_navigation_controller
    public :: navigation_controller_create, navigation_controller_push
    public :: navigation_controller_pop, navigation_controller_get_top_view

    !> @brief Navigation transition types
    integer, parameter, public :: NAV_TRANSITION_PUSH = 1
    integer, parameter, public :: NAV_TRANSITION_POP = 2
    integer, parameter, public :: NAV_TRANSITION_MODAL = 3

    !> @brief Maximum navigation stack depth
    integer, parameter :: MAX_NAV_STACK = 10

    !> @brief Navigation controller
    type :: forge_navigation_controller
        private
        type(c_ptr) :: view_controllers(MAX_NAV_STACK) = c_null_ptr
        integer :: stack_depth = 0
        type(c_ptr) :: navigation_bar = c_null_ptr
        logical :: shows_navigation_bar = .true.
    contains
        procedure :: push_view_controller => nav_controller_push
        procedure :: pop_view_controller => nav_controller_pop
        procedure :: pop_to_root => nav_controller_pop_to_root
        procedure :: get_top_view_controller => nav_controller_get_top
        procedure :: get_view_controller_count => nav_controller_get_count
        procedure :: set_navigation_bar_hidden => nav_controller_set_bar_hidden
    end type forge_navigation_controller

contains

    !> @brief Create navigation controller with root view controller
    function navigation_controller_create(root_view_controller) result(controller)
        type(c_ptr), intent(in) :: root_view_controller
        type(forge_navigation_controller) :: controller

        controller%stack_depth = 1
        controller%view_controllers(1) = root_view_controller
        controller%shows_navigation_bar = .true.
        ! Create navigation bar (would integrate with actual widget system)
        controller%navigation_bar = c_null_ptr
    end function navigation_controller_create

    !> @brief Push view controller onto navigation stack
    subroutine navigation_controller_push(controller, view_controller)
        type(forge_navigation_controller), intent(inout) :: controller
        type(c_ptr), intent(in) :: view_controller

        if (controller%stack_depth < MAX_NAV_STACK) then
            controller%stack_depth = controller%stack_depth + 1
            controller%view_controllers(controller%stack_depth) = view_controller
            ! Trigger push animation/transition
        end if
    end subroutine navigation_controller_push

    !> @brief Pop view controller from navigation stack
    function navigation_controller_pop(controller) result(popped_controller)
        type(forge_navigation_controller), intent(inout) :: controller
        type(c_ptr) :: popped_controller

        popped_controller = c_null_ptr
        if (controller%stack_depth > 1) then
            popped_controller = controller%view_controllers(controller%stack_depth)
            controller%view_controllers(controller%stack_depth) = c_null_ptr
            controller%stack_depth = controller%stack_depth - 1
            ! Trigger pop animation/transition
        end if
    end function navigation_controller_pop

    !> @brief Get top view controller
    function navigation_controller_get_top_view(controller) result(view_controller)
        type(forge_navigation_controller), intent(in) :: controller
        type(c_ptr) :: view_controller

        if (controller%stack_depth > 0) then
            view_controller = controller%view_controllers(controller%stack_depth)
        else
            view_controller = c_null_ptr
        end if
    end function navigation_controller_get_top_view

    !> @brief Push view controller (method version)
    subroutine nav_controller_push(this, view_controller)
        class(forge_navigation_controller), intent(inout) :: this
        type(c_ptr), intent(in) :: view_controller

        call navigation_controller_push(this, view_controller)
    end subroutine nav_controller_push

    !> @brief Pop view controller (method version)
    function nav_controller_pop(this) result(popped_controller)
        class(forge_navigation_controller), intent(inout) :: this
        type(c_ptr) :: popped_controller

        popped_controller = navigation_controller_pop(this)
    end function nav_controller_pop

    !> @brief Pop to root view controller
    subroutine nav_controller_pop_to_root(this)
        class(forge_navigation_controller), intent(inout) :: this

        do while (this%stack_depth > 1)
            this%view_controllers(this%stack_depth) = c_null_ptr
            this%stack_depth = this%stack_depth - 1
        end do
    end subroutine nav_controller_pop_to_root

    !> @brief Get top view controller (method version)
    function nav_controller_get_top(this) result(view_controller)
        class(forge_navigation_controller), intent(in) :: this
        type(c_ptr) :: view_controller

        view_controller = navigation_controller_get_top_view(this)
    end function nav_controller_get_top

    !> @brief Get view controller count
    function nav_controller_get_count(this) result(count)
        class(forge_navigation_controller), intent(in) :: this
        integer :: count

        count = this%stack_depth
    end function nav_controller_get_count

    !> @brief Set navigation bar visibility
    subroutine nav_controller_set_bar_hidden(this, hidden)
        class(forge_navigation_controller), intent(inout) :: this
        logical, intent(in) :: hidden

        this%shows_navigation_bar = .not. hidden
    end subroutine nav_controller_set_bar_hidden

end module forge_navigation_controller