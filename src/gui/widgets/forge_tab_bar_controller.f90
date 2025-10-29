!> @brief Tab bar controller for mobile UI patterns
!> @details Tab-based navigation with multiple view controllers
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_tab_bar_controller
    use iso_c_binding
    use forge_types
    use forge_widgets
    implicit none
    private

    public :: forge_tab_bar_controller, forge_tab_bar_item
    public :: tab_bar_controller_create, tab_bar_controller_add_tab
    public :: tab_bar_controller_select_tab, tab_bar_controller_get_selected_view

    !> @brief Maximum number of tabs
    integer, parameter :: MAX_TABS = 5

    !> @brief Tab bar item
    type :: forge_tab_bar_item
        character(len=64) :: title = ""
        character(len=256) :: image_name = ""
        integer :: badge_count = 0
        logical :: enabled = .true.
    end type forge_tab_bar_item

    !> @brief Tab bar controller
    type :: forge_tab_bar_controller
        private
        type(c_ptr) :: view_controllers(MAX_TABS) = c_null_ptr
        type(forge_tab_bar_item) :: tab_items(MAX_TABS)
        integer :: tab_count = 0
        integer :: selected_index = 0
        type(c_ptr) :: tab_bar = c_null_ptr
    contains
        procedure :: add_tab => tab_controller_add_tab
        procedure :: select_tab => tab_controller_select_tab
        procedure :: get_selected_view_controller => tab_controller_get_selected
        procedure :: get_tab_count => tab_controller_get_count
        procedure :: set_badge_count => tab_controller_set_badge
    end type forge_tab_bar_controller

contains

    !> @brief Create tab bar controller
    function tab_bar_controller_create() result(controller)
        type(forge_tab_bar_controller) :: controller

        controller%tab_count = 0
        controller%selected_index = 0
        controller%tab_bar = c_null_ptr  ! Would create actual tab bar widget
    end function tab_bar_controller_create

    !> @brief Add tab to controller
    subroutine tab_bar_controller_add_tab(controller, view_controller, tab_item)
        type(forge_tab_bar_controller), intent(inout) :: controller
        type(c_ptr), intent(in) :: view_controller
        type(forge_tab_bar_item), intent(in) :: tab_item

        if (controller%tab_count < MAX_TABS) then
            controller%tab_count = controller%tab_count + 1
            controller%view_controllers(controller%tab_count) = view_controller
            controller%tab_items(controller%tab_count) = tab_item

            if (controller%selected_index == 0) then
                controller%selected_index = 1  ! Select first tab by default
            end if
        end if
    end subroutine tab_bar_controller_add_tab

    !> @brief Select tab by index
    subroutine tab_bar_controller_select_tab(controller, index)
        type(forge_tab_bar_controller), intent(inout) :: controller
        integer, intent(in) :: index

        if (index >= 1 .and. index <= controller%tab_count) then
            controller%selected_index = index
            ! Trigger tab change animation/transition
        end if
    end subroutine tab_bar_controller_select_tab

    !> @brief Get selected view controller
    function tab_bar_controller_get_selected_view(controller) result(view_controller)
        type(forge_tab_bar_controller), intent(in) :: controller
        type(c_ptr) :: view_controller

        if (controller%selected_index > 0 .and. controller%selected_index <= controller%tab_count) then
            view_controller = controller%view_controllers(controller%selected_index)
        else
            view_controller = c_null_ptr
        end if
    end function tab_bar_controller_get_selected_view

    !> @brief Add tab (method version)
    subroutine tab_controller_add_tab(this, view_controller, tab_item)
        class(forge_tab_bar_controller), intent(inout) :: this
        type(c_ptr), intent(in) :: view_controller
        type(forge_tab_bar_item), intent(in) :: tab_item

        call tab_bar_controller_add_tab(this, view_controller, tab_item)
    end subroutine tab_controller_add_tab

    !> @brief Select tab (method version)
    subroutine tab_controller_select_tab(this, index)
        class(forge_tab_bar_controller), intent(inout) :: this
        integer, intent(in) :: index

        call tab_bar_controller_select_tab(this, index)
    end subroutine tab_controller_select_tab

    !> @brief Get selected view controller (method version)
    function tab_controller_get_selected(this) result(view_controller)
        class(forge_tab_bar_controller), intent(in) :: this
        type(c_ptr) :: view_controller

        view_controller = tab_bar_controller_get_selected_view(this)
    end function tab_controller_get_selected

    !> @brief Get tab count
    function tab_controller_get_count(this) result(count)
        class(forge_tab_bar_controller), intent(in) :: this
        integer :: count

        count = this%tab_count
    end function tab_controller_get_count

    !> @brief Set badge count for tab
    subroutine tab_controller_set_badge(this, tab_index, badge_count)
        class(forge_tab_bar_controller), intent(inout) :: this
        integer, intent(in) :: tab_index, badge_count

        if (tab_index >= 1 .and. tab_index <= this%tab_count) then
            this%tab_items(tab_index)%badge_count = badge_count
        end if
    end subroutine tab_controller_set_badge

end module forge_tab_bar_controller