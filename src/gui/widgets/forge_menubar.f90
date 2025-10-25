!> @brief MenuBar and Menu widgets
!> @details Application menu system
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_menubar
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    use forge_string_utils
    implicit none
    private

    public :: QMenuBar, QMenu, QAction

    !> @brief Menu action (menu item)
    type :: QAction
        private
        type(QString) :: text
        type(QString) :: icon_path
        type(QString) :: shortcut
        type(QString) :: tooltip
        type(QString) :: status_tip
        logical :: checkable = .false.
        logical :: checked = .false.
        logical :: enabled = .true.
        logical :: visible = .true.
        logical :: separator = .false.
        type(signal_void) :: triggered
        type(signal_bool) :: toggled
    contains
        procedure :: set_text => action_set_text
        procedure :: get_text => action_get_text
        procedure :: set_icon => action_set_icon
        procedure :: set_shortcut => action_set_shortcut
        procedure :: set_tooltip => action_set_tooltip
        procedure :: set_checkable => action_set_checkable
        procedure :: set_checked => action_set_checked
        procedure :: is_checked => action_is_checked
        procedure :: set_enabled => action_set_enabled
        procedure :: trigger => action_trigger
    end type QAction

    !> @brief Menu widget
    type, extends(forge_widget) :: QMenu
        private
        type(QString) :: title
        type(QAction), allocatable :: actions(:)
        type(QMenu), allocatable :: submenus(:)
        integer :: action_count = 0
        integer :: submenu_count = 0
        type(signal_void) :: about_to_show
        type(signal_void) :: about_to_hide
    contains
        procedure :: set_title => menu_set_title
        procedure :: get_title => menu_get_title
        procedure :: add_action => menu_add_action
        procedure :: add_separator => menu_add_separator
        procedure :: add_menu => menu_add_menu
        procedure :: clear => menu_clear
        procedure :: popup => menu_popup
    end type QMenu

    !> @brief MenuBar widget
    type, extends(forge_widget) :: QMenuBar
        private
        type(QMenu), allocatable :: menus(:)
        integer :: menu_count = 0
    contains
        procedure :: add_menu => menubar_add_menu
        procedure :: add_action => menubar_add_action
        procedure :: clear => menubar_clear
    end type QMenuBar

contains

    ! ========== QAction Implementation ==========

    subroutine action_set_text(this, text)
        class(QAction), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine action_set_text

    function action_get_text(this) result(text)
        class(QAction), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function action_get_text

    subroutine action_set_icon(this, icon_path)
        class(QAction), intent(inout) :: this
        character(len=*), intent(in) :: icon_path
        call this%icon_path%set(icon_path)
    end subroutine action_set_icon

    subroutine action_set_shortcut(this, shortcut)
        class(QAction), intent(inout) :: this
        character(len=*), intent(in) :: shortcut
        call this%shortcut%set(shortcut)
    end subroutine action_set_shortcut

    subroutine action_set_tooltip(this, tooltip)
        class(QAction), intent(inout) :: this
        character(len=*), intent(in) :: tooltip
        call this%tooltip%set(tooltip)
    end subroutine action_set_tooltip

    subroutine action_set_checkable(this, checkable)
        class(QAction), intent(inout) :: this
        logical, intent(in) :: checkable
        this%checkable = checkable
    end subroutine action_set_checkable

    subroutine action_set_checked(this, checked)
        class(QAction), intent(inout) :: this
        logical, intent(in) :: checked
        logical :: old_state
        
        if (.not. this%checkable) return
        
        old_state = this%checked
        this%checked = checked
        
        if (old_state .neqv. checked) then
            call this%toggled%emit(checked)
        end if
    end subroutine action_set_checked

    function action_is_checked(this) result(checked)
        class(QAction), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function action_is_checked

    subroutine action_set_enabled(this, enabled)
        class(QAction), intent(inout) :: this
        logical, intent(in) :: enabled
        this%enabled = enabled
    end subroutine action_set_enabled

    subroutine action_trigger(this)
        class(QAction), intent(inout) :: this
        
        if (.not. this%enabled) return
        
        if (this%checkable) then
            call this%set_checked(.not. this%checked)
        end if
        
        call this%triggered%emit()
    end subroutine action_trigger

    ! ========== QMenu Implementation ==========

    subroutine menu_set_title(this, title)
        class(QMenu), intent(inout) :: this
        character(len=*), intent(in) :: title
        call this%title%set(title)
    end subroutine menu_set_title

    function menu_get_title(this) result(title)
        class(QMenu), intent(in) :: this
        character(len=:), allocatable :: title
        title = this%title%get()
    end function menu_get_title

    function menu_add_action(this, text) result(action)
        class(QMenu), intent(inout) :: this
        character(len=*), intent(in) :: text
        type(QAction) :: action
        type(QAction), allocatable :: temp(:)
        
        if (.not. allocated(this%actions)) then
            allocate(this%actions(10))
        else if (this%action_count >= size(this%actions)) then
            allocate(temp(size(this%actions) * 2))
            temp(1:this%action_count) = this%actions(1:this%action_count)
            call move_alloc(temp, this%actions)
        end if
        
        this%action_count = this%action_count + 1
        call this%actions(this%action_count)%set_text(text)
        action = this%actions(this%action_count)
    end function menu_add_action

    subroutine menu_add_separator(this)
        class(QMenu), intent(inout) :: this
        type(QAction) :: action
        
        action = this%add_action("")
        action%separator = .true.
    end subroutine menu_add_separator

    function menu_add_menu(this, title) result(submenu)
        class(QMenu), intent(inout) :: this
        character(len=*), intent(in) :: title
        type(QMenu) :: submenu
        type(QMenu), allocatable :: temp(:)
        
        if (.not. allocated(this%submenus)) then
            allocate(this%submenus(5))
        else if (this%submenu_count >= size(this%submenus)) then
            allocate(temp(size(this%submenus) * 2))
            temp(1:this%submenu_count) = this%submenus(1:this%submenu_count)
            call move_alloc(temp, this%submenus)
        end if
        
        this%submenu_count = this%submenu_count + 1
        call this%submenus(this%submenu_count)%set_title(title)
        submenu = this%submenus(this%submenu_count)
    end function menu_add_menu

    subroutine menu_clear(this)
        class(QMenu), intent(inout) :: this
        this%action_count = 0
        this%submenu_count = 0
    end subroutine menu_clear

    subroutine menu_popup(this, pos)
        class(QMenu), intent(inout) :: this
        type(forge_position), intent(in), optional :: pos
        ! TODO: Show menu at position
        call this%about_to_show%emit()
    end subroutine menu_popup

    ! ========== QMenuBar Implementation ==========

    function menubar_add_menu(this, title) result(menu)
        class(QMenuBar), intent(inout) :: this
        character(len=*), intent(in) :: title
        type(QMenu) :: menu
        type(QMenu), allocatable :: temp(:)
        
        if (.not. allocated(this%menus)) then
            allocate(this%menus(10))
        else if (this%menu_count >= size(this%menus)) then
            allocate(temp(size(this%menus) * 2))
            temp(1:this%menu_count) = this%menus(1:this%menu_count)
            call move_alloc(temp, this%menus)
        end if
        
        this%menu_count = this%menu_count + 1
        call this%menus(this%menu_count)%set_title(title)
        menu = this%menus(this%menu_count)
    end function menubar_add_menu

    function menubar_add_action(this, text) result(action)
        class(QMenuBar), intent(inout) :: this
        character(len=*), intent(in) :: text
        type(QAction) :: action
        ! TODO: Add action directly to menu bar
        call action%set_text(text)
    end function menubar_add_action

    subroutine menubar_clear(this)
        class(QMenuBar), intent(inout) :: this
        this%menu_count = 0
    end subroutine menubar_clear

end module forge_menubar

