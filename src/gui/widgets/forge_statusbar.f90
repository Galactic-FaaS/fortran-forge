!> @brief StatusBar widget
!> @details Status bar for displaying application status
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_statusbar
    use forge_widgets
    use forge_types
    use forge_string_utils
    implicit none
    private

    public :: QStatusBar

    !> @brief Status bar widget
    type, extends(forge_widget) :: QStatusBar
        private
        type(QString) :: message
        integer :: timeout = 0  ! 0 = permanent
        type(forge_widget), allocatable :: permanent_widgets(:)
        integer :: permanent_count = 0
    contains
        procedure :: show_message => statusbar_show_message
        procedure :: clear_message => statusbar_clear_message
        procedure :: get_message => statusbar_get_message
        procedure :: add_permanent_widget => statusbar_add_permanent
        procedure :: remove_widget => statusbar_remove_widget
    end type QStatusBar

contains

    subroutine statusbar_show_message(this, message, timeout)
        class(QStatusBar), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: timeout
        
        call this%message%set(message)
        
        if (present(timeout)) then
            this%timeout = timeout
        else
            this%timeout = 0  ! Permanent
        end if
        
        ! TODO: Start timer if timeout > 0
    end subroutine statusbar_show_message

    subroutine statusbar_clear_message(this)
        class(QStatusBar), intent(inout) :: this
        call this%message%clear()
        this%timeout = 0
    end subroutine statusbar_clear_message

    function statusbar_get_message(this) result(message)
        class(QStatusBar), intent(in) :: this
        character(len=:), allocatable :: message
        message = this%message%get()
    end function statusbar_get_message

    subroutine statusbar_add_permanent(this, widget)
        class(QStatusBar), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        class(forge_widget), allocatable :: temp(:)
        
        if (.not. allocated(this%permanent_widgets)) then
            allocate(this%permanent_widgets(5))
        else if (this%permanent_count >= size(this%permanent_widgets)) then
            allocate(temp(size(this%permanent_widgets) * 2))
            temp(1:this%permanent_count) = this%permanent_widgets(1:this%permanent_count)
            call move_alloc(temp, this%permanent_widgets)
        end if
        
        this%permanent_count = this%permanent_count + 1
        this%permanent_widgets(this%permanent_count) = widget
    end subroutine statusbar_add_permanent

    subroutine statusbar_remove_widget(this, widget)
        class(QStatusBar), intent(inout) :: this
        class(forge_widget), intent(in) :: widget
        ! TODO: Remove specific widget
    end subroutine statusbar_remove_widget

end module forge_statusbar

