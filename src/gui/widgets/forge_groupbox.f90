!> @brief GroupBox widget (container with title)
!> @details Container widget with a frame and title
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_groupbox
    use forge_widgets
    use forge_types
    use forge_string_utils
    use forge_layout
    implicit none
    private

    public :: QGroupBox

    !> @brief GroupBox widget
    type, extends(forge_widget) :: QGroupBox
        private
        type(QString) :: title
        logical :: checkable = .false.
        logical :: checked = .true.
        logical :: flat = .false.
        class(forge_layout_base), allocatable :: layout
    contains
        procedure :: set_title => groupbox_set_title
        procedure :: get_title => groupbox_get_title
        procedure :: set_checkable => groupbox_set_checkable
        procedure :: is_checkable => groupbox_is_checkable
        procedure :: set_checked => groupbox_set_checked
        procedure :: is_checked => groupbox_is_checked
        procedure :: set_flat => groupbox_set_flat
        procedure :: is_flat => groupbox_is_flat
        procedure :: set_layout => groupbox_set_layout
    end type QGroupBox

contains

    subroutine groupbox_set_title(this, title)
        class(QGroupBox), intent(inout) :: this
        character(len=*), intent(in) :: title
        call this%title%set(title)
    end subroutine groupbox_set_title

    function groupbox_get_title(this) result(title)
        class(QGroupBox), intent(in) :: this
        character(len=:), allocatable :: title
        title = this%title%get()
    end function groupbox_get_title

    subroutine groupbox_set_checkable(this, checkable)
        class(QGroupBox), intent(inout) :: this
        logical, intent(in) :: checkable
        this%checkable = checkable
    end subroutine groupbox_set_checkable

    function groupbox_is_checkable(this) result(checkable)
        class(QGroupBox), intent(in) :: this
        logical :: checkable
        checkable = this%checkable
    end function groupbox_is_checkable

    subroutine groupbox_set_checked(this, checked)
        class(QGroupBox), intent(inout) :: this
        logical, intent(in) :: checked
        if (this%checkable) this%checked = checked
    end subroutine groupbox_set_checked

    function groupbox_is_checked(this) result(checked)
        class(QGroupBox), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function groupbox_is_checked

    subroutine groupbox_set_flat(this, flat)
        class(QGroupBox), intent(inout) :: this
        logical, intent(in) :: flat
        this%flat = flat
    end subroutine groupbox_set_flat

    function groupbox_is_flat(this) result(flat)
        class(QGroupBox), intent(in) :: this
        logical :: flat
        flat = this%flat
    end function groupbox_is_flat

    subroutine groupbox_set_layout(this, layout)
        class(QGroupBox), intent(inout) :: this
        class(forge_layout_base), intent(in) :: layout
        
        if (allocated(this%layout)) deallocate(this%layout)
        allocate(this%layout, source=layout)
    end subroutine groupbox_set_layout

end module forge_groupbox

