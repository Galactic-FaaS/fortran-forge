!> @brief Mobile-optimized layout system
!> @details Responsive layouts for various mobile screen sizes and orientations
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_mobile_layout
    use iso_c_binding
    use forge_types
    use forge_layout
    use forge_platform
    implicit none
    private

    public :: forge_mobile_layout_manager
    public :: mobile_layout_create, mobile_layout_add_constraint
    public :: mobile_layout_update_for_orientation, mobile_layout_get_safe_area

    !> @brief Safe area insets (for notches, home indicators, etc.)
    type :: safe_area_insets
        integer(c_int) :: top = 0
        integer(c_int) :: left = 0
        integer(c_int) :: bottom = 0
        integer(c_int) :: right = 0
    end type safe_area_insets

    !> @brief Layout constraint for mobile
    type :: mobile_layout_constraint
        type(c_ptr) :: widget = c_null_ptr
        integer :: attribute = 0  ! width, height, centerX, centerY, etc.
        integer :: relation = 0   ! equal, greaterThan, lessThan
        type(c_ptr) :: related_widget = c_null_ptr
        integer :: related_attribute = 0
        real(c_float) :: multiplier = 1.0
        real(c_float) :: constant = 0.0
        integer :: priority = 1000  ! 0-1000, higher = more important
    end type mobile_layout_constraint

    !> @brief Mobile layout manager
    type :: forge_mobile_layout_manager
        private
        type(safe_area_insets) :: safe_area
        type(mobile_layout_constraint), allocatable :: constraints(:)
        integer :: constraint_count = 0
        integer :: current_orientation = ORIENTATION_PORTRAIT
        integer :: screen_width = 375   ! Default iPhone width
        integer :: screen_height = 667  ! Default iPhone height
    contains
        procedure :: add_constraint => mobile_layout_add_constraint
        procedure :: update_for_orientation => mobile_layout_update_for_orientation
        procedure :: get_safe_area => mobile_layout_get_safe_area
        procedure :: apply_layout => mobile_layout_apply_layout
        procedure :: update_screen_size => mobile_layout_update_screen_size
    end type forge_mobile_layout_manager

contains

    !> @brief Create mobile layout manager
    function mobile_layout_create() result(manager)
        type(forge_mobile_layout_manager) :: manager

        ! Initialize with default safe area (no notches)
        manager%safe_area%top = 20    ! Status bar
        manager%safe_area%left = 0
        manager%safe_area%bottom = 0
        manager%safe_area%right = 0

        manager%constraint_count = 0
        manager%current_orientation = ORIENTATION_PORTRAIT
        manager%screen_width = 375
        manager%screen_height = 667

        allocate(manager%constraints(10))  ! Initial capacity
    end function mobile_layout_create

    !> @brief Add layout constraint
    subroutine mobile_layout_add_constraint(this, constraint)
        class(forge_mobile_layout_manager), intent(inout) :: this
        type(mobile_layout_constraint), intent(in) :: constraint

        if (this%constraint_count >= size(this%constraints)) then
            call resize_constraint_array(this)
        end if

        this%constraint_count = this%constraint_count + 1
        this%constraints(this%constraint_count) = constraint
    end subroutine mobile_layout_add_constraint

    !> @brief Update layout for orientation change
    subroutine mobile_layout_update_for_orientation(this, orientation)
        class(forge_mobile_layout_manager), intent(inout) :: this
        integer, intent(in) :: orientation

        this%current_orientation = orientation

        ! Update safe area based on orientation and device
        call update_safe_area_for_orientation(this)

        ! Reapply all constraints
        call this%apply_layout()
    end subroutine mobile_layout_update_for_orientation

    !> @brief Get current safe area insets
    function mobile_layout_get_safe_area(this) result(safe_area)
        class(forge_mobile_layout_manager), intent(in) :: this
        type(safe_area_insets) :: safe_area

        safe_area = this%safe_area
    end function mobile_layout_get_safe_area

    !> @brief Apply layout constraints
    subroutine mobile_layout_apply_layout(this)
        class(forge_mobile_layout_manager), intent(inout) :: this
        integer :: i
        type(mobile_layout_constraint) :: constraint

        ! Simple constraint solver (would be more sophisticated in real implementation)
        do i = 1, this%constraint_count
            constraint = this%constraints(i)
            call apply_single_constraint(constraint, this%screen_width, this%screen_height, this%safe_area)
        end do
    end subroutine mobile_layout_apply_layout

    !> @brief Update screen size
    subroutine mobile_layout_update_screen_size(this, width, height)
        class(forge_mobile_layout_manager), intent(inout) :: this
        integer(c_int), intent(in) :: width, height

        this%screen_width = width
        this%screen_height = height

        ! Determine orientation from dimensions
        if (width > height) then
            this%current_orientation = ORIENTATION_LANDSCAPE
        else
            this%current_orientation = ORIENTATION_PORTRAIT
        end if

        call update_safe_area_for_orientation(this)
        call this%apply_layout()
    end subroutine mobile_layout_update_screen_size

    !> @brief Update safe area based on orientation and device characteristics
    subroutine update_safe_area_for_orientation(this)
        class(forge_mobile_layout_manager), intent(inout) :: this

        ! Reset safe area
        this%safe_area%top = 20    ! Status bar
        this%safe_area%left = 0
        this%safe_area%bottom = 0
        this%safe_area%right = 0

        ! Adjust for orientation
        if (this%current_orientation == ORIENTATION_LANDSCAPE) then
            ! In landscape, status bar might be on the side or hidden
            this%safe_area%top = 0
            this%safe_area%left = 20
        end if

        ! Add safe area for devices with notches/home indicators
        ! This would be device-specific in a real implementation
        if (has_notch()) then
            if (this%current_orientation == ORIENTATION_PORTRAIT) then
                this%safe_area%top = 44   ! Notch area
                this%safe_area%bottom = 34 ! Home indicator
            else
                this%safe_area%left = 44  ! Notch on landscape left
                this%safe_area%right = 44 ! Home indicator area
            end if
        end if
    end subroutine update_safe_area_for_orientation

    !> @brief Apply a single layout constraint
    subroutine apply_single_constraint(constraint, screen_width, screen_height, safe_area)
        type(mobile_layout_constraint), intent(in) :: constraint
        integer, intent(in) :: screen_width, screen_height
        type(safe_area_insets), intent(in) :: safe_area
        ! This would actually modify widget positions/sizes
        ! For now, just a placeholder
    end subroutine apply_single_constraint

    !> @brief Check if device has notch (simplified)
    function has_notch() result(has)
        logical :: has
        ! In real implementation, this would detect device model
        has = .true.  ! Assume modern device with notch
    end function has_notch

    !> @brief Resize constraint array
    subroutine resize_constraint_array(this)
        class(forge_mobile_layout_manager), intent(inout) :: this
        type(mobile_layout_constraint), allocatable :: new_constraints(:)
        integer :: new_size

        new_size = size(this%constraints) * 2
        allocate(new_constraints(new_size))
        new_constraints(1:size(this%constraints)) = this%constraints

        deallocate(this%constraints)
        this%constraints = new_constraints
    end subroutine resize_constraint_array

end module forge_mobile_layout