!> @brief QCalendarWidget implementation
!> @details Monthly calendar display with date selection, navigation, and customization
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_calendarwidget
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QCalendarWidget

    !> @brief Date structure for calendar operations
    type :: forge_date
        integer :: year = 2025
        integer :: month = 1
        integer :: day = 1
    contains
        procedure :: set_date => forge_date_set
        procedure :: get_year => forge_date_get_year
        procedure :: get_month => forge_date_get_month
        procedure :: get_day => forge_date_get_day
        procedure :: is_valid => forge_date_is_valid
        procedure :: to_string => forge_date_to_string
    end type forge_date

    !> @brief Calendar widget with date selection and navigation
    type, extends(forge_widget) :: QCalendarWidget
        private
        type(forge_date) :: selected_date
        type(forge_date) :: minimum_date
        type(forge_date) :: maximum_date
        logical :: grid_visible = .true.
        logical :: navigation_bar_visible = .true.
        logical :: horizontal_header_visible = .true.
        logical :: vertical_header_visible = .true.
        integer :: first_day_of_week = 1  ! 1=Monday, 7=Sunday
        integer :: selection_mode = 1     ! 1=Single, 2=Multiple
        type(signal_date) :: clicked
        type(signal_date) :: activated
        type(signal_date) :: current_page_changed
        type(signal_date) :: selection_changed
    contains
        procedure :: set_selected_date => calendar_set_selected_date
        procedure :: selected_date => calendar_selected_date
        procedure :: set_date_range => calendar_set_date_range
        procedure :: set_grid_visible => calendar_set_grid_visible
        procedure :: set_navigation_bar_visible => calendar_set_navigation_bar_visible
        procedure :: set_first_day_of_week => calendar_set_first_day_of_week
        procedure :: set_selection_mode => calendar_set_selection_mode
        procedure :: show_previous_month => calendar_show_previous_month
        procedure :: show_next_month => calendar_show_next_month
        procedure :: show_today => calendar_show_today
        procedure :: month_shown => calendar_month_shown
        procedure :: year_shown => calendar_year_shown
        procedure :: set_current_page => calendar_set_current_page
    end type QCalendarWidget

    !> @brief Date signal type
    type :: signal_date
        private
        integer :: connection_count = 0
        type(slot_date_proc), dimension(:), allocatable :: slots
        type(signal_connection) :: connections(100)
        logical :: blocked = .false.
    contains
        procedure :: connect => signal_date_connect
        procedure :: disconnect => signal_date_disconnect
        procedure :: emit => signal_date_emit
        procedure :: is_connected => signal_date_is_connected
    end type signal_date

    !> @brief Date slot procedure pointer
    type :: slot_date_proc
        procedure(slot_date), pointer, nopass :: proc => null()
    end type slot_date_proc

contains

    ! ========== forge_date Implementation ==========

    subroutine forge_date_set(this, year, month, day)
        class(forge_date), intent(inout) :: this
        integer, intent(in) :: year, month, day
        this%year = year
        this%month = month
        this%day = day
    end subroutine forge_date_set

    function forge_date_get_year(this) result(year)
        class(forge_date), intent(in) :: this
        integer :: year
        year = this%year
    end function forge_date_get_year

    function forge_date_get_month(this) result(month)
        class(forge_date), intent(in) :: this
        integer :: month
        month = this%month
    end function forge_date_get_month

    function forge_date_get_day(this) result(day)
        class(forge_date), intent(in) :: this
        integer :: day
        day = this%day
    end function forge_date_get_day

    function forge_date_is_valid(this) result(valid)
        class(forge_date), intent(in) :: this
        logical :: valid
        integer :: days_in_month

        valid = .false.
        if (this%year < 1 .or. this%year > 9999) return
        if (this%month < 1 .or. this%month > 12) return
        if (this%day < 1) return

        ! Check days in month
        select case (this%month)
        case (1,3,5,7,8,10,12)
            days_in_month = 31
        case (4,6,9,11)
            days_in_month = 30
        case (2)
            ! Leap year check
            if (mod(this%year, 4) == 0 .and. (mod(this%year, 100) /= 0 .or. mod(this%year, 400) == 0)) then
                days_in_month = 29
            else
                days_in_month = 28
            end if
        end select

        if (this%day <= days_in_month) valid = .true.
    end function forge_date_is_valid

    function forge_date_to_string(this, format) result(str)
        class(forge_date), intent(in) :: this
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str
        character(len=10) :: default_format

        if (present(format)) then
            ! Simple format support - just YYYY-MM-DD for now
            write(str, '(I4.4,A,I2.2,A,I2.2)') this%year, '-', this%month, '-', this%day
        else
            write(str, '(I4.4,A,I2.2,A,I2.2)') this%year, '-', this%month, '-', this%day
        end if
    end function forge_date_to_string

    ! ========== QCalendarWidget Implementation ==========

    subroutine calendar_set_selected_date(this, date)
        class(QCalendarWidget), intent(inout) :: this
        type(forge_date), intent(in) :: date

        if (date%is_valid()) then
            this%selected_date = date
            call this%selection_changed%emit(date)
        end if
    end subroutine calendar_set_selected_date

    function calendar_selected_date(this) result(date)
        class(QCalendarWidget), intent(in) :: this
        type(forge_date) :: date
        date = this%selected_date
    end function calendar_selected_date

    subroutine calendar_set_date_range(this, min_date, max_date)
        class(QCalendarWidget), intent(inout) :: this
        type(forge_date), intent(in) :: min_date, max_date

        if (min_date%is_valid() .and. max_date%is_valid()) then
            this%minimum_date = min_date
            this%maximum_date = max_date
        end if
    end subroutine calendar_set_date_range

    subroutine calendar_set_grid_visible(this, visible)
        class(QCalendarWidget), intent(inout) :: this
        logical, intent(in) :: visible
        this%grid_visible = visible
    end subroutine calendar_set_grid_visible

    subroutine calendar_set_navigation_bar_visible(this, visible)
        class(QCalendarWidget), intent(inout) :: this
        logical, intent(in) :: visible
        this%navigation_bar_visible = visible
    end subroutine calendar_set_navigation_bar_visible

    subroutine calendar_set_first_day_of_week(this, day)
        class(QCalendarWidget), intent(inout) :: this
        integer, intent(in) :: day
        if (day >= 1 .and. day <= 7) then
            this%first_day_of_week = day
        end if
    end subroutine calendar_set_first_day_of_week

    subroutine calendar_set_selection_mode(this, mode)
        class(QCalendarWidget), intent(inout) :: this
        integer, intent(in) :: mode
        if (mode >= 1 .and. mode <= 2) then
            this%selection_mode = mode
        end if
    end subroutine calendar_set_selection_mode

    subroutine calendar_show_previous_month(this)
        class(QCalendarWidget), intent(inout) :: this
        type(forge_date) :: current_page

        current_page = this%selected_date
        if (current_page%month == 1) then
            current_page%month = 12
            current_page%year = current_page%year - 1
        else
            current_page%month = current_page%month - 1
        end if

        call this%set_current_page(current_page%year, current_page%month)
    end subroutine calendar_show_previous_month

    subroutine calendar_show_next_month(this)
        class(QCalendarWidget), intent(inout) :: this
        type(forge_date) :: current_page

        current_page = this%selected_date
        if (current_page%month == 12) then
            current_page%month = 1
            current_page%year = current_page%year + 1
        else
            current_page%month = current_page%month + 1
        end if

        call this%set_current_page(current_page%year, current_page%month)
    end subroutine calendar_show_next_month

    subroutine calendar_show_today(this)
        class(QCalendarWidget), intent(inout) :: this
        type(forge_date) :: today

        ! In a real implementation, this would get the current date
        ! For now, use a fixed date
        call today%set_date(2025, 10, 29)
        call this%set_selected_date(today)
        call this%set_current_page(2025, 10)
    end subroutine calendar_show_today

    function calendar_month_shown(this) result(month)
        class(QCalendarWidget), intent(in) :: this
        integer :: month
        month = this%selected_date%get_month()
    end function calendar_month_shown

    function calendar_year_shown(this) result(year)
        class(QCalendarWidget), intent(in) :: this
        integer :: year
        year = this%selected_date%get_year()
    end function calendar_year_shown

    subroutine calendar_set_current_page(this, year, month)
        class(QCalendarWidget), intent(inout) :: this
        integer, intent(in) :: year, month
        type(forge_date) :: new_page

        call new_page%set_date(year, month, 1)
        if (new_page%is_valid()) then
            this%selected_date%year = year
            this%selected_date%month = month
            call this%current_page_changed%emit(new_page)
        end if
    end subroutine calendar_set_current_page

    ! ========== signal_date Implementation ==========

    subroutine signal_date_connect(this, slot)
        class(signal_date), intent(inout) :: this
        procedure(slot_date) :: slot

        if (this%connection_count < size(this%slots)) then
            this%connection_count = this%connection_count + 1
            allocate(this%slots(this%connection_count)%proc, source=slot)
        end if
    end subroutine signal_date_connect

    subroutine signal_date_disconnect(this, slot)
        class(signal_date), intent(inout) :: this
        procedure(slot_date) :: slot
        integer :: i

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc, slot)) then
                this%slots(i)%proc => null()
                exit
            end if
        end do
    end subroutine signal_date_disconnect

    subroutine signal_date_emit(this, date)
        class(signal_date), intent(inout) :: this
        type(forge_date), intent(in) :: date
        integer :: i

        if (this%blocked) return

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc)) then
                call this%slots(i)%proc(date)
            end if
        end do
    end subroutine signal_date_emit

    function signal_date_is_connected(this) result(connected)
        class(signal_date), intent(in) :: this
        logical :: connected
        connected = this%connection_count > 0
    end function signal_date_is_connected

end module forge_calendarwidget