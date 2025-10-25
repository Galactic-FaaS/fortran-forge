!> @brief Date and Time editing widgets
!> @details QDateEdit, QTimeEdit, QDateTimeEdit equivalents
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_dateedit
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    use forge_string_utils
    implicit none
    private

    public :: QDate, QTime, QDateTime
    public :: QDateEdit, QTimeEdit, QDateTimeEdit

    !> @brief Date type
    type :: QDate
        integer :: year = 2025
        integer :: month = 1
        integer :: day = 1
    contains
        procedure :: set_date => qdate_set
        procedure :: get_year => qdate_year
        procedure :: get_month => qdate_month
        procedure :: get_day => qdate_day
        procedure :: to_string => qdate_to_string
        procedure :: is_valid => qdate_is_valid
    end type QDate

    !> @brief Time type
    type :: QTime
        integer :: hour = 0
        integer :: minute = 0
        integer :: second = 0
        integer :: msec = 0
    contains
        procedure :: set_hms => qtime_set_hms
        procedure :: get_hour => qtime_hour
        procedure :: get_minute => qtime_minute
        procedure :: get_second => qtime_second
        procedure :: to_string => qtime_to_string
        procedure :: is_valid => qtime_is_valid
    end type QTime

    !> @brief DateTime type
    type :: QDateTime
        type(QDate) :: date
        type(QTime) :: time
    contains
        procedure :: to_string => qdatetime_to_string
    end type QDateTime

    !> @brief Date edit widget
    type, extends(forge_widget) :: QDateEdit
        private
        type(QDate) :: date_value
        type(QDate) :: minimum_date
        type(QDate) :: maximum_date
        type(QString) :: display_format
        logical :: calendar_popup = .false.
        ! Would need signal with date parameter
    contains
        procedure :: set_date => dateedit_set_date
        procedure :: get_date => dateedit_get_date
        procedure :: set_minimum_date => dateedit_set_minimum
        procedure :: set_maximum_date => dateedit_set_maximum
        procedure :: set_date_range => dateedit_set_range
        procedure :: set_display_format => dateedit_set_format
        procedure :: set_calendar_popup => dateedit_set_popup
    end type QDateEdit

    !> @brief Time edit widget
    type, extends(forge_widget) :: QTimeEdit
        private
        type(QTime) :: time_value
        type(QTime) :: minimum_time
        type(QTime) :: maximum_time
        type(QString) :: display_format
    contains
        procedure :: set_time => timeedit_set_time
        procedure :: get_time => timeedit_get_time
        procedure :: set_display_format => timeedit_set_format
    end type QTimeEdit

    !> @brief DateTime edit widget
    type, extends(forge_widget) :: QDateTimeEdit
        private
        type(QDateTime) :: datetime_value
        type(QString) :: display_format
    contains
        procedure :: set_datetime => datetimeedit_set_datetime
        procedure :: get_datetime => datetimeedit_get_datetime
    end type QDateTimeEdit

contains

    ! ========== QDate Implementation ==========

    subroutine qdate_set(this, year, month, day)
        class(QDate), intent(inout) :: this
        integer, intent(in) :: year, month, day
        
        this%year = year
        this%month = month
        this%day = day
    end subroutine qdate_set

    function qdate_year(this) result(year)
        class(QDate), intent(in) :: this
        integer :: year
        year = this%year
    end function qdate_year

    function qdate_month(this) result(month)
        class(QDate), intent(in) :: this
        integer :: month
        month = this%month
    end function qdate_month

    function qdate_day(this) result(day)
        class(QDate), intent(in) :: this
        integer :: day
        day = this%day
    end function qdate_day

    function qdate_to_string(this, format) result(str)
        class(QDate), intent(in) :: this
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str
        character(len=64) :: buffer
        
        ! Simple ISO format: YYYY-MM-DD
        write(buffer, '(I4.4,A,I2.2,A,I2.2)') this%year, '-', this%month, '-', this%day
        str = trim(buffer)
    end function qdate_to_string

    function qdate_is_valid(this) result(valid)
        class(QDate), intent(in) :: this
        logical :: valid
        
        valid = (this%year > 0 .and. &
                this%month >= 1 .and. this%month <= 12 .and. &
                this%day >= 1 .and. this%day <= 31)
    end function qdate_is_valid

    ! ========== QTime Implementation ==========

    subroutine qtime_set_hms(this, hour, minute, second, msec)
        class(QTime), intent(inout) :: this
        integer, intent(in) :: hour, minute, second
        integer, intent(in), optional :: msec
        
        this%hour = hour
        this%minute = minute
        this%second = second
        if (present(msec)) this%msec = msec
    end subroutine qtime_set_hms

    function qtime_hour(this) result(hour)
        class(QTime), intent(in) :: this
        integer :: hour
        hour = this%hour
    end function qtime_hour

    function qtime_minute(this) result(minute)
        class(QTime), intent(in) :: this
        integer :: minute
        minute = this%minute
    end function qtime_minute

    function qtime_second(this) result(second)
        class(QTime), intent(in) :: this
        integer :: second
        second = this%second
    end function qtime_second

    function qtime_to_string(this, format) result(str)
        class(QTime), intent(in) :: this
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        
        ! Simple format: HH:MM:SS
        write(buffer, '(I2.2,A,I2.2,A,I2.2)') this%hour, ':', this%minute, ':', this%second
        str = trim(buffer)
    end function qtime_to_string

    function qtime_is_valid(this) result(valid)
        class(QTime), intent(in) :: this
        logical :: valid
        
        valid = (this%hour >= 0 .and. this%hour <= 23 .and. &
                this%minute >= 0 .and. this%minute <= 59 .and. &
                this%second >= 0 .and. this%second <= 59)
    end function qtime_is_valid

    ! ========== QDateTime Implementation ==========

    function qdatetime_to_string(this, format) result(str)
        class(QDateTime), intent(in) :: this
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str
        
        str = this%date%to_string() // ' ' // this%time%to_string()
    end function qdatetime_to_string

    ! ========== QDateEdit Implementation ==========

    subroutine dateedit_set_date(this, date)
        class(QDateEdit), intent(inout) :: this
        type(QDate), intent(in) :: date
        
        this%date_value = date
        ! TODO: Emit date_changed signal
    end subroutine dateedit_set_date

    function dateedit_get_date(this) result(date)
        class(QDateEdit), intent(in) :: this
        type(QDate) :: date
        date = this%date_value
    end function dateedit_get_date

    subroutine dateedit_set_minimum(this, date)
        class(QDateEdit), intent(inout) :: this
        type(QDate), intent(in) :: date
        this%minimum_date = date
    end subroutine dateedit_set_minimum

    subroutine dateedit_set_maximum(this, date)
        class(QDateEdit), intent(inout) :: this
        type(QDate), intent(in) :: date
        this%maximum_date = date
    end subroutine dateedit_set_maximum

    subroutine dateedit_set_range(this, min_date, max_date)
        class(QDateEdit), intent(inout) :: this
        type(QDate), intent(in) :: min_date, max_date
        this%minimum_date = min_date
        this%maximum_date = max_date
    end subroutine dateedit_set_range

    subroutine dateedit_set_format(this, format)
        class(QDateEdit), intent(inout) :: this
        character(len=*), intent(in) :: format
        call this%display_format%set(format)
    end subroutine dateedit_set_format

    subroutine dateedit_set_popup(this, popup)
        class(QDateEdit), intent(inout) :: this
        logical, intent(in) :: popup
        this%calendar_popup = popup
    end subroutine dateedit_set_popup

    ! ========== QTimeEdit Implementation ==========

    subroutine timeedit_set_time(this, time)
        class(QTimeEdit), intent(inout) :: this
        type(QTime), intent(in) :: time
        this%time_value = time
    end subroutine timeedit_set_time

    function timeedit_get_time(this) result(time)
        class(QTimeEdit), intent(in) :: this
        type(QTime) :: time
        time = this%time_value
    end function timeedit_get_time

    subroutine timeedit_set_format(this, format)
        class(QTimeEdit), intent(inout) :: this
        character(len=*), intent(in) :: format
        call this%display_format%set(format)
    end subroutine timeedit_set_format

    ! ========== QDateTimeEdit Implementation ==========

    subroutine datetimeedit_set_datetime(this, datetime)
        class(QDateTimeEdit), intent(inout) :: this
        type(QDateTime), intent(in) :: datetime
        this%datetime_value = datetime
    end subroutine datetimeedit_set_datetime

    function datetimeedit_get_datetime(this) result(datetime)
        class(QDateTimeEdit), intent(in) :: this
        type(QDateTime) :: datetime
        datetime = this%datetime_value
    end function datetimeedit_get_datetime

end module forge_dateedit

