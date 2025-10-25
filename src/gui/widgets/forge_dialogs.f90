!> @brief Dialog widgets
!> @details Common dialog types (File, Color, Font, etc.)
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_dialogs
    use forge_window
    use forge_types
    use forge_string_utils
    use forge_signals
    implicit none
    private

    public :: QFileDialog, QColorDialog, QFontDialog, QInputDialog
    public :: QDialog, DialogResult, Accepted, Rejected
    public :: FileMode, AnyFile, ExistingFile, ExistingFiles, Directory

    !> Dialog result codes
    integer, parameter :: Rejected = 0
    integer, parameter :: Accepted = 1

    type :: DialogResult
        integer :: value = Rejected
    end type DialogResult

    !> File dialog modes
    integer, parameter :: AnyFile = 0
    integer, parameter :: ExistingFile = 1
    integer, parameter :: ExistingFiles = 2
    integer, parameter :: Directory = 3

    type :: FileMode
        integer :: value = ExistingFile
    end type FileMode

    !> @brief Base dialog class
    type, extends(forge_window_t) :: QDialog
        private
        type(DialogResult) :: result_code
        logical :: modal = .true.
        type(signal_int) :: finished
        type(signal_void) :: accepted
        type(signal_void) :: rejected
    contains
        procedure :: exec => dialog_exec
        procedure :: accept => dialog_accept
        procedure :: reject => dialog_reject
        procedure :: get_result => dialog_get_result
        procedure :: set_modal => dialog_set_modal
    end type QDialog

    !> @brief File dialog
    type, extends(QDialog) :: QFileDialog
        private
        type(FileMode) :: file_mode
        type(QString) :: directory
        type(QString), allocatable :: filters(:)
        integer :: filter_count = 0
        type(QString), allocatable :: selected_files(:)
        integer :: selected_count = 0
    contains
        procedure :: set_file_mode => filedialog_set_mode
        procedure :: set_directory => filedialog_set_directory
        procedure :: set_name_filter => filedialog_set_filter
        procedure :: get_selected_files => filedialog_get_selected
        procedure :: get_open_filename => filedialog_get_open
        procedure :: get_save_filename => filedialog_get_save
        procedure :: get_existing_directory => filedialog_get_directory
    end type QFileDialog

    !> @brief Color dialog
    type, extends(QDialog) :: QColorDialog
        private
        type(forge_color) :: current_color
        logical :: show_alpha = .false.
    contains
        procedure :: set_current_color => colordialog_set_color
        procedure :: get_current_color => colordialog_get_color
        procedure :: set_options => colordialog_set_options
    end type QColorDialog

    !> @brief Font dialog
    type, extends(QDialog) :: QFontDialog
        private
        type(QString) :: font_family
        integer :: font_size = 12
        logical :: font_bold = .false.
        logical :: font_italic = .false.
    contains
        procedure :: get_font_family => fontdialog_get_family
        procedure :: get_font_size => fontdialog_get_size
    end type QFontDialog

    !> @brief Input dialog
    type, extends(QDialog) :: QInputDialog
        private
        type(QString) :: label_text
        type(QString) :: text_value
        integer :: int_value = 0
        real :: double_value = 0.0
        integer :: input_mode = 0  ! 0=text, 1=int, 2=double
    contains
        procedure :: set_label_text => inputdialog_set_label
        procedure :: get_text_value => inputdialog_get_text
        procedure :: get_int_value => inputdialog_get_int
        procedure :: get_double_value => inputdialog_get_double
    end type QInputDialog

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

    function qdate_to_string(this) result(str)
        class(QDate), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        write(buffer, '(I4.4,A,I2.2,A,I2.2)') this%year, '-', this%month, '-', this%day
        str = trim(buffer)
    end function qdate_to_string

    function qdate_is_valid(this) result(valid)
        class(QDate), intent(in) :: this
        logical :: valid
        valid = (this%month >= 1 .and. this%month <= 12 .and. &
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

    function qtime_to_string(this) result(str)
        class(QTime), intent(in) :: this
        character(len=:), allocatable :: str
        character(len=32) :: buffer
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

    function qdatetime_to_string(this) result(str)
        class(QDateTime), intent(in) :: this
        character(len=:), allocatable :: str
        str = this%date%to_string() // ' ' // this%time%to_string()
    end function qdatetime_to_string

    ! ========== QDialog Implementation ==========

    function dialog_exec(this) result(result_code)
        class(QDialog), intent(inout) :: this
        integer :: result_code
        
        ! TODO: Show modal dialog and run event loop until closed
        result_code = Accepted
        call this%finished%emit(result_code)
    end function dialog_exec

    subroutine dialog_accept(this)
        class(QDialog), intent(inout) :: this
        this%result_code%value = Accepted
        call this%accepted%emit()
        call this%finished%emit(Accepted)
    end subroutine dialog_accept

    subroutine dialog_reject(this)
        class(QDialog), intent(inout) :: this
        this%result_code%value = Rejected
        call this%rejected%emit()
        call this%finished%emit(Rejected)
    end subroutine dialog_reject

    function dialog_get_result(this) result(result_code)
        class(QDialog), intent(in) :: this
        integer :: result_code
        result_code = this%result_code%value
    end function dialog_get_result

    subroutine dialog_set_modal(this, modal)
        class(QDialog), intent(inout) :: this
        logical, intent(in) :: modal
        this%modal = modal
    end subroutine dialog_set_modal

    ! ========== QFileDialog Implementation ==========

    subroutine filedialog_set_mode(this, mode)
        class(QFileDialog), intent(inout) :: this
        integer, intent(in) :: mode
        this%file_mode%value = mode
    end subroutine filedialog_set_mode

    subroutine filedialog_set_directory(this, directory)
        class(QFileDialog), intent(inout) :: this
        character(len=*), intent(in) :: directory
        call this%directory%set(directory)
    end subroutine filedialog_set_directory

    subroutine filedialog_set_filter(this, filter)
        class(QFileDialog), intent(inout) :: this
        character(len=*), intent(in) :: filter
        type(QString), allocatable :: temp(:)
        
        if (.not. allocated(this%filters)) then
            allocate(this%filters(5))
        else if (this%filter_count >= size(this%filters)) then
            allocate(temp(size(this%filters) * 2))
            temp(1:this%filter_count) = this%filters(1:this%filter_count)
            call move_alloc(temp, this%filters)
        end if
        
        this%filter_count = this%filter_count + 1
        call this%filters(this%filter_count)%set(filter)
    end subroutine filedialog_set_filter

    function filedialog_get_selected(this) result(files)
        class(QFileDialog), intent(in) :: this
        type(QString), allocatable :: files(:)
        
        if (allocated(this%selected_files)) then
            allocate(files(this%selected_count))
            files = this%selected_files(1:this%selected_count)
        else
            allocate(files(0))
        end if
    end function filedialog_get_selected

    function filedialog_get_open(parent, caption, dir, filter) result(filename)
        type(forge_window_t), intent(in), optional :: parent
        character(len=*), intent(in), optional :: caption, dir, filter
        character(len=:), allocatable :: filename
        type(QFileDialog) :: dialog
        
        ! TODO: Show file open dialog
        allocate(character(len=0) :: filename)
    end function filedialog_get_open

    function filedialog_get_save(parent, caption, dir, filter) result(filename)
        type(forge_window_t), intent(in), optional :: parent
        character(len=*), intent(in), optional :: caption, dir, filter
        character(len=:), allocatable :: filename
        type(QFileDialog) :: dialog
        
        ! TODO: Show file save dialog
        allocate(character(len=0) :: filename)
    end function filedialog_get_save

    function filedialog_get_directory(parent, caption, dir) result(dirname)
        type(forge_window_t), intent(in), optional :: parent
        character(len=*), intent(in), optional :: caption, dir
        character(len=:), allocatable :: dirname
        type(QFileDialog) :: dialog
        
        ! TODO: Show directory selection dialog
        allocate(character(len=0) :: dirname)
    end function filedialog_get_directory

    ! ========== QColorDialog Implementation ==========

    subroutine colordialog_set_color(this, color)
        class(QColorDialog), intent(inout) :: this
        type(forge_color), intent(in) :: color
        this%current_color = color
    end subroutine colordialog_set_color

    function colordialog_get_color(this) result(color)
        class(QColorDialog), intent(in) :: this
        type(forge_color) :: color
        color = this%current_color
    end function colordialog_get_color

    subroutine colordialog_set_options(this, show_alpha)
        class(QColorDialog), intent(inout) :: this
        logical, intent(in) :: show_alpha
        this%show_alpha = show_alpha
    end subroutine colordialog_set_options

    ! ========== QFontDialog Implementation ==========

    function fontdialog_get_family(this) result(family)
        class(QFontDialog), intent(in) :: this
        character(len=:), allocatable :: family
        family = this%font_family%get()
    end function fontdialog_get_family

    function fontdialog_get_size(this) result(size_val)
        class(QFontDialog), intent(in) :: this
        integer :: size_val
        size_val = this%font_size
    end function fontdialog_get_size

    ! ========== QInputDialog Implementation ==========

    subroutine inputdialog_set_label(this, text)
        class(QInputDialog), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%label_text%set(text)
    end subroutine inputdialog_set_label

    function inputdialog_get_text(this) result(text)
        class(QInputDialog), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text_value%get()
    end function inputdialog_get_text

    function inputdialog_get_int(this) result(value)
        class(QInputDialog), intent(in) :: this
        integer :: value
        value = this%int_value
    end function inputdialog_get_int

    function inputdialog_get_double(this) result(value)
        class(QInputDialog), intent(in) :: this
        real :: value
        value = this%double_value
    end function inputdialog_get_double

end module forge_dialogs

