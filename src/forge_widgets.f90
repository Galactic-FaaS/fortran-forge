!> @brief Widget system for ForGE
!> @details Defines base widget class and common widget types
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_widgets
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_events
    use forge_backend
    implicit none
    private

    public :: forge_widget, forge_button, forge_label, forge_entry
    public :: forge_text_view, forge_progress_bar, forge_separator

    !> Widget type identifiers
    integer, parameter, public :: WIDGET_BUTTON = 1
    integer, parameter, public :: WIDGET_LABEL = 2
    integer, parameter, public :: WIDGET_ENTRY = 3
    integer, parameter, public :: WIDGET_TEXT_VIEW = 4
    integer, parameter, public :: WIDGET_PROGRESS_BAR = 5
    integer, parameter, public :: WIDGET_SEPARATOR = 6
    integer, parameter, public :: WIDGET_SLIDER = 7
    integer, parameter, public :: WIDGET_SPIN_BUTTON = 8
    integer, parameter, public :: WIDGET_COMBO_BOX = 9
    integer, parameter, public :: WIDGET_CHECK_BOX = 10

    !> @brief Base widget class
    type, abstract :: forge_widget
        private
        type(forge_widget_handle) :: handle
        type(forge_string) :: name
        type(forge_size) :: size
        type(forge_position) :: position
        logical :: visible = .true.
        logical :: enabled = .true.
        class(forge_backend_base), pointer :: backend => null()
    contains
        procedure :: show => forge_widget_show
        procedure :: hide => forge_widget_hide
        procedure :: enable => forge_widget_enable
        procedure :: disable => forge_widget_disable
        procedure :: set_size => forge_widget_set_size
        procedure :: set_position => forge_widget_set_position
        procedure :: get_size => forge_widget_get_size
        procedure :: get_position => forge_widget_get_position
        procedure :: is_visible => forge_widget_is_visible
        procedure :: is_enabled => forge_widget_is_enabled
        procedure :: set_name => forge_widget_set_name
        procedure :: get_name => forge_widget_get_name
        procedure :: get_handle => forge_widget_get_handle
    end type forge_widget

    !> @brief Button widget
    type, extends(forge_widget) :: forge_button
        private
        type(forge_string) :: label
        type(forge_event_handler) :: click_handler
    contains
        procedure :: set_label => forge_button_set_label
        procedure :: get_label => forge_button_get_label
        procedure :: on_click => forge_button_on_click
    end type forge_button

    !> @brief Label widget
    type, extends(forge_widget) :: forge_label
        private
        type(forge_string) :: text
    contains
        procedure :: set_text => forge_label_set_text
        procedure :: get_text => forge_label_get_text
    end type forge_label

    !> @brief Entry (text input) widget
    type, extends(forge_widget) :: forge_entry
        private
        type(forge_string) :: text
        type(forge_string) :: placeholder
        logical :: read_only = .false.
        integer :: max_length = 0
        type(forge_event_handler) :: change_handler
    contains
        procedure :: set_text => forge_entry_set_text
        procedure :: get_text => forge_entry_get_text
        procedure :: set_placeholder => forge_entry_set_placeholder
        procedure :: set_read_only => forge_entry_set_read_only
        procedure :: set_max_length => forge_entry_set_max_length
        procedure :: on_change => forge_entry_on_change
    end type forge_entry

    !> @brief Text view (multi-line text) widget
    type, extends(forge_widget) :: forge_text_view
        private
        type(forge_string) :: text
        logical :: editable = .true.
        logical :: word_wrap = .true.
        type(forge_event_handler) :: change_handler
    contains
        procedure :: set_text => forge_text_view_set_text
        procedure :: get_text => forge_text_view_get_text
        procedure :: set_editable => forge_text_view_set_editable
        procedure :: set_word_wrap => forge_text_view_set_word_wrap
        procedure :: on_change => forge_text_view_on_change
    end type forge_text_view

    !> @brief Progress bar widget
    type, extends(forge_widget) :: forge_progress_bar
        private
        real(c_double) :: value = 0.0_c_double  ! 0.0 to 1.0
        type(forge_string) :: text
        logical :: show_text = .true.
    contains
        procedure :: set_value => forge_progress_bar_set_value
        procedure :: get_value => forge_progress_bar_get_value
        procedure :: set_text => forge_progress_bar_set_text
        procedure :: set_show_text => forge_progress_bar_set_show_text
    end type forge_progress_bar

    !> @brief Separator widget
    type, extends(forge_widget) :: forge_separator
        private
        logical :: vertical = .false.
    contains
        procedure :: set_vertical => forge_separator_set_vertical
    end type forge_separator

contains

    ! ========== Base Widget Methods ==========

    subroutine forge_widget_show(this)
        class(forge_widget), intent(inout) :: this
        this%visible = .true.
    end subroutine forge_widget_show

    subroutine forge_widget_hide(this)
        class(forge_widget), intent(inout) :: this
        this%visible = .false.
    end subroutine forge_widget_hide

    subroutine forge_widget_enable(this)
        class(forge_widget), intent(inout) :: this
        this%enabled = .true.
    end subroutine forge_widget_enable

    subroutine forge_widget_disable(this)
        class(forge_widget), intent(inout) :: this
        this%enabled = .false.
    end subroutine forge_widget_disable

    subroutine forge_widget_set_size(this, width, height)
        class(forge_widget), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        call this%size%set(width, height)
    end subroutine forge_widget_set_size

    subroutine forge_widget_set_position(this, x, y)
        class(forge_widget), intent(inout) :: this
        integer(c_int), intent(in) :: x, y
        call this%position%set(x, y)
    end subroutine forge_widget_set_position

    function forge_widget_get_size(this) result(size)
        class(forge_widget), intent(in) :: this
        type(forge_size) :: size
        size = this%size
    end function forge_widget_get_size

    function forge_widget_get_position(this) result(position)
        class(forge_widget), intent(in) :: this
        type(forge_position) :: position
        position = this%position
    end function forge_widget_get_position

    function forge_widget_is_visible(this) result(visible)
        class(forge_widget), intent(in) :: this
        logical :: visible
        visible = this%visible
    end function forge_widget_is_visible

    function forge_widget_is_enabled(this) result(enabled)
        class(forge_widget), intent(in) :: this
        logical :: enabled
        enabled = this%enabled
    end function forge_widget_is_enabled

    subroutine forge_widget_set_name(this, name)
        class(forge_widget), intent(inout) :: this
        character(len=*), intent(in) :: name
        call this%name%set(name)
    end subroutine forge_widget_set_name

    function forge_widget_get_name(this) result(name)
        class(forge_widget), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%name%get()
    end function forge_widget_get_name

    function forge_widget_get_handle(this) result(handle)
        class(forge_widget), intent(in) :: this
        type(forge_widget_handle) :: handle
        handle = this%handle
    end function forge_widget_get_handle

    ! ========== Button Methods ==========

    subroutine forge_button_set_label(this, label)
        class(forge_button), intent(inout) :: this
        character(len=*), intent(in) :: label
        call this%label%set(label)
    end subroutine forge_button_set_label

    function forge_button_get_label(this) result(label)
        class(forge_button), intent(in) :: this
        character(len=:), allocatable :: label
        label = this%label%get()
    end function forge_button_get_label

    subroutine forge_button_on_click(this, callback)
        class(forge_button), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%click_handler%set_callback(callback, EVENT_BUTTON_CLICKED)
    end subroutine forge_button_on_click

    ! ========== Label Methods ==========

    subroutine forge_label_set_text(this, text)
        class(forge_label), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine forge_label_set_text

    function forge_label_get_text(this) result(text)
        class(forge_label), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function forge_label_get_text

    ! ========== Entry Methods ==========

    subroutine forge_entry_set_text(this, text)
        class(forge_entry), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine forge_entry_set_text

    function forge_entry_get_text(this) result(text)
        class(forge_entry), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function forge_entry_get_text

    subroutine forge_entry_set_placeholder(this, placeholder)
        class(forge_entry), intent(inout) :: this
        character(len=*), intent(in) :: placeholder
        call this%placeholder%set(placeholder)
    end subroutine forge_entry_set_placeholder

    subroutine forge_entry_set_read_only(this, read_only)
        class(forge_entry), intent(inout) :: this
        logical, intent(in) :: read_only
        this%read_only = read_only
    end subroutine forge_entry_set_read_only

    subroutine forge_entry_set_max_length(this, max_length)
        class(forge_entry), intent(inout) :: this
        integer, intent(in) :: max_length
        this%max_length = max_length
    end subroutine forge_entry_set_max_length

    subroutine forge_entry_on_change(this, callback)
        class(forge_entry), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%change_handler%set_callback(callback, EVENT_TEXT_CHANGED)
    end subroutine forge_entry_on_change

    ! ========== TextView Methods ==========

    subroutine forge_text_view_set_text(this, text)
        class(forge_text_view), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine forge_text_view_set_text

    function forge_text_view_get_text(this) result(text)
        class(forge_text_view), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function forge_text_view_get_text

    subroutine forge_text_view_set_editable(this, editable)
        class(forge_text_view), intent(inout) :: this
        logical, intent(in) :: editable
        this%editable = editable
    end subroutine forge_text_view_set_editable

    subroutine forge_text_view_set_word_wrap(this, word_wrap)
        class(forge_text_view), intent(inout) :: this
        logical, intent(in) :: word_wrap
        this%word_wrap = word_wrap
    end subroutine forge_text_view_set_word_wrap

    subroutine forge_text_view_on_change(this, callback)
        class(forge_text_view), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%change_handler%set_callback(callback, EVENT_TEXT_CHANGED)
    end subroutine forge_text_view_on_change

    ! ========== ProgressBar Methods ==========

    subroutine forge_progress_bar_set_value(this, value)
        class(forge_progress_bar), intent(inout) :: this
        real(c_double), intent(in) :: value
        this%value = max(0.0_c_double, min(1.0_c_double, value))
    end subroutine forge_progress_bar_set_value

    function forge_progress_bar_get_value(this) result(value)
        class(forge_progress_bar), intent(in) :: this
        real(c_double) :: value
        value = this%value
    end function forge_progress_bar_get_value

    subroutine forge_progress_bar_set_text(this, text)
        class(forge_progress_bar), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
    end subroutine forge_progress_bar_set_text

    subroutine forge_progress_bar_set_show_text(this, show_text)
        class(forge_progress_bar), intent(inout) :: this
        logical, intent(in) :: show_text
        this%show_text = show_text
    end subroutine forge_progress_bar_set_show_text

    ! ========== Separator Methods ==========

    subroutine forge_separator_set_vertical(this, vertical)
        class(forge_separator), intent(inout) :: this
        logical, intent(in) :: vertical
        this%vertical = vertical
    end subroutine forge_separator_set_vertical

end module forge_widgets

