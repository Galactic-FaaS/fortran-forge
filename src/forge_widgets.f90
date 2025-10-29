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
    public :: forge_slider, forge_spin_button, forge_combo_box, forge_check_box
    public :: forge_radio_button

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
    integer, parameter, public :: WIDGET_CALENDAR = 11
    integer, parameter, public :: WIDGET_TREE = 12
    integer, parameter, public :: WIDGET_TABLE = 13
    integer, parameter, public :: WIDGET_GRAPHICS_VIEW = 14

    !> @brief Base widget class
    type, abstract :: forge_widget
        private
        type(forge_widget_handle) :: handle
        type(forge_string) :: name
        type(forge_size) :: size
        type(forge_size) :: minimum_size
        type(forge_size) :: maximum_size
        type(forge_size) :: size_hint
        type(forge_position) :: position
        type(forge_size_policy) :: size_policy
        logical :: visible = .true.
        logical :: enabled = .true.
        logical :: size_hint_valid = .false.
        class(forge_backend_base), pointer :: backend => null()
    contains
        procedure :: show => forge_widget_show
        procedure :: hide => forge_widget_hide
        procedure :: enable => forge_widget_enable
        procedure :: disable => forge_widget_disable
        procedure :: set_size => forge_widget_set_size
        procedure :: set_position => forge_widget_set_position
        procedure :: set_minimum_size => forge_widget_set_minimum_size
        procedure :: set_maximum_size => forge_widget_set_maximum_size
        procedure :: set_size_hint => forge_widget_set_size_hint
        procedure :: set_size_policy => forge_widget_set_size_policy
        procedure :: get_size => forge_widget_get_size
        procedure :: get_position => forge_widget_get_position
        procedure :: get_minimum_size => forge_widget_get_minimum_size
        procedure :: get_maximum_size => forge_widget_get_maximum_size
        procedure :: get_size_hint => forge_widget_get_size_hint
        procedure :: get_size_policy => forge_widget_get_size_policy
        procedure :: is_visible => forge_widget_is_visible
        procedure :: is_enabled => forge_widget_is_enabled
        procedure :: set_name => forge_widget_set_name
        procedure :: get_name => forge_widget_get_name
        procedure :: get_handle => forge_widget_get_handle
        procedure :: invalidate_size_hint => forge_widget_invalidate_size_hint
        procedure :: update_size_hint => forge_widget_update_size_hint
        procedure(update_size_hint_interface), deferred :: calculate_size_hint
    end type forge_widget

    !> Abstract interface for size hint calculation
    abstract interface
        subroutine update_size_hint_interface(this)
            import :: forge_widget
            class(forge_widget), intent(inout) :: this
        end subroutine update_size_hint_interface
    end interface

    !> @brief Button widget
    type, extends(forge_widget) :: forge_button
        private
        type(forge_string) :: label
        type(forge_event_handler) :: click_handler
    contains
        procedure :: set_label => forge_button_set_label
        procedure :: get_label => forge_button_get_label
        procedure :: on_click => forge_button_on_click
        procedure :: calculate_size_hint => forge_button_calculate_size_hint
    end type forge_button

    !> @brief Label widget
    type, extends(forge_widget) :: forge_label
        private
        type(forge_string) :: text
    contains
        procedure :: set_text => forge_label_set_text
        procedure :: get_text => forge_label_get_text
        procedure :: calculate_size_hint => forge_label_calculate_size_hint
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

    !> @brief Slider widget
    type, extends(forge_widget) :: forge_slider
        private
        real(c_double) :: value = 0.0_c_double
        real(c_double) :: minimum = 0.0_c_double
        real(c_double) :: maximum = 100.0_c_double
        real(c_double) :: step = 1.0_c_double
        logical :: vertical = .false.
        type(forge_event_handler) :: value_changed_handler
    contains
        procedure :: set_value => forge_slider_set_value
        procedure :: get_value => forge_slider_get_value
        procedure :: set_range => forge_slider_set_range
        procedure :: set_step => forge_slider_set_step
        procedure :: set_vertical => forge_slider_set_vertical
        procedure :: on_value_changed => forge_slider_on_value_changed
    end type forge_slider

    !> @brief Spin button widget
    type, extends(forge_widget) :: forge_spin_button
        private
        real(c_double) :: value = 0.0_c_double
        real(c_double) :: minimum = 0.0_c_double
        real(c_double) :: maximum = 100.0_c_double
        real(c_double) :: step = 1.0_c_double
        integer :: digits = 0
        type(forge_event_handler) :: value_changed_handler
    contains
        procedure :: set_value => forge_spin_button_set_value
        procedure :: get_value => forge_spin_button_get_value
        procedure :: set_range => forge_spin_button_set_range
        procedure :: set_step => forge_spin_button_set_step
        procedure :: set_digits => forge_spin_button_set_digits
        procedure :: on_value_changed => forge_spin_button_on_value_changed
    end type forge_spin_button

    !> @brief Combo box widget
    type, extends(forge_widget) :: forge_combo_box
        private
        type(forge_string), allocatable :: items(:)
        integer :: selected_index = 0
        logical :: editable = .false.
        type(forge_event_handler) :: selection_changed_handler
    contains
        procedure :: add_item => forge_combo_box_add_item
        procedure :: remove_item => forge_combo_box_remove_item
        procedure :: clear_items => forge_combo_box_clear_items
        procedure :: set_selected_index => forge_combo_box_set_selected_index
        procedure :: get_selected_index => forge_combo_box_get_selected_index
        procedure :: get_selected_text => forge_combo_box_get_selected_text
        procedure :: set_editable => forge_combo_box_set_editable
        procedure :: on_selection_changed => forge_combo_box_on_selection_changed
    end type forge_combo_box

    !> @brief Check box widget
    type, extends(forge_widget) :: forge_check_box
        private
        type(forge_string) :: label
        logical :: checked = .false.
        type(forge_event_handler) :: toggled_handler
    contains
        procedure :: set_label => forge_check_box_set_label
        procedure :: get_label => forge_check_box_get_label
        procedure :: set_checked => forge_check_box_set_checked
        procedure :: get_checked => forge_check_box_get_checked
        procedure :: toggle => forge_check_box_toggle
        procedure :: on_toggled => forge_check_box_on_toggled
    end type forge_check_box

    !> @brief Radio button widget
    type, extends(forge_widget) :: forge_radio_button
        private
        type(forge_string) :: label
        logical :: checked = .false.
        type(forge_event_handler) :: toggled_handler
    contains
        procedure :: set_label => forge_radio_button_set_label
        procedure :: get_label => forge_radio_button_get_label
        procedure :: set_checked => forge_radio_button_set_checked
        procedure :: get_checked => forge_radio_button_get_checked
        procedure :: on_toggled => forge_radio_button_on_toggled
    end type forge_radio_button

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

    subroutine forge_widget_set_minimum_size(this, width, height)
        class(forge_widget), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        call this%minimum_size%set(width, height)
    end subroutine forge_widget_set_minimum_size

    subroutine forge_widget_set_maximum_size(this, width, height)
        class(forge_widget), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        call this%maximum_size%set(width, height)
    end subroutine forge_widget_set_maximum_size

    subroutine forge_widget_set_size_hint(this, width, height)
        class(forge_widget), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        call this%size_hint%set(width, height)
        this%size_hint_valid = .true.
    end subroutine forge_widget_set_size_hint

    subroutine forge_widget_set_size_policy(this, horizontal_policy, vertical_policy, horizontal_stretch, vertical_stretch)
        class(forge_widget), intent(inout) :: this
        integer, intent(in) :: horizontal_policy, vertical_policy
        integer, intent(in), optional :: horizontal_stretch, vertical_stretch
        call this%size_policy%set_horizontal_policy(horizontal_policy)
        call this%size_policy%set_vertical_policy(vertical_policy)
        if (present(horizontal_stretch)) call this%size_policy%set_horizontal_stretch(horizontal_stretch)
        if (present(vertical_stretch)) call this%size_policy%set_vertical_stretch(vertical_stretch)
    end subroutine forge_widget_set_size_policy

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

    function forge_widget_get_minimum_size(this) result(minimum_size)
        class(forge_widget), intent(in) :: this
        type(forge_size) :: minimum_size
        minimum_size = this%minimum_size
    end function forge_widget_get_minimum_size

    function forge_widget_get_maximum_size(this) result(maximum_size)
        class(forge_widget), intent(in) :: this
        type(forge_size) :: maximum_size
        maximum_size = this%maximum_size
    end function forge_widget_get_maximum_size

    function forge_widget_get_size_hint(this) result(size_hint)
        class(forge_widget), intent(in) :: this
        type(forge_size) :: size_hint
        ! Ensure size hint is up to date
        call this%update_size_hint()
        size_hint = this%size_hint
    end function forge_widget_get_size_hint

    function forge_widget_get_size_policy(this) result(size_policy)
        class(forge_widget), intent(in) :: this
        type(forge_size_policy) :: size_policy
        size_policy = this%size_policy
    end function forge_widget_get_size_policy

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

    subroutine forge_widget_invalidate_size_hint(this)
        class(forge_widget), intent(inout) :: this
        this%size_hint_valid = .false.
    end subroutine forge_widget_invalidate_size_hint

    subroutine forge_widget_update_size_hint(this)
        class(forge_widget), intent(inout) :: this
        if (.not. this%size_hint_valid) then
            call this%calculate_size_hint()
            this%size_hint_valid = .true.
        end if
    end subroutine forge_widget_update_size_hint

    ! ========== Button Methods ==========

    subroutine forge_button_set_label(this, label)
        class(forge_button), intent(inout) :: this
        character(len=*), intent(in) :: label
        call this%label%set(label)
        call this%invalidate_size_hint()
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

    subroutine forge_button_calculate_size_hint(this)
        class(forge_button), intent(inout) :: this
        integer :: text_width, text_height
        character(len=:), allocatable :: text

        text = this%get_label()
        text_width = len(text) * 8  ! Approximate character width
        text_height = 16  ! Approximate character height

        ! Add padding for button appearance
        call this%set_size_hint(text_width + 20, text_height + 10)
    end subroutine forge_button_calculate_size_hint

    ! ========== Label Methods ==========

    subroutine forge_label_set_text(this, text)
        class(forge_label), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%text%set(text)
        call this%invalidate_size_hint()
    end subroutine forge_label_set_text

    function forge_label_get_text(this) result(text)
        class(forge_label), intent(in) :: this
        character(len=:), allocatable :: text
        text = this%text%get()
    end function forge_label_get_text

    subroutine forge_label_calculate_size_hint(this)
        class(forge_label), intent(inout) :: this
        integer :: text_width, text_height
        character(len=:), allocatable :: text

        text = this%get_text()
        text_width = len(text) * 8  ! Approximate character width
        text_height = 16  ! Approximate character height

        call this%set_size_hint(text_width, text_height)
    end subroutine forge_label_calculate_size_hint

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

    ! ========== Slider Methods ==========

    subroutine forge_slider_set_value(this, value)
        class(forge_slider), intent(inout) :: this
        real(c_double), intent(in) :: value
        this%value = max(this%minimum, min(this%maximum, value))
    end subroutine forge_slider_set_value

    function forge_slider_get_value(this) result(value)
        class(forge_slider), intent(in) :: this
        real(c_double) :: value
        value = this%value
    end function forge_slider_get_value

    subroutine forge_slider_set_range(this, minimum, maximum)
        class(forge_slider), intent(inout) :: this
        real(c_double), intent(in) :: minimum, maximum
        this%minimum = minimum
        this%maximum = maximum
        this%value = max(minimum, min(maximum, this%value))
    end subroutine forge_slider_set_range

    subroutine forge_slider_set_step(this, step)
        class(forge_slider), intent(inout) :: this
        real(c_double), intent(in) :: step
        this%step = max(0.0_c_double, step)
    end subroutine forge_slider_set_step

    subroutine forge_slider_set_vertical(this, vertical)
        class(forge_slider), intent(inout) :: this
        logical, intent(in) :: vertical
        this%vertical = vertical
    end subroutine forge_slider_set_vertical

    subroutine forge_slider_on_value_changed(this, callback)
        class(forge_slider), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%value_changed_handler%set_callback(callback, EVENT_VALUE_CHANGED)
    end subroutine forge_slider_on_value_changed

    ! ========== Spin Button Methods ==========

    subroutine forge_spin_button_set_value(this, value)
        class(forge_spin_button), intent(inout) :: this
        real(c_double), intent(in) :: value
        this%value = max(this%minimum, min(this%maximum, value))
    end subroutine forge_spin_button_set_value

    function forge_spin_button_get_value(this) result(value)
        class(forge_spin_button), intent(in) :: this
        real(c_double) :: value
        value = this%value
    end function forge_spin_button_get_value

    subroutine forge_spin_button_set_range(this, minimum, maximum)
        class(forge_spin_button), intent(inout) :: this
        real(c_double), intent(in) :: minimum, maximum
        this%minimum = minimum
        this%maximum = maximum
        this%value = max(minimum, min(maximum, this%value))
    end subroutine forge_spin_button_set_range

    subroutine forge_spin_button_set_step(this, step)
        class(forge_spin_button), intent(inout) :: this
        real(c_double), intent(in) :: step
        this%step = max(0.0_c_double, step)
    end subroutine forge_spin_button_set_step

    subroutine forge_spin_button_set_digits(this, digits)
        class(forge_spin_button), intent(inout) :: this
        integer, intent(in) :: digits
        this%digits = max(0, digits)
    end subroutine forge_spin_button_set_digits

    subroutine forge_spin_button_on_value_changed(this, callback)
        class(forge_spin_button), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%value_changed_handler%set_callback(callback, EVENT_VALUE_CHANGED)
    end subroutine forge_spin_button_on_value_changed

    ! ========== Combo Box Methods ==========

    subroutine forge_combo_box_add_item(this, item)
        class(forge_combo_box), intent(inout) :: this
        character(len=*), intent(in) :: item
        type(forge_string), allocatable :: temp(:)
        integer :: n

        if (.not. allocated(this%items)) then
            allocate(this%items(1))
            call this%items(1)%set(item)
        else
            n = size(this%items)
            allocate(temp(n+1))
            temp(1:n) = this%items
            call temp(n+1)%set(item)
            deallocate(this%items)
            this%items = temp
        end if
    end subroutine forge_combo_box_add_item

    subroutine forge_combo_box_remove_item(this, index)
        class(forge_combo_box), intent(inout) :: this
        integer, intent(in) :: index
        type(forge_string), allocatable :: temp(:)
        integer :: n, i

        if (.not. allocated(this%items)) return
        n = size(this%items)
        if (index < 1 .or. index > n) return

        if (n > 1) then
            allocate(temp(n-1))
            do i = 1, index-1
                temp(i) = this%items(i)
            end do
            do i = index+1, n
                temp(i-1) = this%items(i)
            end do
            deallocate(this%items)
            this%items = temp
        else
            deallocate(this%items)
        end if

        if (this%selected_index >= index .and. this%selected_index > 1) then
            this%selected_index = this%selected_index - 1
        end if
    end subroutine forge_combo_box_remove_item

    subroutine forge_combo_box_clear_items(this)
        class(forge_combo_box), intent(inout) :: this
        if (allocated(this%items)) deallocate(this%items)
        this%selected_index = 0
    end subroutine forge_combo_box_clear_items

    subroutine forge_combo_box_set_selected_index(this, index)
        class(forge_combo_box), intent(inout) :: this
        integer, intent(in) :: index
        if (allocated(this%items)) then
            this%selected_index = max(0, min(size(this%items), index))
        else
            this%selected_index = 0
        end if
    end subroutine forge_combo_box_set_selected_index

    function forge_combo_box_get_selected_index(this) result(index)
        class(forge_combo_box), intent(in) :: this
        integer :: index
        index = this%selected_index
    end function forge_combo_box_get_selected_index

    function forge_combo_box_get_selected_text(this) result(text)
        class(forge_combo_box), intent(in) :: this
        character(len=:), allocatable :: text
        if (this%selected_index > 0 .and. allocated(this%items)) then
            text = this%items(this%selected_index)%get()
        else
            text = ""
        end if
    end function forge_combo_box_get_selected_text

    subroutine forge_combo_box_set_editable(this, editable)
        class(forge_combo_box), intent(inout) :: this
        logical, intent(in) :: editable
        this%editable = editable
    end subroutine forge_combo_box_set_editable

    subroutine forge_combo_box_on_selection_changed(this, callback)
        class(forge_combo_box), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%selection_changed_handler%set_callback(callback, 12)
    end subroutine forge_combo_box_on_selection_changed

    ! ========== Check Box Methods ==========

    subroutine forge_check_box_set_label(this, label)
        class(forge_check_box), intent(inout) :: this
        character(len=*), intent(in) :: label
        call this%label%set(label)
    end subroutine forge_check_box_set_label

    function forge_check_box_get_label(this) result(label)
        class(forge_check_box), intent(in) :: this
        character(len=:), allocatable :: label
        label = this%label%get()
    end function forge_check_box_get_label

    subroutine forge_check_box_set_checked(this, checked)
        class(forge_check_box), intent(inout) :: this
        logical, intent(in) :: checked
        this%checked = checked
    end subroutine forge_check_box_set_checked

    function forge_check_box_get_checked(this) result(checked)
        class(forge_check_box), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function forge_check_box_get_checked

    subroutine forge_check_box_toggle(this)
        class(forge_check_box), intent(inout) :: this
        this%checked = .not. this%checked
    end subroutine forge_check_box_toggle

    subroutine forge_check_box_on_toggled(this, callback)
        class(forge_check_box), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%toggled_handler%set_callback(callback, 11)
    end subroutine forge_check_box_on_toggled

    ! ========== Radio Button Methods ==========

    subroutine forge_radio_button_set_label(this, label)
        class(forge_radio_button), intent(inout) :: this
        character(len=*), intent(in) :: label
        call this%label%set(label)
    end subroutine forge_radio_button_set_label

    function forge_radio_button_get_label(this) result(label)
        class(forge_radio_button), intent(in) :: this
        character(len=:), allocatable :: label
        label = this%label%get()
    end function forge_radio_button_get_label

    subroutine forge_radio_button_set_checked(this, checked)
        class(forge_radio_button), intent(inout) :: this
        logical, intent(in) :: checked
        this%checked = checked
    end subroutine forge_radio_button_set_checked

    function forge_radio_button_get_checked(this) result(checked)
        class(forge_radio_button), intent(in) :: this
        logical :: checked
        checked = this%checked
    end function forge_radio_button_get_checked

    subroutine forge_radio_button_on_toggled(this, callback)
        class(forge_radio_button), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        call this%toggled_handler%set_callback(callback, 11)
    end subroutine forge_radio_button_on_toggled

end module forge_widgets

