!> @brief Swipe view widget for mobile interfaces
!> @details Horizontal scrolling view with swipe gestures
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_swipe_view
    use iso_c_binding
    use forge_types
    use forge_widgets
    use forge_touch
    implicit none
    private

    public :: forge_swipe_view
    public :: swipe_view_create, swipe_view_add_page, swipe_view_scroll_to_page

    !> @brief Swipe view
    type, extends(forge_widget) :: forge_swipe_view
        private
        type(c_ptr), allocatable :: pages(:)
        integer :: page_count = 0
        integer :: current_page = 1
        logical :: paging_enabled = .true.
        logical :: shows_page_indicator = .true.
        type(forge_gesture_recognizer) :: gesture_recognizer
    contains
        procedure :: add_page => swipe_view_add_page
        procedure :: remove_page => swipe_view_remove_page
        procedure :: scroll_to_page => swipe_view_scroll_to_page
        procedure :: get_current_page => swipe_view_get_current_page
        procedure :: set_paging_enabled => swipe_view_set_paging_enabled
        procedure :: handle_touch_event => swipe_view_handle_touch
    end type forge_swipe_view

contains

    !> @brief Create swipe view
    function swipe_view_create(x, y, width, height) result(view)
        integer(c_int), intent(in) :: x, y, width, height
        type(forge_swipe_view) :: view

        ! Initialize base widget
        view%rect%x = x
        view%rect%y = y
        view%rect%width = width
        view%rect%height = height
        view%visible = .true.
        view%enabled = .true.

        ! Initialize swipe view specific properties
        view%page_count = 0
        view%current_page = 1
        view%paging_enabled = .true.
        view%shows_page_indicator = .true.

        ! Initialize gesture recognizer
        call init_gesture_recognizer(view%gesture_recognizer)

        allocate(view%pages(10))  ! Initial capacity
        view%pages = c_null_ptr
    end function swipe_view_create

    !> @brief Add page to swipe view
    subroutine swipe_view_add_page(this, page_widget)
        class(forge_swipe_view), intent(inout) :: this
        type(c_ptr), intent(in) :: page_widget

        if (this%page_count >= size(this%pages)) then
            call resize_page_array(this)
        end if

        this%page_count = this%page_count + 1
        this%pages(this%page_count) = page_widget
    end subroutine swipe_view_add_page

    !> @brief Remove page from swipe view
    subroutine swipe_view_remove_page(this, page_index)
        class(forge_swipe_view), intent(inout) :: this
        integer, intent(in) :: page_index
        integer :: i

        if (page_index >= 1 .and. page_index <= this%page_count) then
            do i = page_index, this%page_count - 1
                this%pages(i) = this%pages(i + 1)
            end do
            this%pages(this%page_count) = c_null_ptr
            this%page_count = this%page_count - 1

            if (this%current_page > this%page_count) then
                this%current_page = this%page_count
            end if
        end if
    end subroutine swipe_view_remove_page

    !> @brief Scroll to specific page
    subroutine swipe_view_scroll_to_page(this, page_index, animated)
        class(forge_swipe_view), intent(inout) :: this
        integer, intent(in) :: page_index
        logical, intent(in), optional :: animated

        if (page_index >= 1 .and. page_index <= this%page_count) then
            this%current_page = page_index
            ! Trigger scroll animation if animated is true
        end if
    end subroutine swipe_view_scroll_to_page

    !> @brief Get current page index
    function swipe_view_get_current_page(this) result(page_index)
        class(forge_swipe_view), intent(in) :: this
        integer :: page_index

        page_index = this%current_page
    end function swipe_view_get_current_page

    !> @brief Set paging enabled
    subroutine swipe_view_set_paging_enabled(this, enabled)
        class(forge_swipe_view), intent(inout) :: this
        logical, intent(in) :: enabled

        this%paging_enabled = enabled
    end subroutine swipe_view_set_paging_enabled

    !> @brief Handle touch event
    subroutine swipe_view_handle_touch(this, event)
        class(forge_swipe_view), intent(inout) :: this
        type(forge_event), intent(in) :: event
        type(forge_touch_gesture) :: gestures(10)
        integer :: num_gestures, i

        ! Process touch events through gesture recognizer
        call process_touch_events(this%gesture_recognizer, [event], 1, gestures, num_gestures)

        ! Handle recognized gestures
        do i = 1, num_gestures
            select case (gestures(i)%gesture_type)
            case (GESTURE_SWIPE_LEFT)
                if (this%current_page < this%page_count) then
                    call this%scroll_to_page(this%current_page + 1, .true.)
                end if
            case (GESTURE_SWIPE_RIGHT)
                if (this%current_page > 1) then
                    call this%scroll_to_page(this%current_page - 1, .true.)
                end if
            end select
        end do
    end subroutine swipe_view_handle_touch

    !> @brief Resize page array when capacity is exceeded
    subroutine resize_page_array(this)
        class(forge_swipe_view), intent(inout) :: this
        type(c_ptr), allocatable :: new_pages(:)
        integer :: new_size

        new_size = size(this%pages) * 2
        allocate(new_pages(new_size))
        new_pages(1:size(this%pages)) = this%pages
        new_pages(size(this%pages)+1:new_size) = c_null_ptr

        deallocate(this%pages)
        this%pages = new_pages
    end subroutine resize_page_array

end module forge_swipe_view