!> @brief Window management for ForGE
!> @details Provides high-level window creation and management
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_window
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_events
    use forge_backend
    implicit none
    private

    public :: forge_window_t, forge_window_builder

    !> @brief Window class
    type :: forge_window_t
        private
        type(forge_window_handle) :: handle
        type(forge_string) :: title
        type(forge_size) :: size
        type(forge_position) :: position
        logical :: visible = .false.
        logical :: resizable = .true.
        logical :: decorated = .true.
        class(forge_backend_base), pointer :: backend => null()
        type(forge_event_handler) :: close_handler
    contains
        procedure :: show => forge_window_show
        procedure :: hide => forge_window_hide
        procedure :: close => forge_window_close
        procedure :: set_title => forge_window_set_title
        procedure :: set_size => forge_window_set_size
        procedure :: set_position => forge_window_set_position
        procedure :: get_size => forge_window_get_size
        procedure :: get_position => forge_window_get_position
        procedure :: is_visible => forge_window_is_visible
        procedure :: set_resizable => forge_window_set_resizable
        procedure :: on_close => forge_window_on_close
        procedure :: get_handle => forge_window_get_handle
    end type forge_window_t

    !> @brief Window builder for fluent API
    type :: forge_window_builder
        private
        type(forge_string) :: title
        type(forge_size) :: size
        type(forge_position) :: position
        logical :: resizable = .true.
        logical :: decorated = .true.
        class(forge_backend_base), pointer :: backend => null()
    contains
        procedure :: set_title => forge_window_builder_set_title
        procedure :: set_size => forge_window_builder_set_size
        procedure :: set_position => forge_window_builder_set_position
        procedure :: set_resizable => forge_window_builder_set_resizable
        procedure :: set_decorated => forge_window_builder_set_decorated
        procedure :: set_backend => forge_window_builder_set_backend
        procedure :: build => forge_window_builder_build
    end type forge_window_builder

    ! Import callback interface
    public :: event_callback_interface

contains

    ! ========== forge_window_t Methods ==========

    !> @brief Show the window
    subroutine forge_window_show(this)
        class(forge_window_t), intent(inout) :: this
        
        if (associated(this%backend)) then
            call this%backend%show_window(this%handle)
            this%visible = .true.
        end if
    end subroutine forge_window_show

    !> @brief Hide the window
    subroutine forge_window_hide(this)
        class(forge_window_t), intent(inout) :: this
        
        if (associated(this%backend)) then
            call this%backend%hide_window(this%handle)
            this%visible = .false.
        end if
    end subroutine forge_window_hide

    !> @brief Close and destroy the window
    subroutine forge_window_close(this)
        class(forge_window_t), intent(inout) :: this
        
        if (associated(this%backend)) then
            call this%backend%destroy_window(this%handle)
            this%visible = .false.
        end if
    end subroutine forge_window_close

    !> @brief Set window title
    subroutine forge_window_set_title(this, title)
        class(forge_window_t), intent(inout) :: this
        character(len=*), intent(in) :: title

        call this%title%set(title)
        ! Update backend window title if window is created
        if (associated(this%backend) .and. this%handle%window_id /= 0) then
            ! Backend-specific title update would be implemented here
            ! For now, title is stored locally
        end if
    end subroutine forge_window_set_title

    !> @brief Set window size
    subroutine forge_window_set_size(this, width, height)
        class(forge_window_t), intent(inout) :: this
        integer(c_int), intent(in) :: width, height

        call this%size%set(width, height)
        ! Update backend window size if window is created
        if (associated(this%backend) .and. this%handle%window_id /= 0) then
            ! Backend-specific size update would be implemented here
            ! For now, size is stored locally
        end if
    end subroutine forge_window_set_size

    !> @brief Set window position
    subroutine forge_window_set_position(this, x, y)
        class(forge_window_t), intent(inout) :: this
        integer(c_int), intent(in) :: x, y

        call this%position%set(x, y)
        ! Update backend window position if window is created
        if (associated(this%backend) .and. this%handle%window_id /= 0) then
            ! Backend-specific position update would be implemented here
            ! For now, position is stored locally
        end if
    end subroutine forge_window_set_position

    !> @brief Get window size
    function forge_window_get_size(this) result(size)
        class(forge_window_t), intent(in) :: this
        type(forge_size) :: size
        
        size = this%size
    end function forge_window_get_size

    !> @brief Get window position
    function forge_window_get_position(this) result(position)
        class(forge_window_t), intent(in) :: this
        type(forge_position) :: position
        
        position = this%position
    end function forge_window_get_position

    !> @brief Check if window is visible
    function forge_window_is_visible(this) result(visible)
        class(forge_window_t), intent(in) :: this
        logical :: visible
        
        visible = this%visible
    end function forge_window_is_visible

    !> @brief Set window resizable property
    subroutine forge_window_set_resizable(this, resizable)
        class(forge_window_t), intent(inout) :: this
        logical, intent(in) :: resizable

        this%resizable = resizable
        ! Update backend if window is created
        if (associated(this%backend) .and. this%handle%window_id /= 0) then
            ! Backend-specific resizable update would be implemented here
            ! For now, property is stored locally
        end if
    end subroutine forge_window_set_resizable

    !> @brief Set close event handler
    subroutine forge_window_on_close(this, callback)
        class(forge_window_t), intent(inout) :: this
        procedure(event_callback_interface) :: callback
        
        call this%close_handler%set_callback(callback, EVENT_WINDOW_CLOSED)
    end subroutine forge_window_on_close

    !> @brief Get window handle
    function forge_window_get_handle(this) result(handle)
        class(forge_window_t), intent(in) :: this
        type(forge_window_handle) :: handle
        
        handle = this%handle
    end function forge_window_get_handle

    ! ========== forge_window_builder Methods ==========

    !> @brief Set title in builder
    subroutine forge_window_builder_set_title(this, title)
        class(forge_window_builder), intent(inout) :: this
        character(len=*), intent(in) :: title
        
        call this%title%set(title)
    end subroutine forge_window_builder_set_title

    !> @brief Set size in builder
    subroutine forge_window_builder_set_size(this, width, height)
        class(forge_window_builder), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        
        call this%size%set(width, height)
    end subroutine forge_window_builder_set_size

    !> @brief Set position in builder
    subroutine forge_window_builder_set_position(this, x, y)
        class(forge_window_builder), intent(inout) :: this
        integer(c_int), intent(in) :: x, y
        
        call this%position%set(x, y)
    end subroutine forge_window_builder_set_position

    !> @brief Set resizable in builder
    subroutine forge_window_builder_set_resizable(this, resizable)
        class(forge_window_builder), intent(inout) :: this
        logical, intent(in) :: resizable
        
        this%resizable = resizable
    end subroutine forge_window_builder_set_resizable

    !> @brief Set decorated in builder
    subroutine forge_window_builder_set_decorated(this, decorated)
        class(forge_window_builder), intent(inout) :: this
        logical, intent(in) :: decorated
        
        this%decorated = decorated
    end subroutine forge_window_builder_set_decorated

    !> @brief Set backend in builder
    subroutine forge_window_builder_set_backend(this, backend)
        class(forge_window_builder), intent(inout) :: this
        class(forge_backend_base), target, intent(in) :: backend
        
        this%backend => backend
    end subroutine forge_window_builder_set_backend

    !> @brief Build the window
    function forge_window_builder_build(this, status) result(window)
        class(forge_window_builder), intent(inout) :: this
        type(forge_status), intent(out), optional :: status
        type(forge_window_t) :: window
        type(forge_status) :: local_status
        
        ! Set window properties
        window%title = this%title
        window%size = this%size
        window%position = this%position
        window%resizable = this%resizable
        window%decorated = this%decorated
        window%backend => this%backend
        
        ! Create window through backend
        if (associated(window%backend)) then
            call window%backend%create_window(window%handle, &
                window%title%get(), &
                window%size%width, window%size%height, &
                local_status)
        else
            call local_status%set(FORGE_ERROR_NULL_PTR, "Backend not set")
        end if
        
        if (present(status)) status = local_status
    end function forge_window_builder_build

end module forge_window

